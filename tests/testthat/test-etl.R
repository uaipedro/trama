# CRITÉRIO DE ACEITE DO PROJETO: um ETL real rodando no motor, com preview em
# cada passo e recomputação incremental. É o teste que prova que o núcleo
# serve um domínio de verdade — e, por ser de dados (milissegundos por nó),
# é o que mantém o ciclo de iteração honesto.

test_that("ETL completo roda e dá o resultado certo", {
  skip_if_no_data()
  reg <- etl_registry(); s <- tmp_store(); csv <- fixture_csv()
  out <- tr_value(etl_doc(reg, csv), "ordenar", reg, s)

  expect_equal(nrow(out), 2L)
  expect_equal(names(out), c("regiao", "total_regiao"))
  # sul:  10*1 + 20*2 = 50   (o valor 5 é filtrado)
  # norte: 30*3 + 40*4 + 15*2 = 280
  expect_equal(out$regiao, c("norte", "sul"))
  expect_equal(out$total_regiao, c(280, 50))
})

test_that("cada passo tem preview próprio, com dado e não imagem", {
  skip_if_no_data()
  reg <- etl_registry(); s <- tmp_store()
  doc <- etl_doc(reg, fixture_csv())
  tr_run(doc, registry = reg, store = s)
  plan <- tr_plan(doc, registry = reg, store = s)

  for (id in c("ler", "filtrar", "calc", "resumo", "ordenar")) {
    h <- tr_store_handle(s, plan$units[[id]]$outputs$out)
    expect_equal(h$preview$renderer, "data/table", info = id)
    expect_gt(length(h$preview$data$rows), 0)
    expect_null(h$preview$files)           # dado, não arquivo
    expect_gt(h$summary$linhas, 0)
  }
  expect_equal(tr_store_handle(s, plan$units$ler$outputs$out)$summary$linhas, 6)
  expect_equal(tr_store_handle(s, plan$units$filtrar$outputs$out)$summary$linhas, 5)
})

test_that("editar um param recomputa só dali pra frente", {
  skip_if_no_data()
  reg <- etl_registry(); s <- tmp_store()
  doc <- etl_doc(reg, fixture_csv())
  tr_run(doc, registry = reg, store = s)

  doc2 <- tr_doc_apply(doc, list(op = "set_param", node = "calc",
                                 name = "expr", value = "valor + qtd"), reg)
  ev <- list()
  tr_run(doc2, registry = reg, store = s,
         on_event = function(e) ev[[length(ev) + 1]] <<- e)
  # O MOTOR emite `type`; só o transporte renomeia pra `unit_type` (o motor
  # não sabe o que é Shiny). Ler `type` aqui é ler o contrato do motor.
  st <- vapply(Filter(function(e) !is.null(e$node), ev),
               function(e) paste0(e$node, ":", e$type), "")

  expect_true("ler:cached" %in% st)        # a montante intocado
  expect_true("filtrar:cached" %in% st)
  expect_true("calc:running" %in% st)      # o nó editado
  expect_true("resumo:running" %in% st)    # e o que depende dele
  expect_true("ordenar:running" %in% st)
})

test_that("CSV alterado no disco invalida o cache — o nó é impuro", {
  skip_if_no_data()
  reg <- etl_registry(); s <- tmp_store(); csv <- fixture_csv()
  doc <- etl_doc(reg, csv)
  expect_equal(nrow(tr_value(doc, "ler", reg, s)), 6L)

  Sys.sleep(1.1)
  utils::write.csv(data.frame(regiao = "sul", produto = "a", valor = 1, qtd = 1),
                   csv, row.names = FALSE)
  # Sem `fingerprint` declarado, isto serviria as 6 linhas velhas em silêncio.
  expect_equal(nrow(tr_value(doc, "ler", reg, s)), 1L)
})

test_that("erro de expressão fica no nó certo e bloqueia o jusante", {
  skip_if_no_data()
  reg <- etl_registry(); s <- tmp_store()
  doc <- tr_doc_apply(etl_doc(reg, fixture_csv()),
                      list(op = "set_param", node = "filtrar", name = "expr",
                           value = "coluna_que_nao_existe > 1"), reg)
  ev <- list()
  tr_run(doc, registry = reg, store = s, on_event = function(e) ev[[length(ev) + 1]] <<- e)
  st <- vapply(Filter(function(e) !is.null(e$node), ev),
               function(e) paste0(e$node, ":", e$type), "")

  expect_true("ler:done" %in% st || "ler:cached" %in% st)
  expect_true("filtrar:failed" %in% st)
  expect_true("calc:blocked" %in% st)
  expect_true("ordenar:blocked" %in% st)   # transitivo, dois níveis adiante
  expect_false(any(grepl("calc:running|ordenar:running", st)))
})

test_that("documento do ETL faz round-trip e mantém as chaves", {
  skip_if_no_data()
  reg <- etl_registry()
  doc <- etl_doc(reg, fixture_csv())
  expect_equal(tr_plan(doc, registry = reg)$keys,
               tr_plan(tr_doc_parse(tr_doc_json(doc)), registry = reg)$keys)
  expect_length(tr_doc_validate(doc, reg), 0)
})

test_that("join com duas entradas roda", {
  skip_if_no_data()
  reg <- etl_registry(); s <- tmp_store()
  csv <- fixture_csv()
  lookup <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(regiao = c("sul", "norte"), gerente = c("ana", "beto")),
                   lookup, row.names = FALSE)
  ops <- list(
    list(op = "add_node", type = "data/read_csv", id = "a", params = list(path = csv)),
    list(op = "add_node", type = "data/read_csv", id = "b", params = list(path = lookup)),
    list(op = "add_node", type = "data/join", id = "j", params = list(by = "regiao")),
    list(op = "connect", from_node = "a", from_port = "out", to_node = "j", to_port = "left"),
    list(op = "connect", from_node = "b", from_port = "out", to_node = "j", to_port = "right"))
  doc <- tr_doc(); for (op in ops) doc <- tr_doc_apply(doc, op, reg)
  out <- tr_value(doc, "j", reg, s)
  expect_true("gerente" %in% names(out))
  expect_equal(nrow(out), 6L)
})

test_that("data/read_csv resolve path relativo contra a raiz do projeto, não o cwd do daemon", {
  skip_if_no_data()
  reg <- etl_registry()
  root <- tempfile("proj"); dir.create(root)
  utils::write.csv(data.frame(regiao = c("sul", "norte"), valor = c(1, 2)),
                   file.path(root, "vendas.csv"), row.names = FALSE)
  s <- tr_store(file.path(root, ".trama", "store"), project_root = root)
  doc <- build(reg, list(
    list(op = "add_node", type = "data/read_csv", id = "ler", params = list(path = "vendas.csv"))))

  withr::with_dir(tempdir(), {          # cwd DIFERENTE da raiz do projeto
    out <- tr_value(doc, "ler", reg, s)
    expect_equal(nrow(out), 2L)
    expect_equal(out$regiao, c("sul", "norte"))
  })
})

test_that("nível 1: as funções da coleção são chamáveis direto", {
  skip_if_no_data()
  reg <- etl_registry()
  d <- tr_fn("data/read_csv", reg)(fixture_csv())
  expect_equal(nrow(tr_fn("data/filter", reg)(d, "valor > 8")), 5L)
})
