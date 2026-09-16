# O pool é onde a tese ("o coordenador nunca carrega valor") se prova de fato.
# Até aqui estava validado só por leitura. Este arquivo roda daemons de
# verdade, com o núcleo e a coleção carregados por pkgload dentro deles.

skip_pool <- function() {
  skip_if_not_installed("mirai"); skip_if_not_installed("dplyr"); skip_if_not_installed("readr")
  skip_if_not(dir.exists("../../collections/trama.data"), "coleção trama.data ausente")
}

pool_setup <- function() {
  core <- normalizePath("../.."); coll <- normalizePath("../../collections/trama.data")
  bquote({
    pkgload::load_all(.(core), quiet = TRUE, attach = FALSE)
    pkgload::load_all(.(coll), quiet = TRUE, attach = FALSE, export_all = FALSE)
  })
}

test_that("pool roda o ETL nos daemons e o resultado bate com o sequencial", {
  skip_pool()
  reg <- etl_registry(); csv <- fixture_csv(); doc <- etl_doc(reg, csv)
  s1 <- tmp_store(); s2 <- tmp_store()
  seq <- tr_run(doc, registry = reg, store = s1)
  ex <- tr_executor_pool(2L, registry = reg, setup = pool_setup())
  on.exit(ex$shutdown(), add = TRUE)
  ev <- list()
  par <- tr_run(doc, registry = reg, store = s2, executor = ex,
                on_event = function(e) ev[[length(ev) + 1]] <<- e)
  expect_setequal(par$done, seq$done)
  ty <- tr_get_type("data/table", reg)
  # `results[[node]]` guarda o HANDLE por porta, não a chave nua (mesma
  # ressalva de test-scheduler.R) — `$key` é o campo que `tr_store_get` espera.
  expect_equal(tr_store_get(s2, par$results$ordenar$out$key, ty), tr_store_get(s1, seq$results$ordenar$out$key, ty))
  # Nenhum valor atravessou: o evento `done` carrega handle, não dado.
  d <- Filter(function(e) identical(e$type, "done"), ev)
  expect_true(all(vapply(d, function(e) is.null(e$handles[[1]]$value), TRUE)))
})

test_that("handoff cancela unidade em voo no pool e o progresso chega como evento", {
  skip_pool()
  core <- normalizePath("../.."); slowp <- normalizePath("fixtures/trama.slow")
  reg <- tr_registry()
  pkgload::load_all(slowp, quiet = TRUE, attach = FALSE); tr_use("trama.slow", registry = reg)
  ex <- tr_executor_pool(1L, registry = reg, setup = bquote({
    pkgload::load_all(.(core), quiet = TRUE, attach = FALSE)
    pkgload::load_all(.(slowp), quiet = TRUE, attach = FALSE)
  }))
  on.exit(ex$shutdown(), add = TRUE)
  s <- tmp_store(); ev <- list(); rec <- function(e) ev[[length(ev) + 1]] <<- e
  doc1 <- build(reg, list(list(op = "add_node", type = "slow/sleep", id = "z", params = list(secs = 3))))
  s1 <- tr_scheduler(tr_plan(doc1, registry = reg, store = s), reg, s, ex, rec)
  t0 <- Sys.time()
  while (!s1$step() && !any(vapply(ev, function(e) identical(e$type, "progress"), TRUE))) Sys.sleep(0.05)
  expect_true(any(vapply(ev, function(e) identical(e$type, "progress"), TRUE)))
  doc2 <- tr_doc_apply(doc1, list(op = "set_param", node = "z", name = "secs", value = 0.1), reg)
  plan2 <- tr_plan(doc2, registry = reg, store = s)
  expect_length(s1$handoff(unlist(plan2$keys)), 0L)
  s2 <- tr_scheduler(plan2, reg, s, ex, rec); while (!s2$step()) Sys.sleep(0.02)
  expect_lt(as.numeric(Sys.time() - t0, units = "secs"), 3)   # não esperou os 3s do cancelado
  expect_equal(tr_store_get(s, s2$result()$results$z$out$key, tr_get_type("slow/x", reg)), 0.1)
})

test_that("`ctx_extra` atravessa a fronteira de processo: a cadência de checkpoint vale no pool", {
  # A propriedade que o resto da suíte não alcança. `tr_executor_pool()$submit`
  # SERIALIZA o que manda; um botão que funciona no sequencial e não no pool é o
  # mesmo defeito desta tarefa uma camada abaixo — e era exatamente o estado
  # anterior, com nenhum dos dois executores passando `ctx_extra`.
  #
  # Medido pelo CHECKPOINT no disco, e não por evento: o store é sistema de
  # arquivos compartilhado, então o que o daemon gravou é legível aqui. Com a
  # cadência 2 e a morte no ponto 7, o que sobra é o passo 6; com o default (100)
  # e dez pontos, nada é gravado. A asserção distingue "o valor atravessou" de
  # "checkpoint existe".
  skip_if_not_installed("mirai")
  skip_if_not_installed("pkgload")
  core <- normalizePath("../.."); slowp <- normalizePath("fixtures/trama.slow")
  reg <- tr_registry()
  pkgload::load_all(slowp, quiet = TRUE, attach = FALSE); tr_use("trama.slow", registry = reg)
  ex <- tr_executor_pool(1L, registry = reg, setup = bquote({
    pkgload::load_all(.(core), quiet = TRUE, attach = FALSE)
    pkgload::load_all(.(slowp), quiet = TRUE, attach = FALSE)
  }))
  on.exit(ex$shutdown(), add = TRUE)

  doc <- build(reg, list(
    list(op = "add_node", type = "slow/pontos", id = "fo", params = list(n = 10L)),
    list(op = "add_node", type = "slow/morre", id = "mo", params = list(em = 7)),
    list(op = "add_node", type = "slow/junta", id = "co"),
    list(op = "connect", from_node = "fo", from_port = "out", to_node = "mo", to_port = "x"),
    list(op = "connect", from_node = "mo", from_port = "out", to_node = "co", to_port = "x")))

  s <- tmp_store(); u <- tr_plan(doc, registry = reg, store = s)$units$co
  expect_equal(u$kind, "stream_region")
  r <- tr_run(doc, registry = reg, store = s, executor = ex,
              ctx_extra = list(checkpoint_every = 2))
  expect_true("co" %in% r$skipped)
  expect_equal(readRDS(file.path(s$root, "stream", u$key, "ckpt.rds"))$i, 6L)

  # O contraste, no mesmo pool: sem passar nada, o default de 100 não escreve
  # nada em dez pontos.
  s2 <- tmp_store(); u2 <- tr_plan(doc, registry = reg, store = s2)$units$co
  tr_run(doc, registry = reg, store = s2, executor = ex)
  expect_false(file.exists(file.path(s2$root, "stream", u2$key, "ckpt.rds")))
})
