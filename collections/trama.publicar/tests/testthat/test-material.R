# O material (o valor do nó): metadados, conferências e layout.

test_that("autores com ORCID conferido pelo dígito verificador", {
  a <- .tr_publicar_autores("Josiah Carberry (0000-0002-1825-0097); Ana Lima; Zé (0000-0002-1825-0098)")
  expect_length(a, 3L)
  expect_identical(a[[1]]$nome, "Josiah Carberry")
  expect_true(a[[1]]$orcid_valido)   # o ORCID de exemplo da documentação do ORCID
  expect_null(a[[2]]$orcid)
  expect_false(a[[3]]$orcid_valido)
  expect_true(.tr_publicar_orcid_ok("0000-0001-5109-3700"))
  expect_true(.tr_publicar_orcid_ok("0000-0002-1694-233X"))
})

test_that("próxima versão a partir da última", {
  expect_identical(.tr_publicar_proxima("menor"), "0.1.0")
  expect_identical(.tr_publicar_proxima("correção", "1.2.3"), "1.2.4")
  expect_identical(.tr_publicar_proxima("maior", "1.2.3"), "2.0.0")
})

test_that("layout vazio é o automático; o explícito cita os itens pelo nó", {
  a <- tr_publicar_layout("", c("a", "b"), "a")
  expect_identical(vapply(a$blocos, function(b) b$tipo, ""), c("cabecalho", "item", "fluxo"))
  e <- tr_publicar_layout('[{"tipo":"texto","texto":"## Oi"},{"tipo":"item","id":"b"},{"tipo":"item","id":"z"}]',
                          c("a", "b"), c("a", "b"))
  expect_identical(vapply(e$blocos, function(b) b$tipo, ""), c("texto", "item"))
  expect_match(e$avisos, "'z'", all = FALSE)
  expect_match(e$avisos, "só no pacote\\): a", all = FALSE)
  expect_error(tr_publicar_layout("[{", "a"), class = "tr_publicar_error_bad_layout")
  expect_error(tr_publicar_layout('[{"tipo":"video"}]', "a"), class = "tr_publicar_error_bad_layout")
})

test_that("faltando título ou autor, ORCID errado e item com erro viram erros", {
  falso <- tr_publicar_item("modelo", "M", erro = "sem strip")
  m <- tr_publicar_material(list(x = falso), autores = "Zé (0000-0002-1825-0098)")
  expect_match(m$erros, "título", all = FALSE)
  expect_match(m$erros, "ORCID", all = FALSE)
  expect_match(m$erros, "sem strip", all = FALSE)
  expect_length(m$itens, 0L)
  expect_identical(m$recusados, "x")
})

test_that("material completo: sem erros, layout automático, ambiente", {
  m <- material_exemplo()
  expect_length(m$erros, 0L)
  expect_identical(names(m$itens), c("tab", "fig", "mod"))
  expect_identical(m$metadados$palavras_chave, c("carros", "consumo"))
  expect_identical(m$ambiente$r, paste(R.version$major, R.version$minor, sep = "."))
  conf <- .tr_publicar_conferencia(m)
  expect_true(all(vapply(conf$itens, function(i) i$na_pagina, TRUE)))
})

test_that("pelo motor: a porta nomeia os itens pelo nó e o fio converte tabela e modelo", {
  ex <- doc_exemplo()
  v <- trama::tr_value(ex$doc, "pub", registry = ex$reg, store = trama::tr_store(tempfile()))
  expect_s3_class(v, "tr_publicar_material")
  expect_identical(names(v$itens), c("tab", "mod"))
  expect_identical(v$itens$mod$tipo, "modelo")
  expect_identical(v$itens$tab$tipo, "tabela")
})

test_that("item com dados de treino é recusado por padrão e aceito com o param", {
  marcado <- tr_publicar_item("modelo", "SVM", arquivos = list(modelo.rds = as.raw(1:3)),
                              dados_treino = "A SVM guarda os 7 vetores de suporte.")
  recusa <- tr_publicar_material(list(m = marcado), titulo = "T", autores = "Ana Lima")
  expect_length(recusa$itens, 0L)
  expect_identical(recusa$recusados, "m")
  expect_match(recusa$erros, "carrega linhas do treino", all = FALSE)
  expect_match(recusa$erros, "Aceito publicar dados de treino embutidos", all = FALSE)
  aceito <- tr_publicar_material(list(m = marcado), titulo = "T", autores = "Ana Lima",
                                 aceitar_dados_treino = TRUE)
  expect_identical(names(aceito$itens), "m")
  expect_length(aceito$erros, 0L)
  expect_match(aceito$avisos, "dados de treino embutidos", all = FALSE)
  expect_true(aceito$metadados$aceitar_dados_treino)
})

test_that("pelo motor: SVM recusada sem o param e publicada com ele", {
  skip_if_not_installed("trama.ml")
  skip_if_not_installed("e1071")
  reg <- publicar_registry()
  trama::tr_use("trama.ml", registry = reg)
  f <- trama::tr_flow(reg) |>
    trama::tr_add("tab", "tst/tabela", n = 32L) |>
    trama::tr_add("svm", "ml/svm", resposta = "mpg", preditores = "wt, hp", from = "tab") |>
    trama::tr_add("pub", "publicar/publish", titulo = "T", autores = "Ana Lima", from = "svm")
  s <- trama::tr_store(tempfile())
  v <- trama::tr_value(trama::tr_flow_doc(f), "pub", registry = reg, store = s)
  expect_identical(v$recusados, "svm")
  f2 <- trama::tr_set(f, "pub", aceitar_dados_treino = TRUE)
  v2 <- trama::tr_value(trama::tr_flow_doc(f2), "pub", registry = reg, store = s)
  expect_identical(names(v2$itens), "svm")
})
