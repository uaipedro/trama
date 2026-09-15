test_that("coluna inexistente é erro alto, com as disponíveis na mensagem", {
  d <- df_exemplo()
  expect_error(.tr_data_cols(d, c("valor", "regaio"), "cols"),
               class = "tr_data_error_unknown_column")
  err <- tryCatch(.tr_data_cols(d, "regaio", "cols"), condition = identity)
  expect_match(conditionMessage(err), "regaio")
  expect_match(conditionMessage(err), "regiao, produto, valor, qtd")
  expect_equal(.tr_data_cols(d, c("valor", "qtd"), "cols"), c("valor", "qtd"))
})

test_that("expressão que não faz parse é erro classificado, com o texto ecoado", {
  err <- tryCatch(.tr_data_parse("valor >", "expr"), condition = identity)
  expect_s3_class(err, "tr_data_error_bad_expr")
  expect_match(conditionMessage(err), "expr")
  expect_match(conditionMessage(err), "valor >")
  expect_equal(.tr_data_parse("valor > 1", "expr"), quote(valor > 1))
})

test_that("erro na AVALIAÇÃO vira mensagem com as colunas disponíveis", {
  d <- df_exemplo()
  err <- tryCatch(.tr_data_eval(stop("objeto 'valro' não encontrado"), "expr", d),
                  condition = identity)
  expect_s3_class(err, "tr_data_error_eval")
  expect_match(conditionMessage(err), "regiao, produto, valor, qtd")
})

test_that("toda classe tr_data_error_* usada no código está em tr_data_errors()", {
  # Varre o NAMESPACE, não os `.R` do disco: a versão antiga pulava — em
  # silêncio — sob `R CMD check`, onde o cwd é `<pkg>.Rcheck/tests/testthat/` e
  # `../../R` não existe. O corpo das funções carregadas tem a mesma
  # informação, e agora o catálogo é conferido também no ambiente real.
  ns <- asNamespace("trama.data")
  objs <- mget(ls(ns, all.names = TRUE), envir = ns, inherits = FALSE)
  txt <- unlist(lapply(Filter(is.function, objs),
                       function(f) deparse(f, width.cutoff = 500L)))
  # Só ocorrência ENTRE ASPAS conta como uso — o grep pelo nome nu também
  # varria o próprio catálogo, onde as classes são nomes do `c()`
  # (`tr_data_error_x = "..."`), e a direção "nada morto" nunca podia falhar.
  used <- unique(unlist(regmatches(
    txt, gregexpr('(?<=")tr_data_error_[a-z_]+(?=")', txt, perl = TRUE))))
  documented <- tr_data_errors()$class
  expect_setequal(setdiff(used, documented), character())
  expect_setequal(setdiff(documented, used), character())
})
