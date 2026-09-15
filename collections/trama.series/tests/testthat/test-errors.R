test_that("coluna inexistente é erro alto, com as disponíveis na mensagem", {
  d <- tabela_mensal()
  err <- tryCatch(.tr_series_col(d, "vendsa", "valor"), condition = identity)
  expect_s3_class(err, "tr_series_error_unknown_column")
  expect_match(conditionMessage(err), "vendsa")
  expect_match(conditionMessage(err), "mes, vendas")
})

test_that("frequência 1 onde precisa de sazonal é erro nomeando a frequência", {
  err <- tryCatch(.tr_series_sazonal(serie_anual(), "series/decompose"), condition = identity)
  expect_s3_class(err, "tr_series_error_no_season")
  expect_match(conditionMessage(err), "frequência 1")
  err <- tryCatch(.tr_series_sazonal(stats::ts(1:13, frequency = 12), "x"), condition = identity)
  expect_s3_class(err, "tr_series_error_too_short")
})

test_that("inteiro fora da faixa é erro classificado", {
  expect_error(.tr_series_int(0, "ordem", min = 1), class = "tr_series_error_bad_option")
  expect_error(.tr_series_int(1.5, "ordem", min = 1), class = "tr_series_error_bad_option")
  expect_equal(.tr_series_int(2, "ordem", min = 1), 2L)
})

test_that("toda classe tr_series_error_* usada no código está em tr_series_errors()", {
  # Varre o NAMESPACE, e não os `.R` do disco: sob `R CMD check` o cwd muda e
  # a varredura pelo disco pularia em silêncio — mesma lição das irmãs.
  ns <- asNamespace("trama.series")
  objs <- mget(ls(ns, all.names = TRUE), envir = ns, inherits = FALSE)
  txt <- unlist(lapply(Filter(is.function, objs),
                       function(f) deparse(f, width.cutoff = 500L)))
  used <- unique(unlist(regmatches(
    txt, gregexpr('(?<=")tr_series_error_[a-z_]+(?=")', txt, perl = TRUE))))
  documented <- tr_series_errors()$class
  expect_setequal(setdiff(used, documented), character())
  expect_setequal(setdiff(documented, used), character())
})
