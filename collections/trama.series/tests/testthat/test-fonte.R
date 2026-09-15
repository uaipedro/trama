test_that("example devolve as séries do datasets, e mts vira uma opção por coluna", {
  expect_identical(tr_series_example(), datasets::AirPassengers)
  dax <- tr_series_example("EuStockMarkets$DAX")
  expect_true(stats::is.ts(dax))
  expect_null(dim(dax))
  expect_equal(stats::frequency(dax), 260)
  ofertas <- .tr_series_exemplos()
  expect_true(all(c("Nile", "AirPassengers", "EuStockMarkets$DAX") %in% ofertas))
  # O que o `data/example` oferta não entra aqui: não é série.
  expect_false(any(c("iris", "mtcars", "EuStockMarkets") %in% ofertas))
})

test_that("example recusa nome que não é série, listando as aceitas", {
  for (x in list("iris", "Nilo", "EuStockMarkets", "EuStockMarkets$XYZ", NA, 1)) {
    err <- tryCatch(tr_series_example(x), condition = identity)
    expect_s3_class(err, "tr_series_error_bad_option")
  }
  expect_match(conditionMessage(tryCatch(tr_series_example("Nilo"), condition = identity)),
               "Nile", fixed = TRUE)
})

test_that("from_table ordena pelo tempo e tira o início das datas", {
  d <- tabela_mensal()[sample(36), ]
  x <- tr_series_from_table(d, valor = "vendas", tempo = "mes")
  expect_equal(stats::start(x), c(2020, 1))
  expect_equal(stats::frequency(x), 12)
  expect_equal(as.numeric(x), tabela_mensal()$vendas)
})

test_that("from_table ida e volta pela tabela preserva datas e valores", {
  d <- tabela_mensal()
  volta <- .tr_series_tabela(tr_series_from_table(d, valor = "vendas", tempo = "mes"))
  expect_equal(volta$tempo, d$mes)
  expect_equal(volta$valor, d$vendas)
})

test_that("from_table recusa buraco na grade, dizendo onde", {
  d <- tabela_mensal()[-5, ]
  err <- tryCatch(tr_series_from_table(d, "vendas", "mes"), condition = identity)
  expect_s3_class(err, "tr_series_error_gap")
  expect_match(conditionMessage(err), "2020-04-01")
  expect_match(conditionMessage(err), "2020-06-01")
  anos <- tibble::tibble(ano = c(2000L, 2001L, 2003L), v = 1:3)
  expect_error(tr_series_from_table(anos, "v", "ano", frequencia = 1L), class = "tr_series_error_gap")
})

test_that("from_table recusa tempo repetido, inclusive dois dias no mesmo mês", {
  d <- rbind(tabela_mensal(), tabela_mensal()[3, ])
  expect_error(tr_series_from_table(d, "vendas", "mes"), class = "tr_series_error_duplicate_time")
  d2 <- tabela_mensal(); d2$mes[[2]] <- as.Date("2020-01-15")
  expect_error(tr_series_from_table(d2, "vendas", "mes"), class = "tr_series_error_duplicate_time")
})

test_that("from_table: campos em branco, coluna errada, texto no valor", {
  d <- tabela_mensal()
  expect_error(tr_series_from_table(d, valor = ""), class = "tr_series_error_blank_param")
  expect_error(tr_series_from_table(d, valor = "vendsa"), class = "tr_series_error_unknown_column")
  expect_error(tr_series_from_table(d, "vendas", tempo = "dia"), class = "tr_series_error_unknown_column")
  d$texto <- as.character(d$vendas)
  expect_error(tr_series_from_table(d, "texto"), class = "tr_series_error_not_numeric")
  expect_error(tr_series_from_table(d, "vendas", frequencia = 0L), class = "tr_series_error_bad_option")
})

test_that("from_table: início digitado, e recusado se a data já diz", {
  d <- tabela_mensal()
  x <- tr_series_from_table(d, "vendas", inicio = "2019, 7")
  expect_equal(stats::start(x), c(2019, 7))
  expect_error(tr_series_from_table(d, "vendas", inicio = "2019, 13"), class = "tr_series_error_bad_period")
  expect_error(tr_series_from_table(d, "vendas", inicio = "julho"), class = "tr_series_error_bad_period")
  expect_error(tr_series_from_table(d, "vendas", tempo = "mes", inicio = "2019, 7"),
               class = "tr_series_error_bad_period")
})

test_that("from_table: trimestral por data e anual por inteiro", {
  tri <- tibble::tibble(t = seq(as.Date("2001-04-01"), by = "quarter", length.out = 8), v = 1:8)
  x <- tr_series_from_table(tri, "v", "t", frequencia = 4L)
  expect_equal(stats::start(x), c(2001, 2))
  anos <- tibble::tibble(ano = 1990:1999, v = 1:10)
  y <- tr_series_from_table(anos, "v", "ano", frequencia = 1L)
  expect_equal(stats::start(y), c(1990, 1))
  expect_equal(.tr_series_tabela(y)$tempo, 1990:1999)
})
