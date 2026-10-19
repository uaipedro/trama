# data/describe (R/descrever.R).
#
# Oráculos e tolerâncias:
# - assimetria e curtose: `moments::skewness` (g1) e `moments::kurtosis` - 3
#   (g2, em excesso), Komsta & Novomestky; os dois com divisor n: são o g1 e o
#   g2 de Joanes & Gill (1998), e não o b1/b2 nem o G1/G2 corrigidos.
#   Tolerância 1e-12 (mesma conta, ordem de soma diferente).
# - n, média, dp, mediana e extremos: `stats` direto, 1e-12.
# - Recálculo à mão de g1 e g2 pelas somas, 1e-12, para não depender só do
#   `moments` (que pode não estar instalado: o teste dele é pulado).

test_that("descritivas de iris: momentos iguais ao moments e ao cálculo à mão (1e-12)", {
  d <- datasets::iris
  out <- tr_describe(d)
  expect_identical(out$coluna, names(d)[1:4])
  expect_identical(names(out), c("coluna", "n", "faltantes", "media", "dp", "mediana",
                                 "minimo", "maximo", "assimetria", "curtose_excesso"))
  for (i in seq_len(4)) {
    x <- d[[i]]
    n <- length(x)
    expect_identical(out$n[[i]], n)
    expect_equal(out$media[[i]], mean(x), tolerance = 1e-12)
    expect_equal(out$dp[[i]], stats::sd(x), tolerance = 1e-12)
    expect_equal(out$mediana[[i]], stats::median(x), tolerance = 1e-12)
    expect_equal(c(out$minimo[[i]], out$maximo[[i]]), range(x), tolerance = 1e-12)
    m2 <- sum((x - mean(x))^2) / n; m3 <- sum((x - mean(x))^3) / n; m4 <- sum((x - mean(x))^4) / n
    expect_equal(out$assimetria[[i]], m3 / m2^1.5, tolerance = 1e-12)
    expect_equal(out$curtose_excesso[[i]], m4 / m2^2 - 3, tolerance = 1e-12)
  }
  skip_if_not_installed("moments")
  for (i in seq_len(4)) {
    expect_equal(out$assimetria[[i]], moments::skewness(d[[i]]), tolerance = 1e-12)
    expect_equal(out$curtose_excesso[[i]], moments::kurtosis(d[[i]]) - 3, tolerance = 1e-12)
  }
})

test_that("faltantes ficam fora das contas e são contados; constante sai sem forma", {
  d <- data.frame(a = c(1, 2, NA, 4, 10), b = 5, txt = letters[1:5])
  out <- tr_describe(d)
  expect_identical(out$coluna, c("a", "b"))
  expect_identical(out$n, c(4L, 5L))
  expect_identical(out$faltantes, c(1L, 0L))
  expect_equal(out$media[[1]], mean(c(1, 2, 4, 10)))
  expect_equal(out$dp[[2]], 0)
  expect_true(is.na(out$assimetria[[2]]) && is.na(out$curtose_excesso[[2]]))
})

test_that("em branco, a coluna de índice (trama_indice) não entra; citada, entra", {
  d <- data.frame(tempo = seq(2000, by = 1 / 52, length.out = 10), y = 1:10)
  expect_identical(tr_describe(d)$coluna, c("tempo", "y"))   # tabela comum: tudo
  attr(d, "trama_indice") <- "tempo"
  expect_identical(tr_describe(d)$coluna, "y")
  expect_identical(tr_describe(d, cols = "tempo")$coluna, "tempo")
})

test_that("coluna citada inexistente ou não numérica, e tabela sem numérica, são erro classificado", {
  d <- data.frame(a = 1:3, txt = c("x", "y", "z"))
  expect_error(tr_describe(d, cols = "zz"), class = "tr_data_error_unknown_column")
  e <- tryCatch(tr_describe(d, cols = "a, txt"), error = identity)
  expect_s3_class(e, "tr_data_error_not_numeric")
  expect_match(conditionMessage(e), "txt", fixed = TRUE)
  expect_error(tr_describe(d["txt"]), class = "tr_data_error_not_numeric")
})

test_that("data/describe roda pelo registro", {
  reg <- data_registry()
  fl <- trama::tr_flow(reg) |>
    trama::tr_add("flor", "data/example", dataset = "iris") |>
    trama::tr_add("desc", "data/describe", cols = "Sepal.Length, Petal.Length", from = "flor")
  out <- trama::tr_value(trama::tr_flow_doc(fl), "desc", registry = fl$registry, store = trama::tr_store(tempfile()))
  expect_identical(out$coluna, c("Sepal.Length", "Petal.Length"))
})
