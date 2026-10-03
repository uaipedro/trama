# Preview do `data/table`: perfil por coluna (tipo, NA, forma) medido numa
# amostra, e teto de colunas para tabela larga não inchar o JSON do card.

test_that("preview de data/table traz perfil por coluna", {
  df <- data.frame(x = c(1:9, NA), g = factor(rep(c("a", "a", "b", "c", "d"), 2)),
                   s = rep("z", 10), stringsAsFactors = FALSE)
  pv <- trama.data:::.tr_data_table_preview(df)
  p <- pv$data$perfil
  expect_equal(p$x$tipo, "num")
  expect_equal(p$x$na, 0.1)
  # 1..9 em 10 classes iguais de [1, 9]: o 5 cai na 6ª classe, a 5ª fica vazia.
  expect_equal(p$x$hist, c(1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L))
  expect_equal(sum(p$x$hist), 9L)
  expect_equal(p$g$tipo, "fct")
  expect_equal(p$g$niveis, 4L)
  expect_equal(p$g$top[[1]], list(nivel = "a", prop = 0.4))
  expect_length(p$g$top, 3L)
  expect_equal(p$s$top[[1]]$prop, 1)
  expect_false(pv$data$amostrado)
})

test_that("tabela larga e longa: colunas cortadas, perfil amostrado", {
  largo <- as.data.frame(matrix(1, nrow = 6000, ncol = 50))
  pv <- trama.data:::.tr_data_table_preview(largo)
  expect_length(pv$data$columns, 30L)
  expect_length(pv$data$perfil, 30L)
  expect_equal(pv$data$ncol, 50L)
  expect_equal(pv$data$nrow, 6000L)
  expect_true(pv$data$amostrado)
  expect_equal(pv$data$perfil$V1$hist[[1]], 5000L)
})
