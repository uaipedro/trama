# Preview do `data/table`: só as linhas, sem perfil por coluna (a forma das
# colunas mora no `data/summary`), e teto de colunas para tabela larga não
# inchar o JSON do card.

test_that("preview de data/table não traz perfil", {
  df <- data.frame(x = c(1:9, NA), g = factor(rep(c("a", "b"), 5)))
  pv <- trama.data:::.tr_data_table_preview(df)
  expect_null(pv$data$perfil)
  expect_null(pv$data$amostrado)
  expect_equal(unlist(pv$data$columns), c("x", "g"))
  expect_length(pv$data$rows, 10L)
})

test_that("tabela larga: colunas cortadas no teto, dimensões inteiras", {
  largo <- as.data.frame(matrix(1, nrow = 6000, ncol = 50))
  pv <- trama.data:::.tr_data_table_preview(largo)
  expect_length(pv$data$columns, 30L)
  expect_length(pv$data$rows, 25L)
  expect_equal(pv$data$ncol, 50L)
  expect_equal(pv$data$nrow, 6000L)
})
