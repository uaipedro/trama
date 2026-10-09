test_that("o plano vai ao relatório com delineamento, termos e unidades", {
  p <- tr_experiments_design(estrutura = "fatorial", fatores = "adubo: N0, N1; variedade: A, B",
                             repeticoes = 3L, .seed = 1L)
  p <- tr_experiments_effect(p, valor = 50, nome = "mu", .seed = 1L)
  p <- tr_experiments_error(p, resposta = "y", sd = 1, .seed = 1L)
  r <- tr_experiments_report_plan(p)
  expect_match(r, "| resposta | y (normal) |", fixed = TRUE)
  expect_match(r, "**Termos simulados**", fixed = TRUE)
  expect_no_match(r, ".ef_", fixed = TRUE)
})
