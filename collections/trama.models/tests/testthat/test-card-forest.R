# Mini-forest do card de ajuste: os pontos são os do quadro de coeficientes
# (mesma estimativa e mesmo IC de `confint.lm`), sem o intercepto.

test_that("card do lm traz forest igual a coef/confint", {
  fit <- tr_models_lm(mtcars, formula = "mpg ~ wt + hp")
  d <- trama.models:::.tr_models_fit_preview(fit)
  expect_false(is.null(d$forest))
  expect_equal(d$forest$nivel, "95")
  termos <- vapply(d$forest$pontos, `[[`, "", "termo")
  expect_equal(termos, c("wt", "hp"))
  ref <- stats::confint(stats::lm(mpg ~ wt + hp, mtcars))
  expect_equal(d$forest$pontos[[1]]$est, unname(stats::coef(stats::lm(mpg ~ wt + hp, mtcars))[["wt"]]),
               tolerance = 1e-10)
  expect_equal(d$forest$pontos[[1]]$li, ref["wt", 1], tolerance = 1e-10)
  expect_equal(d$forest$pontos[[2]]$ls, ref["hp", 2], tolerance = 1e-10)
})

test_that("quadro sem intervalo não gera forest", {
  ef <- trama.models:::.tr_models_efeitos(
    data.frame(termo = "trat", p_valor = 0.01), "Quadro da ANOVA")
  expect_null(trama.models:::.tr_models_forest(ef))
  expect_null(trama.models:::.tr_models_forest(NULL))
})
