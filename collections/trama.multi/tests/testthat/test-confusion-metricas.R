# Métricas da matriz de confusão: acurácia, acurácia balanceada (Brodersen et
# al. 2010), kappa de Cohen (1960) e precisão/revocação/F1 por grupo (Sokolova
# & Lapalme 2009). Oráculos: contas à mão numa matriz pequena, o exemplo do
# kappa da Wikipédia (20/5/10/15, κ = 0,4) e `irr::kappa2` / `psych::cohen.kappa`.

test_that("a partir de uma matriz: contas à mão", {
  # real (linhas) × previsto (colunas)
  m <- matrix(c(20, 10, 5, 15), 2, dimnames = list(c("sim", "nao"), c("sim", "nao")))
  r <- .tr_multi_metricas(m)
  v <- function(med, g = NA) r$valor[r$medida == med & (is.na(g) & is.na(r$grupo) | r$grupo %in% g)]
  expect_equal(v("acurácia"), 35 / 50)
  expect_equal(v("kappa de Cohen"), 0.4, tolerance = 1e-12)
  expect_equal(v("revocação", "sim"), 20 / 25)
  expect_equal(v("precisão", "sim"), 20 / 30)
  expect_equal(v("F1", "sim"), 2 * (20 / 30) * (20 / 25) / (20 / 30 + 20 / 25))
  expect_equal(v("revocação", "nao"), 15 / 25)
  expect_equal(v("acurácia balanceada"), (20 / 25 + 15 / 25) / 2)
})

test_that("grupo nunca previsto: precisão e F1 são NA, e não zero", {
  m <- matrix(c(5, 3, 0, 0), 2, dimnames = list(c("a", "b"), c("a", "b")))
  r <- .tr_multi_metricas(m)
  expect_true(is.na(r$valor[r$medida == "precisão" & r$grupo %in% "b"]))
  expect_true(is.na(r$valor[r$medida == "F1" & r$grupo %in% "b"]))
  expect_equal(r$valor[r$medida == "revocação" & r$grupo %in% "b"], 0)
  expect_equal(r$valor[r$medida == "acurácia balanceada"], 0.5)
  expect_equal(r$valor[r$medida == "kappa de Cohen"], 0)
})

test_that("kappa bate com irr e psych nas previsões do iris (3 grupos)", {
  l <- tr_multi_discriminant(iris, grupo = "Species")
  pr <- .tr_multi_prever(l, "cruzada", "multi/confusion")
  r <- tr_multi_confusion(l, tabela = "métricas")
  k <- r$valor[r$medida == "kappa de Cohen"]
  skip_if_not_installed("irr")
  expect_equal(k, irr::kappa2(data.frame(pr$g, pr$classe))$value, tolerance = 1e-12)
  skip_if_not_installed("psych")
  expect_equal(k, suppressWarnings(psych::cohen.kappa(cbind(as.integer(pr$g), as.integer(pr$classe))))$kappa,
               tolerance = 1e-12)
  expect_equal(r$valor[r$medida == "acurácia"], 147 / 150)
  expect_equal(r$valor[r$medida == "acurácia balanceada"], (1 + .96 + .98) / 3)
})

test_that("a tabela padrão continua a matriz, e a opção é conferida", {
  l <- tr_multi_discriminant(iris, grupo = "Species")
  expect_equal(names(tr_multi_confusion(l))[1], "real")
  r <- tr_multi_confusion(l, tabela = "métricas")
  expect_equal(names(r), c("medida", "grupo", "valor"))
  expect_error(tr_multi_confusion(l, tabela = "tudo"), class = "tr_multi_error_bad_option")
})
