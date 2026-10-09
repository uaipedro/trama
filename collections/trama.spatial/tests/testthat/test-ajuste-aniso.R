# Anisotropia geométrica no modelo ajustado.
#
# Oráculo: `gstat::variogramLine(dir =)`, que é ARITMÉTICA do modelo e não
# ajuste — logo não depende de otimizador nenhum, que é a disciplina deste PR.
#
# Convenção conferida em 2026-10-09: `vgm(anis = c(60, 1/3))` guarda
# `ang1 = 60` e `anis1 = 0,3333`; a razão é maior eixo / menor eixo, e o ângulo
# aponta o eixo MAIOR. Medido: exponencial de alcance 300 com essa anisotropia
# tem alcance prático (95% do patamar) de 899 na direção 60 e de 300 na
# direção 150 — razão 3,00, a declarada.

alc_pratico_dir <- function(mv, graus, patamar, frac = 0.95) {
  dir <- c(sin(graus * pi / 180), cos(graus * pi / 180), 0)
  hs <- seq(1, 5000, by = 1)
  g <- gstat::variogramLine(mv, dist_vector = hs, dir = dir)$gamma
  hs[which(g >= frac * patamar)[1]]
}

test_that("razão e ângulo entram no modelo como o gstat os entende", {
  p <- tr_spatial_example("milho_se")
  v <- tr_spatial_variogram(p)
  m <- tr_spatial_variogram_fit(v, familia = "exponencial", razao = 3, angulo = 60)
  expect_equal(m$razao, 3)
  expect_equal(m$angulo, 60)
  mv <- .tr_spatial_vgm_model(m)
  expect_equal(as.numeric(mv$ang1[nrow(mv)]), 60)
  expect_equal(as.numeric(mv$anis1[nrow(mv)]), 1 / 3, tolerance = 1e-9)
})

test_that("o eixo maior fica na direção do ângulo, e a razão é a declarada", {
  # Aqui o modelo é montado À MÃO, com patamar e alcance conhecidos: o teste
  # mede a geometria da anisotropia, não a qualidade de um ajuste.
  mv <- gstat::vgm(1, "Exp", 300, 0, anis = c(60, 1 / 3))
  maior <- alc_pratico_dir(mv, 60, 1)
  menor <- alc_pratico_dir(mv, 150, 1)
  expect_gt(maior, menor)
  expect_equal(maior / menor, 3, tolerance = 1e-2)   # grade de 1 unidade
  expect_equal(menor, 300, tolerance = 2)
})

test_that("razao = 1 é no-op exato: o modelo e o gamma não mudam", {
  p <- tr_spatial_example("milho_se")
  v <- tr_spatial_variogram(p)
  a <- tr_spatial_variogram_fit(v, familia = "esferico")
  b <- tr_spatial_variogram_fit(v, familia = "esferico", razao = 1, angulo = 0)
  expect_equal(a$pepita, b$pepita)
  expect_equal(a$contribuicao, b$contribuicao)
  expect_equal(a$alcance, b$alcance)
  hs <- c(1, 50, 150, 300, 1e4)
  for (d in list(c(0, 1, 0), c(1, 0, 0), c(sin(pi / 3), cos(pi / 3), 0))) {
    expect_equal(
      gstat::variogramLine(.tr_spatial_vgm_model(a), dist_vector = hs, dir = d)$gamma,
      gstat::variogramLine(.tr_spatial_vgm_model(b), dist_vector = hs, dir = d)$gamma)
  }
})

test_that("razao menor que 1 é recusada, e a mensagem manda girar o ângulo", {
  p <- tr_spatial_example("milho_se")
  v <- tr_spatial_variogram(p)
  expect_error(tr_spatial_variogram_fit(v, razao = 0.5),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_variogram_fit(v, razao = 0.5), regexp = "90")
})

test_that("ângulo fora de [0, 180) é recusado", {
  p <- tr_spatial_example("milho_se")
  v <- tr_spatial_variogram(p)
  expect_error(tr_spatial_variogram_fit(v, razao = 2, angulo = 200),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_variogram_fit(v, razao = 2, angulo = -10),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_variogram_fit(v, razao = 2, angulo = 180),
               class = "tr_spatial_error_bad_option")
})

test_that("a krigagem usa a anisotropia do modelo: o mapa muda", {
  p <- tr_spatial_example("milho_se")
  v <- tr_spatial_variogram(p)
  m0 <- tr_spatial_variogram_fit(v, familia = "esferico")
  m1 <- tr_spatial_variogram_fit(v, familia = "esferico", razao = 4, angulo = 30)
  s0 <- tr_spatial_kriging(p, m0, resolucao = 20L)
  s1 <- tr_spatial_kriging(p, m1, resolucao = 20L)
  ok <- !is.na(s0$grade$predito) & !is.na(s1$grade$predito)
  expect_gt(sum(ok), 0)
  expect_gt(max(abs(s0$grade$predito[ok] - s1$grade$predito[ok])), 0)
})

test_that("a nota do modelo diz a anisotropia quando ela existe, e cala quando não", {
  p <- tr_spatial_example("milho_se")
  v <- tr_spatial_variogram(p)
  com <- tr_spatial_variogram_fit(v, razao = 3, angulo = 45)
  sem <- tr_spatial_variogram_fit(v)
  expect_match(com$nota, "[Aa]nisotropia")
  expect_false(grepl("[Aa]nisotropia", sem$nota))
})

test_that("o tipo spatial/model guarda os dois campos novos", {
  p <- tr_spatial_example("milho_se")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), razao = 2, angulo = 10)
  tipo <- spatial_model_type()
  f <- tempfile(fileext = ".rds")
  tipo$store(m, f)
  volta <- tipo$restore(f)
  expect_equal(volta$razao, 2)
  expect_equal(volta$angulo, 10)
})
