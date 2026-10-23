# Oráculo: `geoR::variog4`, a referência do material de aula para diagnosticar
# anisotropia.
#
# Convenções, as mesmas que o PR 1 fixou em `test-variograma-geor.R`:
#   DIREÇÃO — os dois pacotes medem o azimute em sentido horário a partir do
#     NORTE; só a unidade muda (gstat em graus, geoR em radianos). A conversão é
#     `alpha * pi / 180`, e vive aqui, nunca no código do bloco.
#   CLASSES — `boundaries` SEM o zero no gstat, `breaks` COM o zero no geoR.
#
# Concordância medida em 2026-10-09 (gstat 2.1.6, geoR 1.9.6), com `meuse`:
# 5,6e-16 em gamma nas quatro direções, com os pares IDÊNTICOS. Tolerância
# declarada 1e-8; falha nessa escala significa classe ou convenção diferente,
# não ruído numérico, e não se afrouxa.
#
# UM dos testes usa conjunto ASSIMÉTRICO de direções, de propósito:
# {0,45,90,135} é invariante sob alpha -> 90 - alpha (0 vira 90, 45 vira 45,
# 135 vira 135), então o CONJUNTO de curvas não distingue a convenção certa da
# refletida — medido em 2026-10-09: conjuntos simétricos invariantes,
# assimétricos não. Comparar por RÓTULO de direção é o que fecha essa brecha, e
# o conjunto assimétrico a fecha de novo por outro caminho.

# CORTE SEM EMPATE. O `gstat` inclui o limite superior da classe e o `geoR`
# empurra o par para a seguinte: um par cuja distância cai EXATAMENTE num limite
# desloca gamma em ~3e-4, muito acima da tolerância de 1e-8 — medido em
# 2026-10-09. Os oráculos do PR 1 passam porque o corte padrão dos três
# conjuntos não calha de empatar, o que é sorte, não desenho. Estes escolhem o
# corte de propósito e ASSEREM a premissa.
corte_sem_empate <- function(p, n_classes) {
  d <- as.numeric(stats::dist(p$coords))
  base <- .tr_spatial_corte_padrao(p$coords)
  for (f in c(1, 0.998123, 0.996571, 0.994417, 0.991193)) {
    corte <- base * f
    lim <- seq(0, corte, length.out = n_classes + 1L)
    if (!any(d %in% lim)) return(list(corte = corte, lim = lim))
  }
  stop("nenhum corte sem empate encontrado")
}

geo_aniso <- function(p) {
  geoR::as.geodata(cbind(p$coords, p$dados[[p$variavel]]),
                   coords.col = 1:2, data.col = 3)
}

test_that("as curvas direcionais reproduzem geoR::variog4, direção por direção", {
  skip_if_not_installed("geoR")
  p <- tr_spatial_example("milho_pr")
  ce <- corte_sem_empate(p, 10L); corte <- ce$corte; lim <- ce$lim
  expect_equal(sum(as.numeric(stats::dist(p$coords)) %in% lim), 0L)
  a <- tr_spatial_anisotropy(p, direcoes = "0,45,90,135", dist_max = corte,
                             n_classes = 10L, pares_min = 1L)
  o <- suppressMessages(geoR::variog4(
    geo_aniso(p), breaks = lim, max.dist = corte,
    direction = c(0, 45, 90, 135) * pi / 180, tolerance = 22.5 * pi / 180,
    messages = FALSE))
  for (d in c(0, 45, 90, 135)) {
    nossa <- a$tabela[a$tabela$direcao == d, ]
    deles <- o[[as.character(d)]]
    ok <- deles$n > 0
    expect_equal(as.integer(nossa$np), as.integer(deles$n[ok]),
                 info = paste("direção", d))
    expect_equal(nossa$gamma, deles$v[ok], tolerance = 1e-8,
                 info = paste("direção", d))
  }
})

test_that("um conjunto ASSIMÉTRICO de direções pinga a convenção de azimute", {
  skip_if_not_installed("geoR")
  p <- tr_spatial_example("milho_se")
  ce <- corte_sem_empate(p, 6L); corte <- ce$corte; lim <- ce$lim
  expect_equal(sum(as.numeric(stats::dist(p$coords)) %in% lim), 0L)
  dirs <- c(0, 30, 60, 120)            # NÃO é invariante sob 90 - alpha
  a <- tr_spatial_anisotropy(p, direcoes = "0,30,60,120", dist_max = corte,
                             n_classes = 6L, pares_min = 1L)
  for (d in dirs) {
    deles <- suppressMessages(geoR::variog(
      geo_aniso(p), breaks = lim, direction = d * pi / 180,
      tolerance = 22.5 * pi / 180, messages = FALSE))
    nossa <- a$tabela[a$tabela$direcao == d, ]
    ok <- deles$n > 0
    expect_equal(as.integer(nossa$np), as.integer(deles$n[ok]),
                 info = paste("direção", d))
    expect_equal(nossa$gamma, deles$v[ok], tolerance = 1e-8,
                 info = paste("direção", d))
  }
})

test_that("o robusto direcional também reproduz o geoR", {
  skip_if_not_installed("geoR")
  p <- tr_spatial_example("milho_se")
  ce <- corte_sem_empate(p, 6L); corte <- ce$corte; lim <- ce$lim
  expect_equal(sum(as.numeric(stats::dist(p$coords)) %in% lim), 0L)
  a <- tr_spatial_anisotropy(p, direcoes = "0,90", estimador = "robusto",
                             dist_max = corte, n_classes = 6L, pares_min = 1L)
  for (d in c(0, 90)) {
    deles <- suppressMessages(geoR::variog(
      geo_aniso(p), breaks = lim, direction = d * pi / 180,
      tolerance = 22.5 * pi / 180, estimator.type = "modulus", messages = FALSE))
    nossa <- a$tabela[a$tabela$direcao == d, ]
    ok <- deles$n > 0
    expect_equal(nossa$gamma, deles$v[ok], tolerance = 1e-8,
                 info = paste("direção", d))
  }
})

test_that("a tendência removida no direcional reproduz o geoR", {
  skip_if_not_installed("geoR")
  p <- tr_spatial_example("milho_se")
  ce <- corte_sem_empate(p, 6L); corte <- ce$corte; lim <- ce$lim
  expect_equal(sum(as.numeric(stats::dist(p$coords)) %in% lim), 0L)
  a <- tr_spatial_anisotropy(p, direcoes = "0,90", tendencia = "1a ordem",
                             dist_max = corte, n_classes = 6L, pares_min = 1L)
  for (d in c(0, 90)) {
    deles <- suppressMessages(geoR::variog(
      geo_aniso(p), breaks = lim, direction = d * pi / 180,
      tolerance = 22.5 * pi / 180, trend = "1st", messages = FALSE))
    nossa <- a$tabela[a$tabela$direcao == d, ]
    ok <- deles$n > 0
    expect_equal(nossa$gamma, deles$v[ok], tolerance = 1e-8,
                 info = paste("direção", d))
  }
})
