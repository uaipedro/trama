# Oráculo: `geoR::variog`, a referência do material de aula e dos livros-texto.
# Tolerância declarada: 1e-8 em gamma. A concordância real medida é ~1e-14
# (precisão de máquina); a folga cobre variação de plataforma. Falha nessa
# escala significa classes diferentes, não ruído numérico: não afrouxe.
#
# A comparação é feita nas MESMAS classes, passadas explicitamente aos dois
# pacotes, porque cada um escolhe classes padrão diferentes e comparar classes
# diferentes não testaria nada. Convenção que ficou:
#   gstat::variogram(boundaries = ) -> limites SUPERIORES, SEM o zero.
#   geoR::variog(breaks = )         -> limites COM o zero.
# Logo: lim <- seq(0, corte, length.out = n + 1); boundaries = lim[-1], breaks = lim.
#
# CONVENÇÃO DE DIREÇÃO: os dois pacotes medem o azimute igual, em sentido
# horário a partir do NORTE. Só a unidade difere:
#   gstat::variogram(alpha=) -> GRAUS.
#   geoR::variog(direction=) -> RADIANOS, e exige o valor em [0, pi).
# Logo: direction = alpha * pi / 180.
# O ângulo do teste NUNCA é 45: 45 é o ponto fixo de (90 - alpha), a conversão
# errada (anti-horário do Leste) que um dia foi dada como certa, então um teste
# a 45 graus não distingue a certa da errada e fica cego ao que deveria vigiar.
# Por isso 30 e 60, que as separam.

# Distâncias em metros: o corte vem dos dados.

geo <- function(p) {
  geoR::as.geodata(cbind(p$coords, p$dados[[p$variavel]]), coords.col = 1:2, data.col = 3)
}

test_that("o variograma clássico reproduz geoR::variog nas mesmas classes", {
  skip_if_not_installed("geoR")
  p <- tr_spatial_example("milho_pr")
  corte <- .tr_spatial_corte_padrao(p$coords)
  lim <- seq(0, corte, length.out = 11)
  v <- tr_spatial_variogram(p, dist_max = corte, n_classes = 10L, pares_min = 1L)
  o <- geoR::variog(geo(p), breaks = lim, estimator.type = "classical", messages = FALSE)
  expect_equal(as.integer(v$tabela$np), as.integer(o$n))
  expect_equal(v$tabela$gamma, o$v, tolerance = 1e-8)
})

test_that("o estimador robusto reproduz geoR::variog(estimator.type = 'modulus')", {
  skip_if_not_installed("geoR")
  p <- tr_spatial_example("milho_pr")
  corte <- .tr_spatial_corte_padrao(p$coords)
  lim <- seq(0, corte, length.out = 11)
  v <- tr_spatial_variogram(p, estimador = "robusto", dist_max = corte,
                            n_classes = 10L, pares_min = 1L)
  o <- geoR::variog(geo(p), breaks = lim, estimator.type = "modulus", messages = FALSE)
  expect_equal(as.integer(v$tabela$np), as.integer(o$n))
  expect_equal(v$tabela$gamma, o$v, tolerance = 1e-8)
})

test_that("o direcional reproduz geoR::variog, em 30 e 60 graus (nunca 45)", {
  skip_if_not_installed("geoR")
  p <- tr_spatial_example("milho_pr")
  corte <- .tr_spatial_corte_padrao(p$coords)
  lim <- seq(0, corte, length.out = 7)
  for (alpha in c(30, 60)) {         # graus, horário, do Norte (convenção do bloco)
    dir_geor <- alpha * pi / 180     # só a unidade muda
    v <- tr_spatial_variogram(p, dist_max = corte, n_classes = 6L, direcao = alpha,
                              tolerancia = 22.5, pares_min = 1L)
    o <- geoR::variog(geo(p), breaks = lim, direction = dir_geor,
                      tolerance = 22.5 * pi / 180, messages = FALSE)
    expect_equal(as.integer(v$tabela$np), as.integer(o$n), info = paste("alpha", alpha))
    expect_equal(v$tabela$gamma, o$v, tolerance = 1e-8, info = paste("alpha", alpha))
  }
})

test_that("a remoção de tendência reproduz geoR::variog(trend=)", {
  skip_if_not_installed("geoR")
  p <- tr_spatial_example("milho_se")
  corte <- .tr_spatial_corte_padrao(p$coords)
  lim <- seq(0, corte, length.out = 7)
  v <- tr_spatial_variogram(p, dist_max = corte, n_classes = 6L,
                            tendencia = "1a ordem", pares_min = 1L)
  o <- geoR::variog(geo(p), breaks = lim, trend = "1st", messages = FALSE)
  expect_equal(as.integer(v$tabela$np), as.integer(o$n))
  expect_equal(v$tabela$gamma, o$v, tolerance = 1e-8)
})

test_that("a tendência de 2a ordem reproduz geoR::variog(trend = '2nd')", {
  skip_if_not_installed("geoR")
  p <- tr_spatial_example("milho_se")
  corte <- .tr_spatial_corte_padrao(p$coords)
  lim <- seq(0, corte, length.out = 7)
  v <- tr_spatial_variogram(p, dist_max = corte, n_classes = 6L,
                            tendencia = "2a ordem", pares_min = 1L)
  o <- geoR::variog(geo(p), breaks = lim, trend = "2nd", messages = FALSE)
  expect_equal(as.integer(v$tabela$np), as.integer(o$n))
  expect_equal(v$tabela$gamma, o$v, tolerance = 1e-8)
})
