# Oráculo: `geoR::xvalid`, leave-one-out, a referência do material de aula.
#
# O MODELO É FORÇADO dentro do objeto `variomodel` do geoR: o `xvalid` exige
# `model=`, e o que interessa é comparar a MESMA estrutura de covariância nos
# dois pacotes, não o ajuste de cada um. Isso também tira o teste da dependência
# do otimizador — a lição que o mantenedor consertou em 946b37f.
#
# Concordância medida em 2026-10-09 com `meuse`: predito 1,07e-14, variância
# 4,7e-16, e ME, RMSE e MSDR iguais em 10 casas decimais. Tolerância declarada
# 1e-6.

# O `xvalid` usa campos internos do objeto de modelo do geoR (`fix.pars` entre
# eles), e um `variomodel` montado à mão falha com
# `names(fix.pars) <- ...: 'names' attribute [5] must be the same length as the
# vector [0]`. Então o objeto nasce de um `variofit` DE VERDADE e os parâmetros
# do nosso ajuste são FORÇADOS dentro: o que se compara é a mesma estrutura de
# covariância nos dois pacotes, e o ajuste do geoR é descartado — é isso que
# tira o teste da dependência do otimizador.
variomodel_de <- function(m) {
  cm <- c(esferico = "spherical", exponencial = "exponential",
          gaussiano = "gaussian", matern = "matern")[[m$familia]]
  g <- geo_de(m$variograma$pontos)
  vo <- suppressMessages(geoR::variog(g, max.dist = m$variograma$dist_max,
                                      messages = FALSE))
  vf <- suppressWarnings(suppressMessages(geoR::variofit(
    vo, ini.cov.pars = c(m$contribuicao, m$alcance), cov.model = cm,
    nugget = m$pepita, fix.nugget = TRUE, weights = "npairs",
    kappa = if (is.na(m$kappa)) 0.5 else m$kappa,
    fix.kappa = TRUE, messages = FALSE)))
  vf$cov.pars <- c(m$contribuicao, m$alcance)
  vf$nugget <- m$pepita
  vf$kappa <- if (is.na(m$kappa)) 0.5 else m$kappa
  vf
}

geo_de <- function(p) {
  geoR::as.geodata(cbind(p$coords, p$dados[[p$variavel]]),
                   coords.col = 1:2, data.col = 3)
}

test_that("leave-one-out reproduz geoR::xvalid: predito, variância e as quatro métricas", {
  skip_if_not_installed("geoR")
  # Um `xvalid` só, e todas as asserções sobre ele: cada chamada é um laço em R
  # e custa ~15 s. O conjunto é o menor (68 pontos) — os 389 do milho_pr levam
  # minutos sem acrescentar informação, e a krigagem subjacente já está provada
  # nos dois conjuntos em test-krigagem-oraculo.R.
  p <- tr_spatial_example("milho_se")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "esferico")
  # Pepita > 0 importa: com pepita zero a variância no próprio ponto é zero e o
  # MSDR não existe.
  expect_gt(m$pepita, 0)
  nossa <- tr_spatial_validation(p, m, metodo = "leave-one-out")
  deles <- suppressMessages(geoR::xvalid(geo_de(p), model = variomodel_de(m),
                                         messages = FALSE))

  expect_equal(nossa$tabela$predito, deles$predicted, tolerance = 1e-6)
  expect_equal(nossa$tabela$variancia, deles$krige.var, tolerance = 1e-6)
  expect_equal(nossa$metricas$me, mean(deles$error), tolerance = 1e-6)
  expect_equal(nossa$metricas$rmse, sqrt(mean(deles$error^2)), tolerance = 1e-6)
  expect_equal(nossa$metricas$msdr, mean(deles$error^2 / deles$krige.var),
               tolerance = 1e-6)
  expect_equal(nossa$metricas$correlacao,
               stats::cor(deles$predicted, deles$data), tolerance = 1e-6)
  # O sinal do erro é o mesmo nos dois pacotes: observado - predito.
  expect_equal(deles$error, deles$data - deles$predicted, tolerance = 1e-9)
})

test_that("o oráculo vale também no Matérn, onde as parametrizações divergem", {
  skip_if_not_installed("geoR")
  p <- tr_spatial_example("milho_se")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "matern",
                                kappa = 1.5)
  nossa <- tr_spatial_validation(p, m)
  deles <- suppressMessages(geoR::xvalid(geo_de(p), model = variomodel_de(m),
                                         messages = FALSE))
  expect_equal(nossa$tabela$predito, deles$predicted, tolerance = 1e-6)
  expect_equal(nossa$tabela$variancia, deles$krige.var, tolerance = 1e-6)
})
