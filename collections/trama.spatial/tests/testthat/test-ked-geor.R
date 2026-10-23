# A deriva externa (KED): krigagem universal com tendência por covariável.
#
# A covariável tem de ser conhecida em TODA célula onde se prediz, e a grade vem
# do usuário, pela porta `grade`. Nada é inventado: interpolar a covariável por
# dentro subestimaria o erro-padrão do mapa EM SILÊNCIO, porque o erro dessa
# interpolação não se propaga para a variância de krigagem.
#
# Oráculo: `geoR::krige.conv` com `trend.d`/`trend.l`. Concordância medida em
# 2026-10-09 com `meuse`: 8,9e-16 no predito e 3,3e-16 na variância. Tolerância
# declarada 1e-6.

grade_com_cov <- function(p, n = 4L) {
  g <- expand.grid(
    x = seq(min(p$coords[, 1]), max(p$coords[, 1]), length.out = n),
    y = seq(min(p$coords[, 2]), max(p$coords[, 2]), length.out = n))
  names(g) <- p$coord_cols
  # valores arbitrários mas FINITOS e variáveis: o que se testa é o caminho da
  # deriva, não a qualidade da covariável.
  g[[p$covariaveis[[1]]]] <- seq_len(nrow(g)) * 10
  g
}

test_that("o KED reproduz geoR::krige.conv com trend.d e trend.l", {
  skip_if_not_installed("geoR")
  p <- tr_spatial_example("milho_pr")        # o único exemplo com covariável
  cov <- p$covariaveis[[1]]
  expect_equal(cov, "soja_kg_ha")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p, tendencia = "covariavel"),
                                familia = "esferico")
  g <- grade_com_cov(p, 4L)
  nossa <- tr_spatial_kriging_em(p, m, novos = g, tipo = "universal",
                                 tendencia = "covariavel")
  z <- p$dados[[p$variavel]]
  cd <- p$dados[[cov]]; cl <- g[[cov]]
  gd <- geoR::as.geodata(cbind(p$coords, z), coords.col = 1:2, data.col = 3)
  ctrl <- geoR::krige.control(type.krige = "OK", trend.d = ~ cd, trend.l = ~ cl,
                              cov.model = "spherical",
                              cov.pars = c(m$contribuicao, m$alcance),
                              nugget = m$pepita)
  suppressMessages(utils::capture.output(
    o <- geoR::krige.conv(gd, locations = as.matrix(g[, p$coord_cols]),
                          krige = ctrl)))
  expect_equal(nossa$predito, as.numeric(o$predict), tolerance = 1e-6)
  expect_equal(nossa$variancia, as.numeric(o$krige.var), tolerance = 1e-6)
})

test_that("KED sem grade é recusado, e a mensagem diz o nome da covariável", {
  p <- tr_spatial_example("milho_pr")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p, tendencia = "covariavel"))
  expect_error(tr_spatial_kriging(p, m, tipo = "universal",
                                  tendencia = "covariavel", resolucao = 10L),
               class = "tr_spatial_error_missing_drift")
  expect_error(tr_spatial_kriging(p, m, tipo = "universal",
                                  tendencia = "covariavel", resolucao = 10L),
               regexp = "soja_kg_ha")
})

test_that("grade sem a coluna da covariável é recusada", {
  p <- tr_spatial_example("milho_pr")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p, tendencia = "covariavel"))
  g <- grade_com_cov(p); g[[p$covariaveis[[1]]]] <- NULL
  expect_error(tr_spatial_kriging(p, m, grade = g, tipo = "universal",
                                  tendencia = "covariavel"),
               class = "tr_spatial_error_missing_drift")
})

test_that("grade com NA na covariável é recusada, não krigada em silêncio", {
  p <- tr_spatial_example("milho_pr")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p, tendencia = "covariavel"))
  g <- grade_com_cov(p); g[[p$covariaveis[[1]]]][2] <- NA
  expect_error(tr_spatial_kriging(p, m, grade = g, tipo = "universal",
                                  tendencia = "covariavel"),
               class = "tr_spatial_error_missing_drift")
})

test_that("grade com nomes de coluna de coordenada diferentes é recusada", {
  p <- tr_spatial_example("milho_pr")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p, tendencia = "covariavel"))
  g <- grade_com_cov(p)
  names(g)[1:2] <- c("X", "Y")
  expect_error(tr_spatial_kriging(p, m, grade = g, tipo = "universal",
                                  tendencia = "covariavel"),
               class = "tr_spatial_error_missing_drift")
  expect_error(tr_spatial_kriging(p, m, grade = g, tipo = "universal",
                                  tendencia = "covariavel"),
               regexp = paste(p$coord_cols, collapse = "|"))
})

test_that("a grade do usuário vale também para os outros tipos, e vence a resolução", {
  p <- tr_spatial_example("milho_se")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "esferico")
  g <- expand.grid(x = seq(min(p$coords[, 1]), max(p$coords[, 1]), length.out = 3),
                   y = seq(min(p$coords[, 2]), max(p$coords[, 2]), length.out = 3))
  names(g) <- p$coord_cols
  s <- tr_spatial_kriging(p, m, grade = g, resolucao = 60L)
  expect_equal(nrow(s$grade), 9L)          # a grade ligada, não 60x60
  expect_match(s$nota, "[Gg]rade ligada na porta")
  expect_match(s$nota, "resolução é ignorada")
})

test_that("a grade do usuário recusa tabela sem as coordenadas", {
  p <- tr_spatial_example("milho_se")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "esferico")
  expect_error(tr_spatial_kriging(p, m, grade = data.frame(a = 1:3, b = 4:6)),
               class = "tr_spatial_error_missing_drift")
})

# ---- o débito do PR 1, fechado -------------------------------------------------

test_that("o variograma com tendência por covariável reproduz geoR::variog", {
  skip_if_not_installed("geoR")
  # O corte é escolhido para NÃO cair em nenhuma distância observada: o `gstat`
  # inclui o limite superior da classe e o `geoR` empurra para a seguinte, e um
  # par empatado num limite faz |dgamma| saltar para ~3e-4 (medido em
  # 2026-10-09). Com corte sem empate, a concordância é 5,6e-16.
  p <- tr_spatial_example("milho_pr")
  cov <- p$covariaveis[[1]]
  d <- as.numeric(stats::dist(p$coords))
  corte <- .tr_spatial_corte_padrao(p$coords) * 0.998123
  lim <- seq(0, corte, length.out = 11)
  expect_equal(sum(d %in% lim), 0L)        # a premissa do teste, asserida
  v <- tr_spatial_variogram(p, dist_max = corte, n_classes = 10L,
                            tendencia = "covariavel", pares_min = 1L)
  cd <- p$dados[[cov]]
  o <- suppressMessages(geoR::variog(
    geoR::as.geodata(cbind(p$coords, p$dados[[p$variavel]]), coords.col = 1:2,
                     data.col = 3),
    breaks = lim, trend = ~ cd, messages = FALSE))
  expect_equal(as.integer(v$tabela$np), as.integer(o$n))
  expect_equal(v$tabela$gamma, o$v, tolerance = 1e-8)
})
