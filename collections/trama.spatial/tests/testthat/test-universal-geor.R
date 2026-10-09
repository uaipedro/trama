# Oráculo da krigagem universal: `geoR::krige.conv` com `trend.d`/`trend.l`, a
# referência do material de aula.
#
# A TENDÊNCIA VAI CENTRADA E PADRONIZADA NOS DOIS PACOTES, e isso não é
# cosmético: em UTM cru o `krige.conv` com `trend = "1st"`/`"2nd"` fica
# computacionalmente singular (condição recíproca 7,3e-17, medido em
# 2026-10-09), e o próprio `gstat` difere 3,7e-4 na 2ª ordem entre a versão crua
# e a centrada. A krigagem com tendência é invariante a reparametrização linear
# da base, logo essa diferença é erro numérico e a centrada é a confiável.
#
# A geometria segue CRUA: só as colunas da base da tendência são centradas, para
# as distâncias — e portanto a covariância — não mudarem.
#
# Concordância medida com `meuse`: 3,7e-14 na 1ª ordem e 5,3e-14 na 2ª.
# Tolerância declarada 1e-6.

krige_geor_tend <- function(p, m, novos, tendencia) {
  z <- p$dados[[p$variavel]]
  cx <- mean(p$coords[, 1]); cy <- mean(p$coords[, 2])
  sx <- stats::sd(p$coords[, 1]); sy <- stats::sd(p$coords[, 2])
  xd <- (p$coords[, 1] - cx) / sx; yd <- (p$coords[, 2] - cy) / sy
  xl <- (novos[[1]] - cx) / sx;    yl <- (novos[[2]] - cy) / sy
  td <- if (tendencia == "1a ordem") ~ xd + yd else
    ~ xd + yd + I(xd^2) + I(yd^2) + I(xd * yd)
  tl <- if (tendencia == "1a ordem") ~ xl + yl else
    ~ xl + yl + I(xl^2) + I(yl^2) + I(xl * yl)
  cm <- c(esferico = "spherical", exponencial = "exponential",
          gaussiano = "gaussian", matern = "matern")[[m$familia]]
  g <- geoR::as.geodata(cbind(p$coords, z), coords.col = 1:2, data.col = 3)
  ctrl <- geoR::krige.control(
    type.krige = "OK", trend.d = td, trend.l = tl, cov.model = cm,
    cov.pars = c(m$contribuicao, m$alcance), nugget = m$pepita,
    kappa = if (is.na(m$kappa)) 0.5 else m$kappa)
  suppressMessages(utils::capture.output(
    o <- geoR::krige.conv(g, locations = as.matrix(novos), krige = ctrl)))
  o
}

grade_dentro <- function(p, n = 4L) {
  g <- expand.grid(
    x = seq(min(p$coords[, 1]), max(p$coords[, 1]), length.out = n),
    y = seq(min(p$coords[, 2]), max(p$coords[, 2]), length.out = n))
  names(g) <- p$coord_cols
  g
}

test_that("a universal de 1ª e 2ª ordem reproduz geoR::krige.conv", {
  skip_if_not_installed("geoR")
  p <- tr_spatial_example("milho_se")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "esferico")
  novos <- grade_dentro(p)
  for (tend in c("1a ordem", "2a ordem")) {
    nossa <- tr_spatial_kriging_em(p, m, novos = novos, tipo = "universal",
                                   tendencia = tend)
    o <- krige_geor_tend(p, m, novos, tend)
    expect_equal(nossa$predito, as.numeric(o$predict), tolerance = 1e-6,
                 info = tend)
    expect_equal(nossa$variancia, as.numeric(o$krige.var), tolerance = 1e-6,
                 info = tend)
  }
})

test_that("as colunas da tendência saem centradas e padronizadas", {
  p <- tr_spatial_example("milho_pr")
  cols <- .tr_spatial_tendencia_colunas(p$coords, tendencia = "2a ordem")
  expect_true(all(c(".tx", ".ty", ".tx2", ".ty2", ".txy") %in% names(cols)))
  expect_lt(abs(mean(cols$.tx)), 1e-9)            # centrada
  expect_equal(stats::sd(cols$.tx), 1, tolerance = 1e-9)   # padronizada
  expect_lt(max(abs(cols$.tx)), 10)               # escala de desvio-padrão
  # 1ª ordem não traz os termos quadráticos
  c1 <- .tr_spatial_tendencia_colunas(p$coords, tendencia = "1a ordem")
  expect_equal(names(c1), c(".tx", ".ty"))
})

test_that("o alvo usa o centro e a escala dos PONTOS, não os dele", {
  # Se cada ponta centrasse pela própria média, a base da tendência mudaria
  # entre ajuste e predição, e a predição sairia errada sem erro.
  p <- tr_spatial_example("milho_se")
  alvo <- grade_dentro(p, 3L)
  co <- .tr_spatial_tendencia_colunas(p$coords, alvo = as.matrix(alvo),
                                      tendencia = "1a ordem")
  centro <- colMeans(p$coords); escala <- apply(p$coords, 2, stats::sd)
  expect_equal(co$.tx, (alvo[[1]] - centro[[1]]) / escala[[1]])
  expect_false(isTRUE(all.equal(mean(co$.tx), 0)))   # o alvo não é centrado em si
})

test_that("a centragem muda o número na 2ª ordem: a nossa é a centrada", {
  p <- tr_spatial_example("milho_pr")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "esferico")
  novos <- grade_dentro(p, 3L)
  nossa <- tr_spatial_kriging_em(p, m, novos = novos, tipo = "universal",
                                 tendencia = "2a ordem")
  # a versão CRUA, montada aqui à mão, para medir a diferença
  d <- sf::st_as_sf(as.data.frame(p$dados), coords = p$coord_cols,
                    remove = FALSE)
  d$.z <- p$dados[[p$variavel]]
  d$.rx <- p$coords[, 1]; d$.ry <- p$coords[, 2]
  a <- as.data.frame(novos); a$.rx <- a[[1]]; a$.ry <- a[[2]]
  as_sf <- sf::st_as_sf(a, coords = p$coord_cols, remove = FALSE)
  cru <- gstat::krige(
    stats::as.formula(".z ~ .rx + .ry + I(.rx^2) + I(.ry^2) + I(.rx * .ry)"),
    d, as_sf, .tr_spatial_vgm_model(m), debug.level = 0)
  expect_true(all(is.finite(nossa$predito)))
  # A faixa (0, 1) que estava aqui não prendia nada: passaria com 1e-300
  # (centragem virada no-op) e com 0,99 (ordens de magnitude pior que o
  # medido). Os números abaixo são medidos em `milho_pr` em 2026-10-09:
  # diferença RELATIVA de 5,7e-14 na 1ª ordem e 3,6e-10 na 2ª. O 3,7e-4 da spec
  # era do `meuse`, com outra escala de coordenada.
  rel <- function(tend) {
    nos <- tr_spatial_kriging_em(p, m, novos = novos, tipo = "universal",
                                 tendencia = tend)
    f <- if (tend == "1a ordem") ".z ~ .rx + .ry" else
      ".z ~ .rx + .ry + I(.rx^2) + I(.ry^2) + I(.rx * .ry)"
    cr <- gstat::krige(stats::as.formula(f), d, as_sf,
                       .tr_spatial_vgm_model(m), debug.level = 0)
    max(abs(nos$predito - cr$var1.pred)) / mean(abs(nos$predito))
  }
  r1 <- rel("1a ordem"); r2 <- rel("2a ordem")
  # a centragem NÃO é no-op: a diferença existe nas duas ordens
  expect_gt(r1, 1e-16)
  expect_gt(r2, 1e-12)
  # e o mal condicionamento castiga o termo QUADRÁTICO muito mais: é essa
  # relação, e não o valor absoluto, que a centragem existe para evitar
  expect_gt(r2 / r1, 100)
  # as duas na ordem de grandeza medida, com folga de duas décadas
  expect_lt(r1, 1e-11)
  expect_lt(r2, 1e-7)
})

test_that("a universal exige tendência, e tendência sem universal não vale", {
  p <- tr_spatial_example("milho_se")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "esferico")
  expect_error(tr_spatial_kriging(p, m, tipo = "universal",
                                  tendencia = "constante"),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_kriging(p, m, tipo = "universal", tendencia = "nenhuma"),
               class = "tr_spatial_error_bad_option")
})

test_that("a universal pela grade do bloco roda e recorta na borda", {
  p <- tr_spatial_example("milho_se")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "esferico")
  s <- tr_spatial_kriging(p, m, tipo = "universal", tendencia = "1a ordem",
                          resolucao = 20L)
  expect_equal(s$tipo, "universal")
  expect_true(any(!is.na(s$grade$predito)))
  expect_lt(nrow(s$grade), 20L * 20L)
  expect_match(s$nota, "1a ordem|1ª ordem")
})
