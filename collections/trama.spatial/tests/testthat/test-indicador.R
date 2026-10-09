# A krigagem indicadora.
#
# Ela é a krigagem ORDINÁRIA de uma variável 0/1, e o que a torna indicadora é o
# variograma ser DO INDICADOR. Por isso a transformação é um bloco ANTES do
# variograma, e não um valor de `tipo` na krigagem: com um valor de tipo, o
# bloco receberia um modelo ajustado à variável contínua e krigaria indicador
# com ele, em silêncio e errado. O comentário do PR 1 previa o valor de tipo, e
# este PR o corrige.

test_that("o indicador é z <= corte, e o sentido inverte", {
  p <- tr_spatial_example("milho_se")
  z <- p$dados[[p$variavel]]
  corte <- stats::median(z)
  a <- tr_spatial_indicator(p, corte = corte)
  expect_equal(a$dados[[a$variavel]], as.numeric(z <= corte))
  b <- tr_spatial_indicator(p, corte = corte, sentido = ">")
  expect_equal(b$dados[[b$variavel]], as.numeric(z > corte))
  expect_equal(a$dados[[a$variavel]] + b$dados[[b$variavel]], rep(1, length(z)))
})

test_that("o objeto carrega o corte, o sentido e a variável de origem", {
  p <- tr_spatial_example("milho_se")
  corte <- stats::median(p$dados[[p$variavel]])
  a <- tr_spatial_indicator(p, corte = corte)
  expect_equal(a$indicador$corte, corte)
  expect_equal(a$indicador$sentido, "<=")
  expect_equal(a$indicador$variavel_original, p$variavel)
  expect_match(a$nota, "[Ii]ndicador")
  expect_match(a$nota, "PROBABILIDADE|probabilidade")
  # o objeto segue sendo pontos válidos, com coordenadas e borda preservadas
  expect_s3_class(a, "tr_spatial_points")
  expect_equal(a$coords, p$coords)
  expect_equal(a$borda, p$borda)
})

test_that("empate exato no corte segue o sentido declarado", {
  p <- tr_spatial_example("milho_se")
  z <- p$dados[[p$variavel]]
  corte <- z[[1]]                       # um valor OBSERVADO, para empatar
  a <- tr_spatial_indicator(p, corte = corte)
  b <- tr_spatial_indicator(p, corte = corte, sentido = ">")
  expect_equal(a$dados[[a$variavel]][[1]], 1)    # `<=` inclui o empate
  expect_equal(b$dados[[b$variavel]][[1]], 0)    # `>` exclui
})

test_that("corte fora do intervalo dos dados é recusado: indicador constante", {
  p <- tr_spatial_example("milho_se")
  z <- p$dados[[p$variavel]]
  expect_error(tr_spatial_indicator(p, corte = max(z) + 1),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_indicator(p, corte = min(z) - 1),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_indicator(p, corte = max(z) + 1), regexp = "constante")
})

test_that("corte e sentido inválidos são recusados", {
  p <- tr_spatial_example("milho_se")
  expect_error(tr_spatial_indicator(p, corte = NA),
               class = "tr_spatial_error_blank_param")
  expect_error(tr_spatial_indicator(p, corte = stats::median(p$dados[[p$variavel]]),
                                    sentido = "<"),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_indicator(list(a = 1), corte = 1),
               class = "tr_spatial_error_not_points")
})

test_that("o variograma do indicador tem patamar da ordem de p(1-p)", {
  # Relação de sanidade, NÃO oráculo: a variância de um Bernoulli é p(1-p), e o
  # patamar do variograma de um indicador estacionário fica da mesma ordem.
  # Serve para pegar erro grosseiro de escala; a tolerância é larga de propósito.
  p <- tr_spatial_example("milho_pr")
  z <- p$dados[[p$variavel]]
  a <- tr_spatial_indicator(p, corte = stats::median(z))
  pr <- mean(a$dados[[a$variavel]])
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(a), familia = "esferico")
  expect_equal(m$patamar, pr * (1 - pr), tolerance = 0.5)
})

# ---- o recorte em [0,1] --------------------------------------------------------
#
# A krigagem de um indicador SAI de [0,1] de verdade: medido em 2026-10-09, até
# 14 de 144 células fora, com mínimo -0,105 e máximo 1,058. Então o recorte tem
# o que recortar, e a fixture abaixo produz o estouro de propósito —
# extrapolando além da extensão dos pontos, com corte num quantil extremo.

grade_extrapolada <- function(p, folga, n = 12L) {
  g <- expand.grid(
    x = seq(min(p$coords[, 1]) - folga, max(p$coords[, 1]) + folga, length.out = n),
    y = seq(min(p$coords[, 2]) - folga, max(p$coords[, 2]) + folga, length.out = n))
  names(g) <- p$coord_cols
  g
}

test_that("a krigagem de indicador recorta em [0,1] e conta o que recortou", {
  p <- tr_spatial_example("milho_pr")
  z <- p$dados[[p$variavel]]
  a <- tr_spatial_indicator(p, corte = as.numeric(stats::quantile(z, 0.1)))
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(a), familia = "esferico")
  g <- grade_extrapolada(a, folga = 8e4)
  s <- tr_spatial_kriging_em(a, m, novos = g)
  ok <- !is.na(s$predito)
  expect_true(all(s$predito[ok] >= 0 & s$predito[ok] <= 1))
  expect_gt(attr(s, "n_recortadas"), 0)      # houve o que recortar
})

test_that("sem indicador, a krigagem NÃO recorta", {
  p <- tr_spatial_example("milho_pr")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "esferico")
  s <- tr_spatial_kriging_em(p, m, novos = grade_extrapolada(p, folga = 8e4))
  expect_null(attr(s, "n_recortadas"))
})

test_that("a superfície de indicador carrega o recorte e a nota diz", {
  p <- tr_spatial_example("milho_pr")
  z <- p$dados[[p$variavel]]
  a <- tr_spatial_indicator(p, corte = as.numeric(stats::quantile(z, 0.1)))
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(a), familia = "esferico")
  s <- tr_spatial_kriging(a, m, resolucao = 20L)
  expect_false(is.null(s$indicador))
  expect_equal(s$indicador$corte, a$indicador$corte)
  ok <- !is.na(s$grade$predito)
  expect_true(all(s$grade$predito[ok] >= 0 & s$grade$predito[ok] <= 1))
  expect_match(s$nota, "PROBABILIDADE|probabilidade")
})

test_that("o mapa de uma superfície de indicador fala de probabilidade", {
  p <- tr_spatial_example("milho_pr")
  z <- p$dados[[p$variavel]]
  a <- tr_spatial_indicator(p, corte = as.numeric(stats::quantile(z, 0.25)))
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(a), familia = "esferico")
  s <- tr_spatial_kriging(a, m, resolucao = 15L)
  g <- tr_spatial_map(s)
  expect_s3_class(g, "ggplot")
  # o rótulo da legenda vive no NOME da escala de preenchimento, não em
  # `labels`: é `scale_fill_continuous(name = )` que o define.
  nomes <- vapply(g$scales$scales, function(sc) as.character(sc$name %||% "")[1], "")
  expect_true(any(grepl("[Pp]robabilidade", nomes)))
})

test_that("a coleção registra o nó spatial/indicator", {
  co <- trama_collection()
  expect_true("spatial/indicator" %in% vapply(co$nodes, function(x) x$id, ""))
})
