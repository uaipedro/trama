# A borda: casco convexo dos pontos, e a regra de projeção entre borda e pontos.
#
# Por que o casco tem guarda própria: `st_convex_hull` de pontos COLINEARES
# devolve LINESTRING, não POLYGON (medido com sf 1.0.22 em 2026-10-09), e uma
# borda que não é área produziria grade vazia ou recorte sem sentido adiante.

test_that("o casco convexo fecha o polígono e bate com st_convex_hull", {
  set.seed(1)
  co <- cbind(runif(40, 0, 100), runif(40, 0, 100))
  h <- tr_spatial_convex_hull(co)
  expect_true(is.matrix(h) && ncol(h) == 2L)
  expect_equal(h[1, ], h[nrow(h), ])            # fechado
  ref <- sf::st_convex_hull(sf::st_union(sf::st_as_sf(
    as.data.frame(co), coords = c("V1", "V2"))))
  expect_equal(as.numeric(sf::st_area(sf::st_sfc(sf::st_polygon(list(h))))),
               as.numeric(sf::st_area(ref)), tolerance = 1e-8)
})

test_that("pontos colineares não formam casco: erro nomeado, não LINESTRING", {
  co <- cbind(c(1, 2, 3, 4), c(2, 4, 6, 8))   # todos na mesma reta
  expect_error(tr_spatial_convex_hull(co), class = "tr_spatial_error_bad_border")
})

test_that("menos de três pontos distintos não formam casco", {
  expect_error(tr_spatial_convex_hull(cbind(c(1, 1, 1), c(2, 2, 2))),
               class = "tr_spatial_error_bad_border")
})

# ---- a regra de projeção entre a borda e os pontos ------------------------------
#
# Reprojetar é conveniente e esconde erro de CRS do usuário, então a nota do
# objeto diz sempre de qual projeção para qual. Borda em GRAU com pontos
# projetados é reprojetada, e não recusada: a malha do IBGE vem em 4674, e a
# recusa de grau vale para os PONTOS, cuja distância o variograma mede, não para
# o recorte.

borda_sfc <- function(epsg, esc = 1) {
  sf::st_sfc(sf::st_polygon(list(cbind(
    c(0, 1e5, 1e5, 0, 0) * esc, c(0, 0, 1e5, 1e5, 0) * esc))), crs = epsg)
}

borda_de <- function(epsg, fonte = "teste") {
  tr_spatial_boundary_obj(
    .tr_spatial_borda(sf::st_coordinates(borda_sfc(epsg))[, 1:2]),
    sf::st_crs(epsg), fonte, "Borda", "")
}

test_that("borda no mesmo CRS dos pontos entra direto", {
  b <- borda_de(31982)
  out <- .tr_spatial_borda_crs(b, sf::st_crs(31982))
  expect_equal(out, b$poligono)
})

test_that("borda em CRS diferente é reprojetada para o dos pontos", {
  b <- borda_de(31982)
  out <- .tr_spatial_borda_crs(b, sf::st_crs(31983))
  expect_false(isTRUE(all.equal(as.numeric(out), as.numeric(b$poligono))))
  volta <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_polygon(list(unname(out[, 1:2]))), crs = 31983), 31982))[, 1:2]
  expect_equal(unname(volta), unname(b$poligono), tolerance = 1e-6)
  expect_match(attr(out, "nota"), "reprojetada")
})

test_that("borda em graus com pontos projetados é reprojetada, não recusada", {
  g <- sf::st_sfc(sf::st_polygon(list(cbind(
    c(-52, -48, -48, -52, -52), c(-26, -26, -22, -22, -26)))), crs = 4674)
  b <- tr_spatial_boundary_obj(.tr_spatial_borda(sf::st_coordinates(g)[, 1:2]),
                               sf::st_crs(4674), "malha IBGE", "Paraná", "")
  out <- .tr_spatial_borda_crs(b, sf::st_crs(31982))
  expect_gt(max(abs(out)), 1e5)         # virou metro
})

test_that("pontos sem CRS e borda com CRS: aceita no mesmo plano, com nota", {
  b <- borda_de(31982)
  out <- .tr_spatial_borda_crs(b, NA)
  # `out` carrega o atributo `nota`, então a comparação é de valor e de forma,
  # não de objeto inteiro.
  expect_equal(dim(out), dim(b$poligono))
  expect_equal(as.numeric(out), as.numeric(b$poligono))
  expect_match(attr(out, "nota"), "sem projeção declarada")
})

test_that("pontos com CRS e borda sem CRS: recusa", {
  # A borda sem projeção se monta da matriz: `sf::st_sfc(crs = NA)` recusa o NA
  # lógico, e o caso em teste é justamente o de não haver CRS.
  pol <- cbind(c(0, 1e5, 1e5, 0, 0), c(0, 0, 1e5, 1e5, 0))
  b <- tr_spatial_boundary_obj(.tr_spatial_borda(pol), NA, "teste", "Borda", "")
  expect_error(.tr_spatial_borda_crs(b, sf::st_crs(31982)),
               class = "tr_spatial_error_crs_mismatch")
  expect_error(.tr_spatial_borda_crs(b, sf::st_crs(31982)), regexp = "31982")
})
