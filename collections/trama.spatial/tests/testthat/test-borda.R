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
