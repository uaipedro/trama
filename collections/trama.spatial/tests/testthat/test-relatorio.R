test_that("pontos, variograma e modelo vão ao relatório; a superfície, como mapa", {
  pts <- tr_spatial_example()
  expect_match(tr_spatial_report_points(pts), "| projecao | EPSG:31982 |", fixed = TRUE)
  v <- tr_spatial_variogram(pontos = pts)
  expect_match(tr_spatial_report_variogram(v), "| Distância | γ(h) | Pares |", fixed = TRUE)
  mo <- tr_spatial_variogram_fit(variograma = v)
  expect_match(tr_spatial_report_model(mo), "| patamar |", fixed = TRUE)
  expect_s3_class(tr_spatial_report_surface(tr_spatial_kriging(modelo = mo)), "ggplot")
})
