# A leitura de arquivo vetorial, sobre `sf::st_read` (GDAL).
#
# Um shapefile sempre chega como ZIP (.shp + .shx + .dbf + .prj). O GDAL abre o
# zip direto por /vsizip/, sem descompactar, e lê o EPSG do .prj — conferido em
# 2026-10-09 com sf 1.0.22.
#
# Reprojetar na leitura existe porque GeoJSON é latitude e longitude por
# especificação (RFC 7946) e o `spatial/coordinates` recusa grau: sem o param, o
# arquivo mais comum da internet entra e trava no bloco seguinte.

test_that("lê shapefile zipado e devolve atributos mais as coordenadas", {
  z <- fx_shp_zip(fx_pontos(31982))
  d <- tr_spatial_read_points(z)
  expect_s3_class(d, "data.frame")
  expect_true(all(c("x", "y", "v") %in% names(d)))
  expect_equal(nrow(d), 5L)
  expect_true(all(is.finite(d$x)) && all(is.finite(d$y)))
})

test_that("ida-e-volta: as coordenadas lidas são as escritas", {
  p <- fx_pontos(31982)
  d <- tr_spatial_read_points(fx_arquivo(p, "gpkg"))
  ref <- sf::st_coordinates(p)
  expect_equal(d$x, ref[, 1], tolerance = 1e-9)
  expect_equal(d$y, ref[, 2], tolerance = 1e-9)
})

test_that("reprojeção pedida acontece e a não pedida não acontece", {
  p <- fx_pontos(31982)
  sem <- tr_spatial_read_points(fx_arquivo(p, "gpkg"))
  com <- tr_spatial_read_points(fx_arquivo(p, "gpkg"), crs_saida = "31983")
  expect_equal(sem$x, sf::st_coordinates(p)[, 1], tolerance = 1e-9)
  expect_false(isTRUE(all.equal(com$x, sem$x)))
  volta <- sf::st_coordinates(sf::st_transform(
    sf::st_as_sf(com, coords = c("x", "y"), crs = 31983), 31982))
  expect_equal(volta[, 1], sem$x, tolerance = 1e-6)
})

test_that("geojson em grau é reprojetável na leitura, que é o ponto do param", {
  # Fixture em GRAU de verdade: ponto com coordenada de 0 a 1e5 num CRS
  # geográfico não existe (longitude 100.000°), e o PROJ devolve NaN.
  lon <- c(-52.1, -51.4, -50.2, -48.9); lat <- c(-25.8, -24.3, -23.1, -22.4)
  p <- sf::st_sf(v = 1:4, geometry = sf::st_sfc(
    lapply(seq_along(lon), function(i) sf::st_point(c(lon[[i]], lat[[i]]))),
    crs = 4674))
  d <- tr_spatial_read_points(fx_arquivo(p, "geojson"), crs_saida = "31982")
  expect_true(all(is.finite(d$x)) && all(is.finite(d$y)))
  expect_gt(max(abs(d$x)), 1e5)              # virou metro UTM
  sem <- tr_spatial_read_points(fx_arquivo(p, "geojson"))
  expect_lt(max(abs(sem$x)), 180)            # sem reprojetar, segue em grau
})

test_that("arquivo de polígono é recusado pelo leitor de pontos", {
  expect_error(tr_spatial_read_points(fx_arquivo(fx_poligono(), "gpkg")),
               class = "tr_spatial_error_wrong_geometry")
})

test_that("colisão de nome de coluna é recusada, não sobrescrita", {
  p <- fx_pontos(31982)
  p$x <- 1
  expect_error(tr_spatial_read_points(fx_arquivo(p, "gpkg")),
               class = "tr_spatial_error_name_clash")
})

test_that("nomes_coords renomeia as duas colunas", {
  d <- tr_spatial_read_points(fx_arquivo(fx_pontos(31982), "gpkg"),
                              nomes_coords = "leste,norte")
  expect_true(all(c("leste", "norte") %in% names(d)))
  expect_false("x" %in% names(d))
})

test_that("nomes_coords com um nome só é recusado", {
  expect_error(tr_spatial_read_points(fx_arquivo(fx_pontos(31982), "gpkg"),
                                      nomes_coords = "x"),
               class = "tr_spatial_error_bad_option")
})

test_that("arquivo inexistente e extensão não vetorial são recusados", {
  expect_error(tr_spatial_read_points("nao-existe.gpkg"),
               class = "tr_spatial_error_file_not_found")
  csv <- paste0(tempfile("tab"), ".csv")
  writeLines("a,b\n1,2", csv)
  expect_error(tr_spatial_read_points(csv), class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_read_points(""), class = "tr_spatial_error_blank_param")
})

test_that("CRS de saída irreconhecível é recusado", {
  expect_error(tr_spatial_read_points(fx_arquivo(fx_pontos(31982), "gpkg"),
                                      crs_saida = "epsg da minha cabeça"),
               class = "tr_spatial_error_bad_option")
})
