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

# ---- o zip como ele chega de verdade -------------------------------------------
#
# `/vsizip/<zip>` sozinho não acha o .shp quando ele está numa SUBPASTA: o
# caminho interno faz parte do DSN. E com duas camadas a escolha não pode ser
# silenciosa.

test_that("zip com o shapefile numa subpasta é lido", {
  z <- fx_shp_zip(fx_pontos(31982), subpasta = "dados")
  d <- tr_spatial_read_points(z)
  expect_equal(nrow(d), 5L)
  expect_true(all(c("x", "y") %in% names(d)))
})

test_that("zip com duas camadas exige escolha, e o erro lista as que há", {
  raiz <- tempfile("multi"); dir.create(raiz)
  sf::st_write(fx_pontos(31982), file.path(raiz, "sedes.shp"), quiet = TRUE,
               append = FALSE)
  sf::st_write(fx_pontos(31982, n = 3L), file.path(raiz, "postos.shp"),
               quiet = TRUE, append = FALSE)
  zip <- paste0(tempfile("vetor"), ".zip")
  old <- setwd(raiz); utils::zip(zip, list.files("."), flags = "-q"); setwd(old)

  expect_error(tr_spatial_read_points(zip), class = "tr_spatial_error_many_layers")
  expect_error(tr_spatial_read_points(zip), regexp = "sedes")
  expect_error(tr_spatial_read_points(zip), regexp = "postos")
  expect_equal(nrow(tr_spatial_read_points(zip, camada = "postos")), 3L)
  expect_equal(nrow(tr_spatial_read_points(zip, camada = "sedes")), 5L)
  expect_error(tr_spatial_read_points(zip, camada = "inexistente"),
               class = "tr_spatial_error_no_layer")
})

test_that("zip sem shapefile dentro dá erro que mostra o conteúdo", {
  z <- paste0(tempfile("vazio"), ".zip")
  raiz <- tempfile("v"); dir.create(raiz)
  writeLines("a,b", file.path(raiz, "tabela.csv"))
  old <- setwd(raiz); utils::zip(z, "tabela.csv", flags = "-q"); setwd(old)
  expect_error(tr_spatial_read_points(z), class = "tr_spatial_error_no_layer")
  expect_error(tr_spatial_read_points(z), regexp = "tabela.csv")
})

# ---- geometria múltipla --------------------------------------------------------

test_that("MULTIPOINT é explodido em pontos, não recusado", {
  mp <- sf::st_sf(v = 1L, geometry = sf::st_sfc(
    sf::st_multipoint(cbind(c(10, 20, 30), c(40, 50, 60))), crs = 31982))
  d <- tr_spatial_read_points(fx_arquivo(mp, "gpkg"))
  expect_equal(nrow(d), 3L)
  expect_equal(sort(d$x), c(10, 20, 30))
})

test_that("MULTIPOLYGON dissolve como borda, e fica o de maior área", {
  a <- sf::st_polygon(list(cbind(c(0, 10, 10, 0, 0), c(0, 0, 10, 10, 0))))
  b <- sf::st_polygon(list(cbind(c(30, 50, 50, 30, 30), c(30, 30, 50, 50, 30))))
  mp <- sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_multipolygon(list(a, b)),
                                                 crs = 31982))
  bd <- tr_spatial_boundary(fx_arquivo(mp, "gpkg"))
  expect_s3_class(bd, "tr_spatial_boundary")
  expect_equal(bd$area, 20 * 20, tolerance = 1e-8)
  expect_match(bd$nota, "dissolvidos")
})

# ---- a borda lida de arquivo ---------------------------------------------------

test_that("o EPSG da borda vem do .prj do shapefile zipado", {
  bd <- tr_spatial_boundary(fx_shp_zip(fx_poligono(31982)))
  expect_equal(sf::st_crs(bd$crs)$epsg, 31982L)
  expect_equal(bd$area, 1e5 * 1e5, tolerance = 1e-6)
  expect_match(bd$fonte, "\\.zip")
})

test_that("arquivo de pontos é recusado pelo leitor de borda", {
  expect_error(tr_spatial_boundary(fx_arquivo(fx_pontos(31982), "gpkg")),
               class = "tr_spatial_error_wrong_geometry")
})

# ---- o caminho do dado próprio, de ponta a ponta -------------------------------
#
# É o que a 0.1.0 não permitia: sem porta de borda, a grade da krigagem para dado
# do usuário era o retângulo da extensão. Aqui os dados de um exemplo são
# GRAVADOS como arquivos de verdade e lidos de volta, como um usuário faria.

test_that("do arquivo ao mapa: arquivo de pontos e de borda chegam à krigagem", {
  p0 <- tr_spatial_example("milho_se")
  pts <- sf::st_as_sf(p0$dados, coords = p0$coord_cols, crs = p0$crs,
                      remove = TRUE)
  arq_pts <- fx_arquivo(pts, "gpkg")
  arq_bd <- fx_arquivo(sf::st_sf(id = 1L, geometry = sf::st_sfc(
    sf::st_polygon(list(p0$borda)), crs = p0$crs)), "gpkg")

  d <- tr_spatial_read_points(arq_pts, nomes_coords = "leste,norte")
  b <- tr_spatial_boundary(arq_bd)
  p <- tr_spatial_coordinates(d, x = "leste", y = "norte", variavel = p0$variavel,
                              crs = "31984", unidade = "m", borda = b)
  expect_false(is.null(p$borda))
  expect_equal(nrow(p$dados), nrow(p0$dados))

  v <- tr_spatial_variogram(p)
  m <- tr_spatial_variogram_fit(v, familia = "esferico")
  s <- tr_spatial_kriging(p, m, resolucao = 30L)
  expect_true(all(c("predito", "variancia") %in% names(s$grade)))
  expect_true(any(!is.na(s$grade$predito)))
  expect_lt(nrow(s$grade), 30L * 30L)        # a borda recortou a grade
  expect_false(is.null(s$borda))
})

test_that("a borda lida pode ser reprojetada na leitura", {
  bd <- tr_spatial_boundary(fx_arquivo(fx_poligono(31982), "gpkg"),
                            crs_saida = "4674")
  expect_equal(sf::st_crs(bd$crs)$epsg, 4674L)
  expect_lt(max(abs(bd$poligono)), 180)
})
