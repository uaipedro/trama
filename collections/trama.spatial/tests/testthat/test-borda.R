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

# ---- borda a partir de uma geometria do sf -------------------------------------
#
# `.tr_spatial_borda()` guarda UM anel (matriz n×2). Um polígono com ilha
# perderia o buraco em silêncio, e a grade da krigagem cobriria o que devia
# excluir — por isso a contagem de anéis descartados vai na nota.

test_that("polígono com anel interno: o buraco é descartado, e a nota diz quantos", {
  fora <- cbind(c(0, 100, 100, 0, 0), c(0, 0, 100, 100, 0))
  buraco <- cbind(c(40, 60, 60, 40, 40), c(40, 40, 60, 60, 40))
  g <- sf::st_sfc(sf::st_polygon(list(fora, buraco[rev(seq_len(nrow(buraco))), ])),
                  crs = 31982)
  b <- .tr_spatial_boundary_de_sf(g, fonte = "teste.gpkg")
  expect_equal(b$n_aneis_descartados, 1L)
  expect_match(b$nota, "anel interno")
  # a área é a do anel EXTERNO, sem o buraco
  expect_equal(b$area, 100 * 100, tolerance = 1e-8)
})

test_that("sem anel interno, a nota não fala de anel e a contagem é zero", {
  g <- sf::st_sfc(sf::st_polygon(list(cbind(c(0, 10, 10, 0, 0),
                                           c(0, 0, 10, 10, 0)))), crs = 31982)
  b <- .tr_spatial_boundary_de_sf(g, fonte = "simples.gpkg")
  expect_equal(b$n_aneis_descartados, 0L)
  expect_false(grepl("anel", b$nota))
  expect_equal(b$area, 100, tolerance = 1e-8)
  expect_equal(sf::st_crs(b$crs)$epsg, 31982L)
})

# ---- o tipo spatial/boundary ----------------------------------------------------

test_that("o tipo guarda e devolve a borda sem perder polígono nem projeção", {
  b <- borda_de(31982)
  tipo <- spatial_boundary_type()
  expect_equal(tipo$id, "spatial/boundary")
  p <- tempfile(fileext = ".rds")
  tipo$store(b, p)
  volta <- tipo$restore(p)
  expect_s3_class(volta, "tr_spatial_boundary")
  expect_equal(volta$poligono, b$poligono)
  expect_equal(sf::st_crs(volta$crs)$epsg, 31982L)
  expect_equal(volta$area, b$area)
})

test_that("o store do tipo recusa o que não é borda", {
  tipo <- spatial_boundary_type()
  expect_error(tipo$store(list(a = 1), tempfile()),
               class = "tr_spatial_error_not_a_boundary")
  expect_error(tipo$store(tr_spatial_example("milho_se"), tempfile()),
               class = "tr_spatial_error_not_a_boundary")
})

test_that("o adaptador para data/table dá uma linha por vértice", {
  b <- borda_de(31982)
  t <- .tr_spatial_borda_tabela(b)
  expect_s3_class(t, "data.frame")
  expect_equal(names(t), c("vertice", "x", "y"))
  expect_equal(nrow(t), nrow(b$poligono))
  expect_equal(t$vertice, seq_len(nrow(b$poligono)))
  expect_equal(t$x, unname(b$poligono[, 1]))
})

test_that("a coleção registra o tipo spatial/boundary e o adaptador para tabela", {
  co <- trama_collection()
  ids <- vapply(co$types, function(x) x$id, "")
  expect_true("spatial/boundary" %in% ids)
  de <- vapply(co$adapters, function(a) a$from, "")
  para <- vapply(co$adapters, function(a) a$to, "")
  expect_true(any(de == "spatial/boundary" & para == "data/table"))
})
