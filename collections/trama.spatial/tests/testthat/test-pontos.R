tab <- function(n = 20L, seed = 1L) {
  set.seed(seed)
  data.frame(leste = runif(n, 0, 1000), norte = runif(n, 0, 1000),
             z = rnorm(n, 50, 5), alt = rnorm(n, 300, 10))
}

test_that("declara o objeto espacial com coordenadas, unidade e variável", {
  p <- tr_spatial_coordinates(tab(), x = "leste", y = "norte", variavel = "z",
                              unidade = "m", nome = "teste")
  expect_s3_class(p, "tr_spatial_points")
  expect_equal(p$coord_cols, c("leste", "norte"))
  expect_equal(p$variavel, "z")
  expect_equal(p$unidade, "m")
  expect_equal(p$rotulo, "teste")
  expect_equal(dim(p$coords), c(20L, 2L))
  expect_true(is.na(p$crs) || inherits(p$crs, "crs"))
})

test_that("coluna que não existe é erro nomeando o param", {
  expect_error(tr_spatial_coordinates(tab(), x = "oeste", y = "norte", variavel = "z"),
               class = "tr_spatial_error_unknown_column")
})

test_that("coordenada ou variável não numérica é erro", {
  d <- tab(); d$rotulo <- letters[1:20]
  expect_error(tr_spatial_coordinates(d, x = "rotulo", y = "norte", variavel = "z"),
               class = "tr_spatial_error_not_numeric")
  expect_error(tr_spatial_coordinates(d, x = "leste", y = "norte", variavel = "rotulo"),
               class = "tr_spatial_error_not_numeric")
})

# Review Focus 1: variograma em graus é erro silencioso e caro.
test_that("CRS geográfico é recusado, mandando projetar", {
  d <- data.frame(lon = seq(-45, -44, length.out = 20),
                  lat = seq(-21, -20, length.out = 20), z = rnorm(20))
  expect_error(tr_spatial_coordinates(d, x = "lon", y = "lat", variavel = "z", crs = "4326"),
               class = "tr_spatial_error_geographic_crs")
  # E o CRS projetado passa (SIRGAS 2000 / UTM 23S).
  d2 <- tab()
  expect_s3_class(tr_spatial_coordinates(d2, "leste", "norte", "z", crs = "31983"),
                  "tr_spatial_points")
})

# Review Focus 2: sumir com dado em silêncio é o pior desfecho.
test_that("NA na coordenada é erro; NA na variável sai do cálculo E aparece na nota", {
  d <- tab(); d$leste[3] <- NA
  expect_error(tr_spatial_coordinates(d, "leste", "norte", "z"),
               class = "tr_spatial_error_bad_coords")
  d2 <- tab(); d2$z[c(2, 7)] <- NA
  p <- tr_spatial_coordinates(d2, "leste", "norte", "z")
  expect_equal(nrow(p$dados), 18L)
  expect_match(p$nota, "2 linha")
})

test_that("menos de três pontos distintos é erro", {
  d <- data.frame(x = c(1, 1, 1), y = c(2, 2, 2), z = c(1, 2, 3))
  expect_error(tr_spatial_coordinates(d, "x", "y", "z"),
               class = "tr_spatial_error_bad_coords")
})

test_that("coordenada duplicada é NOTA, não erro", {
  d <- tab(10L); d[2, c("leste", "norte")] <- d[1, c("leste", "norte")]
  p <- tr_spatial_coordinates(d, "leste", "norte", "z")
  expect_s3_class(p, "tr_spatial_points")
  expect_match(p$nota, "coincidente")
})

test_that("o adaptador para data/table devolve a tabela com as coordenadas", {
  p <- tr_spatial_coordinates(tab(), "leste", "norte", "z")
  t <- .tr_spatial_pontos_tabela(p)
  expect_true(all(c("leste", "norte", "z") %in% names(t)))
  expect_equal(nrow(t), 20L)
})

test_that("o store recusa objeto que não é de pontos", {
  expect_error(.tr_spatial_pontos_conferir(list(a = 1)),
               class = "tr_spatial_error_not_points")
})

test_that("crs NA vale como CRS não declarado", {
  p <- tr_spatial_coordinates(tab(), "leste", "norte", "z", crs = NA_character_)
  expect_true(is.na(p$crs))
})

# A borda segue direto para sf::st_polygon e para o recorte da grade (Tarefa 6).
test_that("borda aberta é fechada; fechada fica como está", {
  aberta <- data.frame(a = c(0, 1, 1, 0), b = c(0, 0, 1, 1))
  b <- .tr_spatial_borda(aberta)
  expect_equal(dim(b), c(5L, 2L))
  expect_equal(b[1, ], b[5, ])
  fechada <- rbind(as.matrix(aberta), c(0, 0))
  expect_equal(.tr_spatial_borda(fechada), unname(fechada))
})

test_that("borda degenerada, não finita ou com colunas erradas é erro", {
  expect_error(.tr_spatial_borda(rbind(c(0, 0), c(1, 1), c(0, 0))),
               class = "tr_spatial_error_bad_border")
  expect_error(.tr_spatial_borda(rbind(c(0, 0), c(1, 1))),
               class = "tr_spatial_error_bad_border")
  expect_error(.tr_spatial_borda(data.frame(a = c(0, Inf, 0), b = c(0, 0, 1))),
               class = "tr_spatial_error_bad_border")
  expect_error(.tr_spatial_borda(data.frame(a = c(0, NA, 0), b = c(0, 0, 1))),
               class = "tr_spatial_error_bad_border")
  expect_error(.tr_spatial_borda(matrix(1:9, 3)), class = "tr_spatial_error_bad_border")
})

test_that("a borda normalizada é aceita por sf::st_polygon", {
  b <- .tr_spatial_borda(data.frame(a = c(0, 1, 1, 0), b = c(0, 0, 1, 1)))
  expect_no_error(g <- sf::st_polygon(list(b)))
  expect_true(as.numeric(sf::st_area(g)) > 0)
})

test_that("o preview dos pontos leva as colunas de coordenada, na ordem x, y", {
  # O renderer lê `coord_cols` para não formatar coordenada como grandeza; sem
  # o campo ele teria de adivinhar pelos valores.
  d <- data.frame(leste = c(1.5, 2.25, 3, 4.5, 5), norte = c(9, 8.5, 7, 6.25, 5), z = 1:5)
  p <- tr_spatial_coordinates(d, "leste", "norte", "z")
  v <- .tr_spatial_pontos_preview(p)
  expect_identical(unlist(v$coord_cols), c("leste", "norte"))
  expect_true(all(unlist(v$coord_cols) %in% unlist(v$columns)))
})
