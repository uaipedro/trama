# Fixtures de arquivo vetorial, escritas pelo PRÓPRIO sf: nada binário
# versionado no repo, e o ida-e-volta exercita o caminho que o usuário faz.

fx_poligono <- function(epsg = 31982, esc = 1) {
  sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_polygon(list(cbind(
    c(0, 1e5, 1e5, 0, 0) * esc, c(0, 0, 1e5, 1e5, 0) * esc))), crs = epsg))
}

fx_pontos <- function(epsg = 31982, n = 5L) {
  set.seed(7)
  xs <- runif(n, 0, 1e5); ys <- runif(n, 0, 1e5)
  sf::st_sf(v = seq_len(n), nome = paste0("p", seq_len(n)),
            geometry = sf::st_sfc(lapply(seq_len(n), function(i)
              sf::st_point(c(xs[[i]], ys[[i]]))), crs = epsg))
}

# Grava um shapefile e o zipa, opcionalmente dentro de uma subpasta — que é
# como um shapefile baixado costuma chegar.
fx_shp_zip <- function(obj, nome = "camada", subpasta = NULL) {
  base <- tempfile("shp")
  raiz <- if (is.null(subpasta)) base else file.path(base, subpasta)
  dir.create(raiz, recursive = TRUE)
  sf::st_write(obj, file.path(raiz, paste0(nome, ".shp")), quiet = TRUE,
               append = FALSE)
  zip <- paste0(tempfile("vetor"), ".zip")
  old <- setwd(base); on.exit(setwd(old), add = TRUE)
  utils::zip(zip, list.files(".", recursive = TRUE), flags = "-q")
  zip
}

fx_arquivo <- function(obj, ext) {
  p <- paste0(tempfile("vetor"), ".", ext)
  sf::st_write(obj, p, quiet = TRUE, append = FALSE)
  p
}
