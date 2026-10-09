# A leitura de arquivo vetorial, sobre `sf::st_read` (GDAL).
#
# Vive NESTA coleção, e não no `data/read` da `trama.data`, porque estender o
# `data/read` poria `sf` e GDAL como dependência de quem só lê csv.
#
# Um shapefile sempre chega como ZIP (.shp + .shx + .dbf + .prj). O GDAL abre o
# zip direto por `/vsizip/`, sem descompactar, e lê o EPSG do `.prj` — conferido
# em 2026-10-09 com sf 1.0.22. O zip é varrido antes porque o caminho interno
# faz parte do DSN: com o `.shp` numa subpasta, `/vsizip/<zip>` sozinho não acha
# nada.
#
# Dois verbos, dois blocos: ler PONTOS devolve `data/table` e segue pelo
# `spatial/coordinates`, que já tem a declaração de variável, covariáveis, CRS e
# as guardas; ler POLÍGONOS devolve a borda. Cada um recusa a geometria do
# outro, com erro nomeado.

.TR_SPATIAL_EXT_VETOR <- c("shp", "zip", "geojson", "json", "gpkg", "kml")

#' Resolve o caminho num DSN que o GDAL abre, e lista as camadas de dentro.
#' @noRd
.tr_spatial_dsn <- function(caminho) {
  caminho <- trimws(as.character(caminho %||% ""))
  if (!length(caminho) || !nzchar(caminho)) {
    .tr_spatial_abort("tr_spatial_error_blank_param", paste(
      "Arquivo: informe o caminho do arquivo vetorial (.zip de shapefile,",
      ".shp, .geojson, .gpkg ou .kml)."))
  }
  if (!file.exists(caminho)) {
    .tr_spatial_abort("tr_spatial_error_file_not_found",
      sprintf("Arquivo: '%s' não existe.", caminho))
  }
  ext <- tolower(tools::file_ext(caminho))
  if (!ext %in% .TR_SPATIAL_EXT_VETOR) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Arquivo: extensão '%s' não é de arquivo vetorial. Use uma de: %s.",
      ext, paste(.TR_SPATIAL_EXT_VETOR, collapse = ", ")))
  }
  if (!identical(ext, "zip")) return(list(dsn = caminho, internos = character()))
  dentro <- tryCatch(utils::unzip(caminho, list = TRUE)$Name,
                     error = function(e) character())
  shps <- grep("\\.shp$", dentro, value = TRUE, ignore.case = TRUE)
  if (!length(shps)) {
    .tr_spatial_abort("tr_spatial_error_no_layer", sprintf(paste(
      "O zip não tem nenhum .shp dentro, e é de um shapefile zipado que o bloco",
      "precisa. Conteúdo: %s."),
      paste(utils::head(dentro, 10), collapse = ", ")))
  }
  list(dsn = paste0("/vsizip/", caminho, "/", shps[[1]]), internos = shps)
}

#' Lê o arquivo e confere que a geometria é a que o bloco espera.
#' @noRd
.tr_spatial_ler_vetor <- function(caminho, camada, geometria) {
  r <- .tr_spatial_dsn(caminho)
  camada <- trimws(as.character(camada %||% ""))
  if (length(r$internos) > 1L) {
    if (!nzchar(camada)) {
      .tr_spatial_abort("tr_spatial_error_many_layers", sprintf(
        "O arquivo tem %d camadas: %s. Preencha 'Camada' com uma delas.",
        length(r$internos), paste(basename(r$internos), collapse = ", ")))
    }
    alvo <- grep(sprintf("(^|/)%s\\.shp$", camada), r$internos, value = TRUE,
                 ignore.case = TRUE)
    if (!length(alvo)) {
      .tr_spatial_abort("tr_spatial_error_no_layer", sprintf(
        "Camada '%s' não está no arquivo. Há: %s.", camada,
        paste(basename(r$internos), collapse = ", ")))
    }
    r$dsn <- paste0("/vsizip/", caminho, "/", alvo[[1]])
  }
  g <- try(suppressWarnings(
    if (nzchar(camada) && !length(r$internos)) {
      sf::st_read(r$dsn, layer = camada, quiet = TRUE)
    } else {
      sf::st_read(r$dsn, quiet = TRUE)
    }), silent = TRUE)
  if (inherits(g, "try-error")) {
    .tr_spatial_abort("tr_spatial_error_unreadable", paste(
      "O GDAL não conseguiu abrir o arquivo:",
      conditionMessage(attr(g, "condition"))))
  }
  if (!nrow(g)) {
    .tr_spatial_abort("tr_spatial_error_too_few",
      "O arquivo não tem nenhuma feição para ler.")
  }
  tipos <- unique(as.character(sf::st_geometry_type(g)))
  esperado <- if (identical(geometria, "pontos")) c("POINT", "MULTIPOINT") else
    c("POLYGON", "MULTIPOLYGON")
  if (!all(tipos %in% esperado)) {
    .tr_spatial_abort("tr_spatial_error_wrong_geometry", sprintf(paste(
      "Este arquivo tem geometria %s, e o bloco espera %s.",
      "Use o outro bloco de leitura."),
      paste(tipos, collapse = "/"), paste(esperado, collapse = " ou ")))
  }
  # MULTIPOINT é explodido: um GeoJSON de pontos pode vir assim, e recusá-lo
  # como "não pontual" seria errado.
  if (identical(geometria, "pontos") && any(tipos == "MULTIPOINT")) {
    g <- suppressWarnings(sf::st_cast(g, "POINT", warn = FALSE))
  }
  g
}

#' CRS de destino da reprojeção na leitura.
#'
#' Ao contrário de `.tr_spatial_crs()`, este NÃO recusa grau: a recusa de grau
#' vale para o CRS dos PONTOS, onde o variograma mede distância. Aqui o destino
#' pode legitimamente ser geográfico, para quem vai cruzar com outra base.
#' @noRd
.tr_spatial_crs_alvo <- function(crs) {
  txt <- trimws(as.character(crs))
  obj <- try(sf::st_crs(if (grepl("^[0-9]+$", txt)) as.integer(txt) else txt),
             silent = TRUE)
  if (inherits(obj, "try-error") || is.na(obj)) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(paste(
      "Reprojetar para: '%s' não foi reconhecido. Use um código EPSG",
      "(por exemplo 31982)."), crs))
  }
  obj
}

#' Lê um arquivo vetorial de pontos como tabela.
#'
#' @param caminho arquivo: `.zip` de shapefile, `.shp`, `.geojson`, `.gpkg` ou
#'   `.kml`.
#' @param camada camada dentro do arquivo; vazio usa a única que houver.
#' @param crs_saida EPSG de destino; vazio não reprojeta.
#' @param nomes_coords nomes das duas colunas de coordenada, separados por
#'   vírgula.
#' @return `data.frame` com os atributos do arquivo e as duas colunas de
#'   coordenada.
#' @export
tr_spatial_read_points <- function(caminho, camada = "", crs_saida = "",
                                   nomes_coords = "x,y") {
  nomes <- trimws(strsplit(as.character(nomes_coords), ",", fixed = TRUE)[[1]])
  if (length(nomes) != 2L || !all(nzchar(nomes))) {
    .tr_spatial_abort("tr_spatial_error_bad_option", paste(
      "Nomes das coordenadas: dois nomes separados por vírgula,",
      "por exemplo 'x,y'."))
  }
  g <- .tr_spatial_ler_vetor(caminho, camada, "pontos")
  if (nzchar(trimws(as.character(crs_saida)))) {
    g <- sf::st_transform(g, .tr_spatial_crs_alvo(crs_saida))
  }
  d <- sf::st_drop_geometry(g)
  bate <- intersect(nomes, names(d))
  if (length(bate)) {
    .tr_spatial_abort("tr_spatial_error_name_clash", sprintf(paste(
      "O arquivo já tem coluna chamada %s, e o bloco não sobrescreve atributo.",
      "Mude 'Nomes das coordenadas'."), paste(bate, collapse = " e ")))
  }
  co <- sf::st_coordinates(g)[, 1:2, drop = FALSE]
  d[[nomes[[1]]]] <- unname(co[, 1])
  d[[nomes[[2]]]] <- unname(co[, 2])
  d
}

#' Lê um arquivo vetorial de polígonos como a borda da área de estudo.
#'
#' Nome parecido, função outra: `tr_spatial_boundary_obj()` é o construtor do
#' objeto, em `R/borda.R`; esta aqui lê arquivo e é o `fn` do bloco.
#'
#' @inheritParams tr_spatial_read_points
#' @return uma borda (`spatial/boundary`).
#' @export
tr_spatial_boundary <- function(caminho, camada = "", crs_saida = "") {
  g <- .tr_spatial_ler_vetor(caminho, camada, "poligonos")
  if (nzchar(trimws(as.character(crs_saida)))) {
    g <- sf::st_transform(g, .tr_spatial_crs_alvo(crs_saida))
  }
  fonte <- paste(c(basename(caminho),
                   if (nzchar(trimws(as.character(camada)))) trimws(camada)),
                 collapse = ", camada ")
  .tr_spatial_boundary_de_sf(
    g, fonte = fonte, rotulo = tools::file_path_sans_ext(basename(caminho)))
}
