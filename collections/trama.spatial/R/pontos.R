#' Divide o valor de um param de coluna e confere que as colunas existem.
#' @noRd
.tr_spatial_cols <- function(tabela, valor, label) {
  if (is.null(valor) || !nzchar(trimws(paste(valor, collapse = "")))) return(character())
  nomes <- trimws(unlist(strsplit(as.character(valor), ",", fixed = TRUE)))
  nomes <- nomes[nzchar(nomes)]
  falta <- setdiff(nomes, names(tabela))
  if (length(falta)) {
    .tr_spatial_abort("tr_spatial_error_unknown_column",
      sprintf("%s: a tabela não tem a coluna %s.", label,
              paste(sprintf("'%s'", falta), collapse = ", ")))
  }
  nomes
}

#' Uma coluna só, obrigatória.
#' @noRd
.tr_spatial_col1 <- function(tabela, valor, label) {
  n <- .tr_spatial_cols(tabela, valor, label)
  if (length(n) != 1L) {
    .tr_spatial_abort("tr_spatial_error_blank_param",
      sprintf("%s: escolha exatamente uma coluna.", label))
  }
  if (!is.numeric(tabela[[n]])) {
    .tr_spatial_abort("tr_spatial_error_not_numeric",
      sprintf("%s: a coluna '%s' não é numérica.", label, n))
  }
  n
}

#' Resolve o texto do param `crs` num objeto de CRS, recusando graus.
#'
#' Distância euclidiana sobre latitude e longitude não é distância: um grau de
#' longitude vale 111 km no equador e 78 km em Lavras. O variograma sai com
#' número e gráfico plausíveis e errado por um fator que varia dentro da
#' própria área. É o erro que sai verde, então ele morre aqui.
#' @noRd
.tr_spatial_crs <- function(crs) {
  if (is.null(crs) || anyNA(crs) || !nzchar(trimws(as.character(crs)))) return(NA)
  obj <- try(sf::st_crs(suppressWarnings(
    if (grepl("^[0-9]+$", trimws(crs))) as.integer(trimws(crs)) else as.character(crs))),
    silent = TRUE)
  if (inherits(obj, "try-error") || is.na(obj)) {
    .tr_spatial_abort("tr_spatial_error_bad_option",
      sprintf("CRS: '%s' não foi reconhecido. Use um código EPSG (ex.: 31983) ou deixe vazio.", crs))
  }
  if (isTRUE(sf::st_is_longlat(obj))) {
    .tr_spatial_abort("tr_spatial_error_geographic_crs", paste(
      "As coordenadas estão em graus (CRS geográfico).",
      "O variograma mede distância em linha reta, e um grau não é uma distância fixa:",
      "vale cerca de 111 km no equador e menos conforme a latitude sobe.",
      "Projete antes para um CRS métrico (UTM da sua zona, por exemplo EPSG 31983)",
      "e declare esse código aqui."))
  }
  obj
}

.TR_SPATIAL_CAMPOS_PONTOS <- c("dados", "coords", "coord_cols", "variavel",
                               "crs", "unidade", "borda", "covariaveis", "rotulo", "nota")

#' Declara a variável regionalizada: a tabela vira objeto espacial.
#'
#' @param dados uma tabela (`data/table`).
#' @param x,y colunas das coordenadas, numéricas e PROJETADAS.
#' @param variavel coluna da variável regionalizada.
#' @param covariaveis colunas candidatas a tendência externa (texto separado por vírgula).
#' @param crs código EPSG, ou vazio para plano arbitrário.
#' @param unidade unidade da distância; só rotula eixo e alcance.
#' @param nome topo do card.
#' @param borda matriz ou data.frame de duas colunas com o polígono, ou `NULL`.
#' @return um objeto espacial (`spatial/points`).
#' @export
tr_spatial_coordinates <- function(dados, x, y, variavel, covariaveis = "",
                                   crs = "", unidade = "", nome = "", borda = NULL) {
  dados <- as.data.frame(dados)
  cx <- .tr_spatial_col1(dados, x, "Coordenada X")
  cy <- .tr_spatial_col1(dados, y, "Coordenada Y")
  if (identical(cx, cy)) {
    .tr_spatial_abort("tr_spatial_error_bad_coords", "As duas coordenadas são a mesma coluna.")
  }
  cz <- .tr_spatial_col1(dados, variavel, "Variável")
  cov <- .tr_spatial_cols(dados, covariaveis, "Covariáveis")
  obj_crs <- .tr_spatial_crs(crs)

  notas <- character()
  mau <- !is.finite(dados[[cx]]) | !is.finite(dados[[cy]])
  if (any(mau)) {
    .tr_spatial_abort("tr_spatial_error_bad_coords", sprintf(
      "%d linha(s) com coordenada faltante ou infinita. Corrija ou remova antes (data/filter).",
      sum(mau)))
  }
  # Faltante na VARIÁVEL sai do cálculo, mas nunca em silêncio.
  semz <- !is.finite(dados[[cz]])
  if (any(semz)) {
    notas <- c(notas, sprintf("%d linha(s) sem valor em '%s' ficaram de fora.", sum(semz), cz))
    dados <- dados[!semz, , drop = FALSE]
  }
  coords <- as.matrix(dados[, c(cx, cy), drop = FALSE])
  if (nrow(unique(coords)) < 3L) {
    .tr_spatial_abort("tr_spatial_error_bad_coords",
      "Menos de três pontos distintos: não dá para medir dependência espacial.")
  }
  # Ponto coincidente é comum em malha de campo e o gstat lida com ele, mas
  # muda o efeito pepita: quem lê o variograma precisa saber que há.
  dup <- nrow(coords) - nrow(unique(coords))
  if (dup > 0L) {
    notas <- c(notas, sprintf(
      "%d ponto(s) coincidente(s): a variação entre eles entra no efeito pepita.", dup))
  }
  structure(list(
    dados = tibble::as_tibble(dados), coords = coords, coord_cols = c(cx, cy),
    variavel = cz, crs = obj_crs,
    unidade = if (nzchar(trimws(unidade))) trimws(unidade) else NA_character_,
    borda = .tr_spatial_borda(borda), covariaveis = cov,
    rotulo = if (nzchar(trimws(nome))) trimws(nome) else cz,
    nota = paste(notas, collapse = " ")),
    class = "tr_spatial_points")
}

#' Normaliza a borda numa matriz n×2 fechada, ou NULL.
#' @noRd
.tr_spatial_borda <- function(borda) {
  if (is.null(borda)) return(NULL)
  m <- as.matrix(as.data.frame(borda))
  if (ncol(m) != 2L || !is.numeric(m)) {
    .tr_spatial_abort("tr_spatial_error_bad_border",
      "A borda precisa de exatamente duas colunas numéricas (x e y).")
  }
  if (!all(is.finite(m))) {
    .tr_spatial_abort("tr_spatial_error_bad_border",
      "A borda tem coordenada faltante ou infinita.")
  }
  if (!identical(m[1, ], m[nrow(m), ])) m <- rbind(m, m[1, , drop = FALSE])
  if (nrow(unique(m)) < 3L) {
    .tr_spatial_abort("tr_spatial_error_bad_border",
      "A borda precisa de ao menos três vértices distintos para formar um polígono.")
  }
  unname(m)
}
