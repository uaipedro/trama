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
#' @param borda a borda da área de estudo: um objeto `spatial/boundary`, uma
#'   matriz ou data.frame de duas colunas com o polígono, ou `NULL`.
#' @param borda_modo o que fazer quando nenhuma borda é ligada: `nenhuma`, ou
#'   `casco convexo dos pontos`. Borda ligada VENCE este param.
#' @return um objeto espacial (`spatial/points`).
#' @export
tr_spatial_coordinates <- function(dados, x, y, variavel, covariaveis = "",
                                   crs = "", unidade = "", nome = "", borda = NULL,
                                   borda_modo = "nenhuma") {
  if (!borda_modo %in% .TR_SPATIAL_BORDA_MODOS) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Borda: escolha um de %s.", paste(.TR_SPATIAL_BORDA_MODOS, collapse = ", ")))
  }
  dados <- as.data.frame(dados)
  cx <- .tr_spatial_col1(dados, x, "Coordenada X")
  cy <- .tr_spatial_col1(dados, y, "Coordenada Y")
  if (identical(cx, cy)) {
    .tr_spatial_abort("tr_spatial_error_bad_coords", "As duas coordenadas são a mesma coluna.")
  }
  cz <- .tr_spatial_col1(dados, variavel, "Variável")
  cov <- .tr_spatial_cols(dados, covariaveis, "Covariáveis")
  # O variograma padroniza as colunas de coordenada no lugar (ver variograma.R).
  # Variável ou covariável que seja uma delas seria padronizada junto, e o
  # variograma sairia errado por sd(x)^2, sem erro e com gráfico plausível.
  if (cz %in% c(cx, cy)) {
    .tr_spatial_abort("tr_spatial_error_bad_coords", sprintf(
      "Variável: a coluna '%s' é uma das coordenadas. Use uma coluna de valores, não de posição.", cz))
  }
  colide <- intersect(cov, c(cx, cy))
  if (length(colide)) {
    .tr_spatial_abort("tr_spatial_error_bad_coords", sprintf(
      "Covariáveis: %s é coordenada, e não pode ser também covariável.",
      paste(sprintf("'%s'", colide), collapse = ", ")))
  }
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
  # Covariável com faltante: o `gstat` usa `na.fail`, e quem chegasse ao
  # variograma com tendência por covariável — ou ao KED — morreria com
  # "valores em falta em objeto", sem classe nossa e sem dizer qual coluna.
  # A guarda espelha a da grade do KED, que é exemplar.
  for (cc in cov) {
    ruins <- sum(!is.finite(dados[[cc]]))
    if (ruins > 0L) {
      .tr_spatial_abort("tr_spatial_error_bad_coords", sprintf(paste(
        "A covariável '%s' tem %d valor(es) faltante(s) ou infinito(s).",
        "Tendência por covariável e deriva externa não funcionam com buraco:",
        "preencha a coluna, ou tire-a de 'Covariáveis'."), cc, ruins))
    }
  }
  # A borda pode vir de três lugares, nesta ordem de precedência: um objeto
  # `spatial/boundary` ligado na porta, uma matriz passada direto (é como os
  # exemplos a trazem), ou o casco convexo dos próprios pontos.
  if (inherits(borda, "tr_spatial_boundary")) {
    b <- .tr_spatial_borda_crs(borda, obj_crs)
    if (!is.null(attr(b, "nota"))) notas <- c(notas, attr(b, "nota"))
    attr(b, "nota") <- NULL
    borda <- b
  } else if (is.null(borda) && identical(borda_modo, "casco convexo dos pontos")) {
    borda <- tr_spatial_convex_hull(coords)
    notas <- c(notas, paste("Borda: casco convexo dos pontos amostrais.",
                            "Fora dele toda predição é extrapolação."))
  } else {
    borda <- .tr_spatial_borda(borda)
  }
  # COORDENADA QUE PARECE GRAU, COM CRS VAZIO. `.tr_spatial_crs()` recusa CRS
  # geográfico DECLARADO, mas campo vazio virava "plano arbitrário" e o
  # variograma media distância em grau sem uma palavra — o modo de falha que o
  # cabeçalho de `R/errors.R` chama de "o que sai verde". O leitor de vetor
  # criou o caminho de um clique para isso (GeoJSON é lon/lat por
  # especificação), então a nota mora aqui.
  #
  # É NOTA e não erro de propósito: coordenada de um ensaio de 100 m em metro
  # também cabe em [-180, 180], e recusá-la quebraria um caso legítimo. O que
  # distingue grau de metro não está nos números.
  if (!inherits(obj_crs, "crs") && .tr_spatial_parece_grau(coords)) {
    notas <- c(notas, paste(
      "As coordenadas parecem estar em grau (cabem em ±180 e ±90) e nenhum CRS",
      "foi declarado. Se forem grau, a distância do variograma NÃO é métrica:",
      "declare o CRS projetado da região, ou reprojete na leitura do arquivo."))
  }
  .tr_spatial_borda_contem(borda, coords)
  structure(list(
    dados = tibble::as_tibble(dados), coords = coords, coord_cols = c(cx, cy),
    variavel = cz, crs = obj_crs,
    unidade = if (nzchar(trimws(unidade))) trimws(unidade) else NA_character_,
    borda = borda, covariaveis = cov,
    rotulo = if (nzchar(trimws(nome))) trimws(nome) else cz,
    nota = paste(notas, collapse = " ")),
    class = "tr_spatial_points")
}

# Folga, em unidades de coordenada, entre a borda e o ponto mais afastado dela.
# Os exemplos da coleção têm até 673 m de ponto fora do polígono (a sede cai
# fora da malha simplificada), então 2 km NÃO pode ser apertado. Está em metros:
# com coordenadas em outra unidade a folga é outra, e é pela escala dos dados
# em metros que a coleção trabalha.
.TR_SPATIAL_FOLGA_BORDA <- 2000

#' Uma borda que não contém os dados está errada: escala, projeção, ordem das
#' colunas ou lugar. A guarda mora aqui, onde a borda entra no sistema.
#' @noRd
.tr_spatial_borda_contem <- function(borda, coords) {
  if (is.null(borda)) return(invisible(NULL))
  fora <- tryCatch({
    zona <- sf::st_buffer(sf::st_sfc(sf::st_polygon(list(borda))), .TR_SPATIAL_FOLGA_BORDA)
    pts <- sf::st_as_sf(as.data.frame(coords), coords = 1:2)
    lengths(sf::st_intersects(pts, zona)) == 0L
  }, error = function(e) {
    .tr_spatial_abort("tr_spatial_error_bad_border", paste(
      "A borda não forma um polígono utilizável:", conditionMessage(e)))
  })
  if (any(fora)) {
    .tr_spatial_abort("tr_spatial_error_bad_border", sprintf(paste(
      "%d de %d pontos amostrais ficam fora da borda (a mais de %g de distância dela).",
      "A borda provavelmente está em outra unidade, outra projeção ou outro lugar",
      "que as coordenadas, ou com x e y trocados."),
      sum(fora), length(fora), .TR_SPATIAL_FOLGA_BORDA))
  }
  invisible(NULL)
}

#' As coordenadas cabem na faixa de latitude e longitude?
#'
#' Não prova que são grau — um ensaio de 100 m em metro também cabe. Serve só
#' para a nota, que diz "parecem" e manda declarar o CRS.
#' @noRd
.tr_spatial_parece_grau <- function(coords) {
  all(abs(coords[, 1]) <= 180) && all(abs(coords[, 2]) <= 90)
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
