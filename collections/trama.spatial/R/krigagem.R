# A krigagem, sobre `gstat::krige`.
#
# Mapeamento conferido na documentacao do gstat em 2026-10-06:
#   ordinaria -> formula z ~ 1, SEM beta
#   simples   -> formula z ~ 1, COM beta = media conhecida
# A saida traz `var1.pred` e `var1.var`.
#
# Condicionamento: as duas formulas sao `z ~ 1`, sem termo de coordenada no lado
# direito. O problema que a tendencia polinomial tem em UTM cru (quadrados de
# ~1e12, ver variograma.R) nao existe aqui; distancias e covariancias sao
# lineares na escala. Se a krigagem universal entrar, ela precisa da mesma
# centragem do variograma.
#
# A grade nasce aqui e nao num bloco proprio: ela e consequencia da borda e de
# um parametro de resolucao, nao uma decisao analitica autonoma.

# Valores do param `tipo`. A universal e a de indicadora entram como valores
# novos desta lista, sem bloco novo.
.TR_SPATIAL_TIPOS_KRIG <- c("ordinaria", "simples")

#' Valida as opcoes da krigagem. Roda ANTES da grade e do motor.
#' @noRd
.tr_spatial_krig_opcoes <- function(tipo, media, vizinhos_max, dist_max) {
  if (!is.character(tipo) || length(tipo) != 1L || !tipo %in% .TR_SPATIAL_TIPOS_KRIG) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Tipo: escolha um de %s.", paste(.TR_SPATIAL_TIPOS_KRIG, collapse = ", ")))
  }
  # O gstat ACEITA a simples sem beta e resolve com media zero, em silencio.
  if (identical(tipo, "simples") &&
      (!is.numeric(media) || length(media) != 1L || !is.finite(media))) {
    .tr_spatial_abort("tr_spatial_error_no_mean", paste(
      "Krigagem simples supoe a media da populacao CONHECIDA, e ela nao foi informada.",
      "Preencha 'Media', ou use a krigagem ordinaria, que a estima a partir dos dados."))
  }
  # NA puro e logico; o campo vazio do card chega assim.
  vazio <- function(x) length(x) == 1L && is.na(x)
  positivo <- function(x, inteiro) {
    is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0 &&
      (!inteiro || (x >= 1 && x == round(x)))
  }
  if (!vazio(vizinhos_max) && !positivo(vizinhos_max, TRUE)) {
    .tr_spatial_abort("tr_spatial_error_bad_option",
      "Vizinhos: use um inteiro a partir de 1, ou deixe vazio para krigar com todos.")
  }
  if (!vazio(dist_max) && !positivo(dist_max, FALSE)) {
    .tr_spatial_abort("tr_spatial_error_bad_option",
      "Raio: use um numero positivo, ou deixe vazio para krigar com todos.")
  }
  invisible(tipo)
}

#' Grade de predição: retângulo que cobre os pontos e a borda, recortado na borda.
#'
#' O retângulo é a união da extensão dos pontos com a da borda. Se fosse só o da
#' borda, uma borda em escala errada (quilômetros contra metros) produziria uma
#' grade completa e plausível, na escala errada, em vez de uma grade vazia.
#'
#' @param pontos um objeto espacial (`spatial/points`).
#' @param resolucao pontos no lado maior.
#' @return data.frame com as duas colunas de coordenada.
#' @export
tr_spatial_grid <- function(pontos, resolucao = 60L) {
  .tr_spatial_pontos_conferir(pontos)
  if (!is.numeric(resolucao) || length(resolucao) != 1L || !is.finite(resolucao) ||
      resolucao < 5 || resolucao > 500 || resolucao != round(resolucao)) {
    .tr_spatial_abort("tr_spatial_error_bad_option",
      "Resolucao: use um inteiro entre 5 e 500.")
  }
  resolucao <- as.integer(resolucao)
  xs <- pontos$coords[, 1]; ys <- pontos$coords[, 2]
  if (!is.null(pontos$borda)) { xs <- c(xs, pontos$borda[, 1]); ys <- c(ys, pontos$borda[, 2]) }
  lx <- diff(range(xs)); ly <- diff(range(ys))
  maior <- max(lx, ly)
  nx <- if (lx > 0) max(2L, as.integer(round(resolucao * lx / maior))) else 1L
  ny <- if (ly > 0) max(2L, as.integer(round(resolucao * ly / maior))) else 1L
  g <- expand.grid(seq(min(xs), max(xs), length.out = nx),
                   seq(min(ys), max(ys), length.out = ny))
  names(g) <- pontos$coord_cols
  if (!is.null(pontos$borda)) {
    pol <- sf::st_sfc(sf::st_polygon(list(pontos$borda)))
    pts <- sf::st_as_sf(g, coords = pontos$coord_cols)
    g <- g[lengths(sf::st_intersects(pts, pol)) > 0L, , drop = FALSE]
  }
  if (!nrow(g)) {
    .tr_spatial_abort("tr_spatial_error_empty_grid", paste(
      "A grade ficou vazia depois do recorte na borda: nenhuma celula caiu dentro dela.",
      "Confira se a borda esta na mesma unidade e na mesma ordem de colunas que as coordenadas."))
  }
  rownames(g) <- NULL
  g
}

#' Krigagem em pontos arbitrários. É aqui que a conta acontece.
#'
#' `novos` traz as duas colunas de coordenada, pelo nome quando ambas existem e
#' pela posição quando não.
#' @noRd
tr_spatial_kriging_em <- function(pontos, modelo, novos, tipo = "ordinaria",
                                  media = NA, vizinhos_max = NA, dist_max = NA) {
  .tr_spatial_pontos_conferir(pontos)
  .tr_spatial_modelo_conferir(modelo)
  .tr_spatial_krig_opcoes(tipo, media, vizinhos_max, dist_max)
  cx <- pontos$coord_cols[[1]]; cy <- pontos$coord_cols[[2]]
  novos <- as.data.frame(novos)
  if (all(c(cx, cy) %in% names(novos))) {
    novos <- novos[, c(cx, cy), drop = FALSE]
  } else {
    novos <- novos[, 1:2, drop = FALSE]
    names(novos) <- c(cx, cy)
  }
  # Nomes internos: a coluna da variavel pode ter nome nao sintatico.
  d <- data.frame(.z_ = pontos$dados[[pontos$variavel]],
                  .x_ = pontos$coords[, 1], .y_ = pontos$coords[, 2])
  nd <- data.frame(.x_ = novos[[1]], .y_ = novos[[2]])
  args <- list(formula = .z_ ~ 1, locations = ~ .x_ + .y_, data = d, newdata = nd,
               model = .tr_spatial_vgm_model(modelo), debug.level = 0)
  if (identical(tipo, "simples")) args$beta <- as.numeric(media)
  if (!is.na(vizinhos_max)) args$nmax <- as.integer(vizinhos_max)
  if (!is.na(dist_max)) args$maxdist <- as.numeric(dist_max)
  k <- do.call(gstat::krige, args)
  v <- as.numeric(k$var1.var)
  ok <- is.finite(v)
  # Variancia negativa e sintoma de modelo nao definido positivo. Deixar passar
  # produz NaN na raiz, longe da causa. O limite acompanha a escala do modelo:
  # um valor absoluto seria ruido de arredondamento para dados em kg2/ha2 e
  # erro grosseiro para dados em escala unitaria.
  if (any(v[ok] < -1e-8 * (modelo$pepita + modelo$contribuicao))) {
    .tr_spatial_abort("tr_spatial_error_negative_variance", paste(
      "A variancia de krigagem saiu negativa em alguma celula:",
      "o modelo ajustado nao e definido positivo nesta configuracao.",
      "Troque a familia (o gaussiano sem pepita e o caso mais comum)",
      "ou acrescente um efeito pepita."))
  }
  v[ok] <- pmax(v[ok], 0)
  data.frame(novos, predito = as.numeric(k$var1.pred), variancia = v,
             erro_padrao = sqrt(v))
}

#' Krigagem numa grade que cobre a área.
#'
#' @param pontos um objeto espacial (`spatial/points`).
#' @param modelo um modelo ajustado (`spatial/model`).
#' @param tipo `"ordinaria"` ou `"simples"`.
#' @param media a média conhecida; obrigatória na simples.
#' @param resolucao pontos no lado maior da grade.
#' @param vizinhos_max,dist_max vizinhança local; `NA` kriga globalmente.
#' @return uma superfície predita (`spatial/surface`).
#' @export
tr_spatial_kriging <- function(pontos, modelo, tipo = "ordinaria", media = NA,
                               resolucao = 60L, vizinhos_max = NA, dist_max = NA) {
  .tr_spatial_krig_opcoes(tipo, media, vizinhos_max, dist_max)
  g <- tr_spatial_grid(pontos, resolucao)
  grade <- tr_spatial_kriging_em(pontos, modelo, g, tipo, media, vizinhos_max, dist_max)
  viz <- if (is.na(vizinhos_max) && is.na(dist_max)) "global" else paste(
    c(if (!is.na(vizinhos_max)) sprintf("ate %d vizinhos", as.integer(vizinhos_max)),
      if (!is.na(dist_max)) sprintf("raio de %g", as.numeric(dist_max))), collapse = ", ")
  nota <- paste(c(modelo$nota, sprintf("Grade de %d células (resolução %d).",
                                       nrow(grade), as.integer(resolucao))), collapse = " ")
  nota <- trimws(nota)
  sem <- sum(is.na(grade$predito))
  if (sem > 0L) {
    nota <- paste(c(nota, sprintf(paste(
      "%d de %d células ficaram sem predição: nenhum ponto dentro da vizinhança (%s).",
      "Aumente o raio ou deixe-o vazio."), sem, nrow(grade), viz)), collapse = " ")
  }
  structure(list(
    grade = tibble::as_tibble(grade), tipo = tipo, modelo = modelo, pontos = pontos,
    borda = pontos$borda, resolucao = as.integer(resolucao), vizinhanca = viz,
    variavel = pontos$variavel, unidade = pontos$unidade, nota = nota),
    class = "tr_spatial_surface")
}
