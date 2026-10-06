# O variograma empírico, sobre `gstat::variogram`.
#
# Duas convenções, documentadas na ajuda do bloco e travadas em teste:
#
# 1. DIREÇÃO. O param segue o `gstat`: graus, sentido horário, a partir do
#    Norte — a convenção da bússola, que é a que a pessoa espera ao digitar
#    "45". O `geoR` mede o azimute do mesmo jeito (horário, do Norte); só a
#    unidade muda, radianos em vez de graus. A conversão do teste é
#    `alpha * pi / 180`, e aparece lá, nunca aqui dentro.
# 2. CLASSES. O bloco toma o NÚMERO de classes, que é o que se pensa ao olhar
#    um variograma, e as monta como `seq(0, corte, length.out = n + 1)`. Os
#    limites superiores vão ao `gstat` em `boundaries`, o que fixa as classes
#    de forma exata e reprodutível (é o que permite comparar com o `geoR`).

.TR_SPATIAL_ESTIMADORES <- c("classico", "robusto")
.TR_SPATIAL_TENDENCIAS <- c("constante", "1a ordem", "2a ordem", "covariavel")

#' A fórmula do `gstat` para a tendência pedida.
#' @noRd
.tr_spatial_formula <- function(pontos, tendencia) {
  if (!tendencia %in% .TR_SPATIAL_TENDENCIAS) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Tendência: escolha um de %s.", paste(.TR_SPATIAL_TENDENCIAS, collapse = ", ")))
  }
  z <- pontos$variavel; cx <- pontos$coord_cols[[1]]; cy <- pontos$coord_cols[[2]]
  rhs <- switch(tendencia,
    "constante" = "1",
    "1a ordem"  = sprintf("%s + %s", cx, cy),
    "2a ordem"  = sprintf("%s + %s + I(%s^2) + I(%s^2) + I(%s * %s)", cx, cy, cx, cy, cx, cy),
    "covariavel" = {
      if (!length(pontos$covariaveis)) {
        .tr_spatial_abort("tr_spatial_error_blank_param", paste(
          "Tendência por covariável, mas o objeto espacial não declara nenhuma.",
          "Volte ao bloco Coordenadas e preencha 'Covariáveis'."))
      }
      paste(pontos$covariaveis, collapse = " + ")
    })
  stats::as.formula(sprintf("%s ~ %s", z, rhs))
}

#' O padrão do `gstat`: a diagonal da caixa envolvente dividida por três.
#' @noRd
.tr_spatial_corte_padrao <- function(coords) {
  r <- apply(coords, 2, range)
  sqrt(sum((r[2, ] - r[1, ])^2)) / 3
}

#' Variograma empírico de um objeto espacial.
#'
#' @param pontos um objeto espacial (`spatial/points`).
#' @param estimador `"classico"` (Matheron) ou `"robusto"` (Cressie-Hawkins).
#' @param dist_max distância máxima entre pares, na unidade das coordenadas;
#'   `NA` usa o padrão do `gstat` (a diagonal da caixa envolvente dividida por três).
#' @param n_classes número de classes de distância.
#' @param direcao direção em graus, sentido horário, a partir do Norte; `NA` é
#'   omnidirecional.
#' @param tolerancia meia-abertura angular, em graus.
#' @param tendencia `"constante"`, `"1a ordem"`, `"2a ordem"` ou `"covariavel"`.
#' @param pares_min classes com menos pares que isto saem, e a nota conta quantas.
#' @return um variograma empírico (`spatial/variogram`).
#' @export
tr_spatial_variogram <- function(pontos, estimador = "classico", dist_max = NA,
                                 n_classes = 15L, direcao = NA, tolerancia = 22.5,
                                 tendencia = "constante", pares_min = 30L) {
  .tr_spatial_pontos_conferir(pontos)
  if (!estimador %in% .TR_SPATIAL_ESTIMADORES) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Estimador: escolha um de %s.", paste(.TR_SPATIAL_ESTIMADORES, collapse = ", ")))
  }
  n_classes <- as.integer(n_classes)
  if (is.na(n_classes) || n_classes < 3L) {
    .tr_spatial_abort("tr_spatial_error_bad_option", "Classes: use ao menos 3.")
  }
  pares_min <- suppressWarnings(as.integer(pares_min))
  if (length(pares_min) != 1L || is.na(pares_min) || pares_min < 1L) {
    .tr_spatial_abort("tr_spatial_error_bad_option", "Mínimo de pares: use um inteiro a partir de 1.")
  }
  if (!is.na(direcao) && (!is.finite(direcao) || direcao < 0 || direcao > 360)) {
    .tr_spatial_abort("tr_spatial_error_bad_option", "Direção: use graus entre 0 e 360.")
  }
  f <- .tr_spatial_formula(pontos, tendencia)
  d <- as.data.frame(pontos$dados)
  corte <- if (is.na(dist_max)) .tr_spatial_corte_padrao(pontos$coords) else as.numeric(dist_max)
  if (!is.finite(corte) || corte <= 0) {
    .tr_spatial_abort("tr_spatial_error_bad_option", "Distância máxima: use um número positivo.")
  }
  lim <- seq(0, corte, length.out = n_classes + 1L)
  # As coordenadas em metros (~1e6) elevadas ao quadrado (~1e12) deixam a
  # regressão da tendência mal condicionada no `gstat`: o erro relativo chegou
  # a 5e-8 na 2a ordem, contra 1e-13 no `geoR`. Os resíduos de um polinômio
  # não mudam com a mudança afim das coordenadas, então a regressão usa as
  # colunas centradas e padronizadas, e as posições dos pares seguem
  # nas coordenadas originais (`.x_`, `.y_`).
  d$.x_ <- pontos$coords[, 1]; d$.y_ <- pontos$coords[, 2]
  for (k in 1:2) {
    col <- pontos$coord_cols[[k]]
    d[[col]] <- (d[[col]] - mean(d[[col]])) / stats::sd(d[[col]])
  }
  loc <- stats::as.formula("~ .x_ + .y_")
  args <- list(object = f, locations = loc, data = d, boundaries = lim[-1],
               cressie = identical(estimador, "robusto"))
  if (!is.na(direcao)) { args$alpha <- as.numeric(direcao); args$tol.hor <- as.numeric(tolerancia) }
  v <- do.call(gstat::variogram, args)

  t <- tibble::tibble(u = as.numeric(v$dist), gamma = as.numeric(v$gamma),
                      np = as.integer(v$np),
                      direcao = if (is.na(direcao)) NA_real_ else as.numeric(direcao))
  t <- t[t$np >= pares_min, , drop = FALSE]
  # O `gstat` já descarta classes vazias; conta-se contra as pedidas.
  cortadas <- n_classes - nrow(t)
  if (!nrow(t)) {
    .tr_spatial_abort("tr_spatial_error_empty_variogram", sprintf(paste(
      "Nenhuma classe de distância chegou a %d pares.",
      "Baixe o mínimo de pares, use menos classes, ou aumente a distância máxima."),
      pares_min))
  }
  notas <- character()
  if (cortadas > 0L) {
    notas <- c(notas, sprintf("%d das %d classes pedidas ficaram de fora, por terem menos de %d pares.",
                              cortadas, n_classes, pares_min))
  }
  if (nzchar(pontos$nota)) notas <- c(notas, pontos$nota)
  structure(list(
    tabela = t, estimador = estimador, dist_max = corte, n_classes = n_classes,
    direcao = if (is.na(direcao)) NA_real_ else as.numeric(direcao),
    tolerancia = if (is.na(direcao)) NA_real_ else as.numeric(tolerancia),
    tendencia = tendencia, variavel = pontos$variavel, unidade = pontos$unidade,
    pontos = pontos, nota = paste(notas, collapse = " ")),
    class = "tr_spatial_variogram")
}
