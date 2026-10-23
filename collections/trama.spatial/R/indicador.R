# A transformação em indicador, para a krigagem indicadora.
#
# A krigagem indicadora é a krigagem ORDINÁRIA de uma variável 0/1, e o que a
# torna indicadora é o variograma ser DO INDICADOR. Por isso esta transformação
# é um bloco ANTES do variograma, e não um valor do param `tipo` da krigagem: um
# valor de tipo receberia um modelo ajustado à variável contínua e krigaria
# indicador com ele, em silêncio e errado.
#
# O corte fica VISÍVEL no canvas, que é a outra razão de ser um bloco: é a
# decisão analítica central desta técnica.

.TR_SPATIAL_SENTIDOS <- c("<=", ">")

#' Transforma a variável num indicador 0/1.
#'
#' @param pontos um objeto espacial (`spatial/points`).
#' @param corte o valor do corte, na unidade da variável.
#' @param sentido `<=` (probabilidade de NÃO exceder) ou `>` (de exceder).
#' @return um objeto espacial cuja variável é o indicador.
#' @export
tr_spatial_indicator <- function(pontos, corte, sentido = "<=") {
  .tr_spatial_pontos_conferir(pontos)
  if (!is.numeric(corte) || length(corte) != 1L || !is.finite(corte)) {
    .tr_spatial_abort("tr_spatial_error_blank_param",
      "Corte: informe um número, na unidade da variável.")
  }
  if (!is.character(sentido) || length(sentido) != 1L ||
      !sentido %in% .TR_SPATIAL_SENTIDOS) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Sentido: escolha um de %s.", paste(.TR_SPATIAL_SENTIDOS, collapse = ", ")))
  }
  z <- pontos$dados[[pontos$variavel]]
  ind <- if (identical(sentido, "<=")) as.numeric(z <= corte) else
    as.numeric(z > corte)
  if (length(unique(ind)) < 2L) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(paste(
      "Corte %s deixa o indicador CONSTANTE (%s em todos os %d pontos), e",
      "variograma de constante não tem o que dizer. A variável vai de %s a %s:",
      "escolha um corte dentro desse intervalo."),
      format(signif(corte, 6)), format(unique(ind)), length(ind),
      format(signif(min(z), 6)), format(signif(max(z), 6))))
  }
  nome <- sprintf("I(%s %s %s)", pontos$variavel, sentido,
                  format(signif(corte, 6)))
  d <- pontos$dados
  d[[nome]] <- ind
  out <- pontos
  out$dados <- d
  out$variavel <- nome
  out$unidade <- NA_character_
  out$indicador <- list(corte = as.numeric(corte), sentido = sentido,
                        variavel_original = pontos$variavel)
  out$rotulo <- nome
  out$nota <- paste(c(
    sprintf("Indicador de %s %s %s: %d de %d pontos valem 1 (p = %.3f).",
            pontos$variavel, sentido, format(signif(corte, 6)),
            sum(ind), length(ind), mean(ind)),
    "A krigagem deste objeto estima PROBABILIDADE, não a variável.",
    if (nzchar(pontos$nota)) pontos$nota), collapse = " ")
  out
}

#' Recorta o predito em [0,1] quando os pontos são indicador.
#'
#' A krigagem de um indicador sai de [0,1] de verdade: medido em 2026-10-09, até
#' 14 de 144 células fora, com mínimo -0,105 e máximo 1,058. Probabilidade
#' negativa não é resultado, e deixá-la passar põe no mapa uma cor que não
#' existe na escala.
#'
#' A contagem viaja no atributo `n_recortadas`, para a nota do card dizer
#' quantas células precisaram do recorte — recorte silencioso esconderia um
#' modelo ruim.
#' @noRd
.tr_spatial_recorta_indicador <- function(pred, pontos) {
  if (is.null(pontos$indicador)) return(pred)
  ok <- !is.na(pred)
  n <- sum(pred[ok] < 0 | pred[ok] > 1)
  out <- pred
  out[ok] <- pmin(pmax(pred[ok], 0), 1)
  attr(out, "n_recortadas") <- as.integer(n)
  out
}
