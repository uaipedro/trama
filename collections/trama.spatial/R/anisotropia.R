# Variogramas direcionais num objeto só, para diagnosticar anisotropia.
#
# Motor: `gstat::variogram(alpha = c(...))`, que devolve as N direções numa
# tabela com a coluna `dir.hor`. Nenhum motor novo; a tendência passa pelo mesmo
# `.tr_spatial_dados_tendencia()` do variograma.
#
# O bloco NÃO estima razão nem ângulo de anisotropia, e isso é decisão MEDIDA,
# não omissão. Três implementações foram testadas em 2026-10-09 e as três
# reprovaram (detalhe em `docs/revisao-metodologica.md`):
#   1. ajuste do alcance direção por direção, patamar livre: o otimizador fugiu
#      (alcance 31.393 num campo de alcance 150), razão 116,7 contra verdade 3;
#   2. busca em grade sobre (ângulo, razão) comparando `SSErr`: num variograma
#      SEM RUÍDO, gerado do próprio modelo, devolveu 90/5,5 em vez de 60/3 —
#      porque `fit.variogram` não usa `dir.hor` e trata as curvas como nuvem só;
#   3. regra descritiva (cruzamento de 95% da variância): em campos
#      anisotrópicos deu 3,02 / 3,11 / 2,64 / 3,06 / 1,25, e em campos
#      ISOTRÓPICOS deu 1,49 / 1,70 / 1,92 / 2,25 / 3,01 — as duas distribuições
#      se sobrepõem por inteiro.
# Razão e ângulo são DIGITADOS no `spatial/variogram_fit`, lidos deste card.

.TR_SPATIAL_CAMPOS_ANISO <- c("tabela", "direcoes", "estimador", "dist_max",
                              "n_classes", "tolerancia", "tendencia", "envelope",
                              "n_sim", "semente", "variavel", "unidade", "pontos",
                              "nota")

#' Lê o param `direcoes` ("0,45,90,135") num vetor de graus.
#' @noRd
.tr_spatial_direcoes <- function(direcoes) {
  bruto <- strsplit(paste(as.character(direcoes), collapse = ","), ",",
                    fixed = TRUE)[[1]]
  v <- suppressWarnings(as.numeric(trimws(bruto)))
  if (!length(v) || anyNA(v) || any(v < 0 | v >= 180)) {
    .tr_spatial_abort("tr_spatial_error_bad_option", paste(
      "Direções: use números de 0 (inclusive) a 180 (exclusive), separados por",
      "vírgula, por exemplo '0,45,90,135'. O variograma não distingue uma",
      "direção da oposta, então 180 em diante repetiria curva."))
  }
  if (length(v) < 2L) {
    .tr_spatial_abort("tr_spatial_error_bad_option", paste(
      "Direções: com uma direção só não há o que comparar. Use ao menos duas,",
      "ou o bloco `spatial/variogram`, que tem o param Direção."))
  }
  if (anyDuplicated(v) > 0L) {
    .tr_spatial_abort("tr_spatial_error_bad_option",
      "Direções: há direção repetida na lista.")
  }
  sort(v)
}

#' Variogramas direcionais, para diagnosticar anisotropia.
#'
#' Anisotropia é a dependência espacial ter alcance diferente conforme a
#' direção: o variograma omnidirecional a esconde, porque mistura todas. Este
#' bloco separa, e a leitura é **visual** — ele não faz teste de hipótese.
#'
#' @param pontos um objeto espacial (`spatial/points`).
#' @param direcoes graus, horário a partir do Norte, separados por vírgula.
#' @param estimador `classico` ou `robusto`.
#' @param dist_max corte das distâncias; `NA` usa o padrão da coleção.
#' @param n_classes número de classes de distância.
#' @param tolerancia meia-abertura angular, em graus.
#' @param tendencia tendência removida antes, como no `spatial/variogram`.
#' @param pares_min mínimo de pares para a classe entrar.
#' @return um objeto de anisotropia (`spatial/anisotropy`).
#' @export
tr_spatial_anisotropy <- function(pontos, direcoes = "0,45,90,135",
                                  estimador = "classico", dist_max = NA,
                                  n_classes = 10L, tolerancia = 22.5,
                                  tendencia = "constante", pares_min = 30L) {
  .tr_spatial_pontos_conferir(pontos)
  dirs <- .tr_spatial_direcoes(direcoes)
  if (!estimador %in% .TR_SPATIAL_ESTIMADORES) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Estimador: escolha um de %s.", paste(.TR_SPATIAL_ESTIMADORES, collapse = ", ")))
  }
  n_classes <- suppressWarnings(as.integer(n_classes))
  if (length(n_classes) != 1L || is.na(n_classes) || n_classes < 3L) {
    .tr_spatial_abort("tr_spatial_error_bad_option", "Classes: use ao menos 3.")
  }
  pares_min <- suppressWarnings(as.integer(pares_min))
  if (length(pares_min) != 1L || is.na(pares_min) || pares_min < 1L) {
    .tr_spatial_abort("tr_spatial_error_bad_option",
      "Mínimo de pares: use um inteiro a partir de 1.")
  }
  if (!is.numeric(tolerancia) || length(tolerancia) != 1L ||
      !is.finite(tolerancia) || tolerancia <= 0 || tolerancia > 90) {
    .tr_spatial_abort("tr_spatial_error_bad_option",
      "Tolerância: use um número maior que 0 e até 90 graus.")
  }
  corte <- if (is.na(dist_max)) .tr_spatial_corte_padrao(pontos$coords) else
    as.numeric(dist_max)
  if (!is.finite(corte) || corte <= 0) {
    .tr_spatial_abort("tr_spatial_error_bad_option",
      "Distância máxima: use um número positivo.")
  }
  lim <- seq(0, corte, length.out = n_classes + 1L)
  dt <- .tr_spatial_dados_tendencia(pontos, tendencia)
  v <- gstat::variogram(dt$formula, locations = dt$locations, data = dt$dados,
                        alpha = dirs, tol.hor = as.numeric(tolerancia),
                        boundaries = lim[-1],
                        cressie = identical(estimador, "robusto"))
  tab <- tibble::tibble(direcao = as.numeric(v$dir.hor), u = as.numeric(v$dist),
                        gamma = as.numeric(v$gamma), np = as.integer(v$np))
  tab <- tab[order(tab$direcao, tab$u), , drop = FALSE]
  tab <- tab[tab$np >= pares_min, , drop = FALSE]
  if (!nrow(tab)) {
    .tr_spatial_abort("tr_spatial_error_empty_variogram", sprintf(paste(
      "Nenhuma classe chegou a %d pares. Com %d direções cada classe recebe uma",
      "fração dos pares: aumente a tolerância angular, use menos classes, ou",
      "menos direções."), pares_min, length(dirs)))
  }
  faltam <- setdiff(dirs, unique(tab$direcao))
  notas <- c(sprintf("%d direções, tolerância de %g graus, %d classes até %s.",
                     length(dirs), tolerancia, n_classes,
                     .tr_spatial_num(corte)),
             if (length(faltam)) sprintf(
               "Sem pares bastantes nas direções %s, que ficaram de fora.",
               paste(faltam, collapse = ", ")),
             if (nzchar(pontos$nota)) pontos$nota)
  structure(list(
    tabela = tab, direcoes = dirs, estimador = estimador, dist_max = corte,
    n_classes = n_classes, tolerancia = as.numeric(tolerancia),
    tendencia = tendencia, envelope = NULL, n_sim = NA_integer_,
    semente = NA_real_, variavel = pontos$variavel, unidade = pontos$unidade,
    pontos = pontos, nota = paste(notas, collapse = " ")),
    class = "tr_spatial_anisotropy")
}
