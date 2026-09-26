# Os dados de exemplo: um conjunto por técnica, reais do R e simulados com
# efeito PLANTADO.
#
# Os simulados são o que ensina a ler um quadro: a ajuda diz qual híbrido foi
# construído para produzir mais, e o `models/emmeans` tem de achá-lo. Semente e
# gerador próprios, devolvendo o estado do RNG como estava — o `milho_dbc` de
# hoje é o de amanhã, em qualquer computador.

.TR_MODELS_EXEMPLOS <- c("PlantGrowth", "milho_dbc", "racao_dql", "adubo_dbc", "ToothGrowth", "warpbreaks",
                         "npk", "aveia", "sleepstudy", "cbpp", "grouseticks", "InsectSprays", "Puromycin",
                         "mtcars", "cars")

#' Roda `expr` com semente própria, sem mexer na do usuário.
#' @noRd
.tr_models_com_semente <- function(seed, expr) {
  tem <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (tem) antigo <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  antigo_kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(antigo_kind))
    if (tem) assign(".Random.seed", antigo, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  force(expr)
}

#' DBC: 5 híbridos de milho em 4 blocos. H3 foi plantado 1,2 t/ha acima.
#' @noRd
.tr_models_milho <- function() {
  .tr_models_com_semente(2026L, {
    hibrido <- factor(paste0("H", 1:5))
    bloco <- factor(paste0("B", 1:4))
    d <- expand.grid(bloco = bloco, hibrido = hibrido)
    efeito_h <- c(H1 = 0, H2 = 0.3, H3 = 1.2, H4 = -0.2, H5 = 0.4)
    efeito_b <- c(B1 = -0.6, B2 = 0.1, B3 = 0.5, B4 = 0)
    d$producao <- round(8 + efeito_h[as.character(d$hibrido)] + efeito_b[as.character(d$bloco)] +
                          stats::rnorm(nrow(d), sd = 0.35), 2)
    tibble::as_tibble(d[, c("bloco", "hibrido", "producao")])
  })
}

#' DQL 5 × 5: rações em cinco períodos e cinco lotes. R5 foi plantada melhor.
#' @noRd
.tr_models_racao <- function() {
  .tr_models_com_semente(1935L, {
    k <- 5L
    quadrado <- outer(0:(k - 1L), 0:(k - 1L), function(i, j) (i + j) %% k) + 1L
    quadrado <- quadrado[sample(k), sample(k)]
    d <- expand.grid(periodo = paste0("P", 1:k), lote = paste0("L", 1:k), stringsAsFactors = FALSE)
    d$racao <- paste0("R", quadrado[cbind(match(d$periodo, paste0("P", 1:k)), match(d$lote, paste0("L", 1:k)))])
    ef_r <- c(R1 = 0, R2 = 1, R3 = 0.5, R4 = -0.5, R5 = 3)
    ef_p <- stats::setNames(c(-2, -1, 0, 1, 2), paste0("P", 1:k))
    d$ganho_peso <- round(30 + ef_r[d$racao] + ef_p[d$periodo] + stats::rnorm(nrow(d), sd = 0.8), 1)
    tibble::as_tibble(d)
  })
}

#' DBC: 5 doses de nitrogênio em 4 blocos, com resposta QUADRÁTICA plantada.
#'
#' 3 + 0,04 N − 0,00015 N² t/ha: a máxima eficiência técnica está em
#' N = 0,04 / 0,0003 ≈ 133 kg/ha, dentro das doses testadas — o
#' `models/polinomial` tem de escolher o grau 2 e achar a MET perto daí.
#' @noRd
.tr_models_adubo <- function() {
  .tr_models_com_semente(1974L, {
    d <- expand.grid(bloco = factor(paste0("B", 1:4)), dose = c(0, 50, 100, 150, 200))
    ef_b <- c(B1 = -0.2, B2 = 0, B3 = 0.15, B4 = 0.05)
    d$producao <- round(3 + 0.04 * d$dose - 0.00015 * d$dose^2 + ef_b[as.character(d$bloco)] +
                          stats::rnorm(nrow(d), sd = 0.25), 2)
    tibble::as_tibble(d[, c("bloco", "dose", "producao")])
  })
}

#' Carrega um conjunto de exemplo.
#' @param dataset nome do conjunto (ver a ajuda do nó).
#' @return tibble.
#' @export
tr_models_example <- function(dataset = "PlantGrowth") {
  dataset <- .tr_models_enum(dataset, .TR_MODELS_EXEMPLOS, "dataset")
  switch(dataset,
    PlantGrowth = tibble::as_tibble(datasets::PlantGrowth),
    milho_dbc = .tr_models_milho(),
    racao_dql = .tr_models_racao(),
    adubo_dbc = .tr_models_adubo(),
    ToothGrowth = tibble::as_tibble(datasets::ToothGrowth),
    warpbreaks = tibble::as_tibble(datasets::warpbreaks),
    npk = tibble::as_tibble(datasets::npk),
    aveia = {
      o <- MASS::oats
      tibble::tibble(bloco = factor(o$B, ordered = FALSE), variedade = factor(o$V),
                     nitrogenio = factor(o$N), producao = o$Y)
    },
    sleepstudy = tibble::as_tibble(lme4::sleepstudy),
    InsectSprays = tibble::as_tibble(datasets::InsectSprays),
    Puromycin = tibble::as_tibble(datasets::Puromycin),
    mtcars = tibble::as_tibble(cbind(modelo = rownames(datasets::mtcars), datasets::mtcars)),
    cars = tibble::as_tibble(datasets::cars),
    cbpp = { d <- lme4::cbpp; tibble::tibble(rebanho = d$herd, periodo = d$period, casos = d$incidence,
                                             sadios = d$size - d$incidence, tamanho = d$size) },
    grouseticks = tibble::as_tibble(lme4::grouseticks))
}
