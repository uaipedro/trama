# Seleção entre vários modelos por critério de informação (AIC, AICc, BIC),
# com a diferença para o melhor, os pesos de Akaike e a razão de evidência.
# É a leitura "multimodelo" no sentido de Burnham & Anderson (2002): os modelos
# NÃO precisam ser aninhados (para o teste de dois aninhados há o
# `models/compare`), mas precisam explicar a MESMA resposta nas MESMAS linhas.

.TR_MODELS_CRITERIOS <- c("AICc", "AIC", "BIC")
.TR_MODELS_SELECT_CLASSES <- c("lm", "glm", "lmer", "glmer", "gls", "nls")

#' O log-verossimilhança de um modelo na base comparável: máxima verossimilhança.
#'
#' REML só compara estruturas aleatórias com os MESMOS efeitos fixos; como o
#' bloco compara modelos quaisquer, o misto e o GLS são reajustados por ML
#' (o lme4 e o nlme fazem isso no `anova()`, pelo mesmo motivo).
#' @return lista com `ll`, `k` (parâmetros, variância inclusa), `n`.
#' @noRd
.tr_models_info_ic <- function(fit) {
  aj <- fit$ajuste
  if (fit$classe == "lmer") aj <- lme4::refitML(aj)
  else if (fit$classe == "gls" && !identical(aj$method, "ML")) aj <- stats::update(aj, method = "ML")
  ll <- stats::logLik(aj)
  list(ll = as.numeric(ll), k = as.numeric(attr(ll, "df")), n = as.numeric(stats::nobs(aj)))
}

#' A natureza da distribuição da resposta: `"discreta"` (pmf) ou `"continua"`
#' (densidade). Log-verossimilhanças de uma pmf e de uma densidade não se
#' comparam (a de uma densidade muda com a unidade da resposta), então o bloco
#' recusa a mistura. lm, lmer, gls e nls são gaussianos.
#' @noRd
.tr_models_natureza <- function(fit) {
  fam <- if (fit$classe %in% c("glm", "glmer")) stats::family(fit$ajuste)$family else "gaussian"
  if (grepl("^quasi", fam)) return("quasi")
  if (fam %in% c("binomial", "poisson") || grepl("Negative Binomial", fam, fixed = TRUE)) "discreta" else "continua"
}

#' Seleção de modelos por critério de informação.
#'
#' `AICc = AIC + 2k(k + 1) / (n - k - 1)` (Hurvich e Tsai, 1989); os pesos de
#' Akaike são `exp(-delta / 2)` normalizados a soma 1 (Burnham e Anderson, 2002,
#' cap. 2). Com `n - k - 1 <= 0` o AICc não existe e sai `NA` (o modelo fica
#' fora do ranking, no fim).
#'
#' @param modelos lista de objetos `tr_models_fit` (a porta variádica entrega
#'   uma lista), com a mesma resposta e o mesmo número de linhas.
#' @param criterio `"AICc"`, `"AIC"` ou `"BIC"`.
#' @return tibble ordenado do melhor ao pior: `entrada` (posição na porta),
#'   `modelo`, `formula`, `n`, `k`, `log_verossimilhanca`, o critério, `delta`,
#'   `peso`, `peso_acumulado` e `razao_evidencia` (peso do melhor / peso do
#'   modelo).
#' @export
tr_models_select <- function(modelos, criterio = "AICc") {
  no <- "models/select"
  criterio <- .tr_models_enum(criterio, .TR_MODELS_CRITERIOS, "criterio")
  if (inherits(modelos, "tr_models_fit")) modelos <- list(modelos)
  if (length(modelos) < 2L) {
    .tr_models_abort("tr_models_error_not_nested",
                     "'%s' compara dois ou mais modelos; ligue mais um na entrada.", no)
  }
  for (m in modelos) .tr_models_modelo_conferir(m)
  ruim <- Filter(function(m) !m$classe %in% .TR_MODELS_SELECT_CLASSES, modelos)
  if (length(ruim)) {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'%s' não lê %s: precisa de log-verossimilhança (lm, glm, lmer, glmer, gls ",
                            "ou nls). Modelos de outras coleções ficam no 'models/evaluate'."),
                     no, ruim[[1]]$rotulo)
  }
  respostas <- vapply(modelos, function(m) deparse(stats::as.formula(m$formula)[[2]]), character(1))
  if (length(unique(respostas)) > 1L) {
    .tr_models_abort("tr_models_error_not_nested",
                     paste0("'%s': os modelos têm respostas diferentes (%s). O critério só compara modelos ",
                            "da mesma resposta, na mesma escala."), no, paste(unique(respostas), collapse = "; "))
  }
  nat <- vapply(modelos, .tr_models_natureza, character(1))
  if (any(nat == "quasi")) {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'%s' n\u00E3o l\u00EA %s: fam\u00EDlia quasi n\u00E3o tem verossimilhan\u00E7a, ent\u00E3o n\u00E3o h\u00E1 AIC ",
                            "(o QAIC, de Burnham e Anderson, cap. 6, n\u00E3o \u00E9 calculado aqui)."),
                     no, modelos[[which(nat == "quasi")[[1]]]]$rotulo)
  }
  if (length(unique(nat)) > 1L) {
    .tr_models_abort("tr_models_error_not_nested",
                     paste0("'%s': mistura resposta discreta (binomial, Poisson) com cont\u00EDnua (gaussiana, gama). ",
                            "Log-verossimilhan\u00E7as de uma pmf e de uma densidade n\u00E3o se comparam; ",
                            "use fam\u00EDlias da mesma natureza."), no)
  }
  ic <- lapply(modelos, .tr_models_info_ic)
  ns <- vapply(ic, function(i) i$n, numeric(1))
  if (length(unique(ns)) > 1L) {
    .tr_models_abort("tr_models_error_not_nested",
                     paste0("'%s': os modelos usaram %s linhas. Com linhas diferentes o critério mede os dados, ",
                            "não o modelo — tire os faltantes antes, num 'data/drop_na', e ajuste todos na mesma tabela."),
                     no, paste(ns, collapse = ", "))
  }
  ll <- vapply(ic, function(i) i$ll, numeric(1)); k <- vapply(ic, function(i) i$k, numeric(1)); n <- ns[[1]]
  aic <- -2 * ll + 2 * k
  valor <- switch(criterio,
                  AIC = aic,
                  BIC = -2 * ll + log(n) * k,
                  AICc = ifelse(n - k - 1 > 0, aic + 2 * k * (k + 1) / (n - k - 1), NA_real_))
  if (all(is.na(valor))) {
    .tr_models_abort("tr_models_error_not_applicable",
                     "'%s': com n = %d nenhum modelo tem %s (n \u2212 k \u2212 1 \u2264 0). Use AIC ou BIC, ou mais linhas.",
                     no, as.integer(n), criterio)
  }
  delta <- valor - min(valor, na.rm = TRUE)
  w <- exp(-delta / 2); w <- w / sum(w, na.rm = TRUE)
  ord <- order(valor, na.last = TRUE)
  out <- tibble::tibble(
    entrada = seq_along(modelos),
    modelo = vapply(modelos, function(m) m$rotulo, character(1)),
    formula = vapply(modelos, function(m) as.character(m$formula), character(1)),
    n = n, k = k, log_verossimilhanca = ll)
  out[[criterio]] <- valor
  out$delta <- delta; out$peso <- w
  out <- out[ord, , drop = FALSE]
  out$peso_acumulado <- cumsum(ifelse(is.na(out$peso), 0, out$peso))
  # exp(delta / 2), e não peso_melhor / peso: o peso dá underflow com delta > ~1490.
  out$razao_evidencia <- exp(out$delta / 2)
  tibble::as_tibble(out)
}
