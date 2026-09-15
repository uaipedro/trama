# Pressupostos: modelo entra, UM TESTE sai.
#
# Os testes olham os RESÍDUOS do modelo, e não a resposta crua: o que a ANOVA
# pressupõe normal é o erro, e a resposta de um DBC com blocos muito diferentes
# é bimodal sem nada de errado com o experimento. Testar a resposta reprovaria
# o experimento bom, que é o engano que estes blocos existem para evitar.

#' Os grupos de tratamento do modelo, para Levene e Bartlett.
#'
#' Nos delineamentos, as combinações dos tratamentos (sem o bloco: a variância
#' que importa é a de cada tratamento). Nos modelos de fórmula, as colunas-fator
#' dos efeitos fixos. Sem fator nenhum não há grupo, e o card aponta o
#' Breusch-Pagan, que é o teste de variância para preditor contínuo.
#' @noRd
.tr_models_grupos <- function(fit, no) {
  d <- fit$dados
  fatores <- if (length(fit$tratamentos)) fit$tratamentos else {
    f <- reformulas::nobars(stats::as.formula(fit$formula))
    vars <- all.vars(f[[3]])
    grupos_aleat <- unlist(lapply(reformulas::findbars(stats::as.formula(fit$formula)), function(b) all.vars(b[[3]])))
    setdiff(vars[vapply(vars, function(v) is.factor(d[[v]]), TRUE)], grupos_aleat)
  }
  if (!length(fatores)) {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'%s' compara a variância entre grupos, e %s não tem fator de efeito fixo. ",
                            "Para preditor contínuo, use 'models/breusch_pagan'."), no, fit$rotulo)
  }
  g <- interaction(d[, fatores, drop = FALSE], drop = TRUE, sep = ":")
  if (any(table(g) < 2L)) {
    .tr_models_abort("tr_models_error_too_few_rows",
                     "'%s': algum grupo (%s) tem menos de duas observações, e sem repetição não há variância.",
                     no, paste(names(which(table(g) < 2L)), collapse = ", "))
  }
  list(g = g, fatores = fatores)
}

#' Shapiro-Wilk nos resíduos do modelo.
#' @param modelo objeto `tr_models_fit`.
#' @return objeto `tr_models_test`.
#' @export
tr_models_shapiro_residuals <- function(modelo) {
  .tr_models_fit_conferir(modelo)
  no <- "models/shapiro_residuals"
  r <- .tr_models_residuos(modelo, no)
  n <- length(r$residuo)
  if (n < 3L || n > 5000L) {
    .tr_models_abort("tr_models_error_too_few_rows",
                     "'%s': o Shapiro-Wilk aceita de 3 a 5000 resíduos, e há %d.", no, n)
  }
  t <- .tr_models_ajustar(stats::shapiro.test(r$residuo), no)
  .tr_models_teste(
    "Shapiro-Wilk (resíduos)", "os resíduos têm distribuição normal", t$statistic, "W", t$p.value,
    conclusao_sim = "resíduos não normais",
    conclusao_nao = "não há evidência contra a normalidade dos resíduos",
    nota = .tr_models_nota(sprintf("%d resíduos de %s", n, modelo$rotulo),
                           if (modelo$classe == "split") "resíduos do erro (b)" else "",
                           if (modelo$classe == "lmer") "resíduos condicionais" else ""),
    fonte = "Shapiro & Wilk (1965)")
}

#' Levene: as variâncias dos grupos são iguais?
#' @param modelo objeto `tr_models_fit`.
#' @param centro `"mediana"` (Brown-Forsythe, robusto; padrão) ou `"média"` (o
#'   Levene original).
#' @return objeto `tr_models_test`.
#' @export
tr_models_levene <- function(modelo, centro = "mediana") {
  .tr_models_fit_conferir(modelo)
  no <- "models/levene"
  centro <- .tr_models_enum(centro, c("mediana", "média"), "centro")
  r <- .tr_models_residuos(modelo, no, permitir_misto = FALSE)
  g <- .tr_models_grupos(modelo, no)
  t <- .tr_models_ajustar(car::leveneTest(r$residuo, g$g, center = if (centro == "mediana") stats::median else mean), no)
  .tr_models_teste(
    "Levene", sprintf("as variâncias são iguais entre os níveis de %s", paste(g$fatores, collapse = " × ")),
    t$`F value`[[1]], "F", t$`Pr(>F)`[[1]], gl = sprintf("%d; %d", t$Df[[1]], t$Df[[2]]),
    conclusao_sim = "variâncias diferentes entre os grupos",
    conclusao_nao = "não há evidência de variâncias diferentes",
    nota = sprintf("nos resíduos; centro na %s", centro),
    fonte = if (centro == "mediana") "Brown & Forsythe (1974)" else "Levene (1960)")
}

#' Bartlett: as variâncias dos grupos são iguais?
#' @param modelo objeto `tr_models_fit`.
#' @return objeto `tr_models_test`.
#' @export
tr_models_bartlett <- function(modelo) {
  .tr_models_fit_conferir(modelo)
  no <- "models/bartlett"
  r <- .tr_models_residuos(modelo, no, permitir_misto = FALSE)
  g <- .tr_models_grupos(modelo, no)
  t <- .tr_models_ajustar(stats::bartlett.test(r$residuo, g$g), no)
  .tr_models_teste(
    "Bartlett", sprintf("as variâncias são iguais entre os níveis de %s", paste(g$fatores, collapse = " × ")),
    t$statistic, "K²", t$p.value, gl = as.character(t$parameter),
    conclusao_sim = "variâncias diferentes entre os grupos",
    conclusao_nao = "não há evidência de variâncias diferentes",
    nota = "nos resíduos; sensível à falta de normalidade — confira o Shapiro antes",
    fonte = "Bartlett (1937)")
}

#' Breusch-Pagan (versão studentizada de Koenker).
#'
#' Regride o quadrado dos resíduos nos preditores do modelo: se eles explicam o
#' tamanho do resíduo, a variância não é constante. A versão de Koenker (n·R²)
#' não pressupõe normalidade, e é a padrão do `lmtest::bptest`.
#' @param modelo objeto `tr_models_fit`.
#' @return objeto `tr_models_test`.
#' @export
tr_models_breusch_pagan <- function(modelo) {
  .tr_models_fit_conferir(modelo)
  no <- "models/breusch_pagan"
  r <- .tr_models_residuos(modelo, no, permitir_misto = FALSE)
  x <- stats::model.matrix(r$ajuste)
  e2 <- r$residuo^2
  aux <- stats::lm.fit(x, e2)
  r2 <- 1 - sum(aux$residuals^2) / sum((e2 - mean(e2))^2)
  gl <- aux$rank - 1L
  if (gl < 1L) {
    .tr_models_abort("tr_models_error_not_applicable",
                     "'%s': o modelo não tem preditor (só o intercepto), e não há de que a variância dependa.", no)
  }
  est <- length(e2) * r2
  .tr_models_teste(
    "Breusch-Pagan", "a variância dos resíduos é constante", est, "BP",
    stats::pchisq(est, gl, lower.tail = FALSE), gl = as.character(gl),
    conclusao_sim = "variância não constante (heterocedasticidade)",
    conclusao_nao = "não há evidência de heterocedasticidade",
    nota = "studentizado (Koenker), nos preditores do modelo",
    fonte = "Breusch & Pagan (1979); Koenker (1981)")
}

#' Não aditividade de Tukey: bloco e tratamento interagem?
#'
#' O DBC pressupõe que o efeito do tratamento é o mesmo em todo bloco. O teste
#' de um grau de liberdade acrescenta o quadrado do ajustado ao modelo aditivo:
#' se ele é significativo, os efeitos se multiplicam em vez de somar, e uma
#' transformação (log, quase sempre) costuma resolver.
#' @param modelo objeto `tr_models_fit` de um delineamento com bloco (DBC,
#'   fatorial em DBC ou DQL).
#' @return objeto `tr_models_test`.
#' @export
tr_models_tukey_additivity <- function(modelo) {
  .tr_models_fit_conferir(modelo)
  no <- "models/tukey_additivity"
  if (is.null(modelo$delineamento) || !modelo$delineamento %in% c("DBC", "fatorial_dbc", "DQL")) {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'%s' testa a aditividade de bloco e tratamento, e pede um delineamento com ",
                            "bloco ('models/anova_dbc', 'models/anova_factorial' com bloco ou ",
                            "'models/anova_dql'). Chegou %s."), no, modelo$rotulo)
  }
  d <- modelo$dados
  controles <- if (modelo$delineamento == "DQL") {
    setdiff(all.vars(stats::as.formula(modelo$formula)[[3]]), modelo$tratamentos)
  } else modelo$bloco
  d$.trat <- interaction(d[, modelo$tratamentos, drop = FALSE], drop = TRUE)
  f0 <- stats::as.formula(paste(.tr_models_bt(modelo$resposta), "~",
                                paste(c(.tr_models_bt(controles), ".trat"), collapse = " + ")))
  m0 <- stats::lm(f0, data = d)
  d$.q <- stats::fitted(m0)^2
  m1 <- stats::update(m0, . ~ . + .q, data = d)
  a <- .tr_models_ajustar(as.data.frame(stats::anova(m0, m1)), no)
  if (is.na(a$F[[2]])) {
    .tr_models_abort("tr_models_error_no_residual_df",
                     "'%s': não sobra grau de liberdade para o termo de não aditividade.", no)
  }
  .tr_models_teste(
    "Não aditividade de Tukey", "os efeitos de bloco e tratamento são aditivos", a$F[[2]], "F",
    a$`Pr(>F)`[[2]], gl = sprintf("1; %d", as.integer(a$Res.Df[[2]])),
    conclusao_sim = "bloco e tratamento não são aditivos (tente transformar a resposta)",
    conclusao_nao = "não há evidência contra a aditividade",
    fonte = "Tukey (1949)")
}
