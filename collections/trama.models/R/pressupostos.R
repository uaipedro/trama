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

#' Delineamentos com bloco, onde Levene e Bartlett nos resíduos precisam de
#' correção.
#' @noRd
.TR_MODELS_COM_BLOCO <- c("DBC", "fatorial_dbc", "DQL")

#' Os fatores de controle (bloco; linha e coluna) de um delineamento com bloco.
#' @noRd
.tr_models_controles <- function(fit) {
  if (fit$delineamento == "DQL") {
    setdiff(all.vars(stats::as.formula(fit$formula)[[3]]), fit$tratamentos)
  } else fit$bloco
}

#' Levene de O'Neill & Mathews (2002) para delineamento com bloco.
#'
#' Os resíduos de mínimos quadrados de um delineamento com bloco são
#' correlacionados, e a ANOVA dos |resíduos| em tratamento + controles dá um F
#' liberal. O'Neill & Mathews mostram que, no delineamento equilibrado, o teste
#' de mínimos quadrados ponderados é o F comum vezes um multiplicador que só
#' depende do desenho: a razão entre os quadrados médios esperados do resíduo e
#' do tratamento na ANOVA de |e| sob H0. Com e ~ N(0, σ²R), R = I − H,
#' Cov(|e_i|, |e_j|) = σ_i σ_j (2/π)(√(1 − ρ²) + ρ·asen ρ − 1), ρ = correlação
#' dos resíduos; os quadrados médios esperados saem de tr(A Σ). No DBC isso
#' reproduz a forma fechada do artigo (e o `oneilldbc` do ExpDes.pt); no DQL é
#' a mesma conta, conferida por simulação nos testes. O multiplicador acerta a
#' média do F, não a cauda: em desenho pequeno o teste fica conservador
#' (tamanho a 5%, 20000 réplicas: DBC 5 x 6 4,6%, 4 x 3 2,9%; DQL 8 x 8 4,7%,
#' 5 x 5 2,9%, 4 x 4 2,3%).
#' @noRd
.tr_models_levene_om <- function(modelo, g, no) {
  d <- modelo$dados
  ctrl <- .tr_models_controles(modelo)
  dd <- d[, ctrl, drop = FALSE]
  dd$.g <- g
  Xc <- stats::model.matrix(stats::reformulate(.tr_models_bt(ctrl)), dd)
  Xf <- stats::model.matrix(stats::reformulate(c(.tr_models_bt(ctrl), ".g")), dd)
  proj <- function(X) { q <- qr(X); Q <- qr.Q(q)[, seq_len(q$rank), drop = FALSE]; list(H = tcrossprod(Q), k = q$rank) }
  pc <- proj(Xc); pf <- proj(Xf)
  n <- nrow(dd)
  R <- diag(n) - pf$H
  At <- pf$H - pc$H
  glt <- pf$k - pc$k; glr <- n - pf$k
  h <- diag(R)
  if (glt < 1L || glr < 1L) {
    .tr_models_abort("tr_models_error_no_residual_df",
                     "'%s': o delineamento não deixa grau de liberdade para o teste nos |resíduos|.", no)
  }
  # Equilíbrio: cada tratamento o mesmo número de vezes em cada nível de cada
  # controle (bloco; linha e coluna). Alavancas iguais sozinhas não bastam.
  celas_iguais <- all(vapply(ctrl, function(v) length(unique(as.vector(table(g, dd[[v]])))) == 1L, TRUE))
  if (!celas_iguais || diff(range(h)) > 1e-8 * max(h)) {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'%s': o delineamento está desbalanceado (falta ou sobra parcela), e o Levene ",
                            "de O'Neill & Mathews (2002) supõe o delineamento equilibrado. Confira os dados ",
                            "ou use o 'models/plot_diagnostics' (painel escala-locação)."), no)
  }
  e <- as.vector(R %*% d[[modelo$resposta]])
  z <- abs(e)
  rho <- pmin(pmax(R / sqrt(tcrossprod(h)), -1), 1)
  S <- (2 / pi) * (sqrt(1 - rho^2) + rho * asin(rho) - 1) * sqrt(tcrossprod(h))
  f_mq <- (sum(z * (At %*% z)) / glt) / (sum(z * (R %*% z)) / glr)
  m <- (sum(R * S) / glr) / (sum(At * S) / glt)
  f <- m * f_mq
  list(f = f, glt = glt, glr = glr, m = m, p = stats::pf(f, glt, glr, lower.tail = FALSE), ctrl = ctrl)
}

#' Levene e Bartlett recusam a parcela subdividida.
#'
#' O'Neill & Mathews (2002) corrigem um estrato de erro só. Nos resíduos do
#' erro (b) o fator da parcela está confundido com a própria parcela (o "bloco"
#' desse estrato), e a correção deixaria de testar a variância entre os níveis
#' da parcela; o teste comum nesses resíduos correlacionados sai liberal ou
#' conservador conforme o centro (10,5% e 2,5% de rejeição a 5% sob H0 no
#' desenho da aveia, em simulação). Sem correção validada, recusa.
#' @noRd
.tr_models_recusar_split <- function(modelo, no) {
  if (identical(modelo$classe, "split")) {
    .tr_models_abort("tr_models_error_block_design",
                     paste0("'%s': na parcela subdividida os resíduos vêm de dois estratos de erro, e o teste ",
                            "não tem correção publicada para eles (a de O'Neill & Mathews supõe um estrato só). ",
                            "Leia o painel escala-locação do 'models/plot_diagnostics' e, se as variâncias ",
                            "diferirem, ajuste o misto no 'models/lmer'."), no)
  }
  invisible(NULL)
}

#' Levene: as variâncias dos grupos são iguais?
#'
#' No DIC e nos modelos de fórmula, ANOVA dos desvios absolutos dos resíduos em
#' relação ao centro de cada grupo (`car::leveneTest`). Nos delineamentos com
#' bloco (DBC, fatorial em DBC, DQL), o teste de O'Neill & Mathews (2002): ANOVA
#' dos |resíduos| de mínimos quadrados em tratamento + bloco (+ linha e coluna)
#' com o F corrigido; ali o centro é sempre o ajuste do modelo (média), e
#' `centro` não muda o resultado.
#' @param modelo objeto `tr_models_fit`.
#' @param centro `"mediana"` (Brown-Forsythe, robusto; padrão) ou `"média"` (o
#'   Levene original). Ignorado nos delineamentos com bloco.
#' @return objeto `tr_models_test`.
#' @export
tr_models_levene <- function(modelo, centro = "mediana") {
  .tr_models_fit_conferir(modelo)
  no <- "models/levene"
  centro <- .tr_models_enum(centro, c("mediana", "média"), "centro")
  .tr_models_recusar_split(modelo, no)
  r <- .tr_models_residuos(modelo, no, permitir_misto = FALSE)
  g <- .tr_models_grupos(modelo, no)
  h0 <- sprintf("as variâncias são iguais entre os níveis de %s", paste(g$fatores, collapse = " × "))
  if (!is.null(modelo$delineamento) && modelo$delineamento %in% .TR_MODELS_COM_BLOCO) {
    om <- .tr_models_ajustar(.tr_models_levene_om(modelo, g$g, no), no)
    return(.tr_models_teste(
      "Levene (O'Neill-Mathews)", h0, om$f, "F", om$p, gl = sprintf("%d; %d", om$glt, om$glr),
      conclusao_sim = "variâncias diferentes entre os grupos",
      conclusao_nao = "não há evidência de variâncias diferentes",
      nota = sprintf(paste0("|resíduos| de mínimos quadrados em tratamento + %s; F corrigido pelo ",
                            "fator %.4f do delineamento (o centro é o ajuste do modelo)"),
                     paste(om$ctrl, collapse = " + "), om$m),
      fonte = "O'Neill & Mathews (2002)"))
  }
  t <- .tr_models_ajustar(car::leveneTest(r$residuo, g$g, center = if (centro == "mediana") stats::median else mean), no)
  .tr_models_teste(
    "Levene", h0,
    t$`F value`[[1]], "F", t$`Pr(>F)`[[1]], gl = sprintf("%d; %d", t$Df[[1]], t$Df[[2]]),
    conclusao_sim = "variâncias diferentes entre os grupos",
    conclusao_nao = "não há evidência de variâncias diferentes",
    nota = sprintf("nos resíduos; centro na %s", centro),
    fonte = if (centro == "mediana") "Brown & Forsythe (1974)" else "Levene (1960)")
}

#' Bartlett: as variâncias dos grupos são iguais?
#'
#' Só sem bloco: nos resíduos de um DBC/DQL o teste não tem correção publicada
#' para a correlação dos resíduos, e o bloco recusa apontando o Levene de
#' O'Neill & Mathews.
#' @param modelo objeto `tr_models_fit`.
#' @return objeto `tr_models_test`.
#' @export
tr_models_bartlett <- function(modelo) {
  .tr_models_fit_conferir(modelo)
  no <- "models/bartlett"
  .tr_models_recusar_split(modelo, no)
  if (!is.null(modelo$delineamento) && modelo$delineamento %in% .TR_MODELS_COM_BLOCO) {
    .tr_models_abort("tr_models_error_block_design",
                     paste0("'%s': %s tem bloco, e nos resíduos de um delineamento com bloco o Bartlett ",
                            "não tem correção publicada para a correlação dos resíduos (sairia liberal). ",
                            "Use o 'models/levene', que ali aplica a correção de O'Neill & Mathews (2002)."),
                     no, modelo$rotulo)
  }
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
  if (is.null(r$ajuste)) {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'%s' regride os resíduos nos preditores de um modelo linear, e %s não tem matriz ",
                            "de preditores. Olhe os resíduos × ajustados em 'models/plot_diagnostics'."), no, modelo$rotulo)
  }
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
