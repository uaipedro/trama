# Box-Cox: qual potência da resposta deixa o modelo com erro normal e
# variância constante?
#
# Lê o `models/fit` da ANOVA (ou de um lm) e varre λ mantendo o MESMO
# delineamento: a pergunta é sobre a resposta, não sobre o modelo. A conta é a
# do `MASS::boxcox` — log-verossimilhança perfilada com a resposta dividida pela
# média geométrica elevada a λ − 1, o que torna os SQ de λ diferentes
# comparáveis —, reescrita aqui só para devolver a tabela, o IC e o λ
# sugerido em vez de desenhar no dispositivo gráfico.
#
# O λ "arredondado" existe porque ninguém publica y^−0,73: escolhe-se, dentro
# do IC, a potência interpretável mais próxima do ótimo (1/y, log, √y...). Se
# nenhuma cair no IC, o bloco não inventa — diz que nenhuma serve.

.TR_EXP_AN_LAMBDAS <- c(-2, -1, -0.5, 0, 0.5, 1, 2)
.TR_EXP_AN_TRANSF <- c("1/y²", "1/y (recíproca)", "1/√y", "log(y)", "√y", "nenhuma (y)", "y²")

#' A log-verossimilhança perfilada em cada λ, na forma do `MASS::boxcox`.
#'
#' Perto de zero usa o limite logarítmico, com o mesmo corte |λ| ≤ 1/50 do MASS,
#' para que a tabela bata ponto a ponto com a dele.
#' @noRd
.tr_exp_an_boxcox_ll <- function(y, X) {
  n <- length(y)
  xqr <- qr(X)
  # A resposta dividida pela média geométrica: o jacobiano da transformação
  # entra aí, e o SQ residual de cada λ passa a ser comparável.
  logy <- log(y / exp(mean(log(y))))
  function(lambda) vapply(lambda, function(la) {
    yt <- if (abs(la) > 1 / 50) (exp(la * logy) - 1) / la
          else logy * (1 + (la * logy) / 2 * (1 + (la * logy) / 3 * (1 + (la * logy) / 4)))
    -n / 2 * log(sum(qr.resid(xqr, yt)^2))
  }, 1)
}

#' O ajuste linear de efeitos fixos por trás do `models/fit`.
#' @noRd
.tr_exp_an_ajuste_lm <- function(modelo, no) {
  switch(modelo$classe, lm = modelo$ajuste, split = modelo$aux_lm,
    .tr_experiments_abort("tr_experiments_error_not_applicable",
                     paste0("'%s': o Box-Cox perfila a verossimilhança de um modelo linear de efeitos fixos ",
                            "(ANOVA ou lm); o modelo é '%s'."), no, modelo$classe))
}

#' Perfil de Box-Cox de um `models/fit`.
#'
#' @param modelo objeto `tr_models_fit` de ANOVA ou lm (a resposta tem de ser
#'   positiva).
#' @param lambda_min,lambda_max,passo a grade de λ (padrão a do `MASS::boxcox`).
#' @param confianca nível do intervalo para λ (razão de verossimilhança).
#' @inheritParams trama.view::tr_view_finish
#' @return lista com `out` (gráfico do perfil, `view/plot`), `resumo` (tibble de
#'   uma linha) e `perfil` (tibble com λ e log-verossimilhança).
#' @export
tr_experiments_boxcox <- function(modelo, lambda_min = -2, lambda_max = 2, passo = 0.1, confianca = 0.95,
                                  aspecto = "4:3", tema = "padrão", titulo = "", rotulo_x = "",
                                  rotulo_y = "", legenda = "direita") {
  no <- "experiments/boxcox"
  if (!inherits(modelo, "tr_models_fit")) {
    .tr_experiments_abort("tr_experiments_error_bad_option", "'%s' lê um modelo ajustado (models/fit).", no)
  }
  confianca <- as.numeric(confianca)
  if (!is.finite(confianca) || confianca < 0.5 || confianca > 0.999) {
    .tr_experiments_abort("tr_experiments_error_bad_option", "'%s': 'confianca' vai de 0,5 a 0,999.", no)
  }
  lambda_min <- as.numeric(lambda_min); lambda_max <- as.numeric(lambda_max); passo <- as.numeric(passo)
  if (!(lambda_min < lambda_max) || !(passo > 0) || (lambda_max - lambda_min) / passo > 2000) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                     "'%s': a grade de λ precisa de mínimo < máximo e de um passo positivo (até 2000 pontos).", no)
  }
  aj <- .tr_exp_an_ajuste_lm(modelo, no)
  y <- stats::model.response(stats::model.frame(aj))
  if (any(y <= 0)) {
    .tr_experiments_abort("tr_experiments_error_not_applicable",
                     paste0("'%s': a resposta tem valores ≤ 0 (mínimo %s), e y^λ só é definido para y > 0. ",
                            "Some uma constante antes (data/mutate) e diga isso no relatório."), no, format(min(y)))
  }
  ll <- .tr_exp_an_boxcox_ll(y, stats::model.matrix(aj))
  grade <- seq(lambda_min, lambda_max, by = passo)
  perfil <- tibble::tibble(lambda = grade, log_verossimilhanca = ll(grade))
  # O ótimo fino, e não o da grade: com passo 0,1 a grade só acerta a primeira casa.
  o <- stats::optimize(ll, c(lambda_min, lambda_max), maximum = TRUE, tol = 1e-8)
  lhat <- o$maximum; lmax <- o$objective
  # O `optimize` devolve um ponto da ponta quando o perfil só cresce até ela: aí
  # λ̂ não é o ótimo, é a borda da grade, e o máximo está além dela.
  borda <- c(if (lhat - lambda_min < 1e-4 * (lambda_max - lambda_min) && ll(lambda_min) >= ll(lambda_min + passo / 10)) "inferior",
             if (lambda_max - lhat < 1e-4 * (lambda_max - lambda_min) && ll(lambda_max) >= ll(lambda_max - passo / 10)) "superior")
  if (length(borda)) lhat <- if (borda[[1]] == "inferior") lambda_min else lambda_max
  corte <- lmax - stats::qchisq(confianca, 1) / 2
  raiz <- function(a, b) {
    if ((ll(a) - corte) * (ll(b) - corte) > 0) return(NA_real_)
    stats::uniroot(function(l) ll(l) - corte, c(a, b), tol = 1e-10)$root
  }
  li <- raiz(lambda_min, lhat); ls <- raiz(lhat, lambda_max)
  aberto <- c(if (is.na(li)) "inferior", if (is.na(ls)) "superior")
  li_ <- if (is.na(li)) lambda_min else li; ls_ <- if (is.na(ls)) lambda_max else ls
  dentro <- .TR_EXP_AN_LAMBDAS >= li_ & .TR_EXP_AN_LAMBDAS <= ls_
  sug <- if (length(borda)) NA_real_ else if (any(dentro)) {
    cand <- .TR_EXP_AN_LAMBDAS[dentro]
    cand[which.min(abs(cand - lhat))]
  } else NA_real_
  resumo <- tibble::tibble(
    lambda_otimo = lhat, li = li, ls = ls, confianca = confianca,
    lambda_sugerido = sug,
    transformacao = if (length(borda)) "sem sugestão: λ̂ na borda da grade" else if (is.na(sug)) "nenhuma potência simples cai no intervalo" else .TR_EXP_AN_TRANSF[match(sug, .TR_EXP_AN_LAMBDAS)],
    um_no_intervalo = 1 >= li_ && 1 <= ls_,
    na_borda = length(borda) > 0L,
    nota = paste(c(
      if (length(borda)) sprintf("λ̂ está na borda %s da grade (%g): não é o ótimo, o máximo do perfil fica além; amplie a grade",
                                 borda[[1]], lhat),
      if (length(aberto)) sprintf("intervalo aberto no limite %s da grade: amplie a grade", paste(aberto, collapse = " e "))),
      collapse = "; "))

  sinal <- if (!length(borda)) "=" else if (borda[[1]] == "inferior") "≤" else "≥"
  lab <- sprintf("λ̂ %s %.3f%s · IC %g%%: [%s; %s]%s", sinal, lhat, if (length(borda)) " (borda da grade)" else "",
                 100 * confianca,
                 if (is.na(li)) "<" else sprintf("%.3f", li), if (is.na(ls)) ">" else sprintf("%.3f", ls),
                 if (is.na(sug)) "" else sprintf(" · sugerido: %s", resumo$transformacao))
  p <- ggplot2::ggplot(perfil, ggplot2::aes(x = .data[["lambda"]], y = .data[["log_verossimilhanca"]])) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = corte, linetype = "dashed", colour = "grey45") +
    ggplot2::geom_vline(xintercept = c(li, ls)[!is.na(c(li, ls))], linetype = "dotted", colour = "grey45") +
    ggplot2::geom_vline(xintercept = lhat, colour = "#d97706") +
    ggplot2::labs(x = "λ", y = "log-verossimilhança perfilada", subtitle = lab)
  if (!is.na(sug)) p <- p + ggplot2::annotate("point", x = sug, y = ll(sug), size = 3, colour = "#2563eb")
  grafico <- trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
  list(out = grafico, resumo = resumo, perfil = perfil)
}
