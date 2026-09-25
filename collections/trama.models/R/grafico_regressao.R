# O gráfico de regressão das teses: pontos, curva, equação e R², para as duas
# curvas da coleção que têm UMA preditora (dose-resposta e não linear).

#' Número da equação: 4 algarismos significativos, vírgula decimal, SEM os
#' zeros à direita que o `.tr_models_fmt()` guarda nas tabelas ("2,78", não
#' "2,780"). É a mesma regra do `.tr_view_fmt()` da `view/fit_line`, e há
#' teste lá comparando as duas: o mesmo ajuste escreve a mesma equação.
#' @noRd
.tr_models_fmt_eq <- function(x) {
  v <- .tr_models_fmt(x, 4L)
  ifelse(grepl(",", v, fixed = TRUE), sub(",$", "", sub("0+$", "", v)), v)
}

#' Coeficiente com sinal para a equação: " + 0,45", " - 0,0021".
#'
#' Hífen, e não o sinal de menos U+2212: a fonte do tema o desenha como um
#' traço minúsculo no PNG (medido na `view/fit_line`).
#' @noRd
.tr_models_termo_eq <- function(b, sufixo) {
  sprintf(" %s %s%s", if (b < 0) "-" else "+", .tr_models_fmt_eq(abs(b)), sufixo)
}

#' A equação da curva, como as teses a escrevem.
#' @noRd
.tr_models_equacao <- function(m) {
  b <- stats::coef(m$ajuste)
  f <- .tr_models_fmt_eq
  if (m$classe == "dose") {
    pot <- c("x", "x²", "x³")
    return(paste0("ŷ = ", f(b[[1]]), paste(vapply(seq_len(length(b) - 1L), function(j) .tr_models_termo_eq(b[[j + 1L]], pot[[j]]), ""), collapse = "")))
  }
  switch(m$modelo,
    `logístico` = sprintf("ŷ = %s / (1 + exp((%s − x) / %s))", f(b[["Asym"]]), f(b[["xmid"]]), f(b[["scal"]])),
    `Michaelis-Menten` = sprintf("ŷ = %s·x / (%s + x)", f(b[["Vm"]]), f(b[["K"]])),
    `exponencial assintótico` = paste0("ŷ = ", f(b[["Asym"]]), .tr_models_termo_eq(b[["R0"]] - b[["Asym"]], ""),
                                       sprintf("·exp(−%s·x)", f(exp(b[["lrc"]])))),
    Gompertz = sprintf("ŷ = %s·exp(−%s·%s^x)", f(b[["Asym"]]), f(b[["b2"]]), f(b[["b3"]])),
    `linear-platô` = sprintf("ŷ = %s%s se x < %s; ŷ = %s depois", f(b[["a"]]), .tr_models_termo_eq(b[["b"]], "x"),
                             f(b[["x0"]]), f(b[["a"]] + b[["b"]] * b[["x0"]])))
}

#' Gráfico de regressão: pontos, curva, equação e R².
#'
#' A figura das teses: na dose-resposta os pontos são as MÉDIAS das doses (a
#' curva foi ajustada a elas) e, na parábola, a linha tracejada marca a dose de
#' máxima (ou mínima) eficiência técnica; no não linear os pontos são as
#' observações, e no linear-platô a linha marca o início do platô.
#' @param modelo objeto `tr_models_fit` de `models/dose_response` ou `models/nls`.
#' @param observacoes na dose-resposta, mostrar também as parcelas (em cinza)
#'   atrás das médias.
#' @param equacao escrever a equação e o R² no gráfico.
#' @inheritParams trama.view::tr_view_finish
#' @return ggplot.
#' @export
tr_models_plot_regression <- function(modelo, observacoes = FALSE, equacao = TRUE, aspecto = "4:3", tema = "padrão",
                                      titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  .tr_models_fit_conferir(modelo)
  if (!modelo$classe %in% c("dose", "nls")) {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'models/plot_regression' desenha a curva de 'models/dose_response' ou de ",
                            "'models/nls', e chegou %s."), modelo$rotulo)
  }
  dose <- modelo$classe == "dose"
  xn <- if (dose) modelo$tratamentos else modelo$preditor
  y <- modelo$resposta
  obs <- as.data.frame(modelo$dados)
  if (dose) obs[[xn]] <- .tr_models_dose_x(modelo, obs)[[1]]
  pts <- if (dose) as.data.frame(modelo$medias) else obs
  faixa <- range(obs[[xn]])
  grade <- stats::setNames(data.frame(seq(faixa[[1]], faixa[[2]], length.out = 200L)), xn)
  grade$.y <- tr_models_predict_raw(modelo, grade)$previsto
  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data[[xn]]))
  if (dose && isTRUE(observacoes)) {
    p <- p + ggplot2::geom_point(data = obs, ggplot2::aes(y = .data[[y]]), colour = .TR_MODELS_CINZA, alpha = .5, size = 1.4)
  }
  marca <- if (dose && !is.null(modelo$met) && modelo$met$dentro) {
    list(x = modelo$met$x, y = modelo$met$y, rotulo = sprintf("%s: x = %s", if (modelo$met$tipo == "máximo") "MET" else "mínimo",
                                                                .tr_models_fmt(modelo$met$x, 4L)))
  } else if (!dose && modelo$modelo == "linear-platô") {
    b <- stats::coef(modelo$ajuste)
    list(x = b[["x0"]], y = b[["a"]] + b[["b"]] * b[["x0"]], rotulo = sprintf("platô: x = %s", .tr_models_fmt(b[["x0"]], 4L)))
  }
  if (!is.null(marca)) {
    p <- p + ggplot2::geom_vline(xintercept = marca$x, colour = .TR_MODELS_COR_2, linetype = "dashed") +
      ggplot2::annotate("text", x = marca$x, y = marca$y, label = marca$rotulo, hjust = -.05, vjust = -.6, size = 3.2,
                        colour = .TR_MODELS_COR_2)
  }
  p <- p + ggplot2::geom_line(data = grade, ggplot2::aes(y = .data[[".y"]]), colour = .TR_MODELS_COR, linewidth = .9) +
    ggplot2::geom_point(data = pts, ggplot2::aes(y = .data[[y]]), colour = .TR_MODELS_COR, size = 2.6)
  if (isTRUE(equacao)) {
    r2 <- if (dose) modelo$r2 else tr_models_stats(modelo)$r2_pseudo
    texto <- sprintf("%s\n%s = %s", .tr_models_equacao(modelo), if (dose) "R²" else "R² (pseudo)", .tr_models_fmt(r2, 4L))
    p <- p + ggplot2::annotate("text", x = faixa[[1]], y = Inf, label = texto, hjust = 0, vjust = 1.3, size = 3.4)
  }
  p <- p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.05, .22))) +
    ggplot2::labs(x = xn, y = if (dose) sprintf("%s (média por dose)", y) else y)
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}
