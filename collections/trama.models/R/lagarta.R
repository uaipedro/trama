# O gráfico de lagarta: os efeitos aleatórios previstos de um misto, um por
# nível do grupo, com o intervalo de cada um.
#
# É o que o `models/random_effects` resume num número (a variância do grupo)
# aberto em pontos: cada sujeito, bloco ou parcela com o quanto ele se afasta da
# média do modelo. Os pontos são os BLUPs (os modos condicionais), e o intervalo
# vem da variância condicional do `lme4`.
#
# Dois controles fazem a figura conversar com o resultado:
#
# - **intervalo**: 1 ou 2 erros padrão, ou IC de 90, 95 ou 99%. Com poucos
#   níveis muito separados, 95% mostra quem se destaca; com muitos níveis
#   próximos, 1 EP mostra a ordem sem virar um borrão de barras sobrepostas.
# - **faixa do desvio**: a faixa sombreada de ±1 desvio padrão do componente de
#   variância — o MESMO número que o `models/random_effects` mostra. Um nível
#   cuja lagarta sai inteira da faixa é um nível atípico para aquele grupo.

.TR_MODELS_INTERVALOS <- c("IC 95%", "IC 90%", "IC 99%", "± 1 EP", "± 2 EP")

.tr_models_mult_intervalo <- function(intervalo) {
  switch(intervalo, `IC 95%` = stats::qnorm(0.975), `IC 90%` = stats::qnorm(0.95),
         `IC 99%` = stats::qnorm(0.995), `± 1 EP` = 1, `± 2 EP` = 2)
}

#' Gráfico de lagarta dos efeitos aleatórios.
#' @param modelo objeto `tr_models_fit` de um misto (ou parcela subdividida).
#' @param grupo fator de agrupamento; em branco, o primeiro do modelo.
#' @param intervalo largura das barras.
#' @param faixa_desvio sombrear ±1 desvio padrão do componente de variância.
#' @param ordenar ordenar os níveis pelo efeito.
#' @export
tr_models_plot_caterpillar <- function(modelo, grupo = "", intervalo = "IC 95%", faixa_desvio = TRUE,
                                       ordenar = TRUE, aspecto = "3:4", tema = "padrão", titulo = "",
                                       rotulo_x = "", rotulo_y = "", legenda = "abaixo") {
  .tr_models_fit_conferir(modelo)
  no <- "models/plot_caterpillar"
  intervalo <- .tr_models_enum(intervalo, .TR_MODELS_INTERVALOS, "intervalo")
  aj <- .tr_models_misto(modelo, no)
  r <- as.data.frame(lme4::ranef(aj, condVar = TRUE))
  grupos <- unique(as.character(r$grpvar))
  g <- if (.tr_models_preenchido(grupo)) trimws(grupo) else grupos[[1]]
  if (!g %in% grupos) {
    .tr_models_abort("tr_models_error_unknown_column",
                     "Param 'grupo': '%s' não é fator aleatório do modelo. Os do modelo: %s.",
                     g, paste(grupos, collapse = ", "))
  }
  d <- r[r$grpvar == g, , drop = FALSE]
  k <- .tr_models_mult_intervalo(intervalo)
  d$li <- d$condval - k * d$condsd
  d$ls <- d$condval + k * d$condsd
  d$termo <- factor(d$term, levels = unique(d$term))
  # A ordem é a do PRIMEIRO termo (o intercepto, quase sempre) e vale para todos
  # os painéis: com uma ordem por painel, o mesmo sujeito mudaria de linha entre
  # o intercepto e a inclinação, e a leitura cruzada — quem começa alto também
  # sobe mais rápido? — ficaria impossível.
  d$grp <- as.character(d$grp)
  ref <- d[d$termo == levels(d$termo)[[1]], ]
  niveis <- if (isTRUE(ordenar)) ref$grp[order(ref$condval)] else rev(unique(d$grp))
  d$grp <- factor(d$grp, levels = niveis)
  d$destaque <- ifelse(d$li > 0 | d$ls < 0, "intervalo não cruza zero", "cruza zero")
  vc <- as.data.frame(lme4::VarCorr(aj))
  vc <- vc[vc$grp == g & is.na(vc$var2), ]
  faixa <- data.frame(termo = factor(vc$var1, levels = levels(d$termo)), sd = vc$sdcor)
  faixa <- faixa[!is.na(faixa$termo), ]
  p <- ggplot2::ggplot(d, ggplot2::aes(y = .data[["grp"]]))
  if (isTRUE(faixa_desvio) && nrow(faixa)) {
    p <- p + ggplot2::geom_rect(data = faixa, inherit.aes = FALSE,
                                ggplot2::aes(xmin = -.data[["sd"]], xmax = .data[["sd"]], ymin = -Inf, ymax = Inf),
                                fill = .TR_MODELS_COR, alpha = .12)
  }
  p <- p +
    ggplot2::geom_vline(xintercept = 0, colour = .TR_MODELS_CINZA, linetype = "dashed") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data[["li"]], xmax = .data[["ls"]], colour = .data[["destaque"]]),
                           width = 0, linewidth = .7, orientation = "y") +
    ggplot2::geom_point(ggplot2::aes(x = .data[["condval"]], colour = .data[["destaque"]]), size = 1.9) +
    ggplot2::scale_colour_manual(values = c(`intervalo não cruza zero` = .TR_MODELS_COR,
                                            `cruza zero` = .TR_MODELS_CINZA), name = NULL, drop = FALSE) +
    ggplot2::facet_wrap(~termo, scales = "free_x", nrow = 1L) +
    ggplot2::labs(x = "efeito aleatório previsto", y = g,
                  caption = sprintf("barras: %s%s", intervalo,
                                    if (isTRUE(faixa_desvio)) " · faixa: ±1 desvio padrão do componente" else ""))
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}
