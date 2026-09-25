# Avaliar um desenho pela simulação: sortear de novo, muitas vezes, e olhar a
# distribuição das estimativas contra a verdade da população.
#
# É a pergunta que o card de UMA amostra não responde: o intervalo de 95% cobre
# a verdade 95% das vezes? O erro padrão que o estimador calcula é o erro que
# as estimativas de fato têm? A amostra do card é uma realização; a simulação
# mostra o desenho.

.TR_SAMPLING_CAMPOS_SIMULACAO <- c("replicas", "resumo", "verdadeiro", "rotulo", "variavel", "estimador",
                                   "confianca")

#' Simula um desenho contra a população conhecida.
#' @param amostra uma amostra de um bloco de seleção (guarda a receita e a população).
#' @param variavel coluna numérica da população.
#' @param estimador `"média"` ou `"total"`.
#' @param repeticoes quantas amostras sortear.
#' @param confianca nível de confiança, entre 0,5 e 0,999 (0,95 = 95%).
#' @param .seed semente (do núcleo).
#' @return uma simulação (`sampling/simulation`).
#' @export
tr_sampling_simulate <- function(amostra, variavel = "", estimador = "média", repeticoes = 500L,
                                 confianca = 0.95, .seed = 1L) {
  .tr_sampling_amostra_conferir(amostra)
  estimador <- .tr_sampling_enum(estimador, c("média", "total"), "estimador")
  R <- as.integer(.tr_sampling_num(repeticoes, "repeticoes", 20, 10000))
  conf <- .tr_sampling_conf(confianca)
  if (is.null(amostra$populacao) || is.null(amostra$receita)) .tr_sampling_refazer(amostra)
  pop <- amostra$populacao
  v <- .tr_sampling_col(pop, variavel, "variavel")
  .tr_sampling_numerica(pop, v, "variavel")
  y <- pop[[v]]
  verdade <- if (estimador == "média") mean(y, na.rm = TRUE) else sum(y, na.rm = TRUE)
  tipo <- if (estimador == "média") "media" else "total"
  reps <- .tr_sampling_com_semente(.seed, lapply(seq_len(R), function(r) {
    s <- .tr_sampling_refazer(amostra)
    e <- .tr_sampling_estimar(s, s$dados[[v]], tipo, conf)
    c(e$estimativa, e$erro_padrao, e$li, e$ls)
  }))
  m <- do.call(rbind, reps)
  replicas <- tibble::tibble(replica = seq_len(R), estimativa = m[, 1], erro_padrao = m[, 2],
                             li = m[, 3], ls = m[, 4], cobre = m[, 3] <= verdade & verdade <= m[, 4])
  resumo <- tibble::tibble(
    desenho = amostra$rotulo, n = amostra$n, estimador = estimador, variavel = v, repeticoes = R,
    verdadeiro = verdade, media_estimativas = mean(replicas$estimativa),
    vies_relativo_pct = 100 * (mean(replicas$estimativa) - verdade) / verdade,
    ep_empirico = stats::sd(replicas$estimativa),
    ep_estimado = sqrt(mean(replicas$erro_padrao^2)),
    reqm = sqrt(mean((replicas$estimativa - verdade)^2)),
    cobertura_pct = 100 * mean(replicas$cobre))
  structure(list(replicas = replicas, resumo = resumo, verdadeiro = verdade, rotulo = amostra$rotulo,
                 variavel = v, estimador = estimador, confianca = conf),
            class = "tr_sampling_simulation")
}

#' A legenda curta de uma simulação: EP e cobertura.
#' @noRd
.tr_sampling_legenda_sim <- function(r) {
  sprintf("EP %s (estimado %s) · viés %s%% · cobre %s%%", .tr_sampling_fmt(r$ep_empirico),
          .tr_sampling_fmt(r$ep_estimado), .tr_sampling_fmt(r$vies_relativo_pct, 2),
          .tr_sampling_fmt(r$cobertura_pct))
}

#' O card de uma simulação: histograma, verdade e média das estimativas.
#' @noRd
.tr_sampling_plot_uma <- function(sim) {
  r <- sim$resumo
  d <- as.data.frame(sim$replicas)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["estimativa"]])) +
    ggplot2::geom_histogram(bins = 30L, fill = .TR_SAMPLING_COR, colour = NA, alpha = .8) +
    ggplot2::geom_vline(xintercept = sim$verdadeiro, linewidth = .9) +
    ggplot2::geom_vline(xintercept = r$media_estimativas, linewidth = .7, linetype = "dashed",
                        colour = .TR_SAMPLING_COR_2) +
    ggplot2::labs(x = sprintf("%s de %s em %d amostras (linha cheia: verdade; tracejada: média)",
                              sim$estimador, sim$variavel, r$repeticoes),
                  y = "amostras", subtitle = paste(sim$rotulo, "·", .tr_sampling_legenda_sim(r)))
  trama.view::tr_view_finish(p, "16:9", "padrão", "", "", "", "direita")
}

#' Compara simulações: uma faixa por desenho, a verdade e a cobertura.
#' @param simulacoes uma simulação ou uma lista delas (`sampling/simulation`).
#' @param aspecto,tema,titulo,rotulo_x,rotulo_y,legenda cosméticos (ver `trama.view`).
#' @return um ggplot (`view/plot`).
#' @export
tr_sampling_plot_simulation <- function(simulacoes, aspecto = "16:9", tema = "padrão", titulo = "",
                                        rotulo_x = "", rotulo_y = "", legenda = "direita") {
  if (inherits(simulacoes, "tr_sampling_simulation")) simulacoes <- list(simulacoes)
  for (s in simulacoes) .tr_sampling_simulacao_conferir(s)
  rot <- vapply(simulacoes, function(s) sprintf("%s (n = %d)", s$rotulo, s$resumo$n), "")
  rot <- make.unique(rot, sep = " #")
  d <- do.call(rbind, Map(function(s, r) data.frame(desenho = r, estimativa = s$replicas$estimativa), simulacoes, rot))
  d$desenho <- factor(d$desenho, levels = rev(rot))
  info <- data.frame(desenho = factor(rot, levels = rev(rot)),
                     texto = vapply(simulacoes, function(s) .tr_sampling_legenda_sim(s$resumo), ""),
                     x = min(d$estimativa))
  verdades <- unique(vapply(simulacoes, function(s) s$verdadeiro, 0))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["estimativa"]], y = .data[["desenho"]])) +
    ggplot2::geom_violin(fill = .TR_SAMPLING_COR, colour = NA, alpha = .55, scale = "width", orientation = "y") +
    ggplot2::geom_boxplot(width = .12, outlier.shape = NA, fill = NA, orientation = "y") +
    ggplot2::geom_vline(xintercept = verdades, linewidth = .8) +
    ggplot2::geom_text(data = info, ggplot2::aes(x = .data[["x"]], y = .data[["desenho"]], label = .data[["texto"]]),
                       hjust = 0, vjust = -2.2, size = 3.1) +
    ggplot2::labs(y = NULL, x = sprintf("%s de %s (linha: verdade da população)",
                                        simulacoes[[1]]$estimador, simulacoes[[1]]$variavel))
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}
