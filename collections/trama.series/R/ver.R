# Gráficos: todos saem no tipo `view/plot`, da coleção `view`.
#
# Não há segundo tipo de gráfico. O card, a proporção, o tema e o lightbox são
# os da `view`, pela costura que ela exporta (`tr_view_props`,
# `tr_view_finish`), e um correlograma se comporta no canvas exatamente como
# um disperso. Todo `fn` termina em `trama.view::tr_view_finish()`; nenhum
# aplica tema por conta própria — e nenhum chama `theme()` antes dele, porque
# o tema completo que o funil soma por cima apagaria o ajuste em silêncio.

.TR_SERIES_CINZA <- "#8b949e"

#' A série no tempo.
#' @export
tr_series_plot <- function(serie, pontos = FALSE, aspecto = "16:9", tema = "padrão",
                           titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  d <- .tr_series_tabela(serie)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["tempo"]], y = .data[["valor"]])) +
    ggplot2::geom_line(linewidth = .6, colour = .TR_SERIES_COR, na.rm = TRUE)
  if (isTRUE(pontos)) p <- p + ggplot2::geom_point(size = 1.2, colour = .TR_SERIES_COR, na.rm = TRUE)
  p <- p + ggplot2::labs(x = "tempo", y = "valor")
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Quantas defasagens mostrar: a do R (10·log10 n), mas nunca menos de três
#' ciclos numa série sazonal — o correlograma de uma série mensal que para na
#' defasagem 21 esconde o pico da 24, que é metade da conversa.
#' @noRd
.tr_series_lagmax <- function(serie, defasagens) {
  n <- sum(!is.na(serie))
  f <- stats::frequency(serie)
  k <- .tr_series_int(defasagens, "defasagens", min = 0)
  if (k == 0L) k <- max(10 * log10(n), if (f > 1) 3 * f else 0)
  as.integer(min(floor(k), n - 1L))
}

#' O correlograma, ACF ou PACF.
#'
#' O eixo é em DEFASAGENS (1, 2, … 24), e não em anos como o `acf()` do R
#' desenha para série sazonal (0,083, 0,167, … 2): ninguém lê "o pico em 1,0"
#' como "o mesmo mês do ano anterior" sem fazer a conta. As marcas do eixo caem
#' nos múltiplos do ciclo, que é onde se procura sazonalidade.
#'
#' A banda é ±1,96/√n, a do ruído branco. As barras que passam dela saem
#' coloridas: é a leitura que se faz de um correlograma, feita pelo gráfico.
#' Com 24 defasagens, uma ou duas passam por acaso (5% de 24 é 1,2) — a ajuda
#' diz isso, porque é o engano clássico de quem lê correlograma.
#' @noRd
.tr_series_correlograma <- function(serie, defasagens, parcial) {
  no <- if (parcial) "series/pacf" else "series/acf"
  .tr_series_minimo(serie, 4L, no, "um correlograma")
  f <- stats::frequency(serie)
  k <- .tr_series_lagmax(serie, defasagens)
  fn <- if (parcial) stats::pacf else stats::acf
  a <- fn(serie, lag.max = k, plot = FALSE, na.action = stats::na.pass)
  d <- tibble::tibble(defasagem = round(as.numeric(a$lag) * f), r = as.numeric(a$acf))
  d <- d[d$defasagem > 0, ]
  banda <- stats::qnorm(.975) / sqrt(sum(!is.na(serie)))
  d$fora <- abs(d$r) > banda
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["defasagem"]], y = .data[["r"]])) +
    ggplot2::geom_hline(yintercept = 0, colour = .TR_SERIES_CINZA) +
    ggplot2::geom_hline(yintercept = c(-banda, banda), linetype = "dashed", colour = .TR_SERIES_COR_2) +
    ggplot2::geom_segment(ggplot2::aes(xend = .data[["defasagem"]], yend = 0, colour = .data[["fora"]]),
                          linewidth = 1.2) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = .TR_SERIES_CINZA, `TRUE` = .TR_SERIES_COR),
                                 labels = c(`FALSE` = "dentro da banda", `TRUE` = "fora da banda"),
                                 name = NULL) +
    ggplot2::labs(x = "defasagem", y = if (parcial) "autocorrelação parcial" else "autocorrelação")
  if (f > 1) p <- p + ggplot2::scale_x_continuous(breaks = seq(0, k, by = f))
  p
}

#' Correlograma: a autocorrelação em cada defasagem.
#' @export
tr_series_acf <- function(serie, defasagens = 0L, aspecto = "16:9", tema = "padrão",
                          titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  trama.view::tr_view_finish(.tr_series_correlograma(serie, defasagens, FALSE),
                             aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Autocorrelação parcial: a correlação com a defasagem k, descontadas as
#' intermediárias.
#' @export
tr_series_pacf <- function(serie, defasagens = 0L, aspecto = "16:9", tema = "padrão",
                           titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  trama.view::tr_view_finish(.tr_series_correlograma(serie, defasagens, TRUE),
                             aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Gráfico de defasagens: `x_t` contra `x_{t-k}`, um painel por k.
#'
#' É o correlograma sem o resumo: cada painel mostra a nuvem cuja correlação
#' é uma barra do ACF, e aí se vê o que o número esconde — relação curva, um
#' ponto sozinho puxando tudo, dois regimes. Numa série sazonal os pontos saem
#' coloridos pela estação, e o painel do ciclo (12 na mensal) é o que fica
#' colado na diagonal.
#' @export
tr_series_lag_plot <- function(serie, defasagens = 0L, aspecto = "16:9", tema = "padrão",
                               titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  f <- stats::frequency(serie)
  K <- .tr_series_int(defasagens, "defasagens", min = 0, max = 16)
  if (K == 0L) K <- if (f > 1) as.integer(min(f, 12)) else 4L
  n <- length(serie)
  .tr_series_minimo(serie, K + 3L, "series/lag_plot", sprintf("%d defasagens", K))
  x <- as.numeric(serie)
  est <- if (f > 1 && f <= 24) factor(.tr_series_estacoes(f)[as.integer(stats::cycle(serie))],
                                      levels = .tr_series_estacoes(f))
  rotulos <- paste("defasagem", seq_len(K))
  d <- do.call(rbind, lapply(seq_len(K), function(k) {
    tibble::tibble(painel = factor(rotulos[[k]], levels = rotulos),
                   anterior = x[seq_len(n - k)], atual = x[(k + 1):n],
                   estacao = if (is.null(est)) NA else est[(k + 1):n])
  }))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["anterior"]], y = .data[["atual"]])) +
    ggplot2::geom_abline(linetype = "dashed", colour = .TR_SERIES_CINZA)
  p <- if (is.null(est)) {
    p + ggplot2::geom_point(size = 1, alpha = .75, colour = .TR_SERIES_COR, na.rm = TRUE)
  } else {
    p + ggplot2::geom_point(ggplot2::aes(colour = .data[["estacao"]]), size = 1, alpha = .8, na.rm = TRUE)
  }
  p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[["painel"]])) +
    ggplot2::labs(x = "x(t - k)", y = "x(t)", colour = "estação")
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Ano e estação de cada observação — a tabela que os dois gráficos sazonais
#' desenham.
#' @noRd
.tr_series_por_estacao <- function(serie) {
  f <- stats::frequency(serie)
  ciclo <- as.integer(stats::cycle(serie))
  tibble::tibble(ciclo = ciclo,
                 ano = .tr_series_ano(as.numeric(stats::time(serie)), ciclo, f),
                 valor = as.numeric(serie))
}

#' Gráfico sazonal: um traço por ano, percorrendo o ciclo.
#'
#' Responde duas perguntas de uma vez: qual é o FORMATO da sazonalidade (o
#' pico é em julho?) e se ele MUDA com o tempo (os anos recentes, de cor mais
#' clara, repetem a forma dos antigos?). A cor é contínua pelo ano para que a
#' mudança se leia como degradê — na escala contínua do TEMA, que o gráfico não
#' fixa: trocar `continua` no tema do projeto troca este degradê também.
#' @export
tr_series_seasonal_plot <- function(serie, aspecto = "16:9", tema = "padrão", titulo = "",
                                    rotulo_x = "", rotulo_y = "", legenda = "direita") {
  .tr_series_sazonal(serie, "series/seasonal_plot", ciclos = 1L)
  f <- stats::frequency(serie)
  d <- .tr_series_por_estacao(serie)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["ciclo"]], y = .data[["valor"]],
                                       group = .data[["ano"]], colour = .data[["ano"]])) +
    ggplot2::geom_line(linewidth = .6, na.rm = TRUE) +
    ggplot2::labs(x = "estação", y = "valor", colour = "ano")
  if (f <= 24) {
    p <- p + ggplot2::scale_x_continuous(breaks = seq_len(f), labels = .tr_series_estacoes(f))
  }
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Subséries: um painel por estação, com os anos em sequência e a média.
#'
#' O complemento do gráfico sazonal: aquele mostra o formato do ciclo, este
#' mostra como CADA estação evoluiu. Um julho que sobe mais que os outros meses
#' — sazonalidade que muda — é invisível no sazonal e óbvio aqui.
#'
#' Até frequência 24: com 52 painéis semanais o gráfico vira uma parede.
#' @export
tr_series_subseries <- function(serie, aspecto = "16:9", tema = "padrão", titulo = "",
                                rotulo_x = "", rotulo_y = "", legenda = "direita") {
  .tr_series_sazonal(serie, "series/subseries")
  f <- stats::frequency(serie)
  if (f > 24) {
    .tr_series_abort("tr_series_error_bad_frequency",
                     paste0("'series/subseries' desenha um painel por estação, e com frequência %g ",
                            "seriam %g painéis. Agregue antes (series/aggregate)."), f, f)
  }
  d <- .tr_series_por_estacao(serie)
  d$estacao <- factor(.tr_series_estacoes(f)[d$ciclo], levels = .tr_series_estacoes(f))
  medias <- stats::aggregate(valor ~ estacao, data = d, FUN = mean)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["ano"]], y = .data[["valor"]])) +
    ggplot2::geom_hline(data = medias, ggplot2::aes(yintercept = .data[["valor"]]),
                        colour = .TR_SERIES_COR_2, linetype = "dashed") +
    ggplot2::geom_line(colour = .TR_SERIES_COR, na.rm = TRUE) +
    ggplot2::facet_wrap(ggplot2::vars(.data[["estacao"]]), nrow = 1L) +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::labs(x = "anos, dentro de cada estação", y = "valor")
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Os quatro componentes, empilhados, cada um na sua escala.
#'
#' Escala livre por painel, e é o que se tem de ler com cuidado: o sazonal com
#' amplitude de ±40 e o resto de ±10 saem com a MESMA altura. A escala de cada
#' painel está no eixo, e é ela que diz o tamanho do componente.
#' @export
# O default "4:3" é o mesmo que o spec do nó declara (`.tr_series_props(.aspecto
# = "4:3")` em R/collection.R), para que card e console desenhem igual.
tr_series_plot_decomposition <- function(decomposicao, aspecto = "4:3", tema = "padrão",
                                         titulo = "", rotulo_x = "", rotulo_y = "",
                                         legenda = "direita") {
  tab <- .tr_series_decomp_tabela(decomposicao)
  nomes <- c(observado = "observado", tendencia = "tendência", sazonal = "sazonal",
             regressor = "regressor", resto = "resto")
  # O painel do regressor só existe quando a decomposição o tem.
  if (is.null(decomposicao$regressor)) nomes <- nomes[names(nomes) != "regressor"]
  d <- do.call(rbind, lapply(names(nomes), function(nm) {
    tibble::tibble(tempo = tab$tempo, componente = factor(nomes[[nm]], levels = unname(nomes)),
                   valor = tab[[nm]])
  }))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["tempo"]], y = .data[["valor"]])) +
    ggplot2::geom_line(colour = .TR_SERIES_COR, na.rm = TRUE) +
    ggplot2::facet_grid(ggplot2::vars(.data[["componente"]]), scales = "free_y") +
    ggplot2::labs(x = "tempo", y = NULL,
                  subtitle = sprintf("%s, %s", decomposicao$metodo, decomposicao$tipo))
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' O histórico e a previsão, com os leques de 80 e 95%.
#'
#' `historico` corta o passado mostrado, e só ele: com 40 anos de série
#' mensal e 12 meses previstos, o leque vira um risco na ponta direita. Não
#' muda a previsão — ela já foi feita no nó anterior.
#' @export
tr_series_plot_forecast <- function(previsao, historico = 0L, aspecto = "16:9", tema = "padrão",
                                    titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  h <- .tr_series_int(historico, "historico", min = 0)
  hist <- .tr_series_tabela(previsao$x)
  if (h > 0L) hist <- utils::tail(hist, h)
  fc <- .tr_series_forecast_tabela(previsao)
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = fc, ggplot2::aes(x = .data[["tempo"]], ymin = .data[["li_95"]],
                                                 ymax = .data[["ls_95"]]),
                         fill = .TR_SERIES_COR_2, alpha = .25) +
    ggplot2::geom_ribbon(data = fc, ggplot2::aes(x = .data[["tempo"]], ymin = .data[["li_80"]],
                                                 ymax = .data[["ls_80"]]),
                         fill = .TR_SERIES_COR_2, alpha = .45) +
    ggplot2::geom_line(data = hist, ggplot2::aes(x = .data[["tempo"]], y = .data[["valor"]]),
                       colour = .TR_SERIES_CINZA, na.rm = TRUE) +
    ggplot2::geom_line(data = fc, ggplot2::aes(x = .data[["tempo"]], y = .data[["previsto"]]),
                       colour = .TR_SERIES_COR, linewidth = .8) +
    ggplot2::labs(x = "tempo", y = "valor", subtitle = previsao$method)
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}
