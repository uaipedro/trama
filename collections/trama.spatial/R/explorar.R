# Painel exploratório: o `plot(geodata)` do geoR, em ggplot.
#
# Composição. O painel completo sai de `trama.view::tr_combine()`, que a `view`
# exporta justamente para isto. `facet_wrap` sobre uma tabela longa não serve
# aqui: o postplot precisa de `coord_equal()` e a variável contra a coordenada
# precisa de eixos livres, e um facet impõe um só `coord` aos quatro. O
# `patchwork` já é dependência da `view`; a `spatial` não declara nada novo.

.TR_SPATIAL_VISTAS <- c("completo", "mapa", "x", "y", "histograma")

#' Os quartis da variável, como fator ordenado, como no postplot do `geoR`.
#'
#' Quando há valores repetidos, os cortes coincidem e `cut()` recusaria: as
#' classes repetidas se fundem, e o gráfico mostra menos de quatro. Com a
#' variável constante há uma classe só, rotulada pelo valor.
#' @noRd
.tr_spatial_quartis <- function(z) {
  cortes <- unique(stats::quantile(z, 0:4 / 4, names = FALSE))
  if (length(cortes) < 2L) {
    return(factor(rep(format(signif(z[[1]], 4)), length(z))))
  }
  cut(z, cortes, include.lowest = TRUE, dig.lab = 4, ordered_result = TRUE)
}

#' Rótulo de eixo de coordenada, com a unidade quando há.
#' @noRd
.tr_spatial_eixo <- function(nome, unidade) {
  if (is.na(unidade)) nome else sprintf("%s (%s)", nome, unidade)
}

#' A borda como camada, ou nada.
#' @noRd
.tr_spatial_camada_borda <- function(borda) {
  if (is.null(borda)) return(NULL)
  b <- data.frame(x = borda[, 1], y = borda[, 2])
  ggplot2::geom_path(data = b, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
                     inherit.aes = FALSE, linewidth = 0.4)
}

#' Postplot: o ponto no lugar, com o quartil em tamanho E cor.
#'
#' Os dois canais levam a MESMA variável e o MESMO título, e o ggplot os funde
#' numa legenda só. Tamanho sozinho lê em preto e branco; cor sozinha não lê para
#' quem não distingue as cores; juntos, o gráfico sobrevive a qualquer dos dois.
#' O preenchimento é viridis, que sobe em luminosidade, e o contorno é a tinta
#' do tema, para que o quartil mais escuro não suma num fundo escuro.
#' `coord_equal()` sempre: escala diferente nos eixos distorce a geometria.
#' @noRd
.tr_spatial_painel_mapa <- function(pontos) {
  d <- data.frame(x = pontos$coords[, 1], y = pontos$coords[, 2],
                  q = .tr_spatial_quartis(pontos$dados[[pontos$variavel]]))
  n <- nlevels(d$q)
  ggplot2::ggplot(d, ggplot2::aes(x = .data[["x"]], y = .data[["y"]])) +
    .tr_spatial_camada_borda(pontos$borda) +
    ggplot2::geom_point(ggplot2::aes(fill = .data[["q"]], size = .data[["q"]]), shape = 21,
                        stroke = 0.4) +
    ggplot2::scale_fill_viridis_d(name = pontos$variavel, begin = 0.1, end = 1, drop = FALSE) +
    ggplot2::scale_size_manual(name = pontos$variavel,
                               values = seq(1.2, 4, length.out = max(n, 2L))[seq_len(n)],
                               drop = FALSE) +
    ggplot2::coord_equal() +
    ggplot2::guides(x = ggplot2::guide_axis(n.dodge = 2)) +
    ggplot2::labs(x = .tr_spatial_eixo(pontos$coord_cols[[1]], pontos$unidade),
                  y = .tr_spatial_eixo(pontos$coord_cols[[2]], pontos$unidade))
}

#' A variável contra uma coordenada: onde tendência de larga escala aparece.
#' @noRd
.tr_spatial_painel_coord <- function(pontos, eixo) {
  d <- data.frame(c = pontos$coords[, eixo], z = pontos$dados[[pontos$variavel]])
  ggplot2::ggplot(d, ggplot2::aes(x = .data[["c"]], y = .data[["z"]])) +
    ggplot2::geom_point(alpha = 0.7) +
    # Rótulos de UTM têm sete dígitos e, num quarto de figura, se atropelam.
    ggplot2::guides(x = ggplot2::guide_axis(n.dodge = 2)) +
    ggplot2::labs(x = .tr_spatial_eixo(pontos$coord_cols[[eixo]], pontos$unidade),
                  y = pontos$variavel)
}

#' A distribuição da variável.
#' @noRd
.tr_spatial_painel_histograma <- function(pontos) {
  d <- data.frame(z = pontos$dados[[pontos$variavel]])
  ggplot2::ggplot(d, ggplot2::aes(x = .data[["z"]])) +
    ggplot2::geom_histogram(bins = grDevices::nclass.Sturges(d$z)) +
    ggplot2::labs(x = pontos$variavel, y = "frequência")
}

#' Painel exploratório de um objeto espacial.
#'
#' A pergunta das aulas de análise exploratória espacial, em quatro vistas: onde
#' estão os pontos e quanto valem (postplot por quartil, dentro da borda), a
#' variável contra cada coordenada (é aqui que tendência de larga escala
#' aparece) e a distribuição.
#' @param pontos um objeto espacial (`spatial/points`).
#' @param vista `"completo"`, `"mapa"`, `"x"`, `"y"` ou `"histograma"`.
#' @param aspecto,tema,titulo,rotulo_x,rotulo_y,legenda cosméticos (ver `trama.view`).
#' @return um ggplot (`view/plot`).
#' @export
tr_spatial_explore <- function(pontos, vista = "completo", aspecto = "1:1",
                               tema = "padrão", titulo = "", rotulo_x = "",
                               rotulo_y = "", legenda = "direita") {
  .tr_spatial_pontos_conferir(pontos)
  if (!is.character(vista) || length(vista) != 1L || !vista %in% .TR_SPATIAL_VISTAS) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Vista: escolha um de %s.", paste(.TR_SPATIAL_VISTAS, collapse = ", ")))
  }
  if (vista == "completo") {
    return(trama.view::tr_combine(
      list(.tr_spatial_painel_mapa(pontos), .tr_spatial_painel_histograma(pontos),
           .tr_spatial_painel_coord(pontos, 1L), .tr_spatial_painel_coord(pontos, 2L)),
      por_linha = 2, etiquetas = "nenhuma", aspecto = aspecto, tema = tema,
      titulo = titulo, rotulo_x = rotulo_x, rotulo_y = rotulo_y, legenda = legenda))
  }
  p <- switch(vista,
    mapa = .tr_spatial_painel_mapa(pontos),
    x = .tr_spatial_painel_coord(pontos, 1L),
    y = .tr_spatial_painel_coord(pontos, 2L),
    histograma = .tr_spatial_painel_histograma(pontos))
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}
