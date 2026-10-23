# Os gráficos da superfície predita e do modelo ajustado.

.TR_SPATIAL_MOSTRAR <- c("predito", "erro-padrao")

#' A grade tem espaçamento regular nos dois eixos?
#'
#' O `geom_raster` só vale com retícula regular. Grade gerada pelo bloco sempre
#' é; grade ligada na porta pode ser qualquer coisa.
#' @noRd
.tr_spatial_grade_regular <- function(x, y) {
  passo <- function(v) {
    u <- sort(unique(v))
    if (length(u) < 3L) return(TRUE)
    d <- diff(u)
    max(abs(d - stats::median(d))) <= 1e-6 * stats::median(d)
  }
  passo(x) && passo(y)
}

#' O rótulo da legenda do mapa.
#'
#' Superfície vinda de indicador é PROBABILIDADE, e apresentá-la com o nome da
#' variável original a venderia como rendimento, área ou teor — o erro que o
#' recorte em [0,1] já evita no número, e que o rótulo evita na leitura.
#' @noRd
.tr_spatial_map_rotulo <- function(x, mostrar) {
  if (identical(mostrar, "predito") && !is.null(x$indicador)) {
    return(sprintf("Probabilidade de %s %s %s",
                   x$indicador$variavel_original, x$indicador$sentido,
                   format(signif(x$indicador$corte, 6))))
  }
  if (identical(mostrar, "predito")) return(x$variavel)
  if (!is.null(x$indicador)) return("Erro-padrão da probabilidade")
  paste("erro-padrão de", x$variavel)
}

#' Mapa da superfície predita, ou do erro-padrão.
#'
#' O mapa do erro-padrão é o par honesto do mapa do predito: mostra onde a
#' predição vale pouco. Por isso os dois saem do mesmo bloco, com um param, em
#' vez de só o bonito sair fácil.
#' @param superficie uma superfície (`spatial/surface`).
#' @param mostrar `"predito"` ou `"erro-padrao"`.
#' @param isolinhas acrescenta curvas de nível.
#' @param pontos desenha as localizações amostrais por cima.
#' @param aspecto,tema,titulo,rotulo_x,rotulo_y,legenda cosméticos.
#' @return um ggplot (`view/plot`).
#' @export
tr_spatial_map <- function(superficie, mostrar = "predito", isolinhas = FALSE,
                           pontos = TRUE, aspecto = "1:1", tema = "padrão",
                           titulo = "", rotulo_x = "", rotulo_y = "",
                           legenda = "direita") {
  .tr_spatial_superficie_conferir(superficie)
  if (!is.character(mostrar) || length(mostrar) != 1L || !mostrar %in% .TR_SPATIAL_MOSTRAR) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Mostrar: escolha um de %s.", paste(.TR_SPATIAL_MOSTRAR, collapse = ", ")))
  }
  cols <- superficie$pontos$coord_cols
  g <- as.data.frame(superficie$grade)
  d <- data.frame(x = g[[cols[[1]]]], y = g[[cols[[2]]]],
                  valor = if (mostrar == "predito") g$predito else g$erro_padrao)
  nome <- .tr_spatial_map_rotulo(superficie, mostrar)
  # `geom_raster` exige retícula regular. A porta Grade aceita pontos
  # escolhidos — a ajuda recomenda isso —, e com espaçamento desigual o raster
  # desenha as células DESLOCADAS, com um aviso do ggplot que não chega ao card:
  # mapa errado e plausível. Com grade irregular usa-se `geom_tile`, que aceita
  # qualquer espaçamento.
  regular <- .tr_spatial_grade_regular(d$x, d$y)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]))
  p <- p + if (regular) ggplot2::geom_raster(ggplot2::aes(fill = .data[["valor"]]))
           else ggplot2::geom_tile(ggplot2::aes(fill = .data[["valor"]]))
  # Célula sem predição (nenhum ponto no raio) fica cinza, e não transparente:
  # um buraco vazio no mapa parece borda; o cinza diz "aqui não há estimativa".
  p <- p + ggplot2::scale_fill_continuous(name = nome, na.value = "grey60")
  if (isTRUE(isolinhas)) {
    p <- p + ggplot2::geom_contour(ggplot2::aes(z = .data[["valor"]]), colour = "grey35",
                                   linewidth = 0.3, na.rm = TRUE)
  }
  p <- p + .tr_spatial_camada_borda(superficie$borda)
  if (isTRUE(pontos)) {
    amostra <- data.frame(x = superficie$pontos$coords[, 1], y = superficie$pontos$coords[, 2])
    p <- p + ggplot2::geom_point(data = amostra, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
                                 inherit.aes = FALSE, shape = 21, fill = "white",
                                 colour = "black", size = 1, stroke = 0.3)
  }
  # coord_equal() SEMPRE: mapa com escala diferente nos dois eixos distorce a
  # geometria, que é justamente o que o bloco existe para mostrar.
  p <- p + ggplot2::coord_equal() +
    ggplot2::labs(x = .tr_spatial_eixo(cols[[1]], superficie$unidade),
                  y = .tr_spatial_eixo(cols[[2]], superficie$unidade))
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' O variograma empírico com a curva ajustada: o card do modelo.
#'
#' A curva é a do próprio modelo (`gstat::variogramLine`), até a maior distância
#' do empírico, ou até 1,2 vez o alcance prático quando o modelo foi montado sem
#' variograma empírico. A linha tracejada é o patamar.
#' @noRd
.tr_spatial_plot_modelo <- function(modelo, aspecto = "4:3", tema = "padrão") {
  .tr_spatial_modelo_conferir(modelo)
  emp <- modelo$variograma$tabela
  tem <- !is.null(emp) && nrow(emp) > 0L
  ate <- if (tem) max(emp$u) else 1.2 * modelo$alcance_pratico
  # `variogramLine` SEM `dir` desenha a direção LESTE (dir = c(1,0,0)). Com
  # anisotropia isso punha no card a curva de um eixo que o usuário não
  # escolheu, sobre os pontos do variograma OMNIDIRECIONAL, contradizendo o
  # `alcance_pratico` do próprio objeto por um fator igual à razão. A curva
  # segue o EIXO MAIOR, que é o que `alcance` e `alcance_pratico` descrevem, e o
  # subtítulo diz qual direção é — senão o card mostra uma curva sem dizer de
  # onde ela é.
  razao <- modelo$razao %||% 1
  angulo <- modelo$angulo %||% 0
  dir_maior <- c(sin(angulo * pi / 180), cos(angulo * pi / 180), 0)
  curva <- gstat::variogramLine(.tr_spatial_vgm_model(modelo), maxdist = ate,
                                n = 200L, dir = dir_maior)
  unid <- modelo$variograma$unidade
  if (is.null(unid)) unid <- NA_character_
  p <- ggplot2::ggplot()
  if (tem) {
    p <- p + ggplot2::geom_point(data = emp, ggplot2::aes(x = .data[["u"]], y = .data[["gamma"]]))
  }
  p <- p +
    ggplot2::geom_line(data = curva, ggplot2::aes(x = .data[["dist"]], y = .data[["gamma"]])) +
    ggplot2::geom_hline(yintercept = modelo$patamar, linetype = "dashed", linewidth = 0.3) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = .tr_spatial_eixo("distância", unid), y = "semivariância",
                  subtitle = if (razao > 1) sprintf(
                    paste("curva na direção do eixo maior (%g°), razão %g;",
                          "os pontos são do variograma omnidirecional"),
                    angulo, razao) else NULL)
  trama.view::tr_view_finish(p, aspecto, tema)
}
