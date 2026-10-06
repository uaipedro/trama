#' Classes de erro da coleção `spatial`.
#'
#' Mesma doutrina das irmãs: uma tabela que um teste confere contra o código,
#' pra não envelhecer em silêncio.
#'
#' Geoestatística tem o modo de falha que sai VERDE: o variograma calculado em
#' graus de latitude, que devolve número e gráfico plausíveis e está errado por
#' um fator que varia com a latitude; a krigagem simples sem média, que o motor
#' aceita e resolve com beta zero; o ajuste que devolve contribuição negativa
#' sem avisar. Aqui cada um vira card vermelho dizendo o que fazer.
#' @return data.frame com `class` e `when`.
#' @export
tr_spatial_errors <- function() {
  e <- c(
    tr_spatial_error_unknown_column = "param nomeia coluna que não existe na tabela de entrada",
    tr_spatial_error_blank_param = "param obrigatório deixado em branco no card",
    tr_spatial_error_bad_option = "param de escolha ou número fora do conjunto aceito",
    tr_spatial_error_not_numeric = "coluna usada como coordenada ou variável não é numérica",
    tr_spatial_error_geographic_crs = "as coordenadas estão em graus: a distância do variograma não é métrica",
    tr_spatial_error_bad_coords = "coordenada faltante, infinita, ou menos de três pontos distintos",
    tr_spatial_error_too_few = "pontos de menos para estimar o variograma ou krigar",
    tr_spatial_error_empty_variogram = "nenhuma classe de distância sobreviveu ao mínimo de pares",
    tr_spatial_error_no_convergence = "o ajuste não convergiu ou saiu singular a partir de todos os valores iniciais tentados, sem defeito de sinal ou de escala nos números devolvidos",
    tr_spatial_error_bad_fit = "o ajuste devolveu pepita, contribuição ou alcance inválido: negativo, ou alcance prático fora da escala dos dados (só singular ou sem convergência é no_convergence)",
    tr_spatial_error_no_mean = "krigagem simples pedida sem a média conhecida",
    tr_spatial_error_empty_grid = "a grade ficou vazia depois do recorte na borda",
    tr_spatial_error_bad_border = "a borda não é um polígono fechado utilizável",
    tr_spatial_error_negative_variance =
      "a variância de krigagem saiu negativa: o modelo não é definido positivo",
    tr_spatial_error_not_points = "o nó produziu objeto que não é de pontos, e o tipo spatial/points o recusa",
    tr_spatial_error_not_a_variogram =
      "o nó produziu objeto que não é variograma, e o tipo spatial/variogram o recusa",
    tr_spatial_error_not_a_model =
      "o nó produziu objeto que não é modelo, e o tipo spatial/model o recusa",
    tr_spatial_error_not_a_surface =
      "o nó produziu objeto que não é superfície, e o tipo spatial/surface o recusa"
  )
  data.frame(class = names(e), when = unname(e), stringsAsFactors = FALSE)
}

#' O único lugar que levanta erro da coleção.
#' @noRd
.tr_spatial_abort <- function(class, msg, ...) {
  rlang::abort(msg, class = c(class, "tr_spatial_error"), ...)
}
