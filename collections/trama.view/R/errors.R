#' Classes de erro da coleção `view`.
#'
#' Mesmo princípio de `trama::tr_errors()` e de `trama.data::tr_data_errors()`,
#' e mesmo teste de duas direções: uma tabela que um teste confere contra o
#' código, pra não envelhecer em silêncio.
#' @return data.frame com `class` e `when`.
#' @export
tr_view_errors <- function() {
  e <- c(
    tr_view_error_unknown_column = "param nomeia coluna que não existe na tabela de entrada",
    tr_view_error_blank_param =
      "param obrigatório do gráfico deixado em branco (eixo sem coluna não tem desenho honesto)",
    tr_view_error_bad_option = "param de escolha recebeu valor fora do conjunto aceito",
    tr_view_error_not_numeric =
      "medida de gráfico que agrega, ou coluna em eixo log, aponta para coluna que não é número",
    tr_view_error_not_positive =
      "eixo em log sobre coluna com valor <= 0, ou Pareto sobre coluna com valor negativo",
    tr_view_error_levels =
      "coluna de condição com menos valores distintos do que o gráfico liga (halteres, pareamento)",
    tr_view_error_not_unique =
      "pareamento com o par unidade x condição repetido: não há uma medida por unidade",
    tr_view_error_not_a_plot =
      "o nó produziu um objeto que não é um ggplot, e o tipo view/plot recusa guardá-lo"
  )
  data.frame(class = names(e), when = unname(e), stringsAsFactors = FALSE)
}

#' Confere que a coluna citada existe, e devolve o nome.
#'
#' Não usa `any_of()`/`intersect()` pelo mesmo motivo que a coleção `data` não
#' usa: eles descartam nome inexistente em SILÊNCIO, e um gráfico que ignora o
#' eixo mal escrito sai plausível e errado.
#' @noRd
.tr_view_col <- function(data, col, param) {
  if (!col %in% names(data)) {
    rlang::abort(
      sprintf("Param '%s': coluna inexistente: %s. Disponíveis: %s.",
              param, col, paste(names(data), collapse = ", ")),
      class = "tr_view_error_unknown_column")
  }
  col
}

#' Recusa param obrigatório em branco.
#'
#' A doutrina do param vazio, herdada da coleção `data`: vazio é "desligado"
#' quando o nó tem comportamento honesto para o vazio — `cor` em branco é
#' gráfico sem mapeamento de cor, `titulo` em branco é gráfico sem título — e
#' vazio é ERRO quando não tem, que é o caso de um eixo. Um disperso sem coluna
#' no X não tem desenho degradado: tem desenho nenhum.
#' @noRd
.tr_view_obrigatorio <- function(valor, param) {
  if (!length(valor) || !nzchar(trimws(as.character(valor)[[1]]))) {
    rlang::abort(
      sprintf("Param '%s': campo obrigatório em branco. Preencha-o no card.", param),
      class = "tr_view_error_blank_param")
  }
  as.character(valor)[[1]]
}

#' Valor fora do conjunto aceito de um param de escolha.
#'
#' O enum protege pelo lado da UI, mas estes `fn` são nível 1 e chamáveis no
#' console — que é o caminho pelo qual esta coleção se testa. Sem isto, um
#' `tema = "escurro"` cairia num `switch` sem default e devolveria NULL, que o
#' ggplot soma sem reclamar: o gráfico sai, sem tema, em silêncio.
#' @noRd
.tr_view_option <- function(param, valor, aceitos) {
  rlang::abort(
    sprintf("Param '%s': valor inválido '%s'. Aceitos: %s.",
            param, paste(as.character(valor), collapse = ", "),
            paste(aceitos, collapse = ", ")),
    class = "tr_view_error_bad_option")
}
