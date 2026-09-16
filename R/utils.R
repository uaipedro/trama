#' O relógio do motor, numa costura só.
#'
#' Existe para ser mockável. `Sys.time()` é função base, e
#' `testthat::local_mocked_bindings()` só substitui binding que o pacote define
#' ou importa — então os testes de cadência e de `duration` só podiam ser
#' escritos com sono de verdade e limite de parede. Isso mede a velocidade da
#' máquina em vez da aritmética, e piscou vermelho uma vez em cinco execuções
#' numa suíte de mil e quinhentos testes: vermelho intermitente sem nome queima
#' o tempo de quem investiga e, pior, ensina a ignorar vermelho.
#'
#' Só o driver da região usa esta costura, porque é lá que o tempo é LÓGICA
#' (estrangulamento de publicação, `duration` que exclui as leituras de fora) e
#' não só registro. O resto do pacote segue chamando `Sys.time()` direto: um
#' `created` de handle não decide nada.
#' @noRd
.tr_now <- function() Sys.time()

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Ids são sempre qualificados por coleção (`coleção/nome`).
#'
#' No insumo o id era global e o registro abortava com "id duplicado" — o que
#' significa que carregar duas coleções razoáveis viraria erro de carga:
#' `normalize` é nome óbvio demais pra pertencer a um domínio só. O id é
#' chave estrangeira em documento salvo, então qualificar depois invalidaria
#' todo documento existente; por isso é regra desde a primeira linha.
#' @noRd
.tr_check_id <- function(id, what = "id") {
  if (!is.character(id) || length(id) != 1L || is.na(id)) {
    rlang::abort(sprintf("%s deve ser uma string.", what), class = "tr_error_bad_id")
  }
  if (!grepl("^[a-z][a-z0-9_]*/[a-z_][a-z0-9_]*$", id)) {
    rlang::abort(
      sprintf("%s inválido: '%s'. Formato esperado: 'colecao/nome' (minúsculas, _ e dígitos).", what, id),
      class = "tr_error_bad_id"
    )
  }
  invisible(id)
}

.tr_collection_of <- function(id) sub("/.*$", "", id)
