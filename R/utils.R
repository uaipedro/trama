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
      .tr_msg("utils.bad_id_format", what, id),
      class = "tr_error_bad_id"
    )
  }
  invisible(id)
}

.tr_collection_of <- function(id) sub("/.*$", "", id)
