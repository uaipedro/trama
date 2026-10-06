#' Guarda de classe e de campos obrigatórios, usada por todo `store`.
#' @noRd
.tr_spatial_guard <- function(x, classe, campos, erro, artigo) {
  if (!inherits(x, classe) || !all(campos %in% names(x))) {
    .tr_spatial_abort(erro, sprintf("O valor recebido não é %s.", artigo))
  }
  invisible(x)
}

#' NA vira NULL no JSON (o front lê `null` como "sem valor").
#' @noRd
.tr_spatial_nulo <- function(x) if (length(x) != 1L || is.na(x)) NULL else x

#' Uma tabela no formato que o renderer `trama/table` do núcleo lê.
#' @noRd
.tr_spatial_linhas_json <- function(df, max = 60L) {
  df <- utils::head(as.data.frame(df), max)
  list(columns = as.list(names(df)),
       rows = lapply(seq_len(nrow(df)), function(i) {
         lapply(df[i, , drop = FALSE], function(v) {
           v <- v[[1]]
           if (is.factor(v)) as.character(v)
           else if (length(v) == 1L && is.na(v)) NULL
           else if (is.double(v)) signif(v, 6) else v
         })
       }))
}

#' Cosméticos de gráfico, com a proporção padrão da coleção.
#' @noRd
.tr_spatial_props <- function(..., .aspecto = "1:1") {
  ps <- trama.view::tr_view_props(...)
  ps$aspecto$default <- .aspecto
  ps
}
