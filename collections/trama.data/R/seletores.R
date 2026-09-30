# Seletores de coluna escritos como R, só de uma lista fechada.
#
# `data/select` aceita, além da lista de nomes ("regiao, valor"), os seletores do
# tidyselect — `starts_with("x")`, `where(is.numeric)`, `everything()`. O texto é
# lido como R, mas NUNCA avaliado como código livre: a árvore sintática é
# percorrida e só passa o que está na lista abaixo. Qualquer outra chamada
# (`system()`, `paste()`, `(function() ...)()`) é recusada antes do dplyr ver.
# (Os nós `data/mutate` e `data/group_summarise` avaliam R livre de propósito,
# porque a conta É o param; aqui o param é só "quais colunas", então a lista
# fechada custa nada e evita que um fluxo aberto de fora rode o que quiser.)

# Chamadas que recebem só literais (texto; e `ignore.case` lógico).
.TR_DATA_SEL_TEXTO <- c("starts_with", "ends_with", "contains", "matches")
# Predicados aceitos em `where()`.
.TR_DATA_SEL_PREDICADOS <- c("is.numeric", "is.integer", "is.double", "is.character",
                             "is.factor", "is.logical")
.TR_DATA_SEL_OPERADORES <- c("c", ":", "!", "-", "&", "|", "(")

.tr_data_sel_recusa <- function(item, motivo) {
  rlang::abort(
    sprintf(paste0("Selecionar: '%s' não é aceito (%s). Use nomes de coluna ou ",
                   "starts_with(), ends_with(), contains(), matches(), where(is.numeric | is.character | ",
                   "is.factor | is.logical), everything(), last_col(), combinados com c(), :, !, - , & e |."),
            item, motivo),
    class = "tr_data_error_bad_expr")
}

#' Confere uma expressão de seletor contra a lista fechada; devolve os símbolos
#' (nomes de coluna) que ela cita, para conferir a existência depois.
#' @noRd
.tr_data_sel_validar <- function(e, item) {
  if (is.character(e) && length(e) == 1L) return(character())
  if (is.numeric(e)) .tr_data_sel_recusa(item, "posi\u00E7\u00E3o num\u00E9rica n\u00E3o \u00E9 aceita; use o nome da coluna")
  if (is.symbol(e)) return(as.character(e))
  if (!is.call(e)) .tr_data_sel_recusa(item, "expressão inesperada")
  fn <- e[[1]]
  if (!is.symbol(fn)) .tr_data_sel_recusa(item, "chamada de função não permitida")
  nome <- as.character(fn)
  args <- as.list(e)[-1]
  if (nome %in% .TR_DATA_SEL_OPERADORES) {
    if (any(nzchar(names(args) %||% ""))) .tr_data_sel_recusa(item, "argumento nomeado (renomear) n\u00E3o \u00E9 aceito")
    return(unlist(lapply(args, .tr_data_sel_validar, item = item), use.names = FALSE) %||% character())
  }
  if (nome %in% .TR_DATA_SEL_TEXTO) {
    nm <- names(args) %||% rep("", length(args))
    ok <- vapply(seq_along(args), function(i) {
      if (nzchar(nm[[i]])) nm[[i]] == "ignore.case" && (isTRUE(args[[i]]) || isFALSE(args[[i]]))
      else is.character(args[[i]]) && length(args[[i]]) == 1L
    }, logical(1))
    if (!length(args) || !all(ok)) .tr_data_sel_recusa(item, sprintf("%s() recebe textos entre aspas", nome))
    return(character())
  }
  if (nome == "where") {
    if (length(args) != 1L || !is.symbol(args[[1]]) || !as.character(args[[1]]) %in% .TR_DATA_SEL_PREDICADOS) {
      .tr_data_sel_recusa(item, paste0("where() aceita ", paste(.TR_DATA_SEL_PREDICADOS, collapse = ", ")))
    }
    return(character())
  }
  if (nome == "everything" && !length(args)) return(character())
  if (nome == "last_col") {
    n <- if (length(args) == 1L) args[[1]] else NULL
    if (!length(args) || (is.numeric(n) && length(n) == 1L && is.finite(n) && n >= 0 && n == round(n) && is.null(names(args)))) {
      return(character())
    }
  }
  .tr_data_sel_recusa(item, sprintf("função '%s' fora da lista", nome))
}

#' O param `cols` é lista de nomes ou seletor? Devolve as expressões vetadas, ou
#' NULL quando é a lista de nomes de sempre.
#'
#' Nome exato de coluna ganha: uma coluna chamada "Preço (R$)" continua sendo
#' lida como nome. Só cai no seletor quem tem parêntese ou `:` (intervalo `a:c`) e não é uma
#' lista de nomes que existem.
#' @noRd
.tr_data_seletor <- function(texto, dados) {
  if (length(texto) != 1L || !grepl("[(:]", texto)) return(NULL)
  if (all(.as_cols(texto) %in% names(dados))) return(NULL)
  # Lista de nomes com parêntese que não parseia (`Preço (R$), zz`): volta ao
  # caminho dos nomes, onde o erro é o de sempre ("coluna inexistente: zz").
  es <- tryCatch(.tr_data_parse_exprs(texto, "cols"), tr_data_error_bad_expr = function(e) NULL)
  if (is.null(es)) return(NULL)
  simbolos <- unlist(lapply(es, .tr_data_sel_validar, item = texto), use.names = FALSE)
  if (length(simbolos)) .tr_data_cols(dados, unique(simbolos), "cols")
  es
}
