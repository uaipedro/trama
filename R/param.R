#' Declara um parâmetro de nó.
#'
#' `kind` decide o widget no front — e o front resolve o widget por um
#' registro (`registerWidget`), então uma coleção pode trazer `kind` novo sem
#' o núcleo saber o que ele significa. Os kinds embutidos abaixo existem
#' porque são universais, não porque o núcleo os privilegia.
#'
#' `default` é obrigatório: sem ele o documento teria que carregar valor de
#' todo param de todo nó pra ser interpretável, e um nó recém-criado não teria
#' estado válido.
#' @export
tr_param <- function(kind, default, label = NULL, ...) {
  if (missing(default)) {
    rlang::abort("Param sem 'default'.", class = "tr_error_param_no_default")
  }
  structure(c(list(kind = kind, default = default, label = label), list(...)),
            class = "tr_param")
}

#' Param numérico (ponto flutuante).
#' @export
tr_param_num  <- function(default, min = NA, max = NA, step = NULL, label = NULL, unit = NA_character_)
  tr_param("number", default, label, min = min, max = max, step = step, unit = unit)

#' Param inteiro.
#' @export
tr_param_int  <- function(default, min = NA, max = NA, label = NULL)
  tr_param("integer", as.integer(default), label, min = min, max = max)

#' Param de texto livre.
#' @export
tr_param_text <- function(default = "", label = NULL) tr_param("text", default, label)
#' Param booleano.
#' @export
tr_param_bool <- function(default = FALSE, label = NULL) tr_param("boolean", isTRUE(default), label)
#' Param de escolha única entre `choices`.
#' @export
tr_param_enum <- function(default, choices, label = NULL) tr_param("enum", default, label, choices = choices)

#' Param de tema de gráfico.
#'
#' O valor é `"padrão"` ou o nome de um tema do projeto; o `fn` recebe a
#' DEFINIÇÃO resolvida (ver `.tr_theme_resolve`), nunca o nome — é a definição
#' que entra na chave de cache, então editar um tema invalida só quem o usa.
#' @export
tr_param_theme <- function(default = "padr\u00e3o", label = "Tema") tr_param("theme", default, label)

#' Valida o valor de um param contra o `kind` declarado.
#'
#' Params chegam de JSON de um cliente e vão DIRETO pro argumento do `fn` do
#' nó. Sem checar aqui, um `tr_param_int` aceita `"abc"` ou uma lista e o erro
#' só aparece dentro da função do domínio — longe da causa, e como falha de
#' execução em vez de op recusada. "Quem decide é sempre o servidor" só é
#' verdade se o servidor de fato decidir.
#'
#' Kinds desconhecidos passam direto: uma coleção pode declarar `kind` próprio
#' (com widget próprio no front), e o núcleo não tem como saber o que é válido.
#' @noRd
.tr_check_param_value <- function(pspec, value, name) {
  bad <- function(esperado) {
    rlang::abort(sprintf("Param '%s' espera %s.", name, esperado), class = "tr_error_bad_param_value")
  }
  scalar <- function(x) length(x) == 1L && !is.null(x) && !is.list(x) && !is.na(x)

  switch(pspec$kind,
    number = {
      if (!scalar(value) || !is.numeric(value)) bad(.tr_msg("param.expect_number"))
      value <- as.numeric(value)
      if (!is.na(pspec$min) && value < pspec$min) bad(.tr_msg("param.expect_number_min", pspec$min))
      if (!is.na(pspec$max) && value > pspec$max) bad(.tr_msg("param.expect_number_max", pspec$max))
    },
    integer = {
      if (!scalar(value) || !is.numeric(value)) bad(.tr_msg("param.expect_integer"))
      value <- as.integer(value)
      if (!is.na(pspec$min) && value < pspec$min) bad(.tr_msg("param.expect_integer_min", pspec$min))
      if (!is.na(pspec$max) && value > pspec$max) bad(.tr_msg("param.expect_integer_max", pspec$max))
    },
    boolean = {
      if (!scalar(value) || !is.logical(value)) bad(.tr_msg("param.expect_boolean"))
    },
    text = {
      if (!scalar(value) || !is.character(value)) bad(.tr_msg("param.expect_text"))
    },
    # Só a forma: o documento não conhece o projeto, e nome que não existe
    # cai no padrão na resolução (`.tr_theme_resolve`) em vez de travar a op.
    theme = {
      if (!scalar(value) || !is.character(value) || !nzchar(value)) bad(.tr_msg("param.expect_theme"))
    },
    enum = {
      if (!scalar(value) || !as.character(value) %in% pspec$choices) {
        bad(.tr_msg("param.expect_enum", paste(pspec$choices, collapse = ", ")))
      }
      value <- as.character(value)
    },
    value
  )
  value
}
