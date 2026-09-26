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

#' Mostra o param só sob condição sobre outros params do mesmo nó.
#'
#' `tr_when(tr_param_num(0.05), metodo = c("holm", "bonferroni"))` só exibe o
#' campo quando `metodo` vale um dos valores dados. Várias condições valem
#' juntas (E). Os valores podem ser texto, lógico ou número.
#'
#' É puramente de interface: param escondido guarda o valor e continua indo
#' para `fn`. Que cada nome citado seja outro param do nó é conferido em
#' [tr_node()] (`tr_error_bad_when`), porque o param sozinho não conhece os
#' irmãos.
#' @param p Um param de [tr_param()].
#' @param ... Condições nomeadas: `<param> = <valores permitidos>`.
#' @return `p` com `when` preenchido.
#' @export
tr_when <- function(p, ...) {
  if (!inherits(p, "tr_param")) {
    rlang::abort("tr_when(): 'p' não veio de tr_param().", class = "tr_error_bad_when")
  }
  conds <- list(...)
  nms <- names(conds)
  if (!length(conds) || is.null(nms) || any(is.na(nms) | !nzchar(nms))) {
    rlang::abort("tr_when(): toda condição é nomeada pelo param que ela olha.",
                 class = "tr_error_bad_when")
  }
  if (anyDuplicated(nms)) {
    rlang::abort("tr_when(): param repetido nas condições.", class = "tr_error_bad_when")
  }
  for (nm in nms) {
    v <- conds[[nm]]
    if (!(is.character(v) || is.logical(v) || is.numeric(v)) || !length(v) || anyNA(v)) {
      rlang::abort(sprintf("tr_when(): valores de '%s' têm que ser texto, lógico ou número, sem NA.", nm),
                   class = "tr_error_bad_when")
    }
  }
  p$when <- conds
  p
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

#' Param de coluna(s) da tabela que chega por uma entrada.
#'
#' `role` diz que tipo de coluna serve; `multi = FALSE` é uma coluna só (select);
#' `from` nomeia a entrada (padrão: a primeira do nó). Continua `kind = "cols"`:
#' o valor é texto ("a" ou "a, b"), igual aos `cols` já existentes — quem já
#' declara `tr_param("cols", ...)` segue valendo, só sem as anotações que
#' deixam o front oferecer as colunas do `schema` da entrada.
#'
#' `role` inválido aborta com `tr_error_bad_param` e não com o erro cru de
#' `match.arg`: é erro de declaração da coleção, da mesma família dos outros
#' que `tr_node()` levanta.
#' @param default Valor inicial (texto).
#' @param label Rótulo do campo.
#' @param role `"numerica"`, `"categorica"`, `"tempo"` ou `"qualquer"`.
#' @param multi `TRUE` aceita várias colunas separadas por vírgula.
#' @param from Nome da entrada cujas colunas servem; `NULL` é a primeira.
#' @param example Exemplo mostrado no campo.
#' @export
tr_param_col <- function(default = "", label = NULL, role = "qualquer", multi = FALSE,
                         from = NULL, example = NULL) {
  papeis <- c("numerica", "categorica", "tempo", "qualquer")
  if (!is.character(role) || length(role) != 1L || !role %in% papeis) {
    rlang::abort(sprintf("tr_param_col(): 'role' deve ser um de: %s.", paste(papeis, collapse = ", ")),
                 class = "tr_error_bad_param")
  }
  if (!is.null(from) && (!is.character(from) || length(from) != 1L || is.na(from) || !nzchar(from))) {
    rlang::abort("tr_param_col(): 'from' deve ser o nome de uma entrada do nó.", class = "tr_error_bad_param")
  }
  tr_param("cols", default, label, role = role, multi = isTRUE(multi), from = from, example = example)
}

#' Param de tema de gráfico.
#'
#' O valor é `"padrão"` ou o nome de um tema do projeto; o `fn` recebe a
#' DEFINIÇÃO resolvida (ver `.tr_theme_resolve`), nunca o nome — é a definição
#' que entra na chave de cache, então editar um tema invalida só quem o usa.
#' @export
tr_param_theme <- function(default = "padrão", label = "Tema") tr_param("theme", default, label)

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
      if (!scalar(value) || !is.numeric(value)) bad("um número")
      value <- as.numeric(value)
      if (!is.na(pspec$min) && value < pspec$min) bad(sprintf("um número >= %s", pspec$min))
      if (!is.na(pspec$max) && value > pspec$max) bad(sprintf("um número <= %s", pspec$max))
    },
    integer = {
      if (!scalar(value) || !is.numeric(value)) bad("um inteiro")
      value <- as.integer(value)
      if (!is.na(pspec$min) && value < pspec$min) bad(sprintf("um inteiro >= %s", pspec$min))
      if (!is.na(pspec$max) && value > pspec$max) bad(sprintf("um inteiro <= %s", pspec$max))
    },
    boolean = {
      if (!scalar(value) || !is.logical(value)) bad("TRUE ou FALSE")
    },
    text = {
      if (!scalar(value) || !is.character(value)) bad("um texto")
    },
    # Só a forma: o documento não conhece o projeto, e nome que não existe
    # cai no padrão na resolução (`.tr_theme_resolve`) em vez de travar a op.
    theme = {
      if (!scalar(value) || !is.character(value) || !nzchar(value)) bad("o nome de um tema")
    },
    enum = {
      if (!scalar(value) || !as.character(value) %in% pspec$choices) {
        bad(sprintf("um de: %s", paste(pspec$choices, collapse = ", ")))
      }
      value <- as.character(value)
    },
    value
  )
  value
}
