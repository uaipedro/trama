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
#' @examples
#' tr_param("number", default = 1, label = "Valor")
#'
#' # O param que o demo/soma declara, do registro.
#' reg <- tr_registry()
#' tr_use("trama", registry = reg)
#' tr_get_node("demo/soma", reg)$params$k$default
#' @export
tr_param <- function(kind, default, label = NULL, ...) {
  if (missing(default)) {
    rlang::abort("Param sem 'default'.", class = "tr_error_param_no_default")
  }
  structure(c(list(kind = kind, default = default, label = label), list(...)),
            class = "tr_param")
}

#' Param numérico (ponto flutuante).
#' @examples
#' escala <- tr_param_num(1, min = 0, max = 10, step = 0.5, label = "Escala")
#' no <- tr_node("exemplo/escalar", fn = function(x, escala) x * escala,
#'               description = "Multiplica a entrada pela escala.",
#'               inputs = list(x = "demo/num"),
#'               outputs = list(out = "demo/num"),
#'               params = list(escala = escala))
#' no$fn(4, no$params$escala$default)
#' @export
tr_param_num  <- function(default, min = NA, max = NA, step = NULL, label = NULL, unit = NA_character_)
  tr_param("number", default, label, min = min, max = max, step = step, unit = unit)

#' Param inteiro.
#' @examples
#' n <- tr_param_int(3, min = 1, max = 10, label = "Linhas")
#' no <- tr_node("exemplo/primeiras",
#'               fn = function(data, n) utils::head(data, n),
#'               description = "Primeiras linhas da tabela.",
#'               inputs = list(data = "demo/tabela"),
#'               outputs = list(out = "demo/tabela"),
#'               params = list(n = n))
#'
#' reg <- tr_registry()
#' tr_use("trama", registry = reg)
#' no$fn(tr_fn("demo/tabela", reg)(), n$default)
#' @export
tr_param_int  <- function(default, min = NA, max = NA, label = NULL)
  tr_param("integer", as.integer(default), label, min = min, max = max)

#' Param de texto livre.
#' @examples
#' titulo <- tr_param_text("sem titulo", label = "Titulo")
#' no <- tr_node("exemplo/rotular",
#'               fn = function(data, titulo) cbind(titulo = titulo, data),
#'               description = "Acrescenta uma coluna com o titulo.",
#'               inputs = list(data = "demo/tabela"),
#'               outputs = list(out = "demo/tabela"),
#'               params = list(titulo = titulo))
#'
#' reg <- tr_registry()
#' tr_use("trama", registry = reg)
#' names(no$fn(tr_fn("demo/tabela", reg)(), "figuras"))
#' @export
tr_param_text <- function(default = "", label = NULL) tr_param("text", default, label)
#' Param booleano.
#' @examples
#' decrescente <- tr_param_bool(TRUE, label = "Decrescente")
#' no <- tr_node("exemplo/ordenar",
#'               fn = function(data, decrescente) {
#'                 ordem <- order(data$area, decreasing = decrescente)
#'                 data[ordem, , drop = FALSE]
#'               },
#'               description = "Ordena a tabela pela area.",
#'               inputs = list(data = "demo/tabela"),
#'               outputs = list(out = "demo/tabela"),
#'               params = list(decrescente = decrescente))
#'
#' reg <- tr_registry()
#' tr_use("trama", registry = reg)
#' no$fn(tr_fn("demo/tabela", reg)(), TRUE)$area
#' @export
tr_param_bool <- function(default = FALSE, label = NULL) tr_param("boolean", isTRUE(default), label)
#' Param de escolha única entre `choices`.
#' @examples
#' coluna <- tr_param_enum("area", choices = c("lados", "area"),
#'                         label = "Coluna")
#' coluna$choices
#'
#' # O enum do demo/filtrar: valor fora de `choices` e recusado na op.
#' reg <- tr_registry()
#' tr_use("trama", registry = reg)
#' tr_get_node("demo/filtrar", reg)$params$column$choices
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
