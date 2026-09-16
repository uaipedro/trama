#' Declara uma porta.
#'
#' `multiple = TRUE` (porta variádica) existe desde o início porque afeta o
#' schema da aresta (`index`): acrescentar depois seria mudança de formato de
#' documento, não de API. "Combinar N entradas" aparece em todo domínio.
#'
#' `stream = TRUE` marca porta de fluxo: o dado chega ponto a ponto. Ao
#' contrário de `multiple`, não guarda nada por aresta — logo não toca no
#' schema do documento.
#' @export
tr_port <- function(type, required = TRUE, multiple = FALSE, stream = FALSE) {
  .tr_check_id(type, "tipo de porta")
  structure(list(type = type, required = isTRUE(required), multiple = isTRUE(multiple),
                 stream = isTRUE(stream)),
            class = "tr_port")
}

.tr_as_port <- function(x) if (inherits(x, "tr_port")) x else tr_port(x)

#' Declara um node type.
#'
#' **Sem efeito colateral**: devolve uma especificação. Quem registra é
#' `tr_collection()` + `tr_use()`. No insumo, `wm_register_node()` escrevia no
#' registro E criava o wrapper de nível 1 em `globalenv()` no ato — o que
#' impedia o projeto de virar pacote e impedia dois registros isolados
#' coexistirem (todo teste compartilhava o mesmo estado global).
#'
#' `fn` é uma função R comum, usável direto no console — o nível 1 da API. Os
#' argumentos permitidos são os nomes de `inputs`, os de `params`, e os dois
#' reservados `.seed` e `.ctx` (com ponto, pra nunca colidirem com um param
#' que por acaso se chame `seed`).
#'
#' `description` é obrigatória e `help` é opcional: a primeira é a linha que
#' aparece na paleta e no tooltip do card, a segunda é a página que abre no
#' painel lateral. Obrigar a primeira é o que impede uma coleção de nascer
#' muda — e o catálogo é lido por máquina também, então a ausência custa
#' duas vezes.
#'
#' `pure = FALSE` exige `fingerprint(params)`: o que do mundo externo entra na
#' chave de cache (mtime+tamanho de arquivo, ETag de URL). Sem isso, um nó que
#' lê um CSV serviria dado velho em silêncio quando o arquivo muda, porque
#' nada no hash teria mudado. `volatile = TRUE` nunca cacheia entre execuções.
#' @export
tr_node <- function(id, fn, version = 1L, label = NULL, description,
                    help = NULL, category = NULL, inputs = list(), outputs = list(),
                    params = list(), pure = TRUE, fingerprint = NULL,
                    volatile = FALSE, stochastic = FALSE, icon = NULL) {
  .tr_check_id(id, "id de nó")

  # Um nó sem uma linha dizendo o que faz é um nó que ninguém vai saber
  # escolher na paleta — e o catálogo é lido por máquina também, então a
  # ausência custa duas vezes. Mesma régua de `tr_param()` sem `default` e
  # `tr_type()` com `store` sem `restore`: erro alto, na declaração.
  # `trimws(NA_character_)` devolve a string "NA" — que passa por `nzchar()`.
  # Sem o `is.na()` explícito o nó nasce com `description = NA`, e o catálogo
  # (`.tr_json_drop_empty()`) derruba o campo: o nó mudo que esta regra existe
  # pra impedir, só que sem erro nenhum.
  if (missing(description) || !is.character(description) || length(description) != 1L ||
      is.na(description) || !nzchar(trimws(description))) {
    rlang::abort(sprintf("Nó '%s' sem 'description'.", id), class = "tr_error_missing_description")
  }

  # `help` é renderizado como markdown no painel lateral: se um número passar,
  # ele viaja como número no JSON do catálogo e quebra do outro lado.
  if (!is.null(help) && (!is.character(help) || length(help) != 1L || is.na(help))) {
    rlang::abort(sprintf("'help' de '%s' não é uma string única.", id), class = "tr_error_bad_help")
  }

  # Um `icon` que não veio de `tr_icon()` chegaria ao catálogo como uma lista
  # qualquer e viraria erro no front, longe daqui.
  if (!is.null(icon) && !inherits(icon, "tr_icon")) {
    rlang::abort(sprintf("Nó '%s': 'icon' tem que vir de tr_icon().", id),
                 class = "tr_error_bad_icon")
  }

  if (!is.function(fn)) {
    rlang::abort(sprintf("'fn' de '%s' não é função.", id), class = "tr_error_fn_not_function")
  }

  inputs  <- lapply(inputs, .tr_as_port)
  outputs <- lapply(outputs, .tr_as_port)
  if (length(outputs) > 0 && is.null(names(outputs))) {
    rlang::abort(sprintf("Outputs de '%s' precisam ser nomeados.", id), class = "tr_error_unnamed_ports")
  }

  collide <- intersect(names(inputs), names(params))
  if (length(collide) > 0) {
    rlang::abort(
      sprintf("Em '%s', nome usado como input e param ao mesmo tempo: %s.", id, paste(collide, collapse = ", ")),
      class = "tr_error_name_collision"
    )
  }
  for (nm in names(params)) {
    if (!inherits(params[[nm]], "tr_param")) {
      rlang::abort(sprintf("Param '%s' de '%s' não veio de tr_param().", nm, id), class = "tr_error_bad_param")
    }
  }

  # Validação alto e cedo (herdada do insumo, e vale a pena): um argumento de
  # `fn` sem input/param correspondente é erro de declaração que, sem esta
  # checagem, só apareceria como "argumento ausente, sem padrão" no primeiro
  # cache miss — com o nó preso em "computando…" e a mensagem escondida.
  unknown <- setdiff(names(formals(fn)), c(names(inputs), names(params), ".seed", ".ctx"))
  if (length(unknown) > 0) {
    rlang::abort(
      sprintf("Argumento(s) de 'fn' de '%s' sem input/param correspondente: %s.",
              id, paste(unknown, collapse = ", ")),
      class = "tr_error_unknown_fn_arg"
    )
  }
  if (!isTRUE(pure) && is.null(fingerprint)) {
    rlang::abort(
      sprintf("Nó '%s' é impuro mas não declara 'fingerprint'.", id),
      class = "tr_error_missing_fingerprint"
    )
  }

  structure(list(
    id = id, fn = fn, version = as.integer(version), label = label %||% id,
    description = description, help = help, category = category,
    inputs = inputs, outputs = outputs, params = params,
    pure = isTRUE(pure), fingerprint = fingerprint,
    volatile = isTRUE(volatile), stochastic = isTRUE(stochastic), icon = icon
  ), class = "tr_node")
}
