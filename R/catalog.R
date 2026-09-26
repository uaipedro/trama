#' Monta o catálogo — o JSON que o front consome.
#'
#' O front não conhece nenhum tipo de nó: portas, cores, categorias e widgets
#' de param saem todos daqui. Essa foi a propriedade do insumo que mais se
#' provou (sobreviveu a três versões de domínio, a subgrafos e a uma paleta de
#' arrastar sem o front ganhar uma linha de domínio), e é a única coisa
#' herdada sem repensar.
#'
#' `adapters` viaja como pares pro front poder responder "essa aresta pode?"
#' sozinho, sem round-trip — a mesma resposta que `tr_compatible()` dá no
#' servidor, que continua sendo quem valida de verdade.

#' Ausência tem que ser UMA coisa no JSON.
#'
#' `NULL` num list vira `{}` e `NA` vira `null` — então um param sem `label`
#' chegava ao front como `label: {}` e um `max` não declarado como `null`,
#' duas representações da mesma ausência. Aqui ausência some do objeto, e o
#' front testa presença de campo em vez de adivinhar a forma.
#' @noRd
.tr_json_drop_empty <- function(x) {
  keep <- !vapply(x, function(v) is.null(v) || (length(v) == 1L && !is.list(v) && is.na(v)), logical(1))
  x[keep]
}

#' `auto_unbox` colapsa vetor de 1 elemento em escalar — o que é o que
#' queremos pra `default`, e o que NÃO queremos pra `choices`: um enum de uma
#' opção só chegaria como string e o widget iteraria sobre os caracteres dela.
#' `I()` força array em qualquer comprimento.
#' @noRd
.tr_json_param <- function(nm, p) {
  out <- .tr_json_drop_empty(c(list(name = nm), unclass(p)))
  if (!is.null(out$choices)) out$choices <- I(as.character(out$choices))
  # Cada valor de `when` sai array SEMPRE: `metodo = "holm"` sozinho viraria
  # string e o front testaria pertinência em letras.
  if (!is.null(out$when)) out$when <- lapply(out$when, I)
  out
}

#' Textos saem JÁ resolvidos no idioma da sessão: o front não precisa saber
#' de i18n agora, e o objeto cru continua no nó para o exportador do site.
#' `verificar` e `autores` são arrays sempre (`I()`), pela mesma razão de
#' `choices`: um único id viraria string e o front iteraria sobre letras.
#' @noRd
.tr_json_pressuposto <- function(p) {
  .tr_json_drop_empty(list(
    texto = tr_text(p$texto),
    verificar = if (length(p$verificar)) I(p$verificar),
    se_falhar = if (!is.null(p$se_falhar)) tr_text(p$se_falhar)
  ))
}

#' A versão do pacote de uma referência de implementação é lida AQUI, na
#' montagem, e não na declaração: a coleção é escrita uma vez, mas o que roda
#' é o pacote instalado agora. Pacote ausente não é erro — o bloco pode nem
#' ser usado nesta sessão —, só fica sem versão.
#' @noRd
.tr_json_ref <- function(r, versao_de = .tr_pkg_version) {
  txt <- function(x) if (!is.null(x)) tr_text(x)
  versao <- if (!is.null(r$pacote)) versao_de(r$pacote)
  .tr_json_drop_empty(list(
    papel = r$papel, autores = if (length(r$autores)) I(r$autores), ano = r$ano,
    titulo = txt(r$titulo), fonte = txt(r$fonte), doi = r$doi, url = r$url,
    pacote = r$pacote, funcao = r$funcao, versao = versao, nota = txt(r$nota)
  ))
}

#' Versão instalada de um pacote, ou `NULL` se ele não estiver instalado.
#' @noRd
.tr_pkg_version <- function(pacote) {
  tryCatch(as.character(utils::packageVersion(pacote)), error = function(e) NULL)
}

#' Memoiza `.tr_pkg_version()` dentro de UMA montagem de catálogo: vários
#' blocos citam o mesmo pacote, e `packageVersion()` lê o DESCRIPTION do disco.
#' A memória morre com a chamada — pacote atualizado aparece no catálogo seguinte.
#' @noRd
.tr_pkg_version_memo <- function() {
  memo <- new.env(parent = emptyenv())
  function(pacote) {
    if (!exists(pacote, envir = memo, inherits = FALSE)) {
      assign(pacote, list(.tr_pkg_version(pacote)), envir = memo)
    }
    get(pacote, envir = memo, inherits = FALSE)[[1L]]
  }
}

#' Catálogo do registro em forma serializável: tipos, categorias, nós e adaptadores — o que o front usa pra desenhar a paleta e validar conexões sem round-trip.
#' @export
tr_catalog <- function(registry = .tr_default_registry) {
  versao_de <- .tr_pkg_version_memo()
  port_json <- function(nm, p) {
    out <- list(name = nm, type = p$type, required = p$required)
    if (isTRUE(p$multiple)) out$multiple <- TRUE
    if (isTRUE(p$stream)) out$stream <- TRUE
    out
  }
  # Omitido quando vazio, pela mesma regra de `.tr_json_drop_empty()`: o front
  # testa presença do campo.
  tr <- do.call(rbind, unname(registry$transitions))
  transitions <- if (NROW(tr)) unname(lapply(seq_len(nrow(tr)), function(i) {
    list(from = tr$from[[i]], to = tr$to[[i]], n = tr$n[[i]])
  }))
  out <- list(
    schema_version = 1L,
    collections = unname(lapply(registry$collections, .tr_json_drop_empty)),
    types = unname(lapply(registry$types, function(t) {
      list(id = t$id, label = t$label, color = t$color, version = t$version)
    })),
    categories = unname(lapply(registry$categories, function(k) k)),
    adapters = unname(lapply(registry$adapters, function(a) list(from = a$from, to = a$to))),
    nodes = unname(lapply(registry$nodes, function(n) .tr_json_drop_empty(list(
      id = n$id, label = n$label, category = n$category, role = n$role, version = n$version,
      description = n$description, help = n$help, stochastic = n$stochastic,
      online = if (isTRUE(n$online)) TRUE else NULL,
      icon = if (is.null(n$icon)) NULL else unclass(n$icon),
      inputs  = unname(Map(port_json, names(n$inputs),  n$inputs)),
      outputs = unname(Map(port_json, names(n$outputs), n$outputs)),
      params  = unname(Map(.tr_json_param, names(n$params), n$params)),
      pressupostos = unname(lapply(n$pressupostos %||% list(), .tr_json_pressuposto)),
      referencias  = unname(lapply(n$referencias %||% list(), .tr_json_ref, versao_de = versao_de))
    ))))
  )
  if (length(transitions)) out$transitions <- transitions
  out
}

#' `tr_catalog()` como JSON.
#' @export
tr_catalog_json <- function(registry = .tr_default_registry) {
  jsonlite::toJSON(tr_catalog(registry), auto_unbox = TRUE, null = "null", digits = NA)
}
