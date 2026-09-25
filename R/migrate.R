#' Migração de params por versão do nó.
#'
#' Renomear um param (ou trocar sua unidade) sobe a versão do nó, e sem isto
#' todo flow salvo antes da troca quebrava: `tr_doc_validate()` acusava
#' `unknown_param`, e colar um template velho abortava no `add_node`. A coleção
#' declara, em `tr_node(migracoes = )`, uma função por versão de DESTINO —
#' `list(`3` = function(params) ...)` leva os params da v2 para a v3 — e o
#' núcleo aplica em ordem, da `type_version` do documento + 1 até a versão da
#' spec. O núcleo não sabe o que os params significam: só os entrega à função.
#'
#' Regras, todas pra que migrar seja determinístico e aplicado uma vez só:
#'
#' * migra só quem tem ALGUMA migração no intervalo `(de, para]`; versão sem
#'   função no meio da cadeia é identidade (a v4 que só ganhou param com
#'   default não precisa declarar nada). Sem nenhuma, nada muda e a validação
#'   continua acusando `version_drift`, como antes;
#' * nunca migra para trás (documento de versão mais nova que a spec);
#' * ao fim, `type_version` vira a versão da spec — rodar de novo é no-op.
#'
#' Chave de cache: o hash lê os params e a versão da SPEC, não a do documento
#' (`plan.R`/`store.R`). Um documento migrado tem, portanto, a mesma chave que
#' um criado já na versão nova com os mesmos valores; o resultado antigo (de
#' antes do renome) não é reaproveitado porque a versão da spec já mudou a
#' chave quando o nó subiu de versão — migrar não muda isso.
#' @name migracoes
#' @noRd
NULL

#' Valida `migracoes` na declaração do nó: erro alto, onde a coleção a escreveu.
#' @noRd
.tr_check_migracoes <- function(migracoes, version, id) {
  ruim <- function(msg) rlang::abort(sprintf("Nó '%s': %s", id, msg), class = "tr_error_bad_migration")
  if (!is.list(migracoes)) ruim("'migracoes' tem que ser uma list().")
  if (!length(migracoes)) return(list())
  nms <- names(migracoes)
  if (is.null(nms) || any(!nzchar(nms))) ruim("toda migração é nomeada pela versão de destino.")
  alvo <- suppressWarnings(as.integer(nms))
  if (anyNA(alvo) || any(as.character(alvo) != nms) || any(alvo < 2L) || any(alvo > version)) {
    ruim(sprintf("versão de destino fora de 2..%d: %s.", version, paste(nms, collapse = ", ")))
  }
  if (anyDuplicated(alvo)) ruim("versão de destino repetida em 'migracoes'.")
  for (nm in nms) {
    if (!is.function(migracoes[[nm]])) ruim(sprintf("a migração '%s' não é função.", nm))
  }
  migracoes
}

#' Leva `params` da versão `de` até a versão da spec. `NULL` quando não há o
#' que migrar (sem caminho, já atual, ou documento mais novo).
#' @noRd
.tr_migrate_params <- function(spec, params, de, node = NULL) {
  para <- spec$version
  de <- as.integer(de %||% para)
  migs <- spec$migracoes %||% list()
  if (is.na(de) || de >= para) return(NULL)
  passos <- as.character(seq.int(de + 1L, para))
  passos <- passos[passos %in% names(migs)]
  if (!length(passos)) return(NULL)
  quem <- if (is.null(node)) sprintf("'%s'", spec$id) else sprintf("'%s' (%s)", node, spec$id)
  params <- params %||% list()
  for (v in passos) {
    params <- tryCatch(migs[[v]](params), error = function(e) {
      rlang::abort(sprintf("Migração do nó %s para v%s falhou: %s", quem, v, conditionMessage(e)),
                   class = "tr_error_migration", node = node, parent = e)
    })
    if (!is.list(params) || (length(params) && (is.null(names(params)) || any(!nzchar(names(params)))))) {
      rlang::abort(sprintf("Migração do nó %s para v%s não devolveu uma lista nomeada de params.", quem, v),
                   class = "tr_error_migration", node = node)
    }
  }
  params
}

#' Aplica as migrações de params declaradas pelos nós a um documento.
#'
#' Devolve o documento com cada nó migrável na versão da spec e o registro do
#' que mudou em `attr(doc, "migracoes")` (lista de `node`, `type`, `from`,
#' `to`), pra quem abre mostrar "migrado de v2 para v3". Idempotente. Nó de
#' tipo fora do registro, ou sem caminho de migração, passa intocado.
#' @export
tr_doc_migrate <- function(doc, registry = .tr_default_registry) {
  doc <- .tr_as_doc(doc)
  log <- list()
  for (id in names(doc$nodes)) {
    n <- doc$nodes[[id]]
    spec <- registry$nodes[[n$type]]
    if (is.null(spec)) next
    novos <- .tr_migrate_params(spec, n$params, n$type_version, node = id)
    if (is.null(novos)) next
    log[[length(log) + 1]] <- list(node = id, type = n$type,
                                   from = as.integer(n$type_version), to = spec$version)
    doc$nodes[[id]]$params <- novos
    doc$nodes[[id]]$type_version <- spec$version
  }
  attr(doc, "migracoes") <- log
  doc
}

#' As linhas que o usuário lê. Vazio quando nada migrou.
#' @noRd
.tr_migracoes_texto <- function(log) {
  vapply(log, function(m) sprintf("Nó '%s' (%s) migrado de v%d para v%d.",
                                  m$node, m$type, m$from, m$to), "")
}
