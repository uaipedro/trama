# O valor de um nó do projeto, fora do editor.
#
# Usa só o que o núcleo já exporta: `tr_project()` abre o projeto com as
# coleções do `trama.json`, `tr_project_flow()` lê (e migra) o fluxo,
# `tr_value()` devolve o valor (do cache, ou calculando) e `tr_plan()` dá a
# chave do store sem calcular nada, para saber se uma região ficou para trás.
# Tudo roda na raiz do projeto, como no editor: quem lê arquivo resolve o
# caminho relativo a ela, e a chave depende disso.

#' Projeto a partir de um `tr_project`, de uma pasta ou do caminho do documento.
#' @noRd
.td_projeto <- function(projeto = NULL, documento = NULL) {
  if (inherits(projeto, "tr_project")) return(projeto)
  raiz <- projeto %||% .td_raiz_de(documento)
  if (!file.exists(file.path(raiz, "trama.json"))) {
    .td_erro(sprintf("'%s' n\u{E3}o \u{E9} um projeto trama (falta o trama.json).", raiz), "tr_documento_erro_projeto")
  }
  trama::tr_project(raiz, create = FALSE)
}

#' Raiz do projeto de um documento: a primeira pasta acima com `trama.json`.
#' @noRd
.td_raiz_de <- function(documento) {
  if (is.null(documento)) .td_erro("Diga o projeto ou o documento.", "tr_documento_erro_projeto")
  d <- dirname(normalizePath(documento, mustWork = TRUE))
  repeat {
    if (file.exists(file.path(d, "trama.json"))) return(d)
    acima <- dirname(d)
    if (identical(acima, d)) {
      .td_erro(sprintf("O documento '%s' n\u{E3}o est\u{E1} dentro de um projeto trama.", documento),
               "tr_documento_erro_projeto")
    }
    d <- acima
  }
}

.td_na_raiz <- function(projeto, expr) {
  antes <- setwd(projeto$root)
  on.exit(setwd(antes))
  expr
}

#' A saída de um nó: porta, tipo e rótulo; `NULL` se o nó ou a porta sumiram.
#' @noRd
.td_saida <- function(projeto, doc, no, porta = NULL) {
  n <- doc$nodes[[no]]
  if (is.null(n)) return(NULL)
  spec <- projeto$registry$nodes[[n$type]]
  if (is.null(spec) || !length(spec$outputs)) return(NULL)
  porta <- porta %||% names(spec$outputs)[[1]]
  saida <- spec$outputs[[porta]]
  if (is.null(saida)) return(NULL)
  tipo <- tryCatch(trama::tr_get_type(saida$type, projeto$registry), error = function(e) NULL)
  rotulo <- n$label
  list(porta = porta, padrao = identical(porta, names(spec$outputs)[[1]]), tipo = tipo,
       titulo = if (is.character(rotulo) && length(rotulo) == 1L && nzchar(rotulo)) rotulo
                else spec$label %||% no)
}

#' O valor (calcula o que faltar, como `tr_value()` no console).
#' @noRd
.td_valor <- function(projeto, doc, no, porta) {
  .td_na_raiz(projeto, trama::tr_value(doc, no, registry = projeto$registry, store = projeto$store,
                                       port = porta, settings = projeto$settings))
}

#' Chave do store da saída, sem calcular: muda quando muda o que o nó lê,
#' os parâmetros ou a versão do bloco. Serve só para dizer "pode ter mudado";
#' quem confirma é o hash do que se materializa.
#' @noRd
.td_chaves <- function(projeto, doc) {
  plano <- tryCatch(.td_na_raiz(projeto, trama::tr_plan(doc, registry = projeto$registry, store = projeto$store,
                                                        settings = projeto$settings)),
                    error = function(e) NULL)
  if (is.null(plano)) return(list())
  lapply(plano$units, function(u) u$outputs)
}
