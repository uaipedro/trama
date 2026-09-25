#' Templates: um trecho de flow empacotado pra ser colado em outro canvas.
#'
#' É um envelope sobre o documento, e não um formato novo: `doc` é um
#' `document-v1` comum. A marca `trama = "template"` é o que separa template de
#' JSON de dados — sem ela, um `.json` solto no canvas continua sendo fonte de
#' dados.
#' @noRd
.tr_template_versao <- 1L

#' Envelope de template a partir de um documento.
#'
#' Só a estrutura viaja: a posição é normalizada pro canto (0,0) e `colecoes`
#' lista os pacotes que quem cola precisa ter.
#' @export
tr_template <- function(doc, nome, descricao = "", registry = .tr_default_registry) {
  doc <- .tr_template_normalize(.tr_template_strip_data(.tr_as_doc(doc), registry))
  pkgs <- unlist(lapply(names(doc$collections), function(cid) registry$collections[[cid]]$package))
  structure(list(trama = "template", versao = .tr_template_versao, nome = nome,
                 descricao = descricao, colecoes = unique(as.character(pkgs)), doc = doc),
            class = "tr_template")
}

#' Template como JSON (envelope + documento canônico).
#' @export
tr_template_json <- function(tpl) {
  doc_json <- tr_doc_json(tpl$doc)
  env <- list(trama = "template", versao = tpl$versao %||% .tr_template_versao, nome = tpl$nome,
              descricao = tpl$descricao %||% "",
              colecoes = I(as.character(tpl$colecoes %||% character())))
  head <- jsonlite::toJSON(env, auto_unbox = TRUE, pretty = TRUE)
  # O doc já sai canônico de `tr_doc_json` (mapas vazios como `{}`); enxertá-lo
  # como texto evita reconverter e reintroduzir `[]`.
  out <- sub("\n}$", paste0(",\n  \"doc\": ", gsub("\n", "\n  ", doc_json), "\n}"), head)
  structure(out, class = "json")
}

#' O texto é um template do trama? Nunca falha: JSON inválido é só "não".
#' @export
tr_is_template <- function(txt) {
  x <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  is.list(x) && !is.null(names(x)) && identical(x$trama, "template")
}

#' Lê um template a partir de texto JSON.
#' @export
tr_template_parse <- function(txt) {
  x <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(x) || is.null(names(x)) || !identical(x$trama, "template")) {
    rlang::abort("Este JSON não é um template do trama.", class = "tr_error_not_template")
  }
  if (!identical(as.integer(x$versao %||% NA_integer_), .tr_template_versao)) {
    rlang::abort(sprintf("Versão de template não suportada: %s.", x$versao %||% "ausente"),
                 class = "tr_error_bad_format")
  }
  # Reserializa o `doc` pra passar pelo mesmo parse de qualquer documento: é lá
  # que mapas vazios e vetores achatados são reconstituídos.
  doc <- tr_doc_parse(jsonlite::toJSON(x$doc, auto_unbox = TRUE, null = "null", digits = NA))
  structure(list(trama = "template", versao = .tr_template_versao, nome = x$nome %||% "Template",
                 descricao = x$descricao %||% "", colecoes = as.character(unlist(x$colecoes)),
                 doc = doc), class = "tr_template")
}

#' Lê um template de um arquivo `.json`.
#' @export
tr_template_read <- function(path) tr_template_parse(paste(readLines(path, warn = FALSE), collapse = "\n"))

# Canto superior esquerdo do grupo (nós, frames, notas) vira (0,0): quem cola
# decide a posição, e o template não carrega o lugar onde nasceu.
.tr_template_normalize <- function(doc) {
  xs <- c(vapply(doc$ui$positions, function(p) as.numeric(p[[1]]), numeric(1)),
          vapply(doc$ui$frames, function(f) as.numeric(f$x), numeric(1)),
          vapply(doc$ui$notes, function(n) as.numeric(n$x), numeric(1)))
  ys <- c(vapply(doc$ui$positions, function(p) as.numeric(p[[2]]), numeric(1)),
          vapply(doc$ui$frames, function(f) as.numeric(f$y), numeric(1)),
          vapply(doc$ui$notes, function(n) as.numeric(n$y), numeric(1)))
  if (!length(xs)) return(doc)
  dx <- min(xs); dy <- min(ys)
  doc$ui$positions <- .tr_empty_obj(lapply(doc$ui$positions, function(p) c(p[[1]] - dx, p[[2]] - dy)))
  doc$ui$frames <- .tr_empty_obj(lapply(doc$ui$frames, function(f) { f$x <- f$x - dx; f$y <- f$y - dy; f }))
  doc$ui$notes <- .tr_empty_obj(lapply(doc$ui$notes, function(n) { n$x <- n$x - dx; n$y <- n$y - dy; n }))
  doc
}

# Só estrutura: param de kind `path` aponta pra arquivo de quem exportou, que
# quem cola não tem. Volta ao default, e o nó aparece como "falta dado" pelo
# mesmo caminho de qualquer leitor sem arquivo. Nó de tipo fora do registro
# passa intocado: sem a spec não há como saber qual param é caminho.
.tr_template_strip_data <- function(doc, registry) {
  for (id in names(doc$nodes)) {
    spec <- registry$nodes[[doc$nodes[[id]]$type]]
    if (is.null(spec)) next
    for (p in names(doc$nodes[[id]]$params)) {
      ps <- spec$params[[p]]
      if (!is.null(ps) && identical(ps$kind, "path")) doc$nodes[[id]]$params[[p]] <- ps$default
    }
  }
  doc
}
