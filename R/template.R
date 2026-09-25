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

#' Recorte do documento com os nós/frames/notas de `ids` e só as arestas internas.
#'
#' Aresta que sai do grupo é descartada, e não pendurada: no destino ela
#' apontaria pra um nó que não existe. `ids = NULL` devolve o documento todo.
#' @export
tr_doc_subset <- function(doc, ids = NULL) {
  doc <- .tr_as_doc(doc)
  if (is.null(ids)) return(doc)
  keep <- function(m) .tr_empty_obj(m[intersect(names(m), ids)])
  doc$nodes <- keep(doc$nodes)
  doc$edges <- Filter(function(e) e$from$node %in% ids && e$to$node %in% ids, doc$edges)
  for (k in c("positions", "sizes", "views", "modes", "frames", "notes")) doc$ui[[k]] <- keep(doc$ui[[k]])
  # O manifesto encolhe junto: o template não deve exigir coleção que nenhum
  # nó recortado usa.
  usadas <- unique(vapply(doc$nodes, function(n) .tr_collection_of(n$type), ""))
  doc$collections <- .tr_empty_obj(doc$collections[intersect(names(doc$collections), usadas)])
  doc
}

#' Uma op `batch` que insere o template com o canto em `origin`, ids novos.
#'
#' Ids novos a cada chamada: colar o mesmo template duas vezes não pode
#' colidir. Uma op só (e não várias) pra inserção ser um passo de undo.
#' Frames vêm antes dos nós (ficam por baixo); tamanho, vista, modo e arestas
#' vêm depois, porque exigem o nó já criado.
#' @export
tr_template_op <- function(tpl, origin = c(0, 0)) {
  d <- tpl$doc
  novo <- list()
  for (id in c(names(d$nodes), names(d$ui$frames), names(d$ui$notes))) novo[[id]] <- .tr_new_id()
  at <- function(x, y) c(round(origin[[1]] + x), round(origin[[2]] + y))
  ops <- list(); depois <- list()
  for (id in names(d$ui$frames)) {
    f <- d$ui$frames[[id]]; p <- at(f$x, f$y)
    # Sem `order`: o frame colado entra no fim da sequência de slides do destino.
    ops[[length(ops) + 1]] <- list(op = "add_frame", id = novo[[id]], x = p[1], y = p[2], w = f$w, h = f$h,
                                   title = f$title, aspect = f$aspect, color = f$color)
  }
  for (id in names(d$nodes)) {
    n <- d$nodes[[id]]; pos <- d$ui$positions[[id]] %||% c(0, 0)
    ops[[length(ops) + 1]] <- list(op = "add_node", id = novo[[id]], type = n$type, label = n$label,
                                   params = .tr_empty_obj(n$params), seed = n$seed,
                                   position = at(pos[[1]], pos[[2]]))
    # `sizes` guarda o vetor c(w, h), e `resize` o recebe em `w`/`h`.
    sz <- d$ui$sizes[[id]]
    if (!is.null(sz)) depois[[length(depois) + 1]] <- list(op = "resize", node = novo[[id]], w = sz[[1]], h = sz[[2]])
    if (!is.null(d$ui$views[[id]])) depois[[length(depois) + 1]] <-
      list(op = "set_view", node = novo[[id]], view = d$ui$views[[id]])
    if (!is.null(d$ui$modes[[id]])) depois[[length(depois) + 1]] <-
      list(op = "set_mode", node = novo[[id]], modo = d$ui$modes[[id]])
  }
  for (id in names(d$ui$notes)) {
    n <- d$ui$notes[[id]]; p <- at(n$x, n$y)
    ops[[length(ops) + 1]] <- c(list(op = "add_note", id = novo[[id]], x = p[1], y = p[2]),
                                n[setdiff(names(n), c("x", "y"))])
  }
  for (e in d$edges) depois[[length(depois) + 1]] <-
    list(op = "connect", from_node = novo[[e$from$node]], from_port = e$from$port,
         to_node = novo[[e$to$node]], to_port = e$to$port)
  list(op = "batch", ops = c(ops, depois))
}

#' Pasta de templates por escopo.
#'
#' `biblioteca` é pessoal (vale em qualquer projeto); `projeto` viaja junto
#' com a pasta do projeto. Templates de coleção moram em `inst/templates` do
#' pacote e não têm pasta gravável: só são listados.
#' @export
tr_template_dir <- function(escopo = c("biblioteca", "projeto"), root = ".") {
  switch(match.arg(escopo),
         biblioteca = file.path(tools::R_user_dir("trama", "config"), "templates"),
         projeto = file.path(root, "templates"))
}

# Nome de arquivo a partir do nome do template: sem acento, sem espaço, sem
# nada que um sistema de arquivos estranhe.
.tr_slug <- function(x) {
  s <- tolower(iconv(x, to = "ASCII//TRANSLIT", sub = ""))
  s <- gsub("[^a-z0-9]+", "-", s); s <- gsub("^-|-$", "", s)
  if (is.na(s) || !nzchar(s)) "template" else s
}

#' Grava o template em `dir`; nome de arquivo derivado do nome do template.
#' @export
tr_template_save <- function(tpl, dir, overwrite = FALSE) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0(.tr_slug(tpl$nome), ".json"))
  # Dois templates de nomes parecidos caem no mesmo slug: sobrescrever calado
  # apagaria o trabalho de alguém.
  if (file.exists(path) && !overwrite) {
    rlang::abort(sprintf("Já existe um template chamado '%s' aqui.", tpl$nome),
                 class = "tr_error_template_exists")
  }
  writeLines(tr_template_json(tpl), path)
  invisible(path)
}

#' Templates disponíveis: coleções carregadas, biblioteca pessoal, projeto.
#'
#' Uma lista de registros (`escopo`, `nome`, `descricao`, `arquivo`,
#' `colecoes`). Arquivo que não é template — dado solto, JSON quebrado — é
#' ignorado: uma pasta suja não pode esconder os templates bons.
#' @export
tr_template_list <- function(root = ".", registry = .tr_default_registry) {
  pkgs <- as.character(.tr_registry_packages(registry) %||% character())
  pastas <- c(vapply(pkgs, function(p) system.file("templates", package = p), ""),
              tr_template_dir("biblioteca"), tr_template_dir("projeto", root))
  escopos <- c(rep("colecao", length(pkgs)), "biblioteca", "projeto")
  out <- list()
  for (i in seq_along(pastas)) {
    d <- pastas[[i]]
    if (!nzchar(d) || !dir.exists(d)) next
    for (f in sort(list.files(d, "\\.json$", full.names = TRUE))) {
      t <- tryCatch(tr_template_read(f), error = function(e) NULL)
      if (is.null(t)) next
      out[[length(out) + 1]] <- list(escopo = escopos[[i]], nome = t$nome,
                                     descricao = t$descricao, arquivo = normalizePath(f),
                                     colecoes = t$colecoes)
    }
  }
  out
}
