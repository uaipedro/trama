#' Store endereçado por conteúdo — o protocolo, não um cache.
#'
#' É o que permite o worker ser outro processo sem serializar objeto vivo: o
#' worker computa, grava o artefato e o preview sob a chave, e devolve **só o
#' handle**. O coordenador nunca carrega um valor pesado.
#'
#' Layout:
#'   objects/<chave>.<ext>    o valor, gravado pelo `store` do TIPO
#'   handles/<chave>.json     metadados: tipo, tamanho, resumo, preview, erro
#'   previews/<chave>.<ext>   artefato de preview, quando tem arquivo
#'
#' **Escrita atômica** (temp + rename) não é refinamento: o contrato do
#' executor é que o worker pode morrer a qualquer momento (cancelar = matar o
#' daemon, porque cancelamento cooperativo em R não existe). Um arquivo
#' parcial sob uma chave válida seria indistinguível de um resultado bom, para
#' sempre — o cache serviria lixo silenciosamente.
#' @export
tr_store <- function(root, project_root = NULL) {
  for (d in c("objects", "handles", "previews", "tmp")) {
    dir.create(file.path(root, d), recursive = TRUE, showWarnings = FALSE)
  }
  # `project_root` viaja no store (e não na unidade) porque o store é a única
  # coisa que já atravessa a fronteira de processo até o worker. É o que
  # permite a um nó resolver "vendas.csv" contra a raiz do projeto, e não
  # contra o cwd do daemon — que é arbitrário.
  structure(list(root = normalizePath(root),
                 project_root = if (is.null(project_root)) NULL else normalizePath(project_root, mustWork = FALSE)),
            class = "tr_store")
}

#' Resolve um caminho relativo contra a raiz do projeto. Absoluto passa direto;
#' sem raiz conhecida (store solto em teste), cai no cwd — que é o que
#' acontecia antes, só que agora explícito.
#' @export
tr_store_path <- function(store, path) {
  if (is.null(path) || !nzchar(path)) return(path)
  if (grepl("^(/|[A-Za-z]:|~)", path)) return(path.expand(path))
  if (is.null(store$project_root)) return(path)
  file.path(store$project_root, path)
}

.tr_obj_path     <- function(s, key, ext) file.path(s$root, "objects", paste0(key, ".", ext))
.tr_handle_path  <- function(s, key)      file.path(s$root, "handles", paste0(key, ".json"))
.tr_preview_path <- function(s, key, ext) file.path(s$root, "previews", paste0(key, ".", ext))

#' Grava via temporário e renomeia. `file.rename` é atômico dentro do mesmo
#' sistema de arquivos, e por isso o `tmp/` mora DENTRO do store.
#' @noRd
.tr_atomic <- function(s, final, write_fn) {
  tmp <- file.path(s$root, "tmp", paste0(.tr_entropy_hex(16L), ".part"))
  on.exit(unlink(tmp), add = TRUE)
  write_fn(tmp)
  # `store` de tipo que não escreve nada é bug do tipo, e tem que doer aqui —
  # senão vira handle apontando pra objeto inexistente lá na frente.
  if (!file.exists(tmp)) {
    rlang::abort(sprintf("Nada foi escrito para o store em: %s.", final),
                 class = "tr_error_store_write")
  }
  if (!file.rename(tmp, final)) {
    # Falhar aqui em silêncio produziria exatamente o cenário que este módulo
    # afirma ser impossível: `tr_store_put` seguiria e escreveria um handle
    # apontando pra um objeto que não existe (e `bytes` viraria NA sem que
    # ninguém notasse). Disco cheio e permissão são reais.
    if (!file.copy(tmp, final, overwrite = TRUE)) {
      rlang::abort(sprintf("Falha ao gravar no store: %s.", final), class = "tr_error_store_write")
    }
  }
  if (!file.exists(final)) {
    rlang::abort(sprintf("Falha ao gravar no store: %s.", final), class = "tr_error_store_write")
  }
  invisible(final)
}

#' Torna um caminho absoluto do store em relativo à raiz.
#'
#' `sub(paste0("^", root), ...)` trata a raiz como REGEX — um caminho com `+`,
#' `(`, `[` ou `.` (ou `\\` em Windows, sempre) não casa, e o caminho absoluto
#' vaza pro handle. Dois danos: o front recebe caminho de máquina alheia, e
#' `.tr_drop_key` monta `file.path(root, <absoluto>)`, que não apaga nada — GC
#' que não coleta.
#' @noRd
.tr_relpath <- function(store, path) {
  prefix <- paste0(store$root, .Platform$file.sep)
  if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else basename(path)
}

#' Uma chave só "existe" se handle E objeto existem. Handle órfão (remoção
#' externa, GC interrompido) faria o plano marcar `cached = TRUE` e a unidade
#' ser pulada — e o consumidor a jusante explodiria com erro nu de conexão.
#' @export
tr_store_has <- function(store, key) {
  h <- tr_store_handle(store, key)
  if (is.null(h)) return(FALSE)
  if (!is.null(h$error)) return(TRUE)
  file.exists(.tr_obj_path(store, key, h$ext %||% "rds"))
}

#' Handle ilegível conta como ausente, nunca como exceção.
#'
#' Sem isto, um único JSON truncado (worker morto, disco cheio) abortava
#' `tr_store_gc()` e `tr_bust()` — deixando o store PERMANENTEMENTE
#' incoletável, num sistema que cresce sem limite por construção. Disco cheio
#' esperando acontecer, sem caminho de recuperação dentro da API.
#' @export
tr_store_handle <- function(store, key) {
  p <- .tr_handle_path(store, key)
  if (!file.exists(p)) return(NULL)
  tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) NULL)
}

#' Grava valor + handle (+ preview). O handle é escrito POR ÚLTIMO: é ele que
#' marca a chave como presente (`tr_store_has`), então um worker morto no meio
#' deixa no máximo um objeto órfão — nunca um handle apontando pra nada.
#'
#' `collections` é o conjunto de coleções que produziram o artefato. Existe
#' porque `tr_bust(store, coleção)` deriva a coleção de `node_type`, e isso não
#' funciona quando a unidade não é um nó: a região de fluxo grava sob
#' `node_type = "trama/stream_region"` — que lê como coleção "trama" — e mistura
#' coleções de verdade. Ausente (todo nó comum), `tr_bust()` cai no `node_type`,
#' que é onde a informação sempre esteve.
#' @export
tr_store_put <- function(store, key, value, type_spec, node_type = NULL, duration = NA_real_,
                         collections = NULL) {
  ext <- type_spec$ext %||% "rds"
  obj <- .tr_obj_path(store, key, ext)
  .tr_atomic(store, obj, function(tmp) {
    if (is.null(type_spec$store)) saveRDS(value, tmp) else type_spec$store(value, tmp)
  })

  preview <- NULL
  if (!is.null(type_spec$preview)) {
    ctx <- list(file = function(e) {
      p <- .tr_preview_path(store, key, e); dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE); p
    })
    art <- tryCatch(type_spec$preview(value, ctx), error = function(e) {
      list(renderer = "trama/error", data = list(message = conditionMessage(e)))
    })
    if (!is.null(art$files)) {
      # `as.list()` NÃO é decoração: `vapply` devolve vetor de caracteres
      # NOMEADO, e `jsonlite::write_json(auto_unbox = TRUE)` desembrulha um
      # vetor de comprimento 1 para escalar — jogando o nome fora. O handle
      # gravado virava `"files":"previews/k1.png"` em vez de
      # `"files":{"png":"previews/k1.png"}`, e o renderer de imagem
      # (`runtime.js`), ao fazer `files.png || Object.values(files)[0]` sobre
      # uma STRING, recebia `"p"` — a primeira letra. Resultado: `src` 404 e
      # card em branco, sem erro em lugar nenhum. Com dois arquivos o estrago
      # é o mesmo por outro caminho: sai `["a.png","b.svg"]`, nomes perdidos.
      # Lista nomeada atravessa o JSON como objeto, que é o que o contrato de
      # `tr_preview(files =)` promete dos dois lados.
      art$files <- as.list(vapply(art$files, function(f) .tr_relpath(store, f), ""))
    }
    preview <- art
  }

  handle <- list(
    key = key, type = type_spec$id, type_version = type_spec$version,
    # `as.list()` pela mesma razão de `art$files` abaixo: `auto_unbox = TRUE`
    # desembrulharia um vetor de UMA coleção em escalar, e o campo mudaria de
    # forma conforme o número de coleções.
    node_type = node_type,
    collections = if (length(collections)) as.list(collections) else NULL, ext = ext,
    bytes = as.numeric(file.info(obj)$size),
    summary = if (is.null(type_spec$summary)) NULL else
      tryCatch(type_spec$summary(value), error = function(e) NULL),
    preview = preview,
    duration = if (is.na(duration)) NULL else duration,
    created = as.numeric(Sys.time())
  )
  .tr_write_handle(store, key, handle)
  handle
}

#' Erro é VALOR, não exceção que some.
#'
#' Sem isto, um nó que falha não deixa registro, e o jusante tenta executar a
#' cada edição — reexecutando o mundo por causa de uma ponta quebrada. Com o
#' handle de erro, o plano marca o jusante como bloqueado e não roda nada.
#' @export
tr_store_put_error <- function(store, key, message, class = NULL, traceback = NULL,
                               node_type = NULL, collections = NULL) {
  handle <- list(key = key, node_type = node_type,
                 collections = if (length(collections)) as.list(collections) else NULL,
                 error = list(
    message = message, class = class, traceback = traceback
  ), created = as.numeric(Sys.time()))
  .tr_write_handle(store, key, handle)
  handle
}

.tr_write_handle <- function(store, key, handle) {
  .tr_atomic(store, .tr_handle_path(store, key), function(tmp) {
    jsonlite::write_json(handle, tmp, auto_unbox = TRUE, null = "null", digits = NA)
  })
}

#' Um handle representa erro (não valor) quando carrega `$error`.
#' @export
tr_handle_failed <- function(handle) !is.null(handle) && !is.null(handle$error)

#' Lê o valor sob `key`, restaurado pelo `tr_type`; erro alto se a chave estiver ausente ou guardar um erro.
#' @export
tr_store_get <- function(store, key, type_spec) {
  h <- tr_store_handle(store, key)
  if (is.null(h)) rlang::abort(sprintf("Chave ausente no store: %s.", key), class = "tr_error_missing_key")
  if (tr_handle_failed(h)) {
    rlang::abort(sprintf("Chave %s guarda um erro: %s", key, h$error$message), class = "tr_error_failed_key")
  }
  p <- .tr_obj_path(store, key, h$ext)
  if (!file.exists(p)) {
    rlang::abort(sprintf("Handle sem objeto no store: %s.", key), class = "tr_error_missing_object")
  }
  if (is.null(type_spec$restore)) readRDS(p) else type_spec$restore(p)
}

.tr_drop_key <- function(store, key) {
  if (length(key) != 1L || !nzchar(key)) return(invisible(FALSE))
  h <- tr_store_handle(store, key)
  if (!is.null(h) && !is.null(h$ext)) unlink(.tr_obj_path(store, key, h$ext))
  for (f in unlist(h$preview$files)) {
    # Handle pode ter vindo de fora; caminho absoluto nunca é apagado.
    if (!grepl("^(/|[A-Za-z]:)", f)) unlink(file.path(store$root, f))
  }
  unlink(.tr_handle_path(store, key))
  invisible(TRUE)
}

#' Coleta de lixo: o store cresce sem limite por construção (cada edição de
#' param cria uma chave nova). `keep` são as chaves alcançáveis do documento
#' atual; o resto sai por idade.
#' @export
tr_store_gc <- function(store, keep = character(), max_age_days = 7) {
  cutoff <- as.numeric(Sys.time()) - max_age_days * 86400
  removed <- 0L
  for (f in list.files(file.path(store$root, "handles"), pattern = "\\.json$", full.names = TRUE)) {
    key <- sub("\\.json$", "", basename(f))
    if (key %in% keep) next
    h <- tr_store_handle(store, key)   # ilegível -> NULL -> removido como lixo
    if (!is.null(h) && (h$created %||% 0) > cutoff) next
    .tr_drop_key(store, key); removed <- removed + 1L
  }
  unlink(list.files(file.path(store$root, "tmp"), full.names = TRUE))
  removed
}

#' @export
print.tr_store <- function(x, ...) {
  n <- length(list.files(file.path(x$root, "handles"), pattern = "\\.json$"))
  cat(sprintf("<tr_store> %s\n  %d chave(s)\n", x$root, n))
  invisible(x)
}
