#' Base de dados pública que uma coleção oferece no catálogo de bases.
#'
#' O catálogo guarda só METADADOS: o dado mora no pacote R que o publica e só
#' é baixado quando alguém escolhe a base. Por isso `n` e `variaveis` são
#' declarados à mão, e não lidos: com o pacote ainda não instalado não há de
#' onde lê-los, e é justamente antes de instalar que o usuário quer saber se a
#' base serve.
#'
#' O núcleo não sabe carregar dado nenhum. Quem sabe é o bloco `node`, com os
#' `params` dados: escolher a base no modal insere esse bloco no canvas, e a
#' prévia e o CSV rodam a função dele. Assim uma base de `datasets`, de
#' `agridat` ou de uma API futura passa pelo mesmo caminho, e o fluxo salvo
#' guarda a referência, não o dado.
#'
#' @param pacote Pacote R que publica a base.
#' @param nome Nome do objeto no pacote.
#' @param titulo Título curto, para a lista.
#' @param node Id do bloco que carrega a base.
#' @param params Params do bloco (lista nomeada).
#' @param descricao Uma ou duas frases: o que é e para que serve.
#' @param fonte Livro, artigo ou instituição de origem.
#' @param temas Palavras para filtrar (ex.: `"regressão"`, `"agricultura"`).
#' @param n,variaveis Linhas e colunas (número), declarados.
#' @param licenca Licença do dado, como o pacote a declara.
#' @param url Página da base ou do pacote.
#' @export
tr_dataset <- function(pacote, nome, titulo, node, params = list(),
                       descricao = NULL, fonte = NULL, temas = character(),
                       n = NULL, variaveis = NULL, licenca = NULL, url = NULL) {
  str1 <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  for (a in c("pacote", "nome", "titulo", "node")) if (!str1(get(a))) {
    rlang::abort(sprintf("tr_dataset(): '%s' tem que ser um texto.", a),
                 class = "tr_error_bad_dataset")
  }
  if (!is.list(params) || (length(params) && (is.null(names(params)) || !all(nzchar(names(params)))))) {
    rlang::abort(sprintf("tr_dataset() '%s::%s': 'params' tem que ser lista nomeada.", pacote, nome),
                 class = "tr_error_bad_dataset")
  }
  structure(list(id = paste0(pacote, "::", nome), pacote = pacote, nome = nome,
                 titulo = titulo, node = node, params = params, descricao = descricao,
                 fonte = fonte, temas = as.character(temas), n = n,
                 variaveis = variaveis, licenca = licenca, url = url),
            class = "tr_dataset")
}

#' Bases de todas as coleções do registro, com o estado de instalação.
#'
#' A mesma base declarada por duas coleções aparece uma vez (a primeira).
#' `instalado` é lido agora, e não no registro: o usuário instala pacote com o
#' editor aberto, e o catálogo reaberto tem que mostrar isso.
#' @export
tr_datasets <- function(registry = .tr_default_registry) {
  todas <- unlist(lapply(registry$datasets %||% list(), function(x) x), recursive = FALSE)
  todas <- todas[!duplicated(vapply(todas, function(d) d$id, ""))]
  inst <- .tr_pkg_installed_memo()
  unname(lapply(todas, function(d) {
    out <- .tr_json_drop_empty(unclass(d))
    out$temas <- I(d$temas)
    out$instalado <- inst(d$pacote)
    out
  }))
}

#' @noRd
.tr_pkg_installed_memo <- function() {
  memo <- list()
  function(p) {
    if (is.null(memo[[p]])) memo[[p]] <<- nzchar(system.file(package = p))
    memo[[p]]
  }
}

#' @noRd
.tr_dataset_get <- function(registry, id) {
  for (col in registry$datasets %||% list()) for (d in col) if (identical(d$id, id)) return(d)
  rlang::abort(sprintf("Base desconhecida: '%s'.", id), class = "tr_error_unknown_dataset")
}

#' Carrega uma base rodando o bloco que a declara.
#'
#' Só para bloco sem entrada: é isso que o modal promete (a base vira a
#' ORIGEM de um fluxo). Um bloco com várias saídas entrega a primeira.
#' @export
tr_dataset_load <- function(id, registry = .tr_default_registry) {
  d <- .tr_dataset_get(registry, id)
  if (!nzchar(system.file(package = d$pacote))) {
    rlang::abort(sprintf("O pacote '%s' não está instalado.", d$pacote),
                 class = "tr_error_dataset_missing_pkg")
  }
  x <- do.call(tr_fn(d$node, registry), d$params)
  if (is.list(x) && !is.data.frame(x) && length(tr_get_node(d$node, registry)$outputs) > 1L) x <- x[[1L]]
  x
}

#' Prévia serializável: dimensões, nome e classe das colunas e as primeiras linhas.
#' @noRd
.tr_dataset_preview <- function(x, linhas = 8L) {
  x <- as.data.frame(x)
  cab <- utils::head(x, linhas)
  cab[] <- lapply(cab, function(v) if (is.numeric(v)) signif(v, 4) else as.character(v))
  list(n = nrow(x), p = ncol(x),
       colunas = unname(Map(function(nm, v) list(nome = nm, classe = class(v)[1L]), names(x), x)),
       linhas = unname(lapply(seq_len(nrow(cab)), function(i) unname(as.list(cab[i, , drop = FALSE])))))
}

#' Instala pacotes num R à parte e avisa quando acaba.
#'
#' À parte porque `install.packages()` na sessão do editor congelaria o Shiny
#' pelo tempo do download e da compilação. O processo filho grava o log e, ao
#' fim, um arquivo de status; `on_done(ok, log)` é chamado pelo `later` quando
#' ele aparece. Os repositórios são os da sessão (`getOption("repos")`), com
#' CRAN padrão se não houver nenhum.
#' @noRd
.tr_install_bg <- function(pacote, on_done, intervalo = 0.5, limite = 20 * 60) {
  if (!grepl("^[A-Za-z][A-Za-z0-9.]*$", pacote)) {
    rlang::abort(sprintf("Nome de pacote inválido: '%s'.", pacote), class = "tr_error_bad_dataset")
  }
  repos <- getOption("repos")
  if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) repos <- c(CRAN = "https://cloud.r-project.org")
  dir <- tempfile("trama-install-"); dir.create(dir)
  log <- file.path(dir, "log.txt"); fim <- file.path(dir, "status")
  script <- file.path(dir, "install.R")
  writeLines(c(
    sprintf("repos <- %s", paste(deparse(repos), collapse = "")),
    sprintf("lib <- %s; .libPaths(c(lib, .libPaths()))", deparse(.tr_install_lib())),
    sprintf("ok <- tryCatch({ utils::install.packages(%s, repos = repos, lib = lib); nzchar(system.file(package = %s, lib.loc = lib)) }, error = function(e) { message(conditionMessage(e)); FALSE })",
            deparse(pacote), deparse(pacote)),
    sprintf("writeLines(if (isTRUE(ok)) 'ok' else 'erro', %s)", deparse(fim))
  ), script)
  system2(file.path(R.home("bin"), "Rscript"), c("--no-save", "--no-restore", shQuote(script)),
          stdout = log, stderr = log, wait = FALSE)
  # Filho que morre antes de gravar o status (sem memória, R que não sobe)
  # deixaria o `later` checando para sempre e o modal preso em "Instalando…":
  # passado o limite, conta como erro.
  inicio <- Sys.time()
  checar <- function() {
    if (!file.exists(fim) && difftime(Sys.time(), inicio, units = "secs") > limite) {
      txt <- if (file.exists(log)) readLines(log, warn = FALSE) else character()
      on_done(FALSE, paste(c(utils::tail(txt, 15L), "Instalação não terminou no tempo limite."), collapse = "\n"))
    } else if (file.exists(fim)) {
      ok <- identical(readLines(fim, warn = FALSE)[1], "ok")
      txt <- if (file.exists(log)) readLines(log, warn = FALSE) else character()
      on_done(ok, paste(utils::tail(txt, 15L), collapse = "\n"))
    } else {
      later::later(checar, intervalo)
    }
  }
  later::later(checar, intervalo)
  invisible(dir)
}

#' Primeira biblioteca GRAVÁVEL da sessão: num R de sistema a primeira de
#' `.libPaths()` costuma ser a do usuário, mas não é garantido.
#' @noRd
.tr_install_lib <- function() {
  libs <- .libPaths()
  ok <- libs[file.access(libs, 2L) == 0L]
  if (length(ok)) ok[1L] else libs[1L]
}
