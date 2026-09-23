#' Instalação de release: uma biblioteca por release, só binário, e o
#' estado só muda depois que TODOS os pacotes pedidos confirmadamente
#' instalaram — uma falha no meio não pode deixar `estado.json` apontando
#' para uma lib pela metade.
#' @noRd
NULL

.tl_erro_instalacao <- function(mensagem) {
  cond <- structure(
    class = c("tl_error_instalacao", "error", "condition"),
    list(message = mensagem, call = sys.call(-1))
  )
  stop(cond)
}

#' Wrapper em torno de `install.packages()`, só para poder mockar nos
#' testes (o `install.packages()` de verdade só entra no CI, task 4.1).
#' @noRd
tl_install_pkgs <- function(pkgs, lib, repos) {
  utils::install.packages(pkgs, lib = lib, repos = repos, dependencies = NA)
}

#' Wrapper em torno de `remove.packages()`, mesmo motivo de `tl_install_pkgs()`.
#' @noRd
tl_remove_pkgs <- function(pkgs, lib) {
  utils::remove.packages(pkgs, lib = lib)
}

#' Instala uma release numa biblioteca isolada
#'
#' Instala a release descrita em `m` (mais as `colecoes` pedidas) numa
#' biblioteca isolada `tl_lib_dir(m$trama)`, só com pacotes binários.
#'
#' Só grava o estado se TODOS os pacotes pedidos existirem na lib ao final;
#' se algum faltar, apaga a lib parcial e lança erro sem tocar no estado —
#' quem já tinha uma release funcionando continua com ela.
#'
#' @param m Manifesto de release (`tl_manifest_read()`/`tl_manifest_fetch()`).
#' @param colecoes Nomes de coleções a instalar junto com o núcleo.
#' @param progresso Função chamada com uma mensagem a cada pacote (`message()` por padrão).
#' @return (Invisível) o novo estado gravado.
#' @export
tl_install_release <- function(m, colecoes = tl_state_read()$colecoes, progresso = message) {
  lib <- tl_lib_dir(m$trama)
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)

  repos_antigos <- getOption("repos")
  pkgtype_antigo <- getOption("pkgType")
  compilar_antigo <- getOption("install.packages.compile.from.source")
  on.exit(options(
    repos = repos_antigos, pkgType = pkgtype_antigo,
    install.packages.compile.from.source = compilar_antigo
  ), add = TRUE)

  repos <- tl_repos(m)
  options(repos = repos)
  if (.Platform$OS.type == "windows" || identical(Sys.info()[["sysname"]], "Darwin")) {
    options(pkgType = "binary", install.packages.compile.from.source = "never")
  }

  pacotes <- unique(c(names(m$core), colecoes))

  dir.create(tl_log_dir(), recursive = TRUE, showWarnings = FALSE)
  log <- file.path(tl_log_dir(), sprintf("instalacao-%s.log", format(Sys.time(), "%Y%m%d-%H%M%S")))
  con <- file(log, open = "wt")
  sink(con, type = "output")
  on.exit({ sink(type = "output"); close(con) }, add = TRUE)

  # Um `tl_install_pkgs()` por pacote, não um só pra `pacotes` inteiro: pedido
  # na revisão da fase 1 pra `progresso()` acompanhar a instalação de verdade
  # (pacote a pacote), em vez de despejar todas as mensagens de uma vez antes
  # de qualquer coisa ser instalada.
  for (p in pacotes) {
    progresso(sprintf("Instalando %s…", p))
    tl_install_pkgs(p, lib = lib, repos = repos)
  }

  faltando <- pacotes[!vapply(pacotes, function(p) dir.exists(file.path(lib, p)), logical(1))]
  if (length(faltando)) {
    unlink(lib, recursive = TRUE, force = TRUE)
    .tl_erro_instalacao(sprintf(
      "Falhou a instalação de: %s. Veja o log em '%s'.", paste(faltando, collapse = ", "), log
    ))
  }

  s <- tl_state_read()
  anteriores <- unique(c(s$atual, s$anteriores))
  anteriores <- anteriores[nzchar(anteriores) & anteriores != m$trama]
  if (length(anteriores) > 2) {
    for (velha in anteriores[-(1:2)]) unlink(tl_lib_dir(velha), recursive = TRUE, force = TRUE)
    anteriores <- anteriores[1:2]
  }

  novo_estado <- list(atual = m$trama, anteriores = anteriores, colecoes = colecoes, recentes = s$recentes)
  tl_state_write(novo_estado)
  invisible(novo_estado)
}

#' Volta para a release anterior
#'
#' Volta a release atual para a anterior mais recente (a lib da release
#' atual continua no disco, só não é mais a apontada pelo estado — assim dá
#' para avançar de novo sem reinstalar).
#'
#' @return (Invisível) o novo estado gravado.
#' @export
tl_rollback <- function() {
  s <- tl_state_read()
  if (!length(s$anteriores)) {
    .tl_erro_instalacao("Não há release anterior para voltar.")
  }

  novo_estado <- list(
    atual = s$anteriores[[1]],
    anteriores = c(s$atual, s$anteriores[-1]),
    colecoes = s$colecoes,
    recentes = s$recentes
  )
  tl_state_write(novo_estado)
  invisible(novo_estado)
}

#' Instala uma coleção na release atual
#'
#' Instala uma coleção na lib da release atual e a acrescenta ao estado.
#' Recusa qualquer coleção que não conste no manifesto — o manifesto é a
#' única fonte de verdade sobre o que é instalável nesta release.
#'
#' @param m Manifesto de release.
#' @param nome Nome da coleção a instalar.
#' @param progresso Função chamada com uma mensagem de progresso.
#' @return (Invisível) o novo estado gravado.
#' @export
tl_collection_add <- function(m, nome, progresso = message) {
  if (!nome %in% names(m$collections)) {
    .tl_erro_instalacao(sprintf(
      "'%s' não é uma coleção desta release. Coleções disponíveis: %s.",
      nome, paste(names(m$collections), collapse = ", ")
    ))
  }

  s <- tl_state_read()
  lib <- tl_lib_dir(s$atual)

  progresso(sprintf("Instalando %s…", nome))
  tl_install_pkgs(nome, lib = lib, repos = tl_repos(m))

  if (!dir.exists(file.path(lib, nome))) {
    .tl_erro_instalacao(sprintf("Falhou a instalação da coleção '%s'.", nome))
  }

  s$colecoes <- union(s$colecoes, nome)
  tl_state_write(s)
  invisible(s)
}

#' Remove uma coleção da release atual
#'
#' Remove uma coleção da lib da release atual e a tira do estado.
#'
#' @param nome Nome da coleção a remover.
#' @param progresso Função chamada com uma mensagem de progresso.
#' @return (Invisível) o novo estado gravado.
#' @export
tl_collection_remove <- function(nome, progresso = message) {
  s <- tl_state_read()
  lib <- tl_lib_dir(s$atual)

  progresso(sprintf("Removendo %s…", nome))
  tl_remove_pkgs(nome, lib = lib)

  s$colecoes <- setdiff(s$colecoes, nome)
  tl_state_write(s)
  invisible(s)
}
