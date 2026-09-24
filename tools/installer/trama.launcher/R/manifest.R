#' Manifesto de release: contrato público entre a release publicada, o
#' instalador nativo (`bootstrap.R`) e o launcher. Falhar alto e em
#' português é intencional — um manifesto malformado publicado por engano
#' não pode se manifestar três passos depois como um NA silencioso numa
#' instalação de pacote.
#' @noRd
NULL

.tl_manifest_chaves_obrigatorias <- c("trama", "r", "cran_snapshot", "core")

#' Lê e valida o manifesto de release
#'
#' Lê e valida o manifesto de `src` (URL ou caminho local). Lança erro com
#' classe `tl_error_manifesto` e mensagem em português se faltar alguma
#' chave obrigatória, se `cran_snapshot` não for uma data ISO (`AAAA-MM-DD`)
#' ou se `r` não estiver no formato `x.y.z`.
#'
#' @param src URL ou caminho local do manifesto (`release.json`).
#' @return Lista com o manifesto validado.
#' @export
tl_manifest_read <- function(src = tl_manifest_url()) {
  m <- .tl_manifest_ler_bruto(src)

  faltando <- setdiff(.tl_manifest_chaves_obrigatorias, names(m))
  if (length(faltando)) {
    .tl_erro_manifesto(sprintf(
      "Manifesto de release inválido: faltam as chaves obrigatórias: %s.",
      paste(faltando, collapse = ", ")
    ))
  }

  if (!grepl("^\\d+\\.\\d+\\.\\d+$", m$r)) {
    .tl_erro_manifesto(sprintf(
      "Manifesto de release inválido: 'r' deveria estar no formato x.y.z, recebi '%s'.",
      m$r
    ))
  }

  data_snapshot <- tryCatch(as.Date(m$cran_snapshot, format = "%Y-%m-%d"),
                             warning = function(w) NA)
  if (is.na(data_snapshot) || !identical(format(data_snapshot, "%Y-%m-%d"), m$cran_snapshot)) {
    .tl_erro_manifesto(sprintf(
      "Manifesto de release inválido: 'cran_snapshot' deveria ser uma data ISO (AAAA-MM-DD), recebi '%s'.",
      m$cran_snapshot
    ))
  }

  m
}

#' Guarda a última mensagem de erro de `tl_manifest_fetch()`. Existe porque
#' `NULL` não aceita atributo (`attr(x, "erro") <- ...` em `NULL` é erro em
#' R), e `is.null()` é o teste que o resto do motor (`tl_status()`) usa para
#' "estou offline, sigo sem checar atualização" — então o retorno tem que
#' continuar sendo um `NULL` de verdade.
#' @noRd
.tl_estado_fetch <- new.env(parent = emptyenv())
.tl_estado_fetch$erro <- NULL

#' Igual a `tl_manifest_read()`, mas nunca lança erro: se a leitura falhar
#' (offline, timeout de 5s, manifesto malformado), devolve `NULL` e deixa a
#' mensagem em `tl_manifest_fetch_erro()`. É o que o launcher usa para
#' seguir sem checar atualização quando não há rede.
#' @noRd
tl_manifest_fetch <- function(src = tl_manifest_url()) {
  m <- tryCatch(tl_manifest_read(src), error = function(e) e)
  if (inherits(m, "error")) {
    .tl_estado_fetch$erro <- conditionMessage(m)
    return(NULL)
  }
  .tl_estado_fetch$erro <- NULL
  m
}

#' Mensagem do último `tl_manifest_fetch()` que falhou, ou `NULL` se o
#' último foi bem-sucedido (ou nenhum ainda rodou).
#' @noRd
tl_manifest_fetch_erro <- function() .tl_estado_fetch$erro

#' Lê o JSON bruto de `src`, com timeout de 5s para não travar o launcher
#' numa rede ruim.
#' @noRd
.tl_manifest_ler_bruto <- function(src) {
  timeout_antigo <- getOption("timeout")
  options(timeout = 5)
  on.exit(options(timeout = timeout_antigo), add = TRUE)

  tryCatch(
    suppressWarnings(jsonlite::fromJSON(src, simplifyVector = FALSE)),
    error = function(e) {
      .tl_erro_manifesto(sprintf(
        "Não foi possível ler o manifesto de release em '%s': %s", src, conditionMessage(e)
      ))
    }
  )
}

#' Erro com classe `tl_error_manifesto`, sem depender de rlang (o pacote não
#' importa rlang; só jsonlite, shiny, utils e tools).
#' @noRd
.tl_erro_manifesto <- function(mensagem) {
  cond <- structure(
    class = c("tl_error_manifesto", "error", "condition"),
    list(message = mensagem, call = sys.call(-1))
  )
  stop(cond)
}

#' Informação de sistema operacional necessária para montar a URL do P3M.
#' Existe como função à parte (em vez de `.Platform$OS.type` espalhado) só
#' para dar um ponto único para mockar nos testes.
#' @noRd
tl_os <- function() {
  if (.Platform$OS.type == "windows") return(list(tipo = "windows"))
  sistema <- Sys.info()[["sysname"]]
  if (!identical(sistema, "Linux")) {
    return(list(tipo = "unix", codename = NA_character_, sistema = sistema))
  }
  list(tipo = "unix", codename = .tl_codename_linux(), sistema = sistema)
}

#' Campo `chave` de `/etc/os-release` (ex.: `UBUNTU_CODENAME`), ou `NA` se
#' não existir.
#' @noRd
.tl_campo_os_release <- function(linhas, chave) {
  alvo <- grep(sprintf("^%s=", chave), linhas, value = TRUE)
  if (!length(alvo)) return(NA_character_)
  gsub(sprintf('^%s="?|"?$', chave), "", alvo[[1]])
}

#' Codename da distro Linux (ex.: "jammy"), lido de `/etc/os-release`.
#' Prefere `UBUNTU_CODENAME` a `VERSION_CODENAME` — no Ubuntu,
#' `VERSION_CODENAME` às vezes reflete o Debian de base, e é o codename
#' Ubuntu de verdade que o P3M (`__linux__/<codename>/...`) espera. Devolve
#' `NA` se o arquivo não existir ou não tiver nenhum dos dois campos (mesma
#' cópia sincronizada de `bootstrap.R`, `.bs_codename_linux()`).
#' @noRd
.tl_codename_linux <- function() {
  arquivo <- "/etc/os-release"
  if (!file.exists(arquivo)) return(NA_character_)
  linhas <- readLines(arquivo, warn = FALSE)
  ubuntu <- .tl_campo_os_release(linhas, "UBUNTU_CODENAME")
  if (!is.na(ubuntu) && nzchar(ubuntu)) return(ubuntu)
  .tl_campo_os_release(linhas, "VERSION_CODENAME")
}

#' Repositórios do manifesto mais o P3M preso na data do `cran_snapshot`,
#' em binário. No Windows a URL é genérica; no Linux leva o codename da
#' distro quando dá para descobrir (senão cai na URL genérica também).
#'
#' Se `TRAMA_EXTRA_REPOS` estiver setada (uma ou mais URLs `file:///...`
#' separadas por `;`), essas URLs entram NA FRENTE dos repositórios normais,
#' com o nome `"local"` — é assim que `--local <dir>` do bootstrap.R vira um
#' repositório extra com os nossos pacotes em vez de um caminho de
#' instalação separado (ver `bootstrap.R`, que replica esta mesma lógica
#' porque roda antes de o trama.launcher existir).
#' @noRd
tl_repos <- function(m) {
  os <- tl_os()
  sem_codename <- is.null(os$codename) || is.na(os$codename) || !nzchar(os$codename)

  # Linux de verdade sem UBUNTU_CODENAME/VERSION_CODENAME: abortar (mensagem
  # em português, classe `tl_error_manifesto` — mesma família de erro do
  # resto deste arquivo) em vez de cair silenciosamente na URL genérica do
  # P3M, que devolveria pacotes fonte em vez de binários. Outros "unix"
  # (macOS) e Windows continuam caindo na URL genérica normalmente.
  if (identical(os$tipo, "unix") && identical(os[["sistema"]], "Linux") && sem_codename) {
    .tl_erro_manifesto("distribuição Linux não suportada ainda (sem UBUNTU_CODENAME nem VERSION_CODENAME em /etc/os-release).")
  }

  snapshot <- if (identical(os$tipo, "windows") || sem_codename) {
    sprintf("https://packagemanager.posit.co/cran/%s", m$cran_snapshot)
  } else {
    sprintf("https://packagemanager.posit.co/cran/__linux__/%s/%s", os$codename, m$cran_snapshot)
  }
  repos <- if (is.null(m$repos)) character(0) else unlist(m$repos, use.names = FALSE)

  extra <- Sys.getenv("TRAMA_EXTRA_REPOS")
  if (nzchar(extra)) {
    extra_repos <- strsplit(extra, ";", fixed = TRUE)[[1]]
    names(extra_repos) <- rep_len("local", length(extra_repos))
    repos <- c(extra_repos, repos)
  }

  c(repos, P3M = snapshot)
}
