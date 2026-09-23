#' Estado instalado: qual release está ativa, as duas anteriores (para
#' rollback) e as coleções instaladas. Guardado em `estado.json`.
#' @noRd
NULL

#' Estado vazio: nenhuma release instalada ainda. É o que `tl_state_read()`
#' devolve quando `estado.json` não existe — nunca um erro, porque "acabei
#' de instalar o launcher e ainda não tenho release" é um estado normal.
#' @noRd
.tl_estado_vazio <- function() {
  list(atual = "", anteriores = character(0), colecoes = character(0))
}

#' Lê `estado.json`. Devolve o estado vazio se o arquivo não existir.
#' @noRd
tl_state_read <- function() {
  arquivo <- tl_state_file()
  if (!file.exists(arquivo)) return(.tl_estado_vazio())

  bruto <- jsonlite::fromJSON(arquivo, simplifyVector = TRUE)
  list(
    atual = if (is.null(bruto$atual)) "" else as.character(bruto$atual),
    anteriores = if (is.null(bruto$anteriores)) character(0) else as.character(bruto$anteriores),
    colecoes = if (is.null(bruto$colecoes)) character(0) else as.character(bruto$colecoes)
  )
}

#' Escreve `estado.json` de forma atômica: grava num arquivo temporário na
#' mesma pasta e renomeia por cima do destino. Um processo que ler o estado
#' no meio do caminho vê o arquivo antigo inteiro ou o novo inteiro, nunca
#' um JSON pela metade.
#' @noRd
tl_state_write <- function(s) {
  dir.create(tl_home(), recursive = TRUE, showWarnings = FALSE)
  destino <- tl_state_file()
  tmp <- tempfile(pattern = "estado-", tmpdir = tl_home(), fileext = ".json.tmp")
  writeLines(jsonlite::toJSON(s, auto_unbox = TRUE), tmp)
  file.rename(tmp, destino)
  invisible(s)
}

#' Compara `x.y.z` até `x.y`, para decidir se uma release exige trocar de
#' linha do R (e portanto baixar um Setup novo em vez de só atualizar).
#' @noRd
.tl_r_major_minor <- function(versao) {
  partes <- strsplit(as.character(versao), "\\.")[[1]]
  paste(partes[1:2], collapse = ".")
}

#' Versão de um pacote instalado na biblioteca `lib`, ou `NA` se não estiver
#' instalado. Usa `packageDescription()` em vez de `packageVersion()` porque
#' este último lança erro quando o pacote não existe, e aqui "não instalado"
#' é uma resposta válida, não uma exceção.
#' @noRd
.tl_versao_instalada <- function(pacote, lib) {
  desc <- tryCatch(
    suppressWarnings(utils::packageDescription(pacote, lib.loc = lib)),
    error = function(e) NA
  )
  if (is.null(desc) || identical(desc, NA) || isTRUE(is.na(desc))) return(NA_character_)
  desc$Version
}

#' Status da instalação local
#'
#' O que a tela de início lê para desenhar versões, botão de atualizar,
#' coleções e o aviso de troca de linha do R. Nunca lança erro — mesmo sem
#' instalação e sem rede, devolve uma resposta coerente (a página tem que
#' conseguir se desenhar nesse estado).
#'
#' @param m Manifesto (de `tl_manifest_fetch()`) ou `NULL` se offline.
#' @param s Estado local (de `tl_state_read()`).
#' @return Lista com `release_instalada`, `release_disponivel`,
#'   `atualizar`, `r_instalado`, `r_exigido`, `troca_de_r`, `pacotes`
#'   (data.frame nome/instalada/disponível) e `colecoes` (data.frame
#'   nome/titulo/instalada/disponivel/requires).
#' @export
tl_status <- function(m = tl_manifest_fetch(), s = tl_state_read()) {
  lib_atual <- tl_lib_dir(s$atual)

  release_disponivel <- if (is.null(m)) NA_character_ else m$trama
  atualizar <- !is.null(m) && !identical(s$atual, m$trama)

  r_instalado <- getRversion()
  r_exigido <- if (is.null(m)) NA_character_ else m$r
  troca_de_r <- !is.null(m) &&
    !identical(.tl_r_major_minor(r_instalado), .tl_r_major_minor(r_exigido))

  nomes_pacotes <- if (!is.null(m)) names(m$core) else .tl_pacotes_instalados(lib_atual)
  pacotes <- data.frame(
    nome = nomes_pacotes,
    instalada = vapply(nomes_pacotes, .tl_versao_instalada, character(1), lib = lib_atual),
    disponivel = vapply(nomes_pacotes, function(p) {
      if (is.null(m) || is.null(m$core[[p]])) NA_character_ else m$core[[p]]
    }, character(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )

  colecoes <- .tl_status_colecoes(m, s)

  list(
    release_instalada = s$atual,
    release_disponivel = release_disponivel,
    atualizar = atualizar,
    r_instalado = r_instalado,
    r_exigido = r_exigido,
    troca_de_r = troca_de_r,
    pacotes = pacotes,
    colecoes = colecoes
  )
}

#' Nomes das pastas de pacote existentes numa lib (usado quando não há
#' manifesto para saber quais pacotes checar).
#' @noRd
.tl_pacotes_instalados <- function(lib) {
  if (!dir.exists(lib)) return(character(0))
  list.dirs(lib, recursive = FALSE, full.names = FALSE)
}

#' Tabela de coleções: união das oferecidas pelo manifesto com as
#' instaladas localmente, para nenhuma coleção instalada "sumir" da tela só
#' porque ficou de fora de uma release nova.
#' @noRd
.tl_status_colecoes <- function(m, s) {
  ofertadas <- if (is.null(m)) character(0) else names(m$collections)
  nomes <- union(ofertadas, s$colecoes)

  titulo <- vapply(nomes, function(nm) {
    t <- if (!is.null(m)) m$collections[[nm]]$title else NULL
    if (is.null(t)) NA_character_ else t
  }, character(1))

  requires <- lapply(nomes, function(nm) {
    r <- if (!is.null(m)) m$collections[[nm]]$requires else NULL
    if (is.null(r)) character(0) else unlist(r, use.names = FALSE)
  })

  data.frame(
    nome = nomes,
    titulo = unname(titulo),
    instalada = nomes %in% s$colecoes,
    disponivel = nomes %in% ofertadas,
    requires = I(requires),
    stringsAsFactors = FALSE, row.names = NULL
  )
}
