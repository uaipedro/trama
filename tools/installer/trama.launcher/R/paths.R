#' Pasta raiz onde o trama guarda tudo: bibliotecas por release, estado,
#' logs. `TRAMA_HOME` sobrescreve, e é assim que os testes isolam disco.
#' @noRd
tl_home <- function() {
  h <- Sys.getenv("TRAMA_HOME", "")
  if (nzchar(h)) return(normalizePath(h, mustWork = FALSE))
  if (.Platform$OS.type == "windows")
    file.path(Sys.getenv("LOCALAPPDATA"), "Trama")
  else file.path(path.expand("~"), ".local", "share", "trama")
}

#' Biblioteca de pacotes de uma release específica, isolada das demais.
#' @noRd
tl_lib_dir <- function(release) file.path(tl_home(), "lib", release)

#' Arquivo com o estado instalado (release atual, anteriores, coleções).
#' @noRd
tl_state_file <- function() file.path(tl_home(), "estado.json")

#' Pasta de logs de instalação/atualização.
#' @noRd
tl_log_dir <- function() file.path(tl_home(), "logs")

#' URL (ou caminho local) do manifesto de release. `TRAMA_MANIFEST_URL`
#' sobrescreve, para testes e CI apontarem para uma fixture.
#' @noRd
tl_manifest_url <- function() {
  u <- Sys.getenv("TRAMA_MANIFEST_URL", "")
  if (nzchar(u)) u else
    "https://github.com/uaipedro/trama/releases/latest/download/release.json"
}
