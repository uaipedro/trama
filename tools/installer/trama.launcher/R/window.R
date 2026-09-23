#' Janela de aplicativo: abre uma URL sem barra de endereço nem abas — usado
#' tanto por `abrir()` (a tela de início) quanto por `tl_project_open()` (o
#' editor de um projeto). Cai para o navegador padrão (`utils::browseURL()`)
#' quando não encontra Edge (Windows) nem Chrome/Chromium (Linux).
#' @noRd
NULL

#' Comando (executável + argumentos) para abrir `url` como janela de
#' aplicativo, ou `NULL` se nenhum navegador com `--app` foi encontrado —
#' nesse caso `tl_open_window()` cai para `utils::browseURL()`.
#'
#' `which` e `existe` são parâmetros (não `Sys.which()`/`file.exists()`
#' direto no corpo) só para os testes poderem trocar por um SO fake sem
#' mexer no disco de verdade (task 2.4 do plano: "só a escolha do comando").
#' @noRd
.tl_janela_comando <- function(url, os = tl_os(), which = Sys.which, existe = file.exists) {
  candidatos <- if (identical(os$tipo, "windows")) {
    c(
      which("msedge"),
      file.path(Sys.getenv("ProgramFiles(x86)"), "Microsoft", "Edge", "Application", "msedge.exe"),
      file.path(Sys.getenv("ProgramFiles"), "Microsoft", "Edge", "Application", "msedge.exe")
    )
  } else {
    c(which("google-chrome"), which("chromium"), which("chromium-browser"))
  }

  candidatos <- candidatos[nzchar(candidatos)]
  candidatos <- candidatos[vapply(candidatos, existe, logical(1))]
  if (!length(candidatos)) return(NULL)

  list(exe = candidatos[[1]], args = sprintf("--app=%s", url))
}

#' Abre `url` como janela de aplicativo, sem barra de endereço nem abas.
#'
#' @param url URL a abrir.
#' @param comando Comando escolhido por `.tl_janela_comando()`, ou `NULL`
#'   para cair no navegador padrão.
#' @param executar Wrapper em torno de `system2()`, só para poder mockar nos
#'   testes.
#' @param navegador Função chamada quando não há navegador em modo `--app`
#'   disponível. Por padrão `utils::browseURL()`.
#' @noRd
tl_open_window <- function(url, comando = .tl_janela_comando(url),
                            executar = system2, navegador = utils::browseURL) {
  if (is.null(comando)) {
    navegador(url)
    return(invisible(NULL))
  }
  executar(comando$exe, comando$args, wait = FALSE)
  invisible(NULL)
}
