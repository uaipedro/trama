#' Abre o launcher do trama
#'
#' Ponto de entrada chamado pelo atalho instalado (`Trama-Setup.exe` no
#' Windows, `install.sh` no Linux): coloca a biblioteca da release atual na
#' frente do `.libPaths()` e sobe a tela de início do launcher. Se ainda não
#' houver nenhuma release instalada, instala a mais recente do manifesto
#' antes de subir a tela — é o que faz a primeira abertura já mostrar o
#' progresso da instalação em vez de uma tela vazia.
#'
#' @param porta Porta onde a tela de início sobe; se ocupada, usa a
#'   primeira livre a partir daqui.
#' @return Não retorna em uso normal — `shiny::runApp()` bloqueia a sessão
#'   enquanto a tela de início está aberta.
#' @export
abrir <- function(porta = 8725L) {
  s <- tl_state_read()
  if (!nzchar(s$atual)) {
    tl_install_release(tl_manifest_read(), colecoes = character(0))
    s <- tl_state_read()
  }

  .libPaths(c(tl_lib_dir(s$atual), .libPaths()))

  headless <- nzchar(Sys.getenv("TRAMA_HEADLESS"))
  shiny::runApp(
    tl_app(),
    port = .tl_porta_livre(porta),
    # `launch.browser` aceita uma função(url): o Shiny a chama só depois do
    # app estar de pé, exatamente o momento certo para abrir a janela do
    # app (task 2.4) em vez do navegador padrão.
    launch.browser = if (headless) FALSE else tl_open_window,
    host = "127.0.0.1"
  )
}

#' Primeira porta livre a partir de `porta` (inclusive), tentando abrir e
#' fechar um socket de servidor nela. `serverSocket()` é de `base`, então
#' não precisa de import extra.
#' @noRd
.tl_porta_livre <- function(porta, tentativas = 20L) {
  for (p in seq.int(porta, porta + tentativas - 1L)) {
    con <- tryCatch(serverSocket(p), error = function(e) NULL)
    if (!is.null(con)) {
      close(con)
      return(p)
    }
  }
  stop(sprintf("Não encontrei porta livre a partir de %d.", porta), call. = FALSE)
}
