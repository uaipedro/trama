#' Abre o launcher do trama
#'
#' Ponto de entrada chamado pelo atalho instalado (`Trama-Setup.exe` no
#' Windows, `install.sh` no Linux): coloca a biblioteca da release atual na
#' frente do `.libPaths()` (se já houver uma) e sobe a tela de início do
#' launcher. Se ainda não houver nenhuma release instalada, é a tela de
#' início quem dispara a primeira instalação (`tl_server()`, em R/app.R) —
#' *depois* dela estar no ar, não antes: instalar antes do `runApp()`
#' rodava `tl_manifest_read()` sem UI nenhuma para mostrar o progresso ou
#' um erro (e lançava erro de verdade se estivesse offline, derrubando
#' `abrir()` inteiro antes do pesquisador ver qualquer coisa).
#'
#' @param porta Porta onde a tela de início sobe; se ocupada, usa a
#'   primeira livre a partir daqui.
#' @param ja_rodando Função que confere se já há um launcher respondendo em
#'   `porta` (padrão `.tl_launcher_rodando()`) — parâmetro só para os testes
#'   poderem mockar sem abrir um socket de verdade.
#' @param abrir_janela Como abrir a janela quando o launcher já está de pé.
#'   Por padrão `tl_open_window()`.
#' @return Não retorna em uso normal — `shiny::runApp()` bloqueia a sessão
#'   enquanto a tela de início está aberta (a menos que já houvesse um
#'   launcher de pé, caso em que só abre a janela e retorna).
#' @export
abrir <- function(porta = 8725L, ja_rodando = .tl_launcher_rodando, abrir_janela = tl_open_window) {
  # Já existe um launcher desta instalação respondendo nesta porta (outro
  # atalho clicado enquanto o primeiro ainda estava de pé, ex.: duplo clique
  # sem querer): só abre a janela de novo nele, em vez de subir um segundo
  # processo Shiny disputando a mesma porta.
  if (isTRUE(ja_rodando(porta))) {
    abrir_janela(sprintf("http://127.0.0.1:%d", porta))
    return(invisible(NULL))
  }

  s <- tl_state_read()
  if (nzchar(s$atual)) .libPaths(c(tl_lib_dir(s$atual), .Library))

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

#' Confere se já há um launcher do trama respondendo em `porta`: abre uma
#' conexão HTTP simples e procura a marca `class="tl-app"` do
#' `inst/www/launcher.html` na resposta — não basta a porta estar ocupada
#' (poderia ser qualquer outro serviço), tem que ser especificamente esta
#' página.
#' @noRd
.tl_launcher_rodando <- function(porta) {
  con <- tryCatch(
    suppressWarnings(url(sprintf("http://127.0.0.1:%d", porta), open = "rb")),
    error = function(e) NULL, warning = function(w) NULL
  )
  if (is.null(con)) return(FALSE)
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  corpo <- tryCatch(
    suppressWarnings(paste(readLines(con, warn = FALSE, n = 200), collapse = "\n")),
    error = function(e) ""
  )
  grepl('class="tl-app"', corpo, fixed = TRUE)
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
