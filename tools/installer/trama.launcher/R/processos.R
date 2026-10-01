#' Vida dos processos sem terminal: logs, aviso de crash do editor, Sair e
#' encerramento automático do launcher. O launcher roda minimizado (Windows)
#' ou sem terminal (Linux), então é por aqui que o pesquisador vê o que
#' aconteceu e é daqui que o R é encerrado.
#' @noRd
NULL

# --- Logs ---------------------------------------------------------------

#' Log de um processo, em `tl_log_dir()`: `launcher.log` ou `<slug>.log`.
#' @noRd
.tl_log_arquivo <- function(nome) file.path(tl_log_dir(), paste0(nome, ".log"))

#' Log do editor de `caminho`.
#' @noRd
tl_project_log <- function(caminho) .tl_log_arquivo(.tl_slug(normalizePath(caminho, mustWork = FALSE)))

#' Guarda a execução anterior em `<nome>.anterior.log` antes de começar outra:
#' cada processo escreve do zero, e o log do crash de ontem sobrevive a uma
#' reabertura.
#' @noRd
.tl_log_girar <- function(arquivo) {
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(arquivo)) {
    file.rename(arquivo, sub("\\.log$", ".anterior.log", arquivo))
  }
  arquivo
}

#' Código R que desvia stdout e stderr do processo para `arquivo`. Entra no
#' começo do `-e` do editor e em `abrir()`: redirecionar dentro do próprio R
#' funciona igual nos dois SOs, sem depender do `system2(stdout =)` com
#' `wait = FALSE`.
#' @noRd
.tl_log_sink_codigo <- function(arquivo) {
  sprintf(
    paste0(
      ".tl_log <- file(%s, open = \"wt\"); sink(.tl_log); sink(.tl_log, type = \"message\"); ",
      "options(warn = 1); cat(format(Sys.time()), \"- inicio, pid\", Sys.getpid(), \"\\n\")"
    ),
    deparse(arquivo)
  )
}

#' Arquivos de log disponíveis, do mais recente para o mais antigo.
#' @noRd
tl_logs_listar <- function() {
  arqs <- list.files(tl_log_dir(), pattern = "\\.log$", full.names = TRUE)
  if (!length(arqs)) return(character(0))
  basename(arqs[order(file.mtime(arqs), decreasing = TRUE)])
}

#' Últimas `n` linhas de um log. `nome` vem do cliente: só aceita um que
#' `tl_logs_listar()` conhece, nunca um caminho.
#' @noRd
tl_log_ler <- function(nome, n = 400L) {
  if (is.null(nome) || !(nome %in% tl_logs_listar())) return(character(0))
  linhas <- readLines(file.path(tl_log_dir(), nome), warn = FALSE, encoding = "UTF-8")
  utils::tail(linhas, n)
}

# --- Crash do editor ------------------------------------------------------

#' Projetos cujo editor já respondeu ao menos uma vez nesta sessão. Um editor
#' ainda subindo não responde e não pode contar como crash.
#' @noRd
.tl_vistos <- new.env(parent = emptyenv())

#' O processo `pid` existe? No Windows, `tools::pskill()` sempre mata (não
#' há sinal 0), então pergunta ao `tasklist`.
#' @noRd
.tl_pid_vivo <- function(pid) {
  if (is.na(pid)) return(FALSE)
  if (.Platform$OS.type == "windows") {
    saida <- suppressWarnings(system2("tasklist", c("/FI", shQuote(sprintf("PID eq %d", pid)), "/NH"),
                                      stdout = TRUE, stderr = FALSE))
    return(any(grepl(sprintf("\\b%d\\b", pid), saida)))
  }
  isTRUE(tools::pskill(pid, 0L))
}

#' Confere os editores abertos por este launcher. Enquanto o processo existe,
#' um editor que não responde está subindo ou encerrando: espera. Processo
#' morto sai do registro; se o arquivo de PID ficou, ele não chegou ao fim
#' normal (quem termina normalmente apaga o próprio PID) e volta como crash —
#' inclusive a queda na subida (pacote quebrado, erro no projeto).
#'
#' @param aberto Função que diz se o editor responde; parâmetro para os testes.
#' @param vivo Função que diz se um PID existe; parâmetro para os testes.
#' @return Caminhos dos projetos que caíram.
#' @noRd
tl_projects_verificar <- function(aberto = tl_project_aberto, vivo = .tl_pid_vivo) {
  caiu <- character(0)
  for (caminho in ls(.tl_processos_projeto)) {
    if (isTRUE(aberto(caminho))) {
      assign(caminho, TRUE, envir = .tl_vistos)
      next
    }
    pid <- tl_project_pid(caminho)
    if (is.na(pid)) {
      # Sem PID: ainda não gravou (subindo) ou já apagou (fim normal).
      if (exists(caminho, envir = .tl_vistos, inherits = FALSE)) .tl_esquecer_projeto(caminho)
      next
    }
    if (isTRUE(vivo(pid))) next
    caiu <- c(caiu, caminho)
    unlink(.tl_pid_file(caminho))
    .tl_esquecer_projeto(caminho)
  }
  caiu
}

#' Tira `caminho` dos registros em memória do launcher.
#' @noRd
.tl_esquecer_projeto <- function(caminho) {
  for (env in list(.tl_processos_projeto, .tl_pids_projeto, .tl_vistos, .tl_encerra_sozinho)) {
    if (exists(caminho, envir = env, inherits = FALSE)) rm(list = caminho, envir = env)
  }
}

# --- Sair e encerramento automático --------------------------------------

#' Encerra os editores abertos por este launcher.
#' @param matar Função que mata um PID; parâmetro para os testes.
#' @noRd
tl_projects_encerrar <- function(matar = .tl_matar_pid) {
  for (caminho in ls(.tl_processos_projeto)) {
    matar(tl_project_pid(caminho))
    unlink(.tl_pid_file(caminho))
    .tl_esquecer_projeto(caminho)
  }
  invisible(NULL)
}

#' Editores que encerram sozinhos ao fechar a janela (trama >= 0.5.1, com
#' `trama.encerrar_ao_fechar`). Um de release mais antiga nunca encerra, e
#' esperar por ele deixaria o launcher de pé para sempre.
#' @noRd
.tl_encerra_sozinho <- new.env(parent = emptyenv())

#' Algum editor que vai encerrar sozinho ainda responde? Só esses seguram o
#' launcher aberto.
#' @noRd
.tl_algum_editor <- function(aberto = tl_project_aberto) {
  any(vapply(ls(.tl_encerra_sozinho), function(p) isTRUE(aberto(p)), logical(1)))
}

#' O `trama` instalado em `lib` já sabe encerrar sozinho?
#' @noRd
.tl_trama_encerra_sozinho <- function(lib) {
  v <- tryCatch(utils::packageVersion("trama", lib.loc = lib), error = function(e) NULL)
  !is.null(v) && v >= "0.5.1"
}

#' Embrulha o server do launcher para o processo encerrar sozinho: depois que
#' a última janela fecha, confere a cada `intervalo` segundos e para quando
#' não há janela do launcher nem editor de pé. Editor aberto segura o
#' launcher; quando ele fecha (o editor encerra sozinho, ver
#' `trama.encerrar_ao_fechar`), o launcher vai atrás. A primeira conferência
#' só vem depois de `intervalo`, o que cobre o recarregar da página.
#'
#' @param editor Função que diz se há editor de pé; parâmetro para os testes.
#' @noRd
.tl_encerrar_sozinho <- function(server, intervalo = 8, parar = shiny::stopApp,
                                 editor = .tl_algum_editor) {
  # Sem `force`, a promessa de `server` só é lida na primeira sessão, quando
  # o nome em quem chamou já aponta para este embrulho: recursão infinita.
  force(server)
  abertas <- 0L
  conferir <- function() {
    if (abertas > 0L) return(invisible(NULL))
    if (isTRUE(editor())) later::later(conferir, intervalo) else parar()
  }
  function(input, output, session) {
    abertas <<- abertas + 1L
    session$onSessionEnded(function() {
      abertas <<- abertas - 1L
      if (abertas == 0L) later::later(conferir, intervalo)
    })
    server(input, output, session)
  }
}
