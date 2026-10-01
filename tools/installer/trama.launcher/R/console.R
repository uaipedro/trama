#' Console de manutenção: cada comando roda num `Rscript` novo, com a
#' biblioteca da release atual na frente do `.libPaths()` — é para instalar
#' pacote, conferir versão, esse tipo de coisa. Nada sobrevive entre um
#' comando e outro. Roda em segundo plano (`wait = FALSE`) para um
#' `install.packages()` demorado não travar a tela do launcher.
#' @noRd
NULL

#' Script que o `Rscript` do console executa: avalia o código do usuário
#' expressão por expressão, imprimindo o que for visível (como no console do
#' R), com saída e erros no mesmo arquivo e o status gravado ao fim.
#' @noRd
.tl_console_script <- function(codigo, saida, status, lib, pid) {
  c(
    sprintf("writeLines(as.character(Sys.getpid()), %s)", deparse(pid)),
    sprintf(".libPaths(c(%s, .Library))", deparse(lib)),
    "options(repos = c(CRAN = 'https://cloud.r-project.org'), warn = 1)",
    sprintf(".tl_saida <- file(%s, open = 'wt')", deparse(saida)),
    "sink(.tl_saida); sink(.tl_saida, type = 'message')",
    # `q()` no código do usuário pula o fim do script: o finalizador ainda
    # grava o status, senão a tela esperaria para sempre.
    sprintf(".tl_fim <- function(s) if (!file.exists(%s)) writeLines(s, %s)", deparse(status), deparse(status)),
    "invisible(reg.finalizer(.tl_guarda <- new.env(), function(e) .tl_fim('ok'), onexit = TRUE))",
    ".tl_ok <- tryCatch({",
    sprintf("  for (.tl_e in parse(%s, keep.source = FALSE, encoding = 'UTF-8')) {", deparse(codigo)),
    "    .tl_r <- withVisible(eval(.tl_e, globalenv()))",
    "    if (.tl_r$visible) print(.tl_r$value)",
    "  }",
    "  TRUE",
    "}, error = function(e) { message('Erro: ', conditionMessage(e)); FALSE })",
    "sink(type = 'message'); sink(); close(.tl_saida)",
    ".tl_fim(if (.tl_ok) 'ok' else 'erro')"
  )
}

#' Dispara `codigo` num `Rscript` em segundo plano.
#'
#' @param executar Wrapper em torno de `system2()`, para os testes.
#' @return Lista com `id`, `codigo` e os caminhos de `saida` e `status`.
#' @noRd
tl_console_rodar <- function(codigo, lib = tl_lib_dir(tl_state_read()$atual),
                             executar = system2) {
  id <- format(Sys.time(), "%Y%m%d-%H%M%OS3")
  dir <- file.path(tl_home(), "run", "console", id)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  job <- list(
    id = id, codigo = codigo,
    arquivo = file.path(dir, "codigo.R"),
    script = file.path(dir, "rodar.R"),
    saida = file.path(dir, "saida.txt"),
    status = file.path(dir, "status"),
    pid = file.path(dir, "pid")
  )
  writeLines(codigo, job$arquivo, useBytes = TRUE)
  writeLines(.tl_console_script(job$arquivo, job$saida, job$status, lib, job$pid), job$script)
  rscript <- file.path(R.home("bin"), "Rscript")
  executar(rscript, c("--vanilla", shQuote(job$script)), wait = FALSE)
  job
}

#' Estado de um comando do console: saída até aqui, se terminou e se deu certo.
#' @noRd
#' @param vivo Função que diz se um PID existe; parâmetro para os testes.
tl_console_ler <- function(job, vivo = .tl_pid_vivo) {
  # Status antes da saída: o status só é gravado depois que a saída fechou,
  # então com `fim` verdadeiro a leitura seguinte já vem completa.
  fim <- file.exists(job$status)
  saida <- if (file.exists(job$saida)) readLines(job$saida, warn = FALSE, encoding = "UTF-8") else character(0)
  status <- if (fim) readLines(job$status, n = 1, warn = FALSE) else NA_character_
  # O R do comando morreu sem gravar status (morto por fora, segfault): sem
  # isto o console esperaria para sempre.
  if (!fim && file.exists(job$pid)) {
    pid <- suppressWarnings(as.integer(readLines(job$pid, n = 1, warn = FALSE)))
    if (length(pid) == 1 && !is.na(pid) && !isTRUE(vivo(pid))) {
      fim <- TRUE
      saida <- c(saida, "Erro: o R deste comando terminou sem responder.")
    }
  }
  list(id = job$id, saida = saida, fim = fim, ok = identical(status, "ok"))
}

#' Acrescenta um comando terminado a `console.log`, que aparece na aba Logs,
#' e apaga a pasta temporária dele.
#' @noRd
tl_console_registrar <- function(job, estado) {
  arq <- .tl_log_arquivo("console")
  dir.create(dirname(arq), recursive = TRUE, showWarnings = FALSE)
  cat(
    sprintf("## %s (%s)", format(Sys.time()), if (estado$ok) "ok" else "erro"),
    paste0("> ", strsplit(job$codigo, "\n", fixed = TRUE)[[1]]),
    estado$saida, "",
    file = arq, sep = "\n", append = TRUE
  )
  unlink(dirname(job$saida), recursive = TRUE)
  invisible(NULL)
}
