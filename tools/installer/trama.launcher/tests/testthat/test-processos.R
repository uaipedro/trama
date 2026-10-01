# Vida dos processos sem terminal: logs, crash do editor, Sair e
# encerramento automático (R/processos.R).

local_home <- function(env = parent.frame()) {
  home <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(c(TRAMA_HOME = home), .local_envir = env)
  withr::defer({
    for (e in list(.tl_processos_projeto, .tl_pids_projeto, .tl_vistos, .tl_encerra_sozinho)) rm(list = ls(e), envir = e)
  }, envir = env)
  home
}

registrar <- function(caminho, pid = TRUE) {
  assign(caminho, 9999L, envir = .tl_processos_projeto)
  pid_file <- .tl_pid_file(caminho)
  dir.create(dirname(pid_file), recursive = TRUE, showWarnings = FALSE)
  if (pid) writeLines("123", pid_file)
  assign(caminho, pid_file, envir = .tl_pids_projeto)
}

test_that("o log do processo recebe stdout e stderr e o anterior é guardado", {
  local_home()
  arq <- .tl_log_arquivo("editor")
  dir.create(dirname(arq), recursive = TRUE)
  writeLines("rodada velha", arq)
  .tl_log_girar(arq)
  expect_equal(readLines(sub("\\.log$", ".anterior.log", arq)), "rodada velha")

  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(.tl_log_sink_codigo(arq), "cat('saida\\n')", "message('mensagem')", "stop('quebrou')"), script)
  suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)),
                           stdout = FALSE, stderr = FALSE))
  log <- readLines(arq)
  expect_true(any(grepl("saida", log)))
  expect_true(any(grepl("mensagem", log)))
  expect_true(any(grepl("quebrou", log)))
})

test_that("tl_log_ler só lê arquivo que está na lista, nunca caminho", {
  local_home()
  dir.create(tl_log_dir())
  writeLines(c("a", "b", "c"), .tl_log_arquivo("launcher"))
  expect_equal(tl_log_ler("launcher.log", n = 2), c("b", "c"))
  expect_equal(tl_log_ler("../estado.json"), character(0))
  expect_equal(tl_log_ler(NULL), character(0))
})

test_that("editor que responde e depois some: crash só se o processo morreu com PID sobrando", {
  local_home()
  registrar("/p/caiu"); registrar("/p/fechou")
  aberto <- function(p) TRUE
  expect_equal(tl_projects_verificar(aberto, function(pid) TRUE), character(0))

  # /p/fechou terminou normal: apagou o PID.
  unlink(.tl_pid_file("/p/fechou"))
  aberto <- function(p) FALSE
  # Ainda vivo sem responder (encerrando): espera, não é crash.
  expect_equal(tl_projects_verificar(aberto, function(pid) TRUE), character(0))
  expect_setequal(ls(.tl_processos_projeto), "/p/caiu")
  # Processo morto e PID sobrando: crash.
  expect_equal(tl_projects_verificar(aberto, function(pid) FALSE), "/p/caiu")
  expect_length(ls(.tl_processos_projeto), 0)
  expect_false(file.exists(.tl_pid_file("/p/caiu")))
})

test_that("editor que nunca respondeu: subindo enquanto vive, crash quando morre", {
  local_home()
  registrar("/p/subindo"); registrar("/p/sem-pid", pid = FALSE)
  aberto <- function(p) FALSE
  expect_equal(tl_projects_verificar(aberto, function(pid) TRUE), character(0))
  expect_setequal(ls(.tl_processos_projeto), c("/p/subindo", "/p/sem-pid"))
  expect_equal(tl_projects_verificar(aberto, function(pid) FALSE), "/p/subindo")
})

test_that("reiniciar esquece o 'já respondeu' do editor anterior", {
  local_home()
  registrar("/p/a")
  tl_projects_verificar(function(p) TRUE, function(pid) TRUE)
  expect_true(exists("/p/a", envir = .tl_vistos))
  .tl_esquecer_projeto("/p/a")
  expect_false(exists("/p/a", envir = .tl_vistos))
})

test_that("só editor que encerra sozinho segura o launcher", {
  local_home()
  registrar("/p/velho"); registrar("/p/novo")
  assign("/p/novo", TRUE, envir = .tl_encerra_sozinho)
  expect_true(.tl_algum_editor(function(p) TRUE))
  expect_false(.tl_algum_editor(function(p) p == "/p/velho"))
  expect_false(.tl_trama_encerra_sozinho(withr::local_tempdir()))
})

test_that("o token da tela de início é estável na execução e confere", {
  expect_match(.tl_token(), "^[0-9a-f]{48}$")
  expect_identical(.tl_token(), .tl_token())
  expect_true(.tl_token_ok(.tl_token()))
  expect_false(.tl_token_ok(NULL))
  expect_false(.tl_token_ok("chute"))
})

test_that("tl_projects_encerrar mata cada editor e limpa o registro", {
  local_home()
  registrar("/p/a"); registrar("/p/b")
  mortos <- integer(0)
  tl_projects_encerrar(matar = function(pid) mortos <<- c(mortos, pid))
  expect_equal(mortos, c(123L, 123L))
  expect_length(ls(.tl_processos_projeto), 0)
  expect_false(file.exists(.tl_pid_file("/p/a")))
})

test_that("launcher encerra sozinho só sem janela e sem editor", {
  paradas <- 0L
  editor <- TRUE
  srv <- .tl_encerrar_sozinho(function(input, output, session) NULL, intervalo = 0,
                              parar = function() paradas <<- paradas + 1L,
                              editor = function() editor)
  fim <- NULL
  srv(NULL, NULL, list(onSessionEnded = function(f) fim <<- f))
  fim()
  later::run_now(0.1)
  expect_equal(paradas, 0L)
  editor <- FALSE
  later::run_now(0.1)
  expect_equal(paradas, 1L)
})

test_that("recarregar a página não encerra o launcher", {
  paradas <- 0L
  srv <- .tl_encerrar_sozinho(function(input, output, session) NULL, intervalo = 0,
                              parar = function() paradas <<- paradas + 1L,
                              editor = function() FALSE)
  fim <- NULL
  srv(NULL, NULL, list(onSessionEnded = function(f) fim <<- f))
  fim()
  srv(NULL, NULL, list(onSessionEnded = function(f) NULL))
  later::run_now(0.1)
  expect_equal(paradas, 0L)
})
