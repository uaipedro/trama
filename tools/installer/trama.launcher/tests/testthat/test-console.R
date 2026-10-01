# Console de manutenção (R/console.R): roda um Rscript de verdade, curto.

rodar_e_esperar <- function(codigo, lib) {
  job <- tl_console_rodar(codigo, lib = lib)
  for (i in 1:100) {
    estado <- tl_console_ler(job)
    if (estado$fim) break
    Sys.sleep(0.1)
  }
  list(job = job, estado = tl_console_ler(job))
}

test_that("o console imprime o visível, usa a lib da release e marca erro", {
  home <- withr::local_tempdir()
  withr::local_envvar(c(TRAMA_HOME = home))
  lib <- file.path(home, "lib", "r1")
  dir.create(lib, recursive = TRUE)

  r <- rodar_e_esperar("x <- 2\nx * 21\ninvisible(3)\n.libPaths()[1]", lib)
  expect_true(r$estado$fim)
  expect_true(r$estado$ok)
  expect_true(any(grepl("42", r$estado$saida)))
  expect_false(any(grepl("\\b3\\b", r$estado$saida)))
  expect_true(any(grepl(basename(lib), r$estado$saida, fixed = TRUE)))

  r <- rodar_e_esperar("warning('cuidado'); stop('deu ruim')", lib)
  expect_false(r$estado$ok)
  expect_true(any(grepl("cuidado", r$estado$saida)))
  expect_true(any(grepl("Erro: deu ruim", r$estado$saida, fixed = TRUE)))

  tl_console_registrar(r$job, r$estado)
  log <- readLines(.tl_log_arquivo("console"))
  expect_true(any(grepl("(erro)", log, fixed = TRUE)))
  expect_true(any(grepl("> warning('cuidado')", log, fixed = TRUE)))
  expect_false(dir.exists(dirname(r$job$saida)))
})

test_that("q() no código não deixa o console esperando para sempre", {
  home <- withr::local_tempdir()
  withr::local_envvar(c(TRAMA_HOME = home))
  r <- rodar_e_esperar("q('no')", home)
  expect_true(r$estado$fim)
})

test_that("comando cujo R morreu sem status termina com erro", {
  home <- withr::local_tempdir()
  job <- list(id = "x", saida = file.path(home, "saida.txt"), status = file.path(home, "status"),
              pid = file.path(home, "pid"))
  writeLines("parcial", job$saida)
  writeLines("424242", job$pid)
  expect_false(tl_console_ler(job, vivo = function(pid) TRUE)$fim)
  e <- tl_console_ler(job, vivo = function(pid) FALSE)
  expect_true(e$fim)
  expect_false(e$ok)
  expect_match(e$saida[2], "terminou sem responder")
})
