# Projetos: listar (pasta padrão + recentes), criar, marcar como aberto e
# subir o processo do editor (mockado, igual a tl_install_pkgs em
# test-install.R: o Rscript de verdade só entra no CI).

local_home_e_projetos <- function(env = parent.frame()) {
  home <- withr::local_tempdir(.local_envir = env)
  projetos <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(c(TRAMA_HOME = home, TRAMA_PROJECTS = projetos), .local_envir = env)
  projetos
}

test_that("tl_projects_dir respeita TRAMA_PROJECTS", {
  dir <- local_home_e_projetos()
  expect_equal(tl_projects_dir(), normalizePath(dir, mustWork = FALSE))
})

test_that("tl_projects lista vazio quando não há nada", {
  local_home_e_projetos()
  pr <- tl_projects()
  expect_equal(nrow(pr), 0)
})

test_that("tl_projects lista subpastas da pasta padrão", {
  base <- local_home_e_projetos()
  dir.create(file.path(base, "meu-fluxo"))
  pr <- tl_projects()
  expect_equal(pr$nome, "meu-fluxo")
  expect_false(pr$aberto)
})

test_that("tl_project_new cria a pasta e recusa nome inválido ou repetido", {
  local_home_e_projetos()
  caminho <- tl_project_new("estudo 1")
  expect_true(dir.exists(caminho))

  expect_error(tl_project_new(""), regexp = "não é um nome de projeto válido")
  expect_error(tl_project_new("a/b"), regexp = "não é um nome de projeto válido")
  expect_error(tl_project_new("estudo 1"), regexp = "Já existe um projeto")
})

test_that("tl_project_open adiciona aos recentes e chama o motor de subir o editor", {
  local_home_e_projetos()
  caminho <- tl_project_new("estudo-2")
  chamadas <- list()

  porta <- tl_project_open(
    caminho,
    lib = tl_lib_dir("2026.10"),
    abrir_janela = function(url) chamadas[["janela"]] <<- url,
    executar = function(...) chamadas[["executar"]] <<- list(...)
  )

  expect_true(is.numeric(porta))
  expect_true(porta >= 8726L)
  expect_match(chamadas$janela, sprintf("127.0.0.1:%d", porta))
  expect_equal(tl_state_read()$recentes, normalizePath(caminho))
  expect_equal(tl_project_port(caminho), porta)
})

test_that("tl_projects lista o recente mesmo se estiver fora da pasta padrão", {
  local_home_e_projetos()
  fora <- withr::local_tempdir()
  tl_project_open(
    fora, lib = tl_lib_dir("2026.10"),
    abrir_janela = function(url) invisible(NULL), executar = function(...) invisible(NULL)
  )

  pr <- tl_projects()
  expect_true(normalizePath(fora) %in% pr$caminho)
})

test_that("tl_projects ignora recentes que sumiram do disco", {
  local_home_e_projetos()
  fantasma <- file.path(tempdir(), "projeto-que-sumiu")
  s <- tl_state_read()
  s$recentes <- fantasma
  tl_state_write(s)

  pr <- tl_projects()
  expect_false(fantasma %in% pr$caminho)
})

test_that("recentes fica limitado a 10, o mais novo primeiro", {
  local_home_e_projetos()
  for (i in 1:11) {
    caminho <- tl_project_new(sprintf("p%02d", i))
    tl_project_open(
      caminho, lib = tl_lib_dir("2026.10"),
      abrir_janela = function(url) invisible(NULL), executar = function(...) invisible(NULL)
    )
  }

  recentes <- tl_state_read()$recentes
  expect_equal(length(recentes), 10)
  expect_match(recentes[[1]], "p11$")
})

test_that("tl_project_aberto é FALSE para um projeto nunca aberto nesta sessão", {
  local_home_e_projetos()
  caminho <- tl_project_new("nunca-aberto")
  expect_false(tl_project_aberto(caminho))
  expect_true(is.na(tl_project_port(caminho)))
})
