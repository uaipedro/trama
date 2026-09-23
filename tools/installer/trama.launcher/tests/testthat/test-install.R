# tl_install_pkgs() é mockado em todo teste aqui: o install.packages() real
# só entra no CI (fase 4). O que se testa é o caminho crítico ao redor dele —
# estado só muda em sucesso completo, falha parcial não deixa lib pela
# metade nem mexe no estado, e rotação/rollback preservam o histórico certo.

local_home <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(c(TRAMA_HOME = dir), .local_envir = env)
  dir
}

manifesto_teste <- function(trama = "2026.10") {
  list(
    trama = trama, r = as.character(getRversion()), cran_snapshot = "2026-10-01",
    repos = list("https://uaipedro.r-universe.dev"),
    core = list(trama = "0.1.0", trama.data = "0.1.0"),
    collections = list(trama.ml = list(version = "0.1.0", title = "Aprendizado de máquina", requires = list()))
  )
}

# Fabrica pacotes fake: cria lib/<pkg>/DESCRIPTION, que é o que
# `.tl_versao_instalada()`/a checagem de "instalou mesmo" enxergam.
fake_install_ok <- function(pkgs, lib, repos) {
  for (p in pkgs) {
    dir.create(file.path(lib, p), recursive = TRUE, showWarnings = FALSE)
    writeLines(c(sprintf("Package: %s", p), "Version: 1.0.0"), file.path(lib, p, "DESCRIPTION"))
  }
}

# Instala tudo, menos os pacotes em `falhar` — simula falha parcial do
# install.packages() real (rede caiu no meio, pacote não existe no P3M etc.).
fake_install_parcial <- function(falhar) {
  force(falhar)
  function(pkgs, lib, repos) fake_install_ok(setdiff(pkgs, falhar), lib, repos)
}

test_that("instalação de release com sucesso atualiza o estado", {
  local_home()
  testthat::local_mocked_bindings(tl_install_pkgs = fake_install_ok)
  m <- manifesto_teste()

  tl_install_release(m, colecoes = character(0))

  s <- tl_state_read()
  expect_equal(s$atual, "2026.10")
  expect_equal(s$anteriores, character(0))
  expect_true(dir.exists(file.path(tl_lib_dir("2026.10"), "trama")))
})

test_that("falha parcial preserva o estado e apaga a lib nova", {
  local_home()
  tl_state_write(list(atual = "2026.09", anteriores = character(0), colecoes = character(0)))
  testthat::local_mocked_bindings(
    tl_install_pkgs = fake_install_parcial("trama.data")
  )
  m <- manifesto_teste("2026.10")

  expect_error(tl_install_release(m, colecoes = character(0)), class = "tl_error_instalacao")

  s <- tl_state_read()
  expect_equal(s$atual, "2026.09")
  expect_false(dir.exists(tl_lib_dir("2026.10")))
})

test_that("instalação grava log em tl_log_dir()", {
  local_home()
  testthat::local_mocked_bindings(tl_install_pkgs = fake_install_ok)
  m <- manifesto_teste()

  tl_install_release(m, colecoes = character(0))

  logs <- list.files(tl_log_dir(), pattern = "^instalacao-.*\\.log$")
  expect_equal(length(logs), 1)
})

test_that("rotação de releases mantém no máximo 2 anteriores e apaga as libs mais antigas", {
  local_home()
  testthat::local_mocked_bindings(tl_install_pkgs = fake_install_ok)

  for (rel in c("2026.08", "2026.09", "2026.10", "2026.11")) {
    tl_install_release(manifesto_teste(rel), colecoes = character(0))
  }

  s <- tl_state_read()
  expect_equal(s$atual, "2026.11")
  expect_equal(s$anteriores, c("2026.10", "2026.09"))
  expect_false(dir.exists(tl_lib_dir("2026.08")))
  expect_true(dir.exists(tl_lib_dir("2026.09")))
  expect_true(dir.exists(tl_lib_dir("2026.10")))
})

test_that("rollback volta para a release anterior", {
  local_home()
  testthat::local_mocked_bindings(tl_install_pkgs = fake_install_ok)
  tl_install_release(manifesto_teste("2026.09"), colecoes = character(0))
  tl_install_release(manifesto_teste("2026.10"), colecoes = character(0))

  tl_rollback()

  s <- tl_state_read()
  expect_equal(s$atual, "2026.09")
  expect_equal(s$anteriores, "2026.10")
})

test_that("rollback sem release anterior dá erro claro", {
  local_home()
  tl_state_write(list(atual = "2026.10", anteriores = character(0), colecoes = character(0)))

  expect_error(tl_rollback(), class = "tl_error_instalacao")
})

test_that("coleção fora do manifesto é recusada", {
  local_home()
  testthat::local_mocked_bindings(tl_install_pkgs = fake_install_ok)
  m <- manifesto_teste()
  tl_install_release(m, colecoes = character(0))

  expect_error(tl_collection_add(m, "coleção-inexistente"), class = "tl_error_instalacao")

  s <- tl_state_read()
  expect_equal(s$colecoes, character(0))
})

test_that("tl_collection_add instala e registra a coleção no estado", {
  local_home()
  testthat::local_mocked_bindings(tl_install_pkgs = fake_install_ok)
  m <- manifesto_teste()
  tl_install_release(m, colecoes = character(0))

  tl_collection_add(m, "trama.ml")

  s <- tl_state_read()
  expect_equal(s$colecoes, "trama.ml")
  expect_true(dir.exists(file.path(tl_lib_dir("2026.10"), "trama.ml")))
})

test_that("tl_collection_remove desinstala e tira do estado", {
  local_home()
  testthat::local_mocked_bindings(
    tl_install_pkgs = fake_install_ok,
    tl_remove_pkgs = function(pkgs, lib) unlink(file.path(lib, pkgs), recursive = TRUE)
  )
  m <- manifesto_teste()
  tl_install_release(m, colecoes = "trama.ml")

  tl_collection_remove("trama.ml")

  s <- tl_state_read()
  expect_equal(s$colecoes, character(0))
  expect_false(dir.exists(file.path(tl_lib_dir("2026.10"), "trama.ml")))
})
