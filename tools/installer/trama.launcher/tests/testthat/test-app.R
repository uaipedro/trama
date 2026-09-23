# tl_server() é testado via shiny::testServer, sem subir o Shiny de
# verdade — só que as ações (atualizar, instalar/remover coleção, reparar,
# voltar versão) de fato chamam o motor (R/install.R) e o estado muda. O
# visual (R/app.R:tl_ui(), inst/www/launcher.css) não é testado aqui.

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

fake_install_ok <- function(pkgs, lib, repos) {
  for (p in pkgs) {
    dir.create(file.path(lib, p), recursive = TRUE, showWarnings = FALSE)
    writeLines(c(sprintf("Package: %s", p), "Version: 1.0.0"), file.path(lib, p, "DESCRIPTION"))
  }
}

test_that("ação Atualizar instala a release e atualiza o status", {
  local_home()
  testthat::local_mocked_bindings(
    tl_install_pkgs = fake_install_ok,
    tl_manifest_fetch = manifesto_teste
  )

  shiny::testServer(tl_server, {
    expect_equal(status()$release_instalada, "")
    session$setInputs(tl_atualizar = 1)
    expect_equal(status()$release_instalada, "2026.10")
    expect_false(status()$atualizar)
  })

  s <- tl_state_read()
  expect_equal(s$atual, "2026.10")
})

test_that("ação Instalar coleção chama o motor e reflete no status", {
  local_home()
  testthat::local_mocked_bindings(
    tl_install_pkgs = fake_install_ok,
    tl_manifest_fetch = manifesto_teste
  )
  tl_install_release(manifesto_teste(), colecoes = character(0))

  shiny::testServer(tl_server, {
    expect_false(status()$colecoes$instalada[status()$colecoes$nome == "trama.ml"])
    session$setInputs(tl_instalar_trama.ml = 1)
    expect_true(status()$colecoes$instalada[status()$colecoes$nome == "trama.ml"])
  })

  s <- tl_state_read()
  expect_equal(s$colecoes, "trama.ml")
})

test_that("ação Remover coleção chama o motor e reflete no status", {
  local_home()
  testthat::local_mocked_bindings(
    tl_install_pkgs = fake_install_ok,
    tl_remove_pkgs = function(pkgs, lib) unlink(file.path(lib, pkgs), recursive = TRUE),
    tl_manifest_fetch = manifesto_teste
  )
  tl_install_release(manifesto_teste(), colecoes = "trama.ml")

  shiny::testServer(tl_server, {
    expect_true(status()$colecoes$instalada[status()$colecoes$nome == "trama.ml"])
    session$setInputs(tl_remover_trama.ml = 1)
    expect_false(status()$colecoes$instalada[status()$colecoes$nome == "trama.ml"])
  })

  s <- tl_state_read()
  expect_equal(s$colecoes, character(0))
})

test_that("ação Voltar versão chama tl_rollback()", {
  local_home()
  testthat::local_mocked_bindings(
    tl_install_pkgs = fake_install_ok,
    tl_manifest_fetch = manifesto_teste
  )
  tl_install_release(manifesto_teste("2026.09"), colecoes = character(0))
  tl_install_release(manifesto_teste("2026.10"), colecoes = character(0))

  shiny::testServer(tl_server, {
    session$setInputs(tl_voltar_versao = 1)
    expect_equal(status()$release_instalada, "2026.09")
  })

  s <- tl_state_read()
  expect_equal(s$atual, "2026.09")
})

test_that("ação Reparar reinstala a release atual mantendo as coleções", {
  local_home()
  testthat::local_mocked_bindings(
    tl_install_pkgs = fake_install_ok,
    tl_manifest_fetch = manifesto_teste
  )
  tl_install_release(manifesto_teste(), colecoes = "trama.ml")

  shiny::testServer(tl_server, {
    session$setInputs(tl_reparar = 1)
    expect_false(is.na(status()$pacotes$instalada[status()$pacotes$nome == "trama"]))
  })

  s <- tl_state_read()
  expect_equal(s$atual, "2026.10")
  expect_equal(s$colecoes, "trama.ml")
})

test_that("falha numa ação mostra notificação de erro e não derruba a sessão", {
  local_home()
  testthat::local_mocked_bindings(
    tl_install_pkgs = function(pkgs, lib, repos) stop("rede caiu"),
    tl_manifest_fetch = manifesto_teste
  )

  shiny::testServer(tl_server, {
    session$setInputs(tl_atualizar = 1)
    expect_equal(status()$release_instalada, "")
  })
})
