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

test_that("ação Atualizar instala a nova release e atualiza o status", {
  local_home()
  testthat::local_mocked_bindings(
    tl_install_pkgs = fake_install_ok,
    tl_manifest_fetch = manifesto_teste
  )
  tl_install_release(manifesto_teste("2026.09"), colecoes = character(0))

  shiny::testServer(tl_server, {
    expect_equal(status()$release_instalada, "2026.09")
    session$setInputs(tl_atualizar = 1)
    expect_equal(status()$release_instalada, "2026.10")
    expect_false(status()$atualizar)
  })

  s <- tl_state_read()
  expect_equal(s$atual, "2026.10")
})

test_that("sem release nenhuma, o server dispara a primeira instalação sozinho", {
  local_home()
  testthat::local_mocked_bindings(
    tl_install_pkgs = fake_install_ok,
    tl_manifest_fetch = manifesto_teste
  )

  shiny::testServer(tl_server, {
    # Já instalado ao entrar na sessão, sem precisar de nenhum input —
    # abrir() sobe a tela vazia e é o server quem dispara a instalação
    # (revisão da fase 2: instalar antes do runApp() não tinha UI para
    # mostrar progresso nem erro).
    expect_equal(status()$release_instalada, "2026.10")
  })

  s <- tl_state_read()
  expect_equal(s$atual, "2026.10")
})

test_that("sem release e sem internet, mostra aviso com botão de tentar de novo", {
  local_home()
  testthat::local_mocked_bindings(tl_manifest_fetch = function(...) NULL)

  shiny::testServer(tl_server, {
    expect_true(sem_internet())
    expect_equal(status()$release_instalada, "")
  })
})

test_that("Tentar de novo reinstala quando a internet volta", {
  local_home()
  chamadas <- 0
  fetch_intermitente <- function(...) {
    chamadas <<- chamadas + 1
    if (chamadas == 1) NULL else manifesto_teste()
  }
  testthat::local_mocked_bindings(
    tl_install_pkgs = fake_install_ok,
    tl_manifest_fetch = fetch_intermitente
  )

  shiny::testServer(tl_server, {
    expect_true(sem_internet())
    session$setInputs(tl_tentar_instalar = 1)
    expect_false(sem_internet())
    expect_equal(status()$release_instalada, "2026.10")
  })
})

test_that("duplo clique fora do ciclo reativo é barrado por rodar_acao", {
  local_home()
  testthat::local_mocked_bindings(
    tl_install_pkgs = fake_install_ok,
    tl_manifest_fetch = manifesto_teste
  )
  tl_install_release(manifesto_teste("2026.09"), colecoes = character(0))

  shiny::testServer(tl_server, {
    # Chama rodar_acao() de dentro da ação de outra rodar_acao() — simula
    # dois cliques que chegam antes do botão ser desabilitado na tela
    # (a `ocupado()` reactiveVal só reflete na UI na próxima repintura; o
    # flag síncrono do closure é o que barra isso de verdade). Se a trava
    # falhar, a ação de dentro roda e o contador muda.
    chamadas_internas <- 0
    rodar_acao("Instalando…", function(progresso) {
      rodar_acao("Instalando de novo…", function(p2) chamadas_internas <<- chamadas_internas + 1)
      tl_install_release(manifesto(), progresso = progresso)
    })

    expect_equal(chamadas_internas, 0)
  })
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

local_home_e_projetos <- function(env = parent.frame()) {
  home <- local_home(env)
  projetos <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(c(TRAMA_PROJECTS = projetos), .local_envir = env)
  projetos
}

test_that("Novo projeto instala coleções pedidas que faltam e grava trama.json", {
  base <- local_home_e_projetos()
  testthat::local_mocked_bindings(
    tl_install_pkgs = fake_install_ok,
    tl_manifest_fetch = manifesto_teste
  )
  tl_install_release(manifesto_teste(), colecoes = character(0))

  shiny::testServer(tl_server, {
    session$setInputs(
      tl_novo_projeto_nome = "meu-fluxo",
      tl_novo_projeto_colecoes = list("trama.data", "trama.ml")
    )
    session$setInputs(tl_novo_projeto = 1)
  })

  caminho <- file.path(base, "meu-fluxo")
  expect_true(dir.exists(caminho))
  expect_equal(tl_project_collections(caminho), c("trama.data", "trama.ml"))
  expect_true(dir.exists(file.path(tl_lib_dir("2026.10"), "trama.ml")))
})

test_that("abrir projeto que pede coleção não instalada manda a UI perguntar, sem abrir", {
  base <- local_home_e_projetos()
  testthat::local_mocked_bindings(
    tl_install_pkgs = fake_install_ok,
    tl_manifest_fetch = manifesto_teste
  )
  tl_install_release(manifesto_teste(), colecoes = character(0))
  caminho <- tl_project_new("com-ml", colecoes = c("trama.data", "trama.ml"))

  # tl_project_open não roda porque a checagem de coleção faltando barra
  # antes: confirmado indiretamente pelo estado de "aberto" continuar
  # falso (sem porta registrada) depois do input.
  shiny::testServer(tl_server, {
    session$setInputs(tl_abrir_projeto = caminho)
  })
  expect_false(tl_project_aberto(caminho))
})

test_that("Instalar e abrir instala a coleção faltante e abre o projeto", {
  base <- local_home_e_projetos()
  aberto_com <- NULL
  testthat::local_mocked_bindings(
    tl_install_pkgs = fake_install_ok,
    tl_manifest_fetch = manifesto_teste,
    # tl_project_open() de verdade subiria um Rscript e um navegador de
    # verdade (efeito colateral indesejável num teste) — mockado, como o
    # resto do motor de instalação nestes testes.
    tl_project_open = function(caminho, ...) {
      aberto_com <<- caminho
      invisible(9999L)
    }
  )
  tl_install_release(manifesto_teste(), colecoes = character(0))
  caminho <- tl_project_new("abrir-com-ml", colecoes = c("trama.data", "trama.ml"))

  shiny::testServer(tl_server, {
    session$setInputs(tl_instalar_e_abrir_projeto = caminho)
  })

  expect_true(dir.exists(file.path(tl_lib_dir("2026.10"), "trama.ml")))
  expect_equal(aberto_com, caminho)
})

test_that("Salvar coleções de um projeto grava trama.json e instala o que falta", {
  base <- local_home_e_projetos()
  testthat::local_mocked_bindings(
    tl_install_pkgs = fake_install_ok,
    tl_manifest_fetch = manifesto_teste
  )
  tl_install_release(manifesto_teste(), colecoes = character(0))
  caminho <- tl_project_new("editar")

  shiny::testServer(tl_server, {
    session$setInputs(
      tl_salvar_colecoes_projeto = list(caminho = caminho, colecoes = list("trama.data", "trama.ml"))
    )
  })

  expect_equal(tl_project_collections(caminho), c("trama.data", "trama.ml"))
  expect_true(dir.exists(file.path(tl_lib_dir("2026.10"), "trama.ml")))
})
