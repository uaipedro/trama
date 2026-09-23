# O manifesto é o contrato público entre a release publicada, o instalador
# nativo e o launcher: qualquer campo faltando ou malformado tem que falhar
# alto e em português, nunca silenciar num NA que só aparece três passos
# adiante.

fixture <- function(nome = "release-ok.json") test_path("fixtures", nome)

test_that("lê e valida uma fixture correta", {
  m <- tl_manifest_read(fixture())

  expect_equal(m$trama, "2026.10")
  expect_equal(m$r, "4.5.1")
  expect_equal(m$cran_snapshot, "2026-10-01")
  expect_equal(m$core$trama, "0.1.0")
  expect_equal(m$collections$trama.ml$title, "Aprendizado de máquina")
})

test_that("chave obrigatória ausente dá erro claro", {
  bruto <- jsonlite::fromJSON(fixture(), simplifyVector = FALSE)
  bruto$core <- NULL
  caminho <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(bruto, auto_unbox = TRUE), caminho)

  expect_error(tl_manifest_read(caminho), class = "tl_error_manifesto")
  expect_error(tl_manifest_read(caminho), regexp = "core")
})

test_that("cran_snapshot que não é data ISO dá erro", {
  bruto <- jsonlite::fromJSON(fixture(), simplifyVector = FALSE)
  bruto$cran_snapshot <- "01/10/2026"
  caminho <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(bruto, auto_unbox = TRUE), caminho)

  expect_error(tl_manifest_read(caminho), class = "tl_error_manifesto")
})

test_that("versão do r fora do formato x.y.z dá erro", {
  bruto <- jsonlite::fromJSON(fixture(), simplifyVector = FALSE)
  bruto$r <- "4.5"
  caminho <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(bruto, auto_unbox = TRUE), caminho)

  expect_error(tl_manifest_read(caminho), class = "tl_error_manifesto")
})

test_that("tl_manifest_fetch devolve NULL e registra a mensagem quando offline", {
  # NULL não aceita attr() em R (attr(x, "erro") <- ... em NULL é erro), e
  # is.null() é o teste que tl_status() usa para "sigo sem checar
  # atualização" — por isso a mensagem fica em tl_manifest_fetch_erro(),
  # não num atributo do retorno.
  m <- tl_manifest_fetch("http://127.0.0.1:9/x")

  expect_null(m)
  expect_true(nzchar(tl_manifest_fetch_erro()))
})

test_that("tl_manifest_fetch devolve o manifesto quando dá certo", {
  m <- tl_manifest_fetch(fixture())
  expect_equal(m$trama, "2026.10")
})

test_that("tl_repos monta a URL do P3M para Windows", {
  testthat::local_mocked_bindings(tl_os = function() list(tipo = "windows"))
  m <- tl_manifest_read(fixture())

  repos <- tl_repos(m)

  expect_equal(unname(repos["P3M"]), "https://packagemanager.posit.co/cran/2026-10-01")
  expect_equal(unname(repos[1]), "https://uaipedro.r-universe.dev")
})

test_that("tl_repos monta a URL do P3M para Linux com codename", {
  testthat::local_mocked_bindings(
    tl_os = function() list(tipo = "unix", codename = "jammy")
  )
  m <- tl_manifest_read(fixture())

  repos <- tl_repos(m)

  expect_equal(
    unname(repos["P3M"]),
    "https://packagemanager.posit.co/cran/__linux__/jammy/2026-10-01"
  )
})

test_that("tl_repos sem codename cai na URL genérica do P3M", {
  testthat::local_mocked_bindings(
    tl_os = function() list(tipo = "unix", codename = NA_character_)
  )
  m <- tl_manifest_read(fixture())

  repos <- tl_repos(m)

  expect_equal(unname(repos["P3M"]), "https://packagemanager.posit.co/cran/2026-10-01")
})
