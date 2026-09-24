# tl_status() é o que a tela de início lê inteira: tem que dar uma resposta
# sensata em todo ramo, inclusive sem instalação nenhuma e sem rede — nunca
# um erro que derrube a página antes dela conseguir mostrar "instalar".

local_home <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(c(TRAMA_HOME = dir), .local_envir = env)
  dir
}

manifesto_teste <- function(trama = "2026.10", r = as.character(getRversion())) {
  list(
    trama = trama, r = r, cran_snapshot = "2026-10-01",
    core = list(trama = "0.1.0", trama.data = "0.1.0"),
    collections = list(trama.ml = list(version = "0.1.0", title = "Aprendizado de máquina", requires = list()))
  )
}

test_that("estado vazio quando o arquivo não existe", {
  local_home()
  s <- tl_state_read()

  expect_equal(s$atual, "")
  expect_equal(s$anteriores, character(0))
  expect_equal(s$colecoes, character(0))
})

test_that("escrita é atômica: grava em temp e renomeia", {
  local_home()
  tl_state_write(list(atual = "2026.10", anteriores = "2026.09", colecoes = "trama.ml"))

  expect_true(file.exists(tl_state_file()))
  expect_equal(length(list.files(tl_home(), pattern = "^estado-.*\\.json\\.tmp$")), 0)

  lido <- tl_state_read()
  expect_equal(lido$atual, "2026.10")
  expect_equal(lido$anteriores, "2026.09")
  expect_equal(lido$colecoes, "trama.ml")
})

test_that("tl_status sem instalação nenhuma", {
  local_home()
  m <- manifesto_teste()

  st <- tl_status(m = m, s = tl_state_read())

  expect_equal(st$release_instalada, "")
  expect_equal(st$release_disponivel, "2026.10")
  expect_true(st$atualizar)
  expect_false(st$troca_de_r)
  expect_true(all(c("trama", "trama.data") %in% st$pacotes$nome))
  expect_true(all(is.na(st$pacotes$instalada)))
})

test_that("tl_status já atualizado não pede atualização", {
  local_home()
  m <- manifesto_teste(trama = "2026.10")
  s <- list(atual = "2026.10", anteriores = character(0), colecoes = character(0))

  st <- tl_status(m = m, s = s)

  expect_equal(st$release_instalada, "2026.10")
  expect_false(st$atualizar)
})

test_that("tl_status com release nova disponível pede atualização", {
  local_home()
  m <- manifesto_teste(trama = "2026.11")
  s <- list(atual = "2026.10", anteriores = character(0), colecoes = character(0))

  st <- tl_status(m = m, s = s)

  expect_true(st$atualizar)
  expect_equal(st$release_disponivel, "2026.11")
})

test_that("tl_status offline não pede atualização e marca disponível como NA", {
  local_home()
  s <- list(atual = "2026.10", anteriores = character(0), colecoes = character(0))

  st <- tl_status(m = NULL, s = s)

  expect_true(is.na(st$release_disponivel))
  expect_false(st$atualizar)
  expect_false(st$troca_de_r)
})

test_that("tl_status detecta troca de linha do R", {
  local_home()
  m <- manifesto_teste(r = "99.0.0")
  s <- list(atual = "2026.10", anteriores = character(0), colecoes = character(0))

  st <- tl_status(m = m, s = s)

  expect_true(st$troca_de_r)
  expect_equal(st$r_exigido, "99.0.0")
})

test_that("tl_status lista coleções instaladas e disponíveis", {
  local_home()
  m <- manifesto_teste()
  s <- list(atual = "2026.10", anteriores = character(0), colecoes = "trama.ml")

  st <- tl_status(m = m, s = s)

  linha <- st$colecoes[st$colecoes$nome == "trama.ml", ]
  expect_equal(linha$titulo, "Aprendizado de máquina")
  expect_true(linha$instalada)
  expect_true(linha$disponivel)
})

test_that("tl_status usa o Title do DESCRIPTION instalado quando a coleção não está no manifesto", {
  home <- local_home()
  m <- manifesto_teste()
  s <- list(atual = "2026.10", anteriores = character(0), colecoes = c("trama.ml", "trama.fora"))

  lib <- tl_lib_dir("2026.10")
  dir.create(file.path(lib, "trama.fora"), recursive = TRUE)
  writeLines(
    c("Package: trama.fora", "Title: Coleção Fora do Manifesto", "Version: 0.1.0"),
    file.path(lib, "trama.fora", "DESCRIPTION")
  )

  st <- tl_status(m = m, s = s)

  linha <- st$colecoes[st$colecoes$nome == "trama.fora", ]
  expect_equal(linha$titulo, "Coleção Fora do Manifesto")
  expect_false(linha$disponivel)
})

test_that("tl_status cai para o nome sem prefixo trama. quando não há manifesto nem DESCRIPTION instalado", {
  local_home()
  s <- list(atual = "2026.10", anteriores = character(0), colecoes = "trama.sem.descricao")

  st <- tl_status(m = NULL, s = s)

  linha <- st$colecoes[st$colecoes$nome == "trama.sem.descricao", ]
  expect_equal(linha$titulo, "sem.descricao")
})
