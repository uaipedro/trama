# Dependência entre coleções (`Config/trama/requires`). O grafo vem injetado
# por `requires_of` — o núcleo não tem coleção real —, mas `tr_use()` pelo nome
# precisa de namespace de verdade, daí os pacotes mínimos via load_all. A base
# define o tipo; a de cima só tem nó que o consome: carregar a de cima sozinha
# é exatamente o caso que dava tr_error_unknown_type.

fake_pkg <- function(pkg, id, types = list(), nodes = list(), requires = NULL) {
  dir <- file.path(tempfile("pkg"), pkg); dir.create(file.path(dir, "R"), recursive = TRUE)
  writeLines(c(sprintf("Package: %s", pkg), "Version: 0.0.1", "Title: t", "Description: t.",
               "License: MIT", if (length(requires)) sprintf("Config/trama/requires: %s", requires)),
             file.path(dir, "DESCRIPTION"))
  writeLines("export(trama_collection)", file.path(dir, "NAMESPACE"))
  writeLines(sprintf("trama_collection <- function() trama::tr_collection(id = %s, types = %s, nodes = %s)",
                     deparse(id), types, nodes), file.path(dir, "R", "c.R"))
  pkgload::load_all(dir, quiet = TRUE, attach = FALSE, export_all = FALSE)
  dir
}

fake_graph <- function(...) { g <- list(...); function(pkg) g[[pkg]] %||% character() }

local({
  skip_if_not_installed("pkgload")
  fake_pkg("trama.fbase", "fbase", types = 'list(trama::tr_type("fbase/x", label = "X"))', nodes = "list()")
  fake_pkg("trama.ftopo", "ftopo", types = "list()",
           nodes = 'list(trama::tr_node("ftopo/n", fn = identity, description = "d.", inputs = list(x = "fbase/x")))')
})

test_that("coleção pelo nome carrega antes a coleção de que depende", {
  g <- fake_graph(trama.ftopo = "trama.fbase")
  reg <- tr_registry()
  tr_use("trama.ftopo", registry = reg, requires_of = g)
  expect_setequal(names(reg$collections), c("fbase", "ftopo"))
  # Pacote guardado também na dependência: é por ele que o daemon reconstrói.
  expect_equal(reg$collections$fbase$package, "trama.fbase")
  # Sem a declaração, é o erro que o mecanismo existe para evitar.
  expect_error(tr_use("trama.ftopo", registry = tr_registry(), requires_of = fake_graph()),
               class = "tr_error_unknown_type")
})

test_that("dependência já no registro não recarrega e a ordem deixa de importar", {
  g <- fake_graph(trama.ftopo = "trama.fbase")
  reg <- tr_registry()
  tr_use("trama.fbase", registry = reg, requires_of = g)
  expect_no_error(tr_use("trama.ftopo", registry = reg, requires_of = g))
  expect_length(reg$collections, 2)
})

test_that("dependência não instalada dá erro com classe e nada carrega", {
  g <- fake_graph(trama.ftopo = "trama.naoexiste")
  reg <- tr_registry()
  expect_error(tr_use("trama.ftopo", registry = reg, requires_of = g),
               class = "tr_error_collection_requires")
  expect_length(reg$collections, 0)
})

test_that("ciclo entre coleções é erro, não recursão infinita", {
  g <- fake_graph(trama.ftopo = "trama.fbase", trama.fbase = "trama.ftopo")
  expect_error(tr_use("trama.ftopo", registry = tr_registry(), requires_of = g),
               class = "tr_error_collection_cycle")
})

test_that("o leitor do DESCRIPTION separa por vírgula e aceita campo ausente", {
  expect_equal(.tr_collection_requires("trama"), character())
  skip_if_not_installed("trama.view")
  expect_equal(.tr_collection_requires("trama.view"), "trama.data")
})

test_that("projeto listando só trama.series traz data e view sozinho, em qualquer ordem", {
  skip_if_not_installed("trama.series")
  root <- tempfile("proj"); dir.create(root)
  p <- tr_project(root, collections = "trama.series")
  expect_true(all(c("trama.data", "trama.view", "trama.series") %in% .tr_registry_packages(p$registry)))
  p2 <- tr_project(tempfile("proj"), collections = c("trama.series", "trama.data"))
  expect_setequal(unname(.tr_registry_packages(p2$registry)), unname(.tr_registry_packages(p$registry)))
})
