test_that("candidatas: só pacotes trama.* que se declaram extensão", {
  expect_false(.tr_extension_flag("trama"))
  local_mocked_bindings(.tr_extension_flag = function(p) TRUE)
  expect_true(all(grepl("^trama[.]", .tr_extension_candidatos())))
})

test_that("extensão que falha ao carregar é pulada com aviso; as outras ficam", {
  local_mocked_bindings(.tr_extension_carregar = function(p) {
    if (p == "trama.ruim") stop("quebrou")
    list(js = "x.js", package = p, version = "1.0.0")
  })
  expect_message(exts <- tr_extensions(c("trama.boa", "trama.ruim")), "trama.ruim")
  expect_named(exts, "trama.boa")
})

test_that("options(trama.extensions = character()) desliga todas", {
  withr::local_options(trama.extensions = character())
  expect_length(tr_extensions(), 0)
})

test_that("na página, o JS da extensão vem depois das coleções e antes do editor", {
  root <- tempfile("proj")
  project <- tr_project(root)
  project$registry$collections[["x"]] <- list(id = "x", label = "x", version = "1.0.0",
                                              css = "www/vendor/xyflow.css", package = "trama")
  ext <- list(trama.y = list(js = "www/ops.js", package = "trama", version = "2.0.0"))
  ui <- tr_ui(project, ext)
  deps <- htmltools::resolveDependencies(htmltools::findDependencies(ui))
  deps <- lapply(deps, shiny::createWebDependency)
  html <- as.character(htmltools::renderDependencies(deps, "href"))
  col <- regexpr("trama-x-[0-9.]+/xyflow.css", html)
  ex  <- regexpr("trama-ext-y-2.0.0/ops.js", html)
  expect_true(col > 0 && ex > col)
  expect_true(regexpr("editor.js", as.character(ui)) > 0)
})

test_that("o server de cada extensão recebe o projeto; erro numa não derruba a sessão", {
  root <- withr::local_tempdir("proj")
  tr_project_new(root, character())
  p <- tr_project_at(root, tr_registry())
  visto <- NULL
  exts <- list(
    trama.a = list(server = function(input, output, session, ctx) visto <<- ctx$project()$root),
    trama.b = list(server = function(input, output, session, ctx) stop("falhou")))
  expect_message(shiny::testServer(tr_server(p, autosave = FALSE, extensions = exts), {
    expect_true(TRUE)
  }), "trama.b")
  expect_equal(normalizePath(visto), normalizePath(root))
})
