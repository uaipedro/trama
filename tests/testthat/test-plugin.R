test_that("candidatas: só pacotes trama.* que se declaram plugin", {
  expect_false(.tr_plugin_flag("trama"))
  local_mocked_bindings(.tr_plugin_flag = function(p) TRUE)
  expect_true(all(grepl("^trama[.]", .tr_plugin_candidatos())))
})

test_that("plugin que falha ao carregar é pulado com aviso; os outros ficam", {
  local_mocked_bindings(.tr_plugin_carregar = function(p) {
    if (p == "trama.ruim") stop("quebrou")
    list(js = "x.js", package = p, version = "1.0.0")
  })
  expect_message(plugs <- tr_plugins(c("trama.boa", "trama.ruim")), "trama.ruim")
  expect_named(plugs, "trama.boa")
})

test_that("options(trama.plugins = character()) desliga todos", {
  withr::local_options(trama.plugins = character())
  expect_length(tr_plugins(), 0)
})

test_that("na página, o JS do plugin vem depois das coleções e antes do editor", {
  root <- tempfile("proj")
  project <- tr_project(root)
  project$registry$collections[["x"]] <- list(id = "x", label = "x", version = "1.0.0",
                                              css = "www/vendor/xyflow.css", package = "trama")
  plug <- list(trama.y = list(js = "www/ops.js", package = "trama", version = "2.0.0"))
  ui <- tr_ui(project, plug)
  deps <- htmltools::resolveDependencies(htmltools::findDependencies(ui))
  deps <- lapply(deps, shiny::createWebDependency)
  html <- as.character(htmltools::renderDependencies(deps, "href"))
  col <- regexpr("trama-x-[0-9.]+/xyflow.css", html)
  pl  <- regexpr("trama-plugin-y-2.0.0/ops.js", html)
  expect_true(col > 0 && pl > col)
  expect_true(regexpr("editor.js", as.character(ui)) > 0)
})

test_that("o server de cada plugin recebe o projeto; erro num não derruba a sessão", {
  root <- withr::local_tempdir("proj")
  tr_project_new(root, character())
  p <- tr_project_at(root, tr_registry())
  visto <- NULL
  plugs <- list(
    trama.a = list(server = function(input, output, session, ctx) visto <<- ctx$project()$root),
    trama.b = list(server = function(input, output, session, ctx) stop("falhou")))
  expect_message(shiny::testServer(tr_server(p, autosave = FALSE, plugins = plugs), {
    expect_true(TRUE)
  }), "trama.b")
  expect_equal(normalizePath(visto), normalizePath(root))
})
