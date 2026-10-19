test_that("candidatas: só pacotes trama.* que se declaram plugin", {
  expect_false(.tr_plugin_flag("trama"))
  local_mocked_bindings(.tr_plugin_flag = function(p) TRUE, .package = "trama")
  expect_true(all(grepl("^trama[.]", .tr_plugin_candidatos())))
})

test_that("plugin que falha ao carregar é pulado com aviso; os outros ficam", {
  local_mocked_bindings(.tr_plugin_carregar = function(p) {
    if (p == "trama.ruim") stop("quebrou")
    list(js = "x.js", package = p, version = "1.0.0")
  }, .package = "trama")
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

test_that("ctx$recarregar() relê do disco o projeto aberto e manda o documento", {
  root <- withr::local_tempdir("proj")
  tr_project_new(root, character())
  p <- tr_project_at(root, tr_registry())
  ctx_visto <- NULL
  plugs <- list(trama.a = list(server = function(input, output, session, ctx) ctx_visto <<- ctx))
  shiny::testServer(tr_server(p, autosave = FALSE, plugins = plugs), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    doc <- tr_project_flow(p, "main")
    doc$ui <- list(marca_do_teste = TRUE)
    tr_project_save(p, doc, "main")
    ctx_visto$recarregar()
    tipos <- vapply(msgs, function(m) m$type, "")
    expect_true(all(c("project", "document") %in% tipos))
    d <- msgs[[which(tipos == "document")[1]]]$doc
    expect_true(isTRUE(d$ui$marca_do_teste))
  })
})

test_that("coleção instalada: exporta trama_collection e não é plugin (#18)", {
  dir_col <- withr::local_tempdir(); dir_plug <- withr::local_tempdir(); dir_out <- withr::local_tempdir()
  writeLines(c("export(trama_collection)", "export(tr_x)"), file.path(dir_col, "NAMESPACE"))
  writeLines("export(trama_collection)", file.path(dir_plug, "NAMESPACE"))
  writeLines("export(tl_run)", file.path(dir_out, "NAMESPACE"))
  local_mocked_bindings(.tr_plugin_flag = function(p) p == "trama.plug", .package = "trama")
  expect_true(.tr_e_colecao("trama.col", caminho = dir_col))
  expect_false(.tr_e_colecao("trama.plug", caminho = dir_plug))
  expect_false(.tr_e_colecao("trama.launcher", caminho = dir_out))
  expect_false(.tr_e_colecao("trama.sumiu", caminho = character()))
})

test_that("a lista de coleções ignora o que não é trama.* e o que não é coleção", {
  local_mocked_bindings(.tr_e_colecao = function(pkg, ...) pkg == "trama.col", .package = "trama")
  expect_identical(.tr_colecoes_instaladas(c("dplyr", "trama.plug", "trama.col", "trama.col")), "trama.col")
})
