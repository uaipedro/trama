# tl_open_window(): só a escolha do comando é testada (task 2.4 do plano) —
# abrir de fato uma janela de app não é coberto aqui.

test_that("Windows usa msedge encontrado pelo Sys.which", {
  cmd <- .tl_janela_comando(
    "http://127.0.0.1:8725",
    os = list(tipo = "windows"),
    which = function(x) if (identical(x, "msedge")) "C:/Edge/msedge.exe" else "",
    existe = function(x) TRUE
  )
  expect_equal(cmd$exe, "C:/Edge/msedge.exe")
  expect_equal(cmd$args, "--app=http://127.0.0.1:8725")
})

test_that("Windows cai no caminho padrão do Program Files quando Sys.which não acha", {
  withr::local_envvar(c(
    "ProgramFiles(x86)" = "C:/Program Files (x86)",
    "ProgramFiles" = "C:/Program Files"
  ))
  cmd <- .tl_janela_comando(
    "http://x", os = list(tipo = "windows"),
    which = function(x) "",
    existe = function(x) identical(x, "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
  )
  expect_equal(cmd$exe, "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
})

test_that("Windows sem Edge em lugar nenhum devolve NULL", {
  cmd <- .tl_janela_comando(
    "http://x", os = list(tipo = "windows"),
    which = function(x) "", existe = function(x) FALSE
  )
  expect_null(cmd)
})

test_that("Linux tenta google-chrome, depois chromium, depois chromium-browser", {
  cmd <- .tl_janela_comando(
    "http://x", os = list(tipo = "unix"),
    which = function(x) if (identical(x, "chromium")) "/usr/bin/chromium" else "",
    existe = function(x) TRUE
  )
  expect_equal(cmd$exe, "/usr/bin/chromium")
})

test_that("Linux sem nenhum navegador de app devolve NULL", {
  cmd <- .tl_janela_comando(
    "http://x", os = list(tipo = "unix"),
    which = function(x) "", existe = function(x) FALSE
  )
  expect_null(cmd)
})

test_that("tl_open_window roda o comando quando encontrado", {
  chamadas <- list()
  tl_open_window(
    "http://x",
    comando = list(exe = "/usr/bin/chromium", args = "--app=http://x"),
    executar = function(exe, args, wait) chamadas[["cmd"]] <<- list(exe = exe, args = args, wait = wait),
    navegador = function(url) stop("não deveria cair no navegador padrão")
  )
  expect_equal(chamadas$cmd$exe, "/usr/bin/chromium")
  expect_false(chamadas$cmd$wait)
})

test_that("tl_open_window cai no navegador padrão quando não há comando", {
  chamadas <- list()
  tl_open_window(
    "http://x",
    comando = NULL,
    executar = function(...) stop("não deveria chamar executar"),
    navegador = function(url) chamadas[["url"]] <<- url
  )
  expect_equal(chamadas$url, "http://x")
})
