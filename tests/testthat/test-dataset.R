# Catálogo de bases públicas: o núcleo guarda metadados e roda o bloco que a
# coleção declarou, sem saber carregar dado nenhum. Coleção falsa, como em
# helper-collection.R.

bases_collection <- function(datasets = NULL) {
  tab <- tr_type("b/tab", label = "Tabela")
  no <- tr_node("b/carregar", fn = function(pacote, nome) get(nome, envir = asNamespace(pacote)),
                outputs = list(out = "b/tab"), description = "Carrega um objeto de pacote.",
                params = list(pacote = tr_param_text("stats"), nome = tr_param_text("x")))
  datasets <- datasets %||% list(
    tr_dataset("stats", "ecdf", "Função (não tabela)", "b/carregar",
               params = list(pacote = "stats", nome = "ecdf")),
    tr_dataset("datasets", "mtcars", "Carros", "b/carregar",
               params = list(pacote = "datasets", nome = "mtcars"),
               temas = "regressão", n = 32, variaveis = 11, fonte = "Motor Trend (1974)"),
    tr_dataset("pacotequenaoexiste", "coisa", "Ausente", "b/carregar",
               params = list(pacote = "pacotequenaoexiste", nome = "coisa"))
  )
  tr_collection(id = "b", types = list(tab), nodes = list(no), datasets = datasets)
}

reg_bases <- function(...) {
  reg <- tr_registry(); tr_use(bases_collection(...), registry = reg); reg
}

test_that("tr_datasets lista as bases com metadados e estado de instalação", {
  l <- tr_datasets(reg_bases())
  ids <- vapply(l, function(d) d$id, "")
  expect_equal(ids, c("stats::ecdf", "datasets::mtcars", "pacotequenaoexiste::coisa"))
  mt <- l[[2]]
  expect_true(mt$instalado)
  expect_false(l[[3]]$instalado)
  expect_equal(mt$node, "b/carregar")
  expect_equal(mt$params, list(pacote = "datasets", nome = "mtcars"))
  # temas é array mesmo com um só, e ausência some do JSON
  js <- jsonlite::fromJSON(jsonlite::toJSON(l, auto_unbox = TRUE), simplifyVector = FALSE)
  expect_type(js[[2]]$temas, "list")
  expect_null(js[[1]]$fonte)
})

test_that("tr_dataset_load roda o bloco declarado; pacote ausente é erro classificado", {
  reg <- reg_bases()
  expect_identical(tr_dataset_load("datasets::mtcars", reg), datasets::mtcars)
  expect_error(tr_dataset_load("pacotequenaoexiste::coisa", reg),
               class = "tr_error_dataset_missing_pkg")
  expect_error(tr_dataset_load("nada::nada", reg), class = "tr_error_unknown_dataset")
})

test_that("declaração inválida é recusada cedo", {
  expect_error(tr_dataset("p", "", "t", "b/carregar"), class = "tr_error_bad_dataset")
  expect_error(tr_dataset("p", "n", "t", "b/carregar", params = list(1)), class = "tr_error_bad_dataset")
  expect_error(tr_collection("b", datasets = list(list(pacote = "x"))), class = "tr_error_bad_dataset")
  # bloco que não existe no registro
  expect_error(reg_bases(list(tr_dataset("p", "n", "t", "b/nada"))), class = "tr_error_unknown_node")
})

test_that("a prévia traz dimensões, colunas e primeiras linhas", {
  pv <- .tr_dataset_preview(datasets::iris, linhas = 3)
  expect_equal(c(pv$n, pv$p), c(150L, 5L))
  expect_equal(pv$colunas[[5]], list(nome = "Species", classe = "factor"))
  expect_length(pv$linhas, 3L)
  expect_equal(pv$linhas[[1]][[5]], "setosa")
})

test_that("transporte: lista, prévia e CSV pelo canal de eventos", {
  root <- withr::local_tempdir("proj")
  tr_project_new(root, character())
  p <- tr_project_at(root, reg_bases())
  shiny::testServer(tr_server(p, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    ultimo <- function(t) Filter(function(m) identical(m$type, t), msgs) |> rev() |> (\(x) x[[1]])()

    session$setInputs(tr_datasets_list = list(seq = 1))
    expect_length(ultimo("datasets")$datasets, 3L)

    session$setInputs(tr_dataset_preview = list(seq = 2, id = "datasets::mtcars"))
    expect_equal(ultimo("dataset_preview")$preview$n, 32L)

    session$setInputs(tr_dataset_preview = list(seq = 3, id = "pacotequenaoexiste::coisa"))
    expect_match(ultimo("dataset_preview")$erro, "não está instalado")

    session$setInputs(tr_dataset_csv = list(seq = 4, id = "datasets::mtcars"))
    csv <- ultimo("dataset_csv")
    expect_equal(csv$nome, "mtcars")
    lido <- utils::read.csv(text = csv$texto)
    expect_equal(dim(lido), c(32L, 11L))
    expect_equal(lido$mpg, datasets::mtcars$mpg)
  })
})

test_that("fim da instalação, fora de contexto reativo, avisa e reenvia a lista sem derrubar a sessão", {
  root <- withr::local_tempdir("proj")
  tr_project_new(root, character())
  p <- tr_project_at(root, reg_bases())
  pendente <- NULL
  local_mocked_bindings(.tr_install_bg = function(pacote, on_done, ...) { pendente <<- on_done; invisible() },
                        .package = "trama")
  shiny::testServer(tr_server(p, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    session$setInputs(tr_dataset_install = list(seq = 1, pacote = "pacotequenaoexiste"))
    expect_equal(msgs[[1]]$estado, "instalando")
    # Chamado como o `later` chama: sem contexto reativo.
    expect_no_error(pendente(TRUE, "ok"))
    tipos <- vapply(msgs, function(m) m$type, "")
    expect_equal(tail(tipos, 2), c("dataset_install", "datasets"))
  })
})

test_that("instalar recusa pacote que não é de base do catálogo", {
  root <- withr::local_tempdir("proj")
  tr_project_new(root, character())
  p <- tr_project_at(root, reg_bases())
  chamado <- FALSE
  local_mocked_bindings(.tr_install_bg = function(...) { chamado <<- TRUE; invisible() }, .package = "trama")
  shiny::testServer(tr_server(p, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    session$setInputs(tr_dataset_install = list(seq = 1, pacote = "qualquercoisa"))
    expect_false(chamado)
    expect_equal(msgs[[1]]$type, "warning")
  })
})

test_that("instalação cujo filho morre sem status termina em erro pelo limite", {
  res <- NULL
  local_mocked_bindings(system2 = function(...) invisible(0L), .package = "base")
  .tr_install_bg("xyz", function(ok, log) res <<- list(ok = ok, log = log), intervalo = 0.01, limite = 0.05)
  t0 <- Sys.time()
  while (is.null(res) && difftime(Sys.time(), t0, units = "secs") < 5) later::run_now(0.05)
  expect_false(res$ok)
  expect_match(res$log, "tempo limite")
})
