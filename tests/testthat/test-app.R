test_that("importmap só aponta para arquivos locais que existem no pacote", {
  im <- jsonlite::fromJSON(.tr_importmap("trama-x"))$imports
  expect_true(all(startsWith(unlist(im), "./trama-x/")))
  www <- system.file("www", package = "trama")
  for (u in unlist(im)) expect_true(file.exists(file.path(www, sub("^\\./trama-x/", "", u))), info = u)
  expect_true(file.exists(file.path(www, "vendor", "xyflow.css")))
})

# A coleção falsa aponta pro `inst/` do PRÓPRIO trama: `system.file()` precisa
# de um pacote de verdade pra resolver o caminho, e os arquivos de `inst/www`
# existem tanto rodando do fonte quanto sob `R CMD check`. A entrada vai direto
# em `registry$collections` porque é essa a forma que `tr_dependency_collection`
# lê — montar pacote-fixture só pra isso seria cerimônia.
fake_registry <- function(js = NULL, css = NULL, package = "trama") {
  reg <- tr_registry()
  reg$collections[["x"]] <- list(id = "x", label = "x", version = "1.0.0",
                                 js = js, css = css, package = package)
  reg
}

test_that("tr_collection() aceita css e o guarda ao lado do js", {
  col <- tr_collection("x", js = "trama/index.js", css = "trama/x.css")
  expect_equal(col$js, "trama/index.js")
  expect_equal(col$css, "trama/x.css")
  expect_null(tr_collection("x")$css)
})

test_that("js e css de tr_collection() têm que ser caminho relativo único", {
  for (bad in list(1, c("a.css", "b.css"), NA_character_, "", "/abs/x.css", "~/x.css"))
    expect_error(tr_collection("x", css = bad), class = "tr_error_bad_asset", info = deparse(bad))
  expect_error(tr_collection("x", js = "/abs/index.js"), class = "tr_error_bad_asset")
})

test_that("asset de coleção não sobe de pasta, não usa \\ e não fica na raiz do inst/", {
  # A pasta do asset vai inteira pro navegador: cada um destes serviria mais
  # do que a coleção quis (ou outra coisa em outra máquina).
  for (bad in c("../x.js", "trama/../../x.js", "trama/..", "trama\\index.js",
                "index.js", "./index.js"))
    expect_error(tr_collection("x", js = bad), class = "tr_error_bad_asset", info = bad)
  expect_error(tr_collection("x", css = "x.css"), class = "tr_error_bad_asset")
  # Ponto no nome não é `..`, e subpasta funda continua valendo.
  expect_no_error(tr_collection("x", js = "trama/..index.js", css = "trama/a/b.css"))
})

test_that("tr_use() leva o css da coleção pro registro", {
  reg <- tr_registry()
  tr_use(tr_collection("x", css = "trama/x.css"), registry = reg)
  expect_equal(reg$collections$x$css, "trama/x.css")
})

test_that("coleção só com js continua virando uma dependência com o script", {
  deps <- tr_dependency_collection(fake_registry(js = "www/editor.js"), "x")
  expect_length(deps, 1)
  expect_equal(deps[[1]]$name, "trama-x")
  expect_equal(deps[[1]]$script[[1]]$src, "editor.js")
  expect_length(deps[[1]]$stylesheet, 0)
})

test_that("js e css no mesmo diretório viram UMA dependência com os dois", {
  deps <- tr_dependency_collection(fake_registry(js = "www/editor.js", css = "www/trama.css"), "x")
  expect_length(deps, 1)
  expect_equal(deps[[1]]$script[[1]]$src, "editor.js")
  expect_equal(deps[[1]]$stylesheet, "trama.css")
})

test_that("js e css em diretórios diferentes viram duas dependências de nomes distintos", {
  deps <- tr_dependency_collection(fake_registry(js = "www/editor.js", css = "www/vendor/xyflow.css"), "x")
  expect_length(deps, 2)
  nomes <- vapply(deps, `[[`, "", "name")
  expect_false(anyDuplicated(nomes) > 0)
  css <- Filter(function(d) length(d$stylesheet) > 0, deps)
  expect_equal(css[[1]]$stylesheet, "xyflow.css")
  expect_equal(normalizePath(css[[1]]$src$file),
               normalizePath(system.file("www/vendor", package = "trama")))
})

test_that("coleção só com css também carrega", {
  deps <- tr_dependency_collection(fake_registry(css = "www/trama.css"), "x")
  expect_length(deps, 1)
  expect_equal(deps[[1]]$stylesheet, "trama.css")
  expect_length(deps[[1]]$script, 0)
})

test_that("diretório inexistente, sem pacote ou sem asset: nada a carregar", {
  expect_length(tr_dependency_collection(fake_registry(), "x"), 0)
  expect_length(tr_dependency_collection(fake_registry(js = "nada/index.js"), "x"), 0)
  expect_length(tr_dependency_collection(fake_registry(css = "nada/x.css"), "x"), 0)
  expect_length(tr_dependency_collection(fake_registry(js = "www/editor.js", package = NULL), "x"), 0)
  # Um some, o outro fica: cada asset é resolvido por conta própria.
  deps <- tr_dependency_collection(fake_registry(js = "nada/index.js", css = "www/trama.css"), "x")
  expect_length(deps, 1)
  expect_equal(deps[[1]]$stylesheet, "trama.css")
})

test_that("no <head>, o CSS da coleção vem DEPOIS do trama.css do núcleo", {
  root <- tempfile("proj")
  project <- tr_project(root)
  project$registry$collections[["x"]] <- list(id = "x", label = "x", version = "1.0.0",
                                              css = "www/vendor/xyflow.css", package = "trama")
  # O mesmo caminho que o Shiny faz ao servir a página: resolve, troca `file`
  # por `href` (prefixo `nome-versão/`) e emite na ordem da lista.
  deps <- htmltools::resolveDependencies(htmltools::findDependencies(tr_ui(project)))
  deps <- lapply(deps, shiny::createWebDependency)
  html <- as.character(htmltools::renderDependencies(deps, "href"))
  core <- regexpr("trama-[0-9.]+/trama.css", html)
  col  <- regexpr("trama-x-[0-9.]+/xyflow.css", html)
  expect_true(core > 0 && col > 0)
  expect_true(col > core)
})
