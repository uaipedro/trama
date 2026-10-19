# A ação "visualizar": monta numa pasta do projeto, renderiza em segundo
# plano e responde com a URL servida. O Quarto aqui é falso (um script que
# grava o index.html): o render de verdade está em test-pagina.R.

rodar_acao <- function(valor, ctx, limite = 20) {
  resposta <- NULL
  ctx$responder <- function(r) resposta <<- r
  r0 <- .tr_publicar_visualizar(valor, ctx)
  if (!is.null(r0)) return(r0)
  inicio <- Sys.time()
  while (is.null(resposta) && difftime(Sys.time(), inicio, units = "secs") < limite) later::run_now(0.2)
  resposta
}

ctx_exemplo <- function(ex, raiz) {
  s <- trama::tr_store(file.path(raiz, ".trama", "store"))
  servido <- NULL
  list(doc = ex$doc, flow = "main", raiz = raiz, store = s, registry = ex$reg, chave = "abc123def456",
       servir = function(dir, nome) { servido <<- dir; paste0("trama-acao-", nome, "/") })
}

test_that("visualizar monta, renderiza e responde a URL e a conferência", {
  skip_on_os("windows")
  falso <- tempfile("quarto-"); writeLines(c("#!/bin/sh", "echo '<html>ok</html>' > index.html"), falso)
  Sys.chmod(falso, "755")
  withr::local_envvar(QUARTO_PATH = falso)
  ex <- doc_exemplo(); raiz <- tempfile("proj-"); dir.create(raiz)
  r <- rodar_acao(material_exemplo(), ctx_exemplo(ex, raiz))
  expect_null(r$erro)
  expect_identical(r$url, "trama-acao-previa-abc123def456/index.html")
  pasta <- file.path(raiz, ".trama", "publicar", "previa-abc123def456")
  expect_true(file.exists(file.path(pasta, "ro-crate-metadata.json")))
  expect_true(file.exists(file.path(pasta, "flow.json")))
  expect_true(file.exists(file.path(pasta, "index.html")))
  expect_identical(r$conferencia$titulo, "Consumo e peso")
})

test_that("Quarto que falha devolve o log; sem Quarto, o guia de instalação", {
  skip_on_os("windows")
  falso <- tempfile("quarto-"); writeLines(c("#!/bin/sh", "echo 'ERRO: sem pandoc' >&2", "exit 1"), falso)
  Sys.chmod(falso, "755")
  ex <- doc_exemplo(); raiz <- tempfile("proj-"); dir.create(raiz)
  withr::with_envvar(c(QUARTO_PATH = falso), {
    r <- rodar_acao(material_exemplo(), ctx_exemplo(ex, raiz))
  })
  expect_match(r$erro, "sem pandoc", fixed = TRUE)
  withr::with_envvar(c(QUARTO_PATH = file.path(tempdir(), "nao-existe")), {
    local_mocked_bindings(Sys.which = function(...) "", .package = "base")
    r <- rodar_acao(material_exemplo(), ctx_exemplo(ex, raiz))
  })
  expect_match(r$guia, "quarto.org", fixed = TRUE)
})

test_that("dados de entrada não ligados: descrição e sha256 do arquivo", {
  reg <- publicar_registry()
  raiz <- tempfile("proj-"); dir.create(raiz)
  utils::write.csv(utils::head(datasets::mtcars, 5), file.path(raiz, "carros.csv"), row.names = FALSE)
  f <- trama::tr_flow(reg) |>
    trama::tr_add("ler", "data/read", path = "carros.csv") |>
    trama::tr_add("mod", "tst/lm", from = "ler") |>
    trama::tr_add("pub", "publicar/publish", from = "mod")
  s <- trama::tr_store(file.path(raiz, ".trama", "store"))
  withr::with_dir(raiz, trama::tr_run(trama::tr_flow_doc(f), registry = reg, store = s))
  d <- .tr_publicar_dados_entrada(trama::tr_flow_doc(f), reg, s, raiz, ligados = "mod")
  expect_length(d, 1L)
  expect_identical(d[[1]]$no, "ler")
  expect_identical(d[[1]]$arquivo, "carros.csv")
  expect_identical(d[[1]]$sha256, digest::digest(file = file.path(raiz, "carros.csv"), algo = "sha256"))
  expect_identical(d[[1]]$linhas, 5L)
  expect_true("mpg" %in% vapply(d[[1]]$colunas, function(cl) cl$nome, ""))
})

test_that("caminhos absolutos do fluxo viram aviso", {
  doc <- list(nodes = list(a = list(params = list(path = "/home/fulano/dados.csv")), b = list(params = list(path = "x.csv"))))
  expect_identical(.tr_publicar_caminhos_absolutos(doc), "/home/fulano/dados.csv")
})
