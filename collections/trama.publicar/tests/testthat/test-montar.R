# A pasta do pacote: estrutura (snapshot), RO-Crate válido e os textos.

montar_exemplo <- function() {
  ex <- doc_exemplo()
  dir <- tempfile("pacote-")
  tr_publicar_montar(material_exemplo(), dir, doc = ex$doc, rotulos = list(tab = "Tabela", mod = "Modelo"),
                     dados_entrada = list(list(no = "tab", arquivo = "carros.csv", sha256 = "abc", linhas = 30L,
                                               colunas = list(list(nome = "mpg", classe = "numeric")))),
                     data = as.Date("2026-10-19"))
}

test_that("a estrutura do pacote", {
  r <- montar_exemplo()
  expect_snapshot(r$arquivos[!grepl("^_extensions/", r$arquivos)])
  expect_true(file.exists(file.path(r$dir, "_extensions/r-wasm/live/_extension.yml")))
})

test_that("RO-Crate 1.1: descritor, raiz, pessoas e todo arquivo descrito com sha256 que confere", {
  r <- montar_exemplo()
  rc <- jsonlite::fromJSON(file.path(r$dir, "ro-crate-metadata.json"), simplifyVector = FALSE)
  expect_identical(rc[["@context"]][[1]], "https://w3id.org/ro/crate/1.1/context")
  expect_identical(rc[["@context"]][[2]]$fair4ml, "https://w3id.org/fair4ml#")
  grafo <- rc[["@graph"]]
  ids <- vapply(grafo, function(e) e[["@id"]], "")
  expect_false(anyDuplicated(ids) > 0)
  desc <- grafo[[match("ro-crate-metadata.json", ids)]]
  expect_identical(desc$conformsTo[["@id"]], "https://w3id.org/ro/crate/1.1")
  expect_identical(desc$about[["@id"]], "./")
  raiz <- grafo[[match("./", ids)]]
  expect_identical(raiz[["@type"]], "Dataset")
  for (campo in c("name", "description", "datePublished", "license", "author", "hasPart")) expect_false(is.null(raiz[[campo]]))
  # Todo arquivo (menos o próprio descritor e a extensão) está no hasPart e no grafo.
  arquivos <- setdiff(r$arquivos[!grepl("^_extensions/", r$arquivos)], "ro-crate-metadata.json")
  partes <- vapply(raiz$hasPart, function(p) p[["@id"]], "")
  expect_setequal(setdiff(partes, "_extensions/"), arquivos)
  for (a in arquivos) {
    e <- grafo[[match(a, ids)]]
    expect_false(is.null(e), label = a)
    expect_identical(e$sha256, digest::digest(file = file.path(r$dir, a), algo = "sha256"), label = a)
    expect_identical(e$contentSize, as.character(file.info(file.path(r$dir, a))$size), label = a)
  }
  # Toda referência (@id dentro de objeto) aponta para uma entidade do grafo,
  # menos o perfil do descritor, que a especificação não pede descrito.
  refs <- unlist(lapply(grafo, function(e) rapply(e[setdiff(names(e), "@id")], function(x) x, how = "unlist")))
  refs_id <- setdiff(unique(unname(refs[grepl("@id$", names(refs))])), "https://w3id.org/ro/crate/1.1")
  expect_identical(setdiff(refs_id, ids), character())
  # O modelo com FAIR4ML.
  mod <- grafo[[match("itens/mod/modelo.rds", ids)]]
  expect_true("fair4ml:MLModel" %in% unlist(mod[["@type"]]))
  expect_identical(mod[["fair4ml:mlTask"]], "Regression")
  # A pessoa com ORCID.
  expect_true("https://orcid.org/0000-0002-1825-0097" %in% ids)
})

test_that("CITATION.cff é YAML válido com os autores; README e LICENSE preenchidos", {
  skip_if_not_installed("yaml")
  r <- montar_exemplo()
  cff <- yaml::read_yaml(file.path(r$dir, "CITATION.cff"))
  expect_identical(cff$`cff-version`, "1.2.0")
  expect_identical(cff$title, "Consumo e peso")
  expect_identical(cff$authors[[1]]$`family-names`, "Carberry")
  expect_identical(cff$authors[[1]]$orcid, "https://orcid.org/0000-0002-1825-0097")
  expect_identical(cff$version, "0.1.0")
  readme <- paste(readLines(file.path(r$dir, "README.md"), encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("{{", readme, fixed = TRUE))
  expect_match(readme, "carros.csv", fixed = TRUE)
  lic <- paste(readLines(file.path(r$dir, "LICENSE")), collapse = "\n")
  expect_match(lic, "CC-BY-4.0", fixed = TRUE)
  expect_match(lic, "Copyright (c) 2026 Josiah Carberry, Ana Lima", fixed = TRUE)
  fluxo <- jsonlite::fromJSON(file.path(r$dir, "flow.json"), simplifyVector = FALSE)
  expect_true(all(c("tab", "mod", "pub") %in% names(fluxo$nodes)))
  svg <- paste(readLines(file.path(r$dir, "fluxo.svg")), collapse = "")
  expect_match(svg, "<svg", fixed = TRUE)
  expect_match(svg, "Tabela", fixed = TRUE)
})

test_that("montar de novo na mesma pasta dá os mesmos bytes (determinístico)", {
  ex <- doc_exemplo(); m <- material_exemplo(); dir <- tempfile()
  a <- tr_publicar_montar(m, dir, doc = ex$doc, data = as.Date("2026-10-19"))
  h1 <- vapply(file.path(dir, a$arquivos), digest::digest, "", file = TRUE)
  b <- tr_publicar_montar(m, dir, doc = ex$doc, data = as.Date("2026-10-19"))
  h2 <- vapply(file.path(dir, b$arquivos), digest::digest, "", file = TRUE)
  expect_identical(h1, h2)
})
