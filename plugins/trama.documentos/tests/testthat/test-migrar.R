# Migração do trama.publicar (#25): o nó publicar/publish vira um documento
# Página web; o registro de publicações passa do nó para o documento.

# Um fluxo antigo, como o trama.publicar gravava: o nó publicar com os
# params e os itens ligados por arestas (a coleção não está instalada; o
# fluxo é escrito à mão).
fluxo_com_publicar <- function(proj, layout = "", extra = list(), itens = c("tab", "txt")) {
  salvar_fluxo(proj, no("tab", n = 3L), no("txt", "tst/texto", valor = "b"), no("ruim", "tst/falha"))
  caminho <- file.path(proj$root, "flows", "main.json")
  doc <- trama::tr_doc_read(caminho)
  doc$nodes$pub <- list(type = "publicar/publish", type_version = 1L, label = "Publicar",
    params = utils::modifyList(list(
      titulo = "Consumo e peso", descricao = "Material de teste.", palavras_chave = "carros, consumo",
      autores = "Josiah Carberry (0000-0002-1825-0097); Ana Lima", licenca = "CC0-1.0", licenca_codigo = "MIT",
      repositorio = "", versao = "correção", layout = layout, aceitar_dados_treino = FALSE, privado = FALSE),
      extra))
  doc$edges <- c(doc$edges, lapply(itens, function(i) list(from = list(node = i, port = "out"),
                                                           to = list(node = "pub", port = "itens"))))
  doc$ui$positions <- list(tab = c(0, 0), txt = c(0, 100), ruim = c(0, 200), pub = c(300, 0))
  doc$collections$publicar <- "0.2.0"
  trama::tr_doc_write(doc, caminho)
  cfg <- jsonlite::fromJSON(file.path(proj$root, "trama.json"), simplifyVector = FALSE)
  cfg$collections <- I(c(unlist(cfg$collections), "trama.publicar"))
  writeLines(jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = TRUE), file.path(proj$root, "trama.json"))
  caminho
}

registrar_antigo <- function(proj) {
  trama::tr_project_config_set(proj$root, "publicacoes", list(pub = list(
    repo = "pesquisadora/consumo-e-peso", url = "https://github.com/pesquisadora/consumo-e-peso",
    pagina = "https://pesquisadora.github.io/consumo-e-peso/", ultimo = "https://pesquisadora.github.io/consumo-e-peso/ultimo/",
    versao = "0.1.0", tag = "v0.1.0", data = "2026-10-19")))
}

test_that("o nó vira um documento Página web: metadados no YAML, itens como regiões, na ordem", {
  proj <- local_projeto()
  caminho <- fluxo_com_publicar(proj)
  registrar_antigo(proj)
  r <- tr_documento_migrar_publicar(proj)
  expect_identical(r$no, "pub")
  expect_identical(r$documento, "documentos/consumo-e-peso/consumo-e-peso.qmd")
  expect_identical(r$regioes, 2L)
  qmd <- file.path(proj$root, r$documento)
  y <- trama.documentos:::.td_yaml(linhas(qmd))
  expect_identical(y$title, "Consumo e peso")
  expect_identical(y$description, "Material de teste.")
  expect_identical(y$keywords, c("carros", "consumo"))
  expect_identical(y$author[[1]], list(name = "Josiah Carberry", orcid = "0000-0002-1825-0097"))
  expect_identical(y$author[[2]], list(name = "Ana Lima"))
  expect_identical(names(y$format), "pagina-html")
  expect_identical(y$trama$publicar, list(licenca = "CC0-1.0", "licenca-codigo" = "MIT", versao = "correção"))
  # Layout vazio = automático: cabeçalho, os itens na ordem das arestas, o diagrama.
  l <- linhas(qmd)
  pos <- function(x) which(l == x)[1]
  expect_true(pos("## Carregando o R no seu navegador") < pos("## Tabela de teste"))
  expect_true(pos("## Tabela de teste") < regiao(qmd, "tab")$inicio)
  expect_true(pos("## Texto") > pos("## Tabela de teste"))
  expect_true(pos("## O fluxo") > pos("## Texto"))
  expect_identical(unname(estado_de(qmd, proj)), c("atualizada", "atualizada"))
  # A publicação feita passa a ser do documento; a próxima segue nela.
  p <- tr_documento_publicacoes(proj)
  expect_identical(p$documento, r$documento)
  expect_identical(p$repo, "pesquisadora/consumo-e-peso")
  expect_identical(p$versao, "0.1.0")
  expect_null(trama::tr_project_config(proj$root, "publicacoes")$pub)
  # O nó e as arestas dele saem do fluxo; a coleção sai do trama.json.
  doc <- trama::tr_doc_read(caminho)
  expect_setequal(names(doc$nodes), c("tab", "txt", "ruim"))
  expect_false(any(vapply(doc$edges, function(e) identical(e$to$node, "pub"), TRUE)))
  expect_null(doc$ui$positions$pub)
  expect_null(doc$collections$publicar)
  cols <- unlist(jsonlite::fromJSON(file.path(proj$root, "trama.json"), simplifyVector = FALSE)$collections)
  expect_false("trama.publicar" %in% cols)
  # Uma vez só: sem nó, nada mais acontece.
  expect_identical(nrow(tr_documento_migrar_publicar(proj)), 0L)
  expect_identical(nrow(tr_documento_listar(proj)), 1L)
})

test_that("layout escrito: textos, títulos, legendas e a ordem dele; item que falha vira nota", {
  proj <- local_projeto()
  layout <- jsonlite::toJSON(list(
    list(tipo = "texto", texto = "Introdução em **markdown**."),
    list(tipo = "item", id = "txt", titulo = "O texto", legenda = "Uma legenda."),
    list(tipo = "item", id = "ruim"),
    list(tipo = "item", id = "naoligado"),
    list(tipo = "item", id = "tab")), auto_unbox = TRUE)
  fluxo_com_publicar(proj, layout = as.character(layout), itens = c("tab", "txt", "ruim"),
                     extra = list(repositorio = "https://github.com/pesquisadora/meu-repo"))
  r <- tr_documento_migrar_publicar(proj)
  expect_identical(r$regioes, 2L)
  expect_match(r$avisos, "'ruim' não entrou")
  qmd <- file.path(proj$root, r$documento)
  l <- linhas(qmd)
  # Sem bloco "cabecalho" no layout: só o setup do knitr do esqueleto.
  expect_false("## Carregando o R no seu navegador" %in% l)
  expect_true("{{< include _extensions/pagina/_knitr.qmd >}}" %in% l)
  pos <- function(x) which(l == x)[1]
  expect_true(pos("Introdução em **markdown**.") < pos("## O texto"))
  expect_true(pos("## O texto") < pos("Uma legenda."))
  expect_true(any(grepl("O resultado de `ruim` não pôde ser inserido", l, fixed = TRUE)))
  expect_true(pos("Uma legenda.") < pos("## Tabela de teste"))
  expect_false("## O fluxo" %in% l)
  expect_identical(vapply(trama.documentos:::.td_regioes(l), `[[`, "", "chave"), c("txt", "tab"))
  # O repositório pedido no param vira o endereço da publicação.
  expect_identical(tr_documento_publicacoes(proj)$repo, "pesquisadora/meu-repo")
})

test_that("projeto sem nó publicar: só a coleção sai da lista, se estiver lá", {
  proj <- local_projeto()
  salvar_fluxo(proj, no("tab"))
  cfg <- jsonlite::fromJSON(file.path(proj$root, "trama.json"), simplifyVector = FALSE)
  cfg$collections <- I("trama.publicar")
  writeLines(jsonlite::toJSON(cfg, auto_unbox = TRUE), file.path(proj$root, "trama.json"))
  expect_identical(nrow(tr_documento_migrar_publicar(proj)), 0L)
  cfg <- jsonlite::fromJSON(file.path(proj$root, "trama.json"), simplifyVector = FALSE)
  expect_identical(length(cfg$collections), 0L)
  expect_false(dir.exists(file.path(proj$root, "documentos")))
})

test_that("nome de documento já usado ganha sufixo", {
  proj <- local_projeto()
  tr_documento_criar("consumo-e-peso", proj, perfil = FALSE)
  fluxo_com_publicar(proj)
  r <- tr_documento_migrar_publicar(proj)
  expect_identical(r$documento, "documentos/consumo-e-peso-2/consumo-e-peso-2.qmd")
})
