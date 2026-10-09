test_that("todo nó tem description, categoria existente e portas de tipo conhecido", {
  co <- trama_collection()
  cats <- vapply(co$categories, function(x) x$id, "")
  tipos <- c(vapply(co$types, function(x) x$id, ""), "data/table", "view/plot")
  for (nd in co$nodes) {
    expect_true(nzchar(nd$description), info = nd$id)
    expect_true(nd$category %in% cats, info = nd$id)
    for (p in c(nd$inputs, nd$outputs)) {
      tp <- if (inherits(p, "tr_port")) p$type else p
      expect_true(tp %in% tipos, info = paste(nd$id, tp))
    }
  }
})

test_that("todo nó tem ajuda não vazia", {
  for (nd in trama_collection()$nodes) {
    expect_true(!is.null(nd$help) && nzchar(nd$help), info = nd$id)
  }
})

test_that("nenhum nó usa um nome de param que o glossário reserva", {
  # `rotulo` sozinho é a COLUNA de rótulos (docs/glossario-parametros.md:34);
  # texto livre de nomeação é `nome`. `coluna` é proibido em favor de `variavel`.
  proibidos <- c("coluna", "alfa", "nivel", "alvo", "rotulo")
  for (nd in trama_collection()$nodes) {
    ps <- vapply(nd$params, function(p) p$name %||% "", "")
    expect_length(intersect(ps, proibidos), 0L)
  }
})

test_that("o catálogo sai inteiro com data e view carregadas", {
  reg <- spatial_registry()
  cat <- trama::tr_catalog(registry = reg)
  ids <- vapply(cat$nodes, function(x) x$id, "")
  expect_true(all(c("spatial/example", "spatial/coordinates") %in% ids))
})

test_that("a versão da coleção é a mesma no DESCRIPTION e no tr_collection", {
  # Na main de 09/10/2026 as três divergiam: DESCRIPTION 0.1.1, tr_collection
  # 0.1.0 e release.json 0.1.0. Este teste prende duas delas; a terceira é um
  # manifesto de release, fora do pacote.
  d <- read.dcf(system.file("DESCRIPTION", package = "trama.spatial"))[1, "Version"]
  expect_equal(trama_collection()$version, unname(d))
})

test_that("toda função marcada @export está no NAMESPACE", {
  # O NAMESPACE desta coleção é escrito À MÃO. Sob `pkgload::load_all` todas as
  # funções ficam visíveis, então um `@export` esquecido passa pela suíte inteira
  # e só aparece no pacote INSTALADO — que é como o editor carrega a coleção.
  # Foi o que aconteceu na 0.2.0: dez funções novas ficaram de fora, e só a
  # chamada por namespace pegou. Este teste fecha o buraco.
  dir_r <- system.file("..", package = "trama.spatial")
  raiz <- if (dir.exists(file.path(dir_r, "R"))) dir_r else
    normalizePath(file.path(dirname(system.file("DESCRIPTION",
                                                package = "trama.spatial")), ".."))
  skip_if_not(dir.exists(file.path(raiz, "R")), "fonte R/ não disponível")
  linhas <- unlist(lapply(dir(file.path(raiz, "R"), "[.]R$", full.names = TRUE),
                          readLines, warn = FALSE))
  i <- grep("^#'[[:space:]]*@export[[:space:]]*$", linhas)
  nomes <- character()
  for (k in i) {
    # a declaração vem depois do bloco roxygen
    j <- k + 1L
    while (j <= length(linhas) && grepl("^#'", linhas[[j]])) j <- j + 1L
    if (j <= length(linhas)) {
      m <- regmatches(linhas[[j]],
                      regexec("^([a-zA-Z._][a-zA-Z0-9._]*)[[:space:]]*<-[[:space:]]*function",
                              linhas[[j]]))[[1]]
      if (length(m) == 2L) nomes <- c(nomes, m[[2]])
    }
  }
  nomes <- unique(nomes)
  expect_gt(length(nomes), 10L)
  ns <- readLines(file.path(raiz, "NAMESPACE"), warn = FALSE)
  exportados <- regmatches(ns, regexpr("(?<=^export[(])[^)]+", ns, perl = TRUE))
  faltam <- setdiff(nomes, exportados)
  expect_equal(faltam, character(0))
})
