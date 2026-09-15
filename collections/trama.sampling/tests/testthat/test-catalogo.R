# As varreduras das coleções irmãs, aplicadas aqui: como percorrem o registro
# inteiro em vez de listar nós um a um, nó novo nasce REPROVADO até ganhar a
# sua página — que é o ponto.

nos_sampling <- function(reg) Filter(function(n) startsWith(n$id, "sampling/"), reg$nodes)

test_that("a coleção carrega sobre data e view, e não sozinha", {
  expect_no_error(sampling_registry())
  reg <- trama::tr_registry()
  err <- tryCatch(trama::tr_use(trama_collection(), registry = reg), condition = identity)
  expect_s3_class(err, "tr_error_unknown_type")
})

test_that("as categorias não pisam nas das outras coleções", {
  reg <- trama::tr_registry()
  trama::tr_use("trama.data", registry = reg)
  trama::tr_use("trama.view", registry = reg)
  antes <- names(reg$categories)
  ids <- vapply(trama_collection()$categories, function(k) k$id, "")
  expect_length(intersect(ids, antes), 0L)
  # Nem nas da `models`, que usa `modelo_*`.
  expect_true(all(startsWith(ids, "amostra_")))
})

test_that("todo nó tem help no formato, e todo campo digitável tem exemplo", {
  reg <- sampling_registry()
  digitaveis <- c("expr", "cols", "path", "text")
  nos <- nos_sampling(reg)
  expect_length(nos, 29L)
  for (n in nos) {
    for (secao in c("## Descrição", "## Parâmetros", "## Valor", "## Exemplos", "## Veja também")) {
      expect_match(n$help, secao, fixed = TRUE, info = n$id)
    }
    for (nm in names(n$params)) {
      p <- n$params[[nm]]
      if (!p$kind %in% digitaveis) next
      expect_true(!is.null(p$example) && nzchar(p$example), info = paste(n$id, nm))
    }
  }
})

test_that("todo nó estocástico recebe a semente do núcleo e explica o sorteio", {
  reg <- sampling_registry()
  for (n in nos_sampling(reg)) {
    tem_seed <- ".seed" %in% names(formals(n$fn))
    expect_identical(n$stochastic, tem_seed, info = n$id)
    expect_identical(grepl(.tr_sampling_ajuda_semente(), n$help, fixed = TRUE), tem_seed, info = n$id)
  }
})

test_that("toda estimativa traz a leitura da régua do CV, e o JS registra os renderers", {
  reg <- sampling_registry()
  for (n in nos_sampling(reg)) {
    if (!identical(n$outputs$out$type, "sampling/estimate")) next
    expect_true(grepl(.tr_sampling_ajuda_cv(), n$help, fixed = TRUE), info = n$id)
  }
  js <- readLines(system.file("trama", "index.js", package = "trama.sampling"))
  for (r in c("sampling/plan", "sampling/sample", "sampling/estimate")) {
    expect_true(any(grepl(sprintf('registerRenderer("%s"', r), js, fixed = TRUE)), info = r)
  }
})

test_that("todo gráfico da coleção é view/plot com os seis cosméticos e a ajuda deles", {
  reg <- sampling_registry()
  graficos <- Filter(function(n) identical(n$outputs$out$type, "view/plot"), nos_sampling(reg))
  expect_length(graficos, 4L)
  comuns <- c("aspecto", "tema", "titulo", "rotulo_x", "rotulo_y", "legenda")
  for (n in graficos) {
    expect_equal(utils::tail(names(n$params), 6L), comuns, info = n$id)
    expect_true(grepl(trama.view::tr_view_help_appearance(), n$help, fixed = TRUE), info = n$id)
    expect_equal(n$params$aspecto$default, formals(n$fn)$aspecto, info = n$id)
  }
})

test_that("todo param do spec existe no fn com o mesmo default", {
  # O card chama o `fn` com os params do spec; um nome que só existe no spec
  # quebra o card, e um default diferente faz card e console discordarem.
  reg <- sampling_registry()
  for (n in nos_sampling(reg)) {
    f <- formals(n$fn)
    for (nm in names(n$params)) {
      expect_true(nm %in% names(f), info = paste(n$id, nm))
      expect_equal(n$params[[nm]]$default, eval(f[[nm]]), info = paste(n$id, nm))
    }
  }
})

test_that("toda referência cruzada da ajuda aponta pra nó ou tipo que existe", {
  reg <- sampling_registry()
  conhecidos <- c(names(reg$nodes), names(reg$types))
  for (n in nos_sampling(reg)) {
    citados <- regmatches(n$help, gregexpr("`[a-z][a-z0-9_.]*/[a-z0-9_]+`", n$help))[[1]]
    citados <- unique(gsub("`", "", citados, fixed = TRUE))
    expect_true(length(citados) > 0L, info = n$id)
    for (id in citados) expect_true(id %in% conhecidos, info = paste(n$id, "cita", id))
  }
})

test_that("os exemplos da ajuda rodam como DSL de verdade, e o fluxo inteiro calcula", {
  # Exemplo que não roda é pior que exemplo nenhum. E não basta montar o fluxo:
  # o último nó de cada exemplo é CALCULADO, para que um param com nome certo
  # e valor impossível também reprove.
  reg <- sampling_registry()
  env <- new.env(parent = asNamespace("trama"))
  env$reg <- reg
  for (n in nos_sampling(reg)) {
    blocos <- regmatches(n$help, gregexpr("```r\n.*?\n```", n$help))[[1]]
    expect_true(length(blocos) > 0L, info = n$id)
    for (b in blocos) {
      codigo <- sub("^```r\n", "", sub("\n```$", "", b))
      f <- NULL
      expect_no_error(f <- eval(parse(text = codigo), envir = env), message = n$id)
      if (is.null(f)) next
      nos_doc <- trama::tr_flow_doc(f)$nodes
      tipos <- vapply(nos_doc, function(x) x$type, "")
      # O exemplo tem de USAR o nó da página que o traz.
      expect_true(n$id %in% tipos, info = paste(n$id, "não aparece no próprio exemplo"))
      ultimo <- utils::tail(names(nos_doc), 1L)
      expect_no_error(rodar(f, ultimo), message = paste(n$id, "calculando", ultimo))
    }
  }
})

test_that("o catálogo sai com a ajuda e os adaptadores", {
  cat_json <- as.character(trama::tr_catalog_json(sampling_registry()))
  expect_match(cat_json, "Exemplo de amostragem", fixed = TRUE)
  expect_match(cat_json, "\"from\":\"sampling/sample\"", fixed = TRUE)
})

test_that("o fn de todo nó está exportado no NAMESPACE", {
  # O NAMESPACE é escrito à MÃO; a fonte da verdade é o ARQUIVO, e não
  # `getNamespaceExports()`, porque o `load_all` da suíte exporta tudo.
  expect_true(file.exists("../../NAMESPACE"))
  linhas <- grep("^export\\(", readLines("../../NAMESPACE"), value = TRUE)
  exportados <- sub("^export\\((.*)\\)$", "\\1", linhas)
  ns <- asNamespace("trama.sampling")
  fns <- Filter(is.function, mget(ls(ns, all.names = TRUE), envir = ns, inherits = FALSE))
  for (n in nos_sampling(sampling_registry())) {
    bate <- vapply(fns, identical, TRUE, n$fn)
    expect_true(any(bate), info = paste(n$id, "tem fn que não está no namespace"))
    nome <- names(fns)[bate][[1]]
    expect_true(nome %in% exportados, info = paste(n$id, "usa", nome, "— falta export() no NAMESPACE"))
  }
})
