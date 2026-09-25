# As varreduras das coleções irmãs, aplicadas aqui: como percorrem o registro
# inteiro em vez de listar nós um a um, nó novo nasce REPROVADO até ganhar a
# sua página — que é o ponto.

nos_models <- function(reg) Filter(function(n) startsWith(n$id, "models/"), reg$nodes)

test_that("a coleção carrega sobre data e view, e não sozinha", {
  expect_no_error(models_registry())
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
})

test_that("todo nó tem help no formato, e todo campo digitável tem exemplo", {
  reg <- models_registry()
  digitaveis <- c("expr", "cols", "path", "text")
  nos <- nos_models(reg)
  expect_length(nos, 42L)  # Fase 7: models/predict e models/rls; rigor: friedman, scott_knott
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

test_that("todo gráfico da coleção é view/plot com os seis cosméticos e a ajuda deles", {
  reg <- models_registry()
  graficos <- Filter(function(n) identical(n$outputs$out$type, "view/plot"), nos_models(reg))
  expect_length(graficos, 3L)
  comuns <- c("aspecto", "tema", "titulo", "rotulo_x", "rotulo_y", "legenda")
  for (n in graficos) {
    expect_equal(utils::tail(names(n$params), 6L), comuns, info = n$id)
    expect_true(grepl(trama.view::tr_view_help_appearance(), n$help, fixed = TRUE), info = n$id)
    # O default do spec e o do `fn` têm de bater: o card desenha pelo spec, o
    # console pelo `fn`.
    expect_equal(n$params$aspecto$default, formals(n$fn)$aspecto, info = n$id)
  }
})

test_that("todo param do spec existe no fn com o mesmo default", {
  # O card chama o `fn` com os params do spec; um nome que só existe no spec
  # quebra o card, e um default diferente faz card e console discordarem.
  reg <- models_registry()
  for (n in nos_models(reg)) {
    f <- formals(n$fn)
    for (nm in names(n$params)) {
      expect_true(nm %in% names(f), info = paste(n$id, nm))
      expect_equal(n$params[[nm]]$default, eval(f[[nm]]), info = paste(n$id, nm))
    }
  }
})

test_that("toda referência cruzada da ajuda aponta pra nó ou tipo que existe", {
  reg <- models_registry()
  conhecidos <- c(names(reg$nodes), names(reg$types))
  for (n in nos_models(reg)) {
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
  reg <- models_registry()
  env <- new.env(parent = asNamespace("trama"))
  env$reg <- reg
  for (n in nos_models(reg)) {
    blocos <- regmatches(n$help, gregexpr("```r\n.*?\n```", n$help))[[1]]
    for (b in blocos) {
      codigo <- sub("^```r\n", "", sub("\n```$", "", b))
      f <- NULL
      expect_no_error(f <- eval(parse(text = codigo), envir = env), message = n$id)
      if (is.null(f)) next
      ultimo <- utils::tail(names(trama::tr_flow_doc(f)$nodes), 1L)
      expect_no_error(rodar(f, ultimo), message = paste(n$id, "calculando", ultimo))
    }
  }
})

test_that("o catálogo sai com a ajuda e os adaptadores", {
  cat_json <- as.character(trama::tr_catalog_json(models_registry()))
  expect_match(cat_json, "Exemplo de modelos", fixed = TRUE)
  expect_match(cat_json, "\"from\":\"models/test\"", fixed = TRUE)
})

test_that("o fn de todo nó está exportado no NAMESPACE", {
  # O NAMESPACE é escrito à MÃO; a fonte da verdade é o ARQUIVO, e não
  # `getNamespaceExports()`, porque o `load_all` da suíte exporta tudo.
  expect_true(file.exists("../../NAMESPACE"))
  linhas <- grep("^export\\(", readLines("../../NAMESPACE"), value = TRUE)
  exportados <- sub("^export\\((.*)\\)$", "\\1", linhas)
  ns <- asNamespace("trama.models")
  fns <- Filter(is.function, mget(ls(ns, all.names = TRUE), envir = ns, inherits = FALSE))
  for (n in nos_models(models_registry())) {
    bate <- vapply(fns, identical, TRUE, n$fn)
    expect_true(any(bate), info = paste(n$id, "tem fn que não está no namespace"))
    nome <- names(fns)[bate][[1]]
    expect_true(nome %in% exportados, info = paste(n$id, "usa", nome, "— falta export() no NAMESPACE"))
  }
})

test_that("todo teste e quadro traz a explicação da régua, e o JS a registra", {
  reg <- models_registry()
  for (n in nos_models(reg)) {
    if (!n$outputs$out$type %in% c("models/test", "models/effects")) next
    expect_true(grepl(.tr_models_ajuda_regua(), n$help, fixed = TRUE), info = n$id)
  }
  js <- readLines(system.file("trama", "index.js", package = "trama.models"))
  for (r in c("models/test", "models/effects", "models/fit")) {
    expect_true(any(grepl(sprintf('registerRenderer("%s"', r), js, fixed = TRUE)), info = r)
  }
})
