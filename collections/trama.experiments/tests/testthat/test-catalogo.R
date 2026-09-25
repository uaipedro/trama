nos_exp <- function(reg) Filter(function(n) startsWith(n$id, "experiments/"), reg$nodes)

test_that("a coleção carrega sobre data/view/models, com categorias prefixadas", {
  expect_no_error(experiments_registry())
  ids <- vapply(trama_collection()$categories, function(k) k$id, "")
  expect_true(all(startsWith(ids, "exp_")))
  expect_setequal(ids, c("exp_planejar", "exp_analisar", "exp_avaliar"))
})

test_that("todo nó tem help no formato e todo param do spec existe no fn com o mesmo default", {
  reg <- experiments_registry()
  for (n in nos_exp(reg)) {
    for (secao in c("## Descrição", "## Parâmetros", "## Valor", "## Exemplos", "## Veja também")) {
      expect_match(n$help, secao, fixed = TRUE, info = n$id)
    }
    f <- formals(n$fn)
    for (nm in names(n$params)) {
      expect_true(nm %in% names(f), info = paste(n$id, nm))
      expect_equal(n$params[[nm]]$default, eval(f[[nm]]), info = paste(n$id, nm))
    }
  }
})

test_that("referências cruzadas existem e os exemplos da ajuda rodam", {
  reg <- experiments_registry()
  conhecidos <- c(names(reg$nodes), names(reg$types))
  env <- new.env(parent = asNamespace("trama")); env$reg <- reg
  for (n in nos_exp(reg)) {
    citados <- unique(gsub("`", "", regmatches(n$help, gregexpr("`[a-z][a-z0-9_.]*/[a-z0-9_]+`", n$help))[[1]]))
    for (id in citados) expect_true(id %in% conhecidos, info = paste(n$id, "cita", id))
    for (b in regmatches(n$help, gregexpr("```r\n.*?\n```", n$help))[[1]]) {
      f <- eval(parse(text = sub("^```r\n", "", sub("\n```$", "", b))), envir = env)
      if (!inherits(f, "tr_flow")) next
      ultimo <- utils::tail(names(trama::tr_flow_doc(f)$nodes), 1L)
      tipo <- trama::tr_flow_doc(f)$nodes[[ultimo]]$type
      # Nó de várias saídas: calcula pela primeira porta.
      porta <- if (length(reg$nodes[[tipo]]$outputs) > 1L) names(reg$nodes[[tipo]]$outputs)[[1]]
      expect_no_error(rodar(f, ultimo, porta), message = n$id)
    }
  }
})

test_that("o fn de todo nó está exportado no NAMESPACE", {
  linhas <- grep("^export\\(", readLines("../../NAMESPACE"), value = TRUE)
  exportados <- sub("^export\\((.*)\\)$", "\\1", linhas)
  ns <- asNamespace("trama.experiments")
  fns <- Filter(is.function, mget(ls(ns, all.names = TRUE), envir = ns, inherits = FALSE))
  for (n in nos_exp(experiments_registry())) {
    nome <- names(fns)[vapply(fns, identical, TRUE, n$fn)][[1]]
    expect_true(nome %in% exportados, info = n$id)
  }
})

test_that("o tipo plan recusa o que não é plano e vira tabela", {
  expect_error(.tr_exp_plano_conferir(list()), class = "tr_experiments_error_not_a_plan")
  expect_match(as.character(trama::tr_catalog_json(experiments_registry())), "\"from\":\"experiments/plan\"",
               fixed = TRUE)
})
