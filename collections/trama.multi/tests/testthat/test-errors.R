test_that("coluna citada inexistente ou não numérica é erro alto, nomeando-a", {
  d <- iris_t()
  err <- tryCatch(.tr_multi_variaveis(d, "Sepal.Length, Sepla.Width"), condition = identity)
  expect_s3_class(err, "tr_multi_error_unknown_column")
  expect_match(conditionMessage(err), "Sepla.Width", fixed = TRUE)
  err <- tryCatch(.tr_multi_variaveis(d, "Sepal.Length, Species"), condition = identity)
  expect_s3_class(err, "tr_multi_error_not_numeric")
  expect_match(conditionMessage(err), "Species", fixed = TRUE)
})

test_that("variáveis em branco são as numéricas, menos as excluídas", {
  d <- iris_t()
  expect_equal(.tr_multi_variaveis(d, ""), names(d)[1:4])
  expect_equal(.tr_multi_variaveis(d, "", excluir = "Petal.Width"), names(d)[1:3])
  expect_error(.tr_multi_variaveis(d, "Sepal.Length"), class = "tr_multi_error_too_few_variables")
})

test_that("faltante, constante e singular viram erro com a pista", {
  d <- iris_t()
  d$Sepal.Length[c(3, 9)] <- NA
  err <- tryCatch(.tr_multi_matriz(d, names(d)[1:4], "multi/pca"), condition = identity)
  expect_s3_class(err, "tr_multi_error_missing_values")
  expect_match(conditionMessage(err), "2 linha", fixed = TRUE)
  expect_match(conditionMessage(err), "data/drop_na", fixed = TRUE)
  d <- iris_t(); d$k <- 1
  expect_error(.tr_multi_matriz(d, c("Sepal.Length", "k"), "x"),
               class = "tr_multi_error_constant_variable")
  d <- iris_t(); d$soma <- d$Sepal.Length + d$Sepal.Width
  m <- .tr_multi_matriz(d, c("Sepal.Length", "Sepal.Width", "soma"), "x")
  expect_error(.tr_multi_correlacao(m, "x"), class = "tr_multi_error_singular_matrix")
})

test_that("inteiro e enum fora da faixa são erro classificado", {
  expect_error(.tr_multi_int(0, "fatores", min = 1), class = "tr_multi_error_bad_option")
  expect_error(.tr_multi_enum("varimaxx", c("varimax"), "rotacao"), class = "tr_multi_error_bad_option")
  expect_equal(.tr_multi_int(2, "fatores", min = 1), 2L)
})

test_that("toda classe tr_multi_error_* usada no código está em tr_multi_errors()", {
  # Varre o NAMESPACE, e não os `.R` do disco: sob `R CMD check` o cwd muda e a
  # varredura pelo disco pularia em silêncio — mesma lição das irmãs.
  ns <- asNamespace("trama.multi")
  objs <- mget(ls(ns, all.names = TRUE), envir = ns, inherits = FALSE)
  txt <- unlist(lapply(Filter(is.function, objs),
                       function(f) deparse(f, width.cutoff = 500L)))
  used <- unique(unlist(regmatches(
    txt, gregexpr('(?<=")tr_multi_error_[a-z_]+(?=")', txt, perl = TRUE))))
  documented <- tr_multi_errors()$class
  expect_setequal(setdiff(used, documented), character())
  expect_setequal(setdiff(documented, used), character())
})
