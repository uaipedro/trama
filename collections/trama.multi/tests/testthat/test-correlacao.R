test_that("matriz geral: correlação e covariância batem com cor() e cov()", {
  d <- tr_multi_example("USArrests")
  X <- as.matrix(d[-1])
  r <- tr_multi_correlation_matrix(d)
  expect_equal(names(r), c("variavel", colnames(X)))
  expect_equal(r$variavel, colnames(X))
  expect_equal(unname(as.matrix(r[-1])), unname(stats::cor(X)))
  cv <- tr_multi_correlation_matrix(d, matriz = "covariância")
  expect_equal(unname(as.matrix(cv[-1])), unname(stats::cov(X)))
  sp <- tr_multi_correlation_matrix(d, cols = "Murder, Assault", metodo = "spearman")
  expect_equal(sp$Assault[[1]], stats::cor(d$Murder, d$Assault, method = "spearman"))
})

test_that("covariância fora de Pearson é recusa, e não covariância dos postos", {
  err <- tryCatch(tr_multi_correlation_matrix(iris_t(), matriz = "covariância", metodo = "kendall"),
                  error = identity)
  expect_s3_class(err, "tr_multi_error_bad_option")
  expect_match(conditionMessage(err), "Pearson", fixed = TRUE)
})

test_that("por grupo: um bloco por grupo e a combinada, que é a covariância da LDA", {
  d <- iris_t()
  m <- tr_multi_correlation_matrix(d, matriz = "covariância", grupo = "Species")
  expect_equal(names(m)[1:2], c("grupo", "variavel"))
  expect_equal(unique(m$grupo), c("setosa", "versicolor", "virginica", "combinada"))
  X <- as.matrix(d[1:4])
  seto <- as.matrix(m[m$grupo == "setosa", -(1:2)])
  expect_equal(unname(seto), unname(stats::cov(X[d$Species == "setosa", ])))
  # A combinada é a média das covariâncias ponderada por n - 1: com grupos do
  # mesmo tamanho, a média simples das três.
  comb <- as.matrix(m[m$grupo == "combinada", -(1:2)])
  media <- Reduce(`+`, lapply(levels(d$Species), function(l) stats::cov(X[d$Species == l, ]))) / 3
  expect_equal(unname(comb), unname(media))
  rc <- tr_multi_correlation_matrix(d, grupo = "Species")
  expect_equal(unname(as.matrix(rc[rc$grupo == "combinada", -(1:2)])), unname(stats::cov2cor(media)))
  # Sem combinada em postos.
  expect_false("combinada" %in% tr_multi_correlation_matrix(d, metodo = "spearman", grupo = "Species")$grupo)
})

test_that("por grupo: constante dentro do grupo, grupo pequeno e faltante no grupo são erro", {
  d <- iris_t()
  d$Petal.Width[d$Species == "setosa"] <- 0.2
  err <- tryCatch(tr_multi_correlation_matrix(d, grupo = "Species"), error = identity)
  expect_s3_class(err, "tr_multi_error_constant_variable")
  expect_match(conditionMessage(err), "setosa", fixed = TRUE)
  d <- iris_t()[c(1:50, 51:52, 101:150), ]
  expect_error(tr_multi_correlation_matrix(d, grupo = "Species"), class = "tr_multi_error_small_group")
  d <- iris_t(); d$Species[3] <- NA
  expect_error(tr_multi_correlation_matrix(d, grupo = "Species"), class = "tr_multi_error_missing_values")
  expect_error(tr_multi_correlation_matrix(iris_t(), grupo = "Especie"),
               class = "tr_multi_error_unknown_column")
})

test_that("no motor: a matriz sai como data/table e segue para a data", {
  reg <- multi_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("v", "multi/example", dataset = "vinhos") |>
    trama::tr_add("m", "multi/correlation_matrix", grupo = "cultivar", from = "v") |>
    trama::tr_add("so", "data/filter", expr = "grupo == 'combinada'", from = "m")
  out <- rodar(f, "so")
  expect_equal(nrow(out), 6L)
  expect_equal(out$variavel[[1]], "alcool")
})
