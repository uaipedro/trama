# Discriminante: paridade com a MASS, os números de livro da iris e o motor.

lda_iris <- function(...) tr_multi_discriminant(iris_t(), grupo = "Species", ...)
X_iris <- function() as.matrix(datasets::iris[1:4])

test_that("a LDA bate com MASS::lda: scaling até o sinal e previsões idênticas", {
  m <- lda_iris()
  ref <- MASS::lda(X_iris(), datasets::iris$Species)
  expect_s3_class(m, "tr_multi_lda")
  expect_equal(m$preditores, names(datasets::iris)[1:4])
  expect_equal_ate_sinal(m$ajuste$scaling, ref$scaling)
  cl <- tr_multi_classify(m)
  pr <- stats::predict(ref, X_iris())
  expect_identical(as.character(cl$previsto), as.character(pr$class))
  expect_equal(unname(as.matrix(cl[paste0("prob_", levels(datasets::iris$Species))])),
               unname(pr$posterior))
  expect_equal(names(cl), c(names(datasets::iris), "previsto", "prob_setosa",
                            "prob_versicolor", "prob_virginica", "LD1", "LD2"))
})

test_that("priors iguais chegam à lda", {
  d <- iris_t()[c(1:20, 51:150), ]
  m <- tr_multi_discriminant(d, grupo = "Species", priors = "iguais")
  expect_equal(unname(m$ajuste$prior), rep(1 / 3, 3))
  expect_equal(unname(lda_iris()$ajuste$prior), rep(1 / 3, 3))
  m2 <- tr_multi_discriminant(d, grupo = "Species")
  expect_equal(unname(m2$ajuste$prior), c(20, 50, 50) / 120)
})

test_that("a confusão cruzada tem o acerto da MASS com CV = TRUE", {
  cv <- MASS::lda(X_iris(), datasets::iris$Species, CV = TRUE)
  tab <- tr_multi_confusion(lda_iris())
  expect_equal(names(tab), c("real", "setosa", "versicolor", "virginica",
                             "total", "acertos", "taxa_acerto"))
  expect_equal(tab$real, c("setosa", "versicolor", "virginica", "total"))
  expect_equal(tab$taxa_acerto[4], mean(cv$class == datasets::iris$Species))
  expect_equal(unname(as.matrix(tab[1:3, 2:4])),
               unname(matrix(as.integer(table(datasets::iris$Species, cv$class)), 3)))
  expect_equal(tab$total[4], 150L)
  expect_equal(unname(unlist(tab[4, 2:4])), unname(as.integer(colSums(tab[1:3, 2:4]))))
  res <- tr_multi_confusion(lda_iris(), validacao = "resubstituição")
  expect_equal(res$acertos[4], 147L)
  expect_error(tr_multi_confusion(lda_iris(), "holdout"), class = "tr_multi_error_bad_option")
})

test_that("funções discriminantes: Wilks e correlações canônicas da iris", {
  f <- tr_multi_discriminant_functions(lda_iris())
  expect_equal(names(f), c("funcao", "autovalor", "proporcao", "correlacao_canonica",
                           "lambda_wilks", "qui_quadrado", "gl", "p_valor"))
  expect_equal(f$autovalor, c(32.1919, 0.2854), tolerance = 1e-3)
  expect_equal(f$correlacao_canonica, c(0.9848, 0.4712), tolerance = 1e-3)
  expect_equal(f$lambda_wilks[1], 0.0234, tolerance = 1e-2)
  expect_equal(f$gl, c(8L, 3L))
  expect_lt(f$p_valor[2], .001)
  # Com priors proporcionais, os autovalores são o svd² da lda reescalado.
  ref <- MASS::lda(X_iris(), datasets::iris$Species)
  expect_equal(f$autovalor, ref$svd^2 * 2 / 147, tolerance = 1e-8)
  expect_equal(sum(f$proporcao), 1)
})

test_that("coeficientes brutos, padronizados e de estrutura", {
  m <- lda_iris()
  br <- tr_multi_discriminant_functions(m, "coeficientes")
  expect_equal(names(br), c("variavel", "LD1", "LD2"))
  expect_equal(unname(as.matrix(br[-1])), unname(m$ajuste$scaling))
  # Estrutura: correlação DENTRO dos grupos entre medida e escore, calculada à mão.
  X <- X_iris(); g <- datasets::iris$Species
  z <- stats::predict(m$ajuste, X)$x
  cent <- function(a) a - apply(a, 2, function(v) stats::ave(v, g))
  estr <- stats::cor(cent(X), cent(z))
  e <- tr_multi_discriminant_functions(m, "estrutura")
  expect_equal(unname(as.matrix(e[-1])), unname(estr), tolerance = 1e-8)
  pd <- tr_multi_discriminant_functions(m, "padronizados")
  expect_equal(pd$LD1, unname(br$LD1 * sqrt(diag(stats::cov(cent(X)) * 149 / 147))), tolerance = 1e-8)
})

test_that("a quadrática não tem funções, e o erro explica", {
  q <- lda_iris(metodo = "quadrática")
  err <- tryCatch(tr_multi_discriminant_functions(q), condition = identity)
  expect_s3_class(err, "tr_multi_error_bad_option")
  expect_match(conditionMessage(err), "QDA", fixed = TRUE)
})

test_that("QDA: bate com MASS::qda, classifica e confunde", {
  q <- lda_iris(metodo = "quadrática")
  ref <- MASS::qda(X_iris(), datasets::iris$Species)
  cl <- tr_multi_classify(q)
  expect_identical(as.character(cl$previsto),
                   as.character(stats::predict(ref, X_iris())$class))
  expect_false("LD1" %in% names(cl))
  cv <- MASS::qda(X_iris(), datasets::iris$Species, CV = TRUE)
  expect_equal(tr_multi_confusion(q)$taxa_acerto[4], mean(cv$class == datasets::iris$Species))
})

test_that("M de Box: iris rejeita (≈140,94, 20 gl), vinhos não", {
  b <- tr_multi_box_m(iris_t(), grupo = "Species")
  expect_equal(nrow(b), 1L)
  expect_equal(names(b), c("teste", "h0", "m", "qui_quadrado", "gl", "p_valor",
                           "decisao_5", "leitura"))
  expect_equal(b$qui_quadrado, 140.94, tolerance = 1e-4)
  expect_equal(b$m, 146.66, tolerance = 1e-4)
  expect_equal(b$gl, 20L)
  expect_lt(b$p_valor, .001)
  expect_equal(b$decisao_5, "rejeita H0")
  v <- tr_multi_box_m(tr_multi_example("vinhos"), grupo = "cultivar")
  expect_equal(v$gl, 42L)
  expect_gt(v$p_valor, .05)
  expect_equal(v$decisao_5, "não rejeita H0")
})

test_that("M de Box recusa covariância singular nomeando o grupo", {
  d <- iris_t()
  d$Sepal.Width[d$Species == "setosa"] <- 3
  err <- tryCatch(tr_multi_box_m(d, grupo = "Species"), condition = identity)
  expect_s3_class(err, "tr_multi_error_singular_matrix")
  expect_match(conditionMessage(err), "setosa", fixed = TRUE)
})

test_that("erros: grupo único, grupo pequeno, NA no grupo, colinear, coluna", {
  d <- iris_t()
  expect_error(tr_multi_discriminant(d[1:50, ], grupo = "Species"),
               class = "tr_multi_error_one_group")
  pequeno <- d[c(1:50, 51:53, 101:150), ]
  err <- tryCatch(tr_multi_discriminant(pequeno, grupo = "Species", metodo = "quadrática"),
                  condition = identity)
  expect_s3_class(err, "tr_multi_error_small_group")
  expect_match(conditionMessage(err), "'versicolor' tem 3", fixed = TRUE)
  expect_no_error(tr_multi_discriminant(pequeno, grupo = "Species"))
  expect_error(tr_multi_discriminant(d[c(1:50, 51, 101:150), ], grupo = "Species"),
               class = "tr_multi_error_small_group")
  na <- d; na$Species[c(2, 7)] <- NA
  err <- tryCatch(tr_multi_discriminant(na, grupo = "Species"), condition = identity)
  expect_s3_class(err, "tr_multi_error_missing_values")
  expect_match(conditionMessage(err), "2 linha", fixed = TRUE)
  col <- d; col$soma <- col$Sepal.Length + col$Petal.Length
  expect_error(tr_multi_discriminant(col, grupo = "Species"),
               class = "tr_multi_error_singular_matrix")
  expect_error(tr_multi_discriminant(d, grupo = ""), class = "tr_multi_error_blank_param")
  expect_error(tr_multi_discriminant(d, grupo = "Especie"), class = "tr_multi_error_unknown_column")
  expect_error(tr_multi_discriminant(d, grupo = "Species", metodo = "logística"),
               class = "tr_multi_error_bad_option")
})

test_that("níveis vazios do grupo são descartados", {
  d <- iris_t()[1:100, ]
  m <- tr_multi_discriminant(d, grupo = "Species")
  expect_equal(levels(tr_multi_classify(m)$previsto), c("setosa", "versicolor"))
  expect_equal(nrow(tr_multi_confusion(m)), 3L)
})

test_that("classify com tabela nova: colunas faltando, NA, e sem a coluna do grupo", {
  m <- lda_iris()
  novos <- iris_t()[c(1, 51, 101), 1:4]
  cl <- tr_multi_classify(m, novos)
  expect_equal(nrow(cl), 3L)
  expect_equal(as.character(cl$previsto), c("setosa", "versicolor", "virginica"))
  expect_equal(names(cl)[1:5], c(names(novos), "previsto"))
  # Uma linha só também classifica (desvio padrão NA não é recusa aqui).
  expect_equal(nrow(tr_multi_classify(m, novos[1, ])), 1L)
  err <- tryCatch(tr_multi_classify(m, novos[, 1:2]), condition = identity)
  expect_s3_class(err, "tr_multi_error_new_data_columns")
  expect_match(conditionMessage(err), "Petal.Length, Petal.Width", fixed = TRUE)
  na <- novos; na$Petal.Width[2] <- NA
  expect_error(tr_multi_classify(m, na), class = "tr_multi_error_missing_values")
  # Reclassificar a saída substitui as colunas, em vez de duplicá-las.
  de_novo <- tr_multi_classify(m, cl)
  expect_equal(names(de_novo), names(cl))
})

test_that("nomes de grupo com espaço viram prob_ citável", {
  m <- tr_multi_discriminant(tr_multi_example("caranguejos"), grupo = "grupo")
  cl <- tr_multi_classify(m)
  expect_true(all(c("prob_azul_fêmea", "prob_laranja_macho") %in% names(cl)))
  expect_equal(ncol(tr_multi_confusion(m)), 1L + 4L + 3L)
  expect_equal(length(m$preditores), 5L)
})

test_that("o gráfico: disperso com 3 grupos, densidade com 2, eixos auxiliares na QDA", {
  p <- tr_multi_plot_discriminant(lda_iris())
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$x, "LD1 (99,1% da separação)", fixed = TRUE)
  expect_true(any(vapply(p$layers, function(l) inherits(l$stat, "StatEllipse"), TRUE)))
  expect_no_error(ggplot2::ggplot_build(p))
  sem <- tr_multi_plot_discriminant(lda_iris(), elipses = FALSE, x = 2L, y = 1L)
  expect_false(any(vapply(sem$layers, function(l) inherits(l$stat, "StatEllipse"), TRUE)))
  dois <- tr_multi_plot_discriminant(tr_multi_discriminant(iris_t()[51:150, ], grupo = "Species"))
  expect_true(any(vapply(dois$layers, function(l) inherits(l$geom, "GeomDensity"), TRUE)))
  expect_no_error(ggplot2::ggplot_build(dois))
  q <- tr_multi_plot_discriminant(lda_iris(metodo = "quadrática"))
  expect_match(q$labels$subtitle, "auxiliar", fixed = TRUE)
  expect_no_error(ggplot2::ggplot_build(q))
  expect_error(tr_multi_plot_discriminant(lda_iris(), x = 3L), class = "tr_multi_error_bad_option")
  expect_error(tr_multi_plot_discriminant(lda_iris(), x = 1L, y = 1L),
               class = "tr_multi_error_bad_option")
})

test_that("o tipo multi/lda: guard, identidade, resumo, preview e adaptador", {
  tipo <- multi_lda_type()
  m <- lda_iris()
  path <- tempfile(fileext = ".rds")
  expect_error(tipo$store(list(a = 1), path), class = "tr_multi_error_not_a_lda")
  tipo$store(m, path)
  expect_identical(tipo$restore(path), m)
  r <- tipo$summary(m)
  expect_equal(r$metodo, "linear")
  expect_equal(r$preditores, 4L)
  expect_equal(r$acerto_resubstituicao, 0.98)
  expect_match(r$grupos, "setosa (50)", fixed = TRUE)
  tab <- .tr_multi_lda_tabela(m)
  expect_identical(tab, tr_multi_classify(m))
  # A porta é `multi/classifier`: o que não é modelo nenhum cai no erro do
  # classificador; um `tr_multi_lda` capenga ainda cai no da discriminante.
  expect_error(tr_multi_classify(list()), class = "tr_multi_error_not_a_classifier")
  expect_error(tr_multi_classify(structure(list(), class = "tr_multi_lda")),
               class = "tr_multi_error_not_a_lda")
  skip_if_not_installed("png")
  art <- tipo$preview(m, ctx_tmp())
  expect_true(file.exists(art$files$png))
  expect_equal(dim(png::readPNG(art$files$png))[2], 1600L)
})

test_that("motor: exemplo -> discriminante -> classify com novos, e -> view/points", {
  reg <- multi_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("iris", "multi/example", dataset = "iris") |>
    trama::tr_add("lda", "multi/discriminant", grupo = "Species", from = "iris") |>
    trama::tr_add("cabeca", "data/slice_head", n = 7L, from = "iris") |>
    trama::tr_add("cl", "multi/classify", from = "lda") |>
    trama::tr_link("cabeca", "cl:novos") |>
    trama::tr_add("so_treino", "multi/classify", from = "lda") |>
    trama::tr_add("pts", "view/points", x = "LD1", y = "LD2", cor = "previsto", from = "lda")
  cl <- rodar(f, "cl")
  expect_equal(nrow(cl), 7L)
  expect_true(all(cl$previsto == "setosa"))
  expect_equal(nrow(rodar(f, "so_treino")), 150L)
  expect_s3_class(rodar(f, "pts"), "ggplot")
})
