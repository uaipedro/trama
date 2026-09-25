# Discriminante: paridade com a MASS, os números de livro da iris e o motor.

lda_iris <- function(...) tr_multi_discriminant(iris_t(), resposta = "Species", ...)
X_iris <- function() as.matrix(datasets::iris[1:4])

test_that("a LDA bate com MASS::lda: scaling até o sinal e previsões idênticas", {
  m <- lda_iris()
  ref <- MASS::lda(X_iris(), datasets::iris$Species)
  expect_s3_class(m, c("tr_multi_lda", "tr_models_fit"), exact = TRUE)
  expect_equal(m$preditores, names(datasets::iris)[1:4])
  expect_equal_ate_sinal(m$ajuste$scaling, ref$scaling)
  cl <- trama.models::tr_models_predict(m)
  pr <- stats::predict(ref, X_iris())
  expect_identical(as.character(cl$previsto), as.character(pr$class))
  expect_equal(unname(as.matrix(cl[paste0("prob_", levels(datasets::iris$Species))])),
               unname(pr$posterior))
  expect_equal(names(cl), c(names(datasets::iris), "previsto", "prob_setosa",
                            "prob_versicolor", "prob_virginica", "LD1", "LD2"))
})

test_that("priors iguais chegam à lda", {
  d <- iris_t()[c(1:20, 51:150), ]
  m <- tr_multi_discriminant(d, resposta = "Species", priors = "iguais")
  expect_equal(unname(m$ajuste$prior), rep(1 / 3, 3))
  expect_equal(unname(lda_iris()$ajuste$prior), rep(1 / 3, 3))
  m2 <- tr_multi_discriminant(d, resposta = "Species")
  expect_equal(unname(m2$ajuste$prior), c(20, 50, 50) / 120)
})

test_that("a confusão cruzada tem o acerto da MASS com CV = TRUE", {
  cv <- MASS::lda(X_iris(), datasets::iris$Species, CV = TRUE)
  tab <- trama.models::tr_models_confusion(lda_iris())
  expect_equal(names(tab), c("real", "setosa", "versicolor", "virginica",
                             "total", "acertos", "taxa_acerto"))
  expect_equal(tab$real, c("setosa", "versicolor", "virginica", "total"))
  expect_equal(tab$taxa_acerto[4], mean(cv$class == datasets::iris$Species))
  expect_equal(unname(as.matrix(tab[1:3, 2:4])),
               unname(matrix(as.integer(table(datasets::iris$Species, cv$class)), 3)))
  expect_equal(tab$total[4], 150L)
  expect_equal(unname(unlist(tab[4, 2:4])), unname(as.integer(colSums(tab[1:3, 2:4]))))
  res <- trama.models::tr_models_confusion(lda_iris(), validacao = "resubstituição")
  expect_equal(res$acertos[4], 147L)
  expect_error(trama.models::tr_models_confusion(lda_iris(), validacao = "holdout"),
               class = "tr_models_error_bad_option")
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
  cl <- trama.models::tr_models_predict(q)
  expect_identical(as.character(cl$previsto),
                   as.character(stats::predict(ref, X_iris())$class))
  expect_false("LD1" %in% names(cl))
  cv <- MASS::qda(X_iris(), datasets::iris$Species, CV = TRUE)
  expect_equal(trama.models::tr_models_confusion(q)$taxa_acerto[4], mean(cv$class == datasets::iris$Species))
})

test_that("M de Box: iris rejeita (≈140,94, 20 gl), vinhos não", {
  b <- tr_multi_box_m(iris_t(), grupo = "Species")
  expect_s3_class(b, "tr_test")
  expect_equal(b$estatistica, 140.94, tolerance = 1e-4)
  expect_equal(b$extra$m_box, 146.66, tolerance = 1e-4)
  expect_equal(b$gl, "20")
  expect_lt(b$p_valor, .001)
  expect_equal(b$decisao_5, "rejeita H0")
  # A linha de tabela, pelo adaptador da `data`, guarda o que a tabela antiga
  # tinha de útil: estatística, gl, p-valor, decisão e o M.
  tb <- trama::tr_test_table(b)
  expect_equal(nrow(tb), 1L)
  expect_true(all(c("estatistica", "gl", "p_valor", "decisao_5", "conclusao", "extra_m_box") %in% names(tb)))
  v <- tr_multi_box_m(tr_multi_example("vinhos"), grupo = "cultivar")
  expect_equal(v$gl, "42")
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
  expect_error(tr_multi_discriminant(d[1:50, ], resposta = "Species"),
               class = "tr_multi_error_one_group")
  pequeno <- d[c(1:50, 51:53, 101:150), ]
  err <- tryCatch(tr_multi_discriminant(pequeno, resposta = "Species", metodo = "quadrática"),
                  condition = identity)
  expect_s3_class(err, "tr_multi_error_small_group")
  expect_match(conditionMessage(err), "'versicolor' tem 3", fixed = TRUE)
  expect_no_error(tr_multi_discriminant(pequeno, resposta = "Species"))
  expect_error(tr_multi_discriminant(d[c(1:50, 51, 101:150), ], resposta = "Species"),
               class = "tr_multi_error_small_group")
  na <- d; na$Species[c(2, 7)] <- NA
  err <- tryCatch(tr_multi_discriminant(na, resposta = "Species"), condition = identity)
  expect_s3_class(err, "tr_multi_error_missing_values")
  expect_match(conditionMessage(err), "2 linha", fixed = TRUE)
  col <- d; col$soma <- col$Sepal.Length + col$Petal.Length
  expect_error(tr_multi_discriminant(col, resposta = "Species"),
               class = "tr_multi_error_singular_matrix")
  expect_error(tr_multi_discriminant(d, resposta = ""), class = "tr_multi_error_blank_param")
  expect_error(tr_multi_discriminant(d, resposta = "Especie"), class = "tr_multi_error_unknown_column")
  expect_error(tr_multi_discriminant(d, resposta = "Species", metodo = "logística"),
               class = "tr_multi_error_bad_option")
})

test_that("o gráfico: disperso com 3 grupos, densidade com 2, eixos auxiliares na QDA", {
  p <- tr_multi_plot_discriminant(lda_iris())
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$x, "LD1 (99,1% da separação)", fixed = TRUE)
  expect_true(any(vapply(p$layers, function(l) inherits(l$stat, "StatEllipse"), TRUE)))
  expect_no_error(ggplot2::ggplot_build(p))
  sem <- tr_multi_plot_discriminant(lda_iris(), elipses = FALSE, x = 2L, y = 1L)
  expect_false(any(vapply(sem$layers, function(l) inherits(l$stat, "StatEllipse"), TRUE)))
  dois <- tr_multi_plot_discriminant(tr_multi_discriminant(iris_t()[51:150, ], resposta = "Species"))
  expect_true(any(vapply(dois$layers, function(l) inherits(l$geom, "GeomDensity"), TRUE)))
  expect_no_error(ggplot2::ggplot_build(dois))
  q <- tr_multi_plot_discriminant(lda_iris(metodo = "quadrática"))
  expect_match(q$labels$subtitle, "auxiliar", fixed = TRUE)
  expect_no_error(ggplot2::ggplot_build(q))
  expect_error(tr_multi_plot_discriminant(lda_iris(), x = 3L), class = "tr_multi_error_bad_option")
  expect_error(tr_multi_plot_discriminant(lda_iris(), x = 1L, y = 1L),
               class = "tr_multi_error_bad_option")
})

test_that("contrato: info, stats, importância, coeficientes e a tabela do adaptador", {
  m <- lda_iris()
  i <- trama.models::tr_models_info(m)
  expect_equal(i$tarefa, "classificacao")
  expect_equal(i$resposta, "Species")
  expect_equal(i$niveis, c("setosa", "versicolor", "virginica"))
  expect_equal(i$n, 150L)
  s <- trama.models::tr_models_fit_stats(m)
  expect_equal(nrow(s), 1L)
  expect_equal(s$acerto_resubstituicao, 0.98)
  expect_equal(s$lambda_wilks, tr_multi_discriminant_functions(m)$lambda_wilks[1], tolerance = 1e-10)
  imp <- trama.models::tr_models_importance_table(m)
  expect_setequal(imp$termo, m$preditores)
  expect_true(all(diff(imp$importancia) <= 0))
  expect_error(trama.models::tr_models_importance_table(lda_iris(metodo = "quadrática")),
               class = "tr_models_error_not_applicable")
  err <- tryCatch(trama.models::tr_models_coefficients(m), condition = identity)
  expect_s3_class(err, "tr_models_error_not_applicable")
  expect_match(conditionMessage(err), "multi/discriminant_functions", fixed = TRUE)
  # O adaptador models/fit -> data/table é o treino classificado.
  expect_identical(trama.models::tr_models_as_table(m), trama.models::tr_models_predict(m))
})

test_that("round-trip no store de models/fit, e o card é o plano discriminante", {
  reg <- multi_registry()
  tipo <- reg$types[["models/fit"]]
  m <- lda_iris()
  path <- tempfile(fileext = ".rds")
  tipo$store(m, path)
  back <- tipo$restore(path)
  expect_identical(back, m)
  expect_equal(trama.models::tr_models_confusion(back)$acertos[4], 147L)
  skip_if_not_installed("png")
  art <- tipo$preview(m, ctx_tmp())
  expect_true(file.exists(art$files$png))
  expect_equal(dim(png::readPNG(art$files$png))[2], 1600L)
})

test_that("leitor da discriminante recusa outro modelo com erro de classe", {
  lm_fit <- trama.models::tr_models_lm(iris_t(), resposta = "Sepal.Length", preditores = "Petal.Length")
  for (f in list(tr_multi_discriminant_functions, tr_multi_plot_discriminant,
                 tr_multi_jackknife_discriminant)) {
    err <- tryCatch(f(lm_fit), condition = identity)
    expect_s3_class(err, "tr_multi_error_not_a_lda")
    expect_match(conditionMessage(err), "precisa de uma Discriminante", fixed = TRUE)
  }
  expect_error(tr_multi_discriminant_functions(tr_multi_logistic(tr_multi_example("pima"), resposta = "diabetes")),
               class = "tr_multi_error_not_a_lda")
})

test_that("motor: exemplo -> discriminante -> models/predict com dados, e -> view/points", {
  reg <- multi_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("iris", "multi/example", dataset = "iris") |>
    trama::tr_add("lda", "multi/discriminant", resposta = "Species", from = "iris") |>
    trama::tr_add("cabeca", "data/slice_head", n = 7L, from = "iris") |>
    trama::tr_add("cl", "models/predict", from = "lda") |>
    trama::tr_link("cabeca", "cl:dados") |>
    trama::tr_add("so_treino", "models/predict", from = "lda") |>
    trama::tr_add("pts", "view/points", x = "LD1", y = "LD2", cor = "previsto", from = "lda")
  cl <- rodar(f, "cl")
  expect_equal(nrow(cl), 7L)
  expect_true(all(cl$previsto == "setosa"))
  expect_equal(nrow(rodar(f, "so_treino")), 150L)
  expect_s3_class(rodar(f, "pts"), "ggplot")
})
