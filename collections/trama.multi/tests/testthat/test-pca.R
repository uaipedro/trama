ua_t <- function() tr_multi_example("USArrests")
ua_vars <- c("Murder", "Assault", "UrbanPop", "Rape")

test_that("paridade com prcomp, até o sinal, e colunas CP", {
  d <- iris_t()
  x <- tr_multi_pca(d)
  ref <- stats::prcomp(as.matrix(d[1:4]), center = TRUE, scale. = TRUE)
  expect_s3_class(x, "tr_multi_pca")
  expect_equal(x$variaveis, names(d)[1:4])
  expect_equal(colnames(x$ajuste$x), paste0("CP", 1:4))
  expect_equal(colnames(x$ajuste$rotation), paste0("CP", 1:4))
  expect_equal(x$ajuste$sdev, ref$sdev)
  expect_equal_ate_sinal(x$ajuste$rotation, ref$rotation)
  expect_equal_ate_sinal(x$ajuste$x, ref$x)
})

test_that("USArrests: sem padronizar, Assault domina o CP1", {
  bruta <- tr_multi_pca(ua_t(), padronizar = FALSE)
  padr <- tr_multi_pca(ua_t())
  expect_false(bruta$padronizado)
  expect_equal(names(which.max(abs(bruta$ajuste$rotation[, "CP1"]))), "Assault")
  expect_gt(abs(bruta$ajuste$rotation["Assault", "CP1"]), .99)
  expect_gt(tr_multi_pca_variance(bruta)$proporcao[[1]], .95)
  v <- tr_multi_pca_variance(padr)
  expect_equal(v$proporcao[[1]], .6201, tolerance = 1e-3)
  expect_lt(max(abs(padr$ajuste$rotation[, "CP1"])), .6)
})

test_that("recusas: coluna de texto, faltante, uma variável só", {
  expect_error(tr_multi_pca(iris_t(), "Sepal.Length, Species"), class = "tr_multi_error_not_numeric")
  expect_error(tr_multi_pca(iris_t(), "Sepal.Length"), class = "tr_multi_error_too_few_variables")
  d <- iris_t(); d$Sepal.Width[3] <- NA
  expect_error(tr_multi_pca(d), class = "tr_multi_error_missing_values")
})

test_that("tabela de variância soma 1 e é coerente", {
  v <- tr_multi_pca_variance(tr_multi_pca(ua_t()))
  expect_named(v, c("componente", "autovalor", "desvio", "proporcao", "acumulada"))
  expect_equal(v$componente, paste0("CP", 1:4))
  expect_equal(sum(v$proporcao), 1)
  expect_equal(v$acumulada[[4]], 1)
  expect_equal(sum(v$autovalor), 4)
  expect_equal(v$desvio^2, v$autovalor)
})

test_that("cargas: correlações são cor(X, escores) nos dois modos", {
  for (padr in c(TRUE, FALSE)) {
    x <- tr_multi_pca(ua_t(), padronizar = padr)
    l <- tr_multi_pca_loadings(x)
    expect_named(l, c("variavel", paste0("CP", 1:4)))
    esperado <- stats::cor(as.matrix(ua_t()[ua_vars]), x$ajuste$x)
    expect_equal(unname(as.matrix(l[-1])), unname(esperado), info = padr)
    expect_equal(unname(rowSums(as.matrix(l[-1])^2)), rep(1, 4))
  }
  x <- tr_multi_pca(ua_t())
  a <- tr_multi_pca_loadings(x, "autovetores", 2L)
  expect_named(a, c("variavel", "CP1", "CP2"))
  expect_equal(unname(as.matrix(a[-1])), unname(x$ajuste$rotation[, 1:2]))
  expect_error(tr_multi_pca_loadings(x, componentes = 5L), class = "tr_multi_error_bad_option")
  expect_error(tr_multi_pca_loadings(x, tipo = "cargas"), class = "tr_multi_error_bad_option")
})

test_that("gráficos devolvem ggplot com a dimensão da view e desenham", {
  x <- tr_multi_pca(iris_t())
  ua <- tr_multi_pca(ua_t(), padronizar = FALSE)
  ps <- list(
    tr_multi_scree(x), tr_multi_scree(ua),
    tr_multi_biplot(x), tr_multi_biplot(x, cor = "Species"),
    tr_multi_biplot(tr_multi_pca(ua_t()), 1L, 3L, cor = "UrbanPop", rotulo = "nome", setas = FALSE),
    tr_multi_correlation_circle(x), tr_multi_correlation_circle(ua, 2L, 3L),
    tr_multi_plot_correlation(tr_multi_example("questionario")),
    tr_multi_plot_correlation(iris_t(), ordenar = FALSE, valores = FALSE)
  )
  for (p in ps) {
    expect_s3_class(p, "ggplot")
    expect_length(attr(p, "tr_view_dim"), 2L)
    expect_no_error(ggplot2::ggplot_build(p))
  }
  expect_error(tr_multi_biplot(x, 1L, 1L), class = "tr_multi_error_bad_option")
  expect_error(tr_multi_biplot(x, 5L), class = "tr_multi_error_bad_option")
  expect_error(tr_multi_biplot(x, cor = "Especie"), class = "tr_multi_error_unknown_column")
})

test_that("mapa de correlações ordena os blocos do questionário", {
  p <- tr_multi_plot_correlation(tr_multi_example("questionario"))
  ordem <- levels(p$data$v1)
  expect_setequal(ordem, c(paste0("ans", 1:5), paste0("soc", 1:5), paste0("org", 1:5)))
  # Cada fator fica contíguo: o soc5 invertido junto dos outros soc.
  pos <- split(seq_along(ordem), substr(ordem, 1, 3))
  for (g in c("ans", "soc")) expect_equal(diff(range(pos[[g]])), 4L, info = g)
  expect_equal(levels(tr_multi_plot_correlation(iris_t(), ordenar = FALSE)$data$v1), names(iris_t())[1:4])
})

test_that("adaptador: colunas não usadas seguidas dos escores", {
  x <- tr_multi_pca(iris_t())
  t <- .tr_multi_pca_tabela(x)
  expect_s3_class(t, "tbl_df")
  expect_named(t, c("Species", paste0("CP", 1:4)))
  expect_equal(nrow(t), 150L)
  expect_named(.tr_multi_pca_tabela(tr_multi_pca(ua_t(), "Murder, Assault")),
               c("nome", "UrbanPop", "Rape", "CP1", "CP2"))
})

test_that("resumo do card", {
  r <- .tr_multi_pca_resumo(tr_multi_pca(ua_t()))
  expect_equal(r$variaveis, 4L)
  expect_equal(r$observacoes, 50L)
  expect_true(r$padronizado)
  expect_equal(r$variancia_cp1, "62,0%")
  expect_equal(r$autovalor_maior_que_1, 1L)
  expect_null(.tr_multi_pca_resumo(tr_multi_pca(ua_t(), padronizar = FALSE))$autovalor_maior_que_1)
})

test_that("tipo: store é funil, e restore devolve o mesmo objeto", {
  ty <- multi_pca_type()
  f <- tempfile(fileext = ".rds")
  expect_error(ty$store(iris_t(), f), class = "tr_multi_error_not_a_pca")
  x <- tr_multi_pca(iris_t())
  ty$store(x, f)
  expect_identical(ty$restore(f), x)
  expect_error(tr_multi_scree(iris_t()), class = "tr_multi_error_not_a_pca")
})

test_that("preview do tipo é PNG pela view", {
  skip_if_not_installed("png")
  art <- multi_pca_type()$preview(tr_multi_pca(iris_t()), ctx_tmp())
  expect_equal(art$renderer, "trama/image")
  expect_true(file.exists(art$files$png))
  expect_no_error(png::readPNG(art$files$png))
})

test_that("motor: multi/example -> multi/pca -> view/points pelo adaptador", {
  reg <- multi_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("iris", "multi/example", dataset = "iris") |>
    trama::tr_add("pca", "multi/pca", from = "iris") |>
    trama::tr_add("pts", "view/points", x = "CP1", y = "CP2", cor = "Species", from = "pca")
  p <- rodar(f, "pts")
  expect_s3_class(p, "ggplot")
  expect_true(all(c("CP1", "CP2", "Species") %in% names(p$data)))
})
