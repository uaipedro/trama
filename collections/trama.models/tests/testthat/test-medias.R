test_that("letras: casos de resposta conhecida", {
  # Ninguém difere: todos "a".
  expect_equal(.tr_models_letras(c(3, 2, 1), matrix(FALSE, 3, 3)), c("a", "a", "a"))
  # Todos diferem: uma letra cada, "a" para a maior.
  d <- matrix(TRUE, 3, 3); diag(d) <- FALSE
  expect_equal(.tr_models_letras(c(1, 3, 2), d), c("c", "a", "b"))
  # Escada: 1 = 2, 2 = 3, 1 ≠ 3.
  d <- matrix(FALSE, 3, 3); d[1, 3] <- d[3, 1] <- TRUE
  expect_equal(.tr_models_letras(c(10, 8, 6), d), c("a", "ab", "b"))
})

test_that("as letras do milho acham o híbrido plantado acima", {
  e <- tr_models_emmeans(milho_dbc(), "hibrido")
  expect_equal(e$tabela$grupo[e$tabela$hibrido == "H3"], "a")
  expect_false(grepl("a", e$tabela$grupo[e$tabela$hibrido == "H1"]))
  # As médias ajustadas do balanceado são as médias da tabela.
  expect_equal(e$tabela$media, as.numeric(tapply(ex("milho_dbc")$producao, ex("milho_dbc")$hibrido, mean)))
})

test_that("letras concordam com os p-valores do Tukey de cada par", {
  e <- tr_models_emmeans(milho_dbc(), "hibrido")
  pp <- tr_models_pairwise(e)$tabela
  let <- stats::setNames(e$tabela$grupo, as.character(e$tabela$hibrido))
  for (i in seq_len(nrow(pp))) {
    par <- strsplit(pp$termo[[i]], " - ", fixed = TRUE)[[1]]
    comum <- length(intersect(strsplit(let[[par[1]]], "")[[1]], strsplit(let[[par[2]]], "")[[1]])) > 0
    expect_equal(comum, pp$p_valor[[i]] >= 0.05, info = pp$termo[[i]])
  }
})

test_that("desdobramento com 'por' faz letras dentro de cada condição", {
  fa <- tr_models_anova_factorial(ex("ToothGrowth"), "len", "supp, dose")
  e <- tr_models_emmeans(fa, "dose", por = "supp")
  expect_equal(nrow(e$tabela), 6L)
  oj <- e$tabela[e$tabela$supp == "OJ", ]
  expect_equal(oj$grupo[oj$dose == "0.5"], "b")
  p <- tr_models_pairwise(e)$tabela
  expect_true(all(grepl("| supp", p$termo, fixed = TRUE)))
  expect_s3_class(tr_models_plot_means(e), "ggplot")
})

test_that("Dunnett contra o controle, e recusas claras", {
  dic <- tr_models_anova_dic(ex("PlantGrowth"), "weight", "group")
  e <- tr_models_emmeans(dic, "group")
  d <- tr_models_pairwise(e, "contra controle", "ctrl", "dunnett")$tabela
  expect_equal(nrow(d), 2L)
  expect_error(tr_models_pairwise(e, "contra controle", "controle"), class = "tr_models_error_unknown_level")
  expect_error(tr_models_pairwise(e, "todos os pares", ajuste = "dunnett"), class = "tr_models_error_bad_option")
  l <- tr_models_lm(ex("mtcars"), formula = "mpg ~ cyl")
  expect_error(tr_models_emmeans(l, "cyl"), class = "tr_models_error_not_applicable")
})

test_that("GLM na escala da resposta e parcela subdividida pelo misto", {
  g <- tr_models_glm(ex("InsectSprays"), "count", "spray")
  e <- tr_models_emmeans(g, "spray")
  expect_equal(e$tabela$media[[1]], mean(ex("InsectSprays")$count[ex("InsectSprays")$spray == "A"]),
               tolerance = 1e-6)
  sp <- tr_models_anova_split_plot(ex("aveia"), "producao", "variedade", "nitrogenio", "bloco")
  en <- tr_models_emmeans(sp, "nitrogenio")
  expect_true(all(is.finite(en$tabela$erro_padrao)))
})
