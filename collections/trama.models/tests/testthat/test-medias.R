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

test_that("Dunnett exato bate com multcomp::glht(mcp(Dunnett)) (oráculo)", {
  skip_if_not_installed("multcomp")
  # Oráculo: multcomp (Hothorn, Bretz & Westfall 2008), mesma integração da t
  # multivariada. Com 2 contrastes a integral é exata (tolerância 1e-6); com 5,
  # o erro de Genz-Bretz é de ~1e-3 (tolerância absoluta 2e-3).
  casos <- list(list(d = ex("PlantGrowth"), y = "weight", x = "group", ctl = "ctrl", tol = 1e-6),
                list(d = ex("InsectSprays"), y = "count", x = "spray", ctl = "A", tol = 2e-3))
  for (k in casos) {
    dic <- tr_models_anova_dic(k$d, k$y, k$x)
    e <- tr_models_emmeans(dic, k$x)
    d <- tr_models_pairwise(e, "contra controle", k$ctl, "dunnett", .seed = 7L)$tabela
    dd <- as.data.frame(k$d); dd[[k$x]] <- stats::relevel(factor(dd[[k$x]]), k$ctl)
    fit <- stats::aov(stats::reformulate(k$x, k$y), dd)
    set.seed(7)
    g <- multcomp::glht(fit, linfct = do.call(multcomp::mcp, stats::setNames(list("Dunnett"), k$x)))
    ref_p <- unname(summary(g)$test$pvalues)
    set.seed(7)
    ref_ci <- stats::confint(g)$confint
    expect_equal(d$p_valor, ref_p, tolerance = k$tol, ignore_attr = TRUE)
    expect_equal(d$li_95, unname(ref_ci[, "lwr"]), tolerance = k$tol)
    expect_equal(d$ls_95, unname(ref_ci[, "upr"]), tolerance = k$tol)
  }
  # PlantGrowth, valores de referência (multcomp 2 contrastes, exato):
  d <- tr_models_pairwise(tr_models_emmeans(tr_models_anova_dic(ex("PlantGrowth"), "weight", "group"), "group"),
                          "contra controle", "ctrl", "dunnett")$tabela
  expect_equal(d$p_valor, c(0.3226957, 0.1534859), tolerance = 1e-6)
})

test_that("Dunnett exato é reprodutível com a semente do nó", {
  e <- tr_models_emmeans(tr_models_anova_dic(ex("InsectSprays"), "count", "spray"), "spray")
  a <- tr_models_pairwise(e, "contra controle", "A", "dunnett", .seed = 11L)$tabela
  b <- tr_models_pairwise(e, "contra controle", "A", "dunnett", .seed = 11L)$tabela
  expect_identical(a$p_valor, b$p_valor)
  expect_identical(a$li_95, b$li_95)
})
