test_that("pressupostos batem com as funções de referência", {
  m <- milho_dbc()
  res <- stats::residuals(m$ajuste)
  expect_equal(tr_models_shapiro_residuals(m)$p_valor, stats::shapiro.test(res)$p.value)
  expect_equal(tr_models_bartlett(m)$p_valor, stats::bartlett.test(res, m$dados$hibrido)$p.value)
  expect_equal(tr_models_levene(m)$p_valor,
               car::leveneTest(res, m$dados$hibrido)$`Pr(>F)`[[1]])
  t <- tr_models_tukey_additivity(m)
  expect_s3_class(t, "tr_models_test")
  expect_match(t$gl, "^1; ")
})

test_that("Breusch-Pagan de Koenker bate com a conta à mão", {
  cars_m <- tr_models_lm(ex("cars"), "dist", "speed")
  e2 <- stats::residuals(cars_m$ajuste)^2
  aux <- summary(stats::lm(e2 ~ ex("cars")$speed))
  est <- nrow(ex("cars")) * aux$r.squared
  bp <- tr_models_breusch_pagan(cars_m)
  expect_equal(bp$estatistica, est)
  expect_equal(bp$p_valor, stats::pchisq(est, 1, lower.tail = FALSE))
})

test_that("pressuposto que não se aplica vira card vermelho explicando", {
  g <- tr_models_glm(ex("InsectSprays"), "count", "spray")
  err <- tryCatch(tr_models_shapiro_residuals(g), condition = identity)
  expect_s3_class(err, "tr_models_error_not_applicable")
  expect_match(conditionMessage(err), "GLM", fixed = TRUE)
  expect_error(tr_models_levene(tr_models_lm(ex("cars"), "dist", "speed")), class = "tr_models_error_not_applicable")
  dic <- tr_models_anova_dic(ex("PlantGrowth"), "weight", "group")
  expect_error(tr_models_tukey_additivity(dic), class = "tr_models_error_not_applicable")
})

test_that("testes clássicos batem com o stats", {
  tg <- ex("ToothGrowth")
  t <- tr_models_t_test(tg, "len", "supp")
  ref <- stats::t.test(len ~ supp, data = tg)
  expect_equal(t$p_valor, ref$p.value)
  expect_equal(t$efeito$valor, unname(diff(rev(ref$estimate))))
  expect_equal(tr_models_t_test(tg, "len", "supp", variancias_iguais = TRUE)$teste, "t de Student")
  expect_equal(tr_models_kruskal(ex("InsectSprays"), "count", "spray")$p_valor,
               stats::kruskal.test(count ~ spray, data = ex("InsectSprays"))$p.value)
  mt <- ex("mtcars")
  expect_equal(tr_models_cor_test(mt, "wt", "mpg")$efeito$valor, stats::cor(mt$wt, mt$mpg))
  expect_equal(tr_models_fisher_exact(mt, "am", "vs")$p_valor, stats::fisher.test(table(mt$am, mt$vs))$p.value)
  expect_equal(tr_models_chisq(ex("warpbreaks"), "wool", "tension")$estatistica, 0, tolerance = 1e-12)
  expect_equal(tr_models_one_sample_t(ex("PlantGrowth"), "weight", mu = 5)$p_valor,
               stats::t.test(ex("PlantGrowth")$weight, mu = 5)$p.value)
  expect_equal(tr_models_shapiro(ex("PlantGrowth"), "weight")$p_valor,
               stats::shapiro.test(ex("PlantGrowth")$weight)$p.value)
  s <- datasets::sleep
  d <- data.frame(a = s$extra[1:10], b = s$extra[11:20])
  expect_equal(tr_models_paired_t(d, "a", "b")$p_valor, stats::t.test(d$a - d$b)$p.value)
  expect_s3_class(tr_models_wilcoxon(tg, "len", "supp"), "tr_models_test")
})

test_that("conclusão segue a alternativa, e grupo que não é dois é erro", {
  tg <- ex("ToothGrowth")
  t <- tr_models_t_test(tg, "len", "supp", alternativa = "maior")
  expect_match(t$conclusao, "OJ maior", fixed = TRUE)
  expect_error(tr_models_t_test(ex("PlantGrowth"), "weight", "group"), class = "tr_models_error_two_groups")
  expect_error(tr_models_t_test(tg, "len", "supp", alternativa = "diferente"), class = "tr_models_error_bad_option")
})

test_that("qui-quadrado com esperado pequeno aponta o Fisher", {
  mt <- ex("mtcars")
  expect_match(tr_models_chisq(mt, "cyl", "gear")$nota, "models/fisher_exact", fixed = TRUE)
})

test_that("o guard dos tipos recusa objeto sem os campos", {
  tipo <- models_test_type()
  expect_error(tipo$store(list(teste = "x"), tempfile()), class = "tr_models_error_not_a_test")
  expect_error(models_fit_type()$store(stats::lm(mpg ~ wt, mtcars), tempfile()), class = "tr_models_error_not_a_fit")
  expect_error(models_effects_type()$store(.tr_models_efeitos(data.frame(a = 1), "x"), tempfile()),
               class = "tr_models_error_not_effects")
  expect_error(models_emm_type()$store(list(), tempfile()), class = "tr_models_error_not_emm")
})

test_that("previews dos tipos saem sem erro para todo modelo", {
  mods <- list(milho_dbc(),
               tr_models_lm(ex("mtcars"), formula = "mpg ~ wt"),
               tr_models_glm(ex("InsectSprays"), "count", "spray"),
               tr_models_lmer(ex("sleepstudy"), formula = "Reaction ~ Days + (1 | Subject)"),
               tr_models_anova_split_plot(ex("aveia"), "producao", "variedade", "nitrogenio", "bloco"))
  for (m in mods) {
    pv <- models_fit_type()$preview(m, ctx_tmp())
    expect_equal(pv$renderer, "models/fit", info = m$rotulo)
    expect_true(length(pv$data$linhas) > 0L, info = m$rotulo)
    expect_no_error(jsonlite::toJSON(pv$data, auto_unbox = TRUE, null = "null"))
    expect_s3_class(.tr_models_fit_tabela(m), "data.frame")
  }
  pv <- models_effects_type()$preview(tr_models_anova_table(milho_dbc()), ctx_tmp())
  q <- pv$data$quadro
  expect_equal(vapply(q$colunas, `[[`, "", "rotulo"), c("FV", "GL", "SQ", "QM", "Fc", "Pr > F"))
  expect_equal(q$linhas[[length(q$linhas)]]$termo, "Total")
  pv <- models_test_type()$preview(tr_models_shapiro_residuals(milho_dbc()), ctx_tmp())
  expect_equal(pv$data$estrelas, "ns")
  pv <- models_emm_type()$preview(tr_models_emmeans(milho_dbc(), "hibrido"), ctx_tmp())
  expect_true(file.exists(pv$files$png %||% unlist(pv$files)[[1]]))
})

test_that("qui-quadrado 2 × 2: sem Yates por padrão, com Yates como opção (oráculo)", {
  # Physicians' Health Study (aspirina × infarto), Agresti, An Introduction to
  # Categorical Data Analysis, cap. 2: placebo 189/10845, aspirina 104/10933.
  # Oráculo duplo: forma fechada do X² de Pearson, n(ad − bc)² / (r1 r2 c1 c2),
  # e da versão de Yates (1934), n(|ad − bc| − n/2)² / (r1 r2 c1 c2), e
  # stats::chisq.test. Tolerância 1e-10 (relativa).
  a <- 189; b <- 10845; c <- 104; d <- 10933; n <- a + b + c + d
  den <- (a + b) * (c + d) * (a + c) * (b + d)
  pearson <- n * (a * d - b * c)^2 / den
  yates <- n * (abs(a * d - b * c) - n / 2)^2 / den
  expect_equal(round(pearson, 2), 25.01)
  dados <- data.frame(
    grupo = rep(c("placebo", "placebo", "aspirina", "aspirina"), c(a, b, c, d)),
    infarto = rep(c("sim", "não", "sim", "não"), c(a, b, c, d)))
  tab <- table(dados$grupo, dados$infarto)
  sem <- tr_models_chisq(dados, "grupo", "infarto")
  expect_equal(unname(sem$estatistica), pearson, tolerance = 1e-10)
  expect_equal(sem$p_valor, stats::chisq.test(tab, correct = FALSE)$p.value, tolerance = 1e-10)
  com <- tr_models_chisq(dados, "grupo", "infarto", correcao = TRUE)
  expect_equal(unname(com$estatistica), yates, tolerance = 1e-10)
  expect_equal(com$p_valor, stats::chisq.test(tab, correct = TRUE)$p.value, tolerance = 1e-10)
  expect_false(formals(tr_models_chisq)$correcao)
})
