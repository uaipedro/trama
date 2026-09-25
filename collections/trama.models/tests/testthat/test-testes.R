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
  tipo <- trama::tr_get_type("data/test", models_registry())
  expect_error(tipo$store(list(teste = "x"), tempfile()), class = "tr_error_not_a_test")
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
    expect_s3_class(tr_models_as_table(m), "data.frame")
  }
  pv <- models_effects_type()$preview(tr_models_anova_table(milho_dbc()), ctx_tmp())
  q <- pv$data$quadro
  expect_equal(vapply(q$colunas, `[[`, "", "rotulo"), c("FV", "GL", "SQ", "QM", "Fc", "Pr > F"))
  expect_equal(q$linhas[[length(q$linhas)]]$termo, "Total")
  # O teste sai no tipo único: card do núcleo, store em round-trip e a linha
  # de tabela pelo adaptador da `data`.
  reg <- models_registry()
  sh <- tr_models_shapiro_residuals(milho_dbc())
  ty <- trama::tr_get_type("data/test", reg)
  pv <- ty$preview(sh, ctx_tmp())
  expect_equal(pv$renderer, "trama/test")
  expect_equal(trama::tr_test_table(sh)$significancia, "ns")
  arq <- tempfile(fileext = ".rds"); ty$store(sh, arq)
  expect_identical(ty$restore(arq), sh)
  tb <- trama::tr_adapter_for("data/test", "data/table", reg)$fn(sh)
  expect_s3_class(tb, "tbl_df"); expect_equal(nrow(tb), 1L)
  pv <- models_emm_type()$preview(tr_models_emmeans(milho_dbc(), "hibrido"), ctx_tmp())
  expect_true(file.exists(pv$files$png %||% unlist(pv$files)[[1]]))
})

# Referência: `dunn.test::dunn.test(..., altp = TRUE)` 1.3.6 (a mesma conta do
# `FSA::dunnTest`) no InsectSprays com os sprays A a D, fixa aqui. Os empates
# são muitos (contagens), o que exercita a correção.
test_that("Dunn: z e p batem com o dunn.test, com empates, nas quatro correções", {
  d <- subset(ex("InsectSprays"), spray %in% c("A", "B", "C", "D"))
  r <- tr_models_dunn(d, "count", "spray")
  expect_s3_class(r, "tr_models_effects")
  t <- r$tabela
  expect_equal(t$termo, c("A - B", "A - C", "A - D", "B - C", "B - D", "C - D"))
  expect_equal(t$z, c(-0.2995389540, 4.7487882959, 3.1488119316, 5.0483272499, 3.4483508856, -1.5999763643),
               tolerance = 1e-9)
  expect_equal(t$p_sem_ajuste, c(0.7645288545, 2.046390175e-06, 0.001639356604, 4.456952256e-07,
                                 0.0005640208015, 0.1096038269), tolerance = 1e-8)
  expect_equal(t$p_valor, c(0.7645288545, 1.023195088e-05, 0.004918069812, 2.674171354e-06,
                            0.002256083206, 0.2192076538), tolerance = 1e-8)
  b <- tr_models_dunn(d, "count", "spray", ajuste = "bonferroni")$tabela
  expect_equal(b$p_valor, c(1, 1.227834105e-05, 0.009836139624, 2.674171354e-06, 0.003384124809, 0.6576229613),
               tolerance = 1e-8)
  expect_equal(tr_models_dunn(d, "count", "spray", ajuste = "nenhum")$tabela$p_valor, t$p_sem_ajuste)
  expect_equal(tr_models_dunn(d, "count", "spray", ajuste = "sidak")$tabela$p_valor, 1 - (1 - t$p_sem_ajuste)^6)
  expect_error(tr_models_dunn(d, "count", "spray", ajuste = "tukey"))
})

test_that("Dunn: sem empates é a fórmula de Dunn crua, e a diferença é a de postos médios", {
  d <- data.frame(y = c(1, 2, 3, 10, 11, 12, 20, 21, 22), g = rep(c("x", "y", "w"), each = 3))
  r <- tr_models_dunn(d, "y", "g", ajuste = "nenhum")$tabela
  # Postos 1..9; médias 2, 5, 8; EP = sqrt(9 * 10 / 12 * (1/3 + 1/3)) = sqrt(5).
  expect_equal(r$termo, c("w - x", "w - y", "x - y"))
  expect_equal(r$estimativa, c(6, 3, -3))
  expect_equal(r$z, c(6, 3, -3) / sqrt(5))
  expect_equal(r$p_valor, 2 * pnorm(-abs(c(6, 3, -3) / sqrt(5))))
  expect_error(tr_models_dunn(transform(d, g = "x"), "y", "g"), class = "tr_models_error_one_level")
})
