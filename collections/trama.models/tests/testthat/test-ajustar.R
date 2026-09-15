test_that("lm pelas colunas e pela fórmula dá o mesmo que stats::lm, e a fórmula vence", {
  mt <- ex("mtcars")
  a <- tr_models_lm(mt, "mpg", "wt, hp")
  b <- tr_models_lm(mt, "cyl", "qsec", formula = "mpg ~ wt + hp")
  ref <- stats::lm(mpg ~ wt + hp, data = mt)
  expect_equal(unname(stats::coef(a$ajuste)), unname(stats::coef(ref)))
  expect_equal(unname(stats::coef(b$ajuste)), unname(stats::coef(ref)))
  expect_equal(b$formula, "mpg ~ wt + hp")
  expect_s3_class(a, "tr_models_fit")
})

test_that("fórmula ruim, sem resposta ou com coluna inexistente é erro que diz o que fazer", {
  mt <- ex("mtcars")
  expect_error(tr_models_lm(mt, formula = "mpg ~ wt +"), class = "tr_models_error_bad_formula")
  expect_error(tr_models_lm(mt, formula = "~ wt"), class = "tr_models_error_bad_formula")
  err <- tryCatch(tr_models_lm(mt, formula = "mpg ~ peso"), condition = identity)
  expect_s3_class(err, "tr_models_error_bad_formula")
  expect_match(conditionMessage(err), "peso", fixed = TRUE)
  expect_error(tr_models_lm(mt, "mpg", "Wt"), class = "tr_models_error_unknown_column")
  expect_error(tr_models_lm(mt), class = "tr_models_error_blank_param")
  expect_error(tr_models_lm(mt, "modelo", "wt"), class = "tr_models_error_not_numeric")
})

test_that("faltantes saem à vista, e o ambiente da fórmula não arrasta a função", {
  mt <- ex("mtcars"); mt$hp[c(2, 5)] <- NA
  m <- tr_models_lm(mt, formula = "mpg ~ wt + hp")
  expect_equal(m$descartadas, 2L)
  expect_equal(nrow(m$dados), 30L)
  expect_identical(environment(stats::formula(m$ajuste)), globalenv())
})

test_that("nomes com espaço e acento entram na fórmula montada", {
  d <- tibble::tibble(`produção total` = c(5, 6, 7, 8, 6, 9), `adubação` = c("a", "b", "a", "b", "a", "b"))
  m <- tr_models_lm(d, "produção total", "adubação")
  expect_equal(length(stats::coef(m$ajuste)), 2L)
})

test_that("glm confere com stats::glm e recusa contagem negativa", {
  ins <- ex("InsectSprays")
  g <- tr_models_glm(ins, "count", "spray", familia = "poisson")
  ref <- stats::glm(count ~ spray, stats::poisson(), data = ins)
  expect_equal(unname(stats::coef(g$ajuste)), unname(stats::coef(ref)))
  ins$count[[1]] <- -1
  expect_error(tr_models_glm(ins, "count", "spray"), class = "tr_models_error_bad_option")
  expect_error(tr_models_glm(ex("mtcars"), "am", "wt", familia = "logit"), class = "tr_models_error_bad_option")
})

test_that("lmer pela fórmula e pelo atalho, e recusa fórmula sem termo aleatório", {
  s <- ex("sleepstudy")
  a <- tr_models_lmer(s, formula = "Reaction ~ Days + (1 | Subject)")
  b <- tr_models_lmer(s, resposta = "Reaction", fixos = "Days", grupo = "Subject")
  expect_equal(lme4::fixef(a$ajuste), lme4::fixef(b$ajuste))
  expect_s4_class(a$ajuste, "lmerModLmerTest")
  expect_error(tr_models_lmer(s, formula = "Reaction ~ Days"), class = "tr_models_error_bad_formula")
  # O modelo volta do disco sabendo se reajustar.
  f <- tempfile(); saveRDS(a, f); a2 <- readRDS(f)
  expect_no_error(tr_models_random_test(a2))
})

test_that("delineamentos reproduzem o quadro do aov e transformam o desenho em fator", {
  d <- ex("ToothGrowth")
  fa <- tr_models_anova_factorial(d, "len", "supp, dose")
  expect_true(is.factor(fa$dados$dose))
  ref <- summary(stats::aov(len ~ supp * factor(dose), data = d))[[1]]
  q <- tr_models_anova_table(fa)$tabela
  expect_equal(q$F[1:3], unname(ref$`F value`[1:3]), tolerance = 1e-10)
  expect_equal(fa$delineamento, "fatorial_dic")
  expect_error(tr_models_anova_factorial(d, "len", "supp"), class = "tr_models_error_bad_option")
})

test_that("DBC, DQL e DIC montam o modelo certo", {
  m <- milho_dbc()
  expect_equal(m$formula, "producao ~ bloco + hibrido")
  expect_equal(m$bloco, "bloco")
  r <- ex("racao_dql")
  q <- tr_models_anova_dql(r, "ganho_peso", "racao", "periodo", "lote")
  expect_equal(nrow(tr_models_anova_table(q)$tabela), 5L)
  r$lote[r$lote == "L5"] <- "L4"
  expect_error(tr_models_anova_dql(r, "ganho_peso", "racao", "periodo", "lote"),
               class = "tr_models_error_bad_option")
})

test_that("fator com um nível e resíduo sem grau de liberdade viram erro", {
  pg <- ex("PlantGrowth")
  expect_error(tr_models_anova_dic(pg[pg$group == "ctrl", ], "weight", "group"),
               class = "tr_models_error_one_level")
  milho <- ex("milho_dbc")
  expect_error(tr_models_lm(milho, formula = "producao ~ bloco * hibrido"),
               class = "tr_models_error_no_residual_df")
})

test_that("parcela subdividida: erros (a) e (b) como no livro, e o misto dá os mesmos F", {
  av <- ex("aveia")
  sp <- tr_models_anova_split_plot(av, "producao", "variedade", "nitrogenio", "bloco")
  q <- tr_models_anova_table(sp)$tabela
  expect_equal(q$termo, c("bloco", "variedade", "Resíduo (a)", "nitrogenio",
                          "variedade:nitrogenio", "Resíduo (b)", "Total"))
  expect_equal(q$gl, c(5, 2, 10, 3, 6, 45, 71))
  expect_equal(sum(q$sq[q$termo != "Total"]), q$sq[q$termo == "Total"])
  a3 <- as.data.frame(stats::anova(sp$aux_misto, type = 3))
  expect_equal(q$F[q$termo == "variedade"], a3["variedade", "F value"], tolerance = 1e-6)
  expect_equal(q$F[q$termo == "nitrogenio"], a3["nitrogenio", "F value"], tolerance = 1e-6)
  expect_error(tr_models_anova_table(sp, "III"), class = "tr_models_error_not_applicable")
  expect_error(tr_models_coefficients(sp), class = "tr_models_error_not_applicable")
})
