# Atributo `trama_ferramentas`: o resultado registra as ferramentas do ramo que
# de fato correu, e o relatório cita essas (não a declaração do bloco). Cada
# caso confere o ramo, não só a presença do atributo.

ferr <- function(x) attr(x, "trama_ferramentas", exact = TRUE)

test_that("anova_table: SQ tipo I em lm é stats::anova; II e III são car::Anova", {
  fit <- tr_models_lm(tr_models_example("PlantGrowth"), formula = "weight ~ group")
  expect_equal(ferr(tr_models_anova_table(fit, tipo_sq = "I")), "stats::anova")
  expect_equal(ferr(tr_models_anova_table(fit, tipo_sq = "II")), "car::Anova")
  expect_equal(ferr(tr_models_anova_table(fit, tipo_sq = "III")), "car::Anova")
})

test_that("anova_table: gls é nlme::anova.gls; misto (lmer) é stats::anova", {
  s <- tr_models_example("sleepstudy")
  misto <- tr_models_lmer(s, formula = "Reaction ~ Days + (1 | Subject)")
  expect_equal(ferr(tr_models_anova_table(misto)), "stats::anova")
  g <- tr_models_gls(as.data.frame(nlme::Ovary) |>
                       transform(Mare = factor(as.character(Mare)),
                                 s = sin(2 * pi * Time), c = cos(2 * pi * Time)),
                     formula = "follicles ~ s + c", correlacao = "ar1", grupo = "Mare")
  expect_equal(ferr(tr_models_anova_table(g, tipo_sq = "I")), "nlme::anova.gls")
})

test_that("anova_table: GLM Poisson é stats; binomial negativa com SQ III passa pelo MASS", {
  d <- as.data.frame(MASS::quine)
  pois <- tr_models_glm(d, formula = "Days ~ Eth + Sex + Age", familia = "poisson")
  expect_equal(ferr(tr_models_anova_table(pois, tipo_sq = "I")), "stats::anova")
  expect_equal(ferr(tr_models_anova_table(pois, tipo_sq = "II")), "car::Anova")
  nb <- tr_models_glm(d, formula = "Days ~ Eth + Sex + Age", familia = "binomial negativa")
  expect_equal(ferr(tr_models_anova_table(nb, tipo_sq = "III")), c("MASS::glm.nb", "car::Anova"))
})

test_that("glm: a fonte do ajuste registra stats::glm, ou MASS::glm.nb na binomial negativa", {
  d <- as.data.frame(MASS::quine)
  expect_equal(ferr(tr_models_glm(d, formula = "Days ~ Eth + Sex", familia = "poisson")), "stats::glm")
  expect_equal(ferr(tr_models_glm(d, formula = "Days ~ Eth + Sex", familia = "binomial negativa")),
               "MASS::glm.nb")
})

test_that("linear_hypothesis: nas médias é emmeans; nos coeficientes, car (ou lmerTest no misto)", {
  dic <- tr_models_anova_dic(tr_models_example("PlantGrowth"), "weight", "group")
  medias <- tr_models_linear_hypothesis(dic, "0 1 -1", "group")
  expect_equal(ferr(medias), "emmeans::contrast")
  expect_false(any(grepl("^car::|^lmerTest::", ferr(medias))))

  coef <- tr_models_linear_hypothesis(tr_models_lm(datasets::mtcars, formula = "mpg ~ wt + hp"), "wt = 0")
  expect_equal(ferr(coef), "car::linearHypothesis")

  misto <- tr_models_lmer(tr_models_example("sleepstudy"), formula = "Reaction ~ Days + (1 | Subject)")
  h <- tr_models_linear_hypothesis(misto, "Days = 0")
  expect_equal(ferr(h), "lmerTest::contest")
})

test_that("permutation: só stats::anova, sem coin", {
  fit <- tr_models_lm(data.frame(y = c(2, 3, 4, 5, 8, 9, 10, 11), g = factor(rep(c("a", "b"), each = 4))),
                      formula = "y ~ g")
  got <- tr_models_permutation(fit, termo = "g", reamostras = 99, .seed = 1L)
  expect_equal(ferr(got), "stats::anova")
})

test_that("select: AIC/logLik em base; misto entra com lme4::refitML", {
  mt <- datasets::mtcars
  ajs <- list(tr_models_lm(mt, formula = "mpg ~ wt"), tr_models_lm(mt, formula = "mpg ~ wt + hp"))
  expect_equal(ferr(tr_models_select(ajs)), "stats::logLik")
})

test_that("random_effects não se cita (só lê o ajuste); exemplos de pacote fora da base registram o pacote", {
  misto <- tr_models_lmer(tr_models_example("sleepstudy"), formula = "Reaction ~ Days + (1 | Subject)")
  expect_null(ferr(tr_models_random_effects(misto)))
  expect_equal(ferr(tr_models_example("aveia")), "MASS::oats")
  expect_null(ferr(tr_models_example("PlantGrowth")))
})

test_that("coefficients: VIF do car só em lm com 2+ termos sem interação; intervals no gls", {
  lm2 <- tr_models_lm(datasets::mtcars, formula = "mpg ~ wt + hp")
  expect_equal(ferr(tr_models_coefficients(lm2)), "car::vif")
  lm1 <- tr_models_lm(datasets::mtcars, formula = "mpg ~ wt")
  expect_length(ferr(tr_models_coefficients(lm1)), 0L)
  misto <- tr_models_lmer(tr_models_example("sleepstudy"), formula = "Reaction ~ Days + (1 | Subject)")
  expect_length(ferr(tr_models_coefficients(misto)), 0L)
})

test_that("bootstrap: boot sempre; emmeans só nas médias", {
  fit <- tr_models_lm(datasets::mtcars, formula = "mpg ~ wt")
  coef <- tr_models_bootstrap(fit, reamostras = 199L, .seed = 1L)
  expect_equal(ferr(coef), c("boot::boot", "boot::boot.ci"))
  dic <- tr_models_anova_dic(tr_models_example("PlantGrowth"), "weight", "group")
  medias <- tr_models_bootstrap(dic, quantidade = "médias", reamostras = 199L, .seed = 1L)
  expect_equal(ferr(medias), c("boot::boot", "boot::boot.ci", "emmeans::emmeans"))
})
