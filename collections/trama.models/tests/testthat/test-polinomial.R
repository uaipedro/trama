# Regressão polinomial nos tratamentos quantitativos da ANOVA (polinômios
# ortogonais: SQ por grau, falta de ajuste, equação, R²).

algodao <- function() {
  # Montgomery, Design and Analysis of Experiments, tabela 3.1: resistência à
  # tração pela % de algodão, 5 níveis igualmente espaçados, 5 repetições.
  data.frame(algodao = rep(seq(15, 35, 5), each = 5),
             resistencia = c(7, 7, 15, 11, 9, 12, 17, 12, 18, 18, 14, 18, 18, 19, 19,
                             19, 25, 22, 19, 23, 7, 10, 11, 15, 11))
}

linha <- function(t, termo) t$tabela[t$tabela$termo == termo, ]

test_that("polinomial reproduz Montgomery (SQ 33,62; 343,21; 64,98; 33,95) e o lm com poly()", {
  d <- algodao()
  m <- tr_models_anova_dic(d, "resistencia", "algodao")
  t <- tr_models_polinomial(m, "algodao", grau = 3L)
  expect_s3_class(t, "tr_models_effects")
  expect_equal(round(linha(t, "linear")$sq, 2), 33.62)
  expect_equal(round(linha(t, "quadrático")$sq, 2), 343.21)
  expect_equal(round(linha(t, "cúbico")$sq, 2), 64.98)
  expect_equal(round(linha(t, "falta de ajuste")$sq, 2), 33.95)  # o quártico, com grau 3
  expect_equal(linha(t, "falta de ajuste")$gl, 1)
  # Oráculo: sequencial de lm(y ~ poly(x, 4)) coluna a coluna.
  p <- stats::poly(d$algodao, 4)
  a <- anova(stats::lm(d$resistencia ~ p[, 1] + p[, 2] + p[, 3] + p[, 4]))
  expect_equal(c(linha(t, "linear")$sq, linha(t, "quadrático")$sq, linha(t, "cúbico")$sq,
                 linha(t, "falta de ajuste")$sq), a$`Sum Sq`[1:4], tolerance = 1e-10)
  expect_equal(linha(t, "cúbico")$F, a$`F value`[[3]], tolerance = 1e-10)
  expect_equal(linha(t, "cúbico")$p_valor, a$`Pr(>F)`[[3]], tolerance = 1e-10)
  expect_equal(linha(t, "Resíduo")$qm, a$`Mean Sq`[[5]], tolerance = 1e-10)
  # Equação do maior grau significativo (cúbico) e R² = SQ da regressão / SQ de tratamentos.
  cf <- stats::coef(stats::lm(resistencia ~ algodao + I(algodao^2) + I(algodao^3), data = d))
  expect_equal(t$coeficientes, unname(cf), tolerance = 1e-8)
  expect_equal(t$grau_equacao, 3L)
  expect_equal(t$r2, sum(a$`Sum Sq`[1:3]) / sum(a$`Sum Sq`[1:4]), tolerance = 1e-10)
})

test_that("polinomial no DBC e com espaçamento desigual e repetições desiguais bate com o lm", {
  set.seed(7)
  dose <- c(0, 50, 100, 200, 400)
  d <- expand.grid(dose = dose, bloco = paste0("B", 1:4))
  d$y <- 10 + 0.03 * d$dose - 5e-5 * d$dose^2 + as.integer(factor(d$bloco)) + stats::rnorm(nrow(d))
  m <- tr_models_anova_dbc(d, "y", "dose", "bloco")
  t <- tr_models_polinomial(m, "dose", grau = 2L)
  p <- stats::poly(d$dose, 4)
  a <- anova(stats::lm(y ~ factor(bloco) + p[, 1] + p[, 2] + p[, 3] + p[, 4], data = d))
  expect_equal(linha(t, "linear")$sq, a$`Sum Sq`[[2]], tolerance = 1e-10)
  expect_equal(linha(t, "quadrático")$sq, a$`Sum Sq`[[3]], tolerance = 1e-10)
  expect_equal(linha(t, "falta de ajuste")$sq, sum(a$`Sum Sq`[4:5]), tolerance = 1e-10)
  expect_equal(linha(t, "falta de ajuste")$gl, 2)
  expect_equal(linha(t, "Resíduo")$gl, a$Df[[6]])
  # Soma dos graus + falta de ajuste = SQ de tratamentos do quadro.
  q <- tr_models_anova_table(m)$tabela
  expect_equal(sum(t$tabela$sq[t$tabela$termo != "Resíduo"]), q$sq[q$termo == "dose"], tolerance = 1e-10)
  # DIC com repetições desiguais: sequencial do lm em x, x², ...
  d2 <- d[-c(1, 7, 8), ]
  m2 <- tr_models_anova_dic(d2, "y", "dose")
  t2 <- tr_models_polinomial(m2, "dose", grau = 2L)
  a2 <- anova(stats::lm(y ~ dose + I(dose^2) + factor(dose), data = d2))
  expect_equal(c(linha(t2, "linear")$sq, linha(t2, "quadrático")$sq, linha(t2, "falta de ajuste")$sq),
               a2$`Sum Sq`[1:3], tolerance = 1e-10)
})

test_that("polinomial recusa tratamento não numérico, grau alto e delineamento fora do DIC/DBC", {
  pg <- tr_models_anova_dic(ex("PlantGrowth"), "weight", "group")
  expect_error(tr_models_polinomial(pg, "group"), class = "tr_models_error_not_applicable")
  m <- tr_models_anova_dic(algodao(), "resistencia", "algodao")
  expect_error(tr_models_polinomial(m, "algodao", grau = 5L), class = "tr_models_error_bad_option")
  expect_error(tr_models_polinomial(tr_models_glm(ex("InsectSprays"), "count", "spray"), "spray"),
               class = "tr_models_error_not_applicable")
})
