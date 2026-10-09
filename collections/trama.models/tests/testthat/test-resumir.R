test_that("SQ tipo II e III batem com o car, e o III reajusta em soma zero", {
  wb <- ex("warpbreaks")
  wb <- wb[-c(1, 2, 30), ]  # desbalanceado, para os tipos diferirem
  m <- tr_models_anova_factorial(wb, "breaks", "wool, tension")
  q2 <- tr_models_anova_table(m, "II")$tabela
  ref2 <- car::Anova(stats::lm(breaks ~ wool * tension, data = wb), type = 2)
  expect_equal(q2$F[1:3], unname(ref2$`F value`[1:3]), tolerance = 1e-8)
  q3 <- tr_models_anova_table(m, "III")$tabela
  ref3 <- car::Anova(stats::lm(breaks ~ wool * tension, data = wb,
                               contrasts = list(wool = "contr.sum", tension = "contr.sum")), type = 3)
  expect_equal(q3$F[1:3], unname(ref3$`F value`[2:4]), tolerance = 1e-8)
  expect_false(isTRUE(all.equal(q2$F[[1]], q3$F[[1]])))
})

test_that("o quadro tipo I fecha: as SQ somam o Total, com n − 1 gl; II e III não têm Total", {
  m <- milho_dbc()
  q <- tr_models_anova_table(m)$tabela
  expect_equal(q$termo, c("bloco", "hibrido", "Resíduo", "Total"))
  expect_equal(sum(q$sq[1:3]), q$sq[[4]])
  expect_equal(q$gl[[4]], nrow(m$dados) - 1)
  expect_false("Total" %in% tr_models_anova_table(m, "II")$tabela$termo)
})

test_that("o quadro traz CV e média no rodapé, e as estrelas seguem o summary", {
  q <- tr_models_anova_table(milho_dbc())
  expect_match(q$rodape$CV, "%$")
  expect_equal(.tr_models_estrelas(c(0.0001, 0.005, 0.03, 0.07, 0.5, NA)), c("***", "**", "*", ".", "ns", ""))
  expect_s3_class(q, "tr_models_effects")
})

test_that("coeficientes batem com o summary e exponenciam no GLM", {
  mt <- ex("mtcars")
  l <- tr_models_lm(mt, formula = "mpg ~ wt")
  c1 <- tr_models_coefficients(l)$tabela
  s <- stats::coef(summary(stats::lm(mpg ~ wt, data = mt)))
  expect_equal(c1$p_valor, unname(s[, 4]))
  g <- tr_models_glm(mt, formula = "am ~ wt", familia = "binomial")
  ce <- tr_models_coefficients(g, exponenciar = TRUE)$tabela
  expect_equal(ce$estimativa, unname(exp(stats::coef(g$ajuste))))
  expect_error(tr_models_coefficients(l, exponenciar = TRUE), class = "tr_models_error_not_applicable")
})

test_that("VIF/GVIF e influência batem com os oráculos car e stats", {
  d <- as.data.frame(USArrests)
  m <- tr_models_lm(d, formula = "Murder ~ Assault + UrbanPop + Rape")
  got <- tr_models_coefficients(m)$tabela
  ref <- car::vif(m$ajuste)
  expect_equal(got$vif[match(names(ref), got$termo)], unname(ref), tolerance = 1e-10)
  expect_true(all(is.na(got$vif[got$termo == "(Intercept)"])))
  d$regiao <- factor(rep(c("A", "B", "C"), length.out = nrow(d)))
  mf <- tr_models_lm(d, formula = "Murder ~ Assault + UrbanPop + regiao")
  gf <- tr_models_coefficients(mf)$tabela
  rf <- car::vif(mf$ajuste)
  linhas <- which(grepl("^regiao", gf$termo))
  expect_equal(gf$vif[linhas], rep(rf["regiao", "GVIF"], length(linhas)), tolerance = 1e-10)
  expect_equal(gf$gvif_ajustado[linhas], rep(rf["regiao", "GVIF^(1/(2*Df))"], length(linhas)), tolerance = 1e-10)

  infl <- tr_models_influence(m)
  refi <- stats::influence.measures(m$ajuste)
  expect_equal(infl$tabela$alavanca, as.numeric(stats::hatvalues(m$ajuste)), tolerance = 1e-10)
  expect_equal(infl$tabela$residuo_estudentizado, as.numeric(stats::rstudent(m$ajuste)), tolerance = 1e-10)
  expect_equal(infl$tabela$cook, as.numeric(stats::cooks.distance(m$ajuste)), tolerance = 1e-10)
  expect_equal(infl$tabela$dffits, as.numeric(stats::dffits(m$ajuste)), tolerance = 1e-10)
  expect_equal(infl$tabela$influente, as.logical(apply(refi$is.inf, 1L, any)))
  dfb <- refi$infmat[, grepl("^dfb\\.", colnames(refi$infmat)), drop = FALSE]
  nossas <- grep("^dfbetas_", names(infl$tabela), value = TRUE)
  expect_equal(nossas[[1]], "dfbetas_intercepto")
  expect_equal(unname(as.matrix(infl$tabela[nossas])), unname(dfb), tolerance = 1e-10)
})

test_that("medidas de ajuste têm sempre as mesmas colunas", {
  mt <- ex("mtcars")
  a <- tr_models_fit_stats(tr_models_lm(mt, formula = "mpg ~ wt"))
  b <- tr_models_fit_stats(tr_models_lmer(ex("sleepstudy"), formula = "Reaction ~ Days + (1 | Subject)"))
  c <- tr_models_fit_stats(tr_models_glm(mt, formula = "am ~ wt", familia = "binomial"))
  expect_equal(names(a), names(b)); expect_equal(names(a), names(c))
  expect_equal(a$r2, summary(stats::lm(mpg ~ wt, data = mt))$r.squared)
  expect_true(b$r2_condicional > b$r2_marginal)
  expect_true(c$desvio_explicado > 0 && c$desvio_explicado < 1)
})

test_that("R² marginal e condicional do misto só com intercepto seguem a fórmula", {
  m <- tr_models_lmer(ex("sleepstudy"), formula = "Reaction ~ Days + (1 | Subject)")
  aj <- m$ajuste
  vf <- stats::var(stats::fitted(stats::lm(Reaction ~ Days, data = lme4::sleepstudy)))
  vf <- stats::var(as.vector(lme4::getME(aj, "X") %*% lme4::fixef(aj)))
  va <- as.data.frame(lme4::VarCorr(aj))$vcov[[1]]
  ve <- stats::sigma(aj)^2
  r <- .tr_models_r2_misto(aj)
  expect_equal(r[["marginal"]], vf / (vf + va + ve))
  expect_equal(r[["condicional"]], (vf + va) / (vf + va + ve))
})

test_that("efeitos aleatórios e o teste deles", {
  m <- tr_models_lmer(ex("sleepstudy"), formula = "Reaction ~ Days + (Days | Subject)")
  v <- tr_models_random_effects(m)
  expect_equal(v$componente, c("(Intercept)", "Days", "corr((Intercept), Days)", "resíduo"))
  expect_equal(sum(v$proporcao, na.rm = TRUE), 1)
  r <- tr_models_random_test(m)$tabela
  expect_equal(r$gl[[2]], 2)
  expect_error(tr_models_random_effects(milho_dbc()), class = "tr_models_error_not_applicable")
})

test_that("resíduos ao lado da tabela, sem sobrescrever coluna existente", {
  m <- milho_dbc()
  r <- tr_models_residuals(m)
  expect_equal(r$residuo, unname(stats::residuals(m$ajuste)))
  d <- ex("milho_dbc"); d$residuo <- 1
  r2 <- tr_models_residuals(tr_models_anova_dbc(d, "producao", "hibrido", "bloco"))
  expect_true(all(c("residuo", "residuo_modelo") %in% names(r2)))
  expect_s3_class(tr_models_plot_diagnostics(m), "ggplot")
})

test_that("comparar modelos aninhados, e recusar os não aninhados", {
  mt <- ex("mtcars")
  m1 <- tr_models_lm(mt, formula = "mpg ~ wt")
  m2 <- tr_models_lm(mt, formula = "mpg ~ wt + hp")
  t <- tr_models_compare(m2, m1)
  expect_equal(t$p_valor, stats::anova(stats::lm(mpg ~ wt, mt), stats::lm(mpg ~ wt + hp, mt))$`Pr(>F)`[[2]])
  expect_match(t$h0, "hp", fixed = TRUE)
  m3 <- tr_models_lm(mt, formula = "mpg ~ qsec")
  expect_error(tr_models_compare(m1, m3), class = "tr_models_error_not_nested")
  mt2 <- mt; mt2$hp[[1]] <- NA
  expect_error(tr_models_compare(m1, tr_models_lm(mt2, formula = "mpg ~ wt + hp")),
               class = "tr_models_error_not_nested")
  s <- ex("sleepstudy")
  a <- tr_models_lmer(s, formula = "Reaction ~ Days + (1 | Subject)")
  b <- tr_models_lmer(s, formula = "Reaction ~ Days + (Days | Subject)")
  expect_equal(tr_models_compare(a, b)$teste, "Razão de verossimilhança")
})

test_that("tipo III com covariável em interação avisa que o fator é testado na covariável zero", {
  d <- data.frame(y = c(5.1, 6.3, 7.2, 8.4, 4.8, 6.9, 8.8, 10.1, 5.5, 6.0, 7.9, 9.7),
                  a = factor(rep(c("p", "q"), each = 6)), x = rep(c(1, 2, 3), 4) + 10)
  m <- tr_models_lm(d, formula = "y ~ a * x")
  q <- tr_models_anova_table(m, "III")
  expect_match(q$nota, "x = 0")
  # Oráculo: o F de 'a' é o do contraste entre as retas em x = 0 (contr.sum).
  ref <- stats::lm(y ~ a * x, data = d, contrasts = list(a = "contr.sum"))
  expect_equal(q$tabela$F[q$tabela$termo == "a"], car::Anova(ref, type = 3)["a", "F value"], tolerance = 1e-10)
  expect_equal(tr_models_anova_table(tr_models_lm(d, formula = "y ~ a + x"), "III")$nota, "")
})

test_that("VIF fica NA com interação", {
  fit <- tr_models_lm(datasets::warpbreaks, formula = "breaks ~ wool * tension")
  tab <- tr_models_coefficients(fit)$tabela
  expect_true("vif" %in% names(tab))
  expect_true(all(is.na(tab$vif)))
})

test_that("quadro tipo I avisa quando o desenho é desbalanceado, sem mudar o padrão", {
  tg <- ex("ToothGrowth"); tg$dose <- factor(tg$dose)
  # Balanceado (10 por casela): tipo I = tipo II, nada a avisar.
  bal <- tr_models_anova_table(tr_models_lm(tg, formula = "len ~ supp * dose"))
  expect_false(grepl("desbalanceados", bal$nota, fixed = TRUE))
  # Desbalanceado: a SQ sequencial de supp muda com a ordem (Langsrud 2003).
  des <- tg[-(1:5), ]
  q <- tr_models_anova_table(tr_models_lm(des, formula = "len ~ supp * dose"))
  expect_match(q$nota, "desbalanceados", fixed = TRUE)
  expect_match(q$nota, "supp", fixed = TRUE)
  expect_false(grepl("supp:dose", sub(".*SQ de ([^ ]+(, [^ ]+)*) depende.*", "\\1", q$nota), fixed = TRUE))
  s_primeiro <- stats::anova(stats::lm(len ~ supp + dose, des))["supp", "Sum Sq"]
  s_segundo <- stats::anova(stats::lm(len ~ dose + supp, des))["supp", "Sum Sq"]
  expect_gt(abs(s_primeiro - s_segundo), 1)
  # O padrão continua o tipo I, e os tipos II e III não levam o aviso.
  expect_identical(formals(tr_models_anova_table)$tipo_sq, "I")
  expect_false(grepl("desbalanceados", tr_models_anova_table(tr_models_lm(des, formula = "len ~ supp * dose"), "II")$nota,
                     fixed = TRUE))
})

# Oráculo: `statmod::qresid` 1.5.x (Dunn & Smyth 1996, mesmos autores), com a
# mesma semente e o mesmo RNG: a conta e a ordem do sorteio são as mesmas, e a
# tolerância é de arredondamento (1e-10). Gama e gaussiana não sorteiam.
test_that("resíduo quantílico randomizado bate com o statmod::qresid em cada família", {
  skip_if_not_installed("statmod")
  skip_if_not_installed("MASS")
  ins <- ex("InsectSprays")
  mt <- ex("mtcars")
  agr <- data.frame(s = c(3, 7, 9, 12, 15), f = c(17, 13, 11, 8, 5), dose = 1:5)
  casos <- list(
    poisson = tr_models_glm(ins, "count", "spray", familia = "poisson"),
    binomial_01 = tr_models_glm(mt, "am", "wt", familia = "binomial"),
    binomial_agrupada = tr_models_glm(agr, formula = "cbind(s, f) ~ dose", familia = "binomial"),
    negbin = tr_models_glm(ins, "count", "spray", familia = "binomial negativa"),
    gama = tr_models_glm(mt, "mpg", "wt", familia = "gama"),
    gaussiana = tr_models_glm(mt, "mpg", "wt", familia = "gaussiana"))
  for (k in names(casos)) {
    r <- tr_models_residuals(casos[[k]], .seed = 42L)$residuo_quantilico
    RNGkind("Mersenne-Twister", "Inversion", "Rejection"); set.seed(42L)
    esperado <- unname(statmod::qresid(casos[[k]]$ajuste))
    expect_equal(r, esperado, tolerance = 1e-10, label = k)
  }
})

test_that("resíduo quantílico: semente reprodutível, quasi sem distribuição, só no GLM", {
  g <- tr_models_glm(ex("InsectSprays"), "count", "spray", familia = "poisson")
  expect_identical(tr_models_residuals(g, .seed = 3L)$residuo_quantilico,
                   tr_models_residuals(g, .seed = 3L)$residuo_quantilico)
  expect_false(identical(tr_models_residuals(g, .seed = 3L)$residuo_quantilico,
                         tr_models_residuals(g, .seed = 4L)$residuo_quantilico))
  q <- tr_models_glm(ex("InsectSprays"), "count", "spray", familia = "quasipoisson")
  expect_true(all(is.na(tr_models_residuals(q)$residuo_quantilico)))
  m <- tr_models_lm(ex("mtcars"), formula = "mpg ~ wt")
  expect_false("residuo_quantilico" %in% names(tr_models_residuals(m)))
})
