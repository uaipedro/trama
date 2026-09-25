# Oráculos: Montgomery (2017) ex. 3.1 e 6.1 (valores do livro, tolerância 0,01
# por virem com duas casas); `emmeans::contrast`, `car::linearHypothesis`,
# `stats::contr.poly` e o quadro do `aov` com `Error()` (tolerância 1e-8
# relativa, a do ponto flutuante).

test_that("Montgomery 3.1: contrastes ortogonais digitados reproduzem os SQ do livro", {
  m <- trama.models::tr_models_anova_dic(dados_gravacao(), "taxa", "potencia")
  r <- tr_experiments_contrasts(m, "potencia", "digitados",
                                contrastes = "C1: 1 -1 0 0; C2: 1 1 -1 -1; C3: 0 0 1 -1")
  t <- r$out$tabela
  expect_equal(t$sq, c(3276.10, 46948.05, 16646.40), tolerance = 0.01 / 46948)
  expect_equal(sum(t$sq), 66870.55, tolerance = 1e-8)
  expect_equal(t$qm_erro, rep(5339.20 / 16, 3), tolerance = 1e-8)
  expect_match(r$out$rodape$soma, "^confere")
  # car::linearHypothesis no modelo de médias de célula: mesmo F.
  aj <- lm(taxa ~ 0 + potencia, dados_gravacao())
  f_car <- car::linearHypothesis(aj, c(1, -1, 0, 0))$F[[2]]
  expect_equal(t$F[[1]], f_car, tolerance = 1e-8)
})

test_that("polinomiais igualmente espaçados: inteiros das tabelas, emmeans e poly() concordam", {
  m <- trama.models::tr_models_anova_dic(dados_gravacao(), "taxa", "potencia")
  r <- tr_experiments_contrasts(m, "potencia", "polinomiais")
  t <- r$out$tabela
  expect_equal(t$coeficientes, c("-3 -1 1 3", "1 -1 -1 1", "-1 3 -3 1"))
  cp <- stats::contr.poly(4)
  for (j in 1:3) {
    L <- as.numeric(strsplit(t$coeficientes[[j]], " ")[[1]])
    expect_equal(L / sqrt(sum(L^2)), unname(cp[, j]), tolerance = 1e-8)
  }
  em <- summary(emmeans::contrast(emmeans::emmeans(m$ajuste, "potencia"), "poly"))
  expect_equal(t$estimativa, em$estimate, tolerance = 1e-8)
  expect_equal(t$F, em$t.ratio^2, tolerance = 1e-8)
  expect_equal(t$sq, t$sq_regressao, tolerance = 1e-8)
  expect_equal(sum(t$sq), 66870.55, tolerance = 1e-8)
})

test_that("doses desigualmente espaçadas e réplicas desiguais: ortogonal e igual à regressão", {
  d <- dados_desiguais()
  m <- trama.models::tr_models_anova_dic(d, "y", "dose")
  r <- tr_experiments_contrasts(m, "dose", "polinomiais")
  t <- r$out$tabela
  expect_equal(t$sq, t$sq_regressao, tolerance = 1e-8)
  a <- anova(lm(y ~ poly(as.numeric(as.character(dose)), 4), d))
  expect_equal(sum(t$sq), a$`Sum Sq`[[1]], tolerance = 1e-8)
  M <- as.matrix(r$ortogonalidade[, -1])
  expect_true(all(M[upper.tri(M)] == 0))
  expect_match(r$out$rodape$soma, "^confere")
  # Helmert com réplicas desiguais NÃO é ortogonal, e o bloco diz.
  h <- tr_experiments_contrasts(m, "dose", "helmert")
  expect_match(h$out$rodape$soma, "não ortogonais")
  expect_match(h$out$nota, "pares não ortogonais")
})

test_that("Montgomery 6.1: o 2² como contrastes dá os SQ e os efeitos do livro", {
  m <- trama.models::tr_models_anova_factorial(dados_2k(), "y", "A, B")
  r <- tr_experiments_contrasts(m, "A, B", "fatorial 2^k")
  t <- r$out$tabela
  expect_equal(t$termo, c("A", "B", "A:B"))
  expect_equal(t$sq, c(208.33, 75.00, 8.33), tolerance = 0.01 / 75)
  expect_equal(t$efeito, c(8.33, -5.00, 1.67), tolerance = 0.01 / 5)
  expect_equal(sum(t$sq), sum(anova(m$ajuste)$`Sum Sq`[1:3]), tolerance = 1e-8)
})

test_that("controle contra os demais completa um conjunto ortogonal (balanceado)", {
  m <- trama.models::tr_models_anova_dic(dados_gravacao(), "taxa", "potencia")
  r <- tr_experiments_contrasts(m, "potencia", "controle", controle = "200")
  expect_equal(r$out$tabela$termo[[1]], "200 vs demais")
  expect_equal(r$out$tabela$coeficientes[[1]], "-1 -1 3 -1")
  expect_match(r$out$rodape$soma, "^confere")
})

test_that("parcela subdividida: cada contraste usa o seu erro (a ou b)", {
  ms <- split_aveia()
  q <- summary(ms$ajuste)
  erro_a <- q[["Error: bloco:variedade"]][[1]]; erro_b <- q[["Error: Within"]][[1]]
  qm_a <- erro_a["Residuals", "Mean Sq"]; qm_b <- erro_b["Residuals", "Mean Sq"]
  v <- tr_experiments_contrasts(ms, "variedade", "helmert")$out$tabela
  expect_equal(v$qm_erro, rep(qm_a, 2), tolerance = 1e-6)
  expect_equal(v$gl_erro, rep(10, 2), tolerance = 1e-4)
  expect_equal(sum(v$sq), erro_a[trimws(rownames(erro_a)) == "variedade", "Sum Sq"], tolerance = 1e-8)
  n <- tr_experiments_contrasts(ms, "nitrogenio", "polinomiais", doses = "0 0.2 0.4 0.6")$out$tabela
  expect_equal(n$qm_erro, rep(qm_b, 3), tolerance = 1e-6)
  expect_equal(n$gl_erro, rep(45, 3), tolerance = 1e-4)
  expect_equal(n$F, n$sq / qm_b, tolerance = 1e-6)
})

test_that("desdobramento da interação: soma = SQ(N) + SQ(N × V), estimativas do emmeans", {
  ms <- split_aveia()
  r <- tr_experiments_contrasts(ms, "nitrogenio", "polinomiais", doses = "0 0.2 0.4 0.6",
                                dentro = "variedade")
  t <- r$out$tabela
  expect_equal(nrow(t), 9L)
  w <- summary(ms$ajuste)[["Error: Within"]][[1]]
  alvo <- sum(w[trimws(rownames(w)) %in% c("nitrogenio", "variedade:nitrogenio"), "Sum Sq"])
  expect_equal(sum(t$sq), alvo, tolerance = 1e-8)
  em <- summary(emmeans::contrast(emmeans::emmeans(ms$aux_misto, "nitrogenio", by = "variedade"), "poly"))
  expect_equal(t$estimativa, em$estimate, tolerance = 1e-8)
})

test_that("recusas: GLM, doses que não são números, fator numérico", {
  g <- trama.models::tr_models_glm(trama.models::tr_models_example("InsectSprays"), "count", "spray")
  expect_error(tr_experiments_contrasts(g, "spray", "helmert"), class = "tr_experiments_error_not_applicable")
  ms <- split_aveia()
  expect_error(tr_experiments_contrasts(ms, "nitrogenio", "polinomiais"), class = "tr_experiments_error_not_numeric")
  expect_error(tr_experiments_contrasts(ms, "nitrogenio", "polinomiais", doses = "0 1"),
               class = "tr_experiments_error_bad_option")
  m <- trama.models::tr_models_anova_dic(dados_gravacao(), "taxa", "potencia")
  expect_error(tr_experiments_contrasts(m, "potencia", "digitados", contrastes = "1 -1 0 0; 2 -2 0 0"),
               class = "tr_experiments_error_bad_option")
})

test_that("os nós de análise têm ajuda completa e params iguais aos do fn", {
  for (n in .tr_experiments_nos_analisar()) {
    for (s in c("## Descrição", "## Pressupostos", "## Parâmetros", "## Valor", "## Exemplos",
                "## Referências", "## Veja também")) expect_match(n$help, s, fixed = TRUE, info = n$id)
    f <- formals(n$fn)
    for (nm in names(n$params)) {
      expect_true(nm %in% names(f), info = paste(n$id, nm))
      expect_equal(n$params[[nm]]$default, eval(f[[nm]]), info = paste(n$id, nm))
    }
  }
})
