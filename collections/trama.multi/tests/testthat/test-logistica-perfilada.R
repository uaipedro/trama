# IC perfilado da logística ML binária (Venables & Ripley 2002, sec. 7.2;
# Hosmer, Lemeshow & Sturdivant 2013, sec. 1.4): o limite é o β_j em que o
# desvio perfilado sobe χ²₁(confiança) acima do mínimo. Oráculo:
# `confint` do `glm` (o método de perfil do MASS, que desde o R 4.4 mora no
# `stats`: profile em grade + spline). A spline da grade padrão erra ~6e-4 no
# n pequeno; com grade fina (`del = 0.01`) o oráculo fica a 2e-6 do trama, e
# é contra ela que vai a tolerância de 1e-4 (a padrão a 1e-3). Os limites
# também conferem pela definição (desvio perfilado = χ²₁ a 1e-6); p da razão de
# verossimilhanças contra `drop1(test = "LRT")` e o glm sem intercepto.

pima_glm <- function(d, cols) {
  f <- stats::as.formula(paste("diabetes ~", paste(cols, collapse = " + ")))
  stats::glm(f, family = stats::binomial(), data = d)
}

test_that("binária ML: IC perfilado bate com MASS::confint (pima inteiro e pequeno)", {
  pima <- tr_multi_example("pima")
  pequeno <- as.data.frame(pima)[seq(1, nrow(pima), by = 12), ]  # n pequeno, onde Wald ≠ perfil
  cols <- c("glicose", "imc", "pedigree")
  for (d in list(pima, pequeno)) {
    m <- tr_multi_logistic(d, resposta = "diabetes", preditores = paste(cols, collapse = ", "))
    t <- tr_multi_logistic_coefficients(m)
    g <- pima_glm(as.data.frame(d), cols)
    o <- suppressMessages(stats::confint(stats::profile(g, del = 0.01, maxsteps = 1000L)))
    expect_equal(log(t$ic_inf), unname(o[, 1]), tolerance = 1e-4)
    expect_equal(log(t$ic_sup), unname(o[, 2]), tolerance = 1e-4)
    o_padrao <- suppressMessages(stats::confint(g))
    expect_equal(log(c(t$ic_inf, t$ic_sup)), unname(c(o_padrao)), tolerance = 1e-3)
    X <- stats::model.matrix(g)
    for (j in 1:4) for (lim in log(c(t$ic_inf[[j]], t$ic_sup[[j]]))) {
      expect_equal(.tr_multi_logit_desvio_perfil(X, g$y, j, lim) - g$deviance, stats::qchisq(.95, 1),
                   tolerance = 1e-6)
    }
    expect_equal(t$intervalo, rep("perfilado", 4))
    t90 <- tr_multi_logistic_coefficients(m, confianca = 0.9)
    o90 <- suppressMessages(stats::confint(stats::profile(g, del = 0.01, maxsteps = 1000L), level = 0.9))
    expect_equal(log(c(t90$ic_inf, t90$ic_sup)), unname(c(o90)), tolerance = 1e-4)
  }
})

test_that("binária ML: p da razão de verossimilhanças bate com drop1 e com o glm sem intercepto", {
  d <- as.data.frame(tr_multi_example("pima"))
  d <- d[seq(1, nrow(d), by = 12), ]
  cols <- c("glicose", "imc", "pedigree")
  m <- tr_multi_logistic(d, resposta = "diabetes", preditores = paste(cols, collapse = ", "))
  t <- tr_multi_logistic_coefficients(m)
  g <- pima_glm(d, cols)
  dr <- stats::drop1(g, test = "LRT")
  expect_equal(t$p_valor[-1], unname(dr[cols, "Pr(>Chi)"]), tolerance = 1e-10)
  g0 <- stats::update(g, . ~ . - 1)
  expect_equal(t$p_valor[[1]], stats::pchisq(stats::deviance(g0) - stats::deviance(g), 1, lower.tail = FALSE),
               tolerance = 1e-10)
})

test_that("Wald continua como opção e reproduz a fórmula; escala por DP leva o perfil pelo fator", {
  d <- as.data.frame(tr_multi_example("pima")); d <- d[seq(1, nrow(d), by = 12), ]
  m <- tr_multi_logistic(d, resposta = "diabetes", preditores = "glicose, imc, pedigree")
  w <- tr_multi_logistic_coefficients(m, intervalo = "Wald")
  expect_equal(w$intervalo, rep("Wald", 4))
  expect_equal(log(w$ic_sup), w$coeficiente + stats::qnorm(.975) * w$erro_padrao)
  expect_equal(w$p_valor, 2 * stats::pnorm(-abs(w$z)))
  p <- tr_multi_logistic_coefficients(m)
  dp <- tr_multi_logistic_coefficients(m, escala = "desvio padrão")
  expect_equal(log(dp$ic_inf[[2]]), log(p$ic_inf[[2]]) * stats::sd(d$glicose), tolerance = 1e-10)
  expect_equal(dp$p_valor, p$p_valor)
})

test_that("multinomial: sem implementação de referência do perfil, fica Wald e diz", {
  m <- tr_multi_logistic(tr_multi_example("vinhos"), resposta = "cultivar",
                         preditores = "alcool, acidez_malica, magnesio, fenois_totais")
  t <- tr_multi_logistic_coefficients(m)
  expect_equal(unique(t$intervalo), "Wald")
  expect_equal(log(t$ic_sup), t$coeficiente + stats::qnorm(.975) * t$erro_padrao)
})

test_that("Firth aceita Wald como opção; perfilado continua o padrão", {
  d <- data.frame(x1 = 1:10, x2 = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3),
                  g = factor(rep(c("a", "b"), each = 5)))
  m <- tr_multi_logistic(d, resposta = "g", metodo = "firth")
  expect_equal(unique(tr_multi_logistic_coefficients(m)$intervalo), "perfilado")
  w <- tr_multi_logistic_coefficients(m, intervalo = "Wald")
  expect_equal(log(w$ic_inf), w$coeficiente - stats::qnorm(.975) * w$erro_padrao)
  expect_error(tr_multi_logistic_coefficients(m, intervalo = "x"), class = "tr_multi_error_bad_option")
})
