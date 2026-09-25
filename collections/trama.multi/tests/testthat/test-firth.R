# Logística penalizada de Firth (1993) com intervalos de verossimilhança
# penalizada perfilada (Heinze & Schemper 2002). Oráculo: `logistf` no `sex2`
# (o exemplo do pacote, de Heinze & Schemper, com o preditor `dia` quase
# separado), com convergência apertada no oráculo para a comparação valer a
# 1e-6 (coeficientes) e 1e-4 (limites dos intervalos).

sex2_trama <- function() {
  skip_if_not_installed("logistf")
  e <- new.env(); utils::data("sex2", package = "logistf", envir = e)
  d <- e$sex2
  d$case <- factor(d$case, levels = c(0, 1))
  d
}

oraculo_logistf <- function(d) {
  logistf::logistf(case ~ age + oc + vic + vicl + vis + dia, data = transform(d, case = as.integer(case) - 1L),
                   control = logistf::logistf.control(xconv = 1e-12, gconv = 1e-12, lconv = 1e-12, maxit = 200),
                   plcontrol = logistf::logistpl.control(xconv = 1e-12, lconv = 1e-12, maxit = 500))
}

test_that("coeficientes, EP, intervalos perfilados e p batem com o logistf no sex2", {
  d <- sex2_trama()
  o <- oraculo_logistf(d)
  m <- tr_multi_logistic(d, grupo = "case", cols = "age, oc, vic, vicl, vis, dia", metodo = "firth")
  expect_equal(m$metodo, "firth")
  tab <- tr_multi_logistic_coefficients(m)
  expect_equal(tab$termo, c("(intercepto)", "age", "oc", "vic", "vicl", "vis", "dia"))
  expect_equal(tab$coeficiente, unname(stats::coef(o)), tolerance = 1e-6)
  # EP: inversa da informação de Fisher em β̂ (Firth 1993: a variância
  # assintótica é a mesma da ML), calculada nos β do logistf. O `o$var` do
  # logistf é outra convenção — (X'W(1 + h)X)⁻¹, dos dados aumentados — e dá EP
  # 2% a 10% menores aqui; só os intervalos e p (perfilados) entram na decisão.
  X <- stats::model.matrix(~ age + oc + vic + vicl + vis + dia, d)
  pr <- stats::plogis(drop(X %*% stats::coef(o)))
  expect_equal(tab$erro_padrao, unname(sqrt(diag(solve(crossprod(X, X * pr * (1 - pr)))))),
               tolerance = 1e-6)
  expect_equal(log(tab$ic_inf), unname(o$ci.lower), tolerance = 1e-4)
  expect_equal(log(tab$ic_sup), unname(o$ci.upper), tolerance = 1e-4)
  expect_equal(tab$p_valor, unname(o$prob), tolerance = 1e-4)
  expect_equal(tab$intervalo, rep("perfilado", 7))
})

test_that("confiança diferente e escala por desvio padrão continuam batendo", {
  d <- sex2_trama()
  m <- tr_multi_logistic(d, grupo = "case", cols = "age, oc, vic, vicl, vis, dia", metodo = "firth")
  o <- logistf::logistf(case ~ age + oc + vic + vicl + vis + dia,
                        data = transform(d, case = as.integer(case) - 1L), alpha = 0.1,
                        control = logistf::logistf.control(xconv = 1e-12, gconv = 1e-12, lconv = 1e-12, maxit = 200),
                        plcontrol = logistf::logistpl.control(xconv = 1e-12, lconv = 1e-12, maxit = 500))
  t90 <- tr_multi_logistic_coefficients(m, confianca = 0.9)
  expect_equal(log(t90$ic_inf), unname(o$ci.lower), tolerance = 1e-4)
  expect_equal(log(t90$ic_sup), unname(o$ci.upper), tolerance = 1e-4)
  dp <- tr_multi_logistic_coefficients(m, escala = "desvio padrão")
  s <- stats::sd(d$age)
  expect_equal(log(dp$ic_sup[[2]]), log(tr_multi_logistic_coefficients(m)$ic_sup[[2]]) * s,
               tolerance = 1e-8)
})

test_that("separação completa: ML recusa, Firth converge com estimativa finita", {
  d <- data.frame(x1 = c(1:10), x2 = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3),
                  y = factor(rep(c("a", "b"), each = 5)))
  ml <- tr_multi_logistic(d, grupo = "y")
  expect_true(length(ml$separacao) > 0)
  expect_error(tr_multi_logistic_coefficients(ml), class = "tr_multi_error_separation")
  fi <- tr_multi_logistic(d, grupo = "y", metodo = "firth")
  tab <- tr_multi_logistic_coefficients(fi)
  expect_true(all(is.finite(tab$coeficiente)))
  expect_true(all(is.finite(tab$ic_inf)))
  skip_if_not_installed("logistf")
  o <- logistf::logistf(y ~ x1 + x2, data = transform(d, y = as.integer(y) - 1L),
                        control = logistf::logistf.control(xconv = 1e-12, gconv = 1e-12, lconv = 1e-12, maxit = 200),
                        plcontrol = logistf::logistpl.control(xconv = 1e-12, lconv = 1e-12, maxit = 500))
  expect_equal(tab$coeficiente, unname(stats::coef(o)), tolerance = 1e-6)
  expect_equal(log(tab$ic_inf), unname(o$ci.lower), tolerance = 1e-4)
  expect_equal(log(tab$ic_sup), unname(o$ci.upper), tolerance = 1e-4)
})

test_that("Firth prevê, classifica, valida por deixa-um-fora e passa no jackknife", {
  p <- tr_multi_example("pima")
  fi <- tr_multi_logistic(p, grupo = "diabetes", metodo = "firth")
  ml <- tr_multi_logistic(p, grupo = "diabetes")
  # Com n grande o viés de ML é pequeno: as duas quase coincidem.
  expect_equal(tr_multi_logistic_coefficients(fi)$coeficiente,
               tr_multi_logistic_coefficients(ml)$coeficiente, tolerance = 0.05)
  cv <- tr_multi_confusion(fi, validacao = "cruzada")
  expect_s3_class(cv, "tbl_df")
  expect_error(tr_multi_logistic(tr_multi_example("iris"), grupo = "Species", metodo = "firth"),
               class = "tr_multi_error_bad_option")
  expect_error(tr_multi_logistic(p, grupo = "diabetes", metodo = "bayes"),
               class = "tr_multi_error_bad_option")
})

test_that("ML: perfilado por padrão (versão 4), Wald como opção, mesmos coeficientes", {
  p <- tr_multi_example("pima")
  ml <- tr_multi_logistic(p, grupo = "diabetes")
  tab <- tr_multi_logistic_coefficients(ml)
  expect_equal(tab$intervalo, rep("perfilado", nrow(tab)))
  expect_equal(tr_multi_logistic_coefficients(ml, intervalo = "Wald")$intervalo, rep("Wald", nrow(tab)))
  g <- stats::glm(stats::reformulate(ml$preditores, "diabetes"), family = stats::binomial(), data = p)
  expect_equal(tab$coeficiente, unname(stats::coef(g)), tolerance = 1e-8)
})

test_that("perfil difícil (vinhos B × C, quase separado): limites são cruzamentos do χ²₁", {
  # Aqui o `logistf` (maxit 5000) não converge em quatro dos oito limites; nos
  # quatro em que converge, bate. Nos outros, a conferência é a definição: com
  # o coeficiente fixado no limite, 2[l*(β̂) − l*perfil] = χ²₁(0,95).
  v <- tr_multi_example("vinhos")
  v <- v[v$cultivar != "A", ]
  f <- tr_multi_logistic(v, grupo = "cultivar", cols = "alcool, flavonoides, intensidade_cor",
                         metodo = "firth")
  t <- tr_multi_logistic_coefficients(f)
  aj <- f$ajuste
  perfil <- function(j, val) {
    b <- aj$coefficients; b[j] <- val
    r <- .tr_multi_firth_ajuste(aj$X, aj$y, b, seq_along(b) != j)
    expect_true(r$convergiu)
    2 * (aj$lpen - r$lpen)
  }
  for (j in 1:4) {
    expect_equal(perfil(j, log(t$ic_inf[[j]])), stats::qchisq(.95, 1), tolerance = 1e-6)
    expect_equal(perfil(j, log(t$ic_sup[[j]])), stats::qchisq(.95, 1), tolerance = 1e-6)
  }
  # Os quatro limites em que o logistf converge (fixos: logistf 1.26.1).
  expect_equal(log(c(t$ic_sup[[1]], t$ic_inf[[2]], t$ic_sup[[3]], t$ic_inf[[4]])),
               c(-8.800048675, 0.4805977259, -1.855117364, 0.1372993447), tolerance = 1e-6)
  expect_equal(t$coeficiente, c(-104.3051950103, 8.3512149518, -5.2263664230, 0.9716911897),
               tolerance = 1e-8)
})
