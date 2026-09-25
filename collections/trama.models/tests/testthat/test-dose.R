# Dose-resposta: o desdobramento tem de ser o dos livros (contrastes
# ortogonais nas médias), a curva a das médias com o erro da ANOVA, e a MET a
# conta −b1/(2 b2).

adubo <- function() tr_models_anova_dbc(ex("adubo_dbc"), "producao", "dose", "bloco")

test_that("o desdobramento bate com os contrastes polinomiais à mão", {
  d <- ex("adubo_dbc")
  r <- tr_models_polinomial(adubo(), "dose")
  q <- as.data.frame(r$quadro$tabela)
  # Doses igualmente espaçadas, 4 repetições: SQ_j = r (c_j' ȳ)² / Σ c_j².
  mu <- tapply(d$producao, d$dose, mean)
  C <- stats::contr.poly(5)
  sq <- as.vector(4 * (t(C) %*% mu)^2 / colSums(C^2))
  expect_equal(q$sq[q$termo == "Linear"], sq[[1]])
  expect_equal(q$sq[q$termo == "Quadrático"], sq[[2]])
  expect_equal(q$sq[q$termo == "Cúbico"], sq[[3]])
  expect_equal(q$sq[q$termo == "Desvios da regressão"], sq[[4]])
  # Os componentes somam o SQ de tratamentos, e o resíduo é o da ANOVA.
  quadro <- tr_models_anova_table(adubo())$tabela
  expect_equal(q$sq[q$termo == "Tratamentos"], quadro$sq[quadro$termo == "dose"])
  qm <- quadro$qm[quadro$termo == "Resíduo"]
  expect_equal(q$qm[q$termo == "Resíduo"], qm)
  expect_equal(q$F[q$termo == "Linear"], sq[[1]] / qm)
  expect_equal(q$p_valor[q$termo == "Quadrático"], stats::pf(sq[[2]] / qm, 1, 12, lower.tail = FALSE))
})

test_that("o grau automático é o maior componente significativo, com a falta de ajuste dele", {
  r <- tr_models_polinomial(adubo(), "dose")
  expect_equal(r$modelo$grau, 2L)
  q <- as.data.frame(r$quadro$tabela)
  fa <- q[q$termo == "Falta de ajuste (grau 2)", ]
  expect_equal(fa$gl, 2)
  expect_equal(fa$sq, sum(q$sq[q$termo %in% c("Cúbico", "Desvios da regressão")]))
  expect_gt(fa$p_valor, 0.05)
  # O grau fixado não escolhe; acima do possível, recusa.
  expect_equal(tr_models_polinomial(adubo(), "dose", grau = "1")$modelo$grau, 1L)
  expect_error(tr_models_polinomial(tr_models_anova_dic(ex("ToothGrowth"), "len", "dose"), "dose", grau = "3"),
               class = "tr_models_error_bad_option")
})

test_that("a curva é a das médias, com o erro da ANOVA, R² de livro e a MET", {
  d <- ex("adubo_dbc")
  r <- tr_models_polinomial(adubo(), "dose")
  mu <- tapply(d$producao, d$dose, mean)
  x <- c(0, 50, 100, 150, 200)
  ref <- stats::lm(mu ~ x + I(x^2))
  co <- tr_models_coefficients(r$modelo)$tabela
  expect_equal(co$estimativa, unname(stats::coef(ref)))
  # Erro padrão: QM do resíduo da ANOVA sobre r, e não o resíduo das médias.
  qm <- r$modelo$qm_res
  X <- stats::model.matrix(ref)
  expect_equal(co$erro_padrao, unname(sqrt(diag(qm / 4 * solve(crossprod(X))))))
  q <- as.data.frame(r$quadro$tabela)
  expect_equal(r$modelo$r2, sum(q$sq[q$termo %in% c("Linear", "Quadrático")]) / q$sq[q$termo == "Tratamentos"])
  b <- stats::coef(ref)
  expect_equal(r$modelo$met$x, unname(-b[[2]] / (2 * b[[3]])))
  expect_equal(r$modelo$met$tipo, "máximo")
  expect_true(r$modelo$met$dentro)
  expect_match(r$quadro$rodape[["dose de máximo"]], "^14")
  # Prever numa dose que não foi testada é o ponto da curva.
  p <- tr_models_predict(r$modelo, data.frame(dose = 133))
  expect_equal(p$previsto, unname(b[[1]] + b[[2]] * 133 + b[[3]] * 133^2))
})

test_that("recusas: fator que não é dose, fatorial, poucas doses; nada significativo dá grau 0", {
  expect_error(tr_models_polinomial(milho_dbc(), "hibrido"), class = "tr_models_error_not_numeric")
  fa <- tr_models_anova_factorial(ex("ToothGrowth"), "len", "supp, dose")
  expect_error(tr_models_polinomial(fa, "dose"), class = "tr_models_error_not_applicable")
  # Nada significativo no automático: não recusa (a main dava o quadro); a
  # curva é a média geral, grau 0, e a nota diz.
  pg <- ex("PlantGrowth"); pg$dose <- rep(c(0, 1, 2), each = 10)
  set.seed(3); pg$ruido <- stats::rnorm(30)
  z <- tr_models_polinomial(tr_models_anova_dic(pg, "ruido", "dose"), "dose", grau_max = 2L)
  expect_equal(z$modelo$grau, 0L)
  expect_equal(z$modelo$r2, 0)
  expect_equal(unname(stats::coef(z$modelo$ajuste)), mean(pg$ruido))
  expect_match(z$quadro$nota, "média geral (grau 0)", fixed = TRUE)
  duas <- ex("ToothGrowth"); duas <- duas[duas$dose != 2, ]
  expect_error(tr_models_polinomial(tr_models_anova_dic(duas, "len", "dose"), "dose"),
               class = "tr_models_error_too_few_rows")
})

test_that("a curva cumpre o contrato e os leitores de ANOVA a recusam com classe", {
  m <- tr_models_polinomial(adubo(), "dose")$modelo
  expect_s3_class(m, "tr_models_dose")
  expect_equal(tr_models_info(m)$preditores, "dose")
  expect_equal(nrow(tr_models_residuals(m)), 20L)
  expect_equal(tr_models_fit_stats(m)$gl_residuo, 12)
  expect_s3_class(tr_models_anova_table(m), "tr_models_effects")
  expect_error(tr_models_emmeans(m, "bloco"), class = "tr_models_error_not_applicable")
  expect_error(tr_models_shapiro_residuals(m), class = "tr_models_error_not_applicable")
  expect_error(tr_models_predict(m, validacao = "cruzada"), class = "tr_models_error_not_applicable")
  expect_s3_class(tr_models_plot_regression(m), "ggplot")
})

test_that("no motor: duas saídas, e o card da curva é o gráfico", {
  reg <- models_registry()
  fl <- trama::tr_flow(reg) |>
    trama::tr_add("a", "models/example", dataset = "adubo_dbc") |>
    trama::tr_add("dbc", "models/anova_dbc", resposta = "producao", tratamento = "dose", bloco = "bloco", from = "a") |>
    trama::tr_add("reg", "models/polinomial", tratamento = "dose", from = "dbc") |>
    trama::tr_add("graf", "models/plot_regression", from = "reg")
  expect_s3_class(rodar(fl, "reg", port = "quadro"), "tr_models_effects")
  expect_s3_class(rodar(fl, "reg", port = "modelo"), "tr_models_dose")
  expect_s3_class(rodar(fl, "graf"), "ggplot")
})

test_that("avisos: falta de ajuste significativa e grau que satura", {
  # Resposta em degrau: o linear é significativo e o resto não segue o
  # polinômio; a falta de ajuste do grau escolhido tem de aparecer.
  set.seed(4)
  d <- expand.grid(bloco = paste0("B", 1:4), dose = c(0, 50, 100, 150, 200))
  d$y <- c(2, 2, 6, 6, 9)[match(d$dose, c(0, 50, 100, 150, 200))] + stats::rnorm(20, sd = 0.2)
  d$y[d$dose == 150] <- d$y[d$dose == 150] - 3
  r <- tr_models_polinomial(tr_models_anova_dbc(d, "y", "dose", "bloco"), "dose")
  fa <- as.data.frame(r$quadro$tabela)
  g <- r$modelo$grau
  linha <- if (g == 3L) "Desvios da regressão" else sprintf("Falta de ajuste (grau %d)", g)
  expect_lt(fa$p_valor[fa$termo == linha], 0.05)
  expect_match(r$quadro$nota, "não explica toda a variação entre doses", fixed = TRUE)
  expect_match(tr_models_coefficients(r$modelo)$nota, "falta de ajuste", fixed = TRUE)
  # Três doses, grau 2: passa pelas três médias.
  tg <- tr_models_anova_dic(ex("ToothGrowth"), "len", "dose")
  s <- tr_models_polinomial(tg, "dose", grau = "2")
  expect_equal(s$modelo$r2, 1)
  expect_match(s$quadro$nota, "passa por todas as médias", fixed = TRUE)
  expect_false(grepl("passa por todas", tr_models_polinomial(adubo(), "dose")$quadro$nota))
})
