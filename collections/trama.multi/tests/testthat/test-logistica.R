# Logística: paridade com glm e multinom, separação e o objeto.

pima <- function() tr_multi_example("pima")
vinhos4 <- "alcool, acidez_malica, magnesio, fenois_totais"

test_that("binária bate com glm binomial: coeficientes e probabilidades", {
  d <- pima()
  m <- tr_multi_logistic(d, grupo = "diabetes")
  expect_s3_class(m, "tr_multi_logit")
  expect_equal(m$tipo, "binária")
  expect_equal(m$niveis, c("não", "sim"))
  ref <- stats::glm(diabetes ~ gestacoes + glicose + pressao + pele + imc + pedigree + idade,
                    family = stats::binomial(), data = d)
  expect_equal(unname(stats::coef(m$ajuste)), unname(stats::coef(ref)), tolerance = 1e-8)
  pr <- .tr_multi_logit_prever(m, .tr_multi_logit_X(m, d))
  expect_equal(unname(pr$prob[, "sim"]), unname(stats::fitted(ref)), tolerance = 1e-8)
  expect_identical(as.character(pr$classe), unname(ifelse(stats::fitted(ref) >= .5, "sim", "não")))
  expect_length(m$separacao, 0L)
})

test_that("o corte muda a classe prevista, não as probabilidades", {
  d <- pima()
  a <- tr_multi_logistic(d, grupo = "diabetes")
  b <- tr_multi_logistic(d, grupo = "diabetes", corte = .3)
  X <- .tr_multi_logit_X(a, d)
  expect_equal(.tr_multi_logit_prever(a, X)$prob, .tr_multi_logit_prever(b, X)$prob)
  expect_gt(sum(.tr_multi_logit_prever(b, X)$classe == "sim"),
            sum(.tr_multi_logit_prever(a, X)$classe == "sim"))
  expect_error(tr_multi_logistic(d, grupo = "diabetes", corte = 1), class = "tr_multi_error_bad_option")
})

test_that("multinomial bate com nnet::multinom", {
  v <- tr_multi_example("vinhos")
  m <- tr_multi_logistic(v, grupo = "cultivar", cols = vinhos4)
  expect_equal(m$tipo, "multinomial")
  expect_true(is.na(m$corte))
  ref <- nnet::multinom(cultivar ~ alcool + acidez_malica + magnesio + fenois_totais,
                        data = v, trace = FALSE, maxit = 1000)
  pr <- .tr_multi_logit_prever(m, .tr_multi_logit_X(m, v))
  expect_equal(unname(pr$prob), unname(stats::fitted(ref)), tolerance = 1e-4)
  expect_length(m$separacao, 0L)
})

test_that("separação completa é guardada, e não derruba o ajuste", {
  m <- tr_multi_logistic(iris_t(), grupo = "Species")
  expect_equal(m$separacao, "setosa")
  v <- tr_multi_logistic(tr_multi_example("vinhos"), grupo = "cultivar")
  expect_equal(v$separacao, "C")
  expect_error(.tr_multi_sem_separacao(m, "multi/logistic_coefficients"),
               class = "tr_multi_error_separation")
  err <- tryCatch(.tr_multi_sem_separacao(m, "x"), condition = identity)
  expect_match(conditionMessage(err), "setosa", fixed = TRUE)
})

test_that("separação binária: mensagem no plural, com os dois grupos", {
  d <- iris_t()
  d <- d[d$Species != "virginica", ]
  m <- tr_multi_logistic(d, grupo = "Species", cols = "Sepal.Length, Petal.Length")
  expect_setequal(m$separacao, c("setosa", "versicolor"))
  err <- tryCatch(.tr_multi_sem_separacao(m, "x"), condition = identity)
  expect_match(conditionMessage(err), "os grupos 'setosa' e 'versicolor' são separados", fixed = TRUE)
})

test_that("avisos de separação do glm são calados também em português", {
  # O R traduz os avisos do `glm.fit`; comparar com o texto em inglês deixava
  # vazar "probabilidades ajustadas numericamente 0 ou 1 ocorreu" no pt_BR.
  d <- iris_t()
  d <- d[d$Species != "virginica", ]
  withr::with_language("pt_BR", {
    expect_no_warning(tr_multi_logistic(d, grupo = "Species", cols = "Sepal.Length, Petal.Length"))
  })
})

test_that("nome de coluna com espaço e acento não quebra o ajuste", {
  d <- pima()
  names(d)[names(d) == "glicose"] <- "glicose em jejum"
  m <- tr_multi_logistic(d, grupo = "diabetes", cols = "glicose em jejum, imc")
  expect_equal(m$preditores, c("glicose em jejum", "imc"))
})

test_that("recusas herdadas: grupo único, faltante, colinear", {
  d <- pima()
  expect_error(tr_multi_logistic(d[d$diabetes == "sim", ], grupo = "diabetes"),
               class = "tr_multi_error_one_group")
  d2 <- d; d2$imc[3] <- NA
  expect_error(tr_multi_logistic(d2, grupo = "diabetes"), class = "tr_multi_error_missing_values")
  d3 <- d; d3$dobro <- d3$imc * 2
  expect_error(tr_multi_logistic(d3, grupo = "diabetes", cols = "imc, dobro"),
               class = "tr_multi_error_singular_matrix")
})

test_that("coeficientes da binária: Wald do glm e razão de chances", {
  d <- pima()
  m <- tr_multi_logistic(d, grupo = "diabetes", cols = "glicose, imc")
  tab <- tr_multi_logistic_coefficients(m)
  expect_equal(names(tab), c("grupo", "referencia", "termo", "coeficiente", "erro_padrao",
                             "z", "p_valor", "razao_chances", "ic_inf", "ic_sup", "intervalo"))
  ref <- stats::coef(summary(stats::glm(diabetes ~ glicose + imc, stats::binomial(), d)))
  expect_equal(tab$termo, c("(intercepto)", "glicose", "imc"))
  expect_equal(unique(tab$grupo), "sim")
  expect_equal(unique(tab$referencia), "não")
  expect_equal(tab$coeficiente, unname(ref[, 1]), tolerance = 1e-8)
  expect_equal(tab$erro_padrao, unname(ref[, 2]), tolerance = 1e-6)
  expect_equal(tab$p_valor, unname(ref[, 4]), tolerance = 1e-6)
  expect_equal(tab$ic_inf, exp(ref[, 1] - stats::qnorm(.975) * ref[, 2]), tolerance = 1e-6,
               ignore_attr = TRUE)
})

test_that("escala por desvio padrão multiplica coeficiente e EP pelo DP, e não mexe no intercepto", {
  d <- pima()
  m <- tr_multi_logistic(d, grupo = "diabetes", cols = "glicose, imc")
  u <- tr_multi_logistic_coefficients(m)
  s <- tr_multi_logistic_coefficients(m, escala = "desvio padrão")
  expect_equal(s$coeficiente[2], u$coeficiente[2] * stats::sd(d$glicose))
  expect_equal(s$erro_padrao[3], u$erro_padrao[3] * stats::sd(d$imc))
  expect_equal(s$coeficiente[1], u$coeficiente[1])
  expect_equal(s$p_valor, u$p_valor)
})

test_that("coeficientes da multinomial: uma linha por grupo × termo, contra a referência", {
  v <- tr_multi_example("vinhos")
  m <- tr_multi_logistic(v, grupo = "cultivar", cols = vinhos4)
  tab <- tr_multi_logistic_coefficients(m)
  expect_equal(nrow(tab), 2L * 5L)
  expect_equal(unique(tab$grupo), c("B", "C"))
  expect_equal(unique(tab$referencia), "A")
  s <- summary(m$ajuste)
  expect_equal(tab$coeficiente[tab$grupo == "C"], unname(s$coefficients["C", ]), tolerance = 1e-8)
  expect_equal(tab$erro_padrao[tab$grupo == "B"], unname(s$standard.errors["B", ]), tolerance = 1e-8)
})

test_that("coeficientes e gráfico recusam modelo com separação", {
  m <- tr_multi_logistic(iris_t(), grupo = "Species")
  expect_error(tr_multi_logistic_coefficients(m), class = "tr_multi_error_separation")
  expect_error(tr_multi_plot_odds(m), class = "tr_multi_error_separation")
  # O erro nomeia o nó do gráfico, não o dos coeficientes a que ele delega.
  expect_error(tr_multi_plot_odds(m), "multi/plot_odds", fixed = TRUE)
})

test_that("o gráfico das razões de chances desenha, e facetado na multinomial", {
  m <- tr_multi_logistic(pima(), grupo = "diabetes")
  p <- tr_multi_plot_odds(m)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  v <- tr_multi_logistic(tr_multi_example("vinhos"), grupo = "cultivar", cols = vinhos4)
  expect_no_error(ggplot2::ggplot_build(tr_multi_plot_odds(v)))
})

test_that("logistic_coefficients: param `confianca` (versão 3); `nivel` só como alias obsoleto na função R", {
  reg <- multi_registry()
  spec <- reg$nodes[["multi/logistic_coefficients"]]
  expect_identical(spec$version, 3L)
  expect_true("confianca" %in% names(spec$params))
  expect_false("nivel" %in% names(spec$params))
  m <- tr_multi_logistic(tr_multi_example("pima"), grupo = "diabetes", cols = "glicose, imc")
  a <- tr_multi_logistic_coefficients(m, confianca = 0.9)
  expect_warning(b <- tr_multi_logistic_coefficients(m, nivel = 0.9), class = "tr_multi_warning_deprecated")
  expect_equal(a, b)
  expect_error(tr_multi_logistic_coefficients(m, confianca = 2), class = "tr_multi_error_bad_option")
})
