# Logística: paridade com glm e multinom, separação e o objeto.

pima <- function() tr_multi_example("pima")
vinhos4 <- "alcool, acidez_malica, magnesio, fenois_totais"

test_that("binária bate com glm binomial: coeficientes e probabilidades", {
  d <- pima()
  m <- tr_multi_logistic(d, resposta = "diabetes")
  expect_s3_class(m, c("tr_multi_logit", "tr_models_fit"), exact = TRUE)
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
  a <- tr_multi_logistic(d, resposta = "diabetes")
  b <- tr_multi_logistic(d, resposta = "diabetes", corte = .3)
  X <- .tr_multi_logit_X(a, d)
  expect_equal(.tr_multi_logit_prever(a, X)$prob, .tr_multi_logit_prever(b, X)$prob)
  expect_gt(sum(.tr_multi_logit_prever(b, X)$classe == "sim"),
            sum(.tr_multi_logit_prever(a, X)$classe == "sim"))
  expect_error(tr_multi_logistic(d, resposta = "diabetes", corte = 1), class = "tr_multi_error_bad_option")
})

test_that("multinomial bate com nnet::multinom", {
  v <- tr_multi_example("vinhos")
  m <- tr_multi_logistic(v, resposta = "cultivar", preditores = vinhos4)
  expect_equal(m$tipo, "multinomial")
  expect_true(is.na(m$corte))
  ref <- nnet::multinom(cultivar ~ alcool + acidez_malica + magnesio + fenois_totais,
                        data = v, trace = FALSE, maxit = 1000)
  pr <- .tr_multi_logit_prever(m, .tr_multi_logit_X(m, v))
  expect_equal(unname(pr$prob), unname(stats::fitted(ref)), tolerance = 1e-4)
  expect_length(m$separacao, 0L)
})

test_that("separação completa é guardada, e não derruba o ajuste", {
  m <- tr_multi_logistic(iris_t(), resposta = "Species")
  expect_equal(m$separacao, "setosa")
  v <- tr_multi_logistic(tr_multi_example("vinhos"), resposta = "cultivar")
  expect_equal(v$separacao, "C")
  expect_error(.tr_multi_sem_separacao(m, "models/coefficients"),
               class = "tr_multi_error_separation")
  err <- tryCatch(.tr_multi_sem_separacao(m, "x"), condition = identity)
  expect_match(conditionMessage(err), "setosa", fixed = TRUE)
})

test_that("separação binária: mensagem no plural, com os dois grupos", {
  d <- iris_t()
  d <- d[d$Species != "virginica", ]
  m <- tr_multi_logistic(d, resposta = "Species", preditores = "Sepal.Length, Petal.Length")
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
    expect_no_warning(tr_multi_logistic(d, resposta = "Species", preditores = "Sepal.Length, Petal.Length"))
  })
})

test_that("nome de coluna com espaço e acento não quebra o ajuste", {
  d <- pima()
  names(d)[names(d) == "glicose"] <- "glicose em jejum"
  m <- tr_multi_logistic(d, resposta = "diabetes", preditores = "glicose em jejum, imc")
  expect_equal(m$preditores, c("glicose em jejum", "imc"))
})

test_that("recusas herdadas: grupo único, faltante, colinear", {
  d <- pima()
  expect_error(tr_multi_logistic(d[d$diabetes == "sim", ], resposta = "diabetes"),
               class = "tr_multi_error_one_group")
  d2 <- d; d2$imc[3] <- NA
  expect_error(tr_multi_logistic(d2, resposta = "diabetes"), class = "tr_multi_error_missing_values")
  d3 <- d; d3$dobro <- d3$imc * 2
  expect_error(tr_multi_logistic(d3, resposta = "diabetes", preditores = "imc, dobro"),
               class = "tr_multi_error_singular_matrix")
})

coefs <- function(...) trama.models::tr_models_coefficients(...)$tabela

test_that("models/coefficients da binária: Wald do glm, e exponenciar dá a razão de chances", {
  d <- pima()
  m <- tr_multi_logistic(d, resposta = "diabetes", preditores = "glicose, imc")
  ef <- trama.models::tr_models_coefficients(m)
  expect_s3_class(ef, "tr_models_effects")
  tab <- ef$tabela
  expect_equal(names(tab), c("grupo", "termo", "estimativa", "erro_padrao", "z", "p_valor",
                             "li_95", "ls_95"))
  ref <- stats::coef(summary(stats::glm(diabetes ~ glicose + imc, stats::binomial(), d)))
  expect_equal(tab$termo, c("(intercepto)", "glicose", "imc"))
  expect_equal(unique(tab$grupo), "sim")
  expect_equal(tab$estimativa, unname(ref[, 1]), tolerance = 1e-8)
  expect_equal(tab$erro_padrao, unname(ref[, 2]), tolerance = 1e-6)
  expect_equal(tab$p_valor, unname(ref[, 4]), tolerance = 1e-6)
  or <- coefs(m, exponenciar = TRUE)
  expect_false("erro_padrao" %in% names(or))
  expect_equal(or$estimativa, exp(unname(ref[, 1])), tolerance = 1e-8)
  expect_equal(or$li_95, exp(ref[, 1] - stats::qnorm(.975) * ref[, 2]), tolerance = 1e-6,
               ignore_attr = TRUE)
  expect_true(all(c("li_90", "ls_90") %in% names(coefs(m, confianca = 0.9))))
})

test_that("escala por desvio padrão multiplica coeficiente e EP pelo DP, e não mexe no intercepto", {
  d <- pima()
  m <- tr_multi_logistic(d, resposta = "diabetes", preditores = "glicose, imc")
  u <- coefs(m)
  s <- coefs(m, escala = "desvio padrão")
  expect_equal(s$estimativa[2], u$estimativa[2] * stats::sd(d$glicose))
  expect_equal(s$erro_padrao[3], u$erro_padrao[3] * stats::sd(d$imc))
  expect_equal(s$estimativa[1], u$estimativa[1])
  expect_equal(s$p_valor, u$p_valor)
})

test_that("coeficientes da multinomial: uma linha por grupo × termo, contra a referência", {
  v <- tr_multi_example("vinhos")
  m <- tr_multi_logistic(v, resposta = "cultivar", preditores = vinhos4)
  ef <- trama.models::tr_models_coefficients(m)
  tab <- ef$tabela
  expect_equal(nrow(tab), 2L * 5L)
  expect_equal(unique(tab$grupo), c("B", "C"))
  expect_match(ef$nota, "referência: 'A'", fixed = TRUE)
  s <- summary(m$ajuste)
  expect_equal(tab$estimativa[tab$grupo == "C"], unname(s$coefficients["C", ]), tolerance = 1e-8)
  expect_equal(tab$erro_padrao[tab$grupo == "B"], unname(s$standard.errors["B", ]), tolerance = 1e-8)
})

test_that("coeficientes, importância e gráfico recusam modelo com separação", {
  m <- tr_multi_logistic(iris_t(), resposta = "Species")
  expect_error(trama.models::tr_models_coefficients(m), class = "tr_multi_error_separation")
  expect_error(trama.models::tr_models_importance_table(m), class = "tr_multi_error_separation")
  expect_error(tr_multi_plot_odds(m), class = "tr_multi_error_separation")
  expect_error(tr_multi_plot_odds(m), "multi/plot_odds", fixed = TRUE)
})

test_that("contrato: info, stats (AIC, desvio, pseudo-R²) e importância |z|", {
  d <- pima()
  m <- tr_multi_logistic(d, resposta = "diabetes", preditores = "glicose, imc")
  i <- trama.models::tr_models_info(m)
  expect_equal(i$niveis, c("não", "sim"))
  expect_equal(i$preditores, c("glicose", "imc"))
  ref <- stats::glm(diabetes ~ glicose + imc, stats::binomial(), d)
  st <- trama.models::tr_models_fit_stats(m)
  expect_equal(st$aic, stats::AIC(ref))
  expect_equal(st$desvio, ref$deviance)
  expect_equal(st$desvio_nulo, ref$null.deviance, tolerance = 1e-10)
  expect_equal(st$pseudo_r2, 1 - ref$deviance / ref$null.deviance, tolerance = 1e-10)
  imp <- trama.models::tr_models_importance_table(m)
  z <- abs(stats::coef(summary(ref))[-1, 3])
  expect_equal(imp$importancia, unname(sort(z, decreasing = TRUE)), tolerance = 1e-6)
  expect_equal(imp$medida[[1]], "|z|")
  v <- tr_multi_logistic(tr_multi_example("vinhos"), resposta = "cultivar", preditores = vinhos4)
  mn <- nnet::multinom(cultivar ~ 1, data = tr_multi_example("vinhos"), trace = FALSE)
  expect_equal(trama.models::tr_models_fit_stats(v)$desvio_nulo, mn$deviance, tolerance = 1e-6)
})

test_that("round-trip no store de models/fit; o card é o das chances, ou a ROC com separação", {
  reg <- multi_registry()
  tipo <- reg$types[["models/fit"]]
  m <- tr_multi_logistic(pima(), resposta = "diabetes")
  path <- tempfile(fileext = ".rds")
  tipo$store(m, path)
  back <- tipo$restore(path)
  expect_identical(back, m)
  expect_equal(trama.models::tr_models_confusion(back, validacao = "resubstituição")$acertos[3], 419L)
  skip_if_not_installed("png")
  expect_true(file.exists(tipo$preview(m, ctx_tmp())$files$png))
  sep <- tr_multi_logistic(iris_t(), resposta = "Species")
  expect_true(file.exists(tipo$preview(sep, ctx_tmp())$files$png))
})

test_that("leitor da logística recusa outro modelo com erro de classe", {
  lda <- tr_multi_discriminant(pima(), resposta = "diabetes")
  for (f in list(tr_multi_plot_odds, tr_multi_jackknife_logistic)) {
    err <- tryCatch(f(lda), condition = identity)
    expect_s3_class(err, "tr_multi_error_not_a_logit")
    expect_match(conditionMessage(err), "precisa de uma Regressão logística", fixed = TRUE)
  }
})

test_that("o gráfico das razões de chances desenha, e facetado na multinomial", {
  m <- tr_multi_logistic(pima(), resposta = "diabetes")
  p <- tr_multi_plot_odds(m)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  v <- tr_multi_logistic(tr_multi_example("vinhos"), resposta = "cultivar", preditores = vinhos4)
  expect_no_error(ggplot2::ggplot_build(tr_multi_plot_odds(v)))
})
