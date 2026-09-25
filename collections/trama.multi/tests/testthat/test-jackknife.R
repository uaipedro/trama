# Jackknife: as fórmulas contra casos de resposta conhecida, e o alinhamento.

test_that("motor: EP jackknife da média é s/√n, e o viés é zero", {
  d <- tibble::tibble(nome = letters[1:12], x = c(3, 7, 1, 9, 4, 4, 8, 2, 6, 5, 10, 0))
  tab <- .tr_multi_jackknife(d, "x", completo = d,
                             reajustar = function(dd) dd,
                             extrair = function(obj) c(media = mean(obj$x)),
                             no = "x", tabela = "resumo", confianca = .95)
  expect_equal(tab$estimativa, mean(d$x))
  expect_equal(tab$vies, 0, tolerance = 1e-12)
  expect_equal(tab$erro_padrao, stats::sd(d$x) / sqrt(12))
  expect_equal(tab$ic_sup - tab$corrigida, stats::qt(.975, 11) * tab$erro_padrao)
})

test_that("motor: viés da variância com denominador n é o de livro", {
  x <- c(3, 7, 1, 9, 4, 4, 8, 2)
  d <- tibble::tibble(x = x)
  v <- function(obj) c(var_n = mean((obj$x - mean(obj$x))^2))
  tab <- .tr_multi_jackknife(d, "x", d, function(dd) dd, v, "x", "resumo", .95)
  # A corrigida pelo jackknife da variância com n é exatamente a com n − 1.
  expect_equal(tab$corrigida, stats::var(x))
})

test_that("pseudovalores: formato longo, colunas não usadas e média = corrigida", {
  d <- tibble::tibble(nome = letters[1:6], x = c(1, 5, 2, 8, 3, 4), y = c(2, 2, 3, 9, 1, 0))
  ex <- function(obj) c(mx = mean(obj$x), my = mean(obj$y))
  ps <- .tr_multi_jackknife(d, c("x", "y"), d, function(dd) dd, ex, "x", "pseudovalores", .95)
  expect_equal(names(ps), c("obs", "nome", "estatistica", "sem_ela", "pseudovalor", "influencia"))
  expect_equal(nrow(ps), 12L)
  expect_equal(ps$pseudovalor[ps$estatistica == "mx"], d$x)
  expect_equal(ps$nome[ps$estatistica == "my"], d$nome)
})

test_that("pseudovalores: coluna de entrada com nome reservado é substituída pela calculada", {
  d <- tibble::tibble(estatistica = paste("texto", 1:5), nome = letters[1:5], x = c(1, 5, 2, 8, 3))
  ps <- .tr_multi_jackknife(d, "x", d, function(dd) dd, function(o) c(mx = mean(o$x)), "x",
                            "pseudovalores", .95)
  expect_equal(names(ps), c("obs", "nome", "estatistica", "sem_ela", "pseudovalor", "influencia"))
  expect_equal(unique(ps$estatistica), "mx")
})

test_that("casar fatores com coluna de cargas nula não quebra", {
  ref <- matrix(c(.8, .7, .1, .0, .1, .0, .9, .6), 4, dimnames = list(letters[1:4], c("F1", "F2")))
  nula <- ref; nula[, 2] <- 0
  expect_equal(.tr_multi_casar_fatores(nula, ref)[, 1], ref[, 1], ignore_attr = TRUE)
})

test_that("falha numa réplica nomeia a linha; linhas demais é erro", {
  d <- tibble::tibble(x = 1:5)
  err <- tryCatch(.tr_multi_jackknife(d, "x", d, function(dd) {
    if (!3L %in% dd$x) stop("sem o três") else dd
  }, function(o) c(m = mean(o$x)), "multi/jackknife_pca", "resumo", .95), condition = identity)
  expect_s3_class(err, "tr_multi_error_jackknife_replicate")
  expect_match(conditionMessage(err), "linha 3", fixed = TRUE)
  grande <- tibble::tibble(x = seq_len(5001))
  expect_error(.tr_multi_jackknife(grande, "x", grande, identity, function(o) c(m = 1), "x", "resumo", .95),
               class = "tr_multi_error_too_many_rows")
})

test_that("alinhar sinal e casar fatores desfazem troca de sinal e de ordem", {
  ref <- matrix(c(.8, .7, .1, .0, .1, .0, .9, .6), 4, dimnames = list(letters[1:4], c("F1", "F2")))
  virada <- -ref
  expect_equal(.tr_multi_alinhar_sinal(virada, ref), ref, ignore_attr = TRUE)
  trocada <- ref[, c(2, 1)] * rep(c(-1, 1), each = 4)
  expect_equal(.tr_multi_casar_fatores(trocada, ref), ref, ignore_attr = TRUE)
  expect_equal(nrow(.tr_multi_permutacoes(4L)), 24L)
  big <- diag(6); big6 <- big[, c(6, 1:5)]
  expect_equal(.tr_multi_casar_fatores(big6, big), big, ignore_attr = TRUE)
})

test_that("jackknife_pca: autovalores contra laço manual com prcomp", {
  d <- tr_multi_example("USArrests")
  pca <- tr_multi_pca(d)
  tab <- tr_multi_jackknife_pca(pca)
  X <- as.matrix(d[, pca$variaveis])
  reps <- t(vapply(seq_len(nrow(X)), function(i) stats::prcomp(X[-i, ], scale. = TRUE)$sdev^2, numeric(4)))
  n <- nrow(X)
  expect_equal(tab$estatistica, paste0("CP", 1:4))
  expect_equal(tab$erro_padrao, sqrt((n - 1) / n * colSums(sweep(reps, 2, colMeans(reps))^2)),
               ignore_attr = TRUE)
})

test_that("jackknife_pca de cargas não é inflado por troca de sinal", {
  pca <- tr_multi_pca(tr_multi_example("USArrests"))
  tab <- tr_multi_jackknife_pca(pca, estatistica = "cargas")
  expect_equal(nrow(tab), 16L)
  expect_true(all(tab$erro_padrao[grepl(":CP1$", tab$estatistica)] < .2))
  expect_equal(tab$estatistica[1], "Murder:CP1")
})

test_that("jackknife_fa: comunalidades e cargas alinhadas", {
  q <- tr_multi_example("questionario")[1:120, ]
  fa <- tr_multi_factor_analysis(q, fatores = 3L, rotacao = "oblimin")
  expect_true(fa$normalizar)
  tab <- tr_multi_jackknife_fa(fa, estatistica = "cargas")
  expect_equal(nrow(tab), 45L)
  forte <- tab[tab$estatistica %in% c("ans1:F1", "ans1:F2", "ans1:F3"), ]
  expect_true(all(forte$erro_padrao < .15))
  com <- tr_multi_jackknife_fa(fa, estatistica = "comunalidades")
  expect_equal(com$estatistica, fa$variaveis)
})

test_that("jackknife_discriminant: correlação canônica e quadrática recusada", {
  lda <- tr_multi_discriminant(iris_t(), resposta = "Species")
  tab <- tr_multi_jackknife_discriminant(lda)
  expect_equal(tab$estatistica, c("LD1", "LD2"))
  expect_equal(tab$estimativa, c(0.9848, 0.4712), tolerance = 1e-3)
  qda <- tr_multi_discriminant(iris_t(), resposta = "Species", metodo = "quadrática")
  expect_error(tr_multi_jackknife_discriminant(qda), class = "tr_multi_error_bad_option")
})

test_that("jackknife_logistic: coeficientes, EP de Wald ao lado, razões de chances exponenciadas", {
  d <- tr_multi_example("pima")[1:150, ]
  m <- tr_multi_logistic(d, resposta = "diabetes", preditores = "glicose, imc")
  tab <- tr_multi_jackknife_logistic(m)
  expect_equal(tab$estatistica, c("(intercepto)", "glicose", "imc"))
  expect_true("erro_padrao_wald" %in% names(tab))
  expect_equal(tab$erro_padrao, tab$erro_padrao_wald, tolerance = .3)
  or <- tr_multi_jackknife_logistic(m, estatistica = "razões de chances")
  expect_equal(or$estimativa, exp(tab$estimativa))
  expect_equal(or$erro_padrao, tab$erro_padrao)
  # O intervalo exponenciado UMA vez, e não exp(exp(.)).
  expect_equal(or$corrigida, exp(tab$corrigida))
  expect_equal(or$ic_inf, exp(tab$ic_inf))
  expect_equal(or$ic_sup, exp(tab$ic_sup))
  sep <- tr_multi_logistic(iris_t(), resposta = "Species")
  expect_error(tr_multi_jackknife_logistic(sep), class = "tr_multi_error_separation")
})
