# experiments/power e experiments/randomization_test contra oráculos: o poder
# analítico do F não central e do t (stats::pf, stats::power.t.test), a taxa
# nominal sob H0, o p exato do coin e a enumeração à mão do teste pareado de
# Fisher. Tolerâncias: intervalos de Clopper-Pearson de 99,9% (vários
# conferidos no mesmo teste), ou múltiplos declarados do erro de Monte Carlo.

cadeia <- function(p, termos, erro = list(sd = 1), .seed = 1L) tr_experiments_simulate(p, termos, erro, .seed = .seed)
mu <- list(tipo = "intercepto", valor = 10)
# Taxa compatível com o alvo: o teste binomial exato de H0: p = alvo não
# rejeita a 0,1% (Oliveira & Ferreira, 2010), e o alvo cai no IC de
# Clopper-Pearson de 99,9%.
dentro <- function(x, n, alvo) {
  b <- stats::binom.test(x, n, p = alvo, conf.level = 0.999)
  b$p.value > 0.001 && alvo >= b$conf.int[[1]] && alvo <= b$conf.int[[2]]
}

test_that("poder: tabela, gráfico, IC de Clopper-Pearson, semente reprodutível e console intocado", {
  p <- ds("dic", "t: A, B, C", 3L, .seed = 1)
  y <- cadeia(p, list(mu, list(tipo = "fixo", fator = "t", efeitos = "A = 0, B = 0, C = 2")))
  set.seed(5); antes <- stats::runif(1); set.seed(5)
  a <- tr_experiments_power(y, replicas = 20L, .seed = 3)
  expect_equal(stats::runif(1), antes)
  expect_identical(a$tabela, tr_experiments_power(y, replicas = 20L, .seed = 3)$tabela)
  t <- a$tabela
  expect_equal(t$replicas + t$falhas, 20L)
  ic <- stats::binom.test(t$rejeicoes, t$replicas, conf.level = 0.95)$conf.int
  expect_equal(c(t$li, t$ls), as.numeric(ic))
  expect_equal(t$hipotese, "H0 falsa: taxa = poder")
  expect_s3_class(a$out, "ggplot")
  z <- cadeia(p, list(mu, list(tipo = "fixo", fator = "t", efeitos = "A = 0, B = 0, C = 0")))
  expect_equal(tr_experiments_power(z, replicas = 10L)$tabela$hipotese, "H0 verdadeira: taxa = erro tipo I")
})

test_that("poder: recusas tipadas", {
  p <- ds("dic", "t: A, B", 3L)
  expect_error(tr_experiments_power(p), class = "tr_experiments_error_bad_option")  # sem resposta
  y <- cadeia(p, list(mu))
  expect_error(tr_experiments_power(y, termo = "nada", replicas = 10L), "não tem o termo",
               class = "tr_experiments_error_bad_option")
  expect_error(tr_experiments_power(y, analise = "models/nada", replicas = 10L), class = "tr_experiments_error_bad_option")
  expect_error(tr_experiments_power(y, parametros = "xis = 1", replicas = 10L), class = "tr_experiments_error_bad_option")
  expect_error(tr_experiments_power(y, significancia = 0.7, replicas = 10L), class = "tr_experiments_error_bad_option")
  velho <- y; velho$termos[[1]]$argumentos <- NULL
  expect_error(tr_experiments_power(velho, replicas = 10L), "não guardava", class = "tr_experiments_error_bad_option")
})

test_that("(a) DIC e DBC normais: poder simulado ≈ poder do F não central", {
  skip_on_cran()
  # 2000 réplicas; o analítico tem de cair no IC de Clopper-Pearson de 99,9%.
  # DIC k = 4, r = 5, τ = (−1, 0, 0, 1), σ = 1,5: λ = rΣτ²/σ² = 4,44; gl (3, 16).
  p <- ds("dic", "t: A, B, C, D", 5L, .seed = 3)
  y <- cadeia(p, list(mu, list(tipo = "fixo", fator = "t", efeitos = "A = -1, B = 0, C = 0, D = 1")), list(sd = 1.5))
  t <- tr_experiments_power(y, replicas = 2000L, .seed = 11)$tabela
  analitico <- 1 - stats::pf(stats::qf(0.95, 3, 16), 3, 16, ncp = 5 * 2 / 1.5^2)
  expect_true(dentro(t$rejeicoes, t$replicas, analitico))
  # DBC k = 3, b = 6, τ = (−0,8; 0; 0,8), bloco aleatório sd 2, σ = 1:
  # λ = bΣτ²/σ² = 7,68; gl (2, 10). O bloco sai do erro: não entra em λ.
  b <- ds("dbc", "t: A, B, C", 6L, .seed = 3)
  y <- cadeia(b, list(mu, list(tipo = "aleatorio", fator = "bloco", sd = 2),
                      list(tipo = "fixo", fator = "t", efeitos = "A = -0.8, B = 0, C = 0.8")))
  t <- tr_experiments_power(y, replicas = 2000L, .seed = 12)$tabela
  analitico <- 1 - stats::pf(stats::qf(0.95, 2, 10), 2, 10, ncp = 6 * 1.28)
  expect_true(dentro(t$rejeicoes, t$replicas, analitico))
})

test_that("(a) curva: cada ponto da grade bate com o analítico do seu tamanho", {
  skip_on_cran()
  p <- ds("dic", "t: A, B, C", 3L, .seed = 1)
  y <- cadeia(p, list(mu, list(tipo = "fixo", fator = "t", efeitos = "A = -1, B = 0, C = 1")))
  t <- tr_experiments_power(y, replicas = 1000L, repeticoes = "3, 6", .seed = 2)$tabela
  expect_equal(t$repeticoes, c(3L, 6L))
  expect_equal(t$unidades, c(9L, 18L))
  for (i in 1:2) {
    r <- t$repeticoes[[i]]
    analitico <- 1 - stats::pf(stats::qf(0.95, 2, 3 * r - 3), 2, 3 * r - 3, ncp = r * 2)
    expect_true(dentro(t$rejeicoes[[i]], t$replicas[[i]], analitico), info = r)
  }
  expect_gt(t$taxa[[2]], t$taxa[[1]])
})

test_that("(b) dois grupos: poder simulado ≈ stats::power.t.test", {
  skip_on_cran()
  # O F do DIC com 2 tratamentos é t²: o mesmo teste bilateral.
  p <- ds("dic", "t: A, B", 8L, .seed = 4)
  y <- cadeia(p, list(mu, list(tipo = "fixo", fator = "t", efeitos = "A = 0, B = 1.2")))
  t <- tr_experiments_power(y, replicas = 2000L, .seed = 13)$tabela
  analitico <- stats::power.t.test(n = 8, delta = 1.2, sd = 1, sig.level = 0.05)$power
  expect_true(dentro(t$rejeicoes, t$replicas, analitico))
})

test_that("(c) sob H0 a taxa é α; contraste linear com poder e cúbico nulo", {
  skip_on_cran()
  p <- ds("dic", "t: A, B, C, D", 4L, .seed = 5)
  y <- cadeia(p, list(mu, list(tipo = "fixo", fator = "t", efeitos = "A = 0, B = 0, C = 0, D = 0")))
  t <- tr_experiments_power(y, replicas = 2000L, .seed = 14)$tabela
  expect_equal(t$hipotese, "H0 verdadeira: taxa = erro tipo I")
  expect_true(dentro(t$rejeicoes, t$replicas, 0.05))
  t <- tr_experiments_power(y, replicas = 2000L, significancia = 0.01, .seed = 15)$tabela
  expect_true(dentro(t$rejeicoes, t$replicas, 0.01))
  # Contraste: linear = 6 em -3 -1 1 3, r = 4, σ = 1: λ = 6²·4/20 = 7,2; gl (1, 12).
  d <- ds("dic", "dose: 0, 50, 100, 150", 4L, .seed = 6)
  y <- cadeia(d, list(mu, list(tipo = "fixo", fator = "dose", conjunto = "polinomiais",
                               magnitudes = "linear = 6, quadratico = 0, cubico = 0")))
  t <- tr_experiments_power(y, termo = "dose", conjunto = "polinomiais", contraste = "linear",
                            replicas = 2000L, .seed = 16)$tabela
  expect_equal(t$hipotese, "H0 falsa: taxa = poder")
  analitico <- 1 - stats::pf(stats::qf(0.95, 1, 12), 1, 12, ncp = 7.2)
  expect_true(dentro(t$rejeicoes, t$replicas, analitico))
  t <- tr_experiments_power(y, termo = "dose", conjunto = "polinomiais", contraste = "cubico",
                            replicas = 2000L, .seed = 17)$tabela
  expect_equal(t$hipotese, "H0 verdadeira: taxa = erro tipo I")
  expect_true(dentro(t$rejeicoes, t$replicas, 0.05))
})

test_that("(c) parcela subdividida sem efeito de irrigação: a ingênua erra o tipo I, a subdividida não", {
  skip_on_cran()
  # 1000 réplicas. A subdividida tem de conter 0,05 no IC de 99,9%; a ingênua
  # (fatorial em DBC, que testa a irrigação contra o resíduo das subparcelas)
  # passa de 0,15 — o critério do caso de validação.
  s <- ds("parcela_subdividida", "irrigacao: baixa, alta; variedade: A, B, C", 4L, .seed = 42)
  y <- cadeia(s, list(list(tipo = "intercepto", valor = 50), list(tipo = "aleatorio", fator = "bloco", sd = 3),
                      list(tipo = "fixo", fator = "irrigacao", efeitos = "baixa = 0, alta = 0"),
                      list(tipo = "fixo", fator = "variedade", efeitos = "A = 0, B = 1, C = 3"),
                      list(tipo = "aleatorio", fator = "bloco:parcela", sd = 4)))
  sp <- tr_experiments_power(y, termo = "irrigacao", replicas = 1000L, .seed = 21)$tabela
  ing <- tr_experiments_power(y, analise = "models/anova_factorial", parametros = "fatores = irrigacao, variedade; bloco = bloco",
                              termo = "irrigacao", replicas = 1000L, .seed = 21)$tabela
  expect_equal(sp$analise, "models/anova_split_plot")
  expect_equal(sp$hipotese, "H0 verdadeira: taxa = erro tipo I")
  expect_true(dentro(sp$rejeicoes, sp$replicas, 0.05))
  expect_equal(sp$p_binomial, stats::binom.test(sp$rejeicoes, sp$replicas, p = 0.05)$p.value)
  expect_true(is.na(ing$p_binomial) || ing$p_binomial < 0.001)
  expect_gt(ing$taxa, 0.15)
})

# ---------------------------------------------------------------------------

test_that("aleatorização (a): DIC 4 + 4 enumerado = coin exato", {
  p <- ds("dic", "t: A, B", 4L, .seed = 3)
  y <- cadeia(p, list(mu, list(tipo = "fixo", fator = "t", efeitos = "A = 0, B = 1.5")), .seed = 2)
  r <- tr_experiments_randomization_test(y, replicas = 999L)
  t <- r$tabela
  expect_equal(t$metodo, "exato (enumeração)")
  expect_equal(t$alocacoes, choose(8, 4))
  expect_equal(t$admissiveis, 70)
  oraculo <- coin::pvalue(coin::oneway_test(y ~ t, data = as.data.frame(y$unidades), distribution = "exact"))
  expect_equal(t$p_valor, as.numeric(oraculo), tolerance = 1e-12)
  expect_s3_class(r$out, "ggplot")
  expect_equal(nrow(r$distribuicao), 70L)
})

test_that("aleatorização (b): DBC 2 × 6 enumerado = teste pareado de Fisher à mão, e ≈ coin estratificado", {
  b <- ds("dbc", "t: A, B", 6L, .seed = 3)
  y <- cadeia(b, list(mu, list(tipo = "aleatorio", fator = "bloco", sd = 2),
                      list(tipo = "fixo", fator = "t", efeitos = "A = 0, B = 1")), .seed = 2)
  t <- tr_experiments_randomization_test(y)$tabela
  expect_equal(t$admissiveis, 2^6)
  u <- as.data.frame(y$unidades)
  d <- vapply(split(u, u$bloco), function(g) g$y[g$t == "B"] - g$y[g$t == "A"], 1)
  sinais <- as.matrix(expand.grid(rep(list(c(-1, 1)), 6)))
  obs <- abs(sum(d))
  oraculo <- mean(abs(sinais %*% d) >= obs - 1e-9)
  expect_equal(t$p_valor, oraculo, tolerance = 1e-12)
  # coin não enumera com blocos: Monte Carlo de 10^5, EP ≤ 0,0016; tolerância 0,01.
  set.seed(1)
  ap <- coin::pvalue(coin::oneway_test(y ~ t | bloco, data = u, distribution = coin::approximate(nresample = 1e5)))
  expect_lt(abs(t$p_valor - as.numeric(ap)), 0.01)
})

test_that("aleatorização: Monte Carlo ≈ exato, (b + 1)/(R + 1), e semente reprodutível", {
  p <- ds("dic", "t: A, B, C", 2L, .seed = 7)
  y <- cadeia(p, list(mu, list(tipo = "fixo", fator = "t", efeitos = "A = 0, B = 0.5, C = 1")), .seed = 3)
  ex <- tr_experiments_randomization_test(y, metodo = "exato")$tabela
  expect_equal(ex$admissiveis, factorial(6) / factorial(2)^3)
  mc <- tr_experiments_randomization_test(y, metodo = "monte carlo", replicas = 399L, .seed = 5)
  t <- mc$tabela
  expect_equal(t$p_valor, (t$maiores_ou_iguais + 1) / (t$alocacoes + 1))
  # EP de Monte Carlo com R = 399: ≤ 0,025; tolerância de 4 EP.
  expect_lt(abs(t$p_valor - ex$p_valor), 0.1)
  expect_identical(t, tr_experiments_randomization_test(y, metodo = "monte carlo", replicas = 399L, .seed = 5)$tabela)
})

test_that("aleatorização: o re-sorteio respeita o escopo (parcela subdividida) e a resposta fica na unidade", {
  s <- ds("parcela_subdividida", "irrigacao: baixa, alta; variedade: A, B, C", 3L, .seed = 8)
  y <- cadeia(s, list(mu, list(tipo = "aleatorio", fator = "bloco:parcela", sd = 1)))
  for (sem in 1:20) {
    d <- trama.experiments:::.tr_exp_av_realocar(y, y$unidades, "y", sem)
    expect_identical(d$y, y$unidades$y)
    expect_identical(d$parcela, y$unidades$parcela)
    # A irrigação é da parcela: constante nela; cada bloco tem as duas.
    expect_true(all(tapply(as.character(d$irrigacao), d$parcela, function(v) length(unique(v))) == 1L))
    expect_true(all(table(d$bloco, d$irrigacao) == 3L))
    # A variedade é da subparcela: as três em cada parcela.
    expect_true(all(table(d$parcela, d$variedade) == 1L))
  }
  expect_error(tr_experiments_randomization_test(y, metodo = "exato"), class = "tr_experiments_error_bad_option")
  t <- tr_experiments_randomization_test(y, termo = "irrigacao", replicas = 19L)$tabela
  expect_match(t$metodo, "Monte Carlo")
  expect_match(t$escopo, "irrigacao: parcelas dentro do bloco", fixed = TRUE)
})

test_that("aleatorização com dados: junta por unidade e recusa alocação diferente da do plano", {
  p <- ds("dic", "t: A, B", 4L, .seed = 3)
  obs <- data.frame(unidade = rev(p$unidades$unidade), peso = c(5, 7, 6, 9, 4, 8, 7, 10))
  t <- tr_experiments_randomization_test(p, obs, resposta = "peso")$tabela
  u <- p$unidades; u$peso <- obs$peso[match(u$unidade, obs$unidade)]
  oraculo <- coin::pvalue(coin::oneway_test(peso ~ t, data = as.data.frame(u), distribution = "exact"))
  expect_equal(t$p_valor, as.numeric(oraculo), tolerance = 1e-12)
  ruim <- obs
  ruim$t <- ifelse(as.character(u$t[match(obs$unidade, u$unidade)]) == "A", "B", "A")
  expect_error(tr_experiments_randomization_test(p, ruim, resposta = "peso"), "não é a alocação",
               class = "tr_experiments_error_bad_option")
  expect_error(tr_experiments_randomization_test(p), class = "tr_experiments_error_blank_param")
})

test_that("aleatorização (c): em dados normais, p de aleatorização ≈ p do F (Pitman)", {
  skip_on_cran()
  # DBC 4 × 5, 5 conjuntos de dados, R = 1999 (EP de Monte Carlo ≤ 0,011).
  # Tolerância 0,05: 4,5 EP mais a diferença de aproximação F/aleatorização.
  b <- ds("dbc", "t: A, B, C, D", 5L, .seed = 9)
  difs <- vapply(1:5, function(k) {
    y <- cadeia(b, list(mu, list(tipo = "aleatorio", fator = "bloco", sd = 1),
                        list(tipo = "fixo", fator = "t", efeitos = "A = 0, B = 0.3, C = 0.6, D = 0.9")), .seed = 100 + k)
    t <- tr_experiments_randomization_test(y, replicas = 1999L, .seed = k)$tabela
    t$p_valor - t$p_parametrico
  }, 1)
  expect_lt(max(abs(difs)), 0.05)
})
