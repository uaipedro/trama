# experiments/effect e experiments/error, contra oráculos: a identidade da
# soma (exata), a lei dos grandes números e o ajuste de models (com tolerância
# declarada em cada teste, em múltiplos do erro padrão teórico), e o caso da
# parcela subdividida por simulação de Monte Carlo.

ef <- function(...) tr_experiments_effect(...)

test_that("(a) sem erro, a resposta é exatamente a soma dos termos, e cada termo é o analítico", {
  p <- ds("fatorial", "dose: 0, 50, 100; irrigacao: baixa, alta", 3L)
  p <- ef(p, "intercepto", valor = 50)
  p <- ef(p, "fixo", "irrigacao", efeitos = "baixa = 0, alta = 2")
  p <- ef(p, "quantitativo", "dose", coeficientes = "0.1, -0.001")
  p <- ef(p, "interacao", "dose:irrigacao", efeitos = "0:baixa = 1, 0:alta = -1, 50:baixa = 0, 50:alta = 0, 100:baixa = -1, 100:alta = 1")
  p <- ef(p, "aleatorio", "bloco", sd = 3)
  y <- tr_experiments_error(p, sd = 0)
  u <- y$unidades
  expect_equal(u$y, rowSums(as.matrix(u[, grep("^\\.ef_", names(u))])), tolerance = 1e-12)
  x <- as.numeric(as.character(u$dose))
  expect_equal(u$.ef_dose, 0.1 * x - 0.001 * x^2, tolerance = 1e-12)
  expect_equal(u$.ef_irrigacao, ifelse(u$irrigacao == "alta", 2, 0))
  expect_equal(u$.ef_residuo, rep(0, nrow(u)))
  # O efeito aleatório: um valor por bloco, o mesmo em todas as unidades dele.
  expect_true(all(tapply(u$.ef_bloco, u$bloco, function(v) length(unique(v))) == 1L))
  expect_equal(length(y$termos), 5L)
})

test_that("(a) composto central: o quantitativo em x codificado e o produto x1:x2", {
  p <- ds("composto_central", "x1; x2")
  p <- ef(p, "quantitativo", "x1", coeficientes = "2, -1") |> ef("quantitativo", "x1:x2", coeficientes = "0.5")
  u <- tr_experiments_error(p, sd = 0)$unidades
  expect_equal(u$y, 2 * u$x1 - u$x1^2 + 0.5 * u$x1 * u$x2, tolerance = 1e-12)
})

test_that("a conversão contraste -> efeitos é a analítica, e a conferência Σcτ devolve a magnitude", {
  p <- ds("dic", "dose: 0, 50, 100, 150", 2L)
  p <- ef(p, "fixo", "dose", conjunto = "polinomiais", magnitudes = "linear = 4, quadratico = 1, cubico = 0")
  t <- p$termos[[1]]
  # τ = Σ mⱼ cⱼ / Σcⱼ² com os inteiros das tabelas: 4·(-3,-1,1,3)/20 + 1·(1,-1,-1,1)/4.
  esperado <- 4 * c(-3, -1, 1, 3) / 20 + c(1, -1, -1, 1) / 4
  expect_equal(t$verdadeiro$efeito, esperado, tolerance = 1e-12)
  expect_equal(t$conversao$coeficientes, c("-3 -1 1 3", "1 -1 -1 1", "-1 3 -3 1"))
  expect_equal(t$conversao$conferencia, c(4, 1, 0), tolerance = 1e-12)
  # Helmert e controle: a conferência é exata também.
  h <- ef(ds("dic", "t: A, B, C", 2L), "fixo", "t", conjunto = "helmert", magnitudes = "3, -2")
  expect_equal(h$termos[[1]]$conversao$conferencia, c(3, -2), tolerance = 1e-12)
  expect_equal(sum(h$termos[[1]]$verdadeiro$efeito), 0, tolerance = 1e-12)
  k <- ef(ds("dic", "t: ctrl, A, B", 2L), "fixo", "t", conjunto = "controle", controle = "ctrl", magnitudes = "6, 0")
  expect_equal(k$termos[[1]]$verdadeiro$efeito, c(2, -1, -1), tolerance = 1e-12)
})

test_that("interação por produto de contrastes: células com margens nulas e conferência exata", {
  p <- ds("fatorial", "dose: 0, 50, 100; irrigacao: baixa, alta", 2L)
  p <- ef(p, "interacao", "dose:irrigacao", conjunto = "polinomiais",
          doses = "dose: 0, 50, 100; irrigacao: 0, 1", magnitudes = "Linear:Linear = 2")
  v <- p$termos[[1]]$verdadeiro
  expect_equal(v$efeito, c(0.5, -0.5, 0, 0, -0.5, 0.5), tolerance = 1e-12)
  expect_equal(p$termos[[1]]$conversao$conferencia, c(2, 0), tolerance = 1e-12)
})

test_that("(b) médias por nível convergem aos efeitos e o experiments/contrasts recupera as magnitudes", {
  # r = 1000 por nível, σ = 1. EP de uma média = 0,032; tolerância de 0,16 (5 EP).
  # EP do contraste linear (e do cúbico) = √(20/1000) = 0,141 e do quadrático
  # = √(4/1000) = 0,063: tolerância de 4 EP (0,57 e 0,25).
  p <- ds("dic", "dose: 0, 50, 100, 150", 1000L, .seed = 11)
  y <- tr_experiments_simulate(p, list(list(tipo = "intercepto", valor = 20),
                                       list(tipo = "fixo", fator = "dose", conjunto = "polinomiais",
                                            magnitudes = "linear = 4, quadratico = 1, cubico = 0")),
                               list(sd = 1), .seed = 7)
  u <- y$unidades
  tau <- y$termos[[2]]$verdadeiro$efeito
  medias <- tapply(u$y, u$dose, mean)
  expect_lt(max(abs(unname(medias - mean(medias)) - tau)), 0.16)
  fit <- trama.models::tr_models_anova_dic(u, "y", "dose")
  ct <- tr_experiments_contrasts(fit, "dose", "polinomiais")$out$tabela
  expect_lt(abs(ct$estimativa[[1]] - (4)), 0.57)
  expect_lt(abs(ct$estimativa[[2]] - (1)), 0.25)
  expect_lt(abs(ct$estimativa[[3]] - (0)), 0.57)
})

test_that("(c) o sd do efeito aleatório é recuperado pelo lmer (VarCorr)", {
  # 600 blocos, σ_bloco = 3, σ = 1: EP de σ̂_bloco ≈ 3/√(2·600) ≈ 0,087; tol. 0,35.
  p <- ds("dbc", "t: A, B, C, D", 600L, .seed = 5)
  y <- tr_experiments_simulate(p, list(list(tipo = "intercepto", valor = 10),
                                       list(tipo = "aleatorio", fator = "bloco", sd = 3)),
                               list(sd = 1), .seed = 9)
  fit <- trama.models::tr_models_lmer(y$unidades, formula = "y ~ t + (1 | bloco)")
  vc <- as.data.frame(lme4::VarCorr(fit$ajuste))
  expect_lt(abs(vc$sdcor[vc$grp == "bloco"] - (3)), 0.35)
  expect_lt(abs(vc$sdcor[vc$grp == "Residual"] - (1)), 0.05)
})

test_that("(d) AR(1) e simetria composta: correlação empírica entre tempos ≈ ρ^|i-j| e ρ", {
  # 2000 indivíduos: EP de r ≈ (1 − ρ²)/√2000 < 0,02; tolerância 0,06.
  p <- ds("medidas_repetidas", "t: A, B", 1000L, tempos = "0, 1, 2, 3", .seed = 3)
  cor_tempos <- function(pl) {
    u <- pl$unidades
    w <- do.call(cbind, lapply(levels(u$tempo), function(tt) u$.ef_residuo[u$tempo == tt][order(u$individuo[u$tempo == tt])]))
    stats::cor(w)
  }
  ar <- tr_experiments_simulate(p, list(list(tipo = "intercepto", valor = 0)),
                                list(sd = 2, correlacao = "ar1", rho = 0.6), .seed = 1)
  r <- cor_tempos(ar)
  expect_lt(abs(r[1, 2] - (0.6)), 0.06)
  expect_lt(abs(r[1, 3] - (0.36)), 0.06)
  expect_lt(abs(r[1, 4] - (0.216)), 0.06)
  expect_lt(abs(stats::sd(ar$unidades$.ef_residuo) - (2)), 0.1)
  cs <- tr_experiments_simulate(p, list(list(tipo = "intercepto", valor = 0)),
                                list(correlacao = "simetria_composta", rho = 0.4), .seed = 2)
  r <- cor_tempos(cs)
  expect_true(all(abs(r[upper.tri(r)] - 0.4) < 0.06))
  expect_error(tr_experiments_simulate(ds("dbc"), list(list(tipo = "intercepto")), list(correlacao = "ar1")),
               class = "tr_experiments_error_bad_response")
})

test_that("(e) Poisson, binomial e gama: média e variância batem com a família", {
  # N = 10000. EP da média: Poisson √(5/N) = 0,022; binomial √(2,5/N) = 0,016;
  # gama √(4,5/N) = 0,021. Variância: tolerância de 5% (7% na gama, de curtose maior).
  p <- ds("dic", "t: 10", 1000L, .seed = 1)
  um <- function(dist, valor, ...) {
    tr_experiments_simulate(p, list(list(tipo = "intercepto", valor = valor)),
                            list(distribuicao = dist, ...), .seed = 4)$unidades$y
  }
  y <- um("poisson", log(5))
  expect_lt(abs(mean(y) - (5)), 0.08); expect_equal(stats::var(y), 5, tolerance = 0.05)
  y <- um("binomial", 0, ensaios = 10L)
  expect_lt(abs(mean(y) - (5)), 0.06); expect_equal(stats::var(y), 2.5, tolerance = 0.05)
  y <- um("gama", log(3), forma = 2)
  expect_lt(abs(mean(y) - (3)), 0.08); expect_equal(stats::var(y), 4.5, tolerance = 0.07)
})

test_that("perturbações: sd por nível, caudas t, assimetria e perdidas", {
  # sd por nível com n = 1000: EP relativo do sd ≈ 1/√2000 = 2,2%; tol. 7%.
  base <- list(list(tipo = "intercepto", valor = 0))
  u <- tr_experiments_simulate(ds("dic", "t: A, B", 1000L), base, list(sd_por = "t", sds = "A = 1, B = 3"),
                               .seed = 1)$unidades
  s <- tapply(u$y, u$t, stats::sd)
  expect_equal(as.numeric(s), c(1, 3), tolerance = 0.07)
  p <- ds("dic", "t: 10", 1000L, .seed = 1)
  u <- tr_experiments_simulate(p, base, list(sd = 2, caudas_gl = 5), .seed = 1)$unidades
  expect_equal(stats::sd(u$y), 2, tolerance = 0.05)
  # Curtose em excesso da t5 é 6: bem acima da normal (0).
  z <- (u$y - mean(u$y)) / stats::sd(u$y)
  expect_gt(mean(z^4) - 3, 2)
  u <- tr_experiments_simulate(p, base, list(assimetria = 1), .seed = 1)$unidades
  z <- (u$y - mean(u$y)) / stats::sd(u$y)
  expect_lt(abs(mean(z^3) - (1)), 0.15)
  y <- tr_experiments_simulate(p, base, list(perdidas = 0.1), .seed = 1)
  expect_equal(sum(is.na(y$unidades$y)), 1000L)
  expect_length(y$resposta$perdidas, 1000L)
})

test_that("a análise sugerida sai com a resposta e roda", {
  s <- ds("parcela_subdividida", "irrigacao: baixa, alta; variedade: A, B, C", 4L)
  base <- list(list(tipo = "intercepto", valor = 1), list(tipo = "aleatorio", fator = "bloco:parcela", sd = 0.3))
  y <- tr_experiments_simulate(s, base, list(resposta = "prod"))
  expect_equal(y$analise$params$resposta, "prod")
  for (dist in c("poisson", "binomial")) {
    y <- tr_experiments_simulate(s, base, list(distribuicao = dist))
    expect_equal(y$analise$no, "models/glmer")
    fit <- suppressMessages(suppressWarnings(trama.models::tr_models_glmer(
      y$unidades, formula = y$analise$params$formula, familia = y$analise$params$familia)))
    expect_s3_class(fit, "tr_models_fit")
  }
  y <- tr_experiments_simulate(s, base, list(distribuicao = "gama"))
  expect_equal(y$analise$no, "models/glm")
  expect_true(any(grepl("gama com termo aleatório", y$avisos)))
  cv <- tr_experiments_simulate(ds("dbc"), list(list(tipo = "intercepto", valor = 1),
                                                list(tipo = "covariavel", fator = "peso", inclinacao = 0.5, media = 10, sd = 2)),
                                list(), .seed = 1) |> tryCatch(error = function(e) e)
  expect_s3_class(cv, "tr_experiments_error_bad_term")  # sem a coluna 'peso' no plano
  p <- ds("dbc", covariaveis = "peso")
  y <- tr_experiments_simulate(p, list(list(tipo = "intercepto", valor = 1),
                                       list(tipo = "covariavel", fator = "peso", inclinacao = 0.5, media = 10, sd = 2)),
                               list(), .seed = 1)
  expect_false(anyNA(y$unidades$peso))
  expect_equal(y$analise, list(no = "models/lm", params = list(formula = "y ~ bloco + tratamento + peso")))
})

test_that("erros tipados e acionáveis", {
  s <- ds("dbc")
  expect_error(ef(s, "aleatorio", "bloco:parcela"), class = "tr_experiments_error_bad_term")
  expect_error(ef(s, "fixo", "tratamento", efeitos = "A = 1"), "Faltam: B, C, D", class = "tr_experiments_error_bad_term")
  expect_error(ef(s, "fixo", "tratamento", efeitos = "A = 1, B = 0, C = 0, D = 0, E = 1"), "Não existem",
               class = "tr_experiments_error_bad_term")
  expect_error(ef(s, "fixo", "tratamento", conjunto = "helmert", magnitudes = "cubico = 1"),
               class = "tr_experiments_error_bad_term")
  expect_error(ef(s, "interacao", "bloco:tratamento", efeitos = "1:A = 1"), class = "tr_experiments_error_bad_term")
  s1 <- ef(s, "intercepto", valor = 1)
  expect_error(ef(s1, "intercepto", valor = 2), "já há um termo", class = "tr_experiments_error_bad_term")
  expect_error(tr_experiments_error(s), class = "tr_experiments_error_no_terms")
  y <- tr_experiments_error(s1)
  expect_error(ef(y, "fixo", "tratamento"), class = "tr_experiments_error_response_closed")
  expect_error(tr_experiments_error(y), class = "tr_experiments_error_response_closed")
  expect_error(tr_experiments_error(s1, distribuicao = "poisson", caudas_gl = 5), class = "tr_experiments_error_bad_response")
  expect_error(tr_experiments_error(s1, caudas_gl = 2), class = "tr_experiments_error_bad_response")
})

test_that("a cadeia pura é reprodutível pela semente e não mexe na semente do console", {
  p <- ds("dbc")
  termos <- list(list(tipo = "intercepto", valor = 1), list(tipo = "aleatorio", fator = "bloco", sd = 1))
  set.seed(99); antes <- stats::runif(1); set.seed(99)
  a <- tr_experiments_simulate(p, termos, list(), .seed = 5)
  expect_equal(stats::runif(1), antes)
  expect_identical(a$unidades, tr_experiments_simulate(p, termos, list(), .seed = 5)$unidades)
  expect_false(identical(a$unidades$y, tr_experiments_simulate(p, termos, list(), .seed = 6)$unidades$y))
})

test_that("(f) O CASO DE VALIDAÇÃO: parcela subdividida, H0 da irrigação verdadeira", {
  skip_on_cran()
  # 1000 réplicas por cenário, sementes 1001..2000 derivadas pela cadeia.
  # Intervalo binomial de 99,9% para a taxa nominal (três conferências no mesmo
  # teste): 0,05 ± 3,29·√(0,05·0,95/1000) = [0,027; 0,073]. "Bem acima": a
  # ingênua com erro de parcela passa de 0,15.
  # Medido (25/09/2026): com erro de parcela, ingênua 0,409 e subdividida 0,051;
  # sem erro de parcela, ingênua 0,053 e subdividida 0,070. O 0,070 é ruído de
  # Monte Carlo: 5000 réplicas do F do erro (a), sementes 50001..55000, deram
  # 0,0494.
  s <- ds("parcela_subdividida", "irrigacao: baixa, alta; variedade: A, B, C", 4L, .seed = 42)
  p_irr <- function(fit) {
    t <- trama.models::tr_models_anova_table(fit)$tabela
    t$p_valor[t$termo == "irrigacao"][[1]]
  }
  taxas <- function(com) {
    termos <- c(list(list(tipo = "intercepto", valor = 50), list(tipo = "aleatorio", fator = "bloco", sd = 3),
                     list(tipo = "fixo", fator = "irrigacao", efeitos = "baixa = 0, alta = 0"),
                     list(tipo = "fixo", fator = "variedade", efeitos = "A = 0, B = 1, C = 3")),
                if (com) list(list(tipo = "aleatorio", fator = "bloco:parcela", sd = 4)))
    p <- vapply(1:1000, function(i) {
      d <- tr_experiments_simulate(s, termos, list(sd = 1), .seed = 1000L + i)$unidades
      c(p_irr(trama.models::tr_models_anova_factorial(d, "y", "irrigacao, variedade", "bloco")),
        p_irr(suppressMessages(trama.models::tr_models_anova_split_plot(d, "y", "irrigacao", "variedade", "bloco"))))
    }, numeric(2))
    rowMeans(p < 0.05)
  }
  com <- taxas(TRUE)
  sem <- taxas(FALSE)
  expect_gt(com[[1]], 0.15)                      # ingênua, com erro de parcela
  expect_true(com[[2]] >= 0.027 && com[[2]] <= 0.073)  # parcela subdividida
  expect_true(sem[[1]] >= 0.027 && sem[[1]] <= 0.073)
  expect_true(sem[[2]] >= 0.027 && sem[[2]] <= 0.073)
})

test_that("(g) pelo motor: design -> effect ×N -> error -> anova_split_plot e -> contrasts; vista componentes", {
  reg <- experiments_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("plano", "experiments/design", estrutura = "parcela_subdividida",
                  fatores = "irrigacao: baixa, alta; variedade: 0, 50, 100", repeticoes = 4L) |>
    trama::tr_add("mu", "experiments/effect", tipo = "intercepto", valor = 50, from = "plano") |>
    trama::tr_add("bl", "experiments/effect", tipo = "aleatorio", fator = "bloco", sd = 3, from = "mu") |>
    trama::tr_add("va", "experiments/effect", tipo = "fixo", fator = "variedade", conjunto = "polinomiais",
                  magnitudes = "linear = 6, quadratico = 0", from = "bl") |>
    trama::tr_add("ep", "experiments/effect", tipo = "aleatorio", fator = "bloco:parcela", sd = 4, from = "va") |>
    trama::tr_add("y", "experiments/error", resposta = "prod", sd = 1, from = "ep") |>
    trama::tr_add("sp", "models/anova_split_plot", resposta = "prod", parcela = "irrigacao",
                  subparcela = "variedade", bloco = "bloco", from = "y") |>
    trama::tr_add("ct", "experiments/contrasts", fator = "variedade", from = "sp") |>
    trama::tr_add("comp", "experiments/view", aba = "componentes", from = "y")
  expect_s3_class(rodar(f, "y"), "tr_experiments_plan")
  expect_s3_class(rodar(f, "sp"), "tr_models_fit")
  ct <- rodar(f, "ct", "out")
  expect_equal(nrow(ct$tabela), 2L)
  expect_s3_class(rodar(f, "comp"), "ggplot")
  expect_error(tr_experiments_view(ds("dbc"), "componentes"), class = "tr_experiments_error_bad_option")
})
