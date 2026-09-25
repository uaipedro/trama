test_that("pressupostos batem com as funções de referência", {
  m <- milho_dbc()
  res <- stats::residuals(m$ajuste)
  expect_equal(tr_models_shapiro_residuals(m)$p_valor, stats::shapiro.test(res)$p.value)
  dic <- tr_models_anova_dic(ex("PlantGrowth"), "weight", "group")
  res_dic <- stats::residuals(dic$ajuste)
  expect_equal(tr_models_bartlett(dic)$p_valor, stats::bartlett.test(res_dic, dic$dados$group)$p.value)
  expect_equal(tr_models_levene(dic)$p_valor,
               car::leveneTest(res_dic, dic$dados$group)$`Pr(>F)`[[1]])
  t <- tr_models_tukey_additivity(m)
  expect_s3_class(t, "tr_models_test")
  expect_match(t$gl, "^1; ")
})

test_that("Breusch-Pagan de Koenker bate com a conta à mão", {
  cars_m <- tr_models_lm(ex("cars"), "dist", "speed")
  e2 <- stats::residuals(cars_m$ajuste)^2
  aux <- summary(stats::lm(e2 ~ ex("cars")$speed))
  est <- nrow(ex("cars")) * aux$r.squared
  bp <- tr_models_breusch_pagan(cars_m)
  expect_equal(bp$estatistica, est)
  expect_equal(bp$p_valor, stats::pchisq(est, 1, lower.tail = FALSE))
})

test_that("pressuposto que não se aplica vira card vermelho explicando", {
  g <- tr_models_glm(ex("InsectSprays"), "count", "spray")
  err <- tryCatch(tr_models_shapiro_residuals(g), condition = identity)
  expect_s3_class(err, "tr_models_error_not_applicable")
  expect_match(conditionMessage(err), "GLM", fixed = TRUE)
  expect_error(tr_models_levene(tr_models_lm(ex("cars"), "dist", "speed")), class = "tr_models_error_not_applicable")
  dic <- tr_models_anova_dic(ex("PlantGrowth"), "weight", "group")
  expect_error(tr_models_tukey_additivity(dic), class = "tr_models_error_not_applicable")
})

test_that("testes clássicos batem com o stats", {
  tg <- ex("ToothGrowth")
  t <- tr_models_t_test(tg, "len", "supp")
  ref <- stats::t.test(len ~ supp, data = tg)
  expect_equal(t$p_valor, ref$p.value)
  expect_equal(t$efeito$valor, unname(diff(rev(ref$estimate))))
  expect_equal(tr_models_t_test(tg, "len", "supp", variancias_iguais = TRUE)$teste, "t de Student")
  expect_equal(tr_models_kruskal(ex("InsectSprays"), "count", "spray")$p_valor,
               stats::kruskal.test(count ~ spray, data = ex("InsectSprays"))$p.value)
  mt <- ex("mtcars")
  expect_equal(tr_models_cor_test(mt, "wt", "mpg")$efeito$valor, stats::cor(mt$wt, mt$mpg))
  expect_equal(tr_models_fisher_exact(mt, "am", "vs")$p_valor, stats::fisher.test(table(mt$am, mt$vs))$p.value)
  expect_equal(tr_models_chisq(ex("warpbreaks"), "wool", "tension")$estatistica, 0, tolerance = 1e-12)
  expect_equal(tr_models_one_sample_t(ex("PlantGrowth"), "weight", mu = 5)$p_valor,
               stats::t.test(ex("PlantGrowth")$weight, mu = 5)$p.value)
  expect_equal(tr_models_shapiro(ex("PlantGrowth"), "weight")$p_valor,
               stats::shapiro.test(ex("PlantGrowth")$weight)$p.value)
  s <- datasets::sleep
  d <- data.frame(a = s$extra[1:10], b = s$extra[11:20])
  expect_equal(tr_models_paired_t(d, "a", "b")$p_valor, stats::t.test(d$a - d$b)$p.value)
  expect_s3_class(tr_models_wilcoxon(tg, "len", "supp"), "tr_models_test")
})

test_that("conclusão segue a alternativa, e grupo que não é dois é erro", {
  tg <- ex("ToothGrowth")
  t <- tr_models_t_test(tg, "len", "supp", alternativa = "maior")
  expect_match(t$conclusao, "OJ maior", fixed = TRUE)
  expect_error(tr_models_t_test(ex("PlantGrowth"), "weight", "group"), class = "tr_models_error_two_groups")
  expect_error(tr_models_t_test(tg, "len", "supp", alternativa = "diferente"), class = "tr_models_error_bad_option")
})

test_that("qui-quadrado com esperado pequeno aponta o Fisher", {
  mt <- ex("mtcars")
  expect_match(tr_models_chisq(mt, "cyl", "gear")$nota, "models/fisher_exact", fixed = TRUE)
})

test_that("o guard dos tipos recusa objeto sem os campos", {
  tipo <- models_test_type()
  expect_error(tipo$store(list(teste = "x"), tempfile()), class = "tr_models_error_not_a_test")
  expect_error(models_fit_type()$store(stats::lm(mpg ~ wt, mtcars), tempfile()), class = "tr_models_error_not_a_fit")
  expect_error(models_effects_type()$store(.tr_models_efeitos(data.frame(a = 1), "x"), tempfile()),
               class = "tr_models_error_not_effects")
  expect_error(models_emm_type()$store(list(), tempfile()), class = "tr_models_error_not_emm")
})

test_that("previews dos tipos saem sem erro para todo modelo", {
  mods <- list(milho_dbc(),
               tr_models_lm(ex("mtcars"), formula = "mpg ~ wt"),
               tr_models_glm(ex("InsectSprays"), "count", "spray"),
               tr_models_lmer(ex("sleepstudy"), formula = "Reaction ~ Days + (1 | Subject)"),
               tr_models_anova_split_plot(ex("aveia"), "producao", "variedade", "nitrogenio", "bloco"))
  for (m in mods) {
    pv <- models_fit_type()$preview(m, ctx_tmp())
    expect_equal(pv$renderer, "models/fit", info = m$rotulo)
    expect_true(length(pv$data$linhas) > 0L, info = m$rotulo)
    expect_no_error(jsonlite::toJSON(pv$data, auto_unbox = TRUE, null = "null"))
    expect_s3_class(.tr_models_fit_tabela(m), "data.frame")
  }
  pv <- models_effects_type()$preview(tr_models_anova_table(milho_dbc()), ctx_tmp())
  q <- pv$data$quadro
  expect_equal(vapply(q$colunas, `[[`, "", "rotulo"), c("FV", "GL", "SQ", "QM", "Fc", "Pr > F"))
  expect_equal(q$linhas[[length(q$linhas)]]$termo, "Total")
  pv <- models_test_type()$preview(tr_models_shapiro_residuals(milho_dbc()), ctx_tmp())
  expect_equal(pv$data$estrelas, "ns")
  pv <- models_emm_type()$preview(tr_models_emmeans(milho_dbc(), "hibrido"), ctx_tmp())
  expect_true(file.exists(pv$files$png %||% unlist(pv$files)[[1]]))
})

test_that("qui-quadrado 2 × 2: sem Yates por padrão, com Yates como opção (oráculo)", {
  # Physicians' Health Study (aspirina × infarto), Agresti, An Introduction to
  # Categorical Data Analysis, cap. 2: placebo 189/10845, aspirina 104/10933.
  # Oráculo duplo: forma fechada do X² de Pearson, n(ad − bc)² / (r1 r2 c1 c2),
  # e da versão de Yates (1934), n(|ad − bc| − n/2)² / (r1 r2 c1 c2), e
  # stats::chisq.test. Tolerância 1e-10 (relativa).
  a <- 189; b <- 10845; c <- 104; d <- 10933; n <- a + b + c + d
  den <- (a + b) * (c + d) * (a + c) * (b + d)
  pearson <- n * (a * d - b * c)^2 / den
  yates <- n * (abs(a * d - b * c) - n / 2)^2 / den
  expect_equal(round(pearson, 2), 25.01)
  dados <- data.frame(
    grupo = rep(c("placebo", "placebo", "aspirina", "aspirina"), c(a, b, c, d)),
    infarto = rep(c("sim", "não", "sim", "não"), c(a, b, c, d)))
  tab <- table(dados$grupo, dados$infarto)
  sem <- tr_models_chisq(dados, "grupo", "infarto")
  expect_equal(unname(sem$estatistica), pearson, tolerance = 1e-10)
  expect_equal(sem$p_valor, stats::chisq.test(tab, correct = FALSE)$p.value, tolerance = 1e-10)
  com <- tr_models_chisq(dados, "grupo", "infarto", correcao = TRUE)
  expect_equal(unname(com$estatistica), yates, tolerance = 1e-10)
  expect_equal(com$p_valor, stats::chisq.test(tab, correct = TRUE)$p.value, tolerance = 1e-10)
  expect_false(formals(tr_models_chisq)$correcao)
})

# ---- Levene em delineamento com bloco: O'Neill & Mathews (2002) --------------
#
# Oráculo: ExpDes.pt 1.2.2, `oneilldbc()` (Ferreira, Cavalcanti & Nogueira).
# O pacote saiu do CRAN em 2026-06-01, então não entra em Suggests: os p abaixo
# foram obtidos rodando `oneilldbc()` do tarball do arquivo do CRAN. O fator
# de correção fechado do DBC (O'Neill & Mathews 2002, sec. do bloco
# casualizado, na forma do ExpDes.pt) é reescrito aqui como segundo oráculo.
m_om_dbc <- function(t, b) {
  rho <- c(-1 / (t - 1), -1 / (b - 1), 1 / ((b - 1) * (t - 1)))
  w <- (2 / pi) * (sqrt(1 - rho^2) + rho * asin(rho) - 1)
  w0 <- 1 - 2 / pi
  (w0 - w[1] - w[2] + w[3]) / (w0 - w[1] + (b - 1) * (w[2] - w[3]))
}

ex4_expdes <- function() data.frame(
  revol = rep(rep(c(5L, 10L, 15L, 20L), each = 3), 2),
  esterco = rep(c("com", "sem"), each = 12), rep = rep(1:3, 8),
  c = c(18, 15, 26, 17, 23, 20, 26, 16, 21, 27, 22, 21,
        33, 30, 18, 27, 18, 20, 29, 22, 31, 37, 34, 33))

test_that("Levene no DBC é o de O'Neill & Mathews e bate com o ExpDes.pt (warpbreaks)", {
  w <- datasets::warpbreaks
  w$bl <- ave(seq_len(nrow(w)), w$wool, w$tension, FUN = seq_along)
  w$tr <- paste(w$wool, w$tension, sep = ".")
  lv <- tr_models_levene(tr_models_anova_dbc(w, "breaks", "tr", "bl"))
  expect_equal(lv$p_valor, 0.0171832205192268, tolerance = 1e-8)  # oneilldbc()
  expect_equal(lv$gl, "5; 40")
  # F corrigido = m × F da ANOVA de |resíduos| em tratamento + bloco
  d <- tr_models_anova_dbc(w, "breaks", "tr", "bl")
  z <- abs(stats::residuals(d$ajuste))
  a <- stats::anova(stats::lm(z ~ factor(d$dados$bl) + factor(d$dados$tr)))
  expect_equal(lv$estatistica, a$`F value`[[2]] * m_om_dbc(6, 9), tolerance = 1e-10)
  expect_match(lv$fonte, "O'Neill & Mathews (2002)", fixed = TRUE)
  # o fatorial em blocos usa as combinações como tratamento: o mesmo teste
  fat <- tr_models_levene(tr_models_anova_factorial(w, "breaks", "wool, tension", "bl"))
  expect_equal(fat$p_valor, lv$p_valor, tolerance = 1e-10)
})

test_that("Levene no DBC bate com o ExpDes.pt no exemplo ex4 (carbono, fatorial em blocos)", {
  e <- ex4_expdes()
  e$tr <- paste(e$revol, e$esterco)
  lv <- tr_models_levene(tr_models_anova_dbc(e, "c", "tr", "rep"))
  expect_equal(lv$p_valor, 0.307081558249168, tolerance = 1e-8)  # oneilldbc()
  expect_equal(lv$gl, "7; 14")
})

test_that("no bloco o centro é sempre o ajuste de mínimos quadrados, e o card diz", {
  m <- milho_dbc()
  expect_equal(tr_models_levene(m, "mediana")$p_valor, tr_models_levene(m, "média")$p_valor)
  expect_match(tr_models_levene(m)$nota, "mínimos quadrados", fixed = TRUE)
})

test_that("Levene no DQL: o fator de correção leva E(F) ao da F (Monte Carlo sob H0)", {
  # Não há oráculo de pacote para o quadrado latino. O multiplicador vem da
  # mesma conta do artigo (razão dos quadrados médios esperados de |e| sob H0,
  # pelas correlações dos resíduos); aqui ele é conferido por simulação.
  os <- datasets::OrchardSprays
  fit <- tr_models_anova_dql(os, "decrease", "treatment", "rowpos", "colpos")
  lv <- tr_models_levene(fit)
  expect_equal(lv$gl, "7; 42")
  X <- stats::model.matrix(~ factor(rowpos) + factor(colpos), os)
  Xf <- stats::model.matrix(~ factor(rowpos) + factor(colpos) + treatment, os)
  Hc <- qr.Q(qr(X)) %*% t(qr.Q(qr(X)))
  Hf <- qr.Q(qr(Xf)) %*% t(qr.Q(qr(Xf)))
  n <- nrow(os); R <- diag(n) - Hf; At <- Hf - Hc
  set.seed(20020301)
  Z <- abs(R %*% matrix(stats::rnorm(n * 20000), n))
  qmt <- mean(colSums(Z * (At %*% Z))) / 7
  qmr <- mean(colSums(Z * (R %*% Z))) / 42
  z <- abs(stats::residuals(fit$ajuste))
  f_ols <- (sum(z * (At %*% z)) / 7) / (sum(z * (R %*% z)) / 42)
  expect_equal(lv$estatistica / f_ols, qmr / qmt, tolerance = 0.01)
})

test_that("Levene com bloco: tamanho a 5% medido por simulação (≈ 5% no médio, conservador no pequeno)", {
  # O multiplicador de O'Neill & Mathews acerta a média do F sob H0, não a
  # cauda. Sob H0 o F só depende de |R e|, com R = I - H do desenho: simula-se
  # e ~ N(0, I) direto na forma matricial (a mesma conta do bloco, conferida
  # contra tr_models_levene em amostras). 20000 réplicas: EP de Monte Carlo
  # ~0,0015 em 5%; tolerância declarada de 1 ponto.
  dbc <- function(t, b) data.frame(trat = factor(rep(seq_len(t), b)), bloco = factor(rep(seq_len(b), each = t)))
  dql <- function(k) {
    d <- expand.grid(linha = seq_len(k), coluna = seq_len(k))
    d$trat <- factor((d$linha + d$coluna) %% k + 1L)
    d$linha <- factor(d$linha); d$coluna <- factor(d$coluna); d
  }
  tamanho <- function(d, ajustar, ctrl, n_rep = 20000L) {
    set.seed(20020301)
    d$y <- stats::rnorm(nrow(d))
    fit <- ajustar(d)
    lv <- tr_models_levene(fit)
    proj <- function(f) { q <- qr(stats::model.matrix(f, d)); Q <- qr.Q(q)[, seq_len(q$rank)]; tcrossprod(Q) }
    Hc <- proj(stats::reformulate(ctrl)); Hf <- proj(stats::reformulate(c(ctrl, "trat")))
    n <- nrow(d); R <- diag(n) - Hf; At <- Hf - Hc
    gl <- as.integer(strsplit(lv$gl, "; ")[[1]])
    fq <- function(Z) (colSums(Z * (At %*% Z)) / gl[[1]]) / (colSums(Z * (R %*% Z)) / gl[[2]])
    m <- lv$estatistica / fq(matrix(abs(R %*% d$y)))
    # a forma matricial é o bloco: outra amostra, mesmo F
    d2 <- d; d2$y <- stats::rnorm(n)
    expect_equal(tr_models_levene(ajustar(d2))$estatistica, m * fq(matrix(abs(R %*% d2$y))), tolerance = 1e-8)
    Z <- abs(R %*% matrix(stats::rnorm(n * n_rep), n))
    mean(stats::pf(m * fq(Z), gl[[1]], gl[[2]], lower.tail = FALSE) < 0.05)
  }
  f_dbc <- function(d) tr_models_anova_dbc(d, "y", "trat", "bloco")
  f_dql <- function(d) tr_models_anova_dql(d, "y", "trat", "linha", "coluna")
  expect_equal(tamanho(dbc(5, 6), f_dbc, "bloco"), 0.05, tolerance = 0.01 / 0.05)
  expect_equal(tamanho(dql(8), f_dql, c("linha", "coluna")), 0.05, tolerance = 0.01 / 0.05)
  expect_lt(tamanho(dbc(4, 3), f_dbc, "bloco"), 0.04)          # conservador (medido: 2,9%)
  expect_lt(tamanho(dql(5), f_dql, c("linha", "coluna")), 0.04) # conservador (medido: 2,9%)
})

test_that("Levene com bloco: casela repetida num bloco e faltando noutro recusa", {
  m <- ex("milho_dbc")
  i <- which(m$bloco == m$bloco[[1]] & m$hibrido == m$hibrido[[1]])
  j <- which(m$bloco != m$bloco[[1]] & m$hibrido == m$hibrido[[1]])[[1]]
  m$bloco[j] <- m$bloco[i]   # o híbrido aparece 2x no bloco 1 e some de outro bloco
  expect_error(tr_models_levene(tr_models_anova_dbc(m, "producao", "hibrido", "bloco")),
               class = "tr_models_error_not_applicable")
})

test_that("Levene em bloco desbalanceado recusa (o multiplicador supõe equilíbrio)", {
  m <- ex("milho_dbc")
  expect_error(tr_models_levene(tr_models_anova_dbc(m[-1, ], "producao", "hibrido", "bloco")),
               class = "tr_models_error_not_applicable")
})

test_that("Bartlett em delineamento com bloco recusa e aponta o Levene", {
  err <- tryCatch(tr_models_bartlett(milho_dbc()), condition = identity)
  expect_s3_class(err, "tr_models_error_block_design")
  expect_match(conditionMessage(err), "models/levene", fixed = TRUE)
  os <- datasets::OrchardSprays
  expect_error(tr_models_bartlett(tr_models_anova_dql(os, "decrease", "treatment", "rowpos", "colpos")),
               class = "tr_models_error_block_design")
})

test_that("Levene e Bartlett na parcela subdividida recusam (sem correção publicada para dois estratos)", {
  # O'Neill & Mathews (2002) tratam um estrato de erro só. No erro (b) da
  # parcela subdividida o fator da parcela está confundido com a parcela
  # (o "bloco" daquele estrato), então a correção não testa a variância entre
  # os níveis da parcela. Em simulação sob H0 no desenho da aveia (4000
  # réplicas), o Levene comum nos resíduos (b) rejeita 10,5% a 5% (centro na
  # média) e 2,5% (na mediana): sem correção validada, o bloco recusa.
  sp <- tr_models_anova_split_plot(ex("aveia"), "producao", "variedade", "nitrogenio", "bloco")
  for (f in list(tr_models_levene, tr_models_bartlett)) {
    err <- tryCatch(f(sp), condition = identity)
    expect_s3_class(err, "tr_models_error_block_design")
    expect_match(conditionMessage(err), "parcela subdividida", fixed = TRUE)
    expect_match(conditionMessage(err), "models/plot_diagnostics", fixed = TRUE)
  }
  # o Shapiro nos resíduos (b) continua valendo
  expect_s3_class(tr_models_shapiro_residuals(sp), "tr_models_test")
})
