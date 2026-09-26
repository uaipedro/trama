# O intervalo da proporção. Padrão: logit (Korn & Graubard 1999;
# padrão de `survey::svyciprop`), com t nos gl do desenho. Oráculo: o próprio
# `survey::svyciprop(method = "logit")` no MESMO desenho, montado a partir do
# que a amostra carrega (estrato, UPA, correção finita, peso).

desenho_survey <- function(a, nivel = "sim") {
  d <- a$dados
  d$.ind <- as.numeric(d$irrigada == nivel)
  d$.h <- a$desenho$estrato; d$.c <- a$desenho$psu
  fpc <- a$desenho$fpc
  d$.N <- as.numeric(fpc[d$.h])
  survey::svydesign(ids = ~.c, strata = ~.h, fpc = ~.N, weights = ~peso_amostral, data = d, nest = TRUE)
}

conferir_logit <- function(a, nivel = "sim", por = NULL, dominio = NULL) {
  des <- desenho_survey(a, nivel)
  gl <- survey::degf(des)
  est <- tr_sampling_proportion(a, "irrigada", nivel, por = if (is.null(por)) "" else por)$tabela
  if (!is.null(dominio)) {
    est <- est[est[[por]] == dominio, ]
    des <- subset(des, des$variables[[por]] == dominio)
  }
  # df = gl do desenho INTEIRO: o domínio não corta o desenho, e o trama conta
  # todas as UPAs (o `svyciprop` contaria só as do domínio sem o `df`).
  o <- survey::svyciprop(~.ind, des, method = "logit", df = gl)
  ci <- attr(o, "ci")
  expect_equal(est$estimativa, as.numeric(o), tolerance = 1e-8)
  expect_equal(est$li, ci[[1]], tolerance = 1e-6)
  expect_equal(est$ls, ci[[2]], tolerance = 1e-6)
  expect_equal(est$gl, gl)
}

test_that("logit é o padrão e reproduz survey::svyciprop na AAS", {
  skip_if_not_installed("survey")
  f <- ex("fazendas")
  a <- tr_sampling_srs(f, n = 200L, .seed = 11L)
  conferir_logit(a)
  # Valores de 2026-09-25 (survey 4.x): 0,350 [0,289124890889522; 0,416188372013590].
  e <- tr_sampling_proportion(a, "irrigada", "sim")$tabela
  expect_equal(c(e$li, e$ls), c(0.289124890889522, 0.416188372013590), tolerance = 1e-9)
})

test_that("logit reproduz svyciprop na estratificada, em conglomerados e num domínio", {
  skip_if_not_installed("survey")
  f <- ex("fazendas")
  conferir_logit(tr_sampling_stratified(f, "regiao", n = 200L, .seed = 3L))
  conferir_logit(tr_sampling_cluster(f, "municipio", conglomerados = 12L, .seed = 3L))
  a <- tr_sampling_srs(f, n = 300L, .seed = 2L)
  conferir_logit(a, por = "regiao", dominio = "Norte")
})

test_that("logit fica dentro de (0, 1) mesmo com proporção rara, e o Wald não", {
  f <- ex("fazendas")
  a <- tr_sampling_srs(f, n = 40L, .seed = 1L)
  a$dados$irrigada <- c("sim", rep("não", 39))
  lg <- tr_sampling_proportion(a, "irrigada", "sim")$tabela
  wd <- tr_sampling_proportion(a, "irrigada", "sim", intervalo = "wald")$tabela
  expect_gt(lg$li, 0); expect_lt(lg$ls, 1)
  expect_lt(wd$li, 0)
  # Assimétrico: a margem publicada é a maior das duas metades.
  expect_equal(lg$margem, max(lg$estimativa - lg$li, lg$ls - lg$estimativa))
})

test_that("wald é o intervalo anterior (estimativa ± t·EP), para quem o pedir", {
  f <- ex("fazendas")
  a <- tr_sampling_srs(f, n = 200L, .seed = 11L)
  w <- tr_sampling_proportion(a, "irrigada", "sim", intervalo = "wald")$tabela
  a2 <- a; a2$dados$ind <- as.numeric(a$dados$irrigada == "sim")
  m <- tr_sampling_mean(a2, "ind")$tabela
  expect_equal(w$li, m$li); expect_equal(w$ls, m$ls); expect_equal(w$erro_padrao, m$erro_padrao)
})

test_that("wilson com n efetivo: na AAS sem correção finita é o escore de Wilson (prop.test) com n − 1", {
  # Na AAS sem fpc, v(p̂) = p̂(1 − p̂)/(n − 1), logo n_ef = p̂(1 − p̂)/v = n − 1
  # (Kish 1965). Com x_ef = p̂·n_ef, o intervalo é o de
  # Wilson (1927) — `stats::prop.test(correct = FALSE)` —, trocado z por t_gl.
  f <- ex("fazendas")
  d <- f[seq(1, 2400, by = 16), ]
  d$w <- 1
  a <- tr_sampling_design(d, pesos = "w")
  w <- tr_sampling_proportion(a, "irrigada", "sim", intervalo = "wilson")$tabela
  p <- mean(d$irrigada == "sim"); nef <- 149
  expect_equal(w$estimativa, p)
  q <- stats::qt(0.975, 149)
  cen <- (p + q^2 / (2 * nef)) / (1 + q^2 / nef)
  mei <- q / (1 + q^2 / nef) * sqrt(p * (1 - p) / nef + q^2 / (4 * nef^2))
  expect_equal(c(w$li, w$ls), c(cen - mei, cen + mei), tolerance = 1e-12)
  # O mesmo escore com z é o do prop.test, que confere a fórmula.
  wz <- .tr_sampling_wilson(p, nef, stats::qnorm(0.975))
  pt <- suppressWarnings(stats::prop.test(p * nef, nef, correct = FALSE))$conf.int
  expect_equal(unname(wz), as.numeric(pt), tolerance = 1e-10)
})

test_that("intervalo desconhecido é recusado", {
  a <- tr_sampling_srs(ex("fazendas"), n = 50L, .seed = 1L)
  expect_error(tr_sampling_proportion(a, "irrigada", "sim", intervalo = "clopper"),
               class = "tr_sampling_error_bad_option")
})

# Clopper-Pearson com n efetivo (Korn & Graubard 1998, Survey Methodology
# 24(2), 193-201). Oráculo: `survey::svyciprop(method = "beta")`, que é essa
# construção; nos extremos (onde o survey dá NaN), o Clopper-Pearson exato de
# `stats::binom.test` com o n nominal.

test_that("clopper_pearson reproduz svyciprop(method = 'beta') na AAS e em conglomerados", {
  skip_if_not_installed("survey")
  f <- ex("fazendas")
  for (a in list(tr_sampling_srs(f, n = 200L, .seed = 11L),
                 tr_sampling_cluster(f, "municipio", conglomerados = 12L, .seed = 3L),
                 tr_sampling_stratified(f, "regiao", n = 200L, .seed = 3L))) {
    o <- survey::svyciprop(~.ind, desenho_survey(a), method = "beta")
    e <- tr_sampling_proportion(a, "irrigada", "sim", intervalo = "clopper_pearson")$tabela
    expect_equal(c(e$li, e$ls), as.numeric(attr(o, "ci")), tolerance = 1e-6)
  }
  # Valores de 2026-09-25 (survey 4.x), AAS n = 200, semente 11.
  e <- tr_sampling_proportion(tr_sampling_srs(f, n = 200L, .seed = 11L), "irrigada", "sim",
                              intervalo = "clopper_pearson")$tabela
  o <- survey::svyciprop(~.ind, desenho_survey(tr_sampling_srs(f, n = 200L, .seed = 11L)), method = "beta")
  expect_equal(c(e$li, e$ls), as.numeric(attr(o, "ci")), tolerance = 1e-9)
})

test_that("p̂ = 0 ou 1 não degenera no ponto: Clopper-Pearson exato na AAS sem correção finita", {
  f <- ex("fazendas")
  d <- f[seq(1, 2400, by = 16), ]; d$w <- 1
  n <- nrow(d)
  d$irrigada <- "não"
  a <- tr_sampling_design(d, pesos = "w")
  bt <- as.numeric(stats::binom.test(n, n)$conf.int)  # [qbeta(0,025; n, 1), 1]
  for (iv in c("logit", "wilson", "clopper_pearson")) {
    e <- tr_sampling_proportion(a, "irrigada", "não", intervalo = iv)$tabela
    expect_equal(e$estimativa, 1)
    expect_equal(c(e$li, e$ls), bt, tolerance = 1e-10)
  }
  # Proporção 0 de uma categoria ausente não é pedida (nível inexistente); via
  # indicador: 1 − "não" = 0 ⇒ [0, 1 − (α/2)^(1/n)].
  lim <- .tr_sampling_korn_graubard(0, 0, n, n - 1, 0.95)
  expect_equal(lim, as.numeric(stats::binom.test(0, n)$conf.int), tolerance = 1e-10)
  expect_equal(lim[[2]], 1 - 0.025^(1 / n), tolerance = 1e-10)
})

test_that("nos extremos em conglomerados, n nominal ajustado pelos gl do desenho", {
  f <- ex("fazendas")
  a <- tr_sampling_cluster(f, "municipio", conglomerados = 12L, .seed = 3L)
  a$dados$irrigada <- "sim"
  e <- tr_sampling_proportion(a, "irrigada", "sim")$tabela
  n <- nrow(a$dados); gl <- e$gl
  nef <- n * (stats::qt(0.025, n - 1) / stats::qt(0.025, gl))^2
  expect_equal(e$ls, 1)
  expect_equal(e$li, stats::qbeta(0.025, nef, 1), tolerance = 1e-12)
  expect_lt(e$li, 1)
  # Wald continua o intervalo literal (degenera), como documentado.
  w <- tr_sampling_proportion(a, "irrigada", "sim", intervalo = "wald")$tabela
  expect_equal(c(w$li, w$ls), c(1, 1))
})

test_that("estimativa NA (domínio só com faltantes) não quebra o intervalo", {
  t <- tibble::tibble(estimativa = NA_real_, erro_padrao = NA_real_, li = NA_real_, ls = NA_real_,
                      margem = NA_real_, n = 0L, gl = 10)
  for (iv in c("logit", "wilson", "clopper_pearson")) {
    r <- .tr_sampling_ic_proporcao(t, iv, 0.95)
    expect_true(is.na(r$li)); expect_true(is.na(r$ls))
  }
})
