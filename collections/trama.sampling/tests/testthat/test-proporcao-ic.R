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
