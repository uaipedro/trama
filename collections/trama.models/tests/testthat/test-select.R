# models/select: AIC/BIC contra stats; AICc e pesos contra a fórmula fechada de
# Hurvich e Tsai (1989) / Burnham e Anderson (2002, cap. 2), escrita à mão aqui.
# (Oráculo por pacote, MuMIn::AICc/Weights, entra quando o MuMIn estiver
# instalado no ambiente de teste.)

mt <- datasets::mtcars
fs <- c("mpg ~ wt", "mpg ~ wt + hp", "mpg ~ wt * hp", "mpg ~ 1")
ajustes <- function() lapply(fs, function(f) tr_models_lm(mt, formula = f))

mao <- function(f) {
  m <- stats::lm(stats::as.formula(f), mt); ll <- stats::logLik(m)
  k <- attr(ll, "df"); n <- nrow(mt)
  c(k = k, aic = stats::AIC(m), bic = stats::BIC(m), aicc = stats::AIC(m) + 2 * k * (k + 1) / (n - k - 1))
}

test_that("AICc, delta e pesos de Akaike batem com a fórmula fechada", {
  r <- tr_models_select(ajustes())
  ref <- t(vapply(fs, mao, numeric(4)))
  i <- match(r$formula, fs)
  expect_equal(r$AICc, unname(ref[i, "aicc"]), tolerance = 1e-10)
  expect_equal(r$k, unname(ref[i, "k"]))
  d <- ref[, "aicc"] - min(ref[, "aicc"]); w <- exp(-d / 2) / sum(exp(-d / 2))
  expect_equal(r$delta, unname(d[i]), tolerance = 1e-10)
  expect_equal(r$peso, unname(w[i]), tolerance = 1e-10)
  expect_equal(sum(r$peso), 1, tolerance = 1e-12)
  expect_false(is.unsorted(r$AICc))
  expect_equal(r$peso_acumulado[[nrow(r)]], 1, tolerance = 1e-12)
  expect_equal(r$razao_evidencia[[1]], 1)
  expect_equal(r$razao_evidencia, r$peso[[1]] / r$peso, tolerance = 1e-12)
})

test_that("AIC e BIC vêm de stats::AIC / stats::BIC", {
  ref <- t(vapply(fs, mao, numeric(4)))
  a <- tr_models_select(ajustes(), "AIC"); b <- tr_models_select(ajustes(), "BIC")
  expect_equal(a$AIC, unname(ref[match(a$formula, fs), "aic"]), tolerance = 1e-10)
  expect_equal(b$BIC, unname(ref[match(b$formula, fs), "bic"]), tolerance = 1e-10)
})

test_that("mistos são reajustados por ML; glm e lm entram juntos", {
  skip_if_not_installed("lme4")
  d <- ex("milho_dbc")
  m1 <- tr_models_lmer(d, formula = "producao ~ hibrido + (1 | bloco)")
  m2 <- tr_models_lmer(d, formula = "producao ~ 1 + (1 | bloco)")
  r <- tr_models_select(list(m1, m2), "AIC")
  ref <- vapply(list(m1, m2), function(m) stats::AIC(lme4::refitML(m$ajuste)), numeric(1))
  expect_equal(sort(r$AIC), sort(ref), tolerance = 1e-8)
  g <- tr_models_glm(mt, formula = "mpg ~ wt", familia = "gaussiana")
  l <- tr_models_lm(mt, formula = "mpg ~ wt")
  expect_equal(tr_models_select(list(g, l))$AICc[[1]], tr_models_select(list(g, l))$AICc[[2]], tolerance = 1e-8)
})

test_that("recusas: um só modelo, respostas ou linhas diferentes, classe sem logLik, AICc inexistente", {
  m <- tr_models_lm(mt, formula = "mpg ~ wt")
  expect_error(tr_models_select(list(m)), "dois ou mais", class = "tr_models_error_not_nested")
  o <- tr_models_lm(mt, formula = "log(mpg) ~ wt")
  expect_error(tr_models_select(list(m, o)), "respostas diferentes", class = "tr_models_error_not_nested")
  d2 <- mt; d2$wt[1] <- NA
  expect_error(tr_models_select(list(m, tr_models_lm(d2, formula = "mpg ~ wt"))), "linhas", class = "tr_models_error_not_nested")
  expect_error(tr_models_select(list(m, m), "R2"), class = "tr_models_error_bad_option")
  # n = 5 e 4 parâmetros: n - k - 1 = 0, sem AICc.
  peq <- mt[1:5, ]
  a <- tr_models_lm(peq, formula = "mpg ~ wt + hp + disp")
  b <- tr_models_lm(peq, formula = "mpg ~ wt")
  r <- tr_models_select(list(a, b))
  expect_true(is.na(r$AICc[r$formula == "mpg ~ wt + hp + disp"]))
  expect_equal(r$formula[[1]], "mpg ~ wt")
})

test_that("pelo motor: a porta variádica entrega os três modelos", {
  reg <- models_registry()
  fl <- trama::tr_flow(reg) |>
    trama::tr_add("carros", "models/example", dataset = "mtcars") |>
    trama::tr_add("m1", "models/lm", formula = "mpg ~ wt", from = "carros") |>
    trama::tr_add("m2", "models/lm", formula = "mpg ~ wt + hp", from = "carros") |>
    trama::tr_add("r", "models/select", from = c("m1", "m2"))
  out <- rodar(fl, "r")
  expect_equal(nrow(out), 2L)
})

test_that("quasi, mistura discreta x contínua e AICc inexistente em todos são recusados com classe", {
  q <- tr_models_glm(mt, formula = "carb ~ wt", familia = "quasipoisson")
  p <- tr_models_glm(mt, formula = "carb ~ wt", familia = "poisson")
  expect_error(tr_models_select(list(q, p)), "quasi", class = "tr_models_error_not_applicable")
  b <- tr_models_glm(mt, formula = "am ~ wt", familia = "binomial")
  g <- tr_models_glm(mt, formula = "am ~ wt", familia = "gaussiana")
  expect_error(tr_models_select(list(b, g)), "discreta", class = "tr_models_error_not_nested")
  peq <- mt[1:4, ]
  a <- tr_models_lm(peq, formula = "mpg ~ wt + hp"); c <- tr_models_lm(peq, formula = "mpg ~ wt + disp")
  expect_error(tr_models_select(list(a, c)), "nenhum modelo", class = "tr_models_error_not_applicable")
  expect_no_error(tr_models_select(list(a, c), "AIC"))
})

test_that("modelo solto vira lista; gaussiana x gama (ambas contínuas) passam; razão = exp(delta/2)", {
  m <- tr_models_lm(mt, formula = "mpg ~ wt")
  expect_error(tr_models_select(m), "dois ou mais", class = "tr_models_error_not_nested")
  ga <- tr_models_glm(mt, formula = "mpg ~ wt", familia = "gaussiana")
  gm <- tr_models_glm(mt, formula = "mpg ~ wt", familia = "gama")
  r <- tr_models_select(list(ga, gm))
  expect_equal(r$razao_evidencia, exp(r$delta / 2), tolerance = 1e-12)
})

test_that("oráculo publicado: MuMIn::AICc e Weights (quando instalado)", {
  skip_if_not_installed("MuMIn")
  ms <- lapply(fs, function(f) stats::lm(stats::as.formula(f), mt))
  r <- tr_models_select(ajustes())
  ref <- vapply(ms, MuMIn::AICc, numeric(1))
  expect_equal(r$AICc, unname(ref[match(r$formula, fs)]), tolerance = 1e-10)
  expect_equal(r$peso, unname(MuMIn::Weights(ref)[match(r$formula, fs)]), tolerance = 1e-10)
})
