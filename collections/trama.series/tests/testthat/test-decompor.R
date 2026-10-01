test_that("decomposição clássica e STL saem na mesma forma, e somam a série", {
  x <- serie_mensal()
  for (d in list(tr_series_decompose(x), tr_series_stl(x), tr_series_stl(x, 13L, robusta = TRUE))) {
    expect_s3_class(d, "tr_series_decomp")
    soma <- d$tendencia + d$sazonal + d$resto
    ok <- !is.na(soma)
    expect_equal(as.numeric(soma[ok]), as.numeric(x[ok]), tolerance = 1e-8)
  }
})

test_that("multiplicativa multiplica, e dessazonalizada divide", {
  x <- serie_mensal()
  d <- tr_series_decompose(x, "multiplicativa")
  prod <- d$tendencia * d$sazonal * d$resto
  ok <- !is.na(prod)
  expect_equal(as.numeric(prod[ok]), as.numeric(x[ok]), tolerance = 1e-8)
  # O componente leva o nome legível num atributo (a legenda do series/plot);
  # os valores e o tempo são os da conta.
  expect_equal(tr_series_component(d, "dessazonalizada"), x / d$sazonal, ignore_attr = "tr_series_nome")
  expect_equal(tr_series_component(tr_series_stl(x), "dessazonalizada"),
               x - tr_series_stl(x)$sazonal, ignore_attr = "tr_series_nome")
  tend <- tr_series_component(d, "tendencia")
  expect_equal(attr(tend, "tr_series_nome"), "tendência")
  attr(tend, "tr_series_nome") <- NULL
  # Sem as pontas que a média móvel perde: elas não são faltantes, e o bloco
  # seguinte (um KPSS, um ACF) não pode parar por causa delas.
  expect_false(anyNA(tend))
  expect_identical(tend, stats::na.omit(d$tendencia), ignore_attr = "na.action")
})

test_that("decompor recusa série anual, curta, com faltante ou não positiva", {
  expect_error(tr_series_decompose(serie_anual()), class = "tr_series_error_no_season")
  expect_error(tr_series_stl(stats::ts(1:20, frequency = 12)), class = "tr_series_error_too_short")
  expect_error(tr_series_stl(datasets::presidents), class = "tr_series_error_missing_values")
  z <- serie_mensal(); z[[5]] <- 0
  expect_error(tr_series_decompose(z, "multiplicativa"), class = "tr_series_error_nonpositive")
  expect_error(tr_series_stl(serie_mensal(), 5L), class = "tr_series_error_bad_option")
})

test_that("decomposição: tipo, resumo com a força, e adaptador para tabela", {
  ty <- series_decomposition_type()
  d <- tr_series_stl(serie_mensal())
  s <- ty$summary(d)
  expect_equal(s$metodo, "STL")
  expect_gt(s$forca_sazonal, 0.5)
  tab <- .tr_series_decomp_tabela(d)
  expect_equal(names(tab), c("tempo", "observado", "tendencia", "sazonal", "resto"))
  expect_equal(tab$tempo[[1]], as.Date("1949-01-01"))
  expect_error(ty$store(serie_mensal(), tempfile()), class = "tr_series_error_not_a_decomposition")
})

test_that("rótulos de estação seguem a frequência", {
  expect_equal(.tr_series_estacoes(12L)[[1]], "jan")
  expect_equal(.tr_series_estacoes(12L)[[12]], "dez")
  expect_equal(.tr_series_estacoes(4L), c("T1", "T2", "T3", "T4"))
  expect_equal(.tr_series_estacoes(7L)[[7]], "7")
})

# A regressão da série sai como `models/fit`; os componentes vão em
# `$serie_reg`. `reg()` escreve a fórmula que a migração da v1 escreveria para
# `grau`/`sazonalidade` e devolve os componentes junto do ajuste e da matriz.
reg <- function(x, grau = 1L, sazonalidade = TRUE, ...) {
  termos <- c(if (grau == 1L) "t" else if (grau >= 2L) sprintf("poly(t, %d, raw = TRUE)", grau),
              if (sazonalidade) "periodo")
  f <- paste("valor ~", if (length(termos)) paste(termos, collapse = " + ") else "1")
  fit <- tr_series_regression(x, formula = f, ...)
  X <- stats::model.matrix(stats::delete.response(stats::terms(stats::as.formula(fit$formula))),
                           data = fit$dados)
  c(fit$serie_reg, list(ajuste = fit$ajuste, fit = fit, matriz = X))
}
coefs <- function(r) {
  s <- if (inherits(r$ajuste, "gls")) summary(r$ajuste)$tTable else stats::coef(summary(r$ajuste))
  tibble::tibble(termo = rownames(s), estimativa = as.numeric(s[, 1]), erro_padrao = as.numeric(s[, 2]),
                 p_valor = as.numeric(s[, 4]))
}

test_that("regressão soma de volta a série, e o sazonal tem média zero", {
  x <- serie_mensal()
  r <- reg(x)
  expect_s3_class(r$fit, "tr_models_fit")
  expect_equal(as.numeric(r$tendencia + r$sazonal + r$resto), as.numeric(x), tolerance = 1e-8)
  expect_equal(mean(r$sazonal), 0, tolerance = 1e-8)
  expect_equal(stats::frequency(r$tendencia), 12)
  expect_equal(stats::start(r$resto), stats::start(x))
})

test_that("a decomposição não depende do contraste; a tabela de coeficientes sim", {
  x <- serie_mensal()
  a <- reg(x, contraste = "soma_zero")
  b <- reg(x, contraste = "categoria_base")
  expect_equal(as.numeric(a$sazonal), as.numeric(b$sazonal), tolerance = 1e-8)
  expect_equal(as.numeric(a$tendencia), as.numeric(b$tendencia), tolerance = 1e-8)
  expect_false(identical(names(stats::coef(a$ajuste)), names(stats::coef(b$ajuste))))
})

test_that("grau e sazonalidade ligam e desligam os blocos", {
  x <- serie_mensal()
  expect_equal(sum(reg(x, sazonalidade = FALSE)$sazonal), 0)
  expect_length(stats::coef(reg(x, grau = 3L)$ajuste), 1L + 3L + 11L)
  r0 <- reg(x, grau = 0L)
  expect_equal(as.numeric(r0$tendencia), rep(mean(x), length(x)), tolerance = 1e-8)
})

test_that("o sazonal soma zero por ESTAÇÃO, mesmo com anos incompletos", {
  # Setembro a fevereiro: cada mês aparece um número diferente de vezes, e é aí
  # que centrar pela média da amostra deixa de centrar pela do ciclo.
  x <- stats::window(datasets::AirPassengers, start = c(1949, 9), end = c(1952, 2))
  r <- reg(x)
  expect_equal(sum(tapply(r$sazonal, stats::cycle(r$sazonal), mean)), 0, tolerance = 1e-8)
  expect_equal(as.numeric(r$tendencia + r$sazonal + r$resto), as.numeric(x), tolerance = 1e-8)
})

test_that("regressão recusa série sem graus de liberdade para o modelo", {
  # Quatro observações, frequência 2, grau 2: 4 parâmetros para 4 pontos. O
  # ajuste roda, nenhum coeficiente falta, e sai R² ajustado NaN com uma
  # decomposição de aparência perfeita — é o resultado errado com cara de certo.
  expect_error(reg(stats::ts(c(3, 7, 4, 9), frequency = 2, start = c(2000, 1)),
                                    grau = 2L),
               class = "tr_series_error_too_short")
  # Raspando por baixo: 3 parâmetros, 24 observações, e passa.
  expect_s3_class(reg(stats::ts(1:24, frequency = 2), grau = 1L)$fit, "tr_models_fit")
})

test_that("regressão recusa fórmula fora do vocabulário, série anual com periodo, faltante", {
  x <- serie_mensal()
  expect_error(reg(serie_anual()), class = "tr_series_error_no_season")
  expect_error(reg(datasets::presidents), class = "tr_series_error_missing_values")
  expect_error(reg(x, contraste = "meio"), class = "tr_series_error_bad_option")
  expect_error(tr_series_regression(x, formula = "y ~ t"), class = "tr_series_error_bad_option")
  expect_error(tr_series_regression(x, formula = "valor ~ t + renda"), regexp = "renda")
  expect_error(tr_series_regression(x, formula = "valor ~ regressor"), regexp = "entrada")
  expect_error(tr_series_regression(x, formula = "valor ~ t", excluir = "fev"),
               class = "tr_series_error_bad_option")
})

test_that("sem_tendencia: série menos a tendência na aditiva, dividida na multiplicativa", {
  x <- serie_mensal()
  for (d in list(tr_series_decompose(x), tr_series_stl(x),
                 .tr_series_fit_decomp(reg(x)$fit))) {
    st <- tr_series_component(d, "sem_tendencia")
    expect_true(stats::is.ts(st))
    expect_equal(stats::frequency(st), stats::frequency(x))
    expect_false(anyNA(st))
    # Somar a tendência de volta reconstrói a série, sem as pontas que a média
    # móvel da clássica perde.
    expect_equal(as.numeric(st + d$tendencia),
                 as.numeric(stats::window(x, stats::start(st), stats::end(st))))
  }
  # Regressão de grau 1: é a série menos a reta de mínimos quadrados em t,
  # com a sazonalidade dentro.
  r <- .tr_series_fit_decomp(reg(x, grau = 1L)$fit)
  st <- tr_series_component(r, "sem_tendencia")
  expect_equal(as.numeric(st), as.numeric(r$sazonal + r$resto))
  m <- tr_series_decompose(x, "multiplicativa")
  sm <- tr_series_component(m, "sem_tendencia")
  expect_equal(as.numeric(sm * m$tendencia),
               as.numeric(stats::window(x, stats::start(sm), stats::end(sm))))
  # Fator em torno de 1, e não de zero: a divisão, e não a subtração.
  expect_equal(mean(sm, na.rm = TRUE), 1, tolerance = .02)
})

test_that("regressor entra no ajuste com coeficiente e teste, e os componentes somam", {
  set.seed(7)
  x <- serie_mensal()
  # Regressor mais longo que a série (sobra dos dois lados): é recortado.
  z <- stats::ts(stats::rnorm(length(x) + 24), start = c(1948, 1), frequency = 12)
  zj <- as.numeric(stats::window(z, start = stats::start(x), end = stats::end(x)))
  y <- x + 30 * stats::window(z, start = stats::start(x), end = stats::end(x))
  r <- reg(y, grau = 1L, regressor = z)
  tab <- coefs(r)
  expect_true("regressor" %in% tab$termo)
  # Dentro de 3 erros-padrão do verdadeiro: o resto do AirPassengers aditivo é
  # grande (a sazonalidade dele é multiplicativa), e é isso que o SE mede.
  lin <- tab[tab$termo == "regressor", ]
  expect_lt(abs(lin$estimativa - 30), 3 * lin$erro_padrao)
  expect_lt(tab$p_valor[tab$termo == "regressor"], 1e-6)
  expect_equal(as.numeric(r$efeito_regressor), stats::coef(r$ajuste)[["regressor"]] * zj)
  expect_equal(as.numeric(r$tendencia + r$sazonal + r$efeito_regressor + r$resto), as.numeric(y))
  # O F da tendência continua sendo a linha de t, com o regressor no modelo.
  q <- trama.models::tr_models_anova_table(r$fit, tipo_sq = "III")$tabela
  expect_setequal(setdiff(q$termo, "Resíduo"), c("t", "periodo", "regressor"))
  # Grau 0 sem sazonalidade deixa de ser vazio com regressor.
  expect_s3_class(reg(y, grau = 0L, sazonalidade = FALSE, regressor = z)$fit, "tr_models_fit")
  expect_error(reg(x, regressor = stats::window(z, end = c(1955, 12))),
               class = "tr_series_error_no_overlap")
  expect_error(reg(x, regressor = stats::ts(1:200, frequency = 4)),
               class = "tr_series_error_frequency_mismatch")
  zn <- z; zn[30] <- NA
  expect_error(reg(x, regressor = zn), class = "tr_series_error_missing_values")
})

test_that("com regressor, a tendência é só do tempo e o regressor é o quarto componente", {
  set.seed(11)
  x <- serie_mensal()
  z <- stats::ts(stats::rnorm(length(x)), start = stats::start(x), frequency = 12)
  y <- x + 30 * z
  r <- reg(y, grau = 2L, regressor = z)
  # Tendência suave: é exatamente um polinômio de grau 2 em t, sem nada de z.
  tt <- seq_along(y)
  expect_lt(max(abs(stats::residuals(stats::lm(as.numeric(r$tendencia) ~ tt + I(tt^2))))), 1e-8)
  expect_lt(abs(stats::cor(diff(as.numeric(r$tendencia)), diff(as.numeric(z)))), .05)
  d <- .tr_series_fit_decomp(r$fit)
  expect_equal(as.numeric(d$tendencia + d$sazonal + d$regressor + d$resto), as.numeric(y))
  expect_equal(as.numeric(tr_series_component(d, "regressor")),
               stats::coef(r$ajuste)[["regressor"]] * as.numeric(z))
  expect_equal(as.numeric(tr_series_component(d, "sem_tendencia")), as.numeric(y - d$tendencia))
  expect_true("regressor" %in% names(.tr_series_decomp_tabela(d)))
  expect_s3_class(tr_series_plot_decomposition(d), "ggplot")
  expect_error(tr_series_component(tr_series_stl(x), "regressor"),
               class = "tr_series_error_no_component")
  expect_error(tr_series_component(.tr_series_fit_decomp(reg(x)$fit), "regressor"),
               class = "tr_series_error_no_component")
})

test_that("regressor colinear com a tendência é dito como tal", {
  x <- serie_mensal()
  t2 <- stats::ts(2 * seq_along(x) + 5, start = stats::start(x), frequency = 12)
  expect_error(reg(x, grau = 1L, regressor = t2),
               class = "tr_series_error_fit", regexp = "colinear")
})

# ---- Regressão com erro ARMA por GLS (revisão metodológica, fase 1) -----------
# `erro = "arma"` troca o MQO por mínimos quadrados generalizados com erro
# ARMA(p, q), estimado por máxima verossimilhança (`nlme::gls` + `corARMA`).
# Dois oráculos independentes do `nlme`: o `stats::arima` com `xreg` (ML exata
# da regressão com erro ARMA, outro código) e, para AR(1), o MQO sobre os dados
# transformados de Prais-Winsten com o phi estimado — a álgebra do GLS.
test_that("erro AR(1): coeficientes = stats::arima(xreg) por ML", {
  x <- log(datasets::AirPassengers)
  r <- reg(x, grau = 1L, erro = "arma", ar = 1L, ma = 0L)
  expect_s3_class(r$ajuste, "gls")
  X <- r$matriz
  a <- stats::arima(as.numeric(x), order = c(1, 0, 0), xreg = X[, -1], method = "ML",
                    include.mean = TRUE, optim.control = list(maxit = 1000))
  b <- stats::coef(r$ajuste)
  expect_equal(unname(b), unname(stats::coef(a)[-1]), tolerance = 1e-3)
  phi <- unname(stats::coef(r$ajuste$modelStruct$corStruct, unconstrained = FALSE))
  expect_equal(phi, unname(stats::coef(a)[["ar1"]]), tolerance = 1e-3)
  # Log-verossimilhança: as duas maximizam a MESMA função.
  expect_equal(as.numeric(stats::logLik(r$ajuste)), a$loglik, tolerance = 1e-4)
})

test_that("erro AR(1): coeficientes = MQO de Prais-Winsten com o phi do GLS", {
  x <- log(datasets::AirPassengers)
  r <- reg(x, grau = 2L, erro = "arma", ar = 1L)
  phi <- unname(stats::coef(r$ajuste$modelStruct$corStruct, unconstrained = FALSE))
  X <- r$matriz; y <- as.numeric(x); n <- length(y)
  # Prais-Winsten: primeira linha escalada por sqrt(1 - phi²), demais quase-diferenças.
  Xs <- rbind(sqrt(1 - phi^2) * X[1, ], X[-1, ] - phi * X[-n, ])
  ys <- c(sqrt(1 - phi^2) * y[1], y[-1] - phi * y[-n])
  b_pw <- stats::coef(stats::lm.fit(Xs, ys))
  expect_equal(unname(stats::coef(r$ajuste)), unname(b_pw), tolerance = 1e-8)
})

test_that("erro ARMA: a decomposição continua fechando e o sazonal soma zero", {
  x <- serie_mensal()
  r <- reg(x, erro = "arma", ar = 1L, ma = 1L)
  expect_equal(as.numeric(r$tendencia + r$sazonal + r$resto), as.numeric(x), tolerance = 1e-8)
  expect_equal(sum(tapply(as.numeric(r$sazonal), stats::cycle(x), mean)), 0, tolerance = 1e-8)
  expect_equal(r$erro, "arma")
  expect_equal(length(stats::coef(r$ajuste)), 13L)
})

test_that("erro ARMA recusa ordem vazia, e o padrão segue MQO", {
  x <- serie_mensal()
  expect_error(reg(x, erro = "arma", ar = 0L, ma = 0L),
               class = "tr_series_error_bad_option")
  expect_error(reg(x, erro = "outro"), class = "tr_series_error_bad_option")
  expect_s3_class(reg(x)$ajuste, "lm")
})

test_that("GLS: série que o modelo reproduz exato é recusada como singular, não como falta de convergência", {
  e <- tryCatch(reg(stats::ts(rep(1:12, 5), frequency = 12), erro = "arma"),
                condition = identity)
  expect_s3_class(e, "tr_series_error_singular_fit")
  expect_match(conditionMessage(e), "resíduo", fixed = TRUE)
  expect_false(grepl("não convergiu", conditionMessage(e), fixed = TRUE))
})

test_that("GLS: AR perto da raiz unitária avisa (classe) e vai para a nota dos F", {
  set.seed(21)
  x <- stats::ts(as.numeric(stats::arima.sim(list(ar = 0.97), 200)) + 0.01 * (1:200))
  expect_warning(r <- reg(x, grau = 1L, sazonalidade = FALSE, erro = "arma"),
                 class = "tr_series_warn_near_unit_root")
  expect_match(r$aviso, "raiz", fixed = TRUE)
  expect_match(r$fit$nota, "raiz", fixed = TRUE)
  # Longe da raiz, sem aviso.
  set.seed(22)
  y <- stats::ts(as.numeric(stats::arima.sim(list(ar = 0.4), 120)) + 0.05 * (1:120))
  expect_no_warning(r2 <- reg(y, grau = 1L, sazonalidade = FALSE, erro = "arma"))
  expect_null(r2$aviso)
  expect_equal(r2$fit$nota, "")
})

# ---- Regressão por fórmula (trama.series 0.5.0) --------------------------------

test_that("fórmula padrão = lm de valor ~ t + fator do mês com soma zero (oráculo: lm direto)", {
  x <- datasets::AirPassengers
  fit <- tr_series_regression(x)
  expect_s3_class(fit, "tr_models_fit")
  mes <- factor(stats::cycle(x)); stats::contrasts(mes) <- stats::contr.sum
  tt <- seq_along(x)
  o <- stats::lm(as.numeric(x) ~ tt + mes)
  expect_equal(unname(stats::coef(fit$ajuste)), unname(stats::coef(o)), tolerance = 1e-10)
  expect_equal(summary(fit$ajuste)$sigma, summary(o)$sigma, tolerance = 1e-10)
})

test_that("ano, interação e regressor entram pela fórmula", {
  x <- serie_mensal()
  a <- tr_series_regression(x, formula = "valor ~ ano + periodo")
  expect_true("ano" %in% names(stats::coef(a$ajuste)))
  b <- tr_series_regression(x, formula = "valor ~ t * periodo")
  expect_length(stats::coef(b$ajuste), 24L)
  d <- .tr_series_fit_decomp(b)
  expect_equal(as.numeric(d$tendencia + d$sazonal + d$resto), as.numeric(x), tolerance = 1e-8)
  set.seed(3)
  z <- stats::ts(stats::rnorm(length(x)), start = stats::start(x), frequency = 12)
  c1 <- tr_series_regression(x, formula = "valor ~ t", regressor = z)
  expect_match(c1$formula, "regressor", fixed = TRUE)
})

test_that("migração v1 -> v2 escreve a fórmula e dá os mesmos coeficientes do ajuste antigo", {
  reg_col <- trama.series::trama_collection()
  no <- Filter(function(n) n$id == "series/regression", reg_col$nodes)[[1]]
  mig <- no$migracoes[["2"]]
  expect_equal(mig(list())$formula, "valor ~ t + periodo")
  p <- mig(list(grau = 2L, sazonalidade = FALSE, alfa = 0.1, remover_ns = TRUE))
  expect_equal(p$formula, "valor ~ poly(t, 2, raw = TRUE)")
  expect_equal(p$confianca, 0.9)
  expect_null(p$grau); expect_null(p$alfa)
  expect_equal(mig(list(grau = 0L, sazonalidade = FALSE))$formula, "valor ~ 1")
  # Sem sazonalidade, excluir/remover da v1 eram ignorados; a v2 os recusaria.
  q <- mig(list(grau = 1L, sazonalidade = FALSE, excluir = "fev", remover_ns = TRUE))
  expect_s3_class(do.call(tr_series_regression, c(list(serie_mensal()), q)), "tr_models_fit")
  # O ajuste antigo de grau 2 era lm(y ~ t1 + t2 + estacao) com potências cruas.
  x <- serie_mensal()
  fit <- tr_series_regression(x, formula = mig(list(grau = 2L))$formula)
  mes <- factor(stats::cycle(x)); stats::contrasts(mes) <- stats::contr.sum
  tt <- seq_along(x)
  o <- stats::lm(as.numeric(x) ~ tt + I(tt^2) + mes)
  expect_equal(unname(stats::coef(fit$ajuste)), unname(stats::coef(o)), tolerance = 1e-8)
})

test_that("previsão da regressão = forecast::tslm(y ~ trend + season) (oráculo publicado)", {
  x <- datasets::AirPassengers
  fit <- tr_series_regression(x, contraste = "categoria_base")
  fc <- tr_series_forecast(ajuste = fit, horizonte = 18L)
  o <- forecast::forecast(forecast::tslm(x ~ trend + season), h = 18, level = c(80, 95))
  expect_equal(as.numeric(fc$mean), as.numeric(o$mean), tolerance = 1e-8)
  expect_equal(unname(as.matrix(fc$lower)), unname(as.matrix(o$lower)), tolerance = 1e-8)
  expect_equal(unname(as.matrix(fc$upper)), unname(as.matrix(o$upper)), tolerance = 1e-8)
  expect_equal(stats::tsp(fc$mean), stats::tsp(o$mean))
  expect_s3_class(.tr_series_forecast_tabela(fc), "tbl_df")
})

test_that("previsão com erro ARMA = forecast::Arima(xreg) do mesmo desenho", {
  x <- log(datasets::AirPassengers)
  fit <- tr_series_regression(x, erro = "arma", ar = 1L)
  fc <- tr_series_forecast(ajuste = fit, horizonte = 6L)
  tt <- seq_along(x); mes <- factor(stats::cycle(x)); stats::contrasts(mes) <- stats::contr.sum
  X <- stats::model.matrix(~ tt + mes)[, -1]
  tn <- length(x) + 1:6; mn <- factor(((12 + 0:5) %% 12) + 1, levels = 1:12)
  stats::contrasts(mn) <- stats::contr.sum
  Xn <- stats::model.matrix(~ tn + mn)[, -1]
  colnames(Xn) <- colnames(X)
  a <- forecast::Arima(x, order = c(1, 0, 0), xreg = X, method = "ML")
  o <- forecast::forecast(a, xreg = Xn, level = c(80, 95))
  expect_equal(as.numeric(fc$mean), as.numeric(o$mean), tolerance = 1e-6)
  # E os coeficientes do GLS batem com os do Arima: é a mesma verossimilhança.
  expect_equal(unname(stats::coef(fit$ajuste)), unname(stats::coef(a)[-1]), tolerance = 1e-3)
})

test_that("previsão com regressor pede o futuro dele e confere a cobertura", {
  x <- serie_mensal()
  set.seed(5)
  z <- stats::ts(stats::rnorm(length(x) + 6), start = stats::start(x), frequency = 12)
  zj <- stats::window(z, end = stats::end(x))
  y <- x + 10 * zj
  fit <- tr_series_regression(y, regressor = zj)
  expect_error(tr_series_forecast(ajuste = fit, horizonte = 6L), regexp = "futuro")
  expect_error(tr_series_forecast(ajuste = fit, horizonte = 12L, futuro = z), regexp = "Baixe o horizonte para 6")
  fc <- tr_series_forecast(ajuste = fit, horizonte = 6L, futuro = z)
  # Oráculo: o mesmo lm, previsto com o regressor futuro à mão.
  mes <- factor(stats::cycle(y)); stats::contrasts(mes) <- stats::contr.sum
  tt <- seq_along(y); zz <- as.numeric(zj)
  o <- stats::lm(as.numeric(y) ~ tt + mes + zz)
  mn <- factor(((as.integer(stats::cycle(y))[length(y)] + 0:5) %% 12) + 1, levels = 1:12)
  stats::contrasts(mn) <- stats::contr.sum
  attr(mn, "contrasts") <- NULL
  nd <- data.frame(tt = length(y) + 1:6, mes = mn, zz = as.numeric(z)[length(y) + 1:6])
  expect_equal(as.numeric(fc$mean), unname(stats::predict(o, nd)), tolerance = 1e-8)
})

test_that("forecast pede modelo OU ajuste, e o adaptador recusa modelo que não é de série", {
  x <- serie_mensal()
  expect_error(tr_series_forecast(horizonte = 3L), class = "tr_series_error_bad_option")
  fit <- tr_series_regression(x)
  expect_error(tr_series_forecast(ajuste = fit, intervalo = "bootstrap"), class = "tr_series_error_bad_option")
  lm_fit <- trama.models::tr_models_lm(datasets::mtcars, formula = "mpg ~ wt")
  expect_error(.tr_series_fit_decomp(lm_fit), class = "tr_series_error_not_a_regression")
})

test_that("previsão ARMA com base dependente dos dados (poly ortogonal) usa a base do ajuste", {
  y <- stats::window(log(datasets::AirPassengers), end = c(1958, 12))
  a <- tr_series_forecast(ajuste = tr_series_regression(y, "valor ~ poly(t, 2) + periodo", erro = "arma"), horizonte = 6L)
  b <- tr_series_forecast(ajuste = tr_series_regression(y, "valor ~ poly(t, 2, raw = TRUE) + periodo", erro = "arma"), horizonte = 6L)
  # Mesma família de modelos, outra base: a previsão é a mesma.
  expect_equal(as.numeric(a$mean), as.numeric(b$mean), tolerance = 1e-4)
  expect_length(tr_series_forecast(ajuste = tr_series_regression(y, "valor ~ poly(t, 2) + periodo", erro = "arma"),
                                   horizonte = 1L)$mean, 1L)
})

test_that("previsão do detrend com faltante mantém ajustados e resíduos no tempo da série", {
  x <- serie_mensal(); x[5] <- NA
  fc <- tr_series_forecast(ajuste = tr_series_detrend(x, "linear")$ajuste, horizonte = 3L)
  expect_equal(length(fc$fitted), length(x))
  expect_equal(stats::tsp(fc$fitted), stats::tsp(x))
  expect_true(is.na(fc$fitted[5]))
})
