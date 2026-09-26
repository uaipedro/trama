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
  expect_identical(tend, d$tendencia)
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

test_that("regressão soma de volta a série, e o sazonal tem média zero", {
  x <- serie_mensal()
  r <- tr_series_regression(x)
  expect_s3_class(r, "tr_series_reg")
  expect_equal(as.numeric(r$tendencia + r$sazonal + r$resto), as.numeric(x), tolerance = 1e-8)
  expect_equal(mean(r$sazonal), 0, tolerance = 1e-8)
  expect_equal(stats::frequency(r$tendencia), 12)
  expect_equal(stats::start(r$resto), stats::start(x))
})

test_that("a decomposição não depende do contraste; a tabela de coeficientes sim", {
  x <- serie_mensal()
  a <- tr_series_regression(x, contraste = "soma_zero")
  b <- tr_series_regression(x, contraste = "categoria_base")
  expect_equal(as.numeric(a$sazonal), as.numeric(b$sazonal), tolerance = 1e-8)
  expect_equal(as.numeric(a$tendencia), as.numeric(b$tendencia), tolerance = 1e-8)
  expect_false(identical(names(stats::coef(a$ajuste)), names(stats::coef(b$ajuste))))
})

test_that("grau e sazonalidade ligam e desligam os blocos", {
  x <- serie_mensal()
  expect_equal(sum(tr_series_regression(x, sazonalidade = FALSE)$sazonal), 0)
  expect_length(stats::coef(tr_series_regression(x, grau = 3L)$ajuste), 1L + 3L + 11L)
  r0 <- tr_series_regression(x, grau = 0L)
  expect_equal(as.numeric(r0$tendencia), rep(mean(x), length(x)), tolerance = 1e-8)
})

test_that("o sazonal soma zero por ESTAÇÃO, mesmo com anos incompletos", {
  # Setembro a fevereiro: cada mês aparece um número diferente de vezes, e é aí
  # que centrar pela média da amostra deixa de centrar pela do ciclo.
  x <- stats::window(datasets::AirPassengers, start = c(1949, 9), end = c(1952, 2))
  r <- tr_series_regression(x)
  expect_equal(sum(tapply(r$sazonal, stats::cycle(r$sazonal), mean)), 0, tolerance = 1e-8)
  expect_equal(as.numeric(r$tendencia + r$sazonal + r$resto), as.numeric(x), tolerance = 1e-8)
})

test_that("regressão recusa série sem graus de liberdade para o modelo", {
  # Quatro observações, frequência 2, grau 2: 4 parâmetros para 4 pontos. O
  # ajuste roda, nenhum coeficiente falta, e sai R² ajustado NaN com uma
  # decomposição de aparência perfeita — é o resultado errado com cara de certo.
  expect_error(tr_series_regression(stats::ts(c(3, 7, 4, 9), frequency = 2, start = c(2000, 1)),
                                    grau = 2L),
               class = "tr_series_error_too_short")
  # Raspando por baixo: 3 parâmetros, 24 observações, e passa.
  expect_s3_class(tr_series_regression(stats::ts(1:24, frequency = 2), grau = 1L),
                  "tr_series_reg")
})

test_that("regressão recusa modelo vazio, série anual com sazonal, faltante", {
  x <- serie_mensal()
  expect_error(tr_series_regression(x, grau = 0L, sazonalidade = FALSE),
               class = "tr_series_error_empty_model")
  expect_error(tr_series_regression(serie_anual()), class = "tr_series_error_no_season")
  expect_error(tr_series_regression(datasets::presidents), class = "tr_series_error_missing_values")
  expect_error(tr_series_regression(x, grau = 4L), class = "tr_series_error_bad_option")
  expect_error(tr_series_regression(x, contraste = "meio"), class = "tr_series_error_bad_option")
})

test_that("sem_tendencia: série menos a tendência na aditiva, dividida na multiplicativa", {
  x <- serie_mensal()
  for (d in list(tr_series_decompose(x), tr_series_stl(x),
                 .tr_series_reg_decomp(tr_series_regression(x)))) {
    st <- tr_series_component(d, "sem_tendencia")
    expect_true(stats::is.ts(st))
    expect_equal(stats::tsp(st), stats::tsp(x))
    # Somar a tendência de volta reconstrói a série (NA das pontas da clássica
    # ficam NA dos dois lados).
    expect_equal(as.numeric(st + d$tendencia), as.numeric(ifelse(is.na(d$tendencia), NA, x)))
  }
  # Regressão de grau 1: é a série menos a reta de mínimos quadrados em t,
  # com a sazonalidade dentro.
  r <- .tr_series_reg_decomp(tr_series_regression(x, grau = 1L))
  st <- tr_series_component(r, "sem_tendencia")
  expect_equal(as.numeric(st), as.numeric(r$sazonal + r$resto))
  m <- tr_series_decompose(x, "multiplicativa")
  sm <- tr_series_component(m, "sem_tendencia")
  expect_equal(as.numeric(sm * m$tendencia), as.numeric(ifelse(is.na(m$tendencia), NA, x)))
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
  r <- tr_series_regression(y, grau = 1L, regressor = z)
  tab <- .tr_series_reg_tabela(r)
  expect_true("regressor" %in% tab$termo)
  # Dentro de 3 erros-padrão do verdadeiro: o resto do AirPassengers aditivo é
  # grande (a sazonalidade dele é multiplicativa), e é isso que o SE mede.
  lin <- tab[tab$termo == "regressor", ]
  expect_lt(abs(lin$estimativa - 30), 3 * lin$erro_padrao)
  expect_lt(tab$p_valor[tab$termo == "regressor"], 1e-6)
  expect_equal(as.numeric(r$efeito_regressor), stats::coef(r$ajuste)[["regressor"]] * zj)
  expect_equal(as.numeric(r$tendencia + r$sazonal + r$efeito_regressor + r$resto), as.numeric(y))
  # O F de tendência continua sendo o do bloco t, com o regressor no modelo.
  expect_s3_class(tr_series_f_tendencia(r), "tr_series_test")
  # Grau 0 sem sazonalidade deixa de ser vazio com regressor.
  expect_s3_class(tr_series_regression(y, grau = 0L, sazonalidade = FALSE, regressor = z),
                  "tr_series_reg")
  expect_error(tr_series_regression(x, regressor = stats::window(z, end = c(1955, 12))),
               class = "tr_series_error_no_overlap")
  expect_error(tr_series_regression(x, regressor = stats::ts(1:200, frequency = 4)),
               class = "tr_series_error_frequency_mismatch")
  zn <- z; zn[30] <- NA
  expect_error(tr_series_regression(x, regressor = zn), class = "tr_series_error_missing_values")
})

test_that("com regressor, a tendência é só do tempo e o regressor é o quarto componente", {
  set.seed(11)
  x <- serie_mensal()
  z <- stats::ts(stats::rnorm(length(x)), start = stats::start(x), frequency = 12)
  y <- x + 30 * z
  r <- tr_series_regression(y, grau = 2L, regressor = z)
  # Tendência suave: é exatamente um polinômio de grau 2 em t, sem nada de z.
  tt <- seq_along(y)
  expect_lt(max(abs(stats::residuals(stats::lm(as.numeric(r$tendencia) ~ tt + I(tt^2))))), 1e-8)
  expect_lt(abs(stats::cor(diff(as.numeric(r$tendencia)), diff(as.numeric(z)))), .05)
  d <- .tr_series_reg_decomp(r)
  expect_equal(as.numeric(d$tendencia + d$sazonal + d$regressor + d$resto), as.numeric(y))
  expect_equal(as.numeric(tr_series_component(d, "regressor")),
               stats::coef(r$ajuste)[["regressor"]] * as.numeric(z))
  expect_equal(as.numeric(tr_series_component(d, "sem_tendencia")), as.numeric(y - d$tendencia))
  expect_true("regressor" %in% names(.tr_series_decomp_tabela(d)))
  expect_s3_class(tr_series_plot_decomposition(d), "ggplot")
  expect_error(tr_series_component(tr_series_stl(x), "regressor"),
               class = "tr_series_error_no_component")
  expect_error(tr_series_component(.tr_series_reg_decomp(tr_series_regression(x)), "regressor"),
               class = "tr_series_error_no_component")
})

test_that("regressor colinear com a tendência é dito como tal", {
  x <- serie_mensal()
  t2 <- stats::ts(2 * seq_along(x) + 5, start = stats::start(x), frequency = 12)
  expect_error(tr_series_regression(x, grau = 1L, regressor = t2),
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
  r <- tr_series_regression(x, grau = 1L, erro = "arma", ar = 1L, ma = 0L)
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
  r <- tr_series_regression(x, grau = 2L, erro = "arma", ar = 1L)
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
  r <- tr_series_regression(x, erro = "arma", ar = 1L, ma = 1L)
  expect_equal(as.numeric(r$tendencia + r$sazonal + r$resto), as.numeric(x), tolerance = 1e-8)
  expect_equal(sum(tapply(as.numeric(r$sazonal), stats::cycle(x), mean)), 0, tolerance = 1e-8)
  expect_equal(r$erro, "arma")
  tab <- .tr_series_reg_tabela(r)
  expect_equal(nrow(tab), 13L)
  expect_equal(tab$estimativa, unname(stats::coef(r$ajuste)))
})

test_that("erro ARMA recusa ordem vazia, e o padrão segue MQO", {
  x <- serie_mensal()
  expect_error(tr_series_regression(x, erro = "arma", ar = 0L, ma = 0L),
               class = "tr_series_error_bad_option")
  expect_error(tr_series_regression(x, erro = "outro"), class = "tr_series_error_bad_option")
  expect_s3_class(tr_series_regression(x)$ajuste, "lm")
})

test_that("GLS: série que o modelo reproduz exato é recusada como singular, não como falta de convergência", {
  e <- tryCatch(tr_series_regression(stats::ts(rep(1:12, 5), frequency = 12), erro = "arma"),
                condition = identity)
  expect_s3_class(e, "tr_series_error_singular_fit")
  expect_match(conditionMessage(e), "resíduo", fixed = TRUE)
  expect_false(grepl("não convergiu", conditionMessage(e), fixed = TRUE))
})

test_that("GLS: AR perto da raiz unitária avisa (classe) e vai para a nota dos F", {
  set.seed(21)
  x <- stats::ts(as.numeric(stats::arima.sim(list(ar = 0.97), 200)) + 0.01 * (1:200))
  expect_warning(r <- tr_series_regression(x, grau = 1L, sazonalidade = FALSE, erro = "arma"),
                 class = "tr_series_warn_near_unit_root")
  expect_match(r$aviso, "raiz", fixed = TRUE)
  expect_match(tr_series_f_tendencia(r)$nota, "raiz", fixed = TRUE)
  # Longe da raiz, sem aviso.
  set.seed(22)
  y <- stats::ts(as.numeric(stats::arima.sim(list(ar = 0.4), 120)) + 0.05 * (1:120))
  expect_no_warning(r2 <- tr_series_regression(y, grau = 1L, sazonalidade = FALSE, erro = "arma"))
  expect_null(r2$aviso)
  expect_false(grepl("raiz", tr_series_f_tendencia(r2)$nota, fixed = TRUE))
})
