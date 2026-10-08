# Causalidade de Granger (R/mv_causalidade.R).
#
# Oráculos e tolerâncias:
# - Wald: F e p-valor de `vars::causality()` no Canada (VAR(2), constante),
#   tolerância 1e-10. É o mesmo cálculo por dentro; o teste pega um desvio de
#   ligação (causa trocada, método errado, graus de liberdade).
# - Toda-Yamamoto: recalculado À MÃO no Canada, com p + d_max = 2 + 1, tolerância
#   1e-8. O oráculo monta as defasagens a partir de Y, ajusta `lm` por equação,
#   monta a covariância cruzada explicitamente (Sigma ⊗ (X'X)^-1, com Sigma =
#   R'R / (n - k)) e a matriz de restrição R à mão. Não usa nada de `vars`.
# - `aod::wald.test` NÃO foi usado: o pacote não está instalado nesta máquina.

canada <- local({
  e <- new.env()
  data("Canada", package = "vars", envir = e)
  e$Canada
})

ajuste_canada <- function(p = 2L) tr_series_var(canada, defasagens = p, deterministico = "constante")

# Wald de Toda-Yamamoto, à mão. Y: matriz T x K; causa: nomes; p: defasagens
# testadas; dmax: integração adicional (o VAR é ajustado com p + dmax lags).
oraculo_ty <- function(Y, causa, p, dmax) {
  K <- ncol(Y); n <- nrow(Y); q <- p + dmax
  nomes <- colnames(Y)
  y <- Y[(q + 1):n, , drop = FALSE]
  lag <- function(j, l) Y[(q + 1 - l):(n - l), j]
  regs <- list(); rn <- character()
  for (l in seq_len(q)) for (j in seq_len(K)) {
    regs[[length(regs) + 1]] <- lag(j, l)
    rn <- c(rn, paste0(nomes[j], ".l", l))
  }
  regs[[length(regs) + 1]] <- rep(1, nrow(y)); rn <- c(rn, "const")
  dd <- data.frame(do.call(cbind, regs), check.names = FALSE)
  names(dd) <- rn
  fits <- lapply(seq_len(K), function(i) {
    lm(y ~ 0 + ., data = cbind(data.frame(y = y[, i]), dd))
  })
  k <- length(rn)
  X <- as.matrix(dd)
  E <- sapply(fits, residuals)
  Sig <- crossprod(E) / (nrow(y) - k)
  XtXi <- chol2inv(qr.R(qr(X)))  # QR: crossprod + solve perde precisão com defasagens colineares
  # Conferência interna: a diagonal bate com o vcov do lm.
  for (i in seq_len(K)) {
    stopifnot(isTRUE(all.equal(unname(stats::vcov(fits[[i]])), unname(Sig[i, i] * XtXi), tolerance = 1e-10)))
  }
  V <- matrix(0, K * k, K * k)
  for (i in seq_len(K)) for (j in seq_len(K)) {
    V[(i - 1) * k + seq_len(k), (j - 1) * k + seq_len(k)] <- Sig[i, j] * XtXi
  }
  b <- unlist(lapply(fits, stats::coef), use.names = FALSE)
  # Matriz de restrição: lags 1..p de cada causa, na equação de cada outra série.
  outras <- setdiff(nomes, causa)
  filas <- list()
  for (eq in outras) for (c_ in causa) for (l in seq_len(p)) {
    r <- numeric(K * k)
    r[(match(eq, nomes) - 1) * k + match(paste0(c_, ".l", l), rn)] <- 1
    filas[[length(filas) + 1]] <- r
  }
  R <- do.call(rbind, filas)
  Rb <- R %*% b
  W <- drop(t(Rb) %*% solve(R %*% V %*% t(R)) %*% Rb)
  gl <- nrow(R)
  list(W = W, gl = gl, p = stats::pchisq(W, df = gl, lower.tail = FALSE))
}

test_that("Wald: F e p-valor batem com vars::causality no Canada (1e-10)", {
  aj <- ajuste_canada()
  for (causa in list("e", c("e", "prod"), "rw")) {
    ref <- vars::causality(aj$ajuste, cause = causa)$Granger
    out <- tr_series_granger(aj, causa = paste(causa, collapse = ", "), metodo = "wald")
    expect_equal(out$estatistica, unname(ref$statistic[[1]]), tolerance = 1e-10, info = paste(causa, collapse = ","))
    expect_equal(out$p_valor, unname(ref$p.value[[1]]), tolerance = 1e-10, info = paste(causa, collapse = ","))
  }
})

test_that("Wald e Toda-Yamamoto saem como data/test com a H0 do bloco", {
  aj <- ajuste_canada()
  out <- tr_series_granger(aj, causa = "e")
  expect_s3_class(out, "tr_series_test")
  expect_equal(out$h0, "e não Granger-causa prod e rw e U")
  expect_equal(out$rotulo_estat, "F")
  out_ty <- tr_series_granger(aj, causa = "e", metodo = "toda_yamamoto")
  expect_equal(out_ty$rotulo_estat, "qui-quadrado")
})

test_that("Toda-Yamamoto bate com o cálculo à mão no Canada (1e-8)", {
  aj <- ajuste_canada(p = 2L)
  dmax <- .tr_series_dmax(canada)
  # O Canada é I(1) nas séries (ADF): d_max = 1, e o teste recalculado usa o mesmo.
  expect_equal(dmax, 1L)
  for (causa in list("e", c("e", "prod"))) {
    ref <- oraculo_ty(as.matrix(canada), causa, p = 2L, dmax = dmax)
    got <- .tr_series_granger_ty(aj, causa, dmax)
    expect_equal(got$estatistica, ref$W, tolerance = 1e-8, info = paste(causa, collapse = ","))
    expect_equal(got$gl, ref$gl, info = paste(causa, collapse = ","))
    expect_equal(got$p_valor, ref$p, tolerance = 1e-8, info = paste(causa, collapse = ","))
  }
})


test_that("causa inexistente, causa com todas as séries e VECM são recusados com classe", {
  aj <- ajuste_canada()
  expect_error(tr_series_granger(aj, causa = "xx"), class = "tr_series_error_bad_option")
  expect_error(tr_series_granger(aj, causa = "e, prod, rw, U"), class = "tr_series_error_granger_sem_resto")
  expect_error(tr_series_granger(aj, causa = ""), class = "tr_series_error_blank_param")
  expect_error(tr_series_granger(aj, causa = "e", metodo = "xx"), class = "tr_series_error_bad_option")
  expect_error(tr_series_granger(canada, causa = "e"), class = "tr_series_error_not_var")
  v2v <- vars::vec2var(urca::ca.jo(as.matrix(canada), type = "trace", ecdet = "const", K = 2,
                                   spec = "longrun"), r = 1)
  fake <- .tr_series_var(v2v, canada, "VECM", vecm = list(posto = 1L))
  expect_error(tr_series_granger(fake, causa = "e"), class = "tr_series_error_vecm_granger")
})
