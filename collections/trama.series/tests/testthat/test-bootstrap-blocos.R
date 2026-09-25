# Bootstrap de blocos móveis para Mann-Kendall, Cox-Stuart e Pettitt sob
# dependência serial (Kundzewicz & Robson 2004, Hydrol. Sci. J. 49(1):7-19,
# doi:10.1623/hysj.49.1.7.53993; Künsch 1989). Oráculo da MECÂNICA: o mesmo
# bootstrap escrito à mão, com a mesma semente, igual EXATAMENTE (blocos de
# round(sqrt(n)), inícios sorteados com reposição em 1..n − l + 1, emendados e
# cortados em n; 1999 reamostras; p = (1 + #{|T*| >= |T|}) / (B + 1)).

boot_mao <- function(x, estat, seed, B = 1999L) {
  n <- length(x); l <- round(sqrt(n))
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  tb <- numeric(B)
  for (b in seq_len(B)) {
    ini <- sample.int(n - l + 1L, ceiling(n / l), replace = TRUE)
    idx <- unlist(lapply(ini, function(i) i:(i + l - 1L)))[1:n]
    tb[b] <- estat(x[idx])
  }
  t0 <- estat(x)
  (1 + sum(abs(tb) >= abs(t0))) / (B + 1)
}
s_mk <- function(z) { s <- 0; for (i in 1:(length(z) - 1)) s <- s + sum(sign(z[(i + 1):length(z)] - z[i])); s }
k_pet <- function(z) { n <- length(z); max(abs(cumsum(sapply(1:n, function(t) sum(sign(z[t] - z)))))) }
d_cs <- function(z) { n <- length(z); c0 <- ceiling(n / 3); sum(sign(z[(n - c0 + 1):n] - z[1:c0])) }

serie_ar <- function(seed, n, phi, b = 0) {
  set.seed(seed)
  stats::ts(as.numeric(stats::arima.sim(list(ar = phi), n)) + b * seq_len(n))
}

test_that("Mann-Kendall bootstrap_blocos = bootstrap à mão com a mesma semente", {
  for (cs in list(list(1, 60, 0.6, 0), list(2, 45, 0.3, 0.05), list(3, 90, 0.5, 0))) {
    x <- do.call(serie_ar, cs)
    t <- tr_series_mann_kendall(x, correcao = "bootstrap_blocos", .seed = 42L)
    expect_identical(t$p_valor, boot_mao(as.numeric(x), s_mk, 42L))
    expect_equal(t$extra$bloco, as.integer(round(sqrt(length(x)))))
    expect_equal(t$estatistica, tr_series_mann_kendall(x)$estatistica)
    expect_match(t$nota, "bootstrap de blocos", fixed = TRUE)
  }
  x <- serie_ar(1, 60, 0.6)
  a <- tr_series_mann_kendall(x, correcao = "bootstrap_blocos", .seed = 42L)
  expect_identical(a, tr_series_mann_kendall(x, correcao = "bootstrap_blocos", .seed = 42L))
  expect_false(identical(a$p_valor,
                         tr_series_mann_kendall(x, correcao = "bootstrap_blocos", .seed = 7L)$p_valor))
})

test_that("o bootstrap não mexe no RNG de quem chama", {
  set.seed(123); a <- stats::runif(1)
  set.seed(123); invisible(tr_series_mann_kendall(serie_ar(1, 40, 0.5), correcao = "bootstrap_blocos"))
  set.seed(123); expect_identical(stats::runif(1), a)
})

# Tamanho sob H0 (sem tendência, AR(1)) — lento; os números completos (1000
# réplicas por caso) estão no NEWS 0.3.0: com phi = 0,3 e n = 120 o nível é o
# nominal (5,5%); com phi = 0,6 ele cai de ~31% para 7,7% (n = 120) mas NÃO
# chega a 5%. Aqui 400 réplicas; tolerância de Monte Carlo declarada: 3
# erros-padrão binomiais do valor de referência.
test_that("Mann-Kendall bootstrap_blocos: nível nominal com phi = 0,3; perto dele com phi = 0,6", {
  skip_on_cran()
  taxa <- function(phi) {
    set.seed(20260925 + 10 * phi)
    seeds <- sample.int(1e6, 400)
    mean(vapply(seeds, function(s) {
      tr_series_mann_kendall(serie_ar(s, 120, phi), correcao = "bootstrap_blocos",
                             .seed = s)$p_valor < 0.05
    }, logical(1)))
  }
  se <- function(p) 3 * sqrt(p * (1 - p) / 400)
  expect_lt(abs(taxa(0.3) - 0.05), se(0.05))
  expect_lt(abs(taxa(0.6) - 0.077), se(0.077))
})
