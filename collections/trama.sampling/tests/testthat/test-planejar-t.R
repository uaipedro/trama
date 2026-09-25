# Planejamento com t nos gl do desenho (padrão) e z só quando declarado.
#
# Oráculo: a definição. O n planejado tem de ser o MENOR inteiro cuja margem,
# com o mesmo t que o card de `sampling/mean` vai usar (gl = UPAs − estratos),
# fica dentro da pedida — procurado aqui por força bruta, sem a iteração do
# bloco. A correção finita é a de Cochran (1977, eq. 4.2 na forma
# n = n₀/(1 + n₀/N)), que equivale a E² = q²·pq·(1/n − 1/N).

menor_n <- function(margem, E, de = 2L) {
  n <- de
  while (margem(n) > E) n <- n + 1L
  n
}

test_that("size_proportion e size_mean: o menor n com t_{n−1}", {
  for (E in c(0.02, 0.05, 0.1, 0.2, 0.3)) {
    esperado <- menor_n(function(n) stats::qt(0.975, n - 1) * sqrt(0.25 / n), E)
    expect_equal(tr_sampling_size_proportion(erro = E)$n, esperado, info = as.character(E))
  }
  # Com população finita.
  esperado <- menor_n(function(n) stats::qt(0.975, n - 1) * sqrt(0.21 * (1 / n - 1 / 300)), 0.1)
  expect_equal(tr_sampling_size_proportion(proporcao = 0.3, erro = 0.1, populacao = 300)$n, esperado)
  for (E in c(1, 2.5, 5)) {
    esperado <- menor_n(function(n) stats::qt(0.99 + 0.005, n - 1) * 10 / sqrt(n), E)
    expect_equal(tr_sampling_size_mean(desvio_padrao = 10, erro = E, confianca = "99%")$n, esperado, info = as.character(E))
  }
  # z declarado: a fórmula fechada de Cochran, 385 para ±5 pontos.
  expect_equal(tr_sampling_size_proportion(distribuicao = "z")$n, 385L)
  expect_match(tr_sampling_size_proportion()$passos$passo[[1]], "t com")
})

test_that("size_cluster: o menor número de conglomerados com t_{c−1}", {
  base <- tr_sampling_size_proportion(erro = 0.1)
  for (m in c(5, 20)) for (rho in c(0.02, 0.2)) {
    deff <- 1 + (m - 1) * rho
    esperado <- menor_n(function(k) stats::qt(0.975, k - 1) * sqrt(0.25 * deff / (k * m)), 0.1)
    expect_equal(tr_sampling_size_cluster(base, m, rho)$conglomerados, esperado, info = paste(m, rho))
  }
  # Poucos conglomerados é onde o t pesa: 2 gl já dobram o quantil.
  b <- tr_sampling_size_mean(desvio_padrao = 10, erro = 5)
  c_t <- tr_sampling_size_cluster(b, 10, 0.3)
  c_z <- tr_sampling_size_cluster(b, 10, 0.3, distribuicao = "z")
  expect_gt(c_t$conglomerados, c_z$conglomerados)
})

test_that("size_stratified: a margem alcançada usa t com n − H gl e fica dentro da pedida", {
  e <- ex("estratos_fazendas")
  p <- tr_sampling_size_stratified(e, "regiao", "N", "desvio_producao", alocacao = "neyman", erro = 60)
  expect_equal(p$parametros$gl, p$n - nrow(e))
  expect_lte(p$erro_alcancado, 60 + 1e-6)
})

test_that("detectable_difference: n desiguais, p próprios, t e correção finita", {
  g <- tibble::tibble(v = "x", grupo = c("a", "b"), n = c(300, 150), p = c(0.2, 0.3), N = c(1000, 400))
  # z, p comum 0,2, sem fpc: a forma de Fleiss, Levin & Paik (2003, cap. 4)
  # com r = n_b/n_a: n_a = [z_α√((r + 1)p̄q̄) + z_β√(r·p_a q_a + p_b q_b)]²/(r·δ²).
  fleiss_na <- function(pa, pb, r, za, zb) {
    pbar <- (pa + r * pb) / (1 + r)
    (za * sqrt((r + 1) * pbar * (1 - pbar)) + zb * sqrt(r * pa * (1 - pa) + pb * (1 - pb)))^2 / (r * (pb - pa)^2)
  }
  t1 <- tr_sampling_detectable_difference(g, "v", "grupo", "n", proporcao = 0.2, distribuicao = "z")
  d <- t1$diferenca_detectavel_pp / 100
  za <- stats::qnorm(0.975); zb <- stats::qnorm(0.8)
  # O pior caso é um dos quatro (referência a ou b, acima ou abaixo); o que
  # sai tem de satisfazer Fleiss num deles.
  cands <- c(fleiss_na(0.2, 0.2 + d, 0.5, za, zb), fleiss_na(0.2, 0.2 - d, 0.5, za, zb),
             fleiss_na(0.2, 0.2 + d, 2, za, zb) * 1, fleiss_na(0.2, 0.2 - d, 2, za, zb))
  alvo <- c(300, 300, 150, 150)
  expect_true(any(abs(cands - alvo) < 1e-6))
  # p próprios: a referência de cada grupo entra.
  t2 <- tr_sampling_detectable_difference(g, "v", "grupo", "n", proporcao_grupo = "p", distribuicao = "z")
  expect_equal(c(t2$p_a, t2$p_b), c(0.2, 0.3))
  expect_false(isTRUE(all.equal(t2$diferenca_detectavel_pp, t1$diferenca_detectavel_pp)))
  # t com n_a + n_b − 2 gl: maior que z; correção finita: menor que sem.
  t3 <- tr_sampling_detectable_difference(g, "v", "grupo", "n", proporcao_grupo = "p")
  expect_equal(t3$gl, 448)
  expect_gt(t3$diferenca_detectavel_pp, t2$diferenca_detectavel_pp)
  t4 <- tr_sampling_detectable_difference(g, "v", "grupo", "n", proporcao_grupo = "p", populacao = "N")
  expect_lt(t4$diferenca_detectavel_pp, t3$diferenca_detectavel_pp)
  # A equação com fpc, conferida à mão no δ que saiu (referência b, abaixo é o pior aqui ou não: confere os 4).
  dd <- t4$diferenca_detectavel_pp / 100
  q_a <- stats::qt(0.975, 448); q_b <- stats::qt(0.8, 448)
  eq <- function(pa, na, nb, ca, cb, pb) {
    pbar <- (na * pa + nb * pb) / (na + nb)
    q_a * sqrt(pbar * (1 - pbar) * (ca / na + cb / nb)) + q_b * sqrt(pa * (1 - pa) * ca / na + pb * (1 - pb) * cb / nb)
  }
  ca <- 1 - 300 / 1000; cb <- 1 - 150 / 400
  lados <- c(eq(0.2, 300, 150, ca, cb, 0.2 + dd), eq(0.2, 300, 150, ca, cb, 0.2 - dd),
             eq(0.3, 150, 300, cb, ca, 0.3 + dd), eq(0.3, 150, 300, cb, ca, 0.3 - dd))
  expect_true(any(abs(lados - dd) < 1e-9))
  expect_error(tr_sampling_detectable_difference(transform(g, N = c(10, 400)), "v", "grupo", "n", populacao = "N"),
               class = "tr_sampling_error_bad_size")
})

test_that("referral: os gl são das redes (sementes), não das respostas", {
  u <- data.frame(cap = c("A", "B"), pop = c(1e6, 2e6), n = c(20, 30))
  r <- tr_sampling_referral(u, "cap", "pop", "n", convidados = "3", icc = "0.1")
  expect_equal(r$gl[r$nivel == "Total"], 50 - 2)
  a <- r[r$nivel == "A", ]
  expect_equal(a$gl, 19)
  expect_equal(a$margem, stats::qt(0.975, 19) * sqrt(a$deff * 0.25 / 80 * (1 - 80 / 1e6)))
})
