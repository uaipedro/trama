# Conglomerados (ou redes) de tamanhos desiguais: deff = 1 + ((CV² + 1)·m̄ − 1)·ρ
# (Eldridge, Ashby & Kerry 2006, doi:10.1093/ije/dyl129, eq. do deff com CV do
# tamanho). Oráculo: a variância exata da média (razão Σy/Σm) sob o modelo de
# ICC comum, Var = σ²·Σ m_i(1 + (m_i − 1)ρ)/(Σm_i)², contra a da AAS σ²/Σm_i. A
# razão das duas é 1 + ρ(Σm_i²/Σm_i − 1) = 1 + ρ(m̄(1 + CV²) − 1), com CV de
# divisor k — identidade, que o teste confere num vetor de tamanhos real.

test_that("o deff com CV é o da variância exata sob ICC comum", {
  m <- c(5, 12, 20, 33, 8, 40, 17, 25)
  rho <- 0.08
  exato <- sum(m * (1 + (m - 1) * rho)) / sum(m)^2 / (1 / sum(m))
  cv <- sqrt(mean((m - mean(m))^2)) / mean(m)
  expect_equal(.tr_sampling_deff_cv(mean(m), cv, rho), exato, tolerance = 1e-12)
})

test_that("size_cluster: CV 0 reproduz a fórmula de tamanhos iguais, e CV > 0 pede mais conglomerados", {
  base <- tr_sampling_size_proportion(distribuicao = "z")
  c0 <- tr_sampling_size_cluster(base, 20, 0.05, distribuicao = "z")
  cz <- tr_sampling_size_cluster(base, 20, 0.05, cv_tamanho = 0, distribuicao = "z")
  expect_identical(c0$conglomerados, cz$conglomerados)
  # Valor da versão 1 (antes do CV e do t): 385 × 1,95 / 20 → 38 conglomerados.
  expect_equal(c0$conglomerados, as.integer(ceiling(base$parametros$n0 * 1.95 / 20)))
  c6 <- tr_sampling_size_cluster(base, 20, 0.05, cv_tamanho = 0.6, distribuicao = "z")
  deff <- 1 + ((0.6^2 + 1) * 20 - 1) * 0.05
  expect_equal(c6$parametros$deff_conglomerado, deff)
  expect_equal(c6$conglomerados, as.integer(ceiling(base$parametros$n0 * deff / 20)))
  expect_error(tr_sampling_size_cluster(base, 20, 0.05, cv_tamanho = -1), class = "tr_sampling_error_bad_option")
})

test_that("referral: CV 0 é o de antes; CV > 0 aumenta o deff das redes com convidados", {
  u <- data.frame(cap = c("A", "B"), pop = c(1e6, 2e6), n = c(100, 100))
  r0 <- tr_sampling_referral(u, "cap", "pop", "n", convidados = "0, 3", icc = "0.1", distribuicao = "z")
  rc <- tr_sampling_referral(u, "cap", "pop", "n", convidados = "0, 3", icc = "0.1", cv_rede = 0.5,
                             distribuicao = "z")
  expect_equal(r0$deff_agrupamento[r0$convidados == 3], rep(1 + 3 * 0.1, 3))
  expect_equal(rc$deff_agrupamento[rc$convidados == 3], rep(1 + ((0.25 + 1) * 4 - 1) * 0.1, 3))
  # Sem convidados a rede é de 1 pessoa: não há tamanho a variar.
  expect_equal(rc$margem[rc$convidados == 0], r0$margem[r0$convidados == 0])
})
