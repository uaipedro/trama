test_that("simulação: sem viés, cobertura perto da nominal, e EP estimado bate com o empírico", {
  f <- ex("fazendas")
  a <- tr_sampling_srs(f, n = 200L)
  sim <- tr_sampling_simulate(a, "producao_t", repeticoes = 400L, .seed = 1L)
  r <- sim$resumo
  expect_equal(r$verdadeiro, mean(f$producao_t))
  expect_lt(abs(r$vies_relativo_pct), 2)
  expect_gt(r$cobertura_pct, 90)
  expect_equal(r$ep_estimado / r$ep_empirico, 1, tolerance = 0.15)
  # A mesma semente dá a mesma simulação.
  expect_identical(tr_sampling_simulate(a, "producao_t", repeticoes = 30L, .seed = 9L)$replicas,
                   tr_sampling_simulate(a, "producao_t", repeticoes = 30L, .seed = 9L)$replicas)
})

test_that("nas fazendas, a estratificada ganha da AAS e o conglomerado perde", {
  f <- ex("fazendas")
  aas <- tr_sampling_simulate(tr_sampling_srs(f, n = 200L), "producao_t", repeticoes = 200L)
  est <- tr_sampling_simulate(tr_sampling_stratified(f, "regiao", n = 200L), "producao_t", repeticoes = 200L)
  cong <- tr_sampling_simulate(tr_sampling_cluster(f, "municipio", conglomerados = 10L), "producao_t",
                               repeticoes = 200L)
  expect_lt(est$resumo$ep_empirico, aas$resumo$ep_empirico)
  expect_gt(cong$resumo$ep_empirico, aas$resumo$ep_empirico)
  expect_s3_class(tr_sampling_plot_simulation(list(aas, est, cong)), "ggplot")
})

test_that("simulação refaz a pós-estratificação e recusa amostra declarada", {
  f <- ex("fazendas")
  p <- tr_sampling_poststratify(tr_sampling_srs(f, n = 150L), ex("estratos_fazendas"), "regiao", "N")
  sim <- tr_sampling_simulate(p, "producao_t", "total", repeticoes = 50L)
  expect_match(sim$resumo$desenho, "pós-estratificada", fixed = TRUE)
  d <- tr_sampling_design(ex("domicilios"), pesos = "peso")
  expect_error(tr_sampling_simulate(d, "renda"), class = "tr_sampling_error_no_population")
})
