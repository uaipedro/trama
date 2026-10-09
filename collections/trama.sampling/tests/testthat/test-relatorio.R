test_that("amostra e estimativa vão ao relatório como tabelas", {
  a <- tr_sampling_stratified(populacao = tr_sampling_example(), estrato = "regiao", n = 240L, .seed = 1L)
  expect_match(tr_sampling_report_sample(a), "*Primeiras 10 de 240 linhas.*", fixed = TRUE)
  e <- tr_sampling_report_estimate(tr_sampling_mean(amostra = a, variavel = "producao_t", por = "regiao"))
  expect_match(e, "| Estimativa | EP | LI 95% | LS 95% | CV (%) |", fixed = TRUE)
  expect_match(e, "| Norte |", fixed = TRUE)
})
