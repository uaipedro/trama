test_that("o guard dos tipos recusa objeto sem os campos", {
  expect_error(sampling_plan_type()$store(list(n = 1), tempfile()), class = "tr_sampling_error_not_a_plan")
  expect_error(sampling_sample_type()$store(ex("fazendas"), tempfile()), class = "tr_sampling_error_not_a_sample")
  est <- tr_sampling_mean(tr_sampling_srs(ex("fazendas"), n = 50L), "producao_t")
  est$tabela$cv_pct <- NULL
  expect_error(sampling_estimate_type()$store(est, tempfile()), class = "tr_sampling_error_not_an_estimate")
  expect_error(sampling_simulation_type()$store(list(), tempfile()), class = "tr_sampling_error_not_a_simulation")
})

test_that("previews dos tipos saem em JSON para todo desenho", {
  f <- ex("fazendas")
  amostras <- list(
    tr_sampling_srs(f, n = 50L),
    tr_sampling_stratified(f, "regiao", n = 80L),
    tr_sampling_pps(f, "area_ha", n = 40L),
    tr_sampling_cluster(f, "municipio", conglomerados = 5L, probabilidade = "proporcional ao tamanho"),
    tr_sampling_two_stage(ex("escolas"), "escola", conglomerados = 6L, por_conglomerado = 5L),
    tr_sampling_design(ex("domicilios"), pesos = "peso", estrato = "estrato", conglomerado = "setor"))
  for (a in amostras) {
    pv <- sampling_sample_type()$preview(a, ctx_tmp())
    expect_equal(pv$renderer, "sampling/sample", info = a$rotulo)
    expect_true(length(pv$data$estratos) >= 1L, info = a$rotulo)
    expect_no_error(jsonlite::toJSON(pv$data, auto_unbox = TRUE, null = "null"))
    e <- sampling_estimate_type()$preview(tr_sampling_proportion(a, if ("irrigada" %in% names(a$dados)) "irrigada"
                                                                 else if ("reprovado" %in% names(a$dados)) "reprovado"
                                                                 else "internet"), ctx_tmp())
    expect_equal(length(e$data$linhas), 2L, info = a$rotulo)
    expect_no_error(jsonlite::toJSON(e$data, auto_unbox = TRUE, null = "null"))
  }
  planos <- list(tr_sampling_size_mean(), tr_sampling_size_proportion(),
                 tr_sampling_size_stratified(ex("estratos_fazendas"), "regiao", "N", "desvio_producao", n_total = 100L),
                 tr_sampling_size_cluster(tr_sampling_size_proportion(), 10, 0.1))
  for (p in planos) {
    pv <- sampling_plan_type()$preview(p, ctx_tmp())
    expect_true(length(pv$data$passos) >= 2L, info = p$rotulo)
    expect_no_error(jsonlite::toJSON(pv$data, auto_unbox = TRUE, null = "null"))
    expect_s3_class(.tr_sampling_plano_tabela(p), "data.frame")
  }
  sim <- tr_sampling_simulate(amostras[[1]], "producao_t", repeticoes = 20L)
  pv <- sampling_simulation_type()$preview(sim, ctx_tmp())
  expect_true(file.exists(unlist(pv$files)[[1]]))
})
