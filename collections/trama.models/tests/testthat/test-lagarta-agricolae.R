test_that("lagarta: um painel por termo, intervalo pela variância condicional, faixa do desvio", {
  s <- tr_models_lmer(ex("sleepstudy"), formula = "Reaction ~ Days + (Days | Subject)")
  p <- tr_models_plot_caterpillar(s, intervalo = "± 2 EP")
  expect_s3_class(p, "ggplot")
  d <- p$data
  expect_equal(levels(d$termo), c("(Intercept)", "Days"))
  expect_equal(d$ls - d$condval, 2 * d$condsd)
  r <- as.data.frame(lme4::ranef(s$ajuste, condVar = TRUE))
  expect_equal(sort(d$condval), sort(r$condval))
  # A mesma ordem de sujeitos nos dois painéis.
  expect_equal(levels(d$grp), as.character(r$grp[r$term == "(Intercept)"][order(r$condval[r$term == "(Intercept)"])]))
  # A faixa usa o desvio padrão que o models/random_effects mostra.
  faixa <- p$layers[[1]]$data
  ve <- tr_models_random_effects(s)
  expect_equal(faixa$sd[[1]], ve$desvio_padrao[ve$componente == "(Intercept)"])
  expect_error(tr_models_plot_caterpillar(s, grupo = "Sujeito"), class = "tr_models_error_unknown_column")
  expect_error(tr_models_plot_caterpillar(milho_dbc()), class = "tr_models_error_not_applicable")
  expect_s3_class(tr_models_plot_caterpillar(
    tr_models_anova_split_plot(ex("aveia"), "producao", "variedade", "nitrogenio", "bloco")), "ggplot")
})

test_that("lagarta: IC usa a confiança (z = qnorm(1 - (1 - conf)/2))", {
  s <- tr_models_lmer(ex("sleepstudy"), formula = "Reaction ~ Days + (Days | Subject)")
  d <- tr_models_plot_caterpillar(s, intervalo = "IC", confianca = 0.9)$data
  expect_equal(d$ls - d$condval, stats::qnorm(0.95) * d$condsd)
  d <- tr_models_plot_caterpillar(s)$data
  expect_equal(d$ls - d$condval, stats::qnorm(0.975) * d$condsd)
  expect_error(tr_models_plot_caterpillar(s, intervalo = "IC 95%"))
})

test_that("lagarta: fluxo com o enum antigo abre com intervalo e confiança separados, uma vez só", {
  reg <- models_registry()
  doc <- trama::tr_doc_parse('{"format":1,"nodes":{
    "a":{"type":"models/plot_caterpillar","params":{"intervalo":"IC 90%"}},
    "b":{"type":"models/plot_caterpillar","params":{"intervalo":"± 1 EP"}}},"edges":[]}')
  doc <- trama::tr_doc_migrate(doc, reg)
  expect_equal(doc$nodes$a$params$intervalo, "IC")
  expect_equal(doc$nodes$a$params$confianca, 0.9)
  expect_equal(doc$nodes$b$params, list(intervalo = "± 1 EP"))
  attr(doc, "migrated") <- NULL
  expect_identical(trama::tr_doc_migrate(doc, reg), doc)
})

test_that("Duncan e Waller-Duncan batem com o agricolae", {
  m <- milho_dbc()
  ref <- agricolae::duncan.test(m$ajuste, "hibrido", console = FALSE)$groups
  d <- tr_models_duncan(m, "hibrido")
  expect_equal(d$tabela$grupo, trimws(as.character(ref$groups[match(levels(m$dados$hibrido), rownames(ref))])))
  expect_null(d$grade)
  refw <- agricolae::waller.test(m$ajuste, "hibrido", K = 100, console = FALSE)$groups
  w <- tr_models_waller_duncan(m, "hibrido")
  expect_equal(w$tabela$grupo, trimws(as.character(refw$groups[match(levels(m$dados$hibrido), rownames(refw))])))
  expect_match(w$nota, "diferença crítica", fixed = TRUE)
  # O card das médias e o adaptador valem para eles.
  expect_s3_class(tr_models_plot_means(d), "ggplot")
  expect_error(tr_models_pairwise(d), class = "tr_models_error_not_applicable")
})

test_that("na parcela subdividida cada fator usa o seu erro", {
  sp <- tr_models_anova_split_plot(ex("aveia"), "producao", "variedade", "nitrogenio", "bloco")
  q <- tr_models_anova_table(sp)$tabela
  v <- tr_models_duncan(sp, "variedade")
  expect_equal(unique(v$tabela$gl), q$gl[q$termo == "Resíduo (a)"])
  n <- tr_models_duncan(sp, "nitrogenio")
  expect_equal(unique(n$tabela$gl), q$gl[q$termo == "Resíduo (b)"])
  expect_error(tr_models_duncan(sp, "variedade, nitrogenio"), class = "tr_models_error_not_applicable")
  expect_error(tr_models_duncan(tr_models_glm(ex("InsectSprays"), "count", "spray"), "spray"),
               class = "tr_models_error_not_applicable")
  expect_error(tr_models_duncan(milho_dbc(), "variedade"), class = "tr_models_error_unknown_column")
})
