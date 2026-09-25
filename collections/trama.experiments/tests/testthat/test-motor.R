# Pelo motor: o plano vira tabela pelo adaptador e a ANOVA de models roda.

resposta_qualquer <- "round(sin(unidade) * 3 + unidade %% 5, 3)"

test_that("design -> resposta -> ANOVA de models, para cada estrutura clássica", {
  reg <- experiments_registry()
  casos <- list(
    list(e = "dic", no = "models/anova_dic", p = list(tratamento = "tratamento")),
    list(e = "dbc", no = "models/anova_dbc", p = list(tratamento = "tratamento", bloco = "bloco")),
    list(e = "dql", no = "models/anova_dql", p = list(tratamento = "tratamento", linha = "linha", coluna = "coluna")))
  for (cs in casos) {
    f <- trama::tr_flow(reg) |>
      trama::tr_add("plano", "experiments/design", estrutura = cs$e) |>
      trama::tr_add("dados", "data/mutate", name = "y", expr = resposta_qualquer, from = "plano")
    f <- do.call(trama::tr_add, c(list(f, "anova", cs$no, resposta = "y", from = "dados"), cs$p))
    expect_s3_class(rodar(f, "anova"), "tr_models_fit")
  }
  f <- trama::tr_flow(reg) |>
    trama::tr_add("plano", "experiments/design", estrutura = "parcela_subdividida",
                  fatores = "irrigacao: baixa, alta; variedade: A, B, C") |>
    trama::tr_add("dados", "data/mutate", name = "y", expr = resposta_qualquer, from = "plano") |>
    trama::tr_add("sp", "models/anova_split_plot", resposta = "y", parcela = "irrigacao",
                  subparcela = "variedade", bloco = "bloco", from = "dados") |>
    trama::tr_add("fat", "models/anova_factorial", resposta = "y", fatores = "irrigacao, variedade",
                  bloco = "bloco", from = "dados") |>
    trama::tr_add("mapa", "experiments/view", from = "plano")
  expect_s3_class(rodar(f, "sp"), "tr_models_fit")
  expect_s3_class(rodar(f, "fat"), "tr_models_fit")
  expect_s3_class(rodar(f, "mapa"), "ggplot")
})

test_that("a análise sugerida no plano roda como está (com a resposta prefixada)", {
  for (cs in list(list("bib", "t: 7"), list("faixas", "a: 1, 2; b: x, y, z"),
                  list("medidas_repetidas", tempos = "0, 1, 2"), list("crossover", "t: 4", repeticoes = 2L),
                  list("composto_central", "x1; x2"), list("grupos"))) {
    p <- do.call(ds, cs)
    d <- p$unidades
    set.seed(1); d$y <- stats::rnorm(nrow(d))
    fn <- getExportedValue("trama.models", sub("models/", "tr_models_", p$analise$no, fixed = TRUE))
    fit <- suppressMessages(suppressWarnings(fn(d, formula = paste("y", p$analise$params$formula))))
    expect_s3_class(fit, "tr_models_fit")
  }
})

test_that("composto central -> superfície de resposta (nó de análise da coleção)", {
  reg <- experiments_registry()
  skip_if_not("experiments/response_surface" %in% names(reg$nodes))
  f <- trama::tr_flow(reg) |>
    trama::tr_add("plano", "experiments/design", estrutura = "composto_central", fatores = "x1; x2") |>
    trama::tr_add("dados", "data/mutate", name = "y", expr = "50 - (x1 - 0.3)^2 - 2 * x2^2 + sin(unidade) / 10",
                  from = "plano")
  expect_s3_class(rodar(f, "dados"), "data.frame")
})
