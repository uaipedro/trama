# `tr_models_web()` (reduzir.R): o bloco que a página usa para prever no
# navegador. Oráculo: `tr_models_predict_raw()` do modelo completo; o
# `prever` roda num R À PARTE, sem o trama carregado (como no WebR), sobre o
# motor lido de RDS. Tolerância 1e-12.

prever_fora <- function(casos) {
  skip_if_not_installed("callr")
  callr::r(function(casos) {
    lapply(casos, function(cs) {
      f <- tempfile(fileext = ".rds"); saveRDS(cs$motor, f)
      prever <- eval(parse(text = cs$prever))
      prever(readRDS(f), cs$novos)
    })
  }, args = list(casos = casos))
}

test_that("lm, glm e logística preveem no R sem o trama igual ao modelo completo", {
  d <- dados_reduzir()
  ms <- list(lm = tr_models_lm(d, formula = "y ~ poly(x1, 2) + x2 + trat"),
             pois = tr_models_glm(d, formula = "cont ~ x1 + x2", familia = "poisson"),
             logit = tr_models_glm(d, formula = "classe ~ x1 + x2 + trat", familia = "binomial"))
  ws <- lapply(ms, tr_models_web)
  fora <- prever_fora(lapply(ws, function(w) list(motor = w$motor, prever = w$prever, novos = novos_reduzir)))
  for (k in names(ms)) {
    ref <- tr_models_predict_raw(ms[[k]], novos_reduzir)
    if (is.null(ref$prob)) {
      expect_equal(fora[[k]]$previsto, ref$previsto, tolerance = 1e-12)
    } else {
      expect_identical(fora[[k]]$previsto, as.character(ref$previsto))
      expect_equal(unname(as.matrix(fora[[k]][, -1])), unname(ref$prob), tolerance = 1e-12)
      expect_identical(names(fora[[k]]), ws[[k]]$saida$colunas)
    }
    expect_false(inherits(ws[[k]]$motor, "tr_models_fit"))
    expect_length(vazados(ws[[k]]$motor, c(d$x1, d$x2, d$y)), 0L)
  }
})

test_that("as entradas cobrem o treino sem expor mínimo, máximo ou meio", {
  d <- dados_reduzir()
  w <- tr_models_web(tr_models_lm(d, formula = "y ~ x1 + trat"))
  x1 <- w$entradas[[1]]
  expect_identical(x1$tipo, "numero")
  expect_lte(x1$min, min(d$x1)); expect_gte(x1$max, max(d$x1))
  expect_false(any(c(x1$min, x1$max, x1$padrao) %in% d$x1))
  expect_identical(w$entradas[[2]], list(nome = "trat", tipo = "fator", niveis = c("a", "b", "c"), padrao = "a"))
  expect_identical(names(w$pacotes), "stats")
})

test_that("default: sem bloco web; reduzido recusa (as faixas pedem o treino)", {
  expect_null(tr_models_web(structure(list(), class = c("tr_fake", "tr_models_fit"))))
  r <- tr_models_strip(tr_models_lm(dados_reduzir(), formula = "y ~ x1"))
  expect_error(tr_models_web(r), class = "tr_models_error_not_applicable")
})
