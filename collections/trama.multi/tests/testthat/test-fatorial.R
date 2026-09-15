harman24 <- function() tr_multi_example("harman_24_testes")
quest <- function() tr_multi_example("questionario")

# Casa as colunas de `a` com as de `b` pela maior |correlação| e compara até o
# sinal (a ordem e o sinal são convenção desta coleção, não da referência).
expect_cargas_iguais_fa <- function(a, b, tolerance = 1e-4) {
  a <- unname(as.matrix(a)); b <- unname(as.matrix(b))
  par <- apply(abs(stats::cor(a, b)), 1L, which.max)
  expect_setequal(par, seq_len(ncol(b)))
  expect_equal_ate_sinal(a, b[, par, drop = FALSE], tolerance = tolerance)
}

test_that("ML + varimax e promax batem com o factanal", {
  h <- harman24()
  for (r in c("varimax", "promax", "none")) {
    ref <- stats::factanal(h, factors = 4, rotation = r)
    x <- tr_multi_factor_analysis(h, fatores = 4L, rotacao = if (r == "none") "nenhuma" else r)
    expect_cargas_iguais_fa(x$cargas, unclass(ref$loadings))
    expect_equal(unname(x$unicidade), unname(ref$uniquenesses), tolerance = 1e-6)
  }
  ref <- stats::factanal(h, factors = 4)
  x <- tr_multi_factor_analysis(h, fatores = 4L)
  expect_equal(x$ajuste_ml$estatistica, unname(ref$STATISTIC), tolerance = 1e-8)
  expect_equal(x$ajuste_ml$gl, ref$dof)
  expect_equal(x$ajuste_ml$p_valor, unname(ref$PVAL), tolerance = 1e-8)
})

test_that("escores de regressão e de Bartlett batem com o factanal", {
  h <- harman24()
  for (e in c("regression", "Bartlett")) {
    ref <- stats::factanal(h, factors = 4, rotation = "varimax", scores = e)
    x <- tr_multi_factor_analysis(h, fatores = 4L, escores = tolower(sub("regression", "regressão", e)))
    expect_cargas_iguais_fa(x$escores, ref$scores, tolerance = 1e-3)
  }
  expect_null(tr_multi_factor_analysis(h, fatores = 4L, escores = "nenhum")$escores)
})

test_that("PAF bate com o psych::fa(fm = 'pa')", {
  skip_if_not_installed("psych")
  q <- quest()
  x <- tr_multi_factor_analysis(q, fatores = 3L, metodo = "paf", rotacao = "nenhuma")
  ref <- psych::fa(stats::cor(q[-1]), nfactors = 3, n.obs = 400, fm = "pa", rotate = "none",
                   min.err = 1e-10, max.iter = 1000)
  expect_equal(unname(x$comunalidade), unname(ref$communality), tolerance = 1e-3)
  expect_cargas_iguais_fa(x$cargas, unclass(ref$loadings), tolerance = 1e-3)
  expect_null(x$ajuste_ml)
  expect_true(x$convergiu)
})

test_that("Harman 24 com 4 fatores oblimin reproduz a estrutura clássica", {
  x <- tr_multi_factor_analysis(harman24(), fatores = 4L, rotacao = "oblimin")
  dom <- colnames(x$cargas)[max.col(abs(x$cargas))]
  names(dom) <- x$variaveis
  espacial <- dom[c("percepcao_visual", "cubos", "bandeiras", "tabuleiro_formas")]
  verbal <- dom[c("compreensao_paragrafo", "completar_sentencas", "significado_palavras")]
  velocidade <- dom[c("adicao", "contar_pontos")]
  memoria <- dom[c("reconhecer_palavras", "reconhecer_numeros", "objeto_numero")]
  for (g in list(espacial, verbal, velocidade, memoria)) expect_length(unique(g), 1L)
  expect_length(unique(c(espacial[[1]], verbal[[1]], velocidade[[1]], memoria[[1]])), 4L)
  expect_true(all(x$phi[upper.tri(x$phi)] > 0))
})

test_that("questionário com 3 fatores oblimin recupera o que foi plantado", {
  v <- .tr_multi_questionario_verdade()
  x <- tr_multi_factor_analysis(quest(), fatores = 3L, rotacao = "oblimin")
  # Casa cada fator plantado com o estimado pelas cargas.
  par <- apply(abs(stats::cor(v$cargas, x$cargas)), 1L, which.max)
  expect_setequal(par, 1:3)
  s <- sign(diag(stats::cor(v$cargas, x$cargas[, par])))
  est <- x$cargas[, par] %*% diag(s)
  dom_plantado <- max.col(abs(v$cargas))
  expect_equal(max.col(abs(est)), dom_plantado)
  expect_lt(est["soc5", 2], -.4)
  phi <- diag(s) %*% x$phi[par, par] %*% diag(s)
  expect_equal(sign(phi[upper.tri(phi)]), sign(v$phi[upper.tri(v$phi)]))
  # Na varimax Φ é a identidade — a correlação plantada some dali.
  expect_equal(tr_multi_factor_analysis(quest(), fatores = 3L)$phi, diag(3), ignore_attr = TRUE)
})

test_that("o objeto cumpre o contrato: T, Φ, estrutura, nomes", {
  x <- tr_multi_factor_analysis(quest(), fatores = 3L, rotacao = "promax", metodo = "paf")
  expect_s3_class(x, "tr_multi_fa")
  expect_setequal(names(x), .TR_MULTI_CAMPOS_FA)
  expect_equal(unname(x$cargas_brutas %*% t(solve(x$rotmat))), unname(x$cargas), tolerance = 1e-8)
  expect_equal(x$estrutura, x$cargas %*% x$phi)
  expect_equal(colnames(x$cargas), c("F1", "F2", "F3"))
  expect_equal(rownames(x$cargas), x$variaveis)
  expect_equal(dim(x$escores), c(400L, 3L))
  expect_equal(x$n, 400L)
  y <- tr_multi_factor_analysis(harman24(), fatores = 1L)
  expect_equal(y$rotacao, "nenhuma")
})

test_that("erros: fatores demais, rotação inválida, caso Heywood", {
  err <- tryCatch(tr_multi_factor_analysis(harman24(), fatores = 18L), condition = identity)
  expect_s3_class(err, "tr_multi_error_too_many_factors")
  expect_match(conditionMessage(err), "o máximo é 17", fixed = TRUE)
  expect_error(tr_multi_factor_analysis(harman24(), fatores = 18L, metodo = "paf"),
               class = "tr_multi_error_too_many_factors")
  expect_error(tr_multi_factor_analysis(harman24(), rotacao = "geomin"), class = "tr_multi_error_bad_option")
  expect_error(tr_multi_factor_analysis(harman24(), metodo = "minres"), class = "tr_multi_error_bad_option")
  set.seed(3)
  z <- matrix(stats::rnorm(300 * 4), 300)
  d <- as.data.frame(cbind(z, z[, 1] + stats::rnorm(300, sd = .05)))
  err <- tryCatch(tr_multi_factor_analysis(d, fatores = 2L, metodo = "paf"), condition = identity)
  expect_s3_class(err, "tr_multi_error_heywood")
  expect_match(conditionMessage(err), "V1", fixed = TRUE)
  # No ML o mesmo caso não erra: encosta no piso, e o resumo aponta.
  expect_match(.tr_multi_fa_resumo(tr_multi_factor_analysis(d, fatores = 1L))$heywood, "V1")
})

test_that("tabela das cargas: padrão, estrutura, Φ, ordenada", {
  x <- tr_multi_factor_analysis(quest(), fatores = 3L, rotacao = "oblimin")
  p <- tr_multi_fa_loadings(x, ordenar = FALSE)
  expect_equal(names(p), c("variavel", "F1", "F2", "F3", "comunalidade", "unicidade", "complexidade"))
  expect_equal(p$variavel, x$variaveis)
  e <- tr_multi_fa_loadings(x, "estrutura", ordenar = FALSE)
  expect_equal(as.matrix(e[2:4]), unname(x$estrutura), ignore_attr = TRUE)
  phi <- tr_multi_fa_loadings(x, "correlação entre fatores")
  expect_equal(names(phi), c("fator", "F1", "F2", "F3"))
  expect_equal(as.matrix(phi[-1]), unname(x$phi), ignore_attr = TRUE)
  o <- tr_multi_fa_loadings(x)
  dom <- max.col(abs(as.matrix(o[2:4])))
  expect_false(is.unsorted(dom))
  expect_equal(sort(o$variavel), sort(x$variaveis))
  expect_true(all(o$complexidade >= 1 - 1e-12))
  expect_identical(.tr_multi_fa_tabela(x), p)
})

test_that("resumo do card", {
  r <- .tr_multi_fa_resumo(tr_multi_factor_analysis(quest(), fatores = 3L))
  expect_equal(r$metodo, "ml")
  expect_equal(r$fatores, 3L)
  expect_true(r$convergiu)
  expect_gt(r$p_valor_chi2, 0)
  expect_null(.tr_multi_fa_resumo(tr_multi_factor_analysis(quest(), fatores = 3L, metodo = "paf"))$p_valor_chi2)
})

test_that("mapa das cargas é ggplot acabado que constrói", {
  x <- tr_multi_factor_analysis(quest(), fatores = 3L, rotacao = "oblimin")
  g <- tr_multi_plot_loadings(x, corte = .4)
  expect_s3_class(g, "ggplot")
  expect_no_error(ggplot2::ggplot_build(g))
  expect_error(tr_multi_plot_loadings(x, corte = 2), class = "tr_multi_error_bad_option")
})

test_that("tipo multi/fa: guard no store, identidade e preview PNG", {
  tipo <- multi_fa_type()
  x <- tr_multi_factor_analysis(quest(), fatores = 3L)
  f <- tempfile(fileext = ".rds")
  expect_error(tipo$store(list(a = 1), f), class = "tr_multi_error_not_a_fa")
  tipo$store(x, f)
  expect_identical(tipo$restore(f), x)
  skip_if_not_installed("png")
  art <- tipo$preview(x, ctx_tmp())
  expect_equal(art$renderer, "trama/image")
  expect_no_error(png::readPNG(art$files$png))
})

test_that("pelo motor: exemplo -> análise fatorial -> data/arrange pelo adaptador", {
  reg <- multi_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("h", "multi/example", dataset = "harman_24_testes") |>
    trama::tr_add("af", "multi/factor_analysis", fatores = 4L, rotacao = "oblimin", from = "h") |>
    trama::tr_add("ord", "data/arrange", cols = "comunalidade", desc = TRUE, from = "af")
  d <- rodar(f, "ord")
  expect_s3_class(d, "tbl_df")
  expect_equal(nrow(d), 24L)
  expect_false(is.unsorted(rev(d$comunalidade)))
})
