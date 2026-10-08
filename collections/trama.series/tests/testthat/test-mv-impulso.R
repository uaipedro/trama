# Impulso-resposta e decomposição da variância (series/irf, series/fevd).
#
# Oráculo: o próprio `vars` (Pfaff 2008), chamado direto. Tolerância 1e-10
# (máximo da diferença absoluta) em todos os pontos, bandas e frações: é a
# mesma conta, então a diferença esperada é zero. Dados: `vars::Canada`
# (VAR(2) com constante) e o `denmark` do `urca` (VECM, posto 1, como na
# vinheta do `vars`).

tol <- 1e-10

canada_modelo <- function() {
  e <- new.env()
  data("Canada", package = "vars", envir = e)
  tr_series_var(e$Canada, defasagens = 2L, deterministico = "constante")
}

canada_ref <- function() {
  e <- new.env()
  data("Canada", package = "vars", envir = e)
  vars::VAR(e$Canada, p = 2, type = "const")
}

vecm_modelo <- function() {
  e <- new.env()
  data("denmark", package = "urca", envir = e)
  sjd <- as.matrix(e$denmark[, c("LRM", "LRY", "IBO", "IDE")])
  jo <- urca::ca.jo(sjd, ecdet = "const", type = "eigen", K = 2, spec = "longrun")
  .tr_series_var(vars::vec2var(jo, r = 1), sjd, "VECM", vecm = list(posto = 1L))
}

# Compara a tabela da IRF com o `vars::irf`, par a par, em um campo ("valor",
# "li" ou "ls").
confere_irf <- function(d, ref, campo, mapa) {
  for (i in names(ref$irf)) {
    for (j in colnames(ref$irf[[i]])) {
      if (!(i %in% mapa$imp && j %in% mapa$resp)) next
      sel <- d[d$impulso == i & d$resposta == j, ]
      alvo <- switch(campo,
        valor = ref$irf[[i]][, j],
        li = ref$Lower[[i]][, j],
        ls = ref$Upper[[i]][, j])
      expect_lt(max(abs(sel[[campo]] - unname(alvo))), tol, label = paste(i, j, campo))
    }
  }
}

todos <- function(ref) list(imp = names(ref$irf), resp = colnames(ref$irf[[1]]))

test_that("IRF sem banda: pontos iguais a vars::irf(boot = FALSE) a 1e-10", {
  m <- canada_modelo()
  ref <- vars::irf(canada_ref(), n.ahead = 10, ortho = TRUE, boot = FALSE)
  d <- .tr_series_irf_dados(m, "", "", 10L, TRUE, FALSE, 0L, 0.95, NULL)
  expect_equal(nrow(d), 4L * 4L * 11L)
  confere_irf(d, ref, "valor", todos(ref))
  expect_true(all(is.na(d$li)) && all(is.na(d$ls)))
})

test_that("IRF acumulada e não ortogonal: iguais ao vars a 1e-10", {
  m <- canada_modelo()
  ref_acum <- vars::irf(canada_ref(), n.ahead = 10, ortho = TRUE, cumulative = TRUE, boot = FALSE)
  d_acum <- .tr_series_irf_dados(m, "", "", 10L, TRUE, TRUE, 0L, 0.95, NULL)
  confere_irf(d_acum, ref_acum, "valor", todos(ref_acum))

  ref_nao <- vars::irf(canada_ref(), n.ahead = 10, ortho = FALSE, boot = FALSE)
  d_nao <- .tr_series_irf_dados(m, "", "", 10L, FALSE, FALSE, 0L, 0.95, NULL)
  confere_irf(d_nao, ref_nao, "valor", todos(ref_nao))
})

test_that("IRF com bandas: Lower/Upper iguais ao vars::irf(boot = TRUE, seed) a 1e-10", {
  m <- canada_modelo()
  ref <- vars::irf(canada_ref(), n.ahead = 10, ortho = TRUE, boot = TRUE, runs = 50,
                   seed = 42, ci = 0.9)
  d <- .tr_series_irf_dados(m, "", "", 10L, TRUE, FALSE, 50L, 0.9, 42L)
  confere_irf(d, ref, "valor", todos(ref))
  confere_irf(d, ref, "li", todos(ref))
  confere_irf(d, ref, "ls", todos(ref))
})

test_that("a semente padrão é 1 e a do nó é a que vale", {
  m <- canada_modelo()
  ref <- vars::irf(canada_ref(), n.ahead = 5, boot = TRUE, runs = 20, seed = 1, ci = 0.95)
  d <- .tr_series_irf_dados(m, "", "", 5L, TRUE, FALSE, 20L, 0.95, NULL)
  confere_irf(d, ref, "li", todos(ref))
  d7 <- .tr_series_irf_dados(m, "", "", 5L, TRUE, FALSE, 20L, 0.95, 7)
  ref7 <- vars::irf(canada_ref(), n.ahead = 5, boot = TRUE, runs = 20, seed = 7, ci = 0.95)
  confere_irf(d7, ref7, "ls", todos(ref7))
})

test_that("a semente não mexe no gerador de números aleatórios da sessão", {
  m <- canada_modelo()
  set.seed(3); a <- stats::runif(1)
  set.seed(3); .tr_series_irf_dados(m, "", "", 5L, TRUE, FALSE, 20L, 0.95, 9)
  b <- stats::runif(1)
  expect_identical(a, b)
})

test_that("o recorte por impulso e por resposta não muda os pontos escolhidos", {
  m <- canada_modelo()
  ref <- vars::irf(canada_ref(), n.ahead = 10, boot = FALSE)
  d <- .tr_series_irf_dados(m, "prod", "e, U", 10L, TRUE, FALSE, 0L, 0.95, NULL)
  expect_setequal(unique(as.character(d$impulso)), "prod")
  expect_setequal(unique(as.character(d$resposta)), c("e", "U"))
  confere_irf(d, ref, "valor", list(imp = "prod", resp = c("e", "U")))
})

test_that("IRF do VECM (vec2var): pontos e bandas iguais ao vars a 1e-10", {
  m <- vecm_modelo()
  ref <- vars::irf(m$ajuste, n.ahead = 8, ortho = TRUE, boot = FALSE)
  d <- .tr_series_irf_dados(m, "", "", 8L, TRUE, FALSE, 0L, 0.95, NULL)
  confere_irf(d, ref, "valor", todos(ref))
  refb <- vars::irf(m$ajuste, n.ahead = 8, ortho = TRUE, boot = TRUE, runs = 20, seed = 5, ci = 0.95)
  db <- .tr_series_irf_dados(m, "", "", 8L, TRUE, FALSE, 20L, 0.95, 5)
  confere_irf(db, refb, "li", todos(refb))
  confere_irf(db, refb, "ls", todos(refb))
})

test_that("FEVD igual a vars::fevd a 1e-10, e cada linha soma 1 a 1e-10", {
  m <- canada_modelo()
  ref <- vars::fevd(canada_ref(), n.ahead = 10)
  d <- .tr_series_fevd_dados(m, 10L)
  for (s in names(ref)) {
    sel <- d[d$serie == s, ]
    mat <- matrix(sel$fracao, nrow = 10L, ncol = ncol(ref[[s]]))
    expect_lt(max(abs(mat - unname(ref[[s]]))), tol, label = s)
    expect_lt(max(abs(rowSums(mat) - 1)), tol, label = paste("soma", s))
  }
})

test_that("FEVD do VECM igual a vars::fevd a 1e-10", {
  m <- vecm_modelo()
  ref <- vars::fevd(m$ajuste, n.ahead = 6)
  d <- .tr_series_fevd_dados(m, 6L)
  for (s in names(ref)) {
    mat <- matrix(d$fracao[d$serie == s], nrow = 6L)
    expect_lt(max(abs(mat - unname(ref[[s]]))), tol, label = s)
    expect_lt(max(abs(rowSums(mat) - 1)), tol, label = paste("soma", s))
  }
})

test_that("os gráficos montam, com a banda só quando há reamostras", {
  m <- canada_modelo()
  p0 <- tr_series_irf(m, reamostras = 0L)
  expect_s3_class(p0, "ggplot")
  expect_false(any(vapply(p0$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1))))
  p1 <- tr_series_irf(m, reamostras = 20L, .seed = 1)
  expect_true(any(vapply(p1$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1))))
  expect_no_error(ggplot2::ggplot_build(p1))
  pf <- tr_series_fevd(m, horizonte = 8L)
  expect_s3_class(pf, "ggplot")
  expect_no_error(ggplot2::ggplot_build(pf))
})

test_that("nomes de série desconhecidos e params inválidos são recusados", {
  m <- canada_modelo()
  expect_error(tr_series_irf(m, impulso = "xyz"), class = "tr_series_error_bad_option")
  expect_error(tr_series_irf(m, respostas = "nope"), class = "tr_series_error_bad_option")
  expect_error(tr_series_irf(m, horizonte = 0L), class = "tr_series_error_bad_option")
  expect_error(tr_series_irf(m, confianca = 1.2), class = "tr_series_error_bad_option")
  expect_error(tr_series_irf(m, ortogonal = "sim"), class = "tr_series_error_bad_option")
  expect_error(tr_series_fevd(m, horizonte = 0L), class = "tr_series_error_bad_option")
})

test_that("os dois blocos recusam o que não é um ajuste VAR ou VECM", {
  expect_error(tr_series_irf(datasets::AirPassengers), class = "tr_series_error_not_var")
  expect_error(tr_series_fevd(datasets::AirPassengers), class = "tr_series_error_not_var")
})

test_that("IRF com dummies sazonais (VAR construído por series/var): bandas iguais ao vars a 1e-10", {
  # O call guardado pelo series/var tem o `season` como símbolo: é o caso que
  # o congelamento do call cobre (ver .tr_series_ajuste_congelado).
  e <- new.env()
  data("Canada", package = "vars", envir = e)
  m <- tr_series_var(e$Canada, defasagens = 2L, deterministico = "constante", sazonal = TRUE)
  ref <- vars::irf(vars::VAR(e$Canada, p = 2, type = "const", season = 4L), n.ahead = 6,
                   boot = TRUE, runs = 20, seed = 11, ci = 0.95)
  d <- .tr_series_irf_dados(m, "", "", 6L, TRUE, FALSE, 20L, 0.95, 11)
  confere_irf(d, ref, "valor", todos(ref))
  confere_irf(d, ref, "li", todos(ref))
  confere_irf(d, ref, "ls", todos(ref))
})
