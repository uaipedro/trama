# Cointegração: Engle-Granger, Johansen e VECM, com oráculos do `urca` e do `vars`.
#
# Tolerância: 1e-10 (relativa) em tudo que o pacote de referência calcula
# diretamente. A única exceção é a comparação com os críticos publicados de
# MacKinnon (2010), feita a duas casas (ver o teste de Engle-Granger).

test_that("Engle-Granger: estatística igual ao ur.df dos resíduos (Canada, 1e-10)", {
  data(Canada, package = "vars")
  out <- tr_series_engle_granger(Canada, resposta = "e", defasagens = 0L)
  res <- stats::residuals(stats::lm(e ~ prod + rw + U, data = as.data.frame(Canada)))
  p_lags <- as.integer(trunc((length(res) - 1)^(1 / 3)))
  a <- urca::ur.df(as.numeric(res), type = "none", lags = p_lags, selectlags = "AIC")
  expect_equal(out$estatistica, a@teststat[[1]], tolerance = 1e-10)
  expect_equal(out$extra$defasagens, p_lags)
  expect_equal(out$extra$observacoes, length(res))
})

test_that("Engle-Granger: p-valor de N = 1 igual ao punitroot (1e-10)", {
  # O punitroot é a superfície de UMA série (niv = 1). É o teste do plumbing:
  # a função interna que o bloco usa, no caso em que o urca a expõe.
  stat <- -3.094568
  n <- 84L
  expect_equal(.tr_series_urcval(stat, nobs = n, niv = 1L, trend = "c"),
               urca::punitroot(stat, N = n, trend = "c"), tolerance = 1e-10)
  expect_equal(.tr_series_urcval(stat, nobs = n, niv = 1L, trend = "ct"),
               urca::punitroot(stat, N = n, trend = "ct"), tolerance = 1e-10)
})

test_that("Engle-Granger: críticos assintóticos de MacKinnon para N = 2, constante", {
  # Oráculo publicado: MacKinnon (2010), "Critical values for cointegration
  # tests", Queen's Economics Department WP 1227, Tabela 3, N = 2, τc,
  # coeficiente assintótico β∞: −3,89644 (1%), −3,33613 (5%), −3,04445 (10%)
  # — conferido no PDF em 08/10/2026. O urca implementa a superfície de
  # MacKinnon (1996), que difere da de 2010 na 4ª casa: tolerância 5e-4.
  q <- c(0.01, 0.05, 0.10)
  crit <- vapply(q, function(p) {
    stats::uniroot(function(x) .tr_series_eg_p(x, 0, 2) - p, c(-6, -1), tol = 1e-10)$root
  }, numeric(1))
  expect_lte(max(abs(crit - c(-3.89644, -3.33613, -3.04445))), 5e-4)
})

test_that("Engle-Granger: N variáveis e tendência entram na superfície", {
  expect_error(.tr_series_eg_p(-3, 50, 7), class = "tr_series_error_bad_option")
  expect_false(isTRUE(all.equal(.tr_series_eg_p(-3, 50, 2, "c"), .tr_series_eg_p(-3, 50, 2, "ct"))))
})

test_that("Johansen: traço e autovalor iguais ao ca.jo do denmark (1e-10)", {
  # Exemplo da ajuda do urca: ca.jo(sjd, ecdet = "const", K = 2, spec = "longrun",
  # season = 4), com as quatro séries de denmark.
  data(denmark, package = "urca")
  sjd <- denmark[, c("LRM", "LRY", "IBO", "IDE")]
  serie <- stats::ts(as.matrix(sjd), start = c(1974, 1), frequency = 4)
  colnames(serie) <- colnames(sjd)
  for (met in c("eigen", "trace")) {
    ca <- urca::ca.jo(sjd, ecdet = "const", type = met, K = 2, spec = "longrun", season = 4)
    nome <- if (met == "eigen") "autovalor" else "traco"
    out <- tr_series_johansen(serie, metodo = nome, defasagens = 2L,
                              deterministico = "constante restrita", sazonal = TRUE)
    # A linha r0 da tabela é a linha k - r0 do ca.jo: a ordem se inverte.
    expect_equal(unname(out$estatistica), unname(rev(ca@teststat)), tolerance = 1e-10)
    expect_equal(unname(out$critico_10), unname(rev(ca@cval[, "10pct"])), tolerance = 1e-10)
    expect_equal(unname(out$critico_5), unname(rev(ca@cval[, "5pct"])), tolerance = 1e-10)
    expect_equal(unname(out$critico_1), unname(rev(ca@cval[, "1pct"])), tolerance = 1e-10)
    expect_equal(out$decisao_5, ifelse(rev(ca@teststat) > rev(ca@cval[, "5pct"]),
                                       "rejeita H0", "não rejeita H0"))
  }
})

test_that("Johansen: o posto sugerido é o primeiro r em que a H0 não é rejeitada", {
  data(denmark, package = "urca")
  sjd <- denmark[, c("LRM", "LRY", "IBO", "IDE")]
  serie <- stats::ts(as.matrix(sjd), start = c(1974, 1), frequency = 4)
  colnames(serie) <- colnames(sjd)
  out <- tr_series_johansen(serie, metodo = "traco", defasagens = 2L, sazonal = TRUE)
  r_nao <- which(out$decisao_5 == "não rejeita H0")[[1]]
  expect_match(out$nota[[r_nao]], sprintf("posto sugerido r = %d", r_nao - 1L), fixed = TRUE)
  expect_equal(sum(nzchar(out$nota)), 1L)
})

test_that("VECM: coeficientes iguais ao cajorls e previsão igual ao predict(vec2var) (1e-10)", {
  data(Canada, package = "vars")
  out <- tr_series_vecm(Canada, posto = 1L, defasagens = 2L)
  expect_s3_class(out, "tr_series_var")
  expect_equal(out$tipo, "VECM")
  expect_equal(out$vecm$posto, 1L)
  expect_equal(.tr_series_var_rotulo(out), "VECM(2), posto 1")

  ca <- urca::ca.jo(Canada, type = "trace", ecdet = "const", K = 2, spec = "longrun")
  rls <- urca::cajorls(ca, r = 1)
  v <- vars::vec2var(ca, r = 1)

  # Coeficientes: uma linha por (equação, termo), na ordem das equações do cajorls.
  cf <- .tr_series_var_coefs(out)
  esperado <- unlist(lapply(summary(rls$rlm), function(s) s$coefficients[, 1]), use.names = FALSE)
  expect_equal(unname(cf$estimativa), unname(esperado), tolerance = 1e-10)

  # Previsão: média de cada série igual ao predict do vec2var (80%, mesma conta).
  h <- 8L
  fc <- tr_series_forecast(var = out, horizonte = h)
  pr <- stats::predict(v, n.ahead = h, ci = 0.80)$fcst
  for (nm in colnames(Canada)) {
    expect_equal(as.numeric(fc$forecast[[nm]]$mean), as.numeric(pr[[nm]][, "fcst"]), tolerance = 1e-10,
                 info = nm)
  }
})

test_that("VECM: posto fora de 1 a k-1 é recusado com a classe da coleção", {
  data(Canada, package = "vars")
  expect_error(tr_series_vecm(Canada, posto = 4L), class = "tr_series_error_bad_option")
  expect_error(tr_series_vecm(Canada, posto = 0L), class = "tr_series_error_bad_option")
})

test_that("a fonte do Engle-Granger bate com a linha de docs/fontes.md", {
  # A varredura do test-catalogo pula os blocos de série múltipla (precisam de
  # coluna de resposta); a conferência da fonte fica aqui.
  caminho <- "../../../../docs/fontes.md"
  skip_if_not(file.exists(caminho), "docs/fontes.md fora da árvore do repositório")
  doc <- readLines(caminho, encoding = "UTF-8")
  linha <- grep("^\\| `series/engle_granger` \\|", doc, value = TRUE)
  expect_length(linha, 1L)
  fonte <- trimws(strsplit(linha, "|", fixed = TRUE)[[1]][3])
  data(Canada, package = "vars")
  expect_equal(tr_series_engle_granger(Canada, resposta = "e")$fonte, fonte)
})
