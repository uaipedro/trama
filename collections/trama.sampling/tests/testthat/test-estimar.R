test_that("AAS: erro padrão da média e do total é a fórmula do livro, e o deff é 1", {
  f <- ex("fazendas")
  a <- tr_sampling_srs(f, n = 200L, .seed = 11L)
  y <- a$dados$producao_t
  m <- tr_sampling_mean(a, "producao_t")$tabela
  expect_equal(m$estimativa, mean(y))
  expect_equal(m$erro_padrao, sqrt((1 - 200 / 2400) * stats::var(y) / 200))
  expect_equal(m$deff, 1)
  expect_equal(m$gl, 199L)
  expect_equal(m$li, mean(y) - stats::qt(0.975, 199) * m$erro_padrao)
  t <- tr_sampling_total(a, "producao_t")$tabela
  expect_equal(t$erro_padrao, 2400 * m$erro_padrao)
})

test_that("estratificada: a variância é Σ W²(1 − f)s²/n por estrato", {
  f <- ex("fazendas")
  s <- tr_sampling_stratified(f, "regiao", n = 200L, alocacao = "neyman", variavel_auxiliar = "producao_t", .seed = 3L)
  d <- s$dados; Nh <- table(f$regiao)
  v <- sum(vapply(names(Nh), function(h) {
    yh <- d$producao_t[d$regiao == h]; nh <- length(yh)
    (Nh[[h]] / 2400)^2 * (1 - nh / Nh[[h]]) * stats::var(yh) / nh
  }, 0))
  m <- tr_sampling_mean(s, "producao_t")$tabela
  expect_equal(m$erro_padrao, sqrt(v))
  expect_equal(m$estimativa, sum(vapply(names(Nh), function(h) Nh[[h]] / 2400 * mean(d$producao_t[d$regiao == h]), 0)))
  expect_lt(m$deff, 1)
})

test_that("conglomerado em um estágio: a variância do estimador de razão do livro", {
  f <- ex("fazendas")
  cl <- tr_sampling_cluster(f, "municipio", conglomerados = 12L, .seed = 3L)
  tot <- tapply(cl$dados$producao_t, cl$dados$municipio, sum)
  mi <- tapply(cl$dados$producao_t, cl$dados$municipio, length)
  r <- sum(tot) / sum(mi)
  v <- (1 - 12 / 120) / (12 * mean(mi)^2) * sum((tot - r * mi)^2) / 11
  m <- tr_sampling_mean(cl, "producao_t")$tabela
  expect_equal(m$erro_padrao, sqrt(v))
  expect_gt(m$deff, 2)
  expect_equal(m$gl, 11L)
})

test_that("domínio não corta o desenho, e proporção é média de indicador", {
  f <- ex("fazendas")
  a <- tr_sampling_srs(f, n = 300L, .seed = 2L)
  p <- tr_sampling_proportion(a, "irrigada", "sim")$tabela
  expect_equal(p$estimativa, mean(a$dados$irrigada == "sim"))
  todas <- tr_sampling_proportion(a, "irrigada")$tabela
  expect_equal(sum(todas$estimativa), 1)
  dm <- tr_sampling_mean(a, "producao_t", por = "irrigada")$tabela
  expect_equal(dm$irrigada, c("não", "sim"))
  # Domínio numa AAS: a variância da razão, que é maior que a do filtro ingênuo
  # com n do domínio fixo.
  sim <- a$dados$irrigada == "sim"
  y <- a$dados$producao_t
  nd <- sum(sim); n <- 300
  ybar <- mean(y[sim])
  z <- ifelse(sim, y - ybar, 0) / nd
  v <- (1 - n / 2400) * n / (n - 1) * sum((z - mean(z))^2)
  expect_equal(dm$erro_padrao[dm$irrigada == "sim"], sqrt(v))
  expect_error(tr_sampling_proportion(a, "irrigada", "talvez"), class = "tr_sampling_error_bad_option")
  expect_error(tr_sampling_mean(a, "irrigada"), class = "tr_sampling_error_not_numeric")
})

test_that("razão, faltantes à vista e estrato com uma UPA", {
  d <- ex("domicilios")
  s <- tr_sampling_design(d, pesos = "peso", estrato = "estrato", conglomerado = "setor", populacao = "setores_estrato")
  r <- tr_sampling_ratio(s, "renda", "moradores")$tabela
  expect_equal(r$estimativa, sum(d$peso * d$renda) / sum(d$peso * d$moradores))
  expect_true(is.na(r$deff))
  d$renda[1:3] <- NA
  s2 <- tr_sampling_design(d, pesos = "peso", estrato = "estrato", conglomerado = "setor")
  m <- tr_sampling_mean(s2, "renda")
  expect_match(m$nota, "3 linha(s) com faltante", fixed = TRUE)
  expect_equal(m$tabela$n, 417L)
  um <- d[d$setor %in% c(unique(d$setor[d$estrato == "rural"])[1], unique(d$setor[d$estrato == "urbano"])), ]
  s3 <- tr_sampling_design(um, pesos = "peso", estrato = "estrato", conglomerado = "setor")
  expect_error(tr_sampling_mean(s3, "moradores"), class = "tr_sampling_error_lonely_psu")
})

test_that("pós-estratificar reduz o erro quando o pós-estrato explica a variável", {
  f <- ex("fazendas")
  a <- tr_sampling_srs(f, n = 200L, .seed = 5L)
  p <- tr_sampling_poststratify(a, ex("estratos_fazendas"), "regiao", "N")
  expect_lt(tr_sampling_total(p, "producao_t")$tabela$erro_padrao, tr_sampling_total(a, "producao_t")$tabela$erro_padrao)
  # O total da variável que define o pós-estrato sai exato: erro zero.
  p$dados$um <- 1
  expect_equal(tr_sampling_total(p, "um")$tabela$estimativa, 2400)
  expect_equal(tr_sampling_total(p, "um")$tabela$erro_padrao, 0, tolerance = 1e-8)
})

test_that("o gráfico das estimativas sai, com proporção em %", {
  s <- tr_sampling_stratified(ex("fazendas"), "regiao", n = 200L)
  expect_s3_class(tr_sampling_plot_estimates(tr_sampling_proportion(s, "irrigada", por = "regiao")), "ggplot")
  expect_s3_class(tr_sampling_plot_estimates(tr_sampling_mean(s, "producao_t")), "ggplot")
})
