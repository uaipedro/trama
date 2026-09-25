test_that("n de Cochran para média e proporção, com os ajustes na ordem", {
  z <- stats::qnorm(0.975)
  p <- tr_sampling_size_proportion()
  expect_equal(p$parametros$n0, z^2 * 0.25 / 0.05^2)
  expect_equal(p$n, 385L)
  # O clássico das pesquisas de opinião: 3 pontos a 95% dá 1.068.
  expect_equal(tr_sampling_size_proportion(erro = 0.03)$n, 1068L)
  m <- tr_sampling_size_mean(desvio_padrao = 700, erro = 100, populacao = 2400, deff = 1.2, taxa_resposta = 0.8)
  n0 <- (z * 700 / 100)^2
  esperado <- ceiling((n0 * 1.2) / (1 + n0 * 1.2 / 2400) / 0.8)
  expect_equal(m$n, as.integer(esperado))
  expect_equal(nrow(m$passos), 5L)
  expect_equal(utils::tail(m$passos$valor, 1), esperado)
  # Sem ajustes, a escada tem só a fórmula e o arredondamento.
  expect_equal(nrow(tr_sampling_size_mean()$passos), 2L)
})

test_that("erro relativo usa a média, e o piloto dá o desvio e a média", {
  a <- tr_sampling_size_mean(desvio_padrao = 50, media = 500, erro = 10, tipo_erro = "relativo")
  b <- tr_sampling_size_mean(desvio_padrao = 50, erro = 50)
  expect_equal(a$n, b$n)
  expect_error(tr_sampling_size_mean(erro = 10, tipo_erro = "relativo"), class = "tr_sampling_error_bad_option")
  piloto <- tibble::tibble(x = c(10, 12, 9, 11, 13, NA))
  pl <- tr_sampling_size_mean(piloto, "x", erro = 1)
  expect_equal(pl$parametros$S, stats::sd(c(10, 12, 9, 11, 13)))
  expect_match(pl$nota, "piloto", fixed = TRUE)
  expect_error(tr_sampling_size_mean(piloto, "y"), class = "tr_sampling_error_unknown_column")
  expect_error(tr_sampling_size_mean(piloto), class = "tr_sampling_error_blank_param")
})

test_that("recusas de faixa no tamanho", {
  expect_error(tr_sampling_size_proportion(proporcao = 1), class = "tr_sampling_error_bad_option")
  expect_error(tr_sampling_size_proportion(erro = 0), class = "tr_sampling_error_bad_option")
  expect_error(tr_sampling_size_mean(confianca = 0.3), class = "tr_sampling_error_bad_option")
  expect_error(tr_sampling_size_mean(taxa_resposta = 0), class = "tr_sampling_error_bad_option")
})

test_that("alocação: proporcional, Neyman e as travas de mínimo, máximo e soma", {
  N <- c(300, 900, 600, 600); S <- c(1264, 365, 383, 736)
  expect_equal(.tr_sampling_alocar(N, S, 1, 240L, "proporcional"), c(30L, 90L, 60L, 60L))
  ney <- .tr_sampling_alocar(N, S, 1, 300L, "neyman")
  expect_equal(sum(ney), 300L)
  expect_equal(ney, as.integer(round(300 * N * S / sum(N * S))), tolerance = 1)
  expect_equal(.tr_sampling_alocar(N, S, 1, 100L, "igual"), c(25L, 25L, 25L, 25L))
  # Estrato pequeno com desvio enorme: o Neyman pediria mais que N_h, e o
  # excedente é redistribuído.
  a <- .tr_sampling_alocar(c(5, 1000), c(1000, 1), 1, 50L, "neyman")
  expect_equal(a, c(5L, 45L))
  # Estrato de desvio zero ainda recebe 2.
  expect_equal(.tr_sampling_alocar(c(100, 100), c(0, 10), 1, 20L, "neyman"), c(2L, 18L))
  expect_error(.tr_sampling_alocar(N, S, 1, 3000L, "proporcional"), class = "tr_sampling_error_too_large")
  expect_error(.tr_sampling_alocar(N, S, 1, 6L, "proporcional"), class = "tr_sampling_error_too_few")
})

test_that("tamanho estratificado alcança a margem pedida e a ótima corta o estrato caro", {
  e <- ex("estratos_fazendas")
  p <- tr_sampling_size_stratified(e, "regiao", "N", "desvio_producao", alocacao = "neyman", erro = 60)
  expect_lte(p$erro_alcancado, 60 + 1e-6)
  expect_equal(p$n, sum(p$alocacao$n_final))
  W <- e$N / sum(e$N)
  v <- sum(W^2 * (1 - p$alocacao$n / e$N) * e$desvio_producao^2 / p$alocacao$n)
  expect_equal(p$erro_alcancado, stats::qnorm(0.975) * sqrt(v))
  # A Neyman precisa de menos que a proporcional para a mesma margem.
  prop <- tr_sampling_size_stratified(e, "regiao", "N", "desvio_producao", alocacao = "proporcional", erro = 60)
  expect_lt(p$n, prop$n)
  ot <- tr_sampling_size_stratified(e, "regiao", "N", "desvio_producao", "custo", alocacao = "ótima", n_total = 300L)
  ney <- tr_sampling_size_stratified(e, "regiao", "N", "desvio_producao", alocacao = "neyman", n_total = 300L)
  expect_lt(ot$alocacao$n[ot$alocacao$estrato == "Norte"], ney$alocacao$n[ney$alocacao$estrato == "Norte"])
  expect_true(is.na(ney$erro))
  expect_error(tr_sampling_size_stratified(e, "regiao", "N", "desvio_producao", alocacao = "ótima", n_total = 300L),
               class = "tr_sampling_error_blank_param")
  expect_error(tr_sampling_size_stratified(e, "regiao", "N", "desvio_producao"), class = "tr_sampling_error_blank_param")
  r <- tr_sampling_size_stratified(e, "regiao", "N", "desvio_producao", n_total = 200L, taxa_resposta = 0.5)
  expect_equal(r$alocacao$n_final, pmin(e$N, ceiling(r$alocacao$n / 0.5)))
})

test_that("tamanho por conglomerados pelo ICC", {
  base <- tr_sampling_size_proportion()
  c <- tr_sampling_size_cluster(base, tamanho_conglomerado = 20, icc = 0.05)
  deff <- 1 + 19 * 0.05
  expect_equal(c$conglomerados, as.integer(ceiling(base$parametros$n0 * deff / 20)))
  expect_equal(c$tipo, "conglomerados")
  cM <- tr_sampling_size_cluster(base, 20, 0.05, conglomerados = 50)
  cg <- base$parametros$n0 * deff / 20
  expect_equal(cM$conglomerados, as.integer(ceiling(cg / (1 + cg / 50))))
  # ICC zero: é a AAS em pedaços.
  expect_equal(tr_sampling_size_cluster(base, 10, 0)$n, as.integer(10 * ceiling(base$parametros$n0 / 10)))
  expect_error(tr_sampling_size_cluster(c), class = "tr_sampling_error_plan_mismatch")
  expect_s3_class(tr_sampling_size_curve(base), "ggplot")
})

test_that("a curva inclui a confiança do plano, e o ponto do plano cai nela", {
  pl <- tr_sampling_size_proportion(erro = 0.05, confianca = 0.92)
  g <- tr_sampling_size_curve(pl)
  d <- g$data
  expect_setequal(levels(d$confianca), c("90%", "92%", "95%", "99%"))
  na_curva <- d[d$confianca == "92%" & abs(d$erro - pl$erro) < 1e-12, ]
  expect_equal(nrow(na_curva), 1L)
  expect_equal(ceiling(na_curva$n - 1e-9), pl$n)
  # Confiança que já é uma das fixas não duplica a curva.
  expect_length(levels(tr_sampling_size_curve(tr_sampling_size_proportion(erro = 0.05))$data$confianca), 3L)
})
