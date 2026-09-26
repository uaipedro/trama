test_that("margem para n dado é o inverso do tamanho", {
  m <- tr_sampling_margin(n = 25)
  expect_equal(m$erro, stats::qt(0.975, 24) * 0.5 / 5)
  expect_equal(tr_sampling_margin(n = 25, distribuicao = "z")$erro, stats::qnorm(0.975) * 0.5 / 5)
  expect_equal(tr_sampling_margin(n = 385, distribuicao = "z")$erro, 0.05, tolerance = 0.002)
  d <- tr_sampling_margin(n = 400, deff = 2, taxa_resposta = 0.5)
  expect_equal(d$erro, stats::qt(0.975, 199) * sqrt(2 * 0.25 / 200))
  expect_equal(utils::tail(d$passos$unidade, 1), "pontos")
  expect_error(tr_sampling_margin(n = 1), class = "tr_sampling_error_bad_option")
})

u <- tibble::tibble(cap = c("A", "B", "C", "D"), reg = c("X", "X", "Y", "Y"),
                    pop = c(1e6, 1e6, 9e6, 1e6), n = c(25, 25, 25, 25))

test_that("margem por nível: unidade é AAS, agregado paga o deff de Kish", {
  t <- tr_sampling_margin_levels(u, "cap", "pop", "n", "reg")
  expect_equal(t$nivel_tipo, c("total", "reg", "reg", "unidade", "unidade", "unidade", "unidade"))
  a <- t[t$nivel == "A", ]
  # t com gl = UPAs − estratos: 25 − 1 na unidade, 50 − 2 no agregado.
  expect_equal(a$gl, 24)
  expect_equal(a$margem, stats::qt(0.975, 24) * sqrt(0.25 / 25 * (1 - 25 / 1e6)))
  x <- t[t$nivel == "X", ]
  expect_equal(x$deff_ponderacao, 1)  # populações iguais, n iguais: autoponderada
  y <- t[t$nivel == "Y", ]
  W <- c(0.9, 0.1)
  expect_equal(y$deff_ponderacao, 50 * sum(W^2 / 25))
  expect_equal(y$margem, stats::qt(0.975, 48) * sqrt(sum(W^2 * (1 - 25 / c(9e6, 1e6)) * 0.25 / 25)))
  expect_gt(y$margem, x$margem)
  expect_equal(nrow(tr_sampling_margin_levels(u, "cap", "pop", "n")), 5L)
  u2 <- u; u2$n[[1]] <- 1
  expect_error(tr_sampling_margin_levels(u2, "cap", "pop", "n"), class = "tr_sampling_error_too_few")
})

test_that("indicação: k = 0 é a base, e o ICC come o ganho", {
  r <- tr_sampling_referral(u, "cap", "pop", "n", "reg", convidados = "0, 3", icc = "0, 0.2")
  base <- tr_sampling_margin_levels(u, "cap", "pop", "n", "reg")
  k0 <- r[r$convidados == 0 & r$icc == 0.2, ]
  expect_equal(k0$margem, base$margem)
  tot <- r[r$nivel == "Total" & r$convidados == 3, ]
  expect_equal(tot$deff_agrupamento, c(1, 1.6))
  # ICC 0: o n efetivo quadruplica, e a margem cai à metade (a menos da fpc).
  expect_equal(tot$margem[tot$icc == 0], base$margem[1] / 2, tolerance = 1e-4)
  expect_gt(tot$margem[tot$icc == 0.2], tot$margem[tot$icc == 0])
  expect_error(tr_sampling_referral(u, "cap", "pop", "n", icc = "0,1; 2"), class = "tr_sampling_error_bad_option")
  expect_s3_class(tr_sampling_plot_margins(r), "ggplot")
  expect_s3_class(tr_sampling_plot_margins(base), "ggplot")
})

comp <- tibble::tibble(variavel = c("sexo", "sexo", "cor", "cor", "cor"),
                       grupo = c("m", "h", "a", "b", "c"),
                       participacao = c(0.5, 0.5, 0.6, 0.35, 0.05))

test_that("tamanho por grupos: o menor grupo manda", {
  p <- tr_sampling_size_domains(comp, "variavel", "grupo", "participacao", distribuicao = "z")
  expect_equal(p$n, as.integer(ceiling(385 / 0.05)))
  # Com t (gl = n_g − 1), o n por grupo é o menor n com t_{n−1}·√(0,25/n) ≤ 0,05: 387.
  pt <- tr_sampling_size_domains(comp, "variavel", "grupo", "participacao")
  ng <- pt$alocacao$n[[1]]
  expect_lte(stats::qt(0.975, ng - 1) * sqrt(0.25 / ng), 0.05)
  expect_gt(stats::qt(0.975, ng - 2) * sqrt(0.25 / (ng - 1)), 0.05)
  expect_true(p$alocacao$limitante[p$alocacao$grupo == "c"])
  p2 <- tr_sampling_size_domains(comp, "variavel", "grupo", "participacao", participacao_minima = 0.1,
                                  distribuicao = "z")
  expect_equal(p2$n, as.integer(ceiling(385 / 0.35)))
  expect_match(p2$nota, "cor: c", fixed = TRUE)
  expect_equal(tr_sampling_size_domains(comp, "variavel", "grupo", "participacao", taxa_resposta = 0.5,
                                         distribuicao = "z")$n,
               as.integer(2 * ceiling(385 / 0.05)))
  ruim <- comp; ruim$participacao[[1]] <- 0.7
  expect_error(tr_sampling_size_domains(ruim, "variavel", "grupo", "participacao"), class = "tr_sampling_error_bad_option")
  pv <- sampling_plan_type()$preview(p, ctx_tmp())
  expect_no_error(jsonlite::toJSON(pv$data, auto_unbox = TRUE, null = "null"))
})

test_that("diferença detectável segue a fórmula e marca o que se sustenta", {
  t <- tr_sampling_detectable_difference(comp, "variavel", "grupo", "participacao", n_total = 1000,
                                         distribuicao = "z")
  expect_equal(nrow(t), 4L)
  mh <- t[t$variavel == "sexo", ]
  # n iguais, z: a equação de Fleiss é a de stats::power.prop.test.
  p2 <- stats::power.prop.test(n = 500, p1 = 0.5, power = 0.8, sig.level = 0.05, tol = 1e-12)$p2
  expect_equal(mh$diferenca_detectavel_pp, 100 * (p2 - 0.5), tolerance = 1e-8)
  expect_true(mh$sustentavel)
  expect_false(t$sustentavel[t$grupo_a == "a" & t$grupo_b == "c"])
})

test_that("raking bate todas as margens, e com uma variável é a pós-estratificação", {
  e <- ex("escolas")
  a <- tr_sampling_srs(e, n = 400L, .seed = 3L)
  perfil <- ex("perfil_escolas")
  r <- tr_sampling_rake(a, perfil)
  for (v in c("rede", "reprovado")) {
    alvo <- perfil[perfil$variavel == v, ]
    obs <- tapply(r$dados$peso_amostral, r$dados[[v]], sum)[alvo$categoria]
    expect_equal(as.numeric(obs), alvo$total, tolerance = 1e-5)
  }
  so_rede <- perfil[perfil$variavel == "rede", ]
  r1 <- tr_sampling_rake(a, so_rede)
  ps <- tr_sampling_poststratify(a, tibble::tibble(rede = so_rede$categoria, N = so_rede$total), "rede", "N")
  expect_equal(r1$dados$peso_amostral, ps$dados$peso_amostral, tolerance = 1e-8)
  expect_equal(tr_sampling_mean(r1, "nota")$tabela$erro_padrao, tr_sampling_mean(ps, "nota")$tabela$erro_padrao,
               tolerance = 1e-8)
  expect_length(r$receita$pos, 1L)
  expect_no_error(tr_sampling_simulate(r, "nota", repeticoes = 20L))
  ruim <- perfil; ruim$total[ruim$variavel == "rede"] <- ruim$total[ruim$variavel == "rede"] * 2
  expect_error(tr_sampling_rake(a, ruim), class = "tr_sampling_error_bad_option")
  expect_error(tr_sampling_rake(a, perfil, iteracoes = 1L), class = "tr_sampling_error_no_convergence")
})

test_that("margem por pergunta: pior caso por tipo, base e Thompson", {
  niveis <- tr_sampling_margin_levels(u, "cap", "pop", "n", "reg", distribuicao = "z")
  q <- ex("perguntas_exemplo")
  t <- tr_sampling_question_margins(q, niveis, distribuicao = "z")
  expect_equal(nrow(t), nrow(q) * nrow(niveis))
  a <- t[t$nivel == "A", ]
  z <- stats::qnorm(0.975)
  expect_equal(a$margem_pp[a$tipo == "binária"], 100 * z * sqrt(0.25 / 25))
  # Condicional com base 0,2: n de 5, margem √5 vezes pior e não confiável.
  cond <- a[a$base == 0.2, ]
  expect_equal(cond$margem_pp, 100 * z * sqrt(0.25 / 5))
  expect_false(cond$confiavel)
  expect_true(is.na(a$margem_pp[a$tipo == "aberta"]))
  # Thompson: com n = 510 a margem simultânea de pior caso é 5 pontos.
  n510 <- tibble::tibble(nivel = "x", n = 510, deff = 1)
  s <- tr_sampling_question_margins(tibble::tibble(pergunta = "p", tipo = "única", opcoes = 7, base = 1), n510,
                                    distribuicao = "z")
  expect_equal(s$margem_simultanea_pp, 5, tolerance = 0.01)
  expect_gt(s$margem_simultanea_pp, s$margem_pp)
  # A contra B: o dobro da margem de uma proporção; todos os 21 pares, Bonferroni.
  expect_equal(s$margem_diferenca_pp, 2 * s$margem_pp)
  expect_equal(s$margem_todos_pares_pp, 100 * stats::qnorm(1 - 0.05 / 42) * sqrt(1 / 510))
  expect_gt(s$margem_todos_pares_pp, s$margem_diferenca_pp)
  # A covariância negativa da multinomial é o que dobra: conferido por simulação.
  set.seed(20260914)
  sim <- stats::rmultinom(4000, 510, c(0.5, 0.5, rep(0, 5)) + c(-1e-9, -1e-9, rep(2e-9 / 5, 5)))
  expect_equal(stats::sd((sim[1, ] - sim[2, ]) / 510), sqrt(1 / 510), tolerance = 0.05)
  bin <- a[a$tipo == "binária", ]
  expect_true(is.na(bin$margem_diferenca_pp))
  e <- t[t$tipo == "escala" & t$nivel == "A", ]
  expect_equal(e$margem_media_escala, z * 2 / 5)
  r <- tr_sampling_referral(u, "cap", "pop", "n", convidados = "0, 2", icc = "0.1")
  expect_true(all(c("convidados", "icc") %in% names(tr_sampling_question_margins(q, r))))
  expect_error(tr_sampling_question_margins(transform(q, tipo = "sim/não"), niveis),
               class = "tr_sampling_error_bad_option")
})
