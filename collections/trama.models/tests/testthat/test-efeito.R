test_that("effect_size: eta², eta² parcial e ômega² pelas fórmulas, sobre o quadro", {
  m <- milho_dbc()
  q <- as.data.frame(tr_models_anova_table(m)$tabela)
  e <- tr_models_effect_size(m)
  expect_equal(e$termo, c("bloco", "hibrido"))
  sq <- q$sq[1:2]; gl <- q$gl[1:2]
  sqe <- q$sq[q$termo == "Resíduo"]; qme <- q$qm[q$termo == "Resíduo"]; tot <- q$sq[q$termo == "Total"]
  expect_equal(e$eta2, sq / tot)
  expect_equal(e$eta2_parcial, sq / (sq + sqe))
  expect_equal(e$omega2, (sq - gl * qme) / (tot + qme))
  # À mão, do quadro impresso: híbrido SQ 5,16147, resíduo SQ 1,11717 (QM
  # 0,0930975, 12 gl), total 7,91092.
  expect_equal(e$eta2[[2]], 5.16147 / 7.91092, tolerance = 1e-5)
  expect_equal(e$eta2_parcial[[2]], 5.16147 / (5.16147 + 1.11717), tolerance = 1e-5)
  expect_equal(e$omega2[[2]], (5.16147 - 4 * 0.0930975) / (7.91092 + 0.0930975), tolerance = 1e-5)
  # DIC de um fator: eta² = eta² parcial.
  d <- tr_models_effect_size(tr_models_anova_dic(ex("PlantGrowth"), "weight", "group"))
  expect_equal(d$eta2, d$eta2_parcial)
})

test_that("effect_size: cada termo com o seu erro na subdividida; recusa GLM", {
  sp <- tr_models_anova_split_plot(ex("aveia"), "producao", "variedade", "nitrogenio", "bloco")
  e <- tr_models_effect_size(sp)
  expect_equal(e$erro, c("Resíduo (a)", "Resíduo (a)", "Resíduo (b)", "Resíduo (b)"))
  q <- as.data.frame(tr_models_anova_table(sp)$tabela)
  expect_equal(e$eta2_parcial[e$termo == "nitrogenio"],
               q$sq[q$termo == "nitrogenio"] / (q$sq[q$termo == "nitrogenio"] + q$sq[q$termo == "Resíduo (b)"]))
  # F < 1 dá ômega² negativo, e a ajuda manda ler como zero.
  expect_lt(e$omega2[e$termo == "variedade:nitrogenio"], 0)
  expect_error(tr_models_effect_size(tr_models_glm(ex("InsectSprays"), "count", "spray")),
               class = "tr_models_error_not_applicable")
})

test_that("cohen_d: d, g e intervalo pelas fórmulas; d = t de Student · sqrt(1/n1 + 1/n2)", {
  a <- c(5, 7, 8, 6, 9); b <- c(3, 4, 6, 2, 5, 4)
  d0 <- data.frame(y = c(a, b), g = rep(c("A", "B"), c(5, 6)))
  r <- tr_models_cohen_d(d0, "y", "g", confianca = 0.9)
  sp <- sqrt((4 * var(a) + 5 * var(b)) / 9)
  d <- (mean(a) - mean(b)) / sp
  ep <- sqrt(11 / 30 + d^2 / 22)
  j <- 1 - 3 / (4 * 11 - 9)
  expect_equal(r$estimativa, c(d, j * d))
  expect_equal(r$li, c(d - qnorm(.95) * ep, j * (d - qnorm(.95) * ep)))
  expect_equal(r$ls, c(d + qnorm(.95) * ep, j * (d + qnorm(.95) * ep)))
  expect_equal(r$n1, c(5L, 5L)); expect_equal(r$n2, c(6L, 6L))
  # Identidade com o t de variâncias iguais: o mesmo DP combinado, outra escala.
  tg <- ex("ToothGrowth")
  t <- tr_models_cohen_d(tg, "len", "supp")
  st <- t.test(len ~ supp, data = tg, var.equal = TRUE)$statistic
  expect_equal(t$estimativa[[1]], unname(st) * sqrt(1 / 30 + 1 / 30))
  expect_equal(round(c(t$estimativa[[1]], t$li[[1]], t$ls[[1]]), 2), c(0.49, -0.02, 1.01))
  expect_error(tr_models_cohen_d(ex("PlantGrowth"), "weight", "group"), class = "tr_models_error_two_groups")
})

test_that("plot_coefficients: sem intercepto, referência 0 ou 1, ordem e nível", {
  g <- tr_models_glm(ex("mtcars"), formula = "am ~ wt + hp", familia = "binomial")
  p <- tr_models_plot_coefficients(g)
  expect_equal(rev(levels(p$data$termo)), c("wt", "hp"))
  expect_equal(p$layers[[1]]$data$xintercept %||% p$layers[[1]]$aes_params$xintercept %||% 0, 0)
  e <- tr_models_plot_coefficients(g, exponenciar = TRUE, confianca = 0.9, ordenar = "estimativa")
  cf <- tr_models_coefs(g, exponenciar = TRUE, confianca = 0.9)$tabela
  expect_equal(sort(e$data$li), sort(cf$li_90[cf$termo != "(Intercept)"]))
  expect_equal(rev(levels(e$data$termo)), c("wt", "hp")[order(-cf$estimativa[-1])])
  expect_no_error(ggplot2::ggplot_build(e))
  expect_error(tr_models_plot_coefficients(g, ordenar = "alfabética"))
  expect_error(tr_models_plot_coefficients(tr_models_lm(ex("mtcars"), formula = "mpg ~ 1")),
               class = "tr_models_error_not_applicable")
  expect_error(tr_models_plot_coefficients(tr_models_lm(ex("mtcars"), formula = "mpg ~ wt"), exponenciar = TRUE),
               class = "tr_models_error_not_applicable")
})
