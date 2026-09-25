# Friedman (1937) para o DBC com uma observação por casela.

rounding_times <- function() {
  # Hollander & Wolfe (1973, p. 140): tempo para contornar a primeira base,
  # 22 jogadores (blocos) × 3 métodos. O livro dá S = 11,14 (com a correção
  # para empates), p = 0,0038 — o mesmo exemplo de ?friedman.test.
  m <- matrix(c(5.40, 5.50, 5.55, 5.85, 5.70, 5.75, 5.20, 5.60, 5.50, 5.55, 5.50, 5.40,
                5.90, 5.85, 5.70, 5.45, 5.55, 5.60, 5.40, 5.40, 5.35, 5.45, 5.50, 5.35,
                5.25, 5.15, 5.00, 5.85, 5.80, 5.70, 5.25, 5.20, 5.10, 5.65, 5.55, 5.45,
                5.60, 5.35, 5.45, 5.05, 5.00, 4.95, 5.50, 5.50, 5.40, 5.45, 5.55, 5.50,
                5.55, 5.55, 5.35, 5.45, 5.50, 5.55, 5.50, 5.45, 5.25, 5.65, 5.60, 5.40,
                5.70, 5.65, 5.55, 6.30, 6.30, 6.25), ncol = 3, byrow = TRUE)
  data.frame(jogador = rep(seq_len(22), 3),
             metodo = rep(c("round_out", "narrow_angle", "wide_angle"), each = 22),
             tempo = as.vector(m))
}

test_that("Friedman reproduz Hollander & Wolfe (S = 11,14) e o stats::friedman.test", {
  d <- rounding_times()
  t <- tr_models_friedman(d, "tempo", "metodo", "jogador")
  expect_equal(round(t$estatistica, 2), 11.14)                    # publicado
  ref <- stats::friedman.test(tempo ~ metodo | jogador, data = d)
  expect_equal(t$estatistica, unname(ref$statistic), tolerance = 1e-12)
  expect_equal(t$p_valor, ref$p.value, tolerance = 1e-12)
  expect_equal(t$gl, "2")
  # À mão: postos dentro do bloco, Fr com a correção para empates.
  r <- t(apply(matrix(d$tempo, ncol = 3), 1, rank))
  b <- 22; k <- 3
  fr <- 12 / (b * k * (k + 1)) * sum(colSums(r)^2) - 3 * b * (k + 1)
  emp <- sum(apply(matrix(d$tempo, ncol = 3), 1, function(x) { tt <- table(x); sum(tt^3 - tt) }))
  expect_equal(t$estatistica, fr / (1 - emp / (b * k * (k^2 - 1))), tolerance = 1e-12)
  # W de Kendall = Fr / (b (k - 1))
  expect_equal(t$efeito$valor, t$estatistica / (b * (k - 1)), tolerance = 1e-12)
  expect_match(t$nota, "empate", fixed = TRUE)
})

test_that("Friedman: casela repetida recusa; bloco incompleto sai à vista", {
  d <- rounding_times()
  expect_error(tr_models_friedman(rbind(d, d[1, ]), "tempo", "metodo", "jogador"),
               class = "tr_models_error_not_applicable")
  d2 <- d; d2$tempo[[1]] <- NA
  t <- tr_models_friedman(d2, "tempo", "metodo", "jogador")
  ref <- stats::friedman.test(tempo ~ metodo | jogador, data = d[d$jogador != 1, ])
  expect_equal(t$estatistica, unname(ref$statistic), tolerance = 1e-12)
  expect_match(t$nota, "1 bloco incompleto fora", fixed = TRUE)
  expect_error(tr_models_friedman(d[d$jogador == 1, ], "tempo", "metodo", "jogador"),
               class = "tr_models_error_too_few_rows")
  expect_error(tr_models_friedman(d[d$metodo == "wide_angle", ], "tempo", "metodo", "jogador"),
               class = "tr_models_error_one_level")
})

test_that("Friedman no DBC de milho bate com o stats", {
  m <- ex("milho_dbc")
  t <- tr_models_friedman(m, "producao", "hibrido", "bloco")
  expect_equal(t$p_valor, stats::friedman.test(producao ~ hibrido | bloco, data = m)$p.value, tolerance = 1e-12)
})
