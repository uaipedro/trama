# Friedman (1937) para o DBC com uma observação por casela.

rounding_times <- function() {
  # Os dados do exemplo de ?stats::friedman.test (RoundingTimes, que o R
  # atribui a Hollander & Wolfe 1973): tempo para contornar a primeira base,
  # 22 jogadores (blocos) × 3 métodos. S = 11,14 (com a correção para empates)
  # e p = 0,0038 são valores CALCULADOS por nós nesses dados — não conferidos
  # na página do livro.
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

test_that("Friedman nos dados do exemplo do friedman.test (S = 11,14) e o stats::friedman.test", {
  d <- rounding_times()
  t <- tr_models_friedman(d, "tempo", "metodo", "jogador")
  expect_equal(round(t$estatistica, 2), 11.14)                    # calculado, com empates
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

test_that("Friedman: casela repetida recusa; com metodo = \"friedman\" bloco incompleto sai à vista", {
  d <- rounding_times()
  expect_error(tr_models_friedman(rbind(d, d[1, ]), "tempo", "metodo", "jogador"),
               class = "tr_models_error_not_applicable")
  d2 <- d; d2$tempo[[1]] <- NA
  t <- tr_models_friedman(d2, "tempo", "metodo", "jogador", metodo = "friedman")
  ref <- stats::friedman.test(tempo ~ metodo | jogador, data = d[d$jogador != 1, ])
  expect_equal(t$estatistica, unname(ref$statistic), tolerance = 1e-12)
  expect_match(t$nota, "1 bloco incompleto fora", fixed = TRUE)
  expect_error(tr_models_friedman(d[d$jogador == 1, ], "tempo", "metodo", "jogador", metodo = "friedman"),
               class = "tr_models_error_too_few_rows")
  expect_error(tr_models_friedman(d[d$metodo == "wide_angle", ], "tempo", "metodo", "jogador"),
               class = "tr_models_error_one_level")
})

test_that("Friedman no DBC de milho bate com o stats", {
  m <- ex("milho_dbc")
  t <- tr_models_friedman(m, "producao", "hibrido", "bloco")
  expect_equal(t$p_valor, stats::friedman.test(producao ~ hibrido | bloco, data = m)$p.value, tolerance = 1e-12)
})

# ---- Durbin (1951) e Skillings–Mack (1981): blocos incompletos ----

sorvete <- function(pref = c(2, 3, 1, 3, 1, 2, 2, 1, 3, 1, 2, 3, 3, 1, 2, 3, 1, 2, 3, 1, 2)) {
  # Sete variedades de sorvete, sete provadores, três por provador: o BIB
  # (t = 7, b = 7, k = 3, r = 3, λ = 1) do exemplo do `agricolae::durbin.test`,
  # que o atribui a Conover (1999, p. 391). T1 = 12 e p = 0,0620 são CALCULADOS
  # por nós (à mão e no agricolae) — não conferidos na página do livro.
  data.frame(pessoa = rep(1:7, each = 3),
             variedade = c(1, 2, 4, 2, 3, 5, 3, 4, 6, 4, 5, 7, 1, 5, 6, 2, 6, 7, 1, 3, 7),
             pref = pref)
}

test_that("Durbin no BIB do sorvete: T1 = 12 à mão e o agricolae::durbin.test", {
  d <- sorvete()
  t <- tr_models_friedman(d, "pref", "variedade", "pessoa")
  expect_equal(t$teste, "Durbin")
  expect_equal(t$estatistica, 12)              # R_j = 8,9,4,3,5,6,7; A − C = 98 − 84
  expect_equal(t$gl, "6")
  expect_equal(t$p_valor, stats::pchisq(12, 6, lower.tail = FALSE), tolerance = 1e-12)
  expect_match(t$nota, "teste de Durbin", fixed = TRUE)
  ref <- agricolae::durbin.test(d$pessoa, d$variedade, d$pref, console = FALSE)$statistics
  expect_equal(t$estatistica, ref$chisq.value, tolerance = 1e-12)
  expect_equal(t$p_valor, ref$p.value, tolerance = 1e-12)
  expect_identical(tr_models_friedman(d, "pref", "variedade", "pessoa", metodo = "durbin")$estatistica, 12)
})

test_that("Durbin com empates: A = Σ posto² corrige, igual ao agricolae", {
  d <- sorvete(c(1, 1, 2, 3, 1, 2, 2, 2, 3, 1, 2, 3, 3, 1, 2, 3, 1, 1, 3, 1, 2))
  t <- tr_models_friedman(d, "pref", "variedade", "pessoa")
  ref <- agricolae::durbin.test(d$pessoa, d$variedade, d$pref, console = FALSE)$statistics
  expect_equal(t$estatistica, ref$chisq.value, tolerance = 1e-12)
  expect_equal(t$p_valor, ref$p.value, tolerance = 1e-12)
  expect_match(t$nota, "empates", fixed = TRUE)
})

test_that("Durbin recusa desenho que não é BIB e aponta o Skillings–Mack", {
  d <- sorvete()[-1, ]
  expect_error(tr_models_friedman(d, "pref", "variedade", "pessoa", metodo = "durbin"),
               class = "tr_models_error_not_applicable", regexp = "skillings_mack")
  expect_error(tr_models_friedman(rounding_times(), "tempo", "metodo", "jogador", metodo = "durbin"),
               class = "tr_models_error_not_applicable")
  expect_equal(tr_models_friedman(d, "pref", "variedade", "pessoa")$teste, "Skillings–Mack")
})

montagem <- function() {
  # Quatro métodos de montagem em nove blocos com faltantes: o exemplo do
  # `?Skillings.Mack::Ski.Mack`. SM = 15,493 com 3 gl é o que o pacote calcula.
  data.frame(bloco = rep(1:9, each = 4), metodo = rep(c("A", "B", "C", "D"), 9),
             y = c(3.2, 4.1, 3.8, 4.2, 3.1, 3.9, 3.4, 4.0, 4.3, 3.5, 4.6, 4.8, 3.5, 3.6, 3.9, 4.0,
                   3.6, 4.2, 3.7, 3.9, 4.5, 4.7, 3.7, NA, NA, 4.2, 3.4, NA, 4.3, 4.6, 4.4, 4.9,
                   3.5, NA, 3.7, 3.9))
}

sm_ref <- function(d, resp, trat, blc) {
  m <- tapply(d[[resp]], list(factor(d[[trat]]), factor(d[[blc]])), identity)
  utils::capture.output(r <- Skillings.Mack::Ski.Mack(m))
  a <- r$adjustedSum
  drop(a %*% MASS::ginv(r$varCovarMatrix) %*% t(a))
}

test_that("Skillings–Mack no exemplo do pacote Skillings.Mack (SM = 15,493, 3 gl)", {
  d <- montagem()
  t <- tr_models_friedman(d, "y", "metodo", "bloco")
  expect_equal(t$teste, "Skillings–Mack")
  expect_equal(round(t$estatistica, 3), 15.493)
  expect_equal(t$gl, "3")
  expect_equal(t$p_valor, stats::pchisq(t$estatistica, 3, lower.tail = FALSE), tolerance = 1e-12)
  skip_if_not_installed("Skillings.Mack")
  expect_equal(t$estatistica, sm_ref(d, "y", "metodo", "bloco"), tolerance = 1e-8)
})

test_that("Skillings–Mack: igual ao Friedman em dados completos sem empate (propriedade)", {
  set.seed(20260925)
  for (i in 1:20) {
    k <- sample(3:6, 1); b <- sample(3:10, 1)
    d <- data.frame(bl = rep(seq_len(b), each = k), tr = rep(LETTERS[seq_len(k)], b), y = stats::rnorm(k * b))
    sm <- tr_models_friedman(d, "y", "tr", "bl", metodo = "skillings_mack")
    fr <- stats::friedman.test(y ~ tr | bl, data = d)
    expect_equal(sm$estatistica, unname(fr$statistic), tolerance = 1e-10)
    expect_equal(sm$gl, as.character(k - 1))
  }
})

test_that("Skillings–Mack com faltantes ao acaso bate com o pacote (1e-8)", {
  skip_if_not_installed("Skillings.Mack")
  set.seed(7)
  for (i in 1:15) {
    k <- 5; b <- 8
    d <- data.frame(bl = rep(seq_len(b), each = k), tr = rep(letters[1:k], b),
                    y = round(stats::rnorm(k * b), 1))         # arredondado: há empates
    d$y[sample(nrow(d), 6)] <- NA
    cont <- table(d$bl[!is.na(d$y)])
    if (any(cont < 2)) next
    t <- tr_models_friedman(d, "y", "tr", "bl", metodo = "skillings_mack")
    expect_equal(t$estatistica, sm_ref(d, "y", "tr", "bl"), tolerance = 1e-8)
  }
})

test_that("Skillings–Mack: bloco de uma observação sai; desenho desconexo recusa", {
  d <- montagem()
  d2 <- rbind(d, data.frame(bloco = 10, metodo = "A", y = 1))
  t <- tr_models_friedman(d2, "y", "metodo", "bloco")
  expect_equal(t$estatistica, tr_models_friedman(d, "y", "metodo", "bloco")$estatistica)
  expect_match(t$nota, "1 bloco com uma observação só fora", fixed = TRUE)
  desc <- data.frame(bl = rep(1:4, each = 2), tr = c("A", "B", "A", "B", "C", "D", "C", "D"), y = c(1, 2, 2, 1, 1, 2, 1, 2))
  expect_error(tr_models_friedman(desc, "y", "tr", "bl"), class = "tr_models_error_not_applicable")
  expect_error(tr_models_friedman(d, "y", "metodo", "bloco", metodo = "outro"),
               class = "tr_models_error_bad_option")
})
