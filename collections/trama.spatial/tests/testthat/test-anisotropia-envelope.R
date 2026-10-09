# O envelope de referência sob ISOTROPIA.
#
# A hipótese nula certa é "isotrópico com esta estrutura de dependência", e não
# "sem estrutura espacial": reembaralhar os valores entre as posições destruiria
# toda a dependência e responderia a pergunta errada. Por isso o envelope simula
# do modelo ISOTRÓPICO ajustado, nas mesmas posições.
#
# E por isso o envelope depende do otimizador que ajustou esse modelo. Para o
# teste não depender dele — a lição que o mantenedor consertou em 946b37f —, o
# simulador entra por INJEÇÃO (`.simular`): os testes usam campos fixos, e o que
# fica testado é o cálculo da faixa (aritmética de quantis, oráculo à mão) mais
# a reprodutibilidade pela semente.
#
# Poder medido em 2026-10-09 (12 repetições, n = 200, 19 simulações): a fração
# do observado dentro da faixa deu média 0,952 sob isotropia e 0,838 sob
# anisotropia de razão 3 — distribuições que SE SOBREPÕEM. Logo a faixa é
# referência visual, não teste, e o card não exibe fração agregada.

test_that("a faixa é o mínimo e o máximo por direção e classe", {
  chaves <- data.frame(direcao = c(0, 0, 90, 90), u = c(10, 20, 10, 20))
  gammas <- cbind(c(1, 5, 2, 6),
                  c(3, 4, 1, 9),
                  c(2, 7, 4, 7))
  f <- .tr_spatial_faixa(gammas, chaves)
  expect_equal(f$inferior, c(1, 4, 1, 6))
  expect_equal(f$superior, c(3, 7, 4, 9))
  expect_equal(f$direcao, chaves$direcao)
  expect_equal(f$u, chaves$u)
})

test_that("a faixa ignora NA onde o par não existe, sem propagar", {
  chaves <- data.frame(direcao = c(0, 90), u = c(10, 10))
  gammas <- cbind(c(1, NA), c(2, 3))
  f <- .tr_spatial_faixa(gammas, chaves)
  expect_equal(f$inferior, c(1, 3))
  expect_equal(f$superior, c(2, 3))
})

test_that("com simulador injetado, o envelope sai dos campos dados", {
  p <- tr_spatial_example("milho_se")
  n <- nrow(p$dados)
  # Cinco campos fixos e diferentes entre si (cinco é o mínimo que o bloco
  # aceita): constante, rampas em x e y, e duas combinações.
  cx <- p$coords[, 1] / stats::sd(p$coords[, 1])
  cy <- p$coords[, 2] / stats::sd(p$coords[, 2])
  campos <- cbind(rep(1, n), cx, cy, cx + cy, cx - cy)
  a <- tr_spatial_anisotropy(p, direcoes = "0,90", envelope = TRUE, n_sim = 5L,
                             pares_min = 1L,
                             .simular = function(...) campos)
  expect_false(is.null(a$envelope))
  expect_true(all(c("direcao", "u", "inferior", "superior") %in% names(a$envelope)))
  expect_true(all(a$envelope$inferior <= a$envelope$superior))
  expect_equal(nrow(a$envelope), nrow(a$tabela))
  expect_equal(a$n_sim, 5L)
  # a faixa do campo CONSTANTE é zero, então o piso da faixa é zero
  expect_equal(min(a$envelope$inferior), 0, tolerance = 1e-12)
})

test_that("sem envelope o campo fica nulo e n_sim não é inventado", {
  a <- tr_spatial_anisotropy(tr_spatial_example("milho_se"), pares_min = 1L)
  expect_null(a$envelope)
  expect_true(is.na(a$n_sim))
})

test_that("a mesma semente dá o mesmo envelope, e sementes diferentes não", {
  p <- tr_spatial_example("milho_se")
  f <- function(s) tr_spatial_anisotropy(p, direcoes = "0,90", envelope = TRUE,
                                         n_sim = 5L, semente = s,
                                         pares_min = 1L)$envelope
  a <- f(42); b <- f(42); c <- f(7)
  expect_equal(a, b)
  expect_false(isTRUE(all.equal(a, c)))
})

test_that("n_sim fora do aceitável é recusado", {
  p <- tr_spatial_example("milho_se")
  expect_error(tr_spatial_anisotropy(p, envelope = TRUE, n_sim = 2L, pares_min = 1L),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_anisotropy(p, envelope = TRUE, n_sim = 5000L, pares_min = 1L),
               class = "tr_spatial_error_bad_option")
})

test_that("a faixa contém a própria mediana das simulações", {
  # Propriedade elementar: mínimo <= mediana <= máximo, por direção e classe.
  p <- tr_spatial_example("milho_se")
  a <- tr_spatial_anisotropy(p, direcoes = "0,90", envelope = TRUE, n_sim = 9L,
                             semente = 1, pares_min = 1L)
  expect_true(all(a$envelope$inferior <= a$envelope$superior))
  expect_true(all(is.finite(a$envelope$inferior)))
})

test_that("o envelope padrão fecha em tempo de card no maior exemplo", {
  # Sem `skip_on_cran()`: a coleção não vai ao CRAN, o teste custa ~2 s, e sob
  # `test_dir` o skip o desligava — um guarda de custo que não roda não guarda
  # nada.
  p <- tr_spatial_example("cafe_mg")      # 496 pontos, o maior dos três
  t <- system.time(tr_spatial_anisotropy(p, envelope = TRUE, n_sim = 19L,
                                         semente = 1, pares_min = 1L))[["elapsed"]]
  expect_lt(t, 30)   # folga larga sobre o 1,6 s medido com 389 pontos
})
