test_that("KMO, MSA e Bartlett batem com o psych", {
  skip_if_not_installed("psych")
  h <- tr_multi_example("harman_24_testes")
  k <- tr_multi_kmo_bartlett(h)
  expect_equal(names(k), c("medida", "variavel", "valor", "gl", "p_valor", "leitura"))
  expect_equal(k$medida, c("KMO global", rep("MSA", 24), "Bartlett"))
  ref <- psych::KMO(stats::cor(h))
  expect_equal(k$valor[[1]], ref$MSA, tolerance = 1e-10)
  expect_equal(k$valor[2:25], unname(ref$MSAi), tolerance = 1e-10)
  expect_equal(k$variavel[2:25], names(h))
  b <- suppressWarnings(psych::cortest.bartlett(stats::cor(h), n = 145))
  expect_equal(k$valor[[26]], b$chisq, tolerance = 1e-8)
  expect_equal(k$gl[[26]], b$df)
  expect_equal(k$p_valor[[26]], b$p.value, tolerance = 1e-8)
})

test_that("a leitura de Kaiser cai nas faixas certas", {
  expect_equal(.tr_multi_leitura_kmo(c(.95, .9, .85, .75, .65, .55, .49)),
               c("maravilhoso", "maravilhoso", "meritório", "mediano", "medíocre", "miserável",
                 "inaceitável"))
  k <- tr_multi_kmo_bartlett(tr_multi_example("harman_24_testes"))
  expect_equal(k$leitura[[1]], "meritório")
  expect_match(k$leitura[[26]], "^rejeita")
})

test_that("Bartlett não rejeita em variáveis independentes, e diz sem dizer que aceita", {
  set.seed(10)
  d <- as.data.frame(matrix(stats::rnorm(60 * 4), 60, 4))
  k <- tr_multi_kmo_bartlett(d)
  expect_gt(k$p_valor[[6]], .05)
  expect_match(k$leitura[[6]], "^não rejeita")
})

test_that("análise paralela: reprodutível pela semente, sem mexer no RNG do usuário", {
  q <- tr_multi_example("questionario")
  set.seed(5); antes <- stats::runif(1)
  set.seed(5)
  a <- tr_multi_parallel(q, .seed = 11L)
  expect_equal(stats::runif(1), antes)
  expect_identical(a, tr_multi_parallel(q, .seed = 11L))
  expect_false(identical(a$autovalor_aleatorio, tr_multi_parallel(q, .seed = 12L)$autovalor_aleatorio))
  expect_equal(names(a), c("posicao", "autovalor_observado", "autovalor_aleatorio", "reter"))
  expect_equal(a$autovalor_observado, eigen(stats::cor(q[-1]))$values)
})

test_that("análise paralela sugere 3 no questionário e 3 no Harman 24; só a sequência inicial conta", {
  expect_equal(sum(tr_multi_parallel(tr_multi_example("questionario"), .seed = 1L)$reter), 3L)
  # O livro usa 4 fatores; a paralela (percentil 95) acha 3 — o de memória é
  # fraco. A ajuda do nó conta isso.
  h <- tr_multi_parallel(tr_multi_example("harman_24_testes"), .seed = 1L)
  expect_equal(sum(h$reter), 3L)
  expect_equal(which(h$reter), seq_len(sum(h$reter)))
  expect_error(tr_multi_parallel(tr_multi_example("questionario"), percentil = 100L),
               class = "tr_multi_error_bad_option")
})

test_that("pelo motor, o nó recebe a semente do card", {
  reg <- multi_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("q", "multi/example", dataset = "questionario") |>
    trama::tr_add("pa", "multi/parallel", repeticoes = 20L, from = "q")
  a <- rodar(f, "pa")
  expect_s3_class(a, "tbl_df")
  expect_identical(a, rodar(f, "pa"))
})
