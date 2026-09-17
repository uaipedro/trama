# O nó com memória de verdade da coleção (Tarefa 7.2), e o caso que motivou a
# região de fluxo inteira, agora rodando (Tarefa 7.3).

test_that("rls converge para os coeficientes exatos de lm()", {
  set.seed(1); d <- data.frame(x = rnorm(200)); d$y <- 2 + 3 * d$x + rnorm(200, sd = 0.1)
  st <- init_rls(lambda = 1e6)   # prior difusa
  for (i in seq_len(nrow(d))) st <- step_rls(st, d[i, ])$state
  expect_equal(unname(coef_rls(st)), unname(coef(lm(y ~ x, d))), tolerance = 1e-8)
})

test_that("o estado sobrevive a saveRDS/readRDS — é isso que o checkpoint faz", {
  set.seed(1); d <- data.frame(x = rnorm(50)); d$y <- 1 - 2 * d$x + rnorm(50, sd = 0.2)
  st <- init_rls(lambda = 1e4)
  for (i in seq_len(25)) st <- step_rls(st, d[i, ])$state
  arq <- tempfile(fileext = ".rds")
  saveRDS(st, arq)
  st2 <- readRDS(arq)
  expect_identical(st, st2)
  # E o estado restaurado continua a atualização exatamente de onde parou: os
  # 25 pontos restantes, aplicados aos dois estados, chegam ao mesmo lugar.
  for (i in 26:50) { st <- step_rls(st, d[i, ])$state; st2 <- step_rls(st2, d[i, ])$state }
  expect_identical(st, st2)
})

test_that("P não degenera em 1000 passos: continua simétrica e positiva definida", {
  set.seed(2); d <- data.frame(x = rnorm(1000)); d$y <- 1 - 0.5 * d$x + rnorm(1000, sd = 1)
  st <- init_rls(lambda = 1e4)
  for (i in seq_len(nrow(d))) st <- step_rls(st, d[i, ])$state
  expect_equal(st$P, t(st$P))  # simétrica, exatamente — é o que a simetrização garante
  expect_true(all(eigen(st$P, symmetric = TRUE, only.values = TRUE)$values > 0))
})

test_that("preditores em branco usa as demais colunas do ponto, na ordem em que chegam", {
  st <- init_rls(lambda = 1e4)  # preditores = "", resposta = "y" (default)
  r <- step_rls(st, data.frame(x1 = 1, x2 = 2, y = 10))
  expect_equal(r$state$preditores, c("x1", "x2"))
})

test_that("coluna que falta no ponto erra alto, nomeando a coluna", {
  st <- init_rls(preditores = "x", resposta = "y", lambda = 1e4)
  e <- expect_error(step_rls(st, data.frame(x = 1)), class = "tr_models_error_unknown_column")
  expect_match(conditionMessage(e), "y", fixed = TRUE)
})

test_that("'out' não tem coluna 'passo' — quem indexa é data/from_stream", {
  st <- init_rls(lambda = 1e4)
  r <- step_rls(st, data.frame(x = 1, y = 2))
  expect_false("passo" %in% names(r$out))
  expect_equal(names(r$out), c("previsto", "real", "residuo"))
})

# ---- Tarefa 7.3: o caso de validação do desenho, de ponta a ponta ----------
#
# models/fit (FORA da região) + data/to_stream -> models/predict -> from_stream,
# e a última linha é a tese honesta do desenho posta à prova: com um modelo
# ESTÁTICO, prever ponto a ponto tem que dar exatamente o mesmo que prever em
# lote. Se divergir, é bug no driver — não no nó.
test_that("o caso do desenho roda: modelo fora da região, predict elevado ponto a ponto", {
  reg <- models_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("carros", "models/example", dataset = "mtcars") |>
    trama::tr_add("reg", "models/lm", formula = "mpg ~ wt + hp", from = "carros") |>
    trama::tr_add("novos", "models/example", dataset = "mtcars") |>
    trama::tr_add("entra", "data/to_stream", lote = 1L, from = "novos") |>
    trama::tr_add("prever", "models/predict", from = c("reg", "entra")) |>
    trama::tr_add("sai", "data/from_stream", from = "prever")

  hist <- rodar(f, "sai")
  mt <- ex("mtcars")
  expect_equal(nrow(hist), nrow(mt))  # uma linha por ponto

  m <- tr_models_lm(mt, formula = "mpg ~ wt + hp")
  lote <- tr_models_predict(m, mt)

  # Idêntico ao predict em LOTE sobre a tabela inteira — não "próximo": o
  # driver não pode introduzir diferença nenhuma num nó puro e estático.
  expect_identical(hist$previsto, lote$previsto)
})

test_that("o modelo entra na região como input comum: a mesma referência em todo passo", {
  # Prova indireta da propriedade da Fase 4 ("external, lido uma vez"): se o
  # driver relesse o artefato a cada passo, um modelo IMPURO acusaria — mas
  # models/lm é puro, então o teste que importa aqui é o de identidade acima:
  # com leitura única ou N leituras do MESMO artefato imutável, o resultado é
  # o mesmo. O que se confere É a coisa observável desta tarefa: que a região
  # aceita `modelo` como porta comum (não-stream) sem recusa nenhuma das cinco
  # (Fase 1) — a montagem do fluxo acima não abortou, e é essa a evidência.
  reg <- models_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("carros", "models/example", dataset = "mtcars") |>
    trama::tr_add("reg", "models/lm", formula = "mpg ~ wt", from = "carros") |>
    trama::tr_add("novos", "models/example", dataset = "mtcars") |>
    trama::tr_add("entra", "data/to_stream", lote = 1L, from = "novos") |>
    trama::tr_add("prever", "models/predict", from = c("reg", "entra")) |>
    trama::tr_add("sai", "data/from_stream", from = "prever")
  expect_true(is.data.frame(rodar(f, "sai")))
})
