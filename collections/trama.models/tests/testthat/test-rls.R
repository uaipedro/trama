# O nó com memória de verdade da coleção (Tarefa 7.2), e o caso que motivou a
# região de fluxo inteira, agora rodando (Tarefa 7.3).

test_that("rls converge para os coeficientes exatos de lm()", {
  set.seed(1); d <- data.frame(x = rnorm(200)); d$y <- 2 + 3 * d$x + rnorm(200, sd = 0.1)
  st <- init_rls(lambda = 1e6)   # prior difusa
  for (i in seq_len(nrow(d))) st <- step_rls(st, d[i, ])$state
  expect_equal(unname(coef_rls(st)), unname(coef(lm(y ~ x, d))), tolerance = 1e-8)
})

# A ajuda dizia "EXATAMENTE", sem qualificar, e isso só vale no limite
# `lambda -> Inf`. Duas asserções aqui, com papéis diferentes, porque uma sem a
# outra engana:
#
# `expect_equal(st$lambda, 1e6)` é a guarda do DEFAULT. É ela que pega uma
# regressão silenciosa do valor — o bound numérico abaixo não pegaria: medido,
# um default de 1e5 erra 1.7e-7 e passaria folgado.
#
# `expect_lt(erro, 1e-6)` é a guarda da PROMESSA da ajuda: que o default chega
# perto o bastante. Margem medida em 20 sementes: erro máximo 1.8e-8, ou seja
# ~55x de folga, e com o default antigo (1e4) as 20 reprovam.
test_that("o default de lambda (1e6) chega perto o bastante de lm(), como a ajuda promete", {
  set.seed(1); d <- data.frame(x = rnorm(200)); d$y <- 2 + 3 * d$x + rnorm(200, sd = 0.1)
  st <- init_rls(resposta = "y", preditores = "x")   # lambda no default do card/fn
  expect_equal(st$lambda, 1e6)
  for (i in seq_len(nrow(d))) st <- step_rls(st, d[i, ])$state
  erro <- max(abs(unname(coef_rls(st)) - unname(coef(lm(y ~ x, d)))))
  expect_lt(erro, 1e-6)
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
  # Exatamente simétrica, e é a linha de simetrização que garante isso: sem
  # ela `identical(P, t(P))` é FALSE (assimetria medida entre 6e-18 e 1e-10,
  # pequena e não crescente). Ver o cabeçalho de `step_rls()`.
  expect_identical(st$P, t(st$P))
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
  # driver não pode introduzir diferença nenhuma num nó puro e estático. É
  # também a prova indireta da propriedade da Fase 4 ("external, lido uma
  # vez"): se o driver relesse o artefato a cada passo, um modelo IMPURO
  # acusaria — mas models/lm é puro, então leitura única ou N leituras do
  # MESMO artefato imutável dão o mesmo resultado, e é esse `expect_identical`
  # que mede isso, não um teste à parte.
  expect_identical(hist$previsto, lote$previsto)
})

# ---- Blocker B0: `step_rls()` recebendo um ponto de MAIS de uma linha ------
#
# Medido antes do fix (revertendo este arquivo): uma tabela de 12 linhas por
# `data/to_stream(lote = 3L)` -> `models/rls` -> `data/from_stream` devolvia
# histórico de 4 linhas — `real` igual a `y[1], y[4], y[7], y[10]` — porque
# `step_rls()` indexava `dados[1L, ]` e `dados[[resposta]][[1L]]`: as linhas 2
# e 3 de cada lote nunca eram vistas, sem erro nem aviso. Controle: o mesmo
# `lote = 3L` com `data/filter` (nó sem memória) processa as 12 linhas —
# então a perda era do CONTRATO do nó de memória, não do driver.
#
# Decisão: LAÇAR sobre as linhas do ponto (uma atualização de Sherman-Morrison
# por linha), em vez de recusar `nrow(dados) > 1L`. RLS é definido por
# OBSERVAÇÃO — laçar é a leitura honesta — e `models/rls` é o único nó de
# memória da coleção, então recusar lote > 1 tornaria esse parâmetro de
# `data/to_stream` inútil sempre que há memória de verdade no fluxo (a própria
# ajuda de `data/to_stream` recomenda lote > 1 para "quem precisa de mais de
# uma linha por cálculo"). A propriedade que valida a escolha: mesmos dados,
# entregues em lotes de 3 ou de 1, têm que chegar aos MESMOS coeficientes —
# é isso que os dois testes abaixo medem, em dois níveis.
test_that("step_rls() com um ponto de 3 linhas atualiza nas 3 — não só na primeira", {
  # Nível 1, sem flow: chamar step_rls() uma vez com 3 linhas tem que dar o
  # MESMO estado final que chamá-lo 3 vezes, uma linha por vez — é a prova de
  # que o laço interno faz exatamente as mesmas 3 atualizações de
  # Sherman-Morrison, na mesma ordem, e não synthesize resultado nenhum.
  set.seed(3)
  d <- data.frame(x = rnorm(12)); d$y <- 1 + 2 * d$x + rnorm(12, sd = 0.05)

  st_lote <- init_rls(lambda = 1e4)
  st_lote <- step_rls(st_lote, d[1:3, ])$state

  st_um <- init_rls(lambda = 1e4)
  for (i in 1:3) st_um <- step_rls(st_um, d[i, , drop = FALSE])$state

  expect_equal(st_lote$theta, st_um$theta)
  expect_equal(st_lote$P, st_um$P)
  expect_equal(st_lote$n, st_um$n)
  expect_equal(st_lote$n, 3L)

  # `out` tem uma linha POR LINHA do ponto, não uma linha por passo — é o que
  # deixa `data/from_stream` empilhar o histórico completo (ver o teste de
  # fluxo abaixo, que confere o `real` das 12 observações).
  r <- step_rls(init_rls(lambda = 1e4), d[1:3, ])
  expect_equal(nrow(r$out), 3L)
  expect_equal(r$out$real, d$y[1:3])
})

test_that("MUTAÇÃO: reverter para dados[1L, ] faz este teste falhar", {
  # Prova direta de que o teste acima morre se `step_rls()` voltar a olhar só
  # a primeira linha do ponto: chamando a função com `[1L, ]` embutido aqui —
  # a MESMA forma do bug original — o estado difere do laço linha a linha.
  set.seed(3)
  d <- data.frame(x = rnorm(12)); d$y <- 1 + 2 * d$x + rnorm(12, sd = 0.05)

  passo_unico_1L <- function(state, dados) {
    if (is.null(state$theta)) {  # a mesma inicialização preguiçosa de step_rls()
      nomes <- state$preditores
      if (!length(nomes)) nomes <- setdiff(names(dados), state$resposta)
      p <- length(nomes) + 1L
      state$preditores <- nomes; state$theta <- rep(0, p); state$P <- diag(state$lambda, p)
    }
    x <- c(1, as.numeric(unlist(dados[1L, state$preditores, drop = TRUE])))
    y <- as.numeric(dados[[state$resposta]][[1L]])
    P <- state$P; theta <- state$theta
    previsto <- as.numeric(sum(x * theta))
    Px <- as.vector(P %*% x); denom <- 1 + as.numeric(sum(x * Px))
    ganho <- Px / denom; residuo <- y - previsto
    theta <- theta + ganho * residuo
    P <- P - outer(ganho, Px); P <- (P + t(P)) / 2
    state$theta <- theta; state$P <- P; state$n <- state$n + 1L
    list(state = state, out = data.frame(previsto = previsto, real = y, residuo = residuo))
  }

  st_bug <- init_rls(lambda = 1e4)
  st_bug <- passo_unico_1L(st_bug, d[1:3, ])$state  # só vê a linha 1

  st_um <- init_rls(lambda = 1e4)
  for (i in 1:3) st_um <- step_rls(st_um, d[i, , drop = FALSE])$state

  # Com a versão `[1L, ]`, `st_bug` fica no estado de UMA atualização —
  # diferente do estado de TRÊS. Esta asserção falharia (os dois lados
  # seriam iguais) se `step_rls()` de verdade ainda fosse a versão `[1L, ]`.
  expect_false(isTRUE(all.equal(st_bug$theta, st_um$theta)))
  expect_equal(st_bug$n, 1L)
})

test_that("fluxo: lote = 3L usa as 12 observações, e bate com lote = 1L", {
  # A medição do revisor, de ponta a ponta: `data/to_stream(lote = 3L)` ->
  # `models/rls` -> `data/from_stream` sobre uma tabela de 12 linhas.
  reg <- models_registry()
  trama::tr_use(trama::tr_collection(id = "t_b0", version = "1.0.0",
    nodes = list(trama::tr_node("t_b0/dados", fn = function() {
      data.frame(x = 1:12, y = 1:12 * 2 + 1)
    }, outputs = list(out = "data/table"), description = "12 linhas fixas, para o repro de B0."))),
    registry = reg)

  fluxo <- function(lote) {
    trama::tr_flow(reg) |>
      trama::tr_add("dados", "t_b0/dados") |>
      trama::tr_add("entra", "data/to_stream", lote = lote, from = "dados") |>
      trama::tr_add("rls", "models/rls", resposta = "y", preditores = "x", from = "entra") |>
      trama::tr_add("sai", "data/from_stream", from = "rls")
  }

  hist3 <- rodar(fluxo(3L), "sai")
  hist1 <- rodar(fluxo(1L), "sai")

  # As 12 observações, todas presentes — não 4.
  expect_equal(nrow(hist3), 12L)
  expect_equal(hist3$real, 1:12 * 2 + 1)

  # `lote` é knob de throughput, não de semântica: os coeficientes (e o
  # histórico de previsto/real/residuo) são os MESMOS entregues em lotes de 3
  # linhas ou de 1.
  expect_identical(hist3$previsto, hist1$previsto)
  expect_identical(hist3$real, hist1$real)
})
