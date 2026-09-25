test_that("a previsão bate com predict(m, newdata) em ponto flutuante", {
  mt <- ex("mtcars")
  m <- tr_models_lm(mt, formula = "mpg ~ wt")
  novos <- data.frame(wt = c(2, 3, 4))
  p <- tr_models_predict(m, novos)
  ref <- stats::predict(m$ajuste, newdata = novos)
  expect_equal(p$previsto, unname(ref))
  expect_equal(names(p), c("wt", "previsto"))
})

test_that("intervalo de confiança e de predição batem com predict.lm", {
  mt <- ex("mtcars")
  m <- tr_models_lm(mt, formula = "mpg ~ wt")
  novos <- data.frame(wt = c(2, 3, 4))

  pc <- tr_models_predict(m, novos, intervalo = "confianca")
  refc <- stats::predict(m$ajuste, newdata = novos, interval = "confidence")
  expect_equal(pc$previsto, unname(refc[, "fit"]))
  expect_equal(pc$li, unname(refc[, "lwr"]))
  expect_equal(pc$ls, unname(refc[, "upr"]))

  pp <- tr_models_predict(m, novos, intervalo = "predicao")
  refp <- stats::predict(m$ajuste, newdata = novos, interval = "prediction")
  expect_equal(pp$li, unname(refp[, "lwr"]))
  # predição é sempre mais larga que confiança, porque soma a variância do erro
  expect_true(all(pp$ls - pp$li > pc$ls - pc$li))
})

test_that("GLM prevê na escala da resposta, não na do link", {
  mt <- ex("mtcars")
  g <- tr_models_glm(mt, formula = "am ~ wt", familia = "binomial")
  novos <- data.frame(wt = c(2, 3, 4))
  p <- tr_models_predict(g, novos)
  ref <- stats::predict(g$ajuste, newdata = novos, type = "response")
  # Binomial 0/1 é classificação: a probabilidade (escala da resposta) vai em
  # `prob_1`, e `previsto` é a classe pelo corte 0,5.
  expect_equal(p$prob_1, unname(ref))
  expect_equal(p$prob_0, 1 - unname(ref))
  expect_equal(p$previsto, factor(ifelse(unname(ref) >= 0.5, "1", "0"), levels = c("0", "1")))
  # As outras famílias seguem com o número em `previsto`, na escala da resposta.
  po <- tr_models_glm(ex("warpbreaks"), formula = "breaks ~ tension", familia = "poisson")
  nt <- data.frame(tension = c("L", "M"))
  expect_equal(tr_models_predict(po, nt)$previsto,
               unname(stats::predict(po$ajuste, newdata = nt, type = "response")))
})

test_that("coluna preditora faltando em 'dados' erra alto, nomeando a coluna", {
  mt <- ex("mtcars")
  m <- tr_models_lm(mt, formula = "mpg ~ wt + hp")
  novos <- data.frame(wt = c(2, 3))  # falta 'hp'
  e <- expect_error(tr_models_predict(m, novos), class = "tr_models_error_unknown_column")
  expect_match(conditionMessage(e), "hp", fixed = TRUE)
})

test_that("nível de fator não visto no ajuste erra alto, nomeando a coluna e o nível", {
  m <- milho_dbc()  # tratamento = hibrido, 5 níveis
  novos <- data.frame(hibrido = "H99", bloco = levels(m$dados$bloco)[[1]])
  e <- expect_error(tr_models_predict(m, novos), class = "tr_models_error_unknown_level")
  expect_match(conditionMessage(e), "hibrido", fixed = TRUE)
  expect_match(conditionMessage(e), "H99", fixed = TRUE)
})

test_that("intervalo não se aplica fora de models/lm", {
  mt <- ex("mtcars")
  g <- tr_models_glm(mt, formula = "am ~ wt", familia = "binomial")
  novos <- data.frame(wt = 3)
  expect_error(tr_models_predict(g, novos, intervalo = "confianca"),
              class = "tr_models_error_not_applicable")

  misto <- tr_models_lmer(ex("sleepstudy"), formula = "Reaction ~ Days + (1 | Subject)")
  novos2 <- data.frame(Days = 3, Subject = levels(misto$dados$Subject)[[1]])
  expect_error(tr_models_predict(misto, novos2, intervalo = "predicao"),
              class = "tr_models_error_not_applicable")
})

test_that("parcela subdividida não se aplica: o ajuste não é um modelo só", {
  m <- tr_models_anova_split_plot(ex("aveia"), "producao", "variedade", "nitrogenio", "bloco")
  novos <- data.frame(variedade = levels(m$dados$variedade)[[1]],
                      nitrogenio = levels(m$dados$nitrogenio)[[1]],
                      bloco = levels(m$dados$bloco)[[1]])
  expect_error(tr_models_predict(m, novos), class = "tr_models_error_not_applicable")
})

# ---- Mutação (b): sem a checagem, o que o usuário veria de fato ------------
#
# Não é um teste automatizado (a checagem faz parte do produto), e sim o
# registro do achado: chamar `predict()` cru no mesmo cenário do teste de
# coluna faltando acima.
test_that("[achado] sem a checagem, predict.lm() cru NÃO nomeia a coluna que falta", {
  mt <- ex("mtcars")
  m <- tr_models_lm(mt, formula = "mpg ~ wt + hp")
  novos <- data.frame(wt = c(2, 3))  # falta 'hp'
  e <- tryCatch(stats::predict(m$ajuste, newdata = novos), error = function(e) e)
  expect_s3_class(e, "error")
  # A mensagem crua do R não cita 'hp' — é sobre o objeto 'hp' não existir no
  # ambiente de avaliação da fórmula, não sobre a coluna de 'newdata'.
  expect_false(grepl("coluna", conditionMessage(e), fixed = TRUE))
})
