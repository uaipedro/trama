# Métricas de classificação: conta à mão e yardstick como oráculo.
#
# Vieram da `ml` (test-metricas.R da main, para a `ml/evaluate`): o bloco de
# avaliação é o `models/evaluate` desde a Fase 4 da coesão, e as contas da main
# foram portadas para `.tr_models_metricas_classif`. Aqui no modo tabela, com
# a coluna prevista chamada `.pred` como nos testes originais.
avaliar <- function(d, resposta) tr_models_evaluate(dados = d, resposta = resposta, predito = ".pred")

d_mao <- function() data.frame(y = c("a", "a", "a", "b", "b", "c"),
                               .pred = c("a", "a", "b", "b", "c", "c"))
val <- function(z, m, k = NULL) z$valor[z$metrica == m & (if (is.null(k)) is.na(z$classe) else z$classe %in% k)]

test_that("exemplo à mão: 3 classes, 6 linhas", {
  # Tabela observado x previsto: a -> (a 2, b 1); b -> (b 1, c 1); c -> (c 1).
  # Por classe (precisão, revocação, F1): a (1, 2/3, 0,8); b (1/2, 1/2, 1/2);
  # c (1/2, 1, 2/3). Acurácia 4/6. Kappa: pe = (3·2 + 2·2 + 1·2)/36 = 1/3,
  # (2/3 − 1/3)/(1 − 1/3) = 0,5. Pesos pelo suporte 3, 2, 1.
  z <- avaliar(d_mao(), "y")
  expect_equal(val(z, "accuracy"), 4 / 6)
  expect_equal(val(z, "kappa"), 0.5)
  expect_equal(val(z, "precision", c("a", "b", "c")), c(1, 1 / 2, 1 / 2))
  expect_equal(val(z, "recall", c("a", "b", "c")), c(2 / 3, 1 / 2, 1))
  expect_equal(val(z, "f1", c("a", "b", "c")), c(0.8, 0.5, 2 / 3))
  expect_equal(val(z, "balanced_accuracy"), mean(c(2 / 3, 1 / 2, 1)))
  expect_equal(val(z, "macro_f1"), mean(c(0.8, 0.5, 2 / 3)))
  expect_equal(val(z, "weighted_f1"), (3 * 0.8 + 2 * 0.5 + 2 / 3) / 6)
  expect_equal(val(z, "weighted_recall"), val(z, "accuracy"))   # identidade
  expect_equal(z$n[z$metrica == "f1"], c(3L, 2L, 1L))
})

test_that("kappa de Cohen: casos de borda", {
  # Concordância perfeita dá 1; previsão constante dá 0 (pe = po).
  expect_equal(val(avaliar(data.frame(y = c("a", "b"), .pred = c("a", "b")), "y"), "kappa"), 1)
  expect_equal(val(avaliar(data.frame(y = c("a", "b", "b"), .pred = c("b", "b", "b")), "y"), "kappa"), 0)
  expect_true(is.na(val(avaliar(data.frame(y = c("a", "a"), .pred = c("a", "a")), "y"), "kappa")))
})

test_that("yardstick confere macro, ponderadas e kappa", {
  skip_if_not_installed("yardstick")
  set.seed(11)
  lv <- c("x", "y", "z")
  y <- sample(lv, 80, TRUE, prob = c(.5, .3, .2))
  p <- ifelse(runif(80) < .6, y, sample(lv, 80, TRUE))
  z <- avaliar(data.frame(y = y, .pred = p), "y")
  t <- data.frame(truth = factor(y, lv), est = factor(p, lv))
  ys <- function(f, e) f(t, truth, est, estimator = e)$.estimate
  expect_equal(val(z, "kappa"), yardstick::kap(t, truth, est)$.estimate, tolerance = 1e-12)
  expect_equal(val(z, "macro_f1"), ys(yardstick::f_meas, "macro"), tolerance = 1e-12)
  expect_equal(val(z, "weighted_f1"), ys(yardstick::f_meas, "macro_weighted"), tolerance = 1e-12)
  expect_equal(val(z, "macro_precision"), ys(yardstick::precision, "macro"), tolerance = 1e-12)
  expect_equal(val(z, "weighted_precision"), ys(yardstick::precision, "macro_weighted"), tolerance = 1e-12)
  expect_equal(val(z, "macro_recall"), ys(yardstick::recall, "macro"), tolerance = 1e-12)
  expect_equal(val(z, "weighted_recall"), ys(yardstick::recall, "macro_weighted"), tolerance = 1e-12)
  # Acurácia balanceada: média das revocações (Brodersen et al. 2010). No
  # binário coincide com a (sens + espec)/2 do yardstick; no multiclasse o
  # yardstick usa a média de (sens + espec)/2 por classe, outra definição.
  b <- data.frame(truth = factor(y == "x"), est = factor(p == "x", levels = c(FALSE, TRUE)))
  zb <- avaliar(data.frame(y = as.character(b$truth), .pred = as.character(b$est)), "y")
  expect_equal(val(zb, "balanced_accuracy"), yardstick::bal_accuracy(b, truth, est)$.estimate, tolerance = 1e-12)
})

test_that("precisão de classe nunca prevista é NA e sai das médias", {
  # c é observada uma vez e nunca prevista: precisão 0/0, indefinida. Como o
  # zero_division = np.nan do scikit-learn (>= 1.3), fica NA e é excluída da
  # média macro e da ponderada (pesos renormalizados). F1 e revocação de c
  # são definidos (0) e continuam nas médias.
  z <- avaliar(data.frame(y = c("a", "a", "b", "b", "c"),
                                 .pred = c("a", "a", "b", "b", "b")), "y")
  expect_equal(val(z, "precision", c("a", "b")), c(1, 2 / 3))
  expect_true(is.na(val(z, "precision", "c")))
  expect_equal(val(z, "recall", "c"), 0); expect_equal(val(z, "f1", "c"), 0)
  expect_equal(val(z, "macro_precision"), mean(c(1, 2 / 3)))
  expect_equal(val(z, "weighted_precision"), (2 * 1 + 2 * 2 / 3) / 4)
  expect_equal(val(z, "macro_f1"), mean(c(1, 0.8, 0)))
  # previsão constante numa classe não observada: nenhuma precisão definida
  w <- avaliar(data.frame(y = c("a", "b"), .pred = c("z", "z")), "y")
  expect_true(is.na(val(w, "macro_precision"))); expect_true(is.na(val(w, "weighted_precision")))
})
