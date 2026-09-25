# Divisão e validação cruzada para dados dependentes (Roberts et al. 2017):
# por grupo, nenhum grupo cai dos dois lados; no tempo, todo teste é
# posterior a todo treino (origem móvel de Tashman 2000; Hyndman &
# Athanasopoulos, FPP3, sec. 5.10; Bergmeir, Hyndman & Koo 2018).

dados_dep <- function() {
  set.seed(1)
  tibble::tibble(
    y = factor(rep(c("a", "b"), 30)), x = rnorm(60), z = rnorm(60),
    dia = rep(1:20, each = 3),                 # empates: 3 linhas por dia
    lote = rep(sprintf("L%02d", 1:12), each = 5))
}

test_that("split por grupo não reparte grupo e respeita a semente", {
  d <- dados_dep()
  s <- tr_ml_split(d, "y", proporcao = 0.75, estrategia = "grupo", grupo = "lote", seed = 3)
  expect_length(intersect(s$treino$lote, s$teste$lote), 0L)
  expect_equal(length(unique(s$treino$lote)), 9L)        # floor(12 * .75)
  expect_equal(sort(c(s$treino$x, s$teste$x)), sort(d$x))
  s2 <- tr_ml_split(d, "y", proporcao = 0.75, estrategia = "grupo", grupo = "lote", seed = 3)
  expect_identical(s, s2)
  expect_error(tr_ml_split(d, "y", estrategia = "grupo"), class = "tr_ml_error_blank_param")
  expect_error(tr_ml_split(d[d$lote == "L01", ], "y", estrategia = "grupo", grupo = "lote"),
               class = "tr_ml_error_bad_groups")
})

test_that("split temporal põe todo o teste depois do treino, sem partir empates", {
  d <- dados_dep()[sample(60), ]                           # ordem das linhas embaralhada
  s <- tr_ml_split(d, "y", proporcao = 0.7, estrategia = "temporal", ordem = "dia")
  expect_true(max(s$treino$dia) < min(s$teste$dia))
  expect_equal(nrow(s$treino) + nrow(s$teste), 60L)
  expect_equal(max(s$treino$dia), 14L)                    # linha 42 (= floor(60*.7)) é dia 14
  datas <- d; datas$dia <- as.Date("2026-01-01") + datas$dia
  s_data <- tr_ml_split(datas, "y", proporcao = 0.7, estrategia = "temporal", ordem = "dia")
  expect_equal(nrow(s_data$treino), nrow(s$treino))
  expect_error(tr_ml_split(d, "y", estrategia = "temporal"), class = "tr_ml_error_blank_param")
  um_dia <- d; um_dia$dia <- 1L
  expect_error(tr_ml_split(um_dia, "y", estrategia = "temporal", ordem = "dia"),
               class = "tr_ml_error_bad_order")
  na <- d; na$dia[[1]] <- NA
  expect_error(tr_ml_split(na, "y", estrategia = "temporal", ordem = "dia"),
               class = "tr_ml_error_missing")
})

test_that("folds por grupo e de origem móvel: contabilidade exata", {
  d <- dados_dep()
  g <- .tr_ml_folds(d, "grupo", 4L, grupo = "lote")
  expect_length(g, 4L)
  for (f in g) expect_length(intersect(d$lote[f$treino], d$lote[f$validacao]), 0L)
  expect_equal(sort(unlist(lapply(g, `[[`, "validacao"))), 1:60)  # cada linha valida uma vez
  expect_true(all(vapply(g, function(f) length(unique(d$lote[f$validacao])), 1) == 3))

  t <- .tr_ml_folds(d, "temporal", 4L, ordem = "dia")
  expect_length(t, 4L)
  # 20 dias em 5 blocos de 4: origem i treina nos blocos 1..i e valida no i + 1.
  for (i in 1:4) {
    expect_equal(sort(unique(d$dia[t[[i]]$treino])), seq_len(4 * i))
    expect_equal(sort(unique(d$dia[t[[i]]$validacao])), 4 * i + 1:4)
    expect_true(max(d$dia[t[[i]]$treino]) < min(d$dia[t[[i]]$validacao]))
  }
  expect_error(.tr_ml_folds(d, "temporal", 20L, ordem = "dia"), class = "tr_ml_error_bad_folds")
  expect_error(.tr_ml_folds(d, "grupo", 13L, grupo = "lote"), class = "tr_ml_error_bad_folds")
})

test_that("tune com folds por grupo/temporais não usa ordem nem grupo como preditor", {
  d <- dados_dep()
  dg <- d[c("y", "x", "z")]; dg$lote <- rep(1:12, each = 5)   # grupo numérico
  z <- tr_ml_tune(dg, "y", modelo = "cart", tentativas = 2, folds = 3,
                  estrategia = "grupo", grupo = "lote", seed = 5)
  expect_equal(z$estrategia, "grupo")
  expect_setequal(z$modelo$preditores, c("x", "z"))
  zt <- tr_ml_tune(d, "y", modelo = "cart", tentativas = 2, folds = 3,
                   estrategia = "temporal", ordem = "dia", seed = 5)
  expect_setequal(zt$modelo$preditores, c("x", "z"))
  expect_true(all(zt$historico$status == "ok"))
  # A estratégia aleatória padrão reproduz a versão anterior.
  a <- tr_ml_tune(mtcars[, c("mpg", "wt", "hp")], "mpg", tentativas = 3, folds = 3, seed = 19)
  expect_equal(a$estrategia, "aleatoria")
})
