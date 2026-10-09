# Validação cruzada da krigagem: as métricas, as dobras e as guardas.
#
# Convenção conferida em 2026-10-09: o resíduo é `observado - predito` tanto no
# `gstat::krige.cv` quanto no `geoR::xvalid`. MSDR é a média de
# (resíduo / erro-padrão)^2, que é a média do `zscore^2` que o krige.cv devolve.

test_that("as quatro métricas batem com a conta à mão", {
  obs <- c(10, 12, 9, 15)
  pred <- c(11, 11, 10, 14)
  var <- c(1, 4, 1, 4)
  m <- .tr_spatial_metricas(obs, pred, var)
  res <- obs - pred                       # -1, 1, -1, 1
  expect_equal(m$me, mean(res))
  expect_equal(m$rmse, sqrt(mean(res^2)))
  expect_equal(m$msdr, mean(res^2 / var))
  expect_equal(m$correlacao, stats::cor(obs, pred))
  expect_equal(m$me, 0)
  expect_equal(m$rmse, 1)
  expect_equal(m$msdr, mean(c(1, 0.25, 1, 0.25)))
})

test_that("MSDR de 1 é o caso calibrado", {
  obs <- c(1, 2, 3, 4); pred <- c(2, 3, 4, 5); var <- rep(1, 4)
  expect_equal(.tr_spatial_metricas(obs, pred, var)$msdr, 1)
})

test_that("variância zero não propaga Inf no MSDR: erro nomeado", {
  expect_error(.tr_spatial_metricas(c(1, 2), c(1, 3), c(0, 1)),
               class = "tr_spatial_error_bad_fit")
  expect_error(.tr_spatial_metricas(c(1, 2), c(1, 3), c(NA, 1)),
               class = "tr_spatial_error_bad_fit")
})

# ---- o bloco --------------------------------------------------------------------

# Fixture compartilhada no ARQUIVO: ajustar o variograma e rodar uma validação
# leave-one-out custa ~12 s, e refazer isso em cada teste levava a suíte da
# coleção de 44 s para 438 s. O que varia por teste (dobras, semente, guardas)
# continua sendo calculado no teste.
modelo_se <- function(ds = "milho_se") {
  p <- tr_spatial_example(ds)
  list(p = p, m = tr_spatial_variogram_fit(tr_spatial_variogram(p),
                                           familia = "esferico"))
}

FX <- modelo_se()
V_LOO <- tr_spatial_validation(FX$p, FX$m)

test_that("leave-one-out devolve uma linha por ponto, com as colunas do contrato", {
  x <- FX
  v <- V_LOO
  expect_s3_class(v, "tr_spatial_validation")
  expect_equal(nrow(v$tabela), nrow(x$p$dados))
  expect_true(all(c("observado", "predito", "variancia", "residuo", "z", "dobra")
                  %in% names(v$tabela)))
  expect_equal(v$metodo, "leave-one-out")
  expect_equal(v$dobras, nrow(x$p$dados))
})

test_that("o resíduo é observado menos predito, e z é o resíduo padronizado", {
  v <- V_LOO
  expect_equal(v$tabela$residuo, v$tabela$observado - v$tabela$predito,
               tolerance = 1e-12)
  expect_equal(v$tabela$z, v$tabela$residuo / sqrt(v$tabela$variancia),
               tolerance = 1e-12)
  expect_equal(v$metricas$msdr, mean(v$tabela$z^2), tolerance = 1e-12)
})

test_that("k dobras reparte todos os pontos, uma vez cada", {
  x <- FX
  v <- tr_spatial_validation(x$p, x$m, metodo = "k dobras", dobras = 5L,
                             semente = 42)
  expect_equal(nrow(v$tabela), nrow(x$p$dados))
  expect_setequal(unique(v$tabela$dobra), 1:5)
  expect_equal(v$dobras, 5L)
})

test_that("a mesma semente dá a mesma partição, e sementes diferentes não", {
  x <- FX
  f <- function(s) tr_spatial_validation(x$p, x$m, metodo = "k dobras",
                                         dobras = 5L, semente = s)$tabela$dobra
  expect_equal(f(42), f(42))
  expect_false(isTRUE(all.equal(f(42), f(7))))
})

test_that("k dobras e leave-one-out dão resultados diferentes, e ambos válidos", {
  # NÃO se afirma que o erro de k dobras é maior. A expectativa teórica (treinar
  # com menos pontos piora a predição) vale em média, não num conjunto: medido em
  # 2026-10-09 em `milho_se`, RMSE de 1155,6 em leave-one-out contra 1134,7 em 3
  # dobras — menor, pela própria variação da partição sorteada. Afirmar a
  # desigualdade seria um teste que falha por motivo legítimo.
  k3 <- tr_spatial_validation(FX$p, FX$m, metodo = "k dobras", dobras = 3L,
                              semente = 1)
  expect_equal(k3$dobras, 3L)
  expect_equal(V_LOO$dobras, nrow(FX$p$dados))
  expect_true(is.finite(k3$metricas$rmse) && k3$metricas$rmse > 0)
  expect_false(isTRUE(all.equal(k3$tabela$predito, V_LOO$tabela$predito)))
})

test_that("a nota traduz o MSDR em palavras", {
  v <- V_LOO
  expect_match(v$nota, "MSDR")
  expect_match(v$nota, "calibrado|otimista|pessimista")
})

test_that("dobras = 1 é recusado: não deixa nada de fora", {
  x <- FX
  expect_error(tr_spatial_validation(x$p, x$m, metodo = "k dobras", dobras = 1L),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_validation(x$p, x$m, metodo = "k dobras", dobras = 1L),
               regexp = "nada")
})

test_that("dobras maior que o número de pontos é recusado, e a mensagem diz o máximo", {
  x <- FX                                 # 68 pontos
  expect_error(tr_spatial_validation(x$p, x$m, metodo = "k dobras", dobras = 500L),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_validation(x$p, x$m, metodo = "k dobras", dobras = 500L),
               regexp = "68")
})

test_that("método desconhecido e entradas de outro tipo são recusados", {
  x <- FX
  expect_error(tr_spatial_validation(x$p, x$m, metodo = "bootstrap"),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_validation(list(a = 1), x$m),
               class = "tr_spatial_error_not_points")
  expect_error(tr_spatial_validation(x$p, list(a = 1)),
               class = "tr_spatial_error_not_a_model")
})

test_that("a vizinhança limitada chega ao motor e muda o resultado", {
  x <- modelo_se()
  larga <- tr_spatial_validation(x$p, x$m)
  estreita <- tr_spatial_validation(x$p, x$m, vizinhos_max = 4L)
  expect_false(isTRUE(all.equal(larga$tabela$predito, estreita$tabela$predito)))
  expect_match(estreita$nota, "4 vizinhos")
})

# ---- tipo, adaptador e registro -------------------------------------------------

test_that("o tipo guarda e devolve a validação", {
  v <- V_LOO
  tipo <- spatial_validation_type()
  expect_equal(tipo$id, "spatial/validation")
  f <- tempfile(fileext = ".rds")
  tipo$store(v, f)
  volta <- tipo$restore(f)
  expect_s3_class(volta, "tr_spatial_validation")
  expect_equal(volta$metricas, v$metricas)
})

test_that("o store do tipo recusa o que não é validação", {
  tipo <- spatial_validation_type()
  expect_error(tipo$store(list(a = 1), tempfile()),
               class = "tr_spatial_error_not_validation")
})

test_that("o adaptador para data/table traz as métricas e o por-ponto", {
  v <- V_LOO
  t <- .tr_spatial_valid_tabela(v)
  expect_s3_class(t, "data.frame")
  expect_equal(nrow(t), nrow(v$tabela))
  expect_true(all(c("observado", "predito", "residuo", "z") %in% names(t)))
})

test_that("a coleção registra o tipo e o nó spatial/validation", {
  co <- trama_collection()
  expect_true("spatial/validation" %in% vapply(co$types, function(x) x$id, ""))
  expect_true("spatial/validation" %in% vapply(co$nodes, function(x) x$id, ""))
  de <- vapply(co$adapters, function(a) a$from, "")
  para <- vapply(co$adapters, function(a) a$to, "")
  expect_true(any(de == "spatial/validation" & para == "data/table"))
})
