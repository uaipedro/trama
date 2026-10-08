# Canada (vars) é o caso de livro do VAR; carregado sem sujar o ambiente.
can <- function() {
  e <- new.env()
  utils::data("Canada", package = "vars", envir = e)
  e$Canada
}

# Grupo "selecao": correlação cruzada (`series/ccf`) e critérios do VAR
# (`series/var_select`).
#
# Oráculos:
# - series/ccf: `stats::ccf` com o mesmo `lag.max`; tolerância 1e-12 nos valores.
#   A defasagem é a do `ccf` em observações (lag * frequência), conferida a 1e-12.
# - series/var_select: `vars::VARselect` com o mesmo `lag.max`, `type` e `season`;
#   tolerância 1e-10 nos critérios.
# - Vinheta do vars: a seleção publicada para `VARselect(Canada, lag.max = 8,
#   type = "both")` NÃO está instalada no pacote (não há `doc/` em
#   `system.file("doc", package = "vars")`); o teste de seleção publicada fica
#   pendente até conferir o número na fonte.

test_that("ccf: valores iguais aos do stats::ccf a 1e-12, em frequência 4", {
  x <- can()[, "e"]
  y <- can()[, "prod"]
  tab <- .tr_series_ccf_tabela(x, y, defasagens = 8L)
  ref <- stats::ccf(x, y, lag.max = 8L, plot = FALSE, na.action = stats::na.fail)
  expect_equal(tab$r, as.numeric(ref$acf), tolerance = 1e-12)
  expect_equal(tab$defasagem, as.integer(round(as.numeric(ref$lag) * 4)))
  expect_equal(tab$defasagem, -8:8)
})

test_that("ccf: valores a 1e-12 também em série com frequência 1 (DAX e CAC)", {
  x <- datasets::EuStockMarkets[, "DAX"]
  y <- datasets::EuStockMarkets[, "CAC"]
  tab <- .tr_series_ccf_tabela(x, y, defasagens = 12L)
  ref <- stats::ccf(x, y, lag.max = 12L, plot = FALSE, na.action = stats::na.fail)
  expect_equal(tab$r, as.numeric(ref$acf), tolerance = 1e-12)
})

test_that("ccf: sinal da defasagem, com y antecedendo x por 3 observações", {
  # x_t = y_{t-3} exatamente: a correlação cruzada tem de atingir o máximo em k = +3
  # (y antecede x) e não em k = -3. Vale com ruído branco, sem depender do stats.
  set.seed(1)
  y <- stats::rnorm(200)
  yy <- stats::ts(y[4:200], start = 4)
  xx <- stats::ts(y[1:197], start = 4)
  tab <- .tr_series_ccf_tabela(xx, yy, defasagens = 5L)
  expect_equal(tab$defasagem[which.max(tab$r)], 3L)
  # Não é exatamente 1: cada janela tem a sua média, e o ccf centra cada uma.
  expect_gt(tab$r[tab$defasagem == 3L], 0.95)
})

test_that("ccf: banda de ±1,96/√n e marcação das barras de fora", {
  x <- can()[, "e"]
  y <- can()[, "prod"]
  tab <- .tr_series_ccf_tabela(x, y, defasagens = 8L)
  n <- nrow(can())
  expect_equal(attr(tab, "banda"), stats::qnorm(.975) / sqrt(n), tolerance = 1e-12)
  expect_equal(tab$fora, abs(tab$r) > attr(tab, "banda"))
})

test_that("ccf: gráfico de view/plot com as barras negativas e positivas", {
  p <- tr_series_ccf(can()[, "e"], can()[, "prod"], defasagens = 8L)
  expect_s3_class(p, "ggplot")
  expect_equal(p$data$defasagem, -8:8)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ccf: frequências diferentes e período sem sobreposição são erros classificados", {
  err <- tryCatch(.tr_series_ccf_tabela(datasets::AirPassengers, can()[, "e"]),
                  condition = identity)
  expect_s3_class(err, "tr_series_error_frequency_mismatch")
  err <- tryCatch(.tr_series_ccf_tabela(stats::ts(1:20, start = 1), stats::ts(1:20, start = 100)),
                  condition = identity)
  expect_s3_class(err, "tr_series_error_no_overlap")
})

test_that("var_select: critérios iguais aos do vars::VARselect a 1e-10", {
  s <- can()
  tab <- tr_series_var_select(s, max_defasagens = 8L, deterministico = "ambos")
  ref <- vars::VARselect(s, lag.max = 8, type = "both")
  expect_equal(tab$defasagem, 1:8)
  expect_equal(tab$AIC, as.numeric(ref$criteria["AIC(n)", ]), tolerance = 1e-10)
  expect_equal(tab$HQ, as.numeric(ref$criteria["HQ(n)", ]), tolerance = 1e-10)
  expect_equal(tab$SC, as.numeric(ref$criteria["SC(n)", ]), tolerance = 1e-10)
  expect_equal(tab$FPE, as.numeric(ref$criteria["FPE(n)", ]), tolerance = 1e-10)
})

test_that("var_select: a coluna escolhida diz qual critério aponta para cada ordem", {
  # Conta do próprio vars para Canada, lag.max = 8, type = "both": AIC e FPE
  # escolhem 3, HQ escolhe 2, SC escolhe 1 (o `selection` do VARselect).
  tab <- tr_series_var_select(can(), max_defasagens = 8L, deterministico = "ambos")
  ref <- vars::VARselect(can(), lag.max = 8, type = "both")$selection
  expect_equal(tab$escolhida[tab$defasagem == 3L], "AIC, FPE")
  expect_equal(tab$escolhida[tab$defasagem == 2L], "HQ")
  expect_equal(tab$escolhida[tab$defasagem == 1L], "SC")
  expect_equal(sum(nzchar(tab$escolhida)), 3L)
  expect_equal(ref[["AIC(n)"]], 3L)
})

test_that("var_select: determinístico e sazonal chegam ao VARselect", {
  s <- can()
  tab <- tr_series_var_select(s, max_defasagens = 4L, deterministico = "constante", sazonal = TRUE)
  ref <- vars::VARselect(s, lag.max = 4, type = "const", season = 4L)
  expect_equal(tab$AIC, as.numeric(ref$criteria["AIC(n)", ]), tolerance = 1e-10)
})

test_that("var_select: sazonal em série sem ciclo e série com faltante são erros classificados", {
  m <- stats::ts(cbind(a = as.numeric(datasets::Nile)[1:90], b = as.numeric(datasets::Nile)[11:100]))
  err <- tryCatch(tr_series_var_select(m, sazonal = TRUE), condition = identity)
  expect_s3_class(err, "tr_series_error_no_season")
  m2 <- m; m2[5, "a"] <- NA
  err <- tryCatch(tr_series_var_select(m2), condition = identity)
  expect_s3_class(err, "tr_series_error_missing_values")
})
