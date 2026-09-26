# O que só se prova rodando o motor de verdade: os ADAPTADORES. O nível 1 testa
# `.tr_series_tabela()` como função; aqui se prova que o motor a insere
# sozinho na aresta, sem caixa na tela, quando uma série é ligada numa porta
# `data/table` — que é a promessa de "não duplicar a `data`".

rodar <- function(flow, no, port = NULL) {
  s <- trama::tr_store(tempfile())
  trama::tr_value(trama::tr_flow_doc(flow), no, registry = flow$registry, store = s, port = port)
}

test_that("série ligada num nó da data vira tabela tempo/valor pelo adaptador", {
  reg <- series_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("pax", "series/example") |>
    trama::tr_add("filtro", "data/filter", expr = "tempo >= as.Date('1960-01-01')", from = "pax")
  out <- rodar(f, "filtro")
  expect_s3_class(out, "data.frame")
  expect_equal(names(out), c("tempo", "valor"))
  expect_equal(nrow(out), 12L)
  expect_equal(out$valor[[1]], 417)
})

test_that("tabela da data volta a ser série, e segue por operador, teste e gráfico", {
  reg <- series_registry()
  csv <- tempfile(fileext = ".csv")
  set.seed(4)
  utils::write.csv(data.frame(mes = seq(as.Date("2019-01-01"), by = "month", length.out = 60),
                              y = cumsum(stats::rnorm(60)))[sample(60), ], csv, row.names = FALSE)
  f <- trama::tr_flow(reg) |>
    trama::tr_add("ler", "data/read_csv", path = csv) |>
    trama::tr_add("serie", "series/from_table", valor = "y", tempo = "mes", from = "ler") |>
    trama::tr_add("d", "series/diff", from = "serie") |>
    trama::tr_add("teste", "series/adf", from = "d") |>
    trama::tr_add("acf", "series/acf", from = "d")
  expect_equal(stats::start(rodar(f, "serie")), c(2019, 1))
  expect_equal(rodar(f, "teste")$teste, "ADF")
  expect_s3_class(rodar(f, "acf"), "ggplot")
})

test_that("modelo, previsão, acurácia com a entrada opcional, e previsão como tabela", {
  reg <- series_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("pax", "series/example") |>
    trama::tr_add("treino", "series/window", fim = "1958", from = "pax") |>
    trama::tr_add("ets", "series/ets", from = "treino") |>
    trama::tr_add("prev", "series/forecast", horizonte = 24L, from = "ets") |>
    trama::tr_add("so_treino", "series/accuracy", from = "prev") |>
    trama::tr_add("com_teste", "series/accuracy", from = "prev") |>
    trama::tr_link("pax", "com_teste:real") |>
    trama::tr_add("tab", "data/arrange", cols = "tempo", from = "prev")
  expect_equal(rodar(f, "so_treino")$conjunto, "treino")
  expect_equal(rodar(f, "com_teste")$conjunto, c("treino", "teste"))
  tab <- rodar(f, "tab")
  expect_equal(names(tab), c("tempo", "previsto", "li_80", "ls_80", "li_95", "ls_95"))
  expect_equal(nrow(tab), 24L)
})

test_that("decomposição vira tabela, e resíduo vira histograma da view", {
  reg <- series_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("pax", "series/example") |>
    trama::tr_add("stl", "series/stl", from = "pax") |>
    trama::tr_add("tab", "data/select", cols = "tempo, resto", from = "stl") |>
    trama::tr_add("arima", "series/arima", from = "pax") |>
    trama::tr_add("res", "series/residuals", from = "arima") |>
    trama::tr_add("hist", "view/histogram", x = "valor", from = "res")
  expect_equal(names(rodar(f, "tab")), c("tempo", "resto"))
  expect_s3_class(rodar(f, "hist"), "ggplot")
})

test_that("regressão atravessa a aresta: tabela de coeficientes de um lado, componentes do outro", {
  reg <- series_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("pax", "series/example") |>
    trama::tr_add("reg", "series/regression", grau = 2L, from = "pax") |>
    trama::tr_add("coef", "data/arrange", cols = "p_valor", from = "reg") |>
    trama::tr_add("f", "series/f_global", from = "reg") |>
    trama::tr_add("resto", "series/component", componente = "resto", from = "reg") |>
    trama::tr_add("ruido", "series/ljung_box", from = "resto") |>
    trama::tr_add("graf", "series/plot_decomposition", from = "reg")
  expect_equal(names(rodar(f, "coef")),
               c("termo", "estimativa", "erro_padrao", "estatistica_t", "p_valor"))
  expect_equal(rodar(f, "f")$teste, "F global")
  expect_equal(stats::frequency(rodar(f, "resto")), 12)
  expect_equal(rodar(f, "ruido")$teste, "Ljung-Box")
  expect_s3_class(rodar(f, "graf"), "ggplot")
})

test_that("uma série NÃO entra onde o motor não tem adaptador", {
  reg <- series_registry()
  f <- trama::tr_flow(reg) |> trama::tr_add("tab", "data/example")
  err <- tryCatch(trama::tr_add(f, "d", "series/diff", from = "tab"), error = identity)
  expect_s3_class(err, "tr_error_type_mismatch")
})

test_that("intervenção roda no motor (exemplo da ajuda) e o bootstrap usa a semente do nó", {
  reg <- series_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("sb", "series/example", dataset = "Seatbelts$drivers") |>
    trama::tr_add("log", "series/transform", from = "sb") |>
    trama::tr_add("lei", "series/intervencao", data = "1983, 2", p = 1L, d = 0L, q = 0L,
                  P = 1L, D = 1L, Q = 1L, from = "log") |>
    trama::tr_add("ets", "series/ets", from = "log") |>
    trama::tr_add("prev", "series/forecast", intervalo = "bootstrap", from = "ets")
  out <- rodar(f, "lei")
  expect_equal(out$termo[[1]], "intervencao")
  expect_lt(out$ls_95[[1]], 0)
  a <- rodar(f, "prev"); b <- rodar(f, "prev")
  expect_identical(a$upper, b$upper)
})
