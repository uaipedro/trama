test_that("números e p-valores saem como em relatório", {
  expect_equal(tr_data_fmt_num(c(3, 15, 0.4909, -0.0159, NA)), c("3", "15", "0,4909", "-0,0159", ""))
  expect_equal(tr_data_fmt_p(c(0.00012, 0.0492, NA)), c("< 0,001", "0,049", ""))
})

test_that("a parte inteira nunca é arredondada (#32)", {
  # Os números da Tabela de descritivas do VAR IBV × S&P (Morettin, M-IBV-SP).
  expect_equal(tr_data_fmt_num(c(11887.02558, 6895.66174862069, 968.9, 0.390310993246134, -0.0626)),
               c("11.887", "6.896", "968,9", "0,3903", "-0,0626"))
  # Grandes, com sinal, e na fronteira dos 4 significativos.
  expect_equal(tr_data_fmt_num(c(-11887.6, 123456.7, 1234567.89, 9999.4, 9999.7, 9999.94, -1e6 - 0.4)),
               c("-11.888", "123.457", "1.234.568", "9.999", "10.000", "10.000", "-1.000.000"))
  # Pequenos continuam com 4 significativos; minúsculos, em notação científica.
  expect_equal(tr_data_fmt_num(c(0.000123456, -0.00098766, 1.23456, 0.00001234)),
               c("0,0001235", "-0,0009877", "1,235", "1,23e-05"))
  expect_equal(tr_data_fmt_num(c(11887.02558, 12.3456), digitos = 2L), c("11.887", "12"))
})

test_that("a tabela em Markdown escapa a barra no cabeçalho e na célula", {
  md <- tr_data_md_table(data.frame(termo = "a|b", p_valor = 0.5), c(p_valor = "Pr > |t|"))
  expect_equal(md[[1]], "| termo | Pr > \\|t\\| |")
  expect_equal(md[[2]], "|:---|---:|")
  expect_equal(md[[3]], "| a\\|b | 0,500 |")
})

test_that("o report da tabela tira coluna vazia e põe de pé a linha única larga", {
  df <- data.frame(a = 1, b = NA, c = 2, d = 3, e = 4, f = 5, g = 6, h = 7)
  r <- tr_data_report_table(df)
  expect_s3_class(r, "knit_asis")
  expect_match(r, "| medida | valor |", fixed = TRUE)
  expect_match(r, "Colunas sem valor omitidas: b.", fixed = TRUE)
  r2 <- tr_data_report_table(data.frame(x = 1:30))
  expect_match(r2, "Primeiras 20 de 30 linhas.", fixed = TRUE)
})

test_that("o report do teste traz estatística, p, decisão e conclusão", {
  t <- trama::tr_test("Shapiro-Wilk", "os dados são normais", 0.97, "W", p_valor = 0.84,
                      conclusao_sim = "não normais", conclusao_nao = "sem evidência contra a normalidade",
                      fonte = "Shapiro & Wilk (1965)")
  r <- tr_data_report_test(t)
  expect_match(r, "| Teste | W | p-valor | Decisão (5%) |", fixed = TRUE)
  expect_match(r, "0,840", fixed = TRUE)
  expect_match(r, "**Conclusão:** sem evidência contra a normalidade.", fixed = TRUE)
})

test_that("os tipos da coleção declaram o report", {
  reg <- trama::tr_registry(); trama::tr_use("trama.data", registry = reg)
  expect_identical(trama::tr_get_type("data/table", reg)$report, tr_data_report_table)
  expect_identical(trama::tr_get_type("data/test", reg)$report, tr_data_report_test)
})
