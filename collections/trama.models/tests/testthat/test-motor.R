# O que só se prova rodando o motor de verdade: os ADAPTADORES e as portas.

test_that("DBC -> médias -> tabela da data, pelo adaptador", {
  reg <- models_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("milho", "models/example", dataset = "milho_dbc") |>
    trama::tr_add("dbc", "models/anova_dbc", resposta = "producao", tratamento = "hibrido",
                  bloco = "bloco", from = "milho") |>
    trama::tr_add("medias", "models/emmeans", especs = "hibrido", from = "dbc") |>
    trama::tr_add("ordena", "data/arrange", cols = "grupo", from = "medias") |>
    trama::tr_add("quadro", "models/anova_table", from = "dbc") |>
    trama::tr_add("q_tab", "data/filter", expr = "!is.na(p_valor)", from = "quadro") |>
    trama::tr_add("coef", "data/select", cols = "termo, p_valor", from = "dbc")
  o <- rodar(f, "ordena")
  expect_equal(as.character(o$hibrido[[1]]), "H3")
  expect_equal(rodar(f, "q_tab")$termo, c("bloco", "hibrido"))
  expect_equal(names(rodar(f, "coef")), c("termo", "p_valor"))
})

test_that("testes ligados num data/bind_rows viram relatório", {
  reg <- models_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("milho", "models/example", dataset = "milho_dbc") |>
    trama::tr_add("dbc", "models/anova_dbc", resposta = "producao", tratamento = "hibrido",
                  bloco = "bloco", from = "milho") |>
    trama::tr_add("sw", "models/shapiro_residuals", from = "dbc") |>
    trama::tr_add("lev", "models/levene", from = "dbc") |>
    trama::tr_add("rel", "data/bind_rows", from = "sw") |>
    trama::tr_link("lev", "rel:tabelas")
  r <- rodar(f, "rel")
  expect_equal(r$teste, c("Shapiro-Wilk (resíduos)", "Levene"))
  expect_true(all(c("p_valor", "significancia", "decisao_5") %in% names(r)))
})

test_that("compare com duas entradas no motor", {
  reg <- models_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("carros", "models/example", dataset = "mtcars") |>
    trama::tr_add("m1", "models/lm", formula = "mpg ~ wt", from = "carros") |>
    trama::tr_add("m2", "models/lm", formula = "mpg ~ wt + hp", from = "carros") |>
    trama::tr_add("cmp", "models/compare", from = "m1") |>
    trama::tr_link("m2", "cmp:outro")
  expect_equal(rodar(f, "cmp")$teste, "F de modelos aninhados")
})
