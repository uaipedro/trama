# O que só se prova rodando o motor de verdade: os ADAPTADORES, as portas
# opcionais e múltiplas, e os nós estocásticos.

test_that("plano -> estratificada -> média, e a amostra vira tabela pelo adaptador", {
  reg <- sampling_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("pop", "sampling/example", dataset = "fazendas") |>
    trama::tr_add("estratos", "sampling/example", dataset = "estratos_fazendas") |>
    trama::tr_add("plano", "sampling/size_stratified", estrato = "regiao", tamanho = "N",
                  desvio = "desvio_producao", erro = 60, from = "estratos") |>
    trama::tr_add("amostra", "sampling/stratified", estrato = "regiao", from = "pop") |>
    trama::tr_link("plano", "amostra:plano") |>
    trama::tr_add("media", "sampling/mean", variavel = "producao_t", from = "amostra") |>
    trama::tr_add("filtro", "data/filter", expr = "peso_amostral > 10", from = "amostra") |>
    trama::tr_add("aloc", "data/arrange", cols = "n_final", from = "plano")
  pl <- rodar(f, "plano")
  am <- rodar(f, "amostra")
  expect_equal(nrow(am$dados), pl$n)
  expect_s3_class(rodar(f, "media"), "tr_sampling_estimate")
  expect_true(all(rodar(f, "filtro")$peso_amostral > 10))
  expect_equal(nrow(rodar(f, "aloc")), 4L)
})

test_that("piloto opcional, estimativas num relatório e simulações comparadas", {
  reg <- sampling_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("pop", "sampling/example", dataset = "fazendas") |>
    trama::tr_add("piloto", "sampling/srs", n = 30L, from = "pop") |>
    trama::tr_add("plano", "sampling/size_mean", variavel = "producao_t", erro = 60) |>
    trama::tr_link("piloto", "plano:piloto") |>
    trama::tr_add("sem_piloto", "sampling/size_mean", desvio_padrao = 500, erro = 60) |>
    trama::tr_add("aas", "sampling/srs", from = "pop") |>
    trama::tr_link("plano", "aas:plano") |>
    trama::tr_add("m", "sampling/mean", variavel = "producao_t", from = "aas") |>
    trama::tr_add("t", "sampling/total", variavel = "producao_t", from = "aas") |>
    trama::tr_add("rel", "data/bind_rows", from = "m") |>
    trama::tr_link("t", "rel:tabelas") |>
    trama::tr_add("s1", "sampling/simulate", variavel = "producao_t", repeticoes = 30L, from = "aas") |>
    trama::tr_add("estr", "sampling/stratified", estrato = "regiao", n = 100L, from = "pop") |>
    trama::tr_add("s2", "sampling/simulate", variavel = "producao_t", repeticoes = 30L, from = "estr") |>
    trama::tr_add("cmp", "sampling/plot_simulation", from = "s1") |>
    trama::tr_link("s2", "cmp:simulacoes") |>
    trama::tr_add("resumos", "data/bind_rows", from = "s1") |>
    trama::tr_link("s2", "resumos:tabelas")
  expect_match(rodar(f, "plano")$nota, "piloto", fixed = TRUE)
  expect_equal(rodar(f, "sem_piloto")$parametros$S, 500)
  expect_equal(nrow(rodar(f, "aas")$dados), rodar(f, "plano")$n)
  rel <- rodar(f, "rel")
  expect_equal(rel$quantidade, c("Média", "Total"))
  expect_s3_class(rodar(f, "cmp"), "ggplot")
  expect_equal(nrow(rodar(f, "resumos")), 2L)
})
