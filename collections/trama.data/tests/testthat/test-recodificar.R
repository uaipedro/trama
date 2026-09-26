# Separar, juntar, recodificar e amostrar: as bordas que dão resultado errado
# em silêncio se ninguém as trava.

test_that("separar: partes no lugar da original, e número errado de partes é erro", {
  d <- tibble::tibble(id = 1:2, codigo = c("L1_A_1", "L2_B_2"), y = 3:4)
  r <- tr_separate(d, "codigo", "local, trat, rep")
  expect_named(r, c("id", "local", "trat", "rep", "y"))
  expect_equal(r$trat, c("A", "B"))
  expect_named(tr_separate(d, "codigo", "a, b, c", remover = FALSE), c("id", "codigo", "a", "b", "c", "y"))
  expect_error(tr_separate(d, "codigo", "a, b"), class = "tr_data_error_bad_option")
  # Separador literal: "." não é regex.
  expect_equal(tr_separate(tibble::tibble(v = "1.5"), "v", "i, d", separador = ".")$d, "5")
})

test_that("juntar: na posição da primeira, faltante vira NA no texto", {
  d <- tibble::tibble(dia = c(1, 2), mes = c(3, NA), x = 1:2)
  r <- tr_unite(d, "dia, mes", nome = "data", separador = "/")
  expect_named(r, c("data", "x"))
  expect_equal(r$data, c("1/3", "2/NA"))
  expect_error(tr_unite(d, "dia, mes", nome = "x"), class = "tr_data_error_name_collision")
})

test_that("recodificar níveis: de/para, fator preservado, nível inexistente é erro", {
  d <- tibble::tibble(t = factor(c("T1", "T2", "T3", NA)))
  r <- tr_recode(d, "t", niveis = "T1=testemunha; T2=adubado")
  expect_equal(as.character(r$t), c("testemunha", "adubado", "T3", NA))
  expect_equal(levels(r$t), c("testemunha", "adubado", "T3"))
  expect_error(tr_recode(d, "t", niveis = "t1=x"), class = "tr_data_error_bad_option")
  expect_error(tr_recode(d, "t", niveis = "T1 x"), class = "tr_data_error_bad_option")
  expect_error(tr_recode(d, "t"), class = "tr_data_error_bad_option")
})

test_that("faixas: bordas à direita e à esquerda, NA passa, fora e rótulos errados são erro", {
  d <- tibble::tibble(x = c(0, 10, 10.5, 20, NA))
  r <- tr_recode(d, "x", cortes = "0; 10; 20", rotulos = "baixo; alto", nome = "classe")
  expect_equal(as.character(r$classe), c("baixo", "baixo", "alto", "alto", NA))
  expect_equal(r$x, d$x)
  e <- tr_recode(d, "x", cortes = "0; 10; 20", fechado = "esquerda")
  expect_equal(as.character(e$x), c("[0,10)", "[10,20]", "[10,20]", "[10,20]", NA))
  expect_error(tr_recode(d, "x", cortes = "0; 10"), class = "tr_data_error_bad_option")  # 20 fora
  expect_error(tr_recode(d, "x", cortes = "0; 10; 20", rotulos = "a; b; c"), class = "tr_data_error_bad_option")
  expect_error(tr_recode(d, "x", cortes = "0; 10; 20", rotulos = "a"), class = "tr_data_error_bad_option")
  expect_error(tr_recode(d, "x", cortes = "20; 10"), class = "tr_data_error_bad_option")
  expect_equal(as.character(tr_recode(d, "x", cortes = "-Inf; 10,5; Inf", rotulos = "a;b")$x)[1:4],
               c("a", "a", "a", "b"))
})

test_that("amostrar: reprodutível com a semente, por grupo, sem mexer no RNG", {
  d <- df_exemplo()
  set.seed(99); antes <- .Random.seed
  a <- tr_sample(d, n = 3L, .seed = 7L)
  expect_identical(.Random.seed, antes)
  expect_identical(a, tr_sample(d, n = 3L, .seed = 7L))
  expect_equal(nrow(a), 3L)
  g <- tr_sample(d, n = 2L, grupo = "regiao", .seed = 1L)
  expect_equal(as.vector(table(g$regiao)), c(2L, 2L))
  expect_equal(nrow(tr_sample(d, n = 0L, fracao = 0.5, .seed = 1L)), 3L)
  expect_equal(nrow(tr_sample(d, n = 0L, fracao = 2, reposicao = TRUE, .seed = 1L)), 12L)
  expect_error(tr_sample(d, n = 4L, grupo = "regiao"), class = "tr_data_error_bad_option")
  expect_error(tr_sample(d, n = 0L, fracao = 2), class = "tr_data_error_bad_option")
})
