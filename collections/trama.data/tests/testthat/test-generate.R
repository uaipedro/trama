# Gerador: a fonte que fabrica o dado. O que se trava aqui é a promessa que
# torna um nó ALEATÓRIO cacheável — mesma semente, mesma amostra, sempre — e a
# independência entre dois geradores do mesmo fluxo.

test_that("expressão sozinha e sem nome vira a coluna 'valor'", {
  x <- tr_generate(5L, "runif(n, 0, 1)")
  expect_s3_class(x, "tbl_df")
  expect_equal(names(x), "valor")
  expect_equal(nrow(x), 5L)
  expect_true(all(x$valor >= 0 & x$valor <= 1))
})

test_that("colunas nomeadas saem na ordem, e uma enxerga a anterior", {
  x <- tr_generate(4L, "x = rnorm(n), y = 2 * x + 100")
  expect_equal(names(x), c("x", "y"))
  # É esta a propriedade que faz uma simulação se escrever num campo só: sem a
  # avaliação em ordem do `tibble()`, `y` não teria como referenciar `x`.
  expect_equal(x$y, 2 * x$x + 100)
})

test_that("matriz vira tabela e data.frame passa como está", {
  expect_equal(dim(tr_generate(6L, "matrix(rnorm(n), ncol = 2)")), c(3L, 2L))
  expect_equal(names(tr_generate(3L, "data.frame(a = 1:3, b = 4:6)")), c("a", "b"))
})

# O `n` é uma variável à disposição, não uma promessa sobre linhas — e a ajuda
# diz isso. Se um dia virar promessa, é aqui que o teste avisa.
test_that("n é variável da expressão, não o número de linhas garantido", {
  expect_equal(nrow(tr_generate(100L, "1:5")), 5L)
  expect_equal(nrow(tr_generate(100L, "rnorm(n)")), 100L)
})

# ---- Semente ------------------------------------------------------------
# A promessa inteira do nó: aleatório E reproduzível, que é o que permite
# cachear. Sem isto o fluxo salvo hoje abriria com outra amostra amanhã.

test_that("mesma semente dá a mesma amostra, semente diferente dá outra", {
  expect_identical(tr_generate(20L, "rnorm(n)", .seed = 7L),
                   tr_generate(20L, "rnorm(n)", .seed = 7L))
  expect_false(identical(tr_generate(20L, "rnorm(n)", .seed = 7L),
                         tr_generate(20L, "rnorm(n)", .seed = 8L)))
})

# O executor sequencial roda no processo do Shiny: um `set.seed()` que vazasse
# daqui mudaria o sorteio de todo o resto da sessão — e, pior, o do gerador
# vizinho, destruindo a independência que o card promete.
test_that("o nó não deixa o RNG da sessão alterado", {
  set.seed(99L)
  antes <- runif(1)

  set.seed(99L)
  invisible(tr_generate(10L, "rnorm(n)", .seed = 1L))
  expect_equal(runif(1), antes)
})

test_that("a semente entra na chave de cache só porque o nó é stochastic", {
  reg <- data_registry()
  n <- trama::tr_get_node("data/generate", reg)
  expect_true(n$stochastic)
  # `.seed` é do núcleo, não param desta coleção: se virasse param, ele
  # apareceria aqui e a seed passaria a existir em dois lugares.
  expect_equal(sort(names(n$params)), c("expr", "n"))
  expect_true(".seed" %in% names(formals(n$fn)))
})

test_that("dois geradores no mesmo fluxo têm sementes independentes", {
  reg <- data_registry(); s <- trama::tr_store(tempfile())
  f <- trama::tr_flow(reg) |>
    trama::tr_add("a", "data/generate", n = 10L, expr = "rnorm(n)", seed = 1L) |>
    trama::tr_add("b", "data/generate", n = 10L, expr = "rnorm(n)", seed = 2L)

  a <- trama::tr_value(f, "a", reg, s)
  b <- trama::tr_value(f, "b", reg, s)
  expect_false(identical(a$valor, b$valor))

  # Trocar a semente de UM recomputa só ele: é o que separa "re-sortear este
  # card" de "re-sortear o fluxo".
  f2 <- trama::tr_flow(reg, doc = trama::tr_doc_apply(
    f$doc, list(op = "set_seed", node = "a", value = 2L), reg))
  expect_identical(trama::tr_value(f2, "a", reg, s)$valor, b$valor)
  expect_identical(trama::tr_value(f2, "b", reg, s)$valor, b$valor)
})

# ---- Recusas ------------------------------------------------------------

test_that("tamanho inválido aborta classificado, culpando o campo", {
  for (mau in list(0L, -1L, NA_integer_, "dez")) {
    err <- tryCatch(tr_generate(mau, "rnorm(n)"), error = identity)
    expect_equal(class(err)[[1]], "tr_data_error_bad_option")
    expect_match(conditionMessage(err), "n", fixed = TRUE)
  }
})

test_that("expressão que não faz parse e expressão que não avalia se separam", {
  expect_equal(class(tryCatch(tr_generate(3L, "rnorm(n"), error = identity))[[1]],
               "tr_data_error_bad_expr")
  expect_equal(class(tryCatch(tr_generate(3L, "naoexiste(n)"), error = identity))[[1]],
               "tr_data_error_eval")
})

# Sem tabela de entrada não há colunas a listar, e anunciar uma lista vazia
# ("Colunas disponíveis: .") é pior que não anunciar nada.
test_that("o erro de avaliação sem tabela de entrada não promete colunas", {
  err <- tryCatch(tr_generate(3L, "naoexiste(n)"), error = identity)
  expect_false(grepl("Colunas dispon", conditionMessage(err)))
})

test_that("o que não é tabela, vetor nem matriz é recusado pelo tipo", {
  reg <- data_registry(); s <- trama::tr_store(tempfile())
  f <- trama::tr_flow(reg) |>
    trama::tr_add("m", "data/generate", expr = "lm(mpg ~ cyl, mtcars)")

  ev <- list()
  trama::tr_run(f$doc, registry = reg, store = s,
                on_event = function(e) ev[[length(ev) + 1]] <<- e)
  falhas <- Filter(function(e) identical(e$type, "failed"), ev)
  expect_length(falhas, 1L)
  expect_equal(falhas[[1]]$class, "tr_data_error_not_a_table")
})
