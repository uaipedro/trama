# Atributo `trama_ferramentas`: a ferramenta de fora que de fato rodou em cada
# ramo. O relatório cita esse registro, e não a `tr_ref` declarada, quando ele
# existe. Só a ferramenta não-base importa ao relatório, mas o atributo traz a
# que rodou mesmo quando é do `stats`.

ferramentas <- function(x) attr(x, "trama_ferramentas", exact = TRUE)

test_that("distância: a Gower sai do cluster; as outras, do stats::dist", {
  d <- tr_multi_example("iris")
  cols <- "Sepal.Length, Petal.Length"
  expect_equal(ferramentas(tr_multi_distance(d, cols = cols, metodo = "gower")), "cluster::daisy")
  expect_equal(ferramentas(tr_multi_distance(d, cols = cols, metodo = "euclidiana")), "stats::dist")
  expect_equal(ferramentas(tr_multi_distance(d, cols = cols, metodo = "euclidiana padronizada")), "stats::dist")
  expect_equal(ferramentas(tr_multi_distance(d, cols = cols, metodo = "mahalanobis")), "stats::dist")
})

test_that("discriminante: MASS::lda na linear e MASS::qda na quadrática", {
  d <- tr_multi_example("iris")
  prev <- "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width"
  lin <- tr_multi_discriminant(d, resposta = "Species", preditores = prev, metodo = "linear")
  qua <- tr_multi_discriminant(d, resposta = "Species", preditores = prev, metodo = "quadrática")
  expect_equal(ferramentas(lin), "MASS::lda")
  expect_equal(ferramentas(qua), "MASS::qda")
})

test_that("logística: glm na binária, nnet::multinom na multinomial, nada no Firth", {
  pima <- tr_multi_example("pima")
  b <- tr_multi_logistic(pima, resposta = "diabetes", preditores = "glicose, imc, pedigree")
  expect_equal(ferramentas(b), "stats::glm")
  f <- tr_multi_logistic(pima, resposta = "diabetes", preditores = "glicose, imc, pedigree",
                         metodo = "firth")
  expect_identical(ferramentas(f), character())
  m <- tr_multi_logistic(tr_multi_example("iris"), resposta = "Species",
                         preditores = "Sepal.Length, Petal.Length")
  expect_equal(ferramentas(m), "nnet::multinom")
})

test_that("jackknife da discriminante e da logística seguem o ramo do modelo", {
  d <- tr_multi_example("iris")
  prev <- "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width"
  # O jackknife só reamostra a linear (a quadrática não tem funções a reamostrar).
  lin <- tr_multi_discriminant(d, resposta = "Species", preditores = prev, metodo = "linear")
  expect_equal(ferramentas(tr_multi_jackknife_discriminant(lin)), "MASS::lda")
  pima <- as.data.frame(tr_multi_example("pima"))[1:150, ]
  pima$lote <- rep(1:10, each = 15)
  lg <- tr_multi_logistic(pima, resposta = "diabetes", preditores = "glicose, imc, pedigree")
  expect_equal(ferramentas(tr_multi_jackknife_logistic(lg, grupo = "lote")), "stats::glm")
})
