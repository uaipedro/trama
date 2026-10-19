# Os adaptadores `<tipo> -> publicar/item` e o contrato do item.

test_that("tabela vira CSV inteiro e prévia curta", {
  it <- .tr_publicar_de_tabela(datasets::mtcars)
  expect_s3_class(it, "tr_publicar_item")
  lido <- utils::read.csv(text = rawToChar(it$arquivos$dados.csv))
  expect_equal(lido, datasets::mtcars, ignore_attr = TRUE, tolerance = 1e-12)
  expect_equal(nrow(it$componente$previa), 20L)
  expect_equal(it$componente$linhas, 32L)
})

test_that("gráfico vira PNG, sem o objeto ggplot (que leva a tabela)", {
  p <- ggplot2::ggplot(datasets::mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() +
    ggplot2::labs(title = "Peso e consumo")
  it <- .tr_publicar_de_grafico(p)
  expect_identical(names(it$arquivos), "figura.png")
  expect_identical(it$arquivos$figura.png[1:4], as.raw(c(0x89, 0x50, 0x4e, 0x47)))
  expect_identical(it$titulo, "Peso e consumo")
})

test_that("modelo entra reduzido, com motor e bloco web; nada da tabela de treino", {
  d <- datasets::mtcars
  it <- .tr_publicar_de_modelo(trama.models::tr_models_lm(d, formula = "mpg ~ wt + hp"))
  expect_null(it$erro)
  expect_setequal(names(it$arquivos), c("modelo.rds", "motor.rds", "web.json"))
  f <- tempfile(fileext = ".rds"); writeBin(it$arquivos$modelo.rds, f)
  r <- readRDS(f)
  expect_true(r$reduzido); expect_null(r$dados)
  web <- jsonlite::fromJSON(rawToChar(it$arquivos$web.json))
  expect_identical(web$saida$resposta, "mpg")
  expect_identical(it$componente$tipo, "preditor")
  expect_identical(it$metadados$tipo_schema, "fair4ml:MLModel")
})

test_that("modelo sem tr_models_strip() vira item com erro, e não o objeto inteiro", {
  falso <- structure(list(dados = datasets::mtcars), class = c("tr_falso", "tr_models_fit"))
  it <- .tr_publicar_de_modelo(falso)
  expect_match(it$erro, "tr_models_strip")
  expect_length(it$arquivos, 0L)
})

test_that("o contrato do item recusa caminho fora da pasta e componente desconhecido", {
  expect_error(tr_publicar_item("x", arquivos = list("../fora.txt" = "a")), class = "tr_publicar_error_bad_item")
  expect_error(tr_publicar_item("x", arquivos = list("a")), class = "tr_publicar_error_bad_item")
  expect_error(tr_publicar_item("x", componente = list(tipo = "video")), class = "tr_publicar_error_bad_item")
  it <- tr_publicar_item("nota", arquivos = list("nota.md" = "# oi"), componente = list(tipo = "markdown", texto = "oi"))
  expect_identical(rawToChar(it$arquivos[["nota.md"]]), "# oi")
})

test_that("SVM e floresta de regressão saem do adaptador com a marca de dados de treino", {
  skip_if_not_installed("trama.ml")
  skip_if_not_installed("e1071")
  d <- data.frame(x1 = stats::rnorm(40), x2 = stats::rnorm(40)); d$y <- d$x1 + stats::rnorm(40)
  m <- trama.ml::tr_ml_fit(d, resposta = "y", preditores = "x1, x2", modelo = "svm")
  it <- .tr_publicar_de_modelo(m)
  expect_match(it$dados_treino, "vetores de suporte")
  expect_null(it$erro)  # quem recusa é o publicar, que conhece o consentimento
  expect_null(.tr_publicar_de_modelo(trama.models::tr_models_lm(d, formula = "y ~ x1"))$dados_treino)
})
