# data/select com seletores escritos como R, de uma lista fechada.

d <- tibble::tibble(regiao = c("a", "b"), valor = c(1, 2), valor_2 = c(3, 4), qtd = 1:2, `Preço (R$)` = c(9, 8))

test_that("seletores do tidyselect funcionam e dão o mesmo que o dplyr", {
  expect_equal(names(tr_select(d, "starts_with(\"valor\")")), c("valor", "valor_2"))
  expect_equal(names(tr_select(d, "ends_with(\"2\")")), "valor_2")
  expect_equal(names(tr_select(d, "contains(\"al\")")), c("valor", "valor_2"))
  expect_equal(names(tr_select(d, "where(is.numeric)")), c("valor", "valor_2", "qtd", "Preço (R$)"))
  expect_equal(tr_select(d, "where(is.character)"), dplyr::select(d, dplyr::where(is.character)))
  expect_equal(names(tr_select(d, "regiao, starts_with(\"valor\")")), c("regiao", "valor", "valor_2"))
  expect_equal(names(tr_select(d, "everything()")), names(d))
  expect_equal(names(tr_select(d, "starts_with(\"VALOR\", ignore.case = TRUE)")), c("valor", "valor_2"))
  expect_equal(names(tr_select(d, "valor:qtd")), c("valor", "valor_2", "qtd"))
})

test_that("remover com seletor tira só as escolhidas", {
  expect_equal(names(tr_select(d, "starts_with(\"valor\")", remove = TRUE)), c("regiao", "qtd", "Preço (R$)"))
  expect_equal(names(tr_select(d, "where(is.numeric)", remove = TRUE)), "regiao")
})

test_that("a lista de nomes de sempre não muda, inclusive nome com parêntese", {
  expect_equal(names(tr_select(d, "regiao, valor")), c("regiao", "valor"))
  expect_equal(names(tr_select(d, "Preço (R$)")), "Preço (R$)")
  expect_identical(tr_select(d, ""), d)
})

test_that("fora da lista fechada é recusado antes de avaliar", {
  boom <- tempfile(); on.exit(unlink(boom))
  for (txt in c(sprintf("system(\"touch %s\")", boom), "paste(\"a\")", "starts_with(paste(\"v\"))",
                "where(function(x) TRUE)", "where(is.numeric, 1)", "(function() 1)()", "c(regiao[1])",
                "base::starts_with(\"v\")", "starts_with(\"v\", foo = 1)", "starts_with()")) {
    expect_error(tr_select(d, txt), class = "tr_data_error_bad_expr", label = txt)
  }
  expect_false(file.exists(boom))
})

test_that("coluna citada que não existe segue o erro de sempre", {
  expect_error(tr_select(d, "c(regaio, starts_with(\"v\"))"), class = "tr_data_error_unknown_column")
})

test_that("posição numérica, argumento nomeado e ignore.case = NA são recusados; last_col(n) inteiro passa", {
  for (txt in c("c(2)", "c(0)", "c(1.5)", "c(Inf)", "c(novo = valor)", "!c(x = valor)",
                "starts_with(\"v\", ignore.case = NA)", "last_col(-1)", "last_col(1.5)")) {
    expect_error(tr_select(d, txt), class = "tr_data_error_bad_expr", label = txt)
  }
  expect_equal(names(tr_select(d, "last_col()")), "Preço (R$)")
  expect_equal(names(tr_select(d, "last_col(1)")), "qtd")
})

test_that("lista de nomes com parêntese e nome inexistente dá o erro de coluna, não o de sintaxe", {
  expect_error(tr_select(d, "Preço (R$), zz"), class = "tr_data_error_unknown_column")
  expect_equal(names(tr_select(d, "Preço (R$), valor")), c("Preço (R$)", "valor"))
})

test_that("duplicata e dupla negação no modo remover", {
  expect_equal(names(tr_select(d, "c(valor, valor)")), "valor")
  expect_equal(names(tr_select(d, "!c(regiao)", remove = TRUE)), "regiao")
})
