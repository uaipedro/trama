# A ajuda é parte do nó, não enfeite: um nó sem página é um nó que não se
# explica dentro da interface. O teste varre o registro inteiro, então nó novo
# nasce reprovado até ganhar a sua página — que é o ponto.

test_that("todo nó tem help no formato, e todo campo de texto tem exemplo", {
  reg <- data_registry()
  digitaveis <- c("expr", "cols", "path", "text")

  for (n in reg$nodes) {
    expect_true(!is.null(n$help) && nzchar(trimws(n$help)), info = n$id)
    expect_match(n$help, "## Descrição", fixed = TRUE, info = n$id)
    expect_match(n$help, "## Valor", fixed = TRUE, info = n$id)
    if (length(n$params)) expect_match(n$help, "## Parâmetros", fixed = TRUE, info = n$id)

    for (nm in names(n$params)) {
      p <- n$params[[nm]]
      if (!p$kind %in% digitaveis) next
      expect_true(!is.null(p$example) && nzchar(p$example), info = paste(n$id, nm))
    }
  }
})

test_that("o help chega inteiro ao catálogo", {
  cat_json <- trama::tr_catalog_json(data_registry())
  expect_match(as.character(cat_json), "## Descrição", fixed = TRUE)
})
