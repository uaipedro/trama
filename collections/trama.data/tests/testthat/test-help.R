# A ajuda é parte do nó, não enfeite: um nó sem página é um nó que não se
# explica dentro da interface. O teste varre o registro inteiro, então nó novo
# nasce reprovado até ganhar a sua página — que é o ponto.

test_that("todo nó tem help no formato, e todo campo de texto tem exemplo", {
  reg <- data_registry()
  digitaveis <- c("expr", "cols", "path", "text")

  for (n in reg$nodes) {
    expect_true(!is.null(n$help) && nzchar(trimws(n$help)), info = n$id)
    # Formato curto da data e da view (c865c5c): uso, exemplo e usos
    # relacionados; a referência longa fica no site.
    expect_match(n$help, "## Uso principal", fixed = TRUE, info = n$id)
    expect_match(n$help, "## Exemplo curto", fixed = TRUE, info = n$id)
    expect_match(n$help, "## Usos relacionados", fixed = TRUE, info = n$id)

    for (nm in names(n$params)) {
      p <- n$params[[nm]]
      if (!p$kind %in% digitaveis) next
      expect_true(!is.null(p$example) && nzchar(p$example), info = paste(n$id, nm))
    }
  }
})

test_that("o help chega inteiro ao catálogo", {
  cat_json <- trama::tr_catalog_json(data_registry())
  expect_match(as.character(cat_json), "## Uso principal", fixed = TRUE)
})
