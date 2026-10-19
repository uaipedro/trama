# A página: o .qmd gerado e o render com o Quarto local.

test_that("com modelo: live-html, aviso de carregamento, preditor e versões", {
  q <- tr_publicar_pagina(material_exemplo(), data = as.Date("2026-10-19"))
  expect_match(q, "  live-html:", fixed = TRUE)
  expect_match(q, "{{< include ./_extensions/r-wasm/live/_knitr.qmd >}}", fixed = TRUE)
  expect_match(q, "Carregando o R no seu navegador", fixed = TRUE)
  expect_match(q, "viewof v_mod_wt = Inputs.range(", fixed = TRUE)
  expect_match(q, "v_mod_motor_url = new URL(\"itens/mod/motor.rds\", document.baseURI).href", fixed = TRUE)
  expect_match(q, "download.file(v_mod_motor_url, \"mod-motor.rds\")", fixed = TRUE)
  expect_match(q, "## Versões", fixed = TRUE)
  expect_match(q, "orcid: \"0000-0002-1825-0097\"", fixed = TRUE)
})

test_that("sem modelo: html comum, sem WebR", {
  m <- material_exemplo('[{"tipo":"cabecalho"},{"tipo":"texto","texto":"Uma $x^2$."},{"tipo":"item","id":"tab"}]')
  q <- tr_publicar_pagina(m)
  expect_match(q, "format:\n  html:", fixed = TRUE)
  expect_false(grepl("webr", q, fixed = TRUE))
  expect_match(q, "Uma $x^2$.", fixed = TRUE)
  # O layout manda na ordem: o texto antes da tabela.
  expect_lt(regexpr("Uma $x^2$", q, fixed = TRUE), regexpr("## Tabela", q, fixed = TRUE))
})

test_that("tabela markdown escapa a barra e formata números", {
  md <- .tr_publicar_tabela_md(data.frame(a = c(1.23456, NA), b = c("x|y", "z")))
  expect_match(md, "| 1.235 | x\\|y |", fixed = TRUE)
})

test_that("o .qmd renderiza com o Quarto local (a extensão fixada no pacote)", {
  skip_on_cran()
  quarto <- .tr_publicar_quarto()
  skip_if(!nzchar(quarto), "Quarto ausente")
  ex <- doc_exemplo(); dir <- tempfile("render-")
  tr_publicar_montar(material_exemplo(), dir, doc = ex$doc, data = as.Date("2026-10-19"))
  saida <- processx::run(quarto, c("render", "index.qmd"), wd = dir, error_on_status = FALSE)
  expect_identical(saida$status, 0L, label = paste(utils::tail(strsplit(saida$stderr, "\n")[[1]], 10), collapse = "\n"))
  html <- paste(readLines(file.path(dir, "index.html"), warn = FALSE), collapse = "\n")
  expect_match(html, "Consumo e peso", fixed = TRUE)
  expect_match(html, "webr", fixed = TRUE)
  expect_true(file.exists(file.path(dir, "itens/mod/motor.rds")))
})
