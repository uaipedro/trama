# Render com o Quarto local. Só HTML na suíte (rápido); PDF (Typst) e Word
# com TRAMA_RENDER_TODOS=1.

skip_sem_quarto <- function() {
  # No R CMD check (r-universe), não: render é lento e depende do Quarto da máquina.
  skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")), "R CMD check")
  skip_if(!nzchar(trama.documentos:::.td_quarto()), "Quarto não instalado")
}

test_that("o formato do Quarto vem do modelo declarado no YAML", {
  f <- trama.documentos:::.td_formato_quarto
  l <- c("---", "format:", "  basico-html: default", "  basico-typst: default", "---")
  expect_identical(f(l, "html"), "basico-html")
  expect_identical(f(l, "pdf"), "basico-typst")
  expect_identical(f(l, "docx"), "docx")
  expect_identical(f(c("---", "title: x", "---"), "pdf"), "typst")
})

test_that("renderiza em HTML com as regiões e serve a prévia", {
  skip_sem_quarto()
  proj <- local_projeto()
  salvar_fluxo(proj, no("tab", n = 4L))
  qmd <- tr_documento_criar("rel", proj, perfil = FALSE, titulo = "Teste de render")
  tr_documento_inserir(qmd, "tab", proj)
  cat("\nVer @tbl-tab.\n", file = qmd, append = TRUE)
  html <- tr_documento_renderizar(qmd, "html")
  expect_identical(html, file.path(dirname(qmd), "_saida", "rel.html"))
  h <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(h, 'id="trama-tab"', fixed = TRUE)
  expect_match(h, "Tabela&nbsp;1", fixed = TRUE)
  expect_match(h, "Tabela de teste", fixed = TRUE)
  # A saída não se mistura ao texto.
  expect_false(file.exists(file.path(dirname(qmd), "rel.html")))

  p <- tr_documento_previa(qmd, abrir = FALSE)
  on.exit(tr_documento_previa_parar(p), add = TRUE)
  expect_match(p$url, "^http://127\\.0\\.0\\.1:[0-9]+/rel\\.html$")
  servido <- paste(readLines(url(p$url), warn = FALSE), collapse = "\n")
  expect_match(servido, 'id="trama-tab"', fixed = TRUE)
  expect_identical(tr_documento_previa(qmd, abrir = FALSE)$url, p$url)
  expect_identical(tr_documento_previa_parar(p), 1L)
  expect_error(suppressWarnings(readLines(url(p$url))))
})

test_that("PDF via Typst e Word", {
  skip_sem_quarto()
  skip_if_not(identical(Sys.getenv("TRAMA_RENDER_TODOS"), "1"), "TRAMA_RENDER_TODOS=1 para renderizar PDF e Word")
  proj <- local_projeto()
  salvar_fluxo(proj, no("tab", n = 4L))
  qmd <- tr_documento_criar("rel", proj, perfil = FALSE)
  tr_documento_inserir(qmd, "tab", proj)
  pdf <- tr_documento_renderizar(qmd, "pdf")
  expect_identical(readBin(pdf, "raw", 4L), charToRaw("%PDF"))
  docx <- tr_documento_renderizar(qmd, "docx")
  expect_identical(readBin(docx, "raw", 2L), charToRaw("PK"))
})

test_that("Quarto que falha: erro com o log", {
  skip_sem_quarto()
  proj <- local_projeto()
  salvar_fluxo(proj, no("tab"))
  qmd <- tr_documento_criar("rel", proj, perfil = FALSE)
  cat("\n{{< include _trama/nao-existe/x.md >}}\n", file = qmd, append = TRUE)
  expect_error(tr_documento_renderizar(qmd, "html"), class = "tr_documento_erro_render")
})
