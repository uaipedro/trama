# Modelos: o Básico embutido e modelos do usuário, que têm precedência.

test_that("o Básico é embutido, com html, typst e docx", {
  local_usuario()
  m <- tr_documento_modelos()
  b <- m[m$nome == "basico", ]
  expect_identical(b$titulo, "Básico")
  expect_identical(b$formatos, "html, typst, docx")
  expect_identical(b$origem, "embutido")
  dir <- system.file("modelos/basico", package = "trama.documentos")
  expect_true(all(file.exists(file.path(dir, c("_extension.yml", "_brand.yml", "template.qmd", "estilo.css")))))
})

test_that("modelo do usuário: listado, com precedência, e copiado para o documento", {
  local_usuario()
  meu <- file.path(tools::R_user_dir("trama", "data"), "modelos", "meu")
  dir.create(file.path(meu, "partes"), recursive = TRUE)
  writeLines(c("title: Meu", "contributes:", "  formats:", "    html:", "      toc: false"), file.path(meu, "_extension.yml"))
  writeLines(c("---", "title: Meu modelo", "---", "", "# Capa"), file.path(meu, "template.qmd"))
  writeLines("/* parte */", file.path(meu, "partes", "x.css"))
  m <- tr_documento_modelos()
  expect_identical(m$origem[m$nome == "meu"], "usuario")

  proj <- local_projeto()
  # local_projeto() troca a pasta do usuário: recria o modelo nela.
  meu2 <- file.path(tools::R_user_dir("trama", "data"), "modelos", "meu")
  dir.create(dirname(meu2), recursive = TRUE)
  file.copy(meu, dirname(meu2), recursive = TRUE)
  qmd <- tr_documento_criar("doc", proj, modelo = "meu", perfil = FALSE)
  pasta <- dirname(qmd)
  expect_true(file.exists(file.path(pasta, "_extensions/meu/partes/x.css")))
  expect_false(file.exists(file.path(pasta, "_brand.yml")))
  y <- trama.documentos:::.td_yaml(linhas(qmd))
  expect_identical(names(y$format), "meu-html")
  expect_identical(y$title, "Meu modelo")
  expect_identical(tail(linhas(qmd), 1), "# Capa")
  expect_error(tr_documento_criar("x", proj, modelo = "nao-existe"), class = "tr_documento_erro_modelo")
})

test_that("criar recusa pasta que não é projeto", {
  local_usuario()
  expect_error(tr_documento_criar("x", withr::local_tempdir()), class = "tr_documento_erro_projeto")
})
