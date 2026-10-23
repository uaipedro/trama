# O tema dos gráficos do documento: claro, com as cores do `_brand.yml` do
# documento, no lugar do "padrão" do projeto (que costuma ser o escuro).

tema_de <- function(brand) trama.documentos:::.td_tema_documento(brand)

test_that("o tema vem do _brand.yml: fundo claro, cores que se leem, paleta completa", {
  t <- tema_de(yaml::read_yaml(system.file("modelos/consultoria/_brand.yml", package = "trama.documentos")))
  expect_identical(t$fundo, "#ffffff")
  expect_identical(t$texto, "#1f2937")
  # A primária da marca abre a paleta; o azul-gelo (secundária) não se lê
  # sobre o branco e fica de fora.
  expect_identical(t$paleta[[1]], "#003366")
  expect_false("#e6eff6" %in% t$paleta)
  expect_length(t$paleta, 8L)
  expect_identical(t$fonte, "sans")
  expect_identical(names(t), c("base", "tamanho", "fonte", "fundo", "texto", "eixos", "grade", "paleta", "continua"))
  expect_true(grepl("^#[0-9a-f]{6}$", t$eixos) && grepl("^#[0-9a-f]{6}$", t$grade))
  expect_identical(tema_de(yaml::read_yaml(system.file("modelos/artigo/_brand.yml", package = "trama.documentos")))$fonte,
                   "serif")
})

test_that("marca escura ou sem cores ainda dá tema claro", {
  escuro <- tema_de(list(color = list(background = "#11151c", foreground = "#c9d1d9", primary = "#5b8def")))
  expect_identical(escuro$fundo, "#ffffff")
  expect_identical(escuro$texto, "#1f2937")  # o texto claro sumiria no branco
  expect_identical(escuro$paleta[[1]], "#5b8def")
  vazio <- tema_de(list())
  expect_identical(vazio$fundo, "#ffffff")
  expect_identical(vazio$paleta[[1]], "#2563eb")  # a do tema claro do núcleo
  # Nome da paleta e cor curta resolvem.
  t <- tema_de(list(color = list(palette = list(vinho = "#7b2d26"), primary = "vinho", background = "#fff")))
  expect_identical(t$paleta[[1]], "#7b2d26")
  expect_identical(t$fundo, "#ffffff")
})

test_that("o nó em tema 'padrão' segue o documento; o escolhido à mão fica", {
  proj <- local_projeto()
  salvar_fluxo(proj, no("padrao", "tst/tema"), no("fixo", "tst/tema", tema = "escuro"))
  qmd <- tr_documento_criar("rel", proj, modelo = "consultoria", perfil = FALSE)
  tr_documento_inserir(qmd, "padrao", proj)
  tr_documento_inserir(qmd, "fixo", proj)
  csv <- function(chave) utils::read.csv(file.path(dirname(qmd), "_trama", chave, "dados.csv"))
  p <- csv("padrao")
  expect_identical(p$nome, "trama-documento")
  expect_identical(p$fundo, "#ffffff")
  expect_identical(p$cor1, "#003366")
  f <- csv("fixo")
  expect_identical(f$nome, "escuro")
  expect_identical(f$fundo, "#11151c")
  expect_identical(unname(estado_de(qmd, proj)), c("atualizada", "atualizada"))

  # Trocar a cor do _brand.yml do documento deixa a região do "padrão"
  # desatualizada, e atualizar traz a cor nova; a do tema fixo não muda.
  b <- file.path(dirname(qmd), "_brand.yml")
  marca <- yaml::read_yaml(b)
  marca$color$palette$`azul-yucca` <- "#2f6b3a"
  yaml::write_yaml(marca, b)
  expect_identical(estado_de(qmd, proj)[["padrao"]], "desatualizada")
  expect_identical(estado_de(qmd, proj)[["fixo"]], "atualizada")
  tr_documento_atualizar(qmd, proj)
  expect_identical(csv("padrao")$cor1, "#2f6b3a")
})

test_that("o projeto de quem chama não muda", {
  proj <- local_projeto()
  salvar_fluxo(proj, no("padrao", "tst/tema"))
  antes <- proj$settings
  qmd <- tr_documento_criar("rel", proj, perfil = FALSE)
  tr_documento_inserir(qmd, "padrao", proj)
  expect_identical(proj$settings, antes)
  expect_identical(trama::tr_value(trama::tr_project_flow(proj), "padrao", registry = proj$registry,
                                   store = proj$store, settings = proj$settings)$nome, "escuro")
})
