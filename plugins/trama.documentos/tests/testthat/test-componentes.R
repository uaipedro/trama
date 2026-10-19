# A materialização de cada classe de valor (portada dos adaptadores do
# trama.publicar), com as coleções reais.

ctx_de <- function(chave = "x", tipo = NULL, opcoes = list(), titulo = "Título") {
  list(no = chave, porta = "out", chave = chave, titulo = titulo, tipo = tipo, opcoes = opcoes,
       arquivo = function(n) paste0("_trama/", chave, "/", n))
}

registro_real <- function() {
  reg <- trama::tr_registry()
  for (p in c("trama.data", "trama.view", "trama.models")) trama::tr_use(p, registry = reg)
  reg
}

texto <- function(raw) strsplit(rawToChar(raw), "\n", fixed = TRUE)[[1]]

test_that("tabela: CSV inteiro, markdown com legenda e as primeiras linhas", {
  skip_if_not_installed("trama.data")
  tipo <- trama::tr_get_type("data/table", registro_real())
  p <- tr_documento_componente(datasets::mtcars, ctx_de("tab", tipo, list(linhas = "5"), "Carros"))
  expect_s3_class(p, "tr_documento_parte")
  lido <- utils::read.csv(text = rawToChar(p$arquivos$dados.csv))
  expect_equal(lido, datasets::mtcars, ignore_attr = TRUE, tolerance = 1e-12)
  md <- texto(p$arquivos$tabela.md)
  expect_identical(md[[1]], ": Carros {#tbl-tab}")
  expect_length(grep("^\\| [0-9]", md), 5L)
  expect_true(any(grepl("Primeiras 5 de 32 linhas", md)))
  # Vírgula decimal, como no relatório da trama.data: 21 e 2,62 (wt do Mazda RX4).
  expect_true(any(grepl("| 21 | 6 | 160 | 110 | 3,9 | 2,62 |", md, fixed = TRUE)))
  expect_identical(p$corpo, "{{< include _trama/tab/tabela.md >}}")
})

test_that("tabela sem report no tipo: o formato de reserva é o mesmo", {
  p <- tr_documento_componente(data.frame(a = c(1.23456, NA), b = c("x|y", "z")), ctx_de("t"))
  md <- texto(p$arquivos$tabela.md)
  expect_identical(md[3:6], c("| a | b |", "|---:|:---|", "| 1,235 | x\\|y |", "|  | z |"))
})

test_that("gráfico: PNG em qualidade de publicação, sem o objeto ggplot (que leva a tabela)", {
  g <- ggplot2::ggplot(datasets::mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  p <- tr_documento_componente(g, ctx_de("fig", titulo = "Peso e consumo [kg]"))
  expect_identical(names(p$arquivos), "figura.png")
  expect_identical(p$arquivos$figura.png[1:4], as.raw(c(0x89, 0x50, 0x4e, 0x47)))
  expect_identical(p$corpo, "![Peso e consumo \\[kg\\]](_trama/fig/figura.png){#fig-fig}")
  # 8 x 4,5 polegadas a 200 dpi.
  dim <- dim(png::readPNG(p$arquivos$figura.png))
  expect_identical(dim[1:2], c(900L, 1600L))
  ruim <- tr_documento_componente(g, ctx_de("fig", opcoes = list(formato = "gif")))
  expect_match(ruim$erro, "gif")
})

test_that("modelo: reduzido, com motor e bloco web; nada da tabela de treino", {
  skip_if_not_installed("trama.models")
  m <- trama.models::tr_models_lm(datasets::mtcars, formula = "mpg ~ wt + hp")
  p <- tr_documento_componente(m, ctx_de("lm", titulo = "Consumo"))
  expect_null(p$erro)
  expect_setequal(names(p$arquivos), c("modelo.rds", "motor.rds", "web.json", "modelo.md"))
  f <- withr::local_tempfile(fileext = ".rds")
  writeBin(p$arquivos$modelo.rds, f)
  r <- readRDS(f)
  expect_true(r$reduzido)
  expect_null(r$dados)
  expect_identical(jsonlite::fromJSON(rawToChar(p$arquivos$web.json))$saida$resposta, "mpg")
  expect_match(rawToChar(p$arquivos$modelo.md), "ajustad[oa] a 32 linhas; prevê 'mpg' a partir de wt, hp")
  expect_null(p$dados_treino)
  expect_identical(p$metadados$tipo_schema, "fair4ml:MLModel")
})

test_that("modelo sem tr_models_strip(): erro, e não o objeto inteiro", {
  skip_if_not_installed("trama.models")
  falso <- structure(list(dados = datasets::mtcars), class = c("tr_falso", "tr_models_fit"))
  p <- tr_documento_componente(falso, ctx_de("m"))
  expect_match(p$erro, "tr_models_strip")
  expect_length(p$arquivos, 0L)
})

test_that("teste: o report do tipo, com conclusão; sem tipo, o quadro do núcleo", {
  skip_if_not_installed("trama.data")
  t <- trama::tr_test("Shapiro-Wilk", h0 = "os dados são normais", estatistica = 0.95, rotulo_estat = "W",
                      p_valor = 0.2, conclusao_sim = "não normais", conclusao_nao = "sem evidência contra a normalidade")
  tipo <- trama::tr_get_type("data/test", registro_real())
  p <- tr_documento_componente(t, ctx_de("sw", tipo, titulo = "Normalidade"))
  md <- rawToChar(p$arquivos$teste.md)
  expect_match(md, "**Normalidade**", fixed = TRUE)
  expect_match(md, "sem evidência contra a normalidade", fixed = TRUE)
  expect_match(md, "0,2", fixed = TRUE)
  sem <- rawToChar(tr_documento_componente(t, ctx_de("sw"))$arquivos$teste.md)
  expect_match(sem, "sem evidência contra a normalidade", fixed = TRUE)
})

test_that("classe desconhecida: report do tipo, ou erro que diz como estender", {
  p <- tr_documento_componente(structure(list(), class = "coisa"), ctx_de("c"))
  expect_match(p$erro, "tr_documento_componente")
  tipo <- list(report = function(x) "Texto **gerado**.")
  p <- tr_documento_componente(structure(list(), class = "coisa"), ctx_de("c", tipo))
  expect_identical(texto(p$arquivos$resultado.md), c("**Título**", "", "Texto **gerado**."))
  g <- ggplot2::ggplot() + ggplot2::geom_blank()
  p <- tr_documento_componente(structure(list(), class = "coisa"), ctx_de("c", list(report = function(x) g)))
  expect_identical(names(p$arquivos), "figura.png")
})

test_that("o contrato da parte recusa caminho e o nome do manifesto", {
  expect_error(tr_documento_parte("x", arquivos = list("../fora.txt" = "a")), class = "tr_documento_erro_parte")
  expect_error(tr_documento_parte("x", arquivos = list("a/b.txt" = "a")), class = "tr_documento_erro_parte")
  expect_error(tr_documento_parte("x", arquivos = list("a")), class = "tr_documento_erro_parte")
  expect_error(tr_documento_parte("x", arquivos = list(trama.json = "{}")), class = "tr_documento_erro_parte")
  expect_identical(rawToChar(tr_documento_parte("x", arquivos = list(n.md = c("a", "b")))$arquivos$n.md), "a\nb")
})

test_that("de ponta a ponta com as coleções reais: gráfico e modelo no documento", {
  skip_if_not_installed("trama.models")
  local_usuario()
  raiz <- withr::local_tempdir()
  proj <- suppressMessages(trama::tr_project(raiz, collections = c("trama.data", "trama.view", "trama.models")))
  trama::tr_use(trama::tr_collection("tst2", nodes = list(
    trama::tr_node("tst2/carros", fn = function(n) utils::head(datasets::mtcars, n), description = "mtcars.",
                   outputs = list(out = "data/table"), params = list(n = trama::tr_param_int(32L))),
    trama::tr_node("tst2/fig", fn = function(dados) ggplot2::ggplot(dados, ggplot2::aes(wt, mpg)) + ggplot2::geom_point(),
                   description = "Dispersão.", inputs = list(dados = "data/table"), outputs = list(out = "view/plot")),
    trama::tr_node("tst2/lm", fn = function(dados) trama.models::tr_models_lm(dados, formula = "mpg ~ wt"),
                   description = "lm.", inputs = list(dados = "data/table"), outputs = list(out = "models/fit")))),
    registry = proj$registry)
  f <- trama::tr_flow(proj$registry) |>
    trama::tr_add("carros", "tst2/carros", n = 20L) |>
    trama::tr_add("fig", "tst2/fig", from = "carros", label = "Peso e consumo") |>
    trama::tr_add("lm", "tst2/lm", from = "carros")
  trama::tr_project_save(proj, trama::tr_flow_doc(f))
  qmd <- tr_documento_criar("artigo", proj, perfil = FALSE)
  for (n in c("carros", "fig", "lm")) tr_documento_inserir(qmd, n, proj)
  expect_identical(estado_de(qmd, proj), c(carros = "atualizada", fig = "atualizada", lm = "atualizada"))
  expect_true("![Peso e consumo](_trama/fig/figura.png){#fig-fig}" %in% linhas(qmd))
  md <- paste(linhas(file.path(dirname(qmd), "_trama/lm/modelo.md")), collapse = "\n")
  expect_match(md, "20 linhas")

  f <- trama::tr_set(f, "carros", n = 25L)
  trama::tr_project_save(proj, trama::tr_flow_doc(f))
  r <- tr_documento_atualizar(qmd, proj)
  expect_identical(r$estado, c("atualizada", "atualizada", "atualizada"))
  expect_match(paste(linhas(file.path(dirname(qmd), "_trama/lm/modelo.md")), collapse = "\n"), "25 linhas")
  # A região do modelo é só o include: o corpo não mudou, só a pasta.
  expect_identical(regiao(qmd, "lm")$conteudo, "{{< include _trama/lm/modelo.md >}}")
})
