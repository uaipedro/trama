# O destino Página web (#25): o modelo com o quarto-live fixado e o modelo
# estatístico que vira preditor no navegador.

test_that("o modelo Página web cria um documento com WebR: formato, knitr, recursos e variáveis", {
  proj <- local_projeto()
  qmd <- tr_documento_criar("pagina", proj, modelo = "pagina", perfil = FALSE, titulo = "Consumo e peso")
  pasta <- dirname(qmd)
  y <- trama.documentos:::.td_yaml(linhas(qmd))
  expect_identical(names(y$format), "pagina-html")
  expect_identical(y$engine, "knitr")
  expect_identical(y$resources, "_trama/")
  expect_identical(y$trama$publicar$licenca, "CC-BY-4.0")
  expect_identical(y$trama$publicar[["licenca-codigo"]], "MIT")
  expect_identical(y$trama$publicar$versao, "menor")
  # O setup do knitr aponta para a cópia da extensão no documento.
  expect_true("{{< include _extensions/pagina/_knitr.qmd >}}" %in% linhas(qmd))
  expect_true(file.exists(file.path(pasta, "_extensions/pagina/live.lua")))
  expect_true(file.exists(file.path(pasta, "_extensions/pagina/templates/webr-setup.ojs")))
  v <- yaml::read_yaml(file.path(pasta, "_variables.yml"))
  expect_identical(v$versao, "rascunho")
  expect_identical(v$licenca, "[CC-BY-4.0](https://spdx.org/licenses/CC-BY-4.0)")
  expect_true(trama.documentos:::.td_web(linhas(qmd), pasta))
  # O Básico não roda R no navegador e não ganha _variables.yml.
  b <- tr_documento_criar("basico", proj, perfil = FALSE)
  expect_false(trama.documentos:::.td_web(linhas(b), dirname(b)))
  expect_false(file.exists(file.path(dirname(b), "_variables.yml")))
})

test_that("modelo duplicado com outro nome acha o próprio setup do knitr", {
  local_usuario()
  proj <- local_projeto()
  tr_documento_modelo_duplicar("pagina", "minha-pagina")
  qmd <- tr_documento_criar("p2", proj, modelo = "minha-pagina", perfil = FALSE)
  expect_true("{{< include _extensions/minha-pagina/_knitr.qmd >}}" %in% linhas(qmd))
  expect_true(trama.documentos:::.td_web(linhas(qmd), dirname(qmd)))
})

web_exemplo <- function() {
  list(entradas = list(list(nome = "wt", tipo = "numero", min = 1.5, max = 5.4, padrao = 3.5, passo = 0.1),
                       list(nome = "cyl", tipo = "fator", niveis = c("4", "6", "8"), padrao = "6"),
                       list(nome = "am", tipo = "logico")),
       prever = "function(motor, novos) data.frame(previsto = 1)",
       pacotes = list(stats = "4.6.0", ranger = "0.17.0"),
       saida = list(resposta = "mpg", tarefa = "regressao"),
       preprocessamento = "Nenhum.")
}

test_that("o preditor: um controle por entrada, o motor da mesma origem, os pacotes e as versões", {
  l <- trama.documentos:::.td_preditor("lm", "_trama/lm/motor.rds", web_exemplo(), r = "4.6.0")
  expect_true(any(grepl("viewof v_lm_wt = Inputs.range([1.5, 5.4], {value: 3.5, step: 0.1, label: \"wt\"})", l, fixed = TRUE)))
  expect_true(any(grepl("viewof v_lm_cyl = Inputs.select([\"4\",\"6\",\"8\"]", l, fixed = TRUE)))
  expect_true(any(grepl("viewof v_lm_am = Inputs.toggle", l, fixed = TRUE)))
  # Endereço absoluto montado no navegador (o worker do WebR não tem base).
  expect_true(any(grepl("v_lm_motor_url = new URL(\"_trama/lm/motor.rds\", document.baseURI).href", l, fixed = TRUE)))
  expect_true(any(grepl("#|   - v_lm_motor_url", l, fixed = TRUE)))
  # Pacote fora da imagem base do WebR é instalado na célula; stats não.
  expect_true(any(grepl("for (p in c(\"ranger\")) if (!requireNamespace(p, quietly = TRUE)) webr::install(p)", l, fixed = TRUE)))
  expect_true(any(grepl("novos <- data.frame(`wt` = v_lm_wt, `cyl` = v_lm_cyl, `am` = v_lm_am, check.names = FALSE)", l, fixed = TRUE)))
  expect_true(any(grepl("mpg previsto", l, fixed = TRUE)))
  # Versões do treino × navegador, recolhidas.
  expect_true(any(grepl("treino <- c(`R` = \"4.6.0\", `ranger` = \"0.17.0\")", l, fixed = TRUE)))
  expect_true("::: {.callout-note collapse=\"true\"}" %in% l)
  # Classificação mostra a classe e as probabilidades.
  w <- web_exemplo(); w$saida$tarefa <- "classificacao"; w$pacotes <- list(stats = "4.6.0")
  l2 <- trama.documentos:::.td_preditor("arv", "_trama/arv/motor.rds", w)
  expect_true(any(grepl("Classe prevista para mpg", l2, fixed = TRUE)))
  expect_false(any(grepl("webr::install", l2, fixed = TRUE)))
})

test_that("modelo inserido numa Página web vira preditor; num Básico, não", {
  skip_if_not_installed("trama.models")
  local_usuario()
  raiz <- withr::local_tempdir()
  proj <- suppressMessages(trama::tr_project(raiz, collections = c("trama.data", "trama.models")))
  f <- trama::tr_flow(proj$registry) |>
    trama::tr_add("dados", "data/example", dataset = "mtcars") |>
    trama::tr_add("lm", "models/lm", from = "dados", resposta = "mpg", preditores = c("wt", "hp"))
  trama::tr_project_save(proj, trama::tr_flow_doc(f))
  web <- tr_documento_criar("web", proj, modelo = "pagina", perfil = FALSE)
  tr_documento_inserir(web, "lm", proj)
  r <- regiao(web, "lm")
  expect_true("{{< include _trama/lm/preditor.qmd >}}" %in% r$conteudo)
  pred <- linhas(file.path(dirname(web), "_trama/lm/preditor.qmd"))
  expect_true(any(grepl("```{webr}", pred, fixed = TRUE)))
  expect_true(any(grepl("prever <- function", pred, fixed = TRUE)))
  expect_identical(unname(estado_de(web, proj)), "atualizada")
  b <- tr_documento_criar("rel", proj, perfil = FALSE)
  tr_documento_inserir(b, "lm", proj)
  expect_false(file.exists(file.path(dirname(b), "_trama/lm/preditor.qmd")))
  expect_false(any(grepl("preditor", regiao(b, "lm")$conteudo)))
})
