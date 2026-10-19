# Publicar um documento no GitHub (#25): conferência, pacote RO-Crate, envio
# e acompanhamento. O GitHub é um GitHub em memória no lugar das funções do
# trama.github (a API delas tem os próprios testes, contra o GitHub falso);
# o conferir.py e o paginas.sh do workflow rodam de verdade.

test_that("metadados do YAML: autores com ORCID conferido, licenças e tipo de versão", {
  m <- trama.documentos:::.td_pub_meta(list(title = "T", author = autores_exemplo, keywords = c("a", "b"),
                                            description = "R", trama = list(publicar = list(versao = "maior"))))
  expect_identical(m$erros, character())
  expect_identical(m$autores[[1]]$orcid, "0000-0002-1825-0097")
  expect_true(m$autores[[1]]$orcid_valido)
  expect_null(m$autores[[2]]$orcid)
  expect_identical(c(m$licenca, m$licenca_codigo, m$tipo_versao), c("CC-BY-4.0", "MIT", "maior"))
  expect_identical(m$palavras_chave, c("a", "b"))
  # Formas do `author` do Quarto: texto, lista de textos, nome em partes.
  expect_identical(trama.documentos:::.td_pub_autores("Ana")[[1]]$nome, "Ana")
  expect_identical(vapply(trama.documentos:::.td_pub_autores(c("Ana", "Bia")), `[[`, "", "nome"), c("Ana", "Bia"))
  expect_identical(trama.documentos:::.td_pub_autores(list(list(name = list(given = "Ana", family = "Lima"))))[[1]]$nome, "Ana Lima")
  ruim <- trama.documentos:::.td_pub_meta(list(author = list(list(name = "X", orcid = "0000-0002-1825-0098")),
                                               trama = list(publicar = list(licenca = "GPL", versao = "nova"))))
  expect_match(ruim$erros, "Falta o título", all = FALSE)
  expect_match(ruim$erros, "ORCID de X não confere", all = FALSE)
  expect_match(ruim$erros, "Licença desconhecida: GPL", all = FALSE)
  expect_match(ruim$erros, "Tipo de versão desconhecido: nova", all = FALSE)
  expect_match(trama.documentos:::.td_pub_meta(list(title = "T"))$erros, "ao menos um autor")
})

test_that("versões: próxima pelo tipo, a partir da maior tag vX.Y.Z", {
  expect_identical(trama.documentos:::.td_proxima("menor", "0.1.0"), "0.2.0")
  expect_identical(trama.documentos:::.td_proxima("correção", "0.1.0"), "0.1.1")
  expect_identical(trama.documentos:::.td_proxima("maior", "0.3.2"), "1.0.0")
  expect_identical(trama.documentos:::.td_ultima_versao(c("v0.1.0", "v0.10.0", "v0.9.1", "rascunho")), "0.10.0")
  expect_identical(trama.documentos:::.td_ultima_versao(character()), "0.0.0")
})

test_that("conferência: região desatualizada, dados de treino sem aceite e documento sem HTML bloqueiam", {
  x <- doc_publicavel()
  pub <- tr_documento_publicacao(x$qmd, x$proj)
  expect_s3_class(pub, "tr_documento_publicacao")
  expect_identical(pub$erros, character())
  expect_identical(pub$metadados$titulo, "Consumo e peso")
  expect_identical(vapply(pub$regioes, `[[`, "", "chave"), "tab")
  # O fluxo muda: a região fica para trás e a publicação espera o "atualizar".
  salvar_fluxo(x$proj, no("tab", n = 5L), no("mod", "tst/modelo"))
  expect_match(tr_documento_publicacao(x$qmd, x$proj)$erros, "#trama-tab está desatualizada", all = FALSE)
  # Antes de publicar (que atualiza), é só aviso.
  antes <- tr_documento_publicacao(x$qmd, x$proj, desatualizada = "aviso")
  expect_identical(antes$erros, character())
  expect_match(antes$avisos, "publicar atualiza", all = FALSE)
  tr_documento_atualizar(x$qmd, x$proj)
  # Modelo que carrega linhas do treino: aceito para o documento, mas
  # publicar pede o aceite próprio, no YAML.
  tr_documento_inserir(x$qmd, "mod", x$proj, aceitar_dados_treino = TRUE)
  expect_match(tr_documento_publicacao(x$qmd, x$proj)$erros, "aceitar-dados-treino", all = FALSE)
  mudar_yaml(x$qmd, function(y) { y$trama$publicar[["aceitar-dados-treino"]] <- TRUE; y })
  pub <- tr_documento_publicacao(x$qmd, x$proj)
  expect_identical(pub$erros, character())
  expect_match(pub$avisos, "publicada com dados de treino", all = FALSE)
  # Tabela inteira é aviso: fica pública.
  tr_documento_resolver(x$qmd, "tab", "remover")
  tr_documento_inserir(x$qmd, "tab", x$proj, opcoes = list(tabela = "completa"))
  expect_match(tr_documento_publicacao(x$qmd, x$proj)$avisos, "tabela inteira", all = FALSE)
  # Só PDF: o Pages não tem o que mostrar.
  mudar_yaml(x$qmd, function(y) { y$format <- list("basico-typst" = "default"); y })
  expect_match(tr_documento_publicacao(x$qmd, x$proj)$erros, "formato HTML", all = FALSE)
})

test_that("bloco R da pessoa e caminho absoluto no fluxo viram aviso", {
  x <- doc_publicavel()
  cat("\n```{r}\nlibrary(dplyr)\n```\n", file = x$qmd, append = TRUE)
  salvar_fluxo(x$proj, no("tab", n = 4L), no("arq", "tst/texto", valor = "/home/fulana/dados.csv"))
  pub <- tr_documento_publicacao(x$qmd, x$proj)
  expect_match(pub$avisos, "blocos de código R seus", all = FALSE)
  expect_match(pub$avisos, "/home/fulana/dados.csv", all = FALSE)
})

test_that("o pacote: documento como index.qmd, só o _trama/ usado, sem _saida, RO-Crate completo", {
  x <- doc_publicavel()
  pasta <- dirname(x$qmd)
  dir.create(file.path(pasta, "_trama", "velha")); writeLines("x", file.path(pasta, "_trama", "velha", "a.md"))
  dir.create(file.path(pasta, "_saida")); writeLines("x", file.path(pasta, "_saida", "consumo.html"))
  writeLines("bib", file.path(pasta, "refs.bib"))
  dir <- file.path(withr::local_tempdir(), "pacote")
  p <- tr_documento_pacote(x$qmd, dir, x$proj, versao = "0.2.0", repositorio = "https://github.com/p/consumo",
                           data = as.Date("2026-10-19"))
  a <- p$arquivos
  expect_true(all(c("index.qmd", "_quarto.yml", "_variables.yml", "_brand.yml", "_trama/tab/tabela.md",
                    "_trama/tab/dados.csv", "_trama/tab/trama.json", "_extensions/pagina/live.lua", "refs.bib",
                    "flow.json", "fluxo.svg", "dados-entrada.json", "ambiente.json", "README.md", "CITATION.cff",
                    "LICENSE", "ro-crate-metadata.json") %in% a))
  expect_false(any(grepl("^_saida/|^_trama/velha/|consumo\\.qmd$|\\.gitignore", a)))
  expect_identical(linhas(file.path(dir, "index.qmd")), linhas(x$qmd))
  # O Pages serve a página ao lado do index.qmd: sem output-dir.
  expect_null(yaml::read_yaml(file.path(dir, "_quarto.yml"))$project[["output-dir"]])
  v <- yaml::read_yaml(file.path(dir, "_variables.yml"))
  expect_identical(v$versao, "0.2.0")
  expect_identical(v$citacao, "Josiah Carberry; Ana Lima (2026). Consumo e peso. Versão 0.2.0. https://github.com/p/consumo.")
  cff <- linhas(file.path(dir, "CITATION.cff"))
  expect_true(all(c("version: \"0.2.0\"", "date-released: \"2026-10-19\"", "repository: \"https://github.com/p/consumo\"",
                    "    orcid: \"https://orcid.org/0000-0002-1825-0097\"") %in% cff))
  crate <- jsonlite::fromJSON(file.path(dir, "ro-crate-metadata.json"), simplifyVector = FALSE)
  grafo <- stats::setNames(crate[["@graph"]], vapply(crate[["@graph"]], function(g) g[["@id"]], ""))
  raiz <- grafo[["./"]]
  expect_identical(raiz$version, "0.2.0")
  expect_identical(raiz$name, "Consumo e peso")
  partes <- vapply(raiz$hasPart, function(h) h[["@id"]], "")
  expect_setequal(partes, c(setdiff(a[!grepl("^_extensions/", a)], "ro-crate-metadata.json"), "_extensions/"))
  expect_identical(grafo[["_trama/tab/dados.csv"]]$variableMeasured[[1]]$name, "x")
  expect_identical(grafo[["_trama/tab/dados.csv"]]$sha256, digest::digest(file = file.path(dir, "_trama/tab/dados.csv"), algo = "sha256"))
  expect_match(grafo[["_extensions/"]]$name, "quarto-live 0.2.0")
  expect_identical(grafo[["https://orcid.org/0000-0002-1825-0097"]]$name, "Josiah Carberry")
  # Determinístico: a mesma data dá o mesmo pacote.
  dir2 <- file.path(withr::local_tempdir(), "pacote")
  tr_documento_pacote(x$qmd, dir2, x$proj, versao = "0.2.0", repositorio = "https://github.com/p/consumo",
                      data = as.Date("2026-10-19"))
  sha <- function(d) vapply(file.path(d, a), function(f) digest::digest(file = f, algo = "sha256"), "")
  expect_identical(unname(sha(dir)), unname(sha(dir2)))
})

test_that("pacote com bloqueio não sai", {
  x <- doc_publicavel()
  mudar_yaml(x$qmd, function(y) { y$author <- NULL; y })
  expect_error(tr_documento_pacote(x$qmd, withr::local_tempdir(), x$proj), "ao menos um autor",
               class = "tr_documento_erro_publicar")
})

test_that("conferir.py aceita o pacote montado e recusa um arquivo adulterado ou solto", {
  py <- Sys.which("python3")
  skip_if(!nzchar(py), "python3 ausente")
  x <- doc_publicavel()
  dir <- file.path(withr::local_tempdir(), "pacote")
  tr_documento_pacote(x$qmd, dir, x$proj, versao = "0.1.0", data = as.Date("2026-10-19"))
  script <- system.file("github", "conferir.py", package = "trama.documentos")
  ok <- processx::run(py, script, wd = dir, error_on_status = FALSE)
  expect_identical(ok$status, 0L, label = ok$stderr)
  cat("x", file = file.path(dir, "README.md"), append = TRUE)
  writeLines("solto", file.path(dir, "solto.txt"))
  ruim <- processx::run(py, script, wd = dir, error_on_status = FALSE)
  expect_identical(ruim$status, 1L)
  expect_match(ruim$stderr, "sha256 não confere: README.md", fixed = TRUE)
  expect_match(ruim$stderr, "fora do ro-crate-metadata.json: solto.txt", fixed = TRUE)
})

test_that("o workflow fixa as versões das ações, do Quarto e do R", {
  wf <- readLines(system.file("github", "publicar.yml", package = "trama.documentos"))
  usos <- trimws(sub(".*uses:", "", grep("uses:", wf, value = TRUE)))
  expect_true(all(grepl("@v[0-9]+\\.[0-9]+\\.[0-9]+$", usos)), label = paste(usos, collapse = ", "))
  expect_true(any(grepl("version: 1.7.32", wf, fixed = TRUE)))
  expect_true(any(grepl("r-version: \"4.6.0\"", wf, fixed = TRUE)))
  expect_true(any(grepl("tags: [\"v*\"]", wf, fixed = TRUE)))
  expect_true(any(grepl("quarto render index.qmd", wf, fixed = TRUE)))
})

test_that("paginas.sh acumula as versões na branch paginas e não reescreve uma publicada", {
  skip_on_os("windows")
  git <- Sys.which("git")
  skip_if(!nzchar(git), "git ausente")
  raiz <- withr::local_tempdir()
  run <- function(..., wd, env = character()) processx::run(git, c(...), wd = wd, error_on_status = FALSE, env = c("current", env))
  origem <- file.path(raiz, "origem.git"); run("init", "--bare", "-q", origem, wd = raiz)
  trab <- file.path(raiz, "trab"); run("clone", "-q", origem, trab, wd = raiz)
  idt <- c(GIT_AUTHOR_NAME = "t", GIT_AUTHOR_EMAIL = "t@t", GIT_COMMITTER_NAME = "t", GIT_COMMITTER_EMAIL = "t@t")
  writeLines("pacote", file.path(trab, "a.txt"))
  run("add", "-A", wd = trab); run("commit", "-q", "-m", "p", wd = trab, env = idt); run("push", "-q", "origin", "HEAD", wd = trab)
  script <- system.file("github", "paginas.sh", package = "trama.documentos")
  publicar <- function(v) {
    unlink(file.path(trab, c("_build", "_site", "_paginas")), recursive = TRUE)
    run("worktree", "prune", wd = trab)
    dir.create(file.path(trab, "_build"))
    writeLines(sprintf("<p>%s</p>", v), file.path(trab, "_build", "index.html"))
    processx::run("bash", script, wd = trab, error_on_status = FALSE, env = c("current", idt, GITHUB_REF_NAME = v))
  }
  r1 <- publicar("v0.1.0")
  expect_identical(r1$status, 0L, label = r1$stderr)
  r2 <- publicar("v0.2.0")
  expect_identical(r2$status, 0L, label = r2$stderr)
  site <- file.path(trab, "_site")
  expect_true(all(file.exists(file.path(site, c("v0.1.0/index.html", "v0.2.0/index.html", "ultimo/index.html",
                                                "index.html", ".nojekyll")))))
  expect_identical(readLines(file.path(site, "ultimo/index.html")), "<p>v0.2.0</p>")
  expect_identical(readLines(file.path(site, "v0.1.0/index.html")), "<p>v0.1.0</p>")
  r3 <- publicar("v0.1.0")
  expect_identical(r3$status, 1L)
  expect_match(r3$stderr, "já está publicada")
})

# ---- envio ---------------------------------------------------------------------------

test_that("bloqueios e falta de conta saem antes de tocar no GitHub", {
  gh <- github_memoria()
  x <- doc_publicavel()
  mudar_yaml(x$qmd, function(y) { y$author <- NULL; y })
  r <- tr_documento_publicar(x$qmd, x$proj)
  expect_match(unlist(r$bloqueios), "autor", all = FALSE)
  expect_length(gh$chamadas, 0L)
  mudar_yaml(x$qmd, function(y) { y$author <- autores_exemplo; y })
  gh2 <- github_memoria(token = NULL)
  expect_match(unlist(tr_documento_publicar(x$qmd, x$proj)$bloqueios), "menu GitHub")
})

test_that("primeiro envio: repositório público, Pages, ambiente, um commit com o pacote e a tag v0.1.0", {
  gh <- github_memoria()
  x <- doc_publicavel()
  e <- tr_documento_publicar(x$qmd, x$proj)
  expect_identical(gh$chamadas, c("criar_repo", "pages", "ambiente", "commit", "tag"))
  expect_identical(e$repo, "pesquisadora/consumo-e-peso")
  expect_false(gh$repos[["pesquisadora/consumo-e-peso"]]$privado)
  expect_identical(e$tag, "v0.1.0")
  expect_identical(e$pagina, "https://pesquisadora.github.io/consumo-e-peso/v0.1.0/")
  expect_identical(e$ultimo, "https://pesquisadora.github.io/consumo-e-peso/ultimo/")
  expect_identical(e$etapa, "enviado")
  expect_identical(e$documento, "documentos/consumo/consumo.qmd")
  cm <- gh$commits[[1]]
  expect_false(cm$manter_outros)  # a árvore passa a ser exatamente o pacote
  expect_true(all(c("ro-crate-metadata.json", "index.qmd", "CITATION.cff", "flow.json", "_trama/tab/tabela.md",
                    "_variables.yml") %in% names(cm$arquivos)))
  expect_true(all(c(".github/workflows/publicar.yml", ".github/trama/conferir.py", ".github/trama/paginas.sh",
                    ".github/trama/notas.md", ".gitattributes") %in% names(cm$conteudos)))
  expect_match(txt(cm$arquivos[["_variables.yml"]]), "versao: 0.1.0")
  expect_match(cm$conteudos[[".github/trama/notas.md"]], "https://pesquisadora.github.io/consumo-e-peso/v0.1.0/", fixed = TRUE)
  # O estado fica no projeto, para o painel retomar.
  expect_identical(trama.documentos:::.td_envio_ler(x$raiz, e$documento)$sha, e$sha)
})

test_that("o registro é do documento, no trama.json do projeto, e o próximo envio segue no mesmo repositório", {
  gh <- github_memoria()
  x <- doc_publicavel()
  expect_identical(nrow(tr_documento_publicacoes(x$raiz)), 0L)
  tr_documento_publicar(x$qmd, x$proj)
  p <- tr_documento_publicacoes(x$raiz)
  expect_identical(p$documento, "documentos/consumo/consumo.qmd")
  expect_identical(p$repo, "pesquisadora/consumo-e-peso")
  expect_identical(p$pagina, "https://pesquisadora.github.io/consumo-e-peso/")
  expect_identical(p$versao, "0.1.0")
  expect_identical(p$data, format(Sys.Date(), "%Y-%m-%d"))
  # Trocar o título não abre outro repositório; a versão segue a última tag.
  mudar_yaml(x$qmd, function(y) { y$title <- "Outro título"; y })
  e <- tr_documento_publicar(x$qmd, x$proj)
  expect_identical(e$repo, "pesquisadora/consumo-e-peso")
  expect_identical(e$tag, "v0.2.0")
  expect_identical(sum(gh$chamadas == "criar_repo"), 1L)
  expect_identical(tr_documento_publicacoes(x$raiz)$versao, "0.2.0")
})

test_that("nada do registro nem do repositório privado do projeto vai no pacote público", {
  gh <- github_memoria()
  x <- doc_publicavel()
  trama::tr_project_config_set(x$raiz, "versionar", list(repo = "pesquisadora/rascunho-secreto",
                                                         url = "https://github.com/pesquisadora/rascunho-secreto"))
  tr_documento_publicar(x$qmd, x$proj)
  cm <- gh$commits[[1]]
  expect_false("trama.json" %in% c(names(cm$arquivos), names(cm$conteudos)))
  todos <- c(vapply(cm$arquivos, txt, ""), unlist(cm$conteudos))
  expect_false(any(grepl("rascunho-secreto", todos, fixed = TRUE)))
  expect_false(any(grepl("publicacoes", todos, fixed = TRUE)))
})

test_that("repositório privado novo e repositório alheio pedem confirmação", {
  gh <- github_memoria()
  x <- doc_publicavel()
  mudar_yaml(x$qmd, function(y) { y$trama$publicar$privado <- TRUE; y })
  r <- tr_documento_publicar(x$qmd, x$proj)
  expect_match(r$confirmar, "plano pago")
  expect_length(gh$chamadas, 0L)
  e <- tr_documento_publicar(x$qmd, x$proj, confirmado = TRUE)
  expect_true(gh$repos[[e$repo]]$privado)

  gh2 <- github_memoria(repos = list("pesquisadora/meu" = list(repo = "pesquisadora/meu", url = "u")))
  y <- doc_publicavel()
  r2 <- tr_documento_publicar(y$qmd, y$proj, repositorio = "meu")
  expect_match(r2$confirmar, "substitui todo o conteúdo")
  expect_false("commit" %in% gh2$chamadas)
})

test_that("tag que já existe é recusada com a próxima versão sugerida", {
  gh <- github_memoria()
  local_mocked_bindings(.td_ultima_versao = function(tags) "0.0.0", .package = "trama.documentos")
  x <- doc_publicavel()
  tr_documento_publicar(x$qmd, x$proj)
  expect_error(tr_documento_publicar(x$qmd, x$proj), "v0.2.0", class = "tr_documento_erro_publicar")
})

test_that("acompanhar: Actions rodando, release, página no ar, e falha com etapa e log", {
  gh <- github_memoria()
  x <- doc_publicavel()
  expect_null(tr_documento_acompanhar(x$qmd, x$proj))
  tr_documento_publicar(x$qmd, x$proj)
  a <- function() tr_documento_acompanhar(x$qmd, x$proj)
  expect_identical(a()$etapa, "enviado")
  gh$runs <- list(list(id = 1, nome = "publicar", estado = "in_progress", conclusao = NA, url = "https://run/1"))
  expect_identical(a()$etapa, "actions")
  gh$runs[[1]]$estado <- "completed"; gh$runs[[1]]$conclusao <- "success"
  gh$releases[["v0.1.0"]] <- list(tag = "v0.1.0", url = "https://rel")
  local_mocked_bindings(.td_pagina_no_ar = function(url) FALSE, .package = "trama.documentos")
  r <- a()
  expect_identical(r$etapa, "release")
  expect_identical(r$release, "https://rel")
  local_mocked_bindings(.td_pagina_no_ar = function(url) TRUE, .package = "trama.documentos")
  expect_identical(a()$etapa, "no_ar")
  expect_identical(a()$etapa, "no_ar")  # terminado não consulta mais o GitHub

  y <- doc_publicavel()
  tr_documento_publicar(y$qmd, y$proj)
  gh$runs <- list(list(id = 2, nome = "publicar", estado = "completed", conclusao = "failure", url = "https://run/2"))
  f <- tr_documento_acompanhar(y$qmd, y$proj)
  expect_identical(f$etapa, "falhou")
  expect_match(f$detalhe, "Renderizar a página")
  expect_identical(f$log, "https://log/1")
})

test_that("a pasta do documento: extras são aviso, dados e nomes do pacote bloqueiam", {
  x <- doc_publicavel()
  pasta <- dirname(x$qmd)
  writeLines("@book{a}", file.path(pasta, "refs.bib"))
  pub <- tr_documento_publicacao(x$qmd, x$proj)
  expect_match(pub$avisos, "Vai junto, da pasta do documento: refs.bib", all = FALSE)
  expect_identical(pub$erros, character())
  writeLines("cpf,nome", file.path(pasta, "clientes.csv"))
  writeLines("# meu", file.path(pasta, "README.md"))
  pub <- tr_documento_publicacao(x$qmd, x$proj)
  expect_match(pub$erros, "arquivos de dados \\(clientes.csv\\)", all = FALSE)
  expect_match(pub$erros, "README.md, que o pacote gera", all = FALSE)
  unlink(file.path(pasta, "README.md"))
  mudar_yaml(x$qmd, function(y) { y$trama$publicar[["incluir-dados"]] <- TRUE; y })
  expect_identical(tr_documento_publicacao(x$qmd, x$proj)$erros, character())
})

test_that("dois formatos HTML bloqueiam: o Actions renderiza tudo na mesma pasta", {
  x <- doc_publicavel()
  mudar_yaml(x$qmd, function(y) { y$format <- list("pagina-html" = "default", revealjs = "default"); y })
  expect_match(tr_documento_publicacao(x$qmd, x$proj)$erros, "mais de um formato HTML", all = FALSE)
})

test_that("repositório de outro documento, ou o privado do projeto, é recusado antes do GitHub", {
  gh <- github_memoria()
  x <- doc_publicavel()
  tr_documento_publicar(x$qmd, x$proj)
  outro <- tr_documento_criar("outro", x$proj, modelo = "pagina", perfil = FALSE, titulo = "Consumo e peso")
  mudar_yaml(outro, function(y) { y$author <- autores_exemplo; y })
  n <- length(gh$chamadas)
  r <- tr_documento_publicar(outro, x$proj)
  expect_match(unlist(r$bloqueios), "documentos/consumo/consumo.qmd")
  trama::tr_project_config_set(x$raiz, "versionar", list(repo = "pesquisadora/projeto-privado"))
  r2 <- tr_documento_publicar(outro, x$proj, repositorio = "projeto-privado")
  expect_match(unlist(r2$bloqueios), "repositório privado das versões")
  expect_identical(length(gh$chamadas), n)
})
