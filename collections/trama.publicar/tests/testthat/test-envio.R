# O envio ao GitHub (#14): ações "publicar" e "acompanhar", com um GitHub em
# memória no lugar das funções do trama.github (a API delas tem os próprios
# testes contra o GitHub falso, em apoio/trama.github). O conferir.py e o
# paginas.sh do workflow rodam de verdade, contra um repositório git local.

github_memoria <- function(env = parent.frame(), token = "tk", repos = list(), tags = list()) {
  gh <- new.env()
  gh$repos <- repos; gh$tags <- tags; gh$commits <- list(); gh$pages <- list(); gh$ambientes <- character()
  gh$runs <- list(); gh$releases <- list(); gh$chamadas <- character()
  anota <- function(x) gh$chamadas <- c(gh$chamadas, x)
  testthat::local_mocked_bindings(
    tr_github_token = function() token,
    tr_github_usuario = function(token = NULL) list(login = "pesquisadora"),
    tr_github_repo = function(repo, token = NULL) gh$repos[[repo]],
    tr_github_criar_repo = function(nome, privado = TRUE, descricao = NULL, dono = NULL, token = NULL) {
      anota("criar_repo"); r <- paste0(dono %||% "pesquisadora", "/", nome)
      gh$repos[[r]] <- list(repo = r, url = paste0("https://github.com/", r), privado = privado, branch = "main")
      gh$repos[[r]]
    },
    tr_github_tags = function(repo, token = NULL) gh$tags[[repo]] %||% character(),
    tr_github_pages = function(repo, token = NULL) gh$pages[[repo]],
    tr_github_ativar_pages = function(repo, token = NULL) {
      anota("pages"); gh$pages[[repo]] <- list(url = sprintf("https://pesquisadora.github.io/%s/", sub(".*/", "", repo)),
                                              modo = "workflow"); gh$pages[[repo]]
    },
    tr_github_ambiente_tags = function(repo, ambiente = "github-pages", padrao = "v*", token = NULL) {
      anota("ambiente"); invisible(TRUE)
    },
    tr_github_commit = function(repo, mensagem, arquivos = character(), conteudos = list(), apagar = character(),
                                branch = NULL, manter_outros = TRUE, token = NULL) {
      anota("commit")
      gh$commits[[length(gh$commits) + 1L]] <- list(repo = repo, mensagem = mensagem, arquivos = arquivos,
                                                    conteudos = conteudos, manter_outros = manter_outros)
      list(sha = sprintf("%040d", length(gh$commits)), url = "https://github.com/c", mudou = TRUE)
    },
    tr_github_tag = function(repo, tag, sha = NULL, mensagem = NULL, token = NULL) {
      anota("tag")
      if (tag %in% gh$tags[[repo]]) rlang::abort("existe", class = c("tr_github_erro_tag_existe", "tr_github_erro"))
      gh$tags[[repo]] <- c(tag, gh$tags[[repo]])
      list(tag = tag, sha = sha, url = paste0("https://github.com/", repo, "/releases/tag/", tag))
    },
    tr_github_execucoes = function(repo, sha, token = NULL) gh$runs,
    tr_github_falha = function(repo, id, token = NULL) list(job = "publicar", etapa = "Renderizar a página", url = "https://log/1"),
    tr_github_release = function(repo, tag, token = NULL) gh$releases[[tag]],
    .package = "trama.github", .env = env)
  gh
}

ctx_envio <- function(raiz, extra = NULL) {
  ex <- doc_exemplo()
  list(doc = ex$doc, raiz = raiz, store = trama::tr_store(file.path(raiz, ".trama", "store")),
       registry = ex$reg, settings = NULL, chave = "k", extra = extra)
}

test_that("bloqueios saem antes de tocar no GitHub", {
  gh <- github_memoria()
  raiz <- withr::local_tempdir()
  m <- tr_publicar_material(list(x = tr_publicar_item("modelo", "M", erro = "sem strip")), autores = "Ana Lima")
  r <- .tr_publicar_publicar(m, ctx_envio(raiz))
  expect_match(unlist(r$bloqueios), "título", all = FALSE)
  expect_length(gh$chamadas, 0L)
  gh2 <- github_memoria(token = NULL)
  r2 <- .tr_publicar_publicar(material_exemplo(), ctx_envio(raiz))
  expect_match(unlist(r2$bloqueios), "menu GitHub")
})

test_that("primeiro envio: repositório público, Pages, ambiente, um commit com o pacote e a tag v0.1.0", {
  gh <- github_memoria()
  raiz <- withr::local_tempdir()
  e <- .tr_publicar_publicar(material_exemplo(), ctx_envio(raiz))
  expect_identical(gh$chamadas, c("criar_repo", "pages", "ambiente", "commit", "tag"))
  expect_identical(e$repo, "pesquisadora/consumo-e-peso")
  expect_false(gh$repos[["pesquisadora/consumo-e-peso"]]$privado)
  expect_identical(e$tag, "v0.1.0")
  expect_identical(e$pagina, "https://pesquisadora.github.io/consumo-e-peso/v0.1.0/")
  expect_identical(e$etapa, "enviado")
  cm <- gh$commits[[1]]
  expect_false(cm$manter_outros)  # a árvore passa a ser exatamente o pacote
  expect_true(all(c("ro-crate-metadata.json", "index.qmd", "CITATION.cff", "flow.json", "itens/mod/motor.rds") %in% names(cm$arquivos)))
  expect_true(all(c(".github/workflows/publicar.yml", ".github/trama/conferir.py", ".github/trama/paginas.sh",
                    ".github/trama/notas.md", ".gitattributes") %in% names(cm$conteudos)))
  cff <- readLines(cm$arquivos[["CITATION.cff"]], encoding = "UTF-8")
  expect_true("version: \"0.1.0\"" %in% cff)
  expect_true("repository: \"https://github.com/pesquisadora/consumo-e-peso\"" %in% cff)
  # O estado fica no projeto, para o card retomar.
  expect_identical(.tr_publicar_estado_ler(raiz, e$repo)$sha, e$sha)
})

test_that("segundo envio: próxima versão a partir da última tag, sem recriar nada", {
  gh <- github_memoria(repos = list("pesquisadora/consumo-e-peso" = list(repo = "pesquisadora/consumo-e-peso",
                                                                        url = "https://github.com/pesquisadora/consumo-e-peso")),
                       tags = list("pesquisadora/consumo-e-peso" = c("v0.1.0", "v0.1.1", "rascunho")))
  gh$pages[["pesquisadora/consumo-e-peso"]] <- list(url = "https://pesquisadora.github.io/consumo-e-peso/", modo = "workflow")
  e <- .tr_publicar_publicar(material_exemplo(), ctx_envio(withr::local_tempdir()))
  expect_identical(e$tag, "v0.2.0")
  expect_false("criar_repo" %in% gh$chamadas)
  expect_false("pages" %in% gh$chamadas)
})

test_that("repositório privado novo e repositório alheio pedem confirmação", {
  gh <- github_memoria()
  raiz <- withr::local_tempdir()
  m <- material_exemplo(); m$metadados$privado <- TRUE
  r <- .tr_publicar_publicar(m, ctx_envio(raiz))
  expect_match(r$confirmar, "plano pago")
  expect_length(gh$chamadas, 0L)
  e <- .tr_publicar_publicar(m, ctx_envio(raiz, extra = list(confirmado = TRUE)))
  expect_true(gh$repos[[e$repo]]$privado)

  gh2 <- github_memoria(repos = list("pesquisadora/meu" = list(repo = "pesquisadora/meu", url = "u")))
  m2 <- material_exemplo(); m2$metadados$repositorio <- "meu"
  r2 <- .tr_publicar_publicar(m2, ctx_envio(raiz))
  expect_match(r2$confirmar, "substitui todo o conteúdo")
  expect_false("commit" %in% gh2$chamadas)
})

test_that("tag que já existe é recusada com a próxima versão sugerida", {
  gh <- github_memoria()
  local_mocked_bindings(.tr_publicar_ultima_versao = function(tags) "0.0.0", .package = "trama.publicar")
  raiz <- withr::local_tempdir()
  .tr_publicar_publicar(material_exemplo(), ctx_envio(raiz))
  expect_error(.tr_publicar_publicar(material_exemplo(), ctx_envio(raiz)), "v0.2.0", class = "tr_publicar_error_tag")
})

test_that("acompanhar: Actions rodando, falha com etapa e log, e página no ar", {
  gh <- github_memoria()
  raiz <- withr::local_tempdir()
  e <- .tr_publicar_publicar(material_exemplo(), ctx_envio(raiz))
  a <- function() .tr_publicar_acompanhar(NULL, ctx_envio(raiz, extra = list(repo = e$repo)))
  expect_identical(a()$etapa, "enviado")
  gh$runs <- list(list(id = 1, nome = "publicar", estado = "in_progress", conclusao = NA, url = "https://run/1"))
  expect_identical(a()$etapa, "actions")
  gh$runs[[1]]$estado <- "completed"; gh$runs[[1]]$conclusao <- "success"
  gh$releases[["v0.1.0"]] <- list(tag = "v0.1.0", url = "https://rel")
  local_mocked_bindings(.tr_publicar_pagina_no_ar = function(url) FALSE, .package = "trama.publicar")
  r <- a()
  expect_identical(r$etapa, "release")
  expect_identical(r$release, "https://rel")
  local_mocked_bindings(.tr_publicar_pagina_no_ar = function(url) TRUE, .package = "trama.publicar")
  expect_identical(a()$etapa, "no_ar")
  expect_identical(a()$etapa, "no_ar")  # terminado não consulta mais o GitHub

  raiz2 <- withr::local_tempdir()
  e2 <- .tr_publicar_publicar(material_exemplo(), ctx_envio(raiz2))
  gh$runs <- list(list(id = 2, nome = "publicar", estado = "completed", conclusao = "failure", url = "https://run/2"))
  f <- .tr_publicar_acompanhar(NULL, ctx_envio(raiz2, extra = list(repo = e2$repo)))
  expect_identical(f$etapa, "falhou")
  expect_match(f$detalhe, "Renderizar a página")
  expect_identical(f$log, "https://log/1")
})

test_that("o workflow fixa as versões das ações, do Quarto e do R", {
  wf <- readLines(system.file("github", "publicar.yml", package = "trama.publicar"))
  usos <- trimws(sub(".*uses:", "", grep("uses:", wf, value = TRUE)))
  expect_true(all(grepl("@v[0-9]+\\.[0-9]+\\.[0-9]+$", usos)), label = paste(usos, collapse = ", "))
  expect_true(any(grepl("version: 1.7.32", wf, fixed = TRUE)))
  expect_true(any(grepl("r-version: \"4.6.0\"", wf, fixed = TRUE)))
  expect_true(any(grepl("tags: [\"v*\"]", wf, fixed = TRUE)))
})

test_that("conferir.py aceita o pacote montado e recusa um arquivo adulterado", {
  py <- Sys.which("python3")
  skip_if(!nzchar(py), "python3 ausente")
  dir <- withr::local_tempdir()
  tr_publicar_montar(material_exemplo(), dir, doc = doc_exemplo()$doc, data = as.Date("2026-10-19"))
  script <- system.file("github", "conferir.py", package = "trama.publicar")
  ok <- processx::run(py, script, wd = dir, error_on_status = FALSE)
  expect_identical(ok$status, 0L, label = ok$stderr)
  cat("x", file = file.path(dir, "README.md"), append = TRUE)
  writeLines("solto", file.path(dir, "solto.txt"))
  ruim <- processx::run(py, script, wd = dir, error_on_status = FALSE)
  expect_identical(ruim$status, 1L)
  expect_match(ruim$stderr, "sha256 não confere: README.md", fixed = TRUE)
  expect_match(ruim$stderr, "fora do ro-crate-metadata.json: solto.txt", fixed = TRUE)
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
  script <- system.file("github", "paginas.sh", package = "trama.publicar")
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
  # A branch no remoto tem as duas versões.
  arvore <- run("ls-tree", "-r", "--name-only", "paginas", wd = origem)$stdout
  expect_match(arvore, "v0.1.0/index.html", fixed = TRUE)
  expect_match(arvore, "v0.2.0/index.html", fixed = TRUE)
  r3 <- publicar("v0.1.0")
  expect_identical(r3$status, 1L)
  expect_match(r3$stderr, "já está publicada")
})
