# Publicação (#25): documento pronto para publicar, GitHub em memória e o
# fluxo antigo com o nó publicar/publish, para test-publicar, test-migrar e
# test-painel.

# YAML do documento como a pessoa o edita.
mudar_yaml <- function(qmd, f) {
  d <- trama.documentos:::.td_ler(qmd)
  fim <- trama.documentos:::.td_fim_yaml(d$linhas)
  y <- f(trama.documentos:::.td_yaml_listas(d$linhas))
  writeLines(c("---", trama.documentos:::.td_yaml_texto(y), "---", d$linhas[(fim + 1L):length(d$linhas)]), qmd, useBytes = TRUE)
  invisible(qmd)
}

autores_exemplo <- list(list(name = "Josiah Carberry", orcid = "0000-0002-1825-0097"), list(name = "Ana Lima"))

# Projeto com uma tabela e um documento Página web pronto para publicar.
doc_publicavel <- function(env = parent.frame()) {
  proj <- local_projeto(env)
  salvar_fluxo(proj, no("tab", n = 4L), no("mod", "tst/modelo"))
  qmd <- tr_documento_criar("consumo", proj, modelo = "pagina", perfil = FALSE, titulo = "Consumo e peso")
  mudar_yaml(qmd, function(y) {
    y$author <- autores_exemplo
    y$description <- "Material de teste."
    y$keywords <- list("carros", "consumo")
    y
  })
  tr_documento_inserir(qmd, "tab", proj)
  list(proj = proj, qmd = qmd, raiz = proj$root)
}

github_memoria <- function(env = parent.frame(), token = "tk", repos = list(), tags = list()) {
  gh <- new.env()
  gh$repos <- repos; gh$tags <- tags; gh$commits <- list(); gh$pages <- list()
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
      # O pacote é apagado no próximo envio: guarda o conteúdo agora.
      lidos <- lapply(arquivos, function(f) readBin(f, "raw", file.info(f)$size))
      gh$commits[[length(gh$commits) + 1L]] <- list(repo = repo, mensagem = mensagem, arquivos = lidos,
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

txt <- function(raw) rawToChar(raw)

# Um fluxo antigo, como o trama.publicar gravava: o nó publicar com os
# params e os itens ligados por arestas (a coleção não está instalada; o
# fluxo é escrito à mão).
fluxo_com_publicar <- function(proj, layout = "", extra = list(), itens = c("tab", "txt")) {
  salvar_fluxo(proj, no("tab", n = 3L), no("txt", "tst/texto", valor = "b"), no("ruim", "tst/falha"))
  caminho <- file.path(proj$root, "flows", "main.json")
  doc <- trama::tr_doc_read(caminho)
  doc$nodes$pub <- list(type = "publicar/publish", type_version = 1L, label = "Publicar",
    params = utils::modifyList(list(
      titulo = "Consumo e peso", descricao = "Material de teste.", palavras_chave = "carros, consumo",
      autores = "Josiah Carberry (0000-0002-1825-0097); Ana Lima", licenca = "CC0-1.0", licenca_codigo = "MIT",
      repositorio = "", versao = "correção", layout = layout, aceitar_dados_treino = FALSE, privado = FALSE),
      extra))
  doc$edges <- c(doc$edges, lapply(itens, function(i) list(from = list(node = i, port = "out"),
                                                           to = list(node = "pub", port = "itens"))))
  doc$ui$positions <- list(tab = c(0, 0), txt = c(0, 100), ruim = c(0, 200), pub = c(300, 0))
  doc$collections$publicar <- "0.2.0"
  trama::tr_doc_write(doc, caminho)
  cfg <- jsonlite::fromJSON(file.path(proj$root, "trama.json"), simplifyVector = FALSE)
  cfg$collections <- I(c(unlist(cfg$collections), "trama.publicar"))
  writeLines(jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = TRUE), file.path(proj$root, "trama.json"))
  caminho
}

registrar_antigo <- function(proj) {
  trama::tr_project_config_set(proj$root, "publicacoes", list(pub = list(
    repo = "pesquisadora/consumo-e-peso", url = "https://github.com/pesquisadora/consumo-e-peso",
    pagina = "https://pesquisadora.github.io/consumo-e-peso/", ultimo = "https://pesquisadora.github.io/consumo-e-peso/ultimo/",
    versao = "0.1.0", tag = "v0.1.0", data = "2026-10-19")))
}

