# Seções Projeto e Publicações do painel, com um `ctx` de mentira: o projeto
# é uma pasta temporária e `recarregar` só conta as chamadas.
painel_de_teste <- function(raiz, env = parent.frame()) {
  msgs <- list()
  recargas <- 0L
  ctx <- list(project = function() list(root = raiz), recarregar = function() recargas <<- recargas + 1L)
  ctl <- .gh_controlador(enviar = function(m) msgs[[length(msgs) + 1L]] <<- m,
                         agendar = function(f, seg) f(), ctx = ctx)
  list(receber = ctl, msgs = function() msgs, recargas = function() recargas,
       ultima = function(tipo = NULL) {
         ms <- if (is.null(tipo)) msgs else Filter(function(m) identical(m$tipo, tipo), msgs)
         ms[[length(ms)]]
       })
}

projeto_simples <- function(env = parent.frame()) {
  raiz <- withr::local_tempdir("Projeto Painel", .local_envir = env)
  trama::tr_project_new(raiz, character())
  dir.create(file.path(raiz, "flows"), showWarnings = FALSE)
  writeLines('{"nodes": []}', file.path(raiz, "flows", "main.json"))
  writeLines("x <- 1", file.path(raiz, "analise.R"))
  writeLines("a,b\n1,2", file.path(raiz, "vendas.csv"))
  normalizePath(raiz)
}

test_that("projeto sem repositório: nome sugerido e arquivos, sem versões", {
  local_github_falso()
  p <- painel_de_teste(projeto_simples())
  p$receber(list(acao = "projeto"))
  m <- p$ultima()
  expect_equal(m$tipo, "projeto")
  expect_false(m$ligado)
  expect_match(m$nome_sugerido, "^projeto-painel")
  expect_equal(m$arquivos$projeto_n, 3L)
  expect_equal(m$arquivos$dados[[1]]$caminho, "vendas.csv")
  expect_false(m$arquivos$dados[[1]]$incluido)
})

test_that("criar repositório já salva a primeira versão; salvar e marcar", {
  local_github_falso()
  raiz <- projeto_simples()
  p <- painel_de_teste(raiz)
  p$receber(list(acao = "criar_repo", nome = "meu-projeto"))
  expect_true(p$ultima("salvo")$primeira)
  m <- p$ultima("projeto")
  expect_true(m$ligado)
  expect_equal(m$repo, "pesquisadora/meu-projeto")
  expect_equal(m$versoes[[1]]$mensagem, "Primeira versão")

  p$receber(list(acao = "marcar", caminho = "vendas.csv", incluir = TRUE))
  expect_true(p$ultima("arquivos")$arquivos$dados[[1]]$incluido)
  p$receber(list(acao = "salvar", mensagem = "Com os dados de vendas"))
  expect_true(p$ultima("salvo")$mudou)
  expect_equal(p$ultima("projeto")$versoes[[1]]$mensagem, "Com os dados de vendas")
  p$receber(list(acao = "salvar", mensagem = "Nada"))
  expect_false(p$ultima("salvo")$mudou)
})

test_that("erros da seção vêm com contexto e não derrubam o controlador", {
  local_github_falso()
  p <- painel_de_teste(projeto_simples())
  p$receber(list(acao = "salvar", mensagem = "sem repositório"))
  e <- p$ultima()
  expect_equal(e$tipo, "erro")
  expect_equal(e$contexto, "projeto")
  expect_match(e$mensagem, "repositório privado")
})

test_that("restaurar escreve, recarrega o editor e devolve a pasta da cópia", {
  local_github_falso()
  raiz <- projeto_simples()
  p <- painel_de_teste(raiz)
  p$receber(list(acao = "criar_repo", nome = "meu-projeto"))
  v1 <- p$ultima("projeto")$versoes[[1]]$sha
  writeLines("x <- 2", file.path(raiz, "analise.R"))
  p$receber(list(acao = "salvar", mensagem = "x vale 2"))
  p$receber(list(acao = "restaurar", sha = v1))
  r <- p$ultima("restaurado")
  expect_equal(p$recargas(), 1L)
  expect_gte(r$escritos, 1L)
  expect_match(r$copia, "^\\.trama/restauracao/")
  expect_equal(readLines(file.path(raiz, "analise.R")), "x <- 1")
})

test_that("publicações: sem trama.documentos, a seção diz que não está disponível", {
  local_github_falso()
  local_mocked_bindings(.gh_publicacoes_leitor = function() NULL, .package = "trama.github")
  p <- painel_de_teste(projeto_simples())
  p$receber(list(acao = "publicacoes"))
  expect_false(p$ultima()$disponivel)
})

test_that("publicações: registro do projeto + versões e Actions lidos na hora", {
  local_github_falso()
  tr_github_criar_repo("pub", privado = FALSE)
  c1 <- tr_github_commit("pesquisadora/pub", "pacote", conteudos = list("a.txt" = "a"))
  tr_github_tag("pesquisadora/pub", "v1.0.0")
  falso_post("/_falso/config", list(runs = list(list(id = 7, name = "publicar", status = "completed",
                                                     conclusion = "success", head_sha = c1$sha,
                                                     html_url = "https://github.com/pesquisadora/pub/actions/runs/7"))))
  registro <- data.frame(documento = c("documentos/a/a.qmd", "documentos/b/b.qmd"), repo = c("pesquisadora/pub", "pesquisadora/sumiu"),
                         url = c("https://github.com/pesquisadora/pub", "https://github.com/pesquisadora/sumiu"),
                         pagina = "https://pesquisadora.github.io/pub/", ultimo = NA, versao = "1.0.0",
                         tag = "v1.0.0", data = "2026-10-19", stringsAsFactors = FALSE)
  local_mocked_bindings(.gh_publicacoes_leitor = function() function(root) registro, .package = "trama.github")
  p <- painel_de_teste(projeto_simples())
  p$receber(list(acao = "publicacoes"))
  m <- p$ultima()
  expect_true(m$disponivel)
  expect_length(m$itens, 2)
  a <- m$itens[[1]]
  expect_equal(a$documento, "documentos/a/a.qmd")
  expect_equal(a$nome, "a")
  expect_equal(unlist(a$versoes), "v1.0.0")
  expect_equal(a$execucao$conclusao, "success")
  expect_equal(a$release, "https://github.com/pesquisadora/pub/releases/tag/v1.0.0")
  expect_null(a$ultimo)
  # Repositório que não existe mais: erro só no item.
  expect_match(m$itens[[2]]$erro, "Não encontrado")
})
