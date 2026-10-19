test_that("usuário conectado vem da API com o token achado", {
  local_github_falso()
  u <- tr_github_usuario()
  expect_equal(u$login, "pesquisadora")
  expect_equal(u$nome, "Pesquisadora Teste")
  expect_equal(u$plano, "free")
})

test_that("criar repositório: privado por padrão, nome repetido vira erro claro", {
  local_github_falso()
  r <- tr_github_criar_repo("projeto", descricao = "Dissertação")
  expect_equal(r$repo, "pesquisadora/projeto")
  expect_true(r$privado)
  expect_equal(r$branch, "main")
  expect_false(tr_github_criar_repo("publicacao", privado = FALSE)$privado)
  expect_error(tr_github_criar_repo("projeto"), class = "tr_github_erro_repo_existe")
  expect_null(tr_github_repo("pesquisadora/nao-existe"))
  expect_equal(tr_github_repo("pesquisadora/projeto")$repo, "pesquisadora/projeto")
})

test_that("commit grava arquivos, mantém os outros e só então move a branch", {
  local_github_falso()
  tr_github_criar_repo("projeto")
  f <- withr::local_tempfile(fileext = ".json")
  writeLines('{"nodes": []}', f)
  c1 <- tr_github_commit("pesquisadora/projeto", "Primeira versão",
                         arquivos = c("fluxos/main.json" = f),
                         conteudos = list("notas.md" = c("# Notas", "linha 2")))
  expect_true(c1$mudou)
  e <- falso_estado()
  expect_equal(e$refs[["pesquisadora/projeto@heads/main"]], c1$sha)
  # A árvore do commit novo tem o README do auto_init e os dois arquivos.
  com_arv <- Filter(function(a) setequal(names(a), c("README.md", "fluxos/main.json", "notas.md")), e$arvores)
  expect_length(com_arv, 1)
  conteudo <- rawToChar(jsonlite::base64_dec(e$blobs[[com_arv[[1]][["notas.md"]]]]))
  expect_equal(conteudo, "# Notas\nlinha 2")
  expect_equal(e$mensagens[[c1$sha]], "Primeira versão")
})

test_that("commit sem mudança não cria commit nem move a branch", {
  local_github_falso()
  tr_github_criar_repo("projeto")
  c1 <- tr_github_commit("pesquisadora/projeto", "v1", conteudos = list("a.txt" = "a"))
  c2 <- tr_github_commit("pesquisadora/projeto", "v2", conteudos = list("a.txt" = "a"))
  expect_false(c2$mudou)
  expect_equal(c2$sha, c1$sha)
})

test_that("apagar e manter_outros = FALSE definem a árvore", {
  local_github_falso()
  tr_github_criar_repo("projeto")
  tr_github_commit("pesquisadora/projeto", "v1", conteudos = list("a.txt" = "a", "b.txt" = "b"))
  tr_github_commit("pesquisadora/projeto", "tira b", apagar = "b.txt")
  e <- falso_estado()
  expect_true(any(vapply(e$arvores, function(a) setequal(names(a), c("README.md", "a.txt")), TRUE)))
  tr_github_commit("pesquisadora/projeto", "limpo", conteudos = list("so.txt" = "x"), manter_outros = FALSE)
  e <- falso_estado()
  expect_true(any(vapply(e$arvores, function(a) identical(names(a), "so.txt"), TRUE)))
})

test_that("queda no meio do envio não move a branch; repetir funciona", {
  local_github_falso()
  tr_github_criar_repo("projeto")
  antes <- falso_estado()$refs[["pesquisadora/projeto@heads/main"]]
  falso_post("/_falso/config", list(falhar_blob = 2L))
  expect_error(tr_github_commit("pesquisadora/projeto", "v1", conteudos = list("a.txt" = "a", "b.txt" = "b", "c.txt" = "c")),
               class = "tr_github_erro")
  expect_equal(falso_estado()$refs[["pesquisadora/projeto@heads/main"]], antes)
  c1 <- tr_github_commit("pesquisadora/projeto", "v1", conteudos = list("a.txt" = "a", "b.txt" = "b", "c.txt" = "c"))
  expect_equal(falso_estado()$refs[["pesquisadora/projeto@heads/main"]], c1$sha)
})

test_that("branch que mudou no meio do envio é recusada sem forçar", {
  local_github_falso()
  tr_github_criar_repo("projeto")
  falso_post("/_falso/config", list(corrida = TRUE))
  expect_error(tr_github_commit("pesquisadora/projeto", "v1", conteudos = list("a.txt" = "a")),
               class = "tr_github_erro_concorrencia")
})

test_that("arquivo acima de 100 MB é recusado antes de sair", {
  local_github_falso()
  f <- withr::local_tempfile()
  writeBin(raw(1), f)
  local_mocked_bindings(.LIMITE_ARQUIVO = 0)
  expect_error(tr_github_commit("pesquisadora/projeto", "v1", arquivos = c("grande.bin" = f)),
               class = "tr_github_erro_arquivo_grande")
})

test_that("tag leve e anotada; tag repetida é recusada", {
  local_github_falso()
  tr_github_criar_repo("pub")
  c1 <- tr_github_commit("pesquisadora/pub", "pacote", conteudos = list("a.txt" = "a"))
  t1 <- tr_github_tag("pesquisadora/pub", "v1.0.0")
  expect_equal(t1$sha, c1$sha)
  e <- falso_estado()
  expect_equal(e$refs[["pesquisadora/pub@tags/v1.0.0"]], c1$sha)
  expect_error(tr_github_tag("pesquisadora/pub", "v1.0.0"), class = "tr_github_erro_tag_existe")

  t2 <- tr_github_tag("pesquisadora/pub", "v1.0.1", sha = c1$sha, mensagem = "Versão 1.0.1")
  e <- falso_estado()
  obj <- e$refs[["pesquisadora/pub@tags/v1.0.1"]]
  expect_equal(e$tag_objs[[obj]]$object, c1$sha)
  expect_equal(e$tag_objs[[obj]]$message, "Versão 1.0.1")
})

test_that("sem credencial, a API pede para entrar", {
  local_github_falso(token = NULL)
  expect_error(tr_github_usuario(), class = "tr_github_erro_sem_credencial")
})

test_that("credencial recusada vira erro em português", {
  local_github_falso(token = "token-errado")
  expect_error(tr_github_usuario(), "expirou ou foi revogada", class = "tr_github_erro_credencial")
})
