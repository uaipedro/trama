# Projeto temporário com um pouco de tudo: fluxo, código, dados, segredo e
# o store do trama.
projeto_de_teste <- function(env = parent.frame()) {
  raiz <- withr::local_tempdir("proj-versionar", .local_envir = env)
  trama::tr_project_new(raiz, character())
  dir.create(file.path(raiz, "flows"), showWarnings = FALSE)
  writeLines('{"nodes": []}', file.path(raiz, "flows", "main.json"))
  writeLines("x <- 1", file.path(raiz, "analise.R"))
  writeLines("# Notas", file.path(raiz, "README.md"))
  writeLines("a,b\n1,2", file.path(raiz, "vendas.csv"))
  writeLines("nome,cpf\nAna,1", file.path(raiz, "clientes.csv"))
  writeLines("SEGREDO=1", file.path(raiz, ".Renviron"))
  dir.create(file.path(raiz, ".trama", "store"), recursive = TRUE, showWarnings = FALSE)
  writeLines("cache", file.path(raiz, ".trama", "store", "x.rds"))
  writeLines("{}", file.path(raiz, ".trama", "control-agente.json"))
  normalizePath(raiz)
}

test_that("o que vai: projeto sempre, dados só marcados, .trama e segredos nunca", {
  raiz <- projeto_de_teste()
  a <- tr_github_versionar_arquivos(raiz)
  expect_setequal(a$caminho[a$incluido], c("trama.json", "flows/main.json", "analise.R", "README.md"))
  expect_setequal(a$caminho[a$tipo == "dados"], c("vendas.csv", "clientes.csv"))
  expect_false(any(grepl("^\\.trama/|\\.Renviron", a$caminho)))
  expect_match(a$aviso[a$caminho == "clientes.csv"], "pessoais")
  expect_true(is.na(a$aviso[a$caminho == "vendas.csv"]))

  tr_github_versionar_marcar(raiz, "vendas.csv")
  a <- tr_github_versionar_arquivos(raiz)
  expect_true(a$incluido[a$caminho == "vendas.csv"])
  tr_github_versionar_marcar(raiz, "vendas.csv", incluir = FALSE)
  expect_false(tr_github_versionar_arquivos(raiz)$incluido[a$caminho == "vendas.csv"])
  expect_error(tr_github_versionar_marcar(raiz, "analise.R"), "dados")
})

test_that("arquivo acima de 100 MB fica bloqueado e não pode ser marcado", {
  raiz <- projeto_de_teste()
  local_mocked_bindings(.LIMITE_ARQUIVO = 5, .package = "trama.github")
  a <- tr_github_versionar_arquivos(raiz)
  expect_true(a$bloqueado[a$caminho == "vendas.csv"])
  expect_error(tr_github_versionar_marcar(raiz, "vendas.csv"), class = "tr_github_erro_arquivo_grande")
})

test_that("nome sugerido vem da pasta, sem acento", {
  raiz <- file.path(withr::local_tempdir(), "Dissertação Final")
  dir.create(raiz)
  expect_equal(tr_github_versionar_nome(raiz), "dissertacao-final")
})

test_that("criar repositório privado, salvar versões e ler o histórico", {
  local_github_falso()
  raiz <- projeto_de_teste()
  expect_error(tr_github_salvar_versao(raiz, "v1"), class = "tr_github_erro_sem_repositorio")
  cfg <- tr_github_versionar_criar(raiz, "meu-projeto")
  expect_equal(cfg$repo, "pesquisadora/meu-projeto")
  expect_equal(tr_github_versionar_config(raiz)$repo, "pesquisadora/meu-projeto")
  expect_error(tr_github_versionar_criar(raiz), class = "tr_github_erro_ja_versionado")
  expect_true(falso_repo("pesquisadora/meu-projeto")$private)

  expect_error(tr_github_salvar_versao(raiz, "  "), "frase")
  v1 <- tr_github_salvar_versao(raiz, "Primeira versão")
  expect_true(v1$mudou)
  expect_equal(v1$arquivos, 4L)
  arv <- falso_arvore_de("pesquisadora/meu-projeto")
  expect_setequal(names(arv), c("trama.json", "flows/main.json", "analise.R", "README.md"))

  # Sem mudança: nada novo. Com dado marcado e arquivo apagado: retrato novo.
  expect_false(tr_github_salvar_versao(raiz, "de novo")$mudou)
  tr_github_versionar_marcar(raiz, "vendas.csv")
  unlink(file.path(raiz, "README.md"))
  antes <- falso_estado()$blobs_criados
  v2 <- tr_github_salvar_versao(raiz, "Dados de vendas\n\ndetalhe")
  expect_true(v2$mudou)
  expect_setequal(names(falso_arvore_de("pesquisadora/meu-projeto")),
                  c("trama.json", "flows/main.json", "analise.R", "vendas.csv"))
  # Só o trama.json (mudou com a marcação) e o vendas.csv são enviados.
  expect_equal(falso_estado()$blobs_criados - antes, 2L)

  h <- tr_github_versoes(raiz)
  expect_equal(h$mensagem[1:2], c("Dados de vendas", "Primeira versão"))
  expect_equal(h$sha[1], v2$sha)
  expect_s3_class(h$data, "POSIXct")
})

test_that("restaurar: volta os arquivos, guarda o atual, não apaga dado nem toca no .trama", {
  local_github_falso()
  raiz <- projeto_de_teste()
  tr_github_versionar_criar(raiz, "meu-projeto")
  v1 <- tr_github_salvar_versao(raiz, "v1")
  writeLines("x <- 2", file.path(raiz, "analise.R"))
  writeLines("y <- 1", file.path(raiz, "novo.R"))
  tr_github_versionar_marcar(raiz, "vendas.csv")
  tr_github_salvar_versao(raiz, "v2")
  trama::tr_project_config_set(raiz, "publicacoes", list(no1 = list(repo = "pesquisadora/pub")))

  r <- tr_github_restaurar(raiz, v1$sha)
  expect_equal(readLines(file.path(raiz, "analise.R")), "x <- 1")
  expect_false(file.exists(file.path(raiz, "novo.R")))
  expect_true(file.exists(file.path(raiz, "vendas.csv")))        # dado nunca é apagado
  expect_true(file.exists(file.path(raiz, ".trama", "store", "x.rds")))
  expect_setequal(r$removidos, "novo.R")
  expect_true("analise.R" %in% r$escritos)
  # Cópia do estado anterior.
  expect_equal(readLines(file.path(r$copia, "analise.R")), "x <- 2")
  expect_equal(readLines(file.path(r$copia, "novo.R")), "y <- 1")
  # Ligação e publicações são as de agora, não as da versão.
  cfg <- tr_github_versionar_config(raiz)
  expect_equal(unlist(cfg$dados), "vendas.csv")
  expect_equal(trama::tr_project_config(raiz, "publicacoes")$no1$repo, "pesquisadora/pub")
})

test_that("queda no meio do download não muda nenhum arquivo", {
  local_github_falso()
  raiz <- projeto_de_teste()
  tr_github_versionar_criar(raiz, "meu-projeto")
  v1 <- tr_github_salvar_versao(raiz, "v1")
  writeLines("x <- 2", file.path(raiz, "analise.R"))
  writeLines('{"nodes": [1]}', file.path(raiz, "flows", "main.json"))
  tr_github_salvar_versao(raiz, "v2")
  falso_post("/_falso/config", list(falhar_leitura = 2L))
  expect_error(tr_github_restaurar(raiz, v1$sha), class = "tr_github_erro")
  expect_equal(readLines(file.path(raiz, "analise.R")), "x <- 2")
  expect_equal(readLines(file.path(raiz, "flows", "main.json")), '{"nodes": [1]}')
  expect_false(dir.exists(file.path(raiz, ".trama", "restauracao")))
})
