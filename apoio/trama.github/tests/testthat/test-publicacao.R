# O que a publicação (#14) pede ao GitHub, contra o GitHub falso.

test_that("tags e release por tag", {
  local_github_falso()
  tr_github_criar_repo("pub", privado = FALSE)
  expect_identical(tr_github_tags("pesquisadora/pub"), character())
  tr_github_tag("pesquisadora/pub", "v0.1.0")
  tr_github_tag("pesquisadora/pub", "v0.2.0")
  expect_setequal(tr_github_tags("pesquisadora/pub"), c("v0.1.0", "v0.2.0"))
  expect_null(tr_github_release("pesquisadora/pub", "v0.1.0"))
  falso_post("/_falso/config", list(releases = list(`pesquisadora/pub@v0.1.0` = list(
    tag_name = "v0.1.0", html_url = "https://github.com/pesquisadora/pub/releases/tag/v0.1.0",
    assets = list(list(name = "pacote-v0.1.0.zip", browser_download_url = "https://x/pacote.zip"))))))
  r <- tr_github_release("pesquisadora/pub", "v0.1.0")
  expect_identical(r$tag, "v0.1.0")
  expect_identical(r$arquivos[["pacote-v0.1.0.zip"]], "https://x/pacote.zip")
})

test_that("Pages: liga em modo Actions, e já ligado troca o modo", {
  local_github_falso()
  tr_github_criar_repo("pub", privado = FALSE)
  expect_null(tr_github_pages("pesquisadora/pub"))
  p <- tr_github_ativar_pages("pesquisadora/pub")
  expect_identical(p$modo, "workflow")
  expect_identical(p$url, "https://pesquisadora.github.io/pub/")
  expect_identical(tr_github_ativar_pages("pesquisadora/pub")$modo, "workflow")
})

test_that("Pages recusado (repositório privado sem plano) vira erro que explica", {
  local_github_falso()
  tr_github_criar_repo("priv")
  falso_post("/_falso/config", list(pages_recusa = TRUE))
  expect_error(tr_github_ativar_pages("pesquisadora/priv"), class = "tr_github_erro_pages")
})

test_that("ambiente do Pages ganha a regra das tags, sem erro na segunda vez", {
  local_github_falso()
  tr_github_criar_repo("pub", privado = FALSE)
  tr_github_ambiente_tags("pesquisadora/pub")
  tr_github_ambiente_tags("pesquisadora/pub")
  e <- falso_estado()
  amb <- e$ambientes[["pesquisadora/pub@github-pages"]]
  expect_true(amb$deployment_branch_policy$custom_branch_policies)
  expect_false(amb$deployment_branch_policy$protected_branches)
  expect_identical(names(e$politicas), "pesquisadora/pub@github-pages@tag:v*")
})

test_that("execuções do commit e a etapa que falhou", {
  local_github_falso()
  tr_github_criar_repo("pub", privado = FALSE)
  falso_post("/_falso/config", list(
    runs = list(list(id = 7, name = "publicar", head_sha = "abc", status = "completed", conclusion = "failure",
                     html_url = "https://github.com/pesquisadora/pub/actions/runs/7"),
                list(id = 8, name = "outro", head_sha = "zzz", status = "queued")),
    jobs = list(`7` = list(list(name = "publicar", conclusion = "failure", html_url = "https://log/7",
                                steps = list(list(name = "Baixar o pacote", conclusion = "success"),
                                             list(name = "Renderizar a página", conclusion = "failure")))))))
  ex <- tr_github_execucoes("pesquisadora/pub", "abc")
  expect_length(ex, 1L)
  expect_identical(ex[[1]]$conclusao, "failure")
  f <- tr_github_falha("pesquisadora/pub", 7)
  expect_identical(f$etapa, "Renderizar a página")
  expect_identical(f$url, "https://log/7")
  expect_identical(tr_github_execucoes("pesquisadora/pub", "nada"), list())
})
