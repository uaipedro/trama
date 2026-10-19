test_that("ordem das fontes: chaveiro, GITHUB_PAT, GITHUB_TOKEN, gh, gitcreds", {
  local_github_falso(token = NULL)
  expect_null(tr_github_token())

  local_mocked_bindings(.gh_token_gitcreds = function() "do-gitcreds", .package = "trama.github")
  expect_equal(attr(tr_github_token(), "fonte"), "gitcreds")
  local_mocked_bindings(.gh_token_gh_cli = function() "do-gh", .package = "trama.github")
  expect_equal(attr(tr_github_token(), "fonte"), "gh")
  withr::local_envvar(GITHUB_TOKEN = "do-token")
  expect_equal(attr(tr_github_token(), "fonte"), "GITHUB_TOKEN")
  withr::local_envvar(GITHUB_PAT = "do-pat")
  expect_equal(attr(tr_github_token(), "fonte"), "GITHUB_PAT")
  tr_github_salvar_token("do-chaveiro")
  tk <- tr_github_token()
  expect_equal(as.vector(tk), "do-chaveiro")
  expect_equal(attr(tk, "fonte"), "chaveiro")
})

test_that("fonte que falha é pulada", {
  local_github_falso(token = NULL)
  local_mocked_bindings(.gh_token_gh_cli = function() stop("gh quebrado"),
                        .gh_token_gitcreds = function() "do-gitcreds", .package = "trama.github")
  expect_equal(attr(tr_github_token(), "fonte"), "gitcreds")
})

test_that("backend sem persistência é reconhecido; `file` nunca é usado", {
  withr::local_options(keyring_backend = "env")
  expect_false(tr_github_chaveiro()$persistente)
  withr::local_options(keyring_backend = "file")
  expect_equal(tr_github_chaveiro()$backend, "env")
})
