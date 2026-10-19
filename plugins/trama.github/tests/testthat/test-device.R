test_that("device flow: código, espera, devagar e token", {
  local_github_falso(token = NULL)
  p <- tr_github_device_iniciar()
  expect_equal(p$codigo, "ABCD-1234")
  expect_match(p$link, "login/device")
  expect_equal(tr_github_device_conferir(p)$estado, "pendente")
  expect_equal(tr_github_device_conferir(p)$estado, "pendente")  # slow_down
  r <- tr_github_device_conferir(p)
  expect_equal(r$estado, "ok")
  expect_equal(r$token, TOKEN_FALSO)
})

test_that("código vencido não pergunta ao GitHub", {
  local_github_falso(token = NULL)
  p <- tr_github_device_iniciar()
  p$expira_em <- Sys.time() - 1
  expect_equal(tr_github_device_conferir(p)$estado, "expirado")
})

test_that("tr_github_entrar guarda o token no chaveiro e conecta", {
  local_github_falso(token = NULL)
  msgs <- capture_messages(u <- tr_github_entrar(abrir = FALSE))
  expect_match(msgs[1], "ABCD-1234")
  expect_equal(u$login, "pesquisadora")
  tk <- tr_github_token()
  expect_equal(attr(tk, "fonte"), "chaveiro")
  expect_true(tr_github_sair())
  expect_null(tr_github_token())
  expect_false(tr_github_sair())
})

test_that("Client ID errado ou ausente vira erro claro", {
  local_github_falso(token = NULL)
  withr::local_options(trama.github.client_id = "outro")
  expect_error(tr_github_device_iniciar(), "não é de um app válido", class = "tr_github_erro_login")
  withr::local_options(trama.github.client_id = NULL)
  withr::local_envvar(TRAMA_GITHUB_CLIENT_ID = "")
  local_mocked_bindings(.CLIENT_ID_TRAMA = "")
  expect_error(tr_github_client_id(), class = "tr_github_erro_client_id")
  expect_equal(tr_github_client_id(obrigatorio = FALSE), "")
  withr::local_envvar(TRAMA_GITHUB_CLIENT_ID = "do-ambiente")
  expect_equal(tr_github_client_id(), "do-ambiente")
})
