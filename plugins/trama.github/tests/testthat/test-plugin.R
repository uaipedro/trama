# O controlador do menu, sem Shiny: `enviar` guarda as mensagens e `agendar`
# chama na hora (o GitHub falso responde pendente, devagar e ok).
controlador_de_teste <- function() {
  msgs <- list()
  ctl <- .gh_controlador(enviar = function(m) msgs[[length(msgs) + 1L]] <<- m,
                         agendar = function(f, seg) f())
  list(receber = ctl, msgs = function() msgs, ultima = function() msgs[[length(msgs)]])
}

test_that("contrato de plugin: JS e CSS existem no pacote, server é função", {
  e <- trama_plugin()
  expect_true(file.exists(system.file(e$js, package = "trama.github")))
  expect_true(file.exists(system.file(e$css, package = "trama.github")))
  expect_true(is.function(e$server))
  expect_true(isTRUE(as.logical(utils::packageDescription("trama.github", fields = "Config/trama/plugin"))))
})

test_that("estado: desconectado sem credencial, conectado com ela", {
  local_github_falso(token = NULL)
  c <- controlador_de_teste()
  c$receber(list(acao = "estado"))
  m <- c$ultima()
  expect_equal(m$tipo, "estado")
  expect_false(m$conectado)
  expect_true(m$login_disponivel)

  withr::local_envvar(GITHUB_PAT = TOKEN_FALSO)
  c$receber(list(acao = "estado"))
  m <- c$ultima()
  expect_true(m$conectado)
  expect_equal(m$fonte, "GITHUB_PAT")
  expect_equal(m$usuario$login, "pesquisadora")
})

test_that("credencial recusada aparece como não conectado, com o motivo", {
  local_github_falso(token = "vencido")
  c <- controlador_de_teste()
  c$receber(list(acao = "estado"))
  expect_false(c$ultima()$conectado)
  expect_match(c$ultima()$erro, "expirou")
})

test_that("entrar: código, conferências e token guardado no chaveiro", {
  local_github_falso(token = NULL)
  c <- controlador_de_teste()
  c$receber(list(acao = "entrar"))
  tipos <- vapply(c$msgs(), `[[`, "", "tipo")
  expect_equal(tipos, c("codigo", "estado"))
  expect_equal(c$msgs()[[1]]$codigo, "ABCD-1234")
  expect_true(c$ultima()$conectado)
  expect_equal(c$ultima()$fonte, "chaveiro")

  c$receber(list(acao = "sair"))
  expect_false(c$ultima()$conectado)
})

test_that("cancelar invalida a conferência em andamento", {
  local_github_falso(token = NULL)
  pendentes <- list()
  msgs <- list()
  ctl <- .gh_controlador(enviar = function(m) msgs[[length(msgs) + 1L]] <<- m,
                         agendar = function(f, seg) pendentes[[length(pendentes) + 1L]] <<- f)
  ctl(list(acao = "entrar"))
  ctl(list(acao = "cancelar"))
  n <- length(msgs)
  pendentes[[1]]()  # a conferência agendada antes do cancelar não faz nada
  expect_length(msgs, n)
  expect_null(tr_github_token())
})

test_that("sem Client ID, entrar responde com erro que orienta", {
  local_github_falso(token = NULL)
  withr::local_options(trama.github.client_id = NULL)
  withr::local_envvar(TRAMA_GITHUB_CLIENT_ID = "")
  local_mocked_bindings(.CLIENT_ID_TRAMA = "")
  c <- controlador_de_teste()
  c$receber(list(acao = "estado"))
  expect_false(c$ultima()$login_disponivel)
  c$receber(list(acao = "entrar"))
  expect_equal(c$ultima()$tipo, "erro")
  expect_match(c$ultima()$mensagem, "GITHUB_PAT")
})

test_that("guia: passos chegam ao editor", {
  c <- controlador_de_teste()
  c$receber(list(acao = "guia"))
  expect_equal(c$ultima()$tipo, "guia")
  expect_gt(length(c$ultima()$passos), 3)
})
