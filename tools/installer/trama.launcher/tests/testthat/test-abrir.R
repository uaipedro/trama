# abrir(): quando já há um launcher respondendo na porta, só reabre a
# janela em vez de subir outro shiny::runApp() disputando a mesma porta
# (revisão do instalador: duplo clique no atalho não pode subir dois
# launchers). shiny::runApp() de verdade não é exercitado aqui (bloquearia
# a sessão de teste) — só o desvio antes dele.

test_that("abrir() só reabre a janela quando o launcher já está de pé", {
  chamadas <- list(janela = character(0))

  resultado <- abrir(
    porta = 8725L,
    ja_rodando = function(porta) TRUE,
    abrir_janela = function(url) chamadas$janela[[length(chamadas$janela) + 1]] <<- url
  )

  expect_null(resultado)
  expect_match(chamadas$janela, "127.0.0.1:8725")
})

test_that(".tl_launcher_rodando é FALSE quando não há nada respondendo na porta", {
  porta <- .tl_porta_livre(18725L)
  expect_false(.tl_launcher_rodando(porta))
})
