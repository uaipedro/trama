# Servidor MCP (R/mcp.R): protocolo JSON-RPC por linha e tools sobre o CLI.
# As tools sem editor rodam sobre a coleção falsa; as de edição, sobre um
# editor de verdade, ficam no teste à mão (sobem Shiny e navegador).

mcp <- function(msg, ctx = .tr_mcp_contexto(tempdir(), registro = test_registry())) {
  .tr_mcp_responder(.tr_mcp_json(msg), ctx)
}

chamar <- function(nome, args = structure(list(), names = character()), ...) {
  mcp(list(jsonrpc = "2.0", id = 7L, method = "tools/call",
           params = list(name = nome, arguments = args)), ...)$result
}

texto <- function(r) jsonlite::fromJSON(r$content[[1]]$text, simplifyVector = FALSE)

test_that("initialize negocia versão e anuncia tools e instruções", {
  r <- mcp(list(jsonrpc = "2.0", id = 1L, method = "initialize",
                params = list(protocolVersion = "2025-03-26", capabilities = list(),
                              clientInfo = list(name = "teste", version = "0"))))
  expect_equal(r$id, 1L)
  expect_equal(r$result$protocolVersion, "2025-03-26")
  expect_false(is.null(r$result$capabilities$tools))
  expect_equal(r$result$serverInfo$name, "trama")
  expect_match(r$result$instructions, "catalogo")
  desconhecida <- mcp(list(jsonrpc = "2.0", id = 2L, method = "initialize",
                           params = list(protocolVersion = "1999-01-01")))
  expect_equal(desconhecida$result$protocolVersion, .tr_mcp_versoes[[1]])
})

test_that("notificação não tem resposta; método desconhecido e JSON quebrado viram erro", {
  expect_null(mcp(list(jsonrpc = "2.0", method = "notifications/initialized")))
  r <- mcp(list(jsonrpc = "2.0", id = 3L, method = "resources/list"))
  expect_equal(r$error$code, -32601L)
  ctx <- .tr_mcp_contexto(tempdir())
  expect_equal(.tr_mcp_responder("{nada", ctx)$error$code, -32700L)
  # JSON válido que não é objeto não derruba o servidor.
  for (x in c("5", '"x"', "true", "[1,2]")) expect_equal(.tr_mcp_responder(x, ctx)$error$code, -32700L)
  expect_equal(.tr_mcp_responder('{"jsonrpc":"2.0","id":9}', ctx)$error$code, -32600L)
  # Resposta do cliente a uma requisição nossa não recebe resposta.
  expect_null(.tr_mcp_responder('{"jsonrpc":"2.0","id":9,"result":{}}', ctx))
  expect_equal(mcp(list(jsonrpc = "2.0", id = 4L, method = "ping"))$id, 4L)
  expect_equal(.tr_mcp_json(mcp(list(jsonrpc = "2.0", id = 4L, method = "ping"))),
               '{"jsonrpc":"2.0","id":4,"result":{}}')
})

test_that("tools/list traz schema de objeto, obrigatórios existentes e anotações", {
  r <- mcp(list(jsonrpc = "2.0", id = 5L, method = "tools/list"))
  tools <- r$result$tools
  nomes <- vapply(tools, `[[`, "", "name")
  expect_setequal(nomes, c("abrir", "estado", "catalogo", "explicar", "adicionar", "ajustar",
                           "ligar", "remover", "resultado", "desfazer", "aceitar_versao", "validar",
                           "renomear", "posicionar", "frame", "nota", "organizar", "guia"))
  for (t in tools) {
    expect_equal(t$inputSchema$type, "object")
    expect_true(all(unlist(t$inputSchema$required) %in% names(t$inputSchema$properties)), info = t$name)
    expect_true(nzchar(t$description))
    expect_type(t$annotations$readOnlyHint, "logical")
    expect_null(t$editor)
  }
  # Ida e volta pelo JSON: schema sem propriedades continua objeto.
  volta <- jsonlite::fromJSON(.tr_mcp_json(r), simplifyVector = FALSE)
  estado <- Filter(function(t) t$name == "estado", volta$result$tools)[[1]]
  expect_match(.tr_mcp_json(estado$inputSchema), '"properties":\\{\\}')
  # `tools` é array no JSON (lista nomeada viraria objeto e o cliente recusa).
  expect_match(.tr_mcp_json(r), '"tools":\\[')
})

test_that("catalogo e explicar respondem sem editor, sobre o registro", {
  r <- chamar("catalogo", list(busca = "add"))
  expect_false(r$isError)
  expect_true("t/add" %in% vapply(texto(r)$nodes, `[[`, "", "type"))
  e <- chamar("explicar", list(tipo = "t/add"))
  expect_equal(texto(e)$node$id, "t/add")
})

test_that("recusa e argumento faltando voltam como isError com mensagem", {
  r <- chamar("explicar", list(tipo = "t/nada"))
  expect_true(r$isError)
  expect_match(texto(r)$message, "desconhecido")
  f <- chamar("explicar")
  expect_true(f$isError)
  expect_match(texto(f)$message, "Falta argumento: tipo")
  d <- chamar("zzz")
  expect_true(d$isError)
  expect_match(texto(d)$message, "Tool desconhecida")
})

test_that("validar resolve caminho relativo à pasta do projeto", {
  dir <- withr::local_tempdir()
  writeLines('{"format":1,"nodes":{"a":{"type":"t/const","params":{"value":2}}},"edges":[]}',
             file.path(dir, "f.json"))
  r <- chamar("validar", list(arquivo = "f.json"), ctx = .tr_mcp_contexto(dir, registro = test_registry()))
  expect_false(r$isError)
  expect_true(texto(r)$ok)
})

test_that("params da tool viram param=valor que o CLI relê igual", {
  kv <- .tr_mcp_kv(list(resposta = "peso", n = 3, cols = list("a", "b"), x = TRUE))
  p <- .tr_cli_params(kv)
  expect_equal(p$resposta, "peso")
  expect_equal(p$n, 3)
  expect_equal(p$cols, list("a", "b"))
  expect_true(p$x)
  expect_length(.tr_mcp_kv(NULL), 0)
})

test_that("nada impresso durante a tool vaza para a saída do protocolo", {
  ctx <- .tr_mcp_contexto(tempdir(), registro = test_registry())
  entrada <- withr::local_tempfile(lines = c(
    .tr_mcp_json(list(jsonrpc = "2.0", id = 1L, method = "initialize", params = list())),
    .tr_mcp_json(list(jsonrpc = "2.0", method = "notifications/initialized")),
    .tr_mcp_json(list(jsonrpc = "2.0", id = 2L, method = "tools/call",
                      params = list(name = "catalogo", arguments = list(busca = "const"))))))
  saida <- withr::local_tempfile()
  local_mocked_bindings(.tr_mcp_contexto = function(...) ctx, .package = "trama")
  local_mocked_bindings(.tr_cli_sem_editor = function(...) { print("barulho"); list(ok = TRUE) }, .package = "trama")
  con <- file(saida, "w")
  suppressWarnings(utils::capture.output(tr_mcp(entrada = file(entrada), saida = con), type = "message"))
  close(con)
  linhas <- readLines(saida)
  expect_length(linhas, 2)
  for (l in linhas) expect_equal(jsonlite::fromJSON(l)$jsonrpc, "2.0")
})

test_that("resultado com PNG na prévia devolve imagem", {
  png <- withr::local_tempfile(fileext = ".png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), png)
  r <- .tr_mcp_conteudo(list(ok = TRUE, outputs = list(list(preview = list(files = list(png))))))
  expect_false(r$isError)
  expect_equal(r$content[[2]]$type, "image")
  expect_equal(r$content[[2]]$mimeType, "image/png")
})

test_that("guia responde sem editor e as instruções apontam para ele (#19)", {
  r <- chamar("guia")
  expect_false(r$isError)
  g <- texto(r)
  expect_true(any(grepl("frame", unlist(g$guia))))
  expect_true(any(grepl("conclus", unlist(g$guia))))
  expect_match(g$mcp, "posicionar")
  init <- mcp(list(jsonrpc = "2.0", id = 1L, method = "initialize", params = list()))
  expect_match(init$result$instructions, "guia")
  expect_match(init$result$instructions, "organizar")
})

test_that("tools de narrativa viram os mesmos comandos do CLI (#19, #33)", {
  t <- .tr_mcp_tools
  a <- t$posicionar$cmd(list(itens = list(list(id = "a", x = 10, y = -20)), ids = list("b"), dx = 5))
  expect_equal(a$cmd, "move")
  itens <- .tr_cli_itens(a$pos, a$opt)
  expect_equal(itens[[1]], list(id = "a", x = 10, y = -20))
  expect_equal(itens[[2]], list(id = "b", dx = 5, dy = 0))

  f <- t$frame$cmd(list(acao = "criar", titulo = "1. Dados", cor = "verde", blocos = list("a", "b")))
  m <- .tr_cli_frame(f$pos, f$opt, function(...) NULL)
  expect_equal(m[c("cmd", "acao", "title", "color")], list(cmd = "frame", acao = "add", title = "1. Dados", color = "verde"))
  expect_equal(m$nodes, list("a", "b"))
  f <- t$frame$cmd(list(acao = "editar", frame = "f1", titulo = "2. Outro", ordem = 2))
  m <- .tr_cli_frame(f$pos, f$opt, function(...) NULL)
  expect_equal(m[c("acao", "frame", "title", "order")], list(acao = "set", frame = "f1", title = "2. Outro", order = 2))

  n <- t$nota$cmd(list(acao = "criar", texto = "## Etapa\nlinha", bloco = "adf", largura = 400))
  m <- .tr_cli_nota(n$pos, n$opt, function(...) NULL)
  expect_equal(m[c("acao", "text", "node", "w")], list(acao = "add", text = "## Etapa\nlinha", node = "adf", w = 400))

  r <- t$renomear$cmd(list(no = "adf", rotulo = "ADF do log"))
  expect_equal(r, list(cmd = "rename", pos = c("adf", "ADF do log"), opt = list()))
  expect_equal(t$organizar$cmd(list())$cmd, "layout")
  expect_equal(t$adicionar$cmd(list(tipo = "t/const", frame = "f1"))$opt$frame, "f1")
})

test_that("frame e nota sem o argumento da ação recusam antes de abrir editor", {
  r <- chamar("frame", list(acao = "criar"))
  expect_true(r$isError)
  expect_match(texto(r)$message, "titulo")
  r <- chamar("nota", list(acao = "editar", texto = "x"))
  expect_true(r$isError)
  expect_match(texto(r)$message, "nota")
})
