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
                           "ligar", "remover", "resultado", "desfazer", "validar"))
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
