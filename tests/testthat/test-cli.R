# CLI (R/cli.R): toda saída é JSON em stdout, inclusive ajuda e erro de
# argumento. Agente que lê stdout não pode receber backtrace de R.

cli_json <- function(args) {
  out <- utils::capture.output(res <- tr_cli(args))
  list(res = res, json = jsonlite::fromJSON(paste(out, collapse = "\n"), simplifyVector = FALSE))
}

test_that("help sai em JSON com a lista de usos", {
  r <- cli_json("help")
  expect_true(r$json$ok)
  expect_true(length(r$json$uso) > 3)
})

test_that("opção sem valor vira recusa JSON, não erro de R", {
  r <- cli_json(c("state", "--projeto"))
  expect_false(r$json$ok)
  expect_equal(r$json$reason, "args")
  expect_match(r$json$message, "sem valor")
})

test_that("--json é aceita como bandeira", {
  a <- .tr_cli_parse(c("catalog", "--json"))
  expect_true(a$opt$json)
  expect_equal(a$cmd, "catalog")
})

# Modo sem editor: catalog/explain/validate sobre um registro, sem HTTP.
sem_editor <- function(args, reg = test_registry()) {
  a <- .tr_cli_parse(args)
  precisa <- function(n, uso) if (length(a$pos) < n) rlang::abort(paste("Uso:", uso))
  .tr_cli_sem_editor(a, a$pos, precisa, registro = reg)
}

test_that("catalog offline lista os blocos do registro", {
  r <- sem_editor(c("catalog", "--offline"))
  expect_true(r$ok)
  expect_true(r$offline)
  expect_setequal(vapply(r$nodes, `[[`, "", "type"), c("t/const", "t/add", "t/show"))
})

test_that("explain devolve o bloco inteiro e recusa tipo desconhecido", {
  r <- sem_editor(c("explain", "t/add"))
  expect_equal(r$node$id, "t/add")
  expect_equal(vapply(r$node$inputs, `[[`, "", "name"), c("a", "b"))
  expect_equal(r$node$params[[1]]$name, "k")
  expect_error(sem_editor(c("explain", "t/nada")), "desconhecido")
})

test_that("validate aponta problemas de um documento escrito à mão", {
  f <- withr::local_tempfile(fileext = ".json")
  writeLines('{"format":1,"nodes":{
    "a":{"type":"t/const","params":{"value":2}},
    "s":{"type":"t/add","params":{"zzz":1}},
    "o":{"type":"x/orfao"}},
    "edges":[{"from":{"node":"a","port":"out"},"to":{"node":"s","port":"a"}}]}', f)
  r <- sem_editor(c("validate", f))
  expect_false(r$ok)
  kinds <- vapply(r$problems, `[[`, "", "kind")
  expect_true(all(c("unknown_param", "unknown_node_type", "missing_required_input") %in% kinds))
  expect_equal(r$nodes, 3L)
})

test_that("validate passa num documento válido", {
  f <- withr::local_tempfile(fileext = ".json")
  writeLines('{"format":1,"nodes":{"a":{"type":"t/const","params":{"value":2}}},"edges":[]}', f)
  r <- sem_editor(c("validate", f))
  expect_true(r$ok)
  expect_length(r$problems, 0)
})

test_that("validate sem arquivo vira recusa JSON pelo tr_cli", {
  r <- cli_json(c("validate", "/nao/existe.json"))
  expect_false(r$json$ok)
  expect_match(r$json$message, "encontrado")
})

test_that("guide sai sem editor, com o guia de narrativa (#19)", {
  out <- utils::capture.output(res <- tr_cli("guide"))
  expect_true(res$ok)
  expect_true(any(grepl("frame", res$guia)))
  expect_true(jsonlite::validate(paste(out, collapse = "\n")))
})

test_that("move, frame e note: argumentos viram o comando do canal", {
  it <- .tr_cli_itens(c("a=1,2", "b"), list(dx = "3"))
  expect_equal(it, list(list(id = "a", x = 1, y = 2), list(id = "b", dx = 3, dy = 0)))
  expect_error(.tr_cli_itens("a=1", list()), "id=x,y")
  expect_error(.tr_cli_itens("a", list()), "--dx")
  expect_error(.tr_cli_itens("a", list(dx = "muito")), "n\u00famero")

  precisa <- function(n, uso) NULL
  f <- .tr_cli_frame(c("add", "1. Dados", "a", "b"), list(id = "f1", color = "verde"), precisa)
  expect_equal(f, list(cmd = "frame", acao = "add", title = "1. Dados", id = "f1", color = "verde",
                       nodes = list("a", "b")))
  f <- .tr_cli_frame(c("set", "f1"), list(order = "2", title = "Outro"), precisa)
  expect_equal(f[c("acao", "frame", "title", "order")], list(acao = "set", frame = "f1", title = "Outro", order = 2))

  md <- withr::local_tempfile(lines = c("## Etapa", "", "o que se testa"), fileext = ".md")
  n <- .tr_cli_nota(c("add"), list(file = md, node = "adf"), precisa)
  expect_equal(n$text, "## Etapa\n\no que se testa")
  expect_equal(n$node, "adf")
  n <- .tr_cli_nota(c("add", "texto", "solto"), list(x = "10", y = "20"), precisa)
  expect_equal(n[c("text", "x", "y")], list(text = "texto solto", x = 10, y = 20))
  expect_error(.tr_cli_nota("add", list(), precisa), "markdown")
})
