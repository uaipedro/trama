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
