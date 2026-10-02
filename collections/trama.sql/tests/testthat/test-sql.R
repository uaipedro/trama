test_that("pasta, arquivo avulso e bancos locais viram tabelas consultáveis", {
  root <- tempfile(); dir.create(root)
  a <- data.frame(id = 1:3, grupo = c("a", "b", "c"))
  b <- data.frame(id = 1:3, valor = c(10, 20, 30))
  utils::write.csv(a, file.path(root, "pessoas.csv"), row.names = FALSE)
  skip_if_not_installed("arrow")
  arrow::write_parquet(b, file.path(root, "medidas.parquet"))
  src <- tr_sql_source(root)
  expect_setequal(vapply(src$tabelas, `[[`, "", "nome"), c("pessoas", "medidas"))
  pessoas <- src$tabelas[[which(vapply(src$tabelas, `[[`, "", "nome") == "pessoas")]]
  expect_equal(vapply(pessoas$colunas, `[[`, "", "nome"), c("id", "grupo"))
  q <- "SELECT p.grupo, m.valor FROM pessoas p JOIN medidas m USING (id) ORDER BY id"
  got <- tr_sql_query(src, q)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  DBI::dbExecute(con, sprintf("CREATE VIEW pessoas AS SELECT * FROM read_csv_auto('%s')", file.path(root, "pessoas.csv")))
  DBI::dbExecute(con, sprintf("CREATE VIEW medidas AS SELECT * FROM read_parquet('%s')", file.path(root, "medidas.parquet")))
  expect_equal(got, DBI::dbGetQuery(con, q)); DBI::dbDisconnect(con, shutdown = TRUE)
  expect_equal(tr_sql_query(tr_sql_source(file.path(root, "pessoas.csv")), "SELECT * FROM pessoas")$id, 1:3)

  ddb <- file.path(root, "local.duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ddb)
  DBI::dbWriteTable(con, "coisas", a); DBI::dbDisconnect(con, shutdown = TRUE)
  expect_equal(nrow(tr_sql_query(tr_sql_source(ddb), "SELECT * FROM coisas")), 3L)

})

test_that("bancos SQLite funcionam quando RSQLite está instalado", {
  skip_if_not_installed("RSQLite")
  sqlite <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite)
  DBI::dbWriteTable(con, "coisas", data.frame(id = 1:3))
  DBI::dbDisconnect(con)
  expect_equal(nrow(tr_sql_query(tr_sql_source(sqlite), "SELECT * FROM coisas")), 3L)
})

test_that("SQLite ausente produz instrução de instalação útil", {
  skip_if(requireNamespace("RSQLite", quietly = TRUE))
  sqlite <- tempfile(fileext = ".sqlite"); file.create(sqlite)
  expect_error(tr_sql_source(sqlite), "install.packages.*RSQLite")
})

test_that("catálogo registra fonte tipada e consulta com saída data/table", {
  reg <- trama::tr_registry()
  trama::tr_use("trama.data", registry = reg)
  trama::tr_use(trama_collection(), registry = reg)
  cat <- trama::tr_catalog(reg)
  nodes <- stats::setNames(cat$nodes, vapply(cat$nodes, `[[`, "", "id"))
  expect_equal(nodes[["sql/source"]]$outputs[[1]]$type, "sql/source")
  expect_equal(nodes[["sql/query"]]$inputs[[1]]$name, "fonte")
  expect_equal(nodes[["sql/query"]]$outputs[[1]]$type, "data/table")
  expect_equal(nodes[["sql/query"]]$params[[1]]$kind, "sql")
})

test_that("fingerprint acompanha tamanho e mtime e SQL mutável é recusado", {
  f <- tempfile(fileext = ".csv"); writeLines("x\n1", f)
  p <- list(caminho = f)
  unchanged <- .tr_sql_fingerprint(p)
  expect_identical(unchanged, .tr_sql_fingerprint(p))
  Sys.sleep(1.1); writeLines(c("x", "123456"), f)
  expect_false(identical(unchanged, .tr_sql_fingerprint(p)))
  expect_error(tr_sql_query(list(tipo = "arquivo", caminho = f), "DELETE FROM x"), "SELECT ou WITH")
  expect_error(tr_sql_query(list(tipo = "arquivo", caminho = f), "WITH a AS (SELECT 1) DELETE FROM x"), "leitura")
  fonte <- tr_sql_source(f)
  expect_equal(tr_sql_query(fonte, "SELECT 'delete' AS acao")$acao, "delete")
  expect_error(tr_sql_query(list(tipo = "arquivo", caminho = f), "DROP TABLE x"), "SELECT ou WITH")
})

test_that("arquivo alterado muda o valor da fonte (chave de cache da consulta)", {
  f <- tempfile(fileext = ".csv"); writeLines("x\n1", f)
  antes <- tr_sql_source(f)
  Sys.sleep(1.1); writeLines("x\n2", f)
  depois <- tr_sql_source(f)
  expect_identical(antes$tabelas, depois$tabelas)
  expect_false(identical(antes, depois))
  nome <- depois$tabelas[[1]]$nome
  expect_equal(tr_sql_query(depois, paste("SELECT x FROM", nome))$x, 2)
})

test_that("caminho relativo resolve pela raiz do projeto (ctx), na fonte e no fingerprint", {
  raiz <- tempfile(); dir.create(file.path(raiz, "dados"), recursive = TRUE)
  writeLines("x\n1", file.path(raiz, "dados", "a.csv"))
  ctx <- list(path = function(p) file.path(raiz, p))
  expect_equal(tr_sql_source("dados", .ctx = ctx)$tabelas[[1]]$nome, "a")
  fp <- .tr_sql_fingerprint(list(caminho = "dados"), ctx)
  expect_equal(nrow(fp$arquivos), 1L)
})
