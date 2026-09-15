test_that("logger grava um evento por linha, sem handles, e tr_runs lê de volta", {
  root <- tempfile("proj"); dir.create(root)
  reg <- store_registry(); s <- tr_store(file.path(root, ".trama", "store"), project_root = root)
  lg <- tr_run_logger(file.path(root, ".trama", "runs"), "7")
  doc <- build(reg, list(list(op = "add_node", type = "t/const", id = "a", params = list(v = 1))))
  tr_run(doc, registry = reg, store = s, run_id = "7", on_event = lg$log); lg$close()
  df <- tr_runs(list(root = root))
  expect_true(all(c("running", "done", "run_finished") %in% df$type))
  expect_false(grepl("handles", paste(readLines(lg$path), collapse = "")))
  expect_true(is.finite(df$duration[df$type == "done"][[1]]))
})

test_that("tr_project_gc preserva o que QUALQUER flow do projeto alcança", {
  reg <- store_registry()
  root <- tempfile("proj"); dir.create(root)
  dir.create(file.path(root, "flows"), recursive = TRUE)
  s <- tr_store(file.path(root, ".trama", "store"), project_root = root)
  # `tr_project()` carrega coleções por NOME DE PACOTE — a coleção de teste
  # não é um pacote, então o projeto é montado à mão, com a mesma forma.
  project <- structure(list(root = root, registry = reg, store = s,
                            flows_dir = file.path(root, "flows")), class = "tr_project")

  doc1 <- build(reg, list(list(op = "add_node", type = "t/const", id = "a", params = list(v = 1))))
  doc2 <- build(reg, list(list(op = "add_node", type = "t/const", id = "b", params = list(v = 2))))
  tr_doc_write(doc1, file.path(root, "flows", "main.json"))
  tr_doc_write(doc2, file.path(root, "flows", "outro.json"))
  tr_run(doc1, registry = reg, store = s)
  tr_run(doc2, registry = reg, store = s)

  # A chave GRAVADA no store é a de SAÍDA (`outputs$out`), não `unit$key` — a
  # chave da unidade é a base a partir da qual `.tr_out_key()` deriva uma
  # chave por porta (plan.R).
  key1 <- tr_plan(doc1, registry = reg, store = s)$units$a$outputs$out
  key2 <- tr_plan(doc2, registry = reg, store = s)$units$b$outputs$out

  # Chave órfã (fora de qualquer flow), e handles envelhecidos à força — GC
  # só remove por idade, então sem isso `max_age_days = 0` não provaria nada.
  orphan_key <- "orfa000000000000000000000000000"
  tr_store_put(s, orphan_key, list(v = 99), tr_get_type("t/box", reg))
  for (k in c(key1, key2, orphan_key)) {
    h <- tr_store_handle(s, k); h$created <- 0
    jsonlite::write_json(h, file.path(s$root, "handles", paste0(k, ".json")),
                         auto_unbox = TRUE, null = "null", digits = NA)
  }

  removed <- tr_project_gc(project, max_age_days = 0)
  expect_equal(removed, 1L)
  expect_true(tr_store_has(s, key1)); expect_true(tr_store_has(s, key2))
  expect_false(tr_store_has(s, orphan_key))
})
