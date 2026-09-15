# O pool é onde a tese ("o coordenador nunca carrega valor") se prova de fato.
# Até aqui estava validado só por leitura. Este arquivo roda daemons de
# verdade, com o núcleo e a coleção carregados por pkgload dentro deles.

skip_pool <- function() {
  skip_if_not_installed("mirai"); skip_if_not_installed("dplyr"); skip_if_not_installed("readr")
  skip_if_not(dir.exists("../../collections/trama.data"), "coleção trama.data ausente")
}

pool_setup <- function() {
  core <- normalizePath("../.."); coll <- normalizePath("../../collections/trama.data")
  bquote({
    pkgload::load_all(.(core), quiet = TRUE, attach = FALSE)
    pkgload::load_all(.(coll), quiet = TRUE, attach = FALSE, export_all = FALSE)
  })
}

test_that("pool roda o ETL nos daemons e o resultado bate com o sequencial", {
  skip_pool()
  reg <- etl_registry(); csv <- fixture_csv(); doc <- etl_doc(reg, csv)
  s1 <- tmp_store(); s2 <- tmp_store()
  seq <- tr_run(doc, registry = reg, store = s1)
  ex <- tr_executor_pool(2L, registry = reg, setup = pool_setup())
  on.exit(ex$shutdown(), add = TRUE)
  ev <- list()
  par <- tr_run(doc, registry = reg, store = s2, executor = ex,
                on_event = function(e) ev[[length(ev) + 1]] <<- e)
  expect_setequal(par$done, seq$done)
  ty <- tr_get_type("data/table", reg)
  # `results[[node]]` guarda o HANDLE por porta, não a chave nua (mesma
  # ressalva de test-scheduler.R) — `$key` é o campo que `tr_store_get` espera.
  expect_equal(tr_store_get(s2, par$results$ordenar$out$key, ty), tr_store_get(s1, seq$results$ordenar$out$key, ty))
  # Nenhum valor atravessou: o evento `done` carrega handle, não dado.
  d <- Filter(function(e) identical(e$type, "done"), ev)
  expect_true(all(vapply(d, function(e) is.null(e$handles[[1]]$value), TRUE)))
})

test_that("handoff cancela unidade em voo no pool e o progresso chega como evento", {
  skip_pool()
  core <- normalizePath("../.."); slowp <- normalizePath("fixtures/trama.slow")
  reg <- tr_registry()
  pkgload::load_all(slowp, quiet = TRUE, attach = FALSE); tr_use("trama.slow", registry = reg)
  ex <- tr_executor_pool(1L, registry = reg, setup = bquote({
    pkgload::load_all(.(core), quiet = TRUE, attach = FALSE)
    pkgload::load_all(.(slowp), quiet = TRUE, attach = FALSE)
  }))
  on.exit(ex$shutdown(), add = TRUE)
  s <- tmp_store(); ev <- list(); rec <- function(e) ev[[length(ev) + 1]] <<- e
  doc1 <- build(reg, list(list(op = "add_node", type = "slow/sleep", id = "z", params = list(secs = 3))))
  s1 <- tr_scheduler(tr_plan(doc1, registry = reg, store = s), reg, s, ex, rec)
  t0 <- Sys.time()
  while (!s1$step() && !any(vapply(ev, function(e) identical(e$type, "progress"), TRUE))) Sys.sleep(0.05)
  expect_true(any(vapply(ev, function(e) identical(e$type, "progress"), TRUE)))
  doc2 <- tr_doc_apply(doc1, list(op = "set_param", node = "z", name = "secs", value = 0.1), reg)
  plan2 <- tr_plan(doc2, registry = reg, store = s)
  expect_length(s1$handoff(unlist(plan2$keys)), 0L)
  s2 <- tr_scheduler(plan2, reg, s, ex, rec); while (!s2$step()) Sys.sleep(0.02)
  expect_lt(as.numeric(Sys.time() - t0, units = "secs"), 3)   # não esperou os 3s do cancelado
  expect_equal(tr_store_get(s, s2$result()$results$z$out$key, tr_get_type("slow/x", reg)), 0.1)
})
