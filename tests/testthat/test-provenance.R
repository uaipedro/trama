test_that("tr_provenance diz com que R e pacotes cada resultado foi produzido", {
  reg <- store_registry()
  root <- tempfile("proj"); dir.create(file.path(root, "flows"), recursive = TRUE)
  s <- tr_store(file.path(root, ".trama", "store"), project_root = root)
  project <- structure(list(root = root, registry = reg, store = s,
                            flows_dir = file.path(root, "flows")), class = "tr_project")
  doc <- build(reg, list(
    list(op = "add_node", type = "t/const", id = "a", params = list(v = 1)),
    list(op = "add_node", type = "t/inc", id = "b"),
    list(op = "connect", from_node = "a", from_port = "out", to_node = "b", to_port = "x")))
  tr_doc_write(doc, file.path(root, "flows", "main.json"))

  antes <- tr_provenance(project)
  expect_setequal(antes$node, c("a", "b"))
  expect_true(all(antes$status == "pendente"))

  tr_run(doc, registry = reg, store = s)
  pv <- tr_provenance(project)
  expect_true(all(pv$status == "ok"))
  expect_identical(unique(pv$r), paste(R.version$major, R.version$minor, sep = "."))
  expect_identical(unique(pv$pkg_trama), as.character(utils::packageVersion("trama")))
  expect_s3_class(pv$created, "POSIXct")
})
