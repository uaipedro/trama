test_that("toda classe tr_experiments_error_* usada no código está em tr_experiments_errors()", {
  ns <- asNamespace("trama.experiments")
  objs <- mget(ls(ns, all.names = TRUE), envir = ns, inherits = FALSE)
  txt <- unlist(lapply(Filter(is.function, objs), function(f) deparse(f, width.cutoff = 500L)))
  used <- unique(unlist(regmatches(txt, gregexpr('(?<=")tr_experiments_error_[a-z_]+(?=")', txt, perl = TRUE))))
  documented <- tr_experiments_errors()$class
  expect_setequal(setdiff(used, documented), character())
  expect_setequal(setdiff(documented, used), character())
})
