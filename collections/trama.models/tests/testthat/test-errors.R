test_that("toda classe tr_models_error_* usada no código está em tr_models_errors()", {
  # Varre o NAMESPACE, e não os `.R` do disco: sob `R CMD check` o cwd muda e a
  # varredura pelo disco pularia em silêncio — mesma lição das irmãs.
  ns <- asNamespace("trama.models")
  objs <- mget(ls(ns, all.names = TRUE), envir = ns, inherits = FALSE)
  txt <- unlist(lapply(Filter(is.function, objs),
                       function(f) deparse(f, width.cutoff = 500L)))
  used <- unique(unlist(regmatches(
    txt, gregexpr('(?<=")tr_models_error_[a-z_]+(?=")', txt, perl = TRUE))))
  documented <- tr_models_errors()$class
  expect_setequal(setdiff(used, documented), character())
  expect_setequal(setdiff(documented, used), character())
})
