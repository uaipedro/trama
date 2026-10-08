test_that("a tabela de erros está bem formada", {
  e <- tr_spatial_errors()
  expect_true(is.data.frame(e))
  expect_equal(names(e), c("class", "when"))
  expect_true(all(nzchar(e$class)))
  expect_true(all(nzchar(e$when)))
  expect_true(all(startsWith(e$class, "tr_spatial_error_")))
  expect_false(anyDuplicated(e$class) > 0L)
})

test_that("toda classe tr_spatial_error_* usada no código está em tr_spatial_errors()", {
  # Varre o NAMESPACE, e não os `.R` do disco: sob `R CMD check` o cwd muda e a
  # varredura pelo disco pularia em silêncio — mesma lição das irmãs.
  #
  # Só a direção "usada mas não documentada". A inversa ("documentada mas nunca
  # levantada") falharia hoje por motivo legítimo, sem nó que levante nada; entra
  # numa tarefa posterior, quando houver pontos de chamada reais.
  ns <- asNamespace("trama.spatial")
  objs <- mget(ls(ns, all.names = TRUE), envir = ns, inherits = FALSE)
  txt <- unlist(lapply(Filter(is.function, objs),
                       function(f) deparse(f, width.cutoff = 500L)))
  used <- unique(unlist(regmatches(
    txt, gregexpr('(?<=")tr_spatial_error_[a-z_]+(?=")', txt, perl = TRUE))))
  expect_setequal(setdiff(used, tr_spatial_errors()$class), character())
})

test_that(".tr_spatial_abort carrega a classe pedida e a classe guarda-chuva", {
  expect_error(.tr_spatial_abort("tr_spatial_error_blank_param", "vazio"),
               class = "tr_spatial_error_blank_param")
  expect_error(.tr_spatial_abort("tr_spatial_error_blank_param", "vazio"),
               class = "tr_spatial_error")
})
