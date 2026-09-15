test_that("coluna inexistente é erro alto, com as disponíveis na mensagem", {
  d <- df_exemplo()
  expect_error(.tr_view_col(d, "regaio", "x"), class = "tr_view_error_unknown_column")
  err <- tryCatch(.tr_view_col(d, "regaio", "x"), condition = identity)
  expect_match(conditionMessage(err), "regaio")
  expect_match(conditionMessage(err), "regiao, produto, valor, qtd")
  expect_equal(.tr_view_col(d, "valor", "x"), "valor")
})

test_that("param obrigatório em branco é erro classificado, nomeando o campo", {
  err <- tryCatch(.tr_view_obrigatorio("", "x"), condition = identity)
  expect_s3_class(err, "tr_view_error_blank_param")
  expect_match(conditionMessage(err), "'x'")
  expect_equal(.tr_view_obrigatorio("valor", "x"), "valor")
})

test_that("valor fora do enum é erro classificado, listando os aceitos", {
  err <- tryCatch(.tr_view_option("tema", "escurro", c("claro", "escuro")),
                  condition = identity)
  expect_s3_class(err, "tr_view_error_bad_option")
  expect_match(conditionMessage(err), "escurro")
  expect_match(conditionMessage(err), "claro, escuro")
})

test_that("toda classe tr_view_error_* usada no código está em tr_view_errors()", {
  # Varre o NAMESPACE, não os `.R` do disco: sob `R CMD check` o cwd é
  # `<pkg>.Rcheck/tests/testthat/` e `../../R` não existe — a varredura pelo
  # disco pularia em silêncio, que é exatamente o que este teste combate.
  ns <- asNamespace("trama.view")
  objs <- mget(ls(ns, all.names = TRUE), envir = ns, inherits = FALSE)
  txt <- unlist(lapply(Filter(is.function, objs),
                       function(f) deparse(f, width.cutoff = 500L)))
  # Só ocorrência ENTRE ASPAS conta como uso: o nome nu também aparece no
  # próprio catálogo, onde as classes são nomes do `c()`, e aí a direção
  # "nada morto" nunca poderia falhar.
  used <- unique(unlist(regmatches(
    txt, gregexpr('(?<=")tr_view_error_[a-z_]+(?=")', txt, perl = TRUE))))
  documented <- tr_view_errors()$class
  expect_setequal(setdiff(used, documented), character())
  expect_setequal(setdiff(documented, used), character())
})
