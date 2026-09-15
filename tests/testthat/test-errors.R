test_that("toda classe tr_error_* usada no código está documentada em tr_errors()", {
  # Varre o NAMESPACE, não os `.R` do disco. A versão antiga lia `../../R`, que
  # só existe quando o teste roda a partir do fonte: sob `R CMD check` o cwd é
  # `<pkg>.Rcheck/tests/testthat/`, a pasta não existe e o teste-símbolo do
  # catálogo era PULADO — calado — justo no ambiente que mais importa. O corpo
  # das funções carregadas tem a mesma informação e está disponível em todo
  # lugar: `deparse()` reconstrói as chamadas a partir da árvore sintática.
  #
  # `deparse()` sem `useSource` também derruba comentário, o que só melhora a
  # medida: classe citada em comentário deixa de contar como uso.
  ns <- asNamespace("trama")
  objs <- mget(ls(ns, all.names = TRUE), envir = ns, inherits = FALSE)
  txt <- unlist(lapply(Filter(is.function, objs),
                       function(f) deparse(f, width.cutoff = 500L)))
  # Só ocorrência ENTRE ASPAS conta como uso. O grep pelo nome nu também
  # varria o próprio catálogo — onde as classes aparecem como nomes do `c()`,
  # `tr_error_x = "..."` — então `setdiff(documented, used)` era vazio por
  # construção e a direção "nada morto" nunca podia falhar. Aspas separam as
  # duas formas: o nome no catálogo é nu, o uso é string.
  # Não basta casar `class = "..."`: `tr_error_cancelled` e
  # `tr_error_worker_died` são escolhidos numa variável antes do `abort()`.
  used <- unique(unlist(regmatches(
    txt, gregexpr('(?<=")tr_error_[a-z_]+(?=")', txt, perl = TRUE))))
  documented <- tr_errors()$class
  expect_setequal(setdiff(used, documented), character())
  expect_setequal(setdiff(documented, used), character())   # e nada morto
})
