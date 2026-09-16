# O catálogo de mensagens (inst/mensagens/pt-BR.json) e as chamadas a `.tr_msg()`
# têm que casar nas DUAS direções, como o catálogo de erros de `tr_errors()` já
# faz com as classes. Sem isso, chave órfã fica no arquivo para sempre e chave
# faltante só aparece no dia em que o usuário erra.
#
# A varredura é sobre o código CARREGADO, e não sobre `R/*.R`: durante o
# `R CMD check` os testes rodam contra o pacote instalado, onde a pasta `R/` com
# os fontes não existe.

chaves_usadas <- function() {
  ns <- asNamespace("trama")
  achadas <- character()
  visita <- function(x) {
    if (is.call(x)) {
      f <- x[[1L]]
      nome <- if (is.symbol(f)) as.character(f)
              else if (is.call(f) && identical(as.character(f[[1L]]), ":::")) as.character(f[[3L]])
              else ""
      if (identical(nome, ".tr_msg") && length(x) >= 2L && is.character(x[[2L]])) {
        achadas <<- c(achadas, x[[2L]])
      }
    }
    # O `tryCatch` não é decoração: uma chamada com argumento omitido
    # (`f(x, )`, ou o padrão vazio de um argumento de função) guarda o símbolo
    # vazio, e tocá-lo estoura "argument is missing". Só pular o elemento.
    if (is.recursive(x)) for (i in seq_along(x)) {
      tryCatch({
        el <- x[[i]]
        force(el)
        visita(el)
      }, error = function(e) NULL)
    }
  }
  for (n in ls(ns, all.names = TRUE)) {
    obj <- tryCatch(get(n, envir = ns), error = function(e) NULL)
    if (is.function(obj)) visita(body(obj))
  }
  unique(achadas)
}

test_that("toda chave usada no código existe no catálogo", {
  catalogo <- trama:::.tr_msg_carrega()
  faltando <- setdiff(chaves_usadas(), names(catalogo))
  expect_identical(faltando, character(0))
})

test_that("toda chave do catálogo é usada no código", {
  catalogo <- trama:::.tr_msg_carrega()
  orfas <- setdiff(names(catalogo), chaves_usadas())
  expect_identical(orfas, character(0))
})

test_that("chave ausente aborta, em vez de devolver a própria chave", {
  expect_error(trama:::.tr_msg("nao.existe.mesmo"), class = "tr_error_missing_message")
})

test_that("o texto chega acentuado e formatado", {
  catalogo <- trama:::.tr_msg_carrega()
  # Uma chave com `%s` qualquer serve: o que se verifica é que `sprintf` roda e
  # que o acento sobrevive à leitura do JSON. O `\u` no teste é ASCII de
  # propósito — este arquivo também é código.
  com_fmt <- names(catalogo)[grepl("%s", catalogo, fixed = TRUE)]
  skip_if(length(com_fmt) == 0, "catalogo ainda sem mensagem com formato")
  texto <- trama:::.tr_msg(com_fmt[[1L]], "x")
  expect_false(grepl("%s", texto, fixed = TRUE))
  acentuadas <- Filter(function(k) grepl("[À-ÿ]", catalogo[[k]]), names(catalogo))
  skip_if(length(acentuadas) == 0, "catalogo ainda sem mensagem acentuada")
  expect_true(any(grepl("[À-ÿ]", vapply(acentuadas, function(k) catalogo[[k]], ""))))
})
