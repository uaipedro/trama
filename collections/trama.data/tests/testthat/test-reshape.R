# Reformatar é o caso em que o round-trip prova os dois nós de uma vez: se
# `longer` empilha certo e `wider` espalha certo, voltar ao ponto de partida é
# a única asserção que os dois precisam passar juntos.

test_that("longer e wider fecham o círculo", {
  d <- tibble::tibble(regiao = c("sul", "norte"), jan = c(10, 30), fev = c(20, 40))
  lg <- tr_pivot_longer(d, "jan, fev", names_to = "mes", values_to = "total")
  expect_equal(nrow(lg), 4L)
  expect_equal(names(lg), c("regiao", "mes", "total"))
  expect_equal(sort(unique(lg$mes)), c("fev", "jan"))

  wd <- tr_pivot_wider(lg, names_from = "mes", values_from = "total")
  expect_equal(nrow(wd), 2L)
  expect_setequal(names(wd), names(d))
})

test_that("pivot com coluna inexistente falha alto", {
  d <- tibble::tibble(a = 1, b = 2)
  expect_error(tr_pivot_longer(d, "c"), class = "tr_data_error_unknown_column")
  expect_error(tr_pivot_wider(d, names_from = "c", values_from = "b"),
               class = "tr_data_error_unknown_column")
})

# A classe tem que estar em PRIMEIRA ordem: o motor grava `class(e)[1]` no
# card, e um erro embrulhado por verbo tidyr deixaria a classe útil só no pai.
test_that("erro de coluna do pivot chega em primeira ordem", {
  d <- tibble::tibble(a = 1, b = 2)
  expect_equal(class(tryCatch(tr_pivot_longer(d, "c"), error = identity))[[1]],
               "tr_data_error_unknown_column")
  expect_equal(class(tryCatch(tr_pivot_wider(d, names_from = "c", values_from = "b"),
                              error = identity))[[1]],
               "tr_data_error_unknown_column")
  expect_equal(class(tryCatch(tr_pivot_wider(d, names_from = "a", values_from = "z"),
                              error = identity))[[1]],
               "tr_data_error_unknown_column")
})

test_that("longer sem colunas devolve a tabela intacta", {
  d <- df_exemplo()
  expect_identical(tr_pivot_longer(d, ""), d)
})

# `values_fill` é campo de texto do card: número escrito tem que voltar número,
# senão espalhar uma coluna numérica a transformaria em texto só por causa do
# preenchimento.
test_that("values_fill de texto vira número quando é número", {
  d <- tibble::tibble(regiao = c("sul", "norte", "sul"),
                      mes    = c("jan", "jan", "fev"),
                      total  = c(10, 30, 20))
  vazio <- tr_pivot_wider(d, names_from = "mes", values_from = "total")
  expect_true(is.na(vazio$fev[vazio$regiao == "norte"]))

  zero <- tr_pivot_wider(d, names_from = "mes", values_from = "total", values_fill = "0")
  expect_equal(zero$fev[zero$regiao == "norte"], 0)
  expect_true(is.numeric(zero$fev))

  # Preenchimento de texto sobre coluna numérica NÃO é convertido em silêncio:
  # o vctrs recusa o cast. Falha alto, que é o desfecho certo — só não com
  # classe própria da coleção.
  expect_error(tr_pivot_wider(d, names_from = "mes", values_from = "total",
                              values_fill = "vazio"),
               class = "vctrs_error_incompatible_type")

  txt <- tr_pivot_wider(tibble::tibble(regiao = c("sul", "norte", "sul"),
                                       mes    = c("jan", "jan", "fev"),
                                       total  = c("a", "b", "c")),
                        names_from = "mes", values_from = "total", values_fill = "vazio")
  expect_equal(txt$fev[txt$regiao == "norte"], "vazio")
})

# Combinação repetida ABORTA. O tidyr só avisa e devolve coluna de listas, e o
# aviso não sobe até o card: o `fmt()` do runtime faz `String(v)` na lista e a
# célula mostra "1,2" — um número plausível, na tela, que não existe no dado.
test_that("combinação duplicada aborta em vez de virar coluna de listas", {
  d <- tibble::tibble(r = c("sul", "sul"), m = c("jan", "jan"), v = c(1, 2))
  expect_error(tr_pivot_wider(d, names_from = "m", values_from = "v"),
               class = "tr_data_error_duplicate_key")
  err <- tryCatch(tr_pivot_wider(d, names_from = "m", values_from = "v"),
                  error = identity)
  expect_equal(class(err)[[1]], "tr_data_error_duplicate_key")
  expect_match(conditionMessage(err), "r=sul, m=jan")
  expect_match(conditionMessage(err), "group_summarise")

  # Sem repetição, segue passando.
  ok <- tibble::tibble(r = c("sul", "sul"), m = c("jan", "fev"), v = c(1, 2))
  expect_equal(nrow(tr_pivot_wider(ok, names_from = "m", values_from = "v")), 1L)
})

# A mensagem lista no máximo 3 combinações e conta o resto, como `tr_convert`.
test_that("a mensagem de chave repetida corta em 3 e conta o resto", {
  d <- tibble::tibble(r = rep(c("a", "b", "c", "d"), each = 2L),
                      m = "jan", v = 1)
  err <- tryCatch(tr_pivot_wider(d, names_from = "m", values_from = "v"),
                  error = identity)
  expect_match(conditionMessage(err), "e mais 1")
})

# Campo obrigatório em branco: todo `pivot_wider` recém-arrastado da paleta cai
# aqui, porque o default do spec é "". Sem a checagem, a mensagem era
# "coluna(s) inexistente(s): ." — o nome vazio não renderiza.
test_that("wider com campo obrigatório em branco diz qual campo é", {
  d <- tibble::tibble(a = 1, b = 2)
  cls <- function(...) class(tryCatch(tr_pivot_wider(...), error = identity))[[1]]
  expect_equal(cls(d, names_from = "", values_from = "b"), "tr_data_error_blank_param")
  expect_equal(cls(d, names_from = "a", values_from = ""), "tr_data_error_blank_param")
  err <- tryCatch(tr_pivot_wider(d, names_from = "", values_from = "b"), error = identity)
  expect_match(conditionMessage(err), "names_from")
})

# `names_to`/`values_to` têm default NÃO vazio no spec: em branco não é
# "desligado", é card incompleto. Inventar de volta o default faria a tela
# discordar do resultado — o card mostraria vazio e a tabela, "nome".
test_that("longer com names_to/values_to em branco aborta dizendo o campo", {
  d <- tibble::tibble(a = 1, b = 2)
  cls <- function(...) class(tryCatch(tr_pivot_longer(...), error = identity))[[1]]
  expect_equal(cls(d, "a", names_to = ""), "tr_data_error_blank_param")
  expect_equal(cls(d, "a", values_to = ""), "tr_data_error_blank_param")
  err <- tryCatch(tr_pivot_longer(d, "a", values_to = "  "), error = identity)
  expect_match(conditionMessage(err), "values_to")

  # Sem colunas o nó é no-op e nem chega a olhar os outros campos.
  expect_identical(tr_pivot_longer(d, "", names_to = ""), d)
})

# `tabelas` é a PRIMEIRA porta variádica com uso real: `multiple = TRUE` existe
# no núcleo desde o primeiro commit e nenhum nó a usava. Por isso os testes de
# empilhar vão pelo GRAFO — o caminho de `R/plan.R:67` (lista na ordem do
# `index`) só é exercitado assim.

test_that("empilhar aceita N tabelas, na ordem das arestas", {
  reg <- data_registry(); s <- trama::tr_store(tempfile())
  flow <- trama::tr_flow(reg) |>
    trama::tr_add("a", "data/example", dataset = "mtcars") |>
    trama::tr_add("b", "data/example", dataset = "mtcars") |>
    trama::tr_add("junta", "data/bind_rows", from = c("a", "b"))
  out <- trama::tr_value(flow, "junta", reg, s)
  expect_equal(nrow(out), 64L)
})

test_that("empilhar no nível 1 recebe lista", {
  d <- df_exemplo()
  expect_equal(nrow(tr_bind_rows(list(d, d))), 2L * nrow(d))
})

# Duas tabelas IGUAIS não provam ordem nenhuma. Aqui as duas fontes são
# conjuntos diferentes: se a ordem do `index` fosse ignorada, a primeira linha
# do resultado viria do `iris`.
test_that("a ordem do resultado é a ordem das arestas, não a de inserção", {
  reg <- data_registry(); s <- trama::tr_store(tempfile())
  base <- trama::tr_flow(reg) |>
    trama::tr_add("carros", "data/example", dataset = "mtcars") |>
    trama::tr_add("flores", "data/example", dataset = "iris")

  ab <- base |> trama::tr_add("junta", "data/bind_rows", from = c("carros", "flores"))
  out_ab <- trama::tr_value(ab, "junta", reg, s)
  expect_equal(nrow(out_ab), 32L + 150L)
  expect_false(is.na(out_ab$mpg[[1]]))
  expect_true(is.na(out_ab$Species[[1]]))
  expect_equal(names(out_ab)[[1]], "nome")

  ba <- base |> trama::tr_add("junta", "data/bind_rows", from = c("flores", "carros"))
  out_ba <- trama::tr_value(ba, "junta", reg, s)
  expect_equal(nrow(out_ba), 32L + 150L)
  expect_true(is.na(out_ba$mpg[[1]]))
  expect_false(is.na(out_ba$Species[[1]]))
  expect_equal(names(out_ba)[[1]], "Sepal.Length")
})

# O autolink tem que numerar as arestas da porta variádica: sem `index`
# crescente, `R/plan.R:67` ordenaria tudo em 1 e a ordem viraria a de inserção.
test_that("autolink numera as arestas da porta variádica", {
  reg <- data_registry()
  doc <- trama::tr_flow(reg) |>
    trama::tr_add("a", "data/example", dataset = "mtcars") |>
    trama::tr_add("b", "data/example", dataset = "iris") |>
    trama::tr_add("c", "data/example", dataset = "airquality") |>
    trama::tr_add("junta", "data/bind_rows", from = c("a", "b", "c")) |>
    trama::tr_flow_doc()
  es <- Filter(function(e) e$to$node == "junta", doc$edges)
  expect_equal(vapply(es, function(e) e$from$node, ""), c("a", "b", "c"))
  expect_equal(vapply(es, function(e) as.integer(e$index %||% 1L), 1L), 1:3)
  expect_true(all(vapply(es, function(e) e$to$port, "") == "tabelas"))
})

# Round-trip: o código regenerado tem que reconstruir o MESMO grafo, arestas
# variádicas incluídas.
test_that("tr_flow_code reconstrói o fluxo com porta variádica", {
  reg <- data_registry()
  doc <- trama::tr_flow(reg) |>
    trama::tr_add("a", "data/example", dataset = "mtcars") |>
    trama::tr_add("b", "data/example", dataset = "iris") |>
    trama::tr_add("junta", "data/bind_rows", from = c("a", "b")) |>
    trama::tr_flow_doc()
  code <- trama::tr_flow_code(doc, reg)
  volta <- avaliar_codigo(code, reg)
  d2 <- trama::tr_flow_doc(volta)
  norm <- function(d) lapply(d$edges, function(e) c(e$from$node, e$from$port, e$to$node,
                                                    e$to$port, as.character(e$index %||% 1L)))
  expect_equal(norm(d2), norm(doc))
  expect_equal(names(d2$nodes), names(doc$nodes))
  expect_equal(trama::tr_flow_code(d2, reg), code)
})

# Uma tabela só ligada: a lista tem um elemento e o resultado é a própria
# tabela. Nenhuma ligada: porta obrigatória solta, o plano bloqueia ANTES de
# rodar (o erro nunca chega de dentro do `fn`).
test_that("empilhar com uma tabela só devolve a tabela", {
  reg <- data_registry(); s <- trama::tr_store(tempfile())
  flow <- trama::tr_flow(reg) |>
    trama::tr_add("a", "data/example", dataset = "mtcars") |>
    trama::tr_add("junta", "data/bind_rows", from = "a")
  expect_equal(nrow(trama::tr_value(flow, "junta", reg, s)), 32L)
})

test_that("empilhar sem nenhuma tabela é porta obrigatória solta", {
  reg <- data_registry()
  doc <- trama::tr_flow(reg) |>
    trama::tr_add("junta", "data/bind_rows") |>
    trama::tr_flow_doc()
  u <- trama::tr_plan(doc, registry = reg)$units[["junta"]]
  expect_equal(u$invalid, "missing_required_input:tabelas")
})

# "Casando as colunas pelo nome" é promessa da description: coluna que só
# existe numa das tabelas vira NA na outra, sem warning.
test_that("colunas diferentes viram NA, sem aviso", {
  a <- tibble::tibble(x = 1:2, y = c("p", "q"))
  b <- tibble::tibble(x = 3L, z = TRUE)
  expect_silent(out <- tr_bind_rows(list(a, b)))
  expect_setequal(names(out), c("x", "y", "z"))
  expect_equal(out$x, c(1L, 2L, 3L))
  expect_equal(out$y, c("p", "q", NA))
  expect_equal(out$z, c(NA, NA, TRUE))
})

# Tipos incompatíveis na MESMA coluna: o vctrs recusa o cast. Falha alto, que é
# o desfecho certo — só não com classe própria da coleção. Pinado aqui pra não
# mudar sem alguém reparar.
test_that("tipos incompatíveis na mesma coluna falham alto", {
  a <- tibble::tibble(valor = 1)
  b <- tibble::tibble(valor = "muito")
  expect_error(tr_bind_rows(list(a, b)), class = "vctrs_error_incompatible_type")
})

# `data/rename` tem param chamado `from` e `data/join` tem param chamado `type`:
# os dois nomes são formais de `tr_add()`. O gerador precisa mandá-los pro
# `tr_set()`, senão o código sai com argumento duplicado e nem roda. Round-trip
# completo — gerar, AVALIAR o gerado, comparar as chaves de plano — é a prova.
test_that("tr_flow_code roda de volta com params que colidem com a DSL", {
  reg <- data_registry()
  doc <- trama::tr_flow(reg) |>
    trama::tr_add("ler", "data/example", dataset = "mtcars") |>
    trama::tr_add("ren", "data/rename") |>
    trama::tr_link("ler", "ren") |>
    trama::tr_set("ren", from = "mpg", to = "consumo") |>
    trama::tr_add("outra", "data/example", dataset = "iris") |>
    trama::tr_add("j", "data/join", by = "x") |>
    trama::tr_link("ren", "j:left") |>
    trama::tr_link("outra", "j:right") |>
    trama::tr_set("j", type = "left") |>
    trama::tr_flow_doc()
  code <- trama::tr_flow_code(doc, reg)
  expect_no_match(code, "tr_add\\([^)]*from = \"mpg\"")
  expect_no_match(code, "tr_add\\([^)]*type = \"left\"")

  d2 <- trama::tr_flow_doc(avaliar_codigo(code, reg))
  expect_equal(trama::tr_plan(d2, registry = reg)$keys[names(doc$nodes)],
               trama::tr_plan(doc, registry = reg)$keys[names(doc$nodes)])
  expect_equal(d2$nodes$ren$params$from, "mpg")
  expect_equal(d2$nodes$j$params$type, "left")
})
