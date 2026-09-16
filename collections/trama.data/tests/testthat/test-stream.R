# A fronteira da região de fluxo: `data/to_stream` fatia a tabela em pontos e
# `data/from_stream` empilha o histórico de volta.
#
# Os dois se testam pelo NÍVEL 1, como o resto da coleção — `tr_to_stream(d, 3)`
# é chamada de função R comum. O que NÃO se testa pelo nível 1 é a elevação: que
# `data/filter` e `data/mutate` funcionem dentro da região sem mudar uma linha é
# propriedade do driver aplicando o catálogo existente, e só o grafo mostra.

# ---- data/to_stream: fatiar em pontos -----------------------------------

dez_linhas <- function() {
  tibble::tibble(i = 1:10, letra = letters[1:10], valor = (10:1) * 1.5)
}

test_that("lote = 1 dá um ponto por linha, na ordem da tabela", {
  pts <- tr_to_stream(dez_linhas(), lote = 1L)
  expect_length(pts, 10L)
  expect_true(all(vapply(pts, nrow, integer(1)) == 1L))
  # O ponto é uma TABELA de uma linha, com as colunas da entrada — é isso que
  # faz `data/filter` receber o que sempre recebeu (Decisão 4).
  expect_true(all(vapply(pts, is.data.frame, logical(1))))
  expect_equal(names(pts[[1]]), names(dez_linhas()))
  expect_equal(vapply(pts, function(p) p$i, integer(1)), 1:10)
})

test_that("lote = 3 em 10 linhas dá 4 pontos, e o último é PARCIAL", {
  pts <- tr_to_stream(dez_linhas(), lote = 3L)
  # 3,3,3,1 — descartar o resto perderia a última linha da tabela em silêncio,
  # que é o pior modo de falha possível num nó de fatiar.
  expect_equal(vapply(pts, nrow, integer(1)), c(3L, 3L, 3L, 1L))
  expect_equal(unlist(lapply(pts, function(p) p$i)), 1:10)
})

test_that("lote maior que a tabela dá UM ponto com a tabela inteira", {
  pts <- tr_to_stream(dez_linhas(), lote = 99L)
  expect_length(pts, 1L)
  expect_equal(nrow(pts[[1]]), 10L)
})

test_that("max_passos corta a sequência nos PRIMEIROS passos, e 0 é 'todos'", {
  pts <- tr_to_stream(dez_linhas(), lote = 1L, max_passos = 2L)
  expect_length(pts, 2L)
  expect_equal(vapply(pts, function(p) p$i, integer(1)), 1:2)
  # 0 = todos: é a forma inteira do "vazio é desligado" da coleção.
  expect_length(tr_to_stream(dez_linhas(), lote = 1L, max_passos = 0L), 10L)
  # E corta em passos, não em linhas: com lote 3, dois passos são seis linhas.
  expect_equal(nrow(tr_to_stream(dez_linhas(), lote = 3L, max_passos = 2L)[[2]]), 3L)
})

test_that("tabela vazia dá ZERO pontos e não é erro", {
  # Região de zero passos é legítima: é o que um filtro sem resultado a montante
  # produz, e abortar aqui faria um fluxo saudável ficar vermelho.
  pts <- tr_to_stream(dez_linhas()[0, ], lote = 1L)
  expect_length(pts, 0L)
  expect_true(is.list(pts))
})

test_that("ordenar_por ordena ANTES de lotear", {
  # Sem isso o param não significa nada: ordenar depois de fatiar ordenaria
  # dentro de cada lote e a sequência de pontos continuaria a da tabela.
  pts <- tr_to_stream(dez_linhas(), lote = 2L, ordenar_por = "valor")
  expect_equal(pts[[1]]$valor, c(1.5, 3.0))
  expect_equal(unlist(lapply(pts, function(p) p$i)), 10:1)
})

test_that("ordenar_por com coluna inexistente para alto, com as disponíveis", {
  err <- tryCatch(tr_to_stream(dez_linhas(), ordenar_por = "valro"), condition = identity)
  expect_s3_class(err, "tr_data_error_unknown_column")
  expect_match(conditionMessage(err), "valro")
})

test_that("lote fora de 1.. e max_passos negativo param, classificados", {
  # Pelo card o mínimo do param já protege; no nível 1 não há widget, e
  # `seq.int(by = 0)` daria erro cru do R sem dizer qual campo.
  expect_error(tr_to_stream(dez_linhas(), lote = 0L), class = "tr_data_error_bad_option")
  expect_error(tr_to_stream(dez_linhas(), max_passos = -1L),
               class = "tr_data_error_bad_option")
})

test_that("a sequência de pontos é uma LISTA NUA, que é o que o driver exige", {
  # A guarda do driver é "isto é um retângulo?" (`!is.list() || !is.null(dim())`):
  # devolver a tabela inteira faria o laço andar nas COLUNAS e produzir um
  # histórico plausível e errado. É o contrato da fonte, medido do lado de cá.
  pts <- tr_to_stream(dez_linhas(), lote = 2L)
  expect_true(is.list(pts))
  expect_null(dim(pts))
})

test_that("sem tabela ligada, a fonte devolve zero pontos", {
  # A porta é opcional: um card recém-arrastado da paleta não pode explodir.
  expect_length(tr_to_stream(NULL), 0L)
})

# ---- data/from_stream: empilhar o histórico -----------------------------

pontos_de <- function(d) tr_to_stream(d, lote = 1L)

test_that("5 pontos de uma linha empilham em 5 linhas, colunas do PRIMEIRO ponto", {
  d <- dez_linhas()[1:5, ]
  h <- tr_from_stream(pontos_de(d))
  expect_equal(nrow(h), 5L)
  # `passo` primeiro, porque é o eixo x de todo gráfico que se vai querer
  # depois; o resto na ordem do primeiro ponto.
  expect_equal(names(h), c("passo", names(d)))
  expect_equal(h$passo, 1:5)
  expect_equal(h$i, 1:5)
})

test_that("passo = FALSE devolve o histórico sem a coluna do índice", {
  h <- tr_from_stream(pontos_de(dez_linhas()[1:3, ]), passo = FALSE)
  expect_equal(names(h), names(dez_linhas()))
  expect_equal(nrow(h), 3L)
})

test_that("histórico VAZIO é tabela de zero linhas, nunca NULL", {
  # Com zero pontos não há ponto de onde aprender as colunas da tabela, e este
  # nó não vê a entrada da fonte. O que ele sabe declarar é a coluna que é DELE.
  h <- tr_from_stream(list())
  expect_true(is.data.frame(h))
  expect_equal(nrow(h), 0L)
  expect_equal(names(h), "passo")
  h0 <- tr_from_stream(list(), passo = FALSE)
  expect_true(is.data.frame(h0))
  expect_equal(nrow(h0), 0L)
  expect_equal(ncol(h0), 0L)
})

test_that("ponto com coluna a mais no passo 3 é erro classificado NOMEANDO o passo", {
  pts <- pontos_de(dez_linhas()[1:4, ])
  pts[[3]]$extra <- 1
  err <- tryCatch(tr_from_stream(pts), condition = identity)
  expect_s3_class(err, "tr_data_error_stream_columns")
  # O passo no texto é o que permite reproduzir: `rbind` preencheria `NA` na
  # coluna nova dos outros três passos e o histórico sairia verde e errado.
  expect_match(conditionMessage(err), "3", fixed = TRUE)
  expect_match(conditionMessage(err), "extra", fixed = TRUE)
})

test_that("ponto VAZIO é um passo que contribui zero linhas, sem deslocar os índices", {
  # Um `data/filter` elevado devolve zero linhas a toda hora. Isso é UM ponto
  # que por acaso está vazio, não zero pontos: contar linhas em vez de passos
  # deslocaria o índice de todos os pontos seguintes.
  pts <- pontos_de(dez_linhas()[1:4, ])
  pts[[2]] <- pts[[2]][0, ]
  h <- tr_from_stream(pts)
  expect_equal(nrow(h), 3L)
  expect_equal(h$passo, c(1L, 3L, 4L))
})

test_that("fora de região, a tabela inteira vale como histórico de um ponto", {
  # `data/from_stream` ligado direto numa tabela é grafo aceito (sem fonte não
  # há região), e aí o `fn` recebe o retângulo. Percorrê-lo como lista de pontos
  # andaria nas COLUNAS — o mesmo erro que a guarda do driver evita do outro
  # lado da fronteira.
  d <- dez_linhas()
  h <- tr_from_stream(d)
  expect_equal(nrow(h), 10L)
  expect_true(all(h$passo == 1L))
})
