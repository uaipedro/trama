# Coluna inexistente e expressão quebrada tinham que chegar ao card
# classificadas — o modo de falha antigo era o pior possível: rodar, ficar
# verde e devolver resultado errado.

test_that("select/arrange/join com coluna inexistente falham alto", {
  d <- df_exemplo()
  expect_error(tr_select(d, "regaio"), class = "tr_data_error_unknown_column")
  expect_error(tr_arrange(d, "valorr"), class = "tr_data_error_unknown_column")
  expect_error(tr_group_summarise(d, "regiao_x", "n", "dplyr::n()"),
               class = "tr_data_error_unknown_column")
  expect_error(tr_join(d, d, by = "regiao_x"), class = "tr_data_error_unknown_column")
})

# `expect_error(class = ...)` percorre a cadeia de `parent`; o motor não — ele
# grava `class(e)[1]`. Validar DENTRO do verbo de dplyr deixa a classe útil só
# no pai e o card recebe "rlang_error". Estas asserções olham a classe de
# primeira ordem, que é exatamente o que chega na tela.
test_that("a classe do erro é a de primeira ordem, como o motor lê", {
  d <- df_exemplo()
  expect_equal(class(tryCatch(tr_select(d, "regaio"), error = identity))[[1]],
               "tr_data_error_unknown_column")
  expect_equal(class(tryCatch(tr_filter(d, "valro > 1"), error = identity))[[1]],
               "tr_data_error_eval")
})

test_that("expressão inválida e coluna errada em expressão chegam classificadas", {
  d <- df_exemplo()
  expect_error(tr_filter(d, "valor >"), class = "tr_data_error_bad_expr")
  expect_error(tr_filter(d, "valro > 1"), class = "tr_data_error_eval")
  expect_error(tr_mutate(d, "x", "valro * 2"), class = "tr_data_error_eval")
})

test_that("os verbos continuam certos no caminho feliz", {
  d <- df_exemplo()
  expect_equal(nrow(tr_filter(d, "valor > 8")), 5L)
  expect_equal(names(tr_select(d, "regiao, valor")), c("regiao", "valor"))
  expect_equal(tr_arrange(d, "valor", desc = TRUE)$valor[[1]], 40)
  expect_equal(nrow(tr_group_summarise(d, "regiao", "n", "dplyr::n()")), 2L)
})

# `by` é o que destrava a família de funções de janela (rank, lag, cumsum,
# participação no grupo) e o top-N por grupo. Vazio = sem grupo: os dois ramos
# de cada nó precisam continuar cobertos.
test_that("by transforma mutate em função de janela", {
  d <- df_exemplo()
  out <- tr_mutate(d, "part", "valor / sum(valor)", by = "regiao")
  sul <- out$part[out$regiao == "sul"]
  expect_equal(sum(sul), 1)
  expect_equal(nrow(out), nrow(d))          # janela não agrega

  pos <- tr_mutate(d, "pos", "rank(-valor)", by = "regiao")
  expect_equal(pos$pos[pos$regiao == "norte" & pos$valor == 40], 1)
})

test_that("sem by, os três nós seguem sem agrupar", {
  d <- df_exemplo()
  # `.by = all_of(NULL)` tem mesmo que significar "sem grupo".
  expect_equal(tr_mutate(d, "part", "valor / sum(valor)")$part,
               d$valor / sum(d$valor))
  expect_equal(nrow(tr_filter(d, "valor == max(valor)")), 1L)
  expect_equal(nrow(tr_slice_head(d, n = 1)), 1L)
})

test_that("by em filter e slice_head é relativo ao grupo", {
  d <- df_exemplo()
  expect_equal(nrow(tr_filter(d, "valor == max(valor)", by = "regiao")), 2L)
  expect_equal(nrow(tr_slice_head(d, n = 1, by = "regiao")), 2L)
})

test_that("by com coluna inexistente falha alto", {
  expect_error(tr_mutate(df_exemplo(), "x", "1", by = "regiao_x"),
               class = "tr_data_error_unknown_column")
  # E em primeira ordem, que é o que o motor lê.
  expect_equal(class(tryCatch(tr_mutate(df_exemplo(), "x", "1", by = "regiao_x"),
                              error = identity))[[1]],
               "tr_data_error_unknown_column")
  expect_equal(class(tryCatch(tr_filter(df_exemplo(), "valor > 1", by = "regiao_x"),
                              error = identity))[[1]],
               "tr_data_error_unknown_column")
  expect_equal(class(tryCatch(tr_slice_head(df_exemplo(), n = 1, by = "regiao_x"),
                              error = identity))[[1]],
               "tr_data_error_unknown_column")
})

# O resumo é a primeira coisa que se pluga depois do load. O ramo que quebra na
# prática é a coluna toda-NA: sem guard, `min(numeric(0))` devolve `Inf` com
# warning e o card mostra infinito onde devia mostrar vazio.
test_that("resumo devolve uma linha por coluna, e aguenta coluna toda NA", {
  d <- df_exemplo()
  d$vazia <- NA_real_
  d$quando <- as.Date("2026-01-01") + seq_len(nrow(d))
  out <- tr_summary(d)

  expect_equal(nrow(out), ncol(d))
  expect_equal(out$coluna, names(d))
  expect_equal(out$faltantes[out$coluna == "vazia"], nrow(d))
  expect_true(is.na(out$minimo[out$coluna == "vazia"]))
  expect_equal(out$distintos[out$coluna == "regiao"], 2L)
  expect_equal(out$minimo[out$coluna == "valor"], "5")
  expect_match(out$minimo[out$coluna == "quando"], "^2026-01-02$")
})

# Coluna-lista não é comum vindo de CSV, mas é exatamente a coluna que alguém
# pluga o resumo para entender — abortar aí seria o pior momento possível.
test_that("resumo aguenta tipos esquisitos sem abortar", {
  d <- tibble::tibble(
    lst  = list(1:3, "a", NULL, list(x = 1), NA, 9),
    fat  = factor(c("b", "a", "c", "a", "b", "a")),
    lgl  = c(TRUE, FALSE, NA, TRUE, TRUE, FALSE),
    hora = as.POSIXct("2026-01-01 10:00", tz = "UTC") + 0:5)
  out <- tr_summary(d)

  expect_equal(out$tipo, c("list", "factor", "logical", "POSIXct"))
  expect_equal(out$faltantes[out$coluna == "lgl"], 1L)
  expect_equal(out$distintos[out$coluna == "fat"], 3L)
  # Fator resume pelo rótulo, não pelo código inteiro.
  expect_equal(out$minimo[out$coluna == "fat"], "a")
  expect_equal(out$maximo[out$coluna == "fat"], "c")
  # `is.numeric(TRUE)` é FALSE em R: sem tratar lógico à parte, a coluna caía
  # no ramo do NA e o resumo lia como "falhou" justo onde tinha resposta. E o
  # `min` de lógico já volta coagido a inteiro, daí a recoerção — o resumo de
  # uma coluna lógica fala o vocabulário dela.
  expect_equal(out$minimo[out$coluna == "lgl"], "FALSE")
  expect_equal(out$maximo[out$coluna == "lgl"], "TRUE")
  # Lista não tem mínimo que faça sentido: vazio, não erro.
  expect_true(is.na(out$minimo[out$coluna == "lst"]))
  expect_false(is.na(out$exemplo[out$coluna == "lst"]))
})

test_that("convert lê número brasileiro e data com formato", {
  d <- tibble::tibble(v = c("1.234,56", "7,80"), dia = c("31/12/2026", "01/01/2027"))
  out <- tr_convert(d, "v", type = "numero", decimal = ",")
  expect_equal(out$v, c(1234.56, 7.80))

  out2 <- tr_convert(d, "dia", type = "data", format = "%d/%m/%Y")
  expect_s3_class(out2$dia, "Date")
  expect_equal(format(out2$dia[[1]]), "2026-12-31")
})

# Tabela de aceitação do número. Metade dela existe por causa de dois modos de
# falha medidos em revisão: `1234.56` já numérico virava `123456` (round-trip
# por texto, com o ponto lido como separador de milhar) e `"R$ 10"` virava 10.
# Nenhum dos dois produzia NA, então o guard de NA não via nada.
test_that("convert numero converte a grade que deve converter", {
  num <- function(x, dec = ".") {
    tr_convert(tibble::tibble(v = x), "v", type = "numero", decimal = dec)$v
  }
  expect_equal(num("1.234,56", ","), 1234.56)
  expect_equal(num("7,80", ","), 7.8)
  expect_equal(num("1234.56", "."), 1234.56)
  expect_equal(num("1,234.56", "."), 1234.56)
  expect_equal(num(1234.56, ","), 1234.56)   # já numérico: nada de round-trip
  expect_equal(num(1234.56, "."), 1234.56)
  expect_equal(num(NA_character_, ","), NA_real_)
  expect_equal(num(NA_real_, "."), NA_real_)
})

test_that("convert numero recusa texto sujo em vez de adivinhar um valor", {
  sujo <- c("10abc", "abc10", "R$ 10", "1/2", "(50)", "10%", "1.2.3", "1,2,3",
            "1.234,56")   # o último: locale errada, não pode virar 1.23456
  for (x in sujo) {
    err <- tryCatch(tr_convert(tibble::tibble(v = x), "v", type = "numero"),
                    condition = identity)
    expect_s3_class(err, "tr_data_error_bad_conversion")
    expect_match(conditionMessage(err), x, fixed = TRUE)
  }
})

# `inteiro` herda o mesmo caminho de leitura, e por isso a mesma tabela.
test_that("convert inteiro herda a mesma grade de aceitação", {
  int <- function(x, dec = ".") {
    tr_convert(tibble::tibble(v = x), "v", type = "inteiro", decimal = dec)$v
  }
  expect_equal(int("1.234,56", ","), 1235L)
  expect_equal(int("1,234.56", "."), 1235L)
  expect_equal(int(1234.56, ","), 1235L)
  expect_equal(int(NA_character_, ","), NA_integer_)
  for (x in c("10abc", "R$ 10", "(50)", "1.2.3", "1,2,3", "1.234,56")) {
    expect_error(tr_convert(tibble::tibble(v = x), "v", type = "inteiro"),
                 class = "tr_data_error_bad_conversion")
  }
})

test_that("convert e remove_empty recusam valor de param fora do enum", {
  d <- tibble::tibble(v = c("1", "2"))
  expect_error(tr_convert(d, "v", type = "numeroo"),
               class = "tr_data_error_bad_option")
  expect_error(tr_remove_empty(d, "xyz"), class = "tr_data_error_bad_option")
  expect_equal(ncol(tr_remove_empty(d, "ambos")), 1L)
})

test_that("conversão que viraria NA em silêncio é erro alto", {
  d <- tibble::tibble(v = c("10", "dez", "12"))
  err <- tryCatch(tr_convert(d, "v", type = "numero"), condition = identity)
  expect_s3_class(err, "tr_data_error_bad_conversion")
  expect_match(conditionMessage(err), "dez")
})

# Data sem formato cai no ISO do readr: "31/12/2026" viraria NA calado, que é
# justo o que este nó existe para não deixar acontecer.
test_that("data sem formato não some em silêncio", {
  d <- tibble::tibble(dia = c("31/12/2026"))
  expect_error(tr_convert(d, "dia", type = "data"),
               class = "tr_data_error_bad_conversion")
})

test_that("NA de entrada é NA legítimo e não dispara o guard", {
  d <- tibble::tibble(v = c("10", NA, "12"), t = c("a", NA, "c"),
                      f = factor(c("a", NA, "c")))
  out <- tr_convert(d, "v", type = "inteiro")
  expect_equal(out$v, c(10L, NA, 12L))
  expect_type(out$v, "integer")

  # texto e fator nunca criam NA novo — nem sobre fator com NA.
  expect_equal(tr_convert(d, "t", type = "fator")$t, factor(c("a", NA, "c")))
  expect_equal(tr_convert(d, "f", type = "texto")$f, c("a", NA, "c"))
})

test_that("convert cobre os seis tipos e recusa coluna inexistente", {
  d <- tibble::tibble(n = c("3,5", "0,5"), i = c("3", "4"), x = c(1, 2),
                      l = c("TRUE", "false"), dt = c("2026-01-02", "2026-03-04"))
  expect_equal(tr_convert(d, "n", type = "numero", decimal = ",")$n, c(3.5, 0.5))
  expect_equal(tr_convert(d, "i", type = "inteiro")$i, c(3L, 4L))
  expect_equal(tr_convert(d, "x", type = "texto")$x, c("1", "2"))
  expect_equal(tr_convert(d, "l", type = "logico")$l, c(TRUE, FALSE))
  expect_s3_class(tr_convert(d, "dt", type = "data")$dt, "Date")
  expect_s3_class(tr_convert(d, "x", type = "fator")$x, "factor")
  expect_equal(tr_convert(d, "", type = "numero"), d)
  expect_error(tr_convert(d, "nao_existe", type = "numero"),
               class = "tr_data_error_unknown_column")
})

test_that("limpa nomes, remove vazias e mostra duplicadas", {
  d <- tibble::tibble(`Região Norte` = c("a", "a", "b"), `  Valor R$ ` = c(1, 1, 2),
                      so_na = NA_character_)
  lim <- tr_clean_names(d)
  expect_equal(names(lim), c("regiao_norte", "valor_r", "so_na"))

  expect_equal(ncol(tr_remove_empty(lim, "colunas")), 2L)
  expect_equal(ncol(tr_remove_empty(lim, "ambos")), 2L)
  expect_equal(ncol(tr_remove_empty(lim, "linhas")), 3L)

  dup <- tr_get_dupes(lim, "regiao_norte")
  expect_equal(nrow(dup), 2L)
  # Sem colunas o janitor usa todas — o ramo do `suppressMessages()`.
  expect_equal(nrow(tr_get_dupes(lim, "")), 2L)
  expect_true("dupe_count" %in% names(dup))
})

test_that("tipo de junção inválido falha alto, e lógico sai como FALSE/TRUE", {
  d <- df_exemplo()
  err <- tryCatch(tr_join(d, d, by = "regiao", type = "xxx"), error = identity)
  expect_equal(class(err)[[1]], "tr_data_error_bad_option")
  expect_match(conditionMessage(err), "inner, left, right, full, anti")

  lgl <- tr_summary(tibble::tibble(ok = c(TRUE, FALSE, TRUE)))
  expect_equal(lgl$minimo, "FALSE")
  expect_equal(lgl$maximo, "TRUE")
})

test_that("drop_na e replace_na", {
  d <- tibble::tibble(a = c(1, NA, 3), b = c("x", "y", NA))
  # Sem colunas, "faltante em qualquer uma" — que é o que a description promete.
  expect_equal(nrow(tr_drop_na(d, "")), 1L)
  expect_equal(nrow(tr_drop_na(d, "a")), 2L)
  expect_equal(tr_replace_na(d, "a", "0")$a, c(1, 0, 3))
  expect_equal(tr_replace_na(d, "b", "sem")$b, c("x", "y", "sem"))
  expect_error(tr_replace_na(d, "a", "zero"), class = "tr_data_error_bad_conversion")
})

test_that("drop_na e replace_na recusam coluna inexistente, em primeira ordem", {
  d <- tibble::tibble(a = c(1, NA))
  expect_equal(class(tryCatch(tr_drop_na(d, "regaio"), error = identity))[[1]],
               "tr_data_error_unknown_column")
  expect_equal(class(tryCatch(tr_replace_na(d, "regaio", "0"), error = identity))[[1]],
               "tr_data_error_unknown_column")
  expect_equal(class(tryCatch(tr_replace_na(d, "a", "zero"), error = identity))[[1]],
               "tr_data_error_bad_conversion")
})

test_that("replace_na preserva o tipo da coluna em vez de coagir em silêncio", {
  # Cada uma destas linhas era, com atribuição crua, um tipo corrompido calado:
  # lógica virava texto, fator continuava NA (só com aviso) e inteiro virava
  # double. Tipo trocado quebra o verbo numérico lá na frente, longe daqui.
  lgl <- tr_replace_na(tibble::tibble(v = c(TRUE, NA)), "v", "FALSE")$v
  expect_type(lgl, "logical")
  expect_equal(lgl, c(TRUE, FALSE))

  int <- tr_replace_na(tibble::tibble(v = c(1L, NA)), "v", "0")$v
  expect_type(int, "integer")
  expect_equal(int, c(1L, 0L))

  fat <- tr_replace_na(tibble::tibble(v = factor(c("x", NA))), "v", "sem")$v
  expect_s3_class(fat, "factor")
  expect_equal(as.character(fat), c("x", "sem"))
  expect_true("sem" %in% levels(fat))

  dat <- tr_replace_na(tibble::tibble(v = as.Date(c("2020-01-01", NA))), "v", "2026-01-01")$v
  expect_s3_class(dat, "Date")
  expect_equal(dat, as.Date(c("2020-01-01", "2026-01-01")))
})

test_that("replace_na aborta quando o texto não vira o tipo da coluna", {
  cls <- function(d, cl, valor) class(tryCatch(tr_replace_na(d, cl, valor),
                                               error = identity))[[1]]
  expect_equal(cls(tibble::tibble(v = c(TRUE, NA)), "v", "talvez"),
               "tr_data_error_bad_conversion")
  expect_equal(cls(tibble::tibble(v = c(1L, NA)), "v", "0.5"),
               "tr_data_error_bad_conversion")
  expect_equal(cls(tibble::tibble(v = as.Date(c("2020-01-01", NA))), "v", "abacaxi"),
               "tr_data_error_bad_conversion")
})

# `value` em branco é NO-OP porque NÃO preencher é comportamento honesto para
# um campo de preenchimento vazio — campo limpo no card não é ordem de trocar
# NA por string vazia, e em coluna numérica isso abortava. É o lado "desligado"
# da doutrina do param vazio (o outro lado é o `name` do `tr_mutate`, que não
# tem comportamento honesto nenhum e aborta); a doutrina inteira, com a lista
# das exceções, está travada em `test-param-vazio.R`.
test_that("replace_na com valor em branco não mexe na tabela", {
  num <- tibble::tibble(v = c(1, NA))
  expect_identical(tr_replace_na(num, "v", ""), num)
  txt <- tibble::tibble(v = c("x", NA))
  expect_identical(tr_replace_na(txt, "v", "   "), txt)
})

# O fall-through do `.tr_data_na_valor()` assumia TEXTO: qualquer classe fora
# dos ramos conhecidos recebia o `value` cru e a coluna trocava de tipo em
# silêncio (complexa virava texto). Fora do alcance das fontes desta coleção,
# mas assumir é justamente o modo de falha que a coleção não quer.
test_that("replace_na aborta em coluna de tipo que não sabe preencher", {
  cpx <- tibble::tibble(v = c(1 + 2i, NA))
  err <- tryCatch(tr_replace_na(cpx, "v", "3"), error = identity)
  expect_equal(class(err)[[1]], "tr_data_error_bad_conversion")
  expect_match(conditionMessage(err), "complex")

  # Texto e fator continuam passando pelo caminho de sempre.
  expect_equal(tr_replace_na(tibble::tibble(v = c("x", NA)), "v", "sem")$v, c("x", "sem"))
})

test_that("replace_na e drop_na aguentam tabela sem linhas e sem NA", {
  vazia <- tibble::tibble(a = numeric(0), b = character(0))
  expect_equal(nrow(tr_drop_na(vazia, "")), 0L)
  expect_equal(nrow(tr_replace_na(vazia, "a", "0")), 0L)

  cheia <- tibble::tibble(a = c(1, 2))
  expect_equal(tr_replace_na(cheia, "a", "0")$a, c(1, 2))
  expect_equal(nrow(tr_drop_na(cheia, "")), 2L)

  # Param vazio no replace é no-op: campo em branco não é ordem de mexer.
  d <- tibble::tibble(a = c(1, NA))
  expect_equal(tr_replace_na(d, "", "0"), d)
})

# `distinct` REMOVE as repetidas; `get_dupes` (categoria "conhecer") MOSTRA
# quais são. Ver antes de apagar — daí os dois nós, em fases diferentes.
test_that("distinct, rename e select removendo", {
  d <- df_exemplo()
  expect_equal(nrow(tr_distinct(d, "regiao")), 2L)
  expect_equal(nrow(tr_distinct(d, "")), nrow(d))

  ren <- tr_rename(d, "regiao, valor", "uf, preco")
  expect_equal(names(ren)[c(1, 3)], c("uf", "preco"))
  expect_error(tr_rename(d, "regiao, valor", "uf"), class = "tr_data_error_mismatched_names")
  expect_error(tr_rename(d, "regaio", "uf"), class = "tr_data_error_unknown_column")

  expect_equal(names(tr_select(d, "regiao", remove = TRUE)), c("produto", "valor", "qtd"))
})

# O `setNames(from, to)` do `tr_rename()` é fácil de inverter, e a asserção por
# POSIÇÃO acima passaria de qualquer jeito se cada nome fosse só trocado de
# lugar. Esta olha o CONTEÚDO: se o mapa cruzasse, `uf` traria os valores de
# `valor` e `preco` os de `regiao`.
test_that("rename leva o conteúdo junto com o nome", {
  d <- df_exemplo()
  ren <- tr_rename(d, "regiao, valor", "uf, preco")
  expect_equal(ren$uf, d$regiao)
  expect_equal(ren$preco, d$valor)
  expect_equal(tr_rename(d, "", "x"), d)
})

# A classe do erro tem que estar em primeira ordem: é `class(e)[1]` que o motor
# grava no card.
test_that("distinct, rename e select removendo erram em primeira ordem", {
  d <- df_exemplo()
  expect_equal(class(tryCatch(tr_distinct(d, "regaio"), error = identity))[[1]],
               "tr_data_error_unknown_column")
  expect_equal(class(tryCatch(tr_rename(d, "regaio", "uf"), error = identity))[[1]],
               "tr_data_error_unknown_column")
  expect_equal(class(tryCatch(tr_rename(d, "regiao, valor", "uf"), error = identity))[[1]],
               "tr_data_error_mismatched_names")
  expect_equal(class(tryCatch(tr_select(d, "regaio", remove = TRUE), error = identity))[[1]],
               "tr_data_error_unknown_column")
})

# Renomear para um nome que a tabela já tem é validação de PARAM, e param
# inválido é o que esta coleção classifica em todo lugar. Sem a checagem, o
# vctrs abortava com `vctrs_error_names_must_be_unique`, em inglês.
test_that("rename recusa destino que já existe, com classe da coleção", {
  d <- df_exemplo()
  err <- tryCatch(tr_rename(d, "regiao", "produto"), error = identity)
  expect_equal(class(err)[[1]], "tr_data_error_name_collision")
  expect_match(conditionMessage(err), "produto")

  # Dois destinos iguais colidem entre si, mesmo sem bater em coluna existente.
  expect_equal(class(tryCatch(tr_rename(d, "regiao, valor", "x, x"),
                              error = identity))[[1]],
               "tr_data_error_name_collision")

  # Renomear PARA o nome de uma coluna que a própria chamada está renomeando
  # não colide: a troca é simultânea.
  troca <- tr_rename(d, "regiao, produto", "produto, regiao")
  expect_equal(troca$produto, d$regiao)
  expect_equal(troca$regiao, d$produto)

  # Renomear uma coluna para o nome que ela já tem segue sendo inofensivo.
  expect_equal(names(tr_rename(d, "regiao", "regiao")), names(d))
})

# `name` do resumo tem default NÃO vazio no spec, como `names_to`/`values_to`
# do empilhar: em branco é card incompleto, não "desligado". Sem a checagem, o
# vazio descia até o `quos()` e voltava culpando o param 'expr'.
test_that("group_summarise com nome em branco culpa o campo certo", {
  d <- df_exemplo()
  err <- tryCatch(tr_group_summarise(d, "regiao", "", "sum(valor)"), error = identity)
  expect_equal(class(err)[[1]], "tr_data_error_blank_param")
  expect_match(conditionMessage(err), "name")
})

# `expr` do resumo também tem default não vazio (`dplyr::n()`), e resumir sem
# conta não é comportamento nenhum. Em branco, o parser do rlang abortava com
# "`x` must contain exactly 1 expression, not 0" — o único erro em INGLÊS por
# caminho próprio da coleção, e culpando a sintaxe em vez do campo vazio.
test_that("group_summarise com expressão em branco culpa o campo, em português", {
  d <- df_exemplo()
  err <- tryCatch(tr_group_summarise(d, "regiao", "n", "  "), error = identity)
  expect_equal(class(err)[[1]], "tr_data_error_blank_param")
  expect_match(conditionMessage(err), "expr")
  expect_match(conditionMessage(err), "em branco")
})

# O irmão do caso acima: `tr_mutate()` devolvia a tabela INTACTA e verde com o
# nome em branco, enquanto `tr_group_summarise()` abortava no mesmo caso. Não
# existe criar coluna sem nome — a doutrina inteira está em
# `test-param-vazio.R`, aqui fica o caso observável.
test_that("mutate com nome em branco aborta, mas sem expressão continua desligado", {
  d <- df_exemplo()
  err <- tryCatch(tr_mutate(d, "", "valor * 2"), error = identity)
  expect_equal(class(err)[[1]], "tr_data_error_blank_param")
  expect_match(conditionMessage(err), "name")
  expect_equal(class(tryCatch(tr_mutate(d, "  ", "valor * 2"), error = identity))[[1]],
               "tr_data_error_blank_param")
  # Card recém-arrastado: sem expressão, o nó está desligado e não reclama de
  # nada — inclusive do nome, que ele ainda nem vai usar.
  expect_identical(tr_mutate(d, "nova", ""), d)
  expect_identical(tr_mutate(d, "", ""), d)
})

# `.as_cols()` aparava só no ramo do TEXTO. O ramo do vetor é o caminho do
# console — o nível 1 —, e devolvia os nomes crus: o erro saía com o culpado
# invisível, "coluna(s) inexistente(s):  valor ".
test_that(".as_cols apara e descarta o vazio nos dois ramos", {
  expect_equal(.as_cols(" valor , qtd ,, "), c("valor", "qtd"))
  expect_equal(.as_cols(c(" valor ", "qtd")), c("valor", "qtd"))
  expect_equal(.as_cols(c("valor", "", "  ")), "valor")
  expect_equal(.as_cols(NULL), character())
  expect_equal(.as_cols(character()), character())

  d <- df_exemplo()
  expect_equal(names(tr_select(d, c(" valor ", "qtd"))), c("valor", "qtd"))
  expect_equal(names(tr_select(d, " valor , qtd ")), c("valor", "qtd"))
})

# `names_from`/`values_from` são os únicos campos de coluna declarados `text`
# (são UMA coluna, não lista), e por isso não passavam pelo `.as_cols()`: um
# espaço à direita errava com o nome invisível na mensagem.
test_that("pivot_wider apara o espaço dos dois campos de origem", {
  d <- tibble::tibble(cliente = c("a", "b"), mes = c("jan", "jan"), valor = c(1, 2))
  largo <- tr_pivot_wider(d, " mes ", " valor ")
  expect_equal(names(largo), c("cliente", "jan"))
  err <- tryCatch(tr_pivot_wider(d, "  ", "valor"), error = identity)
  expect_equal(class(err)[[1]], "tr_data_error_blank_param")
})

# ---- Tabela de aceitação por tipo de junção ----------------------------
# Os cinco tipos do `switch` de `tr_join()` eram INTERCAMBIÁVEIS: trocar
# `left` por `right` (ou `full` por `anti`) no corpo do nó deixava a suíte
# inteira verde. Um nó que entrega `right_join` quando o card pediu `left` é
# resultado errado em silêncio — o modo de falha que esta coleção existe para
# não ter, e nenhum teste o via.
#
# As fixtures têm uma chave SÓ À ESQUERDA, uma SÓ À DIREITA e uma nos DOIS
# lados, que é o mínimo para os cinco tipos se separarem. Cada linha da tabela
# afirma três coisas — quais chaves saem, quantas linhas e onde há faltante em
# cada lado — e as cinco linhas são mutuamente distintas: nenhum par de tipos
# satisfaz a mesma asserção, então trocar dois deles no `switch` quebra aqui.
df_esq <- function() tibble::tibble(k = c("so_esq", "ambos"), x = c(1, 2))
df_dir <- function() tibble::tibble(k = c("ambos", "so_dir"), y = c(10, 20))

test_that("cada tipo de junção devolve a sua tabela, e nenhuma outra", {
  j <- function(tipo) tr_join(df_esq(), df_dir(), by = "k", type = tipo)

  # inner: só a chave dos dois lados. Nada falta em lado nenhum.
  inner <- j("inner")
  expect_equal(sort(inner$k), "ambos")
  expect_equal(nrow(inner), 1L)
  expect_equal(sum(is.na(inner$x)), 0L)
  expect_equal(sum(is.na(inner$y)), 0L)

  # left: TODAS as da esquerda. A direita é que fica faltante.
  esq <- j("left")
  expect_equal(sort(esq$k), c("ambos", "so_esq"))
  expect_equal(nrow(esq), 2L)
  expect_equal(sum(is.na(esq$x)), 0L)
  expect_equal(sum(is.na(esq$y)), 1L)
  expect_true(is.na(esq$y[esq$k == "so_esq"]))

  # right: o espelho exato do de cima — é o par que a mutação trocava.
  dir <- j("right")
  expect_equal(sort(dir$k), c("ambos", "so_dir"))
  expect_equal(nrow(dir), 2L)
  expect_equal(sum(is.na(dir$x)), 1L)
  expect_equal(sum(is.na(dir$y)), 0L)
  expect_true(is.na(dir$x[dir$k == "so_dir"]))

  # full: as três chaves, e faltante dos dois lados.
  full <- j("full")
  expect_equal(sort(full$k), c("ambos", "so_dir", "so_esq"))
  expect_equal(nrow(full), 3L)
  expect_equal(sum(is.na(full$x)), 1L)
  expect_equal(sum(is.na(full$y)), 1L)

  # anti: só a da esquerda SEM par, e apenas com as colunas da esquerda — a
  # coluna da direita nem existe no resultado, que é o que o separa do inner
  # (mesmo `nrow`, chave diferente, colunas diferentes).
  anti <- j("anti")
  expect_equal(sort(anti$k), "so_esq")
  expect_equal(nrow(anti), 1L)
  expect_equal(names(anti), c("k", "x"))
  expect_false("y" %in% names(anti))
  expect_equal(sum(is.na(anti$x)), 0L)
})

# A prova de que a tabela acima MORDE: os cinco resultados são distintos entre
# si por construção. Se dois tipos coincidissem em chaves, linhas e colunas, o
# `switch` poderia trocá-los sem ninguém ver — que era exatamente o estado
# anterior. Esta asserção falha se um tipo novo (ou uma mutação) fizer dois
# ramos convergirem.
test_that("os cinco tipos de junção não coincidem dois a dois", {
  assinatura <- function(tipo) {
    out <- tr_join(df_esq(), df_dir(), by = "k", type = tipo)
    paste(paste(sort(out$k), collapse = "|"), nrow(out),
          paste(names(out), collapse = "|"),
          sum(is.na(out$x)),
          if ("y" %in% names(out)) sum(is.na(out$y)) else "-")
  }
  tipos <- c("inner", "left", "right", "full", "anti")
  assinaturas <- vapply(tipos, assinatura, "")
  expect_equal(length(unique(assinaturas)), length(tipos))
})

# ---- O que os verbos DEVOLVEM, e não só quantas linhas -----------------
# Contar linhas não separa `slice_head` de `slice_tail`, nem `distinct` que
# guarda a primeira de `distinct` que guarda a última: as mutações abaixo
# passavam pela suíte inteira. Um nó que devolve a ÚLTIMA repetida quando o
# usuário ordenou para ficar com a primeira é resultado errado em silêncio, e
# nenhuma contagem o denuncia.

test_that("distinct guarda a PRIMEIRA de cada grupo, com as outras colunas", {
  # A ordem importa: quem ordena antes é quem escolhe qual repetida sobrevive,
  # e é essa a promessa da página.
  d <- tibble::tibble(k = c("a", "a", "b", "b"), v = c(1, 2, 3, 4),
                      obs = c("primeira", "segunda", "terceira", "quarta"))
  out <- tr_distinct(d, "k")

  expect_equal(nrow(out), 2L)
  # `.keep_all = TRUE`: sem ele, o resultado teria SÓ a coluna `k` e as demais
  # sumiriam — o nó viraria um "listar valores distintos", que é outro nó.
  expect_equal(names(out), names(d))
  # E a linha que fica é a PRIMEIRA, não a última.
  expect_equal(out$v, c(1, 3))
  expect_equal(out$obs, c("primeira", "terceira"))

  # Sem colunas, compara a linha inteira — e aí nada se repete.
  expect_equal(nrow(tr_distinct(d, "")), 4L)
})

test_that("slice_head pega o COMEÇO da tabela, e não o fim", {
  d <- tibble::tibble(v = 1:5, rotulo = letters[1:5])
  out <- tr_slice_head(d, n = 2)
  expect_equal(out$v, 1:2)
  expect_equal(out$rotulo, c("a", "b"))

  # Por grupo, a primeira de CADA grupo — e na ordem em que a tabela chegou,
  # que é o que a página promete (agrupar escolhe quais linhas ficam, não a
  # ordem em que saem).
  g <- tibble::tibble(g = c("z", "a", "z", "a"), v = 1:4)
  topo <- tr_slice_head(g, n = 1, by = "g")
  expect_equal(topo$g, c("z", "a"))
  expect_equal(topo$v, c(1L, 2L))
})

# `cols` do `get_dupes` era ignorável: a fixture antiga tinha repetição por
# coluna e repetição de linha inteira coincidindo, então usar todas as colunas
# dava o mesmo resultado. Aqui elas divergem de propósito.
test_that("get_dupes olha as colunas escolhidas, não a linha inteira", {
  d <- tibble::tibble(k = c("a", "a", "b"), v = c(1, 2, 3))

  por_k <- tr_get_dupes(d, "k")
  expect_equal(nrow(por_k), 2L)
  expect_equal(por_k$k, c("a", "a"))
  expect_equal(por_k$dupe_count, c(2L, 2L))

  # Linha inteira: nenhuma se repete, e a tabela sai vazia — que já é a
  # resposta. Se o nó ignorasse `cols`, os dois casos dariam o mesmo.
  expect_equal(nrow(tr_get_dupes(d, "")), 0L)
})

# O resumo devolvia qualquer número sem ninguém ver: a suíte só contava linhas
# e grupos. Um `sum()` que devolve outra coisa é o pior desfecho de um nó de
# agregação — o painel inteiro fica plausível e errado.
test_that("group_summarise devolve o VALOR da expressão, por grupo", {
  d <- df_exemplo()
  soma <- tr_group_summarise(d, "regiao", "receita", "sum(valor)")
  soma <- soma[order(soma$regiao), ]
  expect_equal(soma$regiao, c("norte", "sul"))
  expect_equal(soma$receita, c(30 + 40 + 15, 10 + 20 + 5))
  expect_equal(names(soma), c("regiao", "receita"))

  # Contagem e média pelo mesmo caminho, para o valor não depender do verbo.
  n <- tr_group_summarise(d, "regiao", "linhas", "dplyr::n()")
  expect_equal(sort(n$linhas), c(3L, 3L))
  media <- tr_group_summarise(d, "regiao", "m", "mean(valor)")
  expect_equal(sort(media$m), sort(c((30 + 40 + 15) / 3, (10 + 20 + 5) / 3)))

  # Sem agrupamento, a tabela inteira numa linha só — o total geral.
  total <- tr_group_summarise(d, "", "total", "sum(valor)")
  expect_equal(nrow(total), 1L)
  expect_equal(total$total, sum(d$valor))
})

# ---- O que as páginas de ajuda prometem ---------------------------------
# Documentação que mente é pior que documentação ausente: o usuário age sobre
# ela. Estas três afirmações estavam ERRADAS na ajuda e não tinham teste
# nenhum — foi por isso que ninguém viu. Corrigido o texto, ficam travadas
# aqui, para não voltarem a divergir do `fn`.

test_that("no modo remover, a ordem digitada NÃO é a ordem da saída", {
  d <- tibble::tibble(produto = "a", qtd = 1, regiao = "sul", valor = 10)
  # Manter: a ordem é a que foi digitada — o nó também reordena colunas.
  expect_equal(names(tr_select(d, "valor, regiao")), c("valor", "regiao"))
  # Remover: as que ficam são as NÃO nomeadas, e elas seguem na ordem
  # original da tabela. A lista digitada não tem como ordenar o que não cita.
  expect_equal(names(tr_select(d, "qtd, regiao", remove = TRUE)),
               c("produto", "valor"))
})

test_that("filter com empate no máximo devolve todas as empatadas", {
  # A ajuda dizia "devolve uma linha". `==` é comparação, não "pegue uma".
  empate <- tibble::tibble(valor = c(10, 10, 5))
  expect_equal(nrow(tr_filter(empate, "valor == max(valor)")), 2L)

  # E por grupo, todas as campeãs de cada grupo, empates inclusive.
  g <- tibble::tibble(g = c("a", "a", "b"), valor = c(7, 7, 1))
  expect_equal(nrow(tr_filter(g, "valor == max(valor)", by = "g")), 3L)
})

test_that("pivot_wider aceita identidade repetida: é ela que gera as colunas", {
  # A ajuda dizia que a identidade repetida PARA o nó. Ela é o caso normal —
  # o mesmo cliente aparece uma vez por mês, e cada uma vira uma coluna. O que
  # o nó recusa é identidade repetida COM o mesmo `names_from`.
  d <- tibble::tibble(cliente = c("a", "a", "b", "b"),
                      mes = c("jan", "fev", "jan", "fev"), v = 1:4)
  expect_equal(sum(duplicated(d$cliente)), 2L)   # a identidade se repete...
  largo <- tr_pivot_wider(d, "mes", "v")         # ...e mesmo assim passa
  expect_equal(names(largo), c("cliente", "jan", "fev"))
  expect_equal(nrow(largo), 2L)

  # Com o mesmo cliente no mesmo mês duas vezes, aí sim para.
  colide <- tibble::tibble(cliente = c("a", "a"), mes = c("jan", "jan"),
                           v = 1:2)
  expect_error(tr_pivot_wider(colide, "mes", "v"),
               class = "tr_data_error_duplicate_key")
})

# ---- Vários resumos no mesmo nó ------------------------------------------
# A vírgula separa resumos, e os nomes casam pela POSIÇÃO — mesmo contrato de
# `tr_rename()`. O risco que estes testes guardam não é o caminho feliz: é o
# desalinho calado, em que o nome de um resumo fica em cima da conta de outro.

test_that("group_summarise calcula vários resumos, casados pela posição", {
  d <- df_exemplo()
  out <- tr_group_summarise(d, "regiao", "receita, media, linhas",
                            "sum(valor), mean(valor), dplyr::n()")
  out <- out[order(out$regiao), ]
  expect_equal(names(out), c("regiao", "receita", "media", "linhas"))
  expect_equal(out$receita, c(30 + 40 + 15, 10 + 20 + 5))
  expect_equal(out$media, c((30 + 40 + 15) / 3, (10 + 20 + 5) / 3))
  expect_equal(out$linhas, c(3L, 3L))

  # Sem agrupar, a tabela inteira numa linha só — com as duas métricas.
  total <- tr_group_summarise(d, "", "soma, n", "sum(valor), dplyr::n()")
  expect_equal(nrow(total), 1L)
  expect_equal(total$soma, sum(d$valor))
})

# O motivo de `expr` não passar por `.as_cols()`: partir na vírgula crua
# quebraria esta expressão no meio e o `na.rm` viraria um "resumo" sozinho.
test_that("vírgula dentro de chamada não separa resumo", {
  d <- df_exemplo()
  d$valor[1] <- NA
  out <- tr_group_summarise(d, "", "soma", "sum(valor, na.rm = TRUE)")
  expect_equal(names(out), "soma")
  expect_equal(out$soma, sum(d$valor, na.rm = TRUE))

  # E a mesma expressão ao lado de outra continua sendo UM resumo.
  dois <- tr_group_summarise(d, "", "soma, n", "sum(valor, na.rm = TRUE), dplyr::n()")
  expect_equal(names(dois), c("soma", "n"))
})

test_that("listas de tamanhos diferentes param o nó, dizendo os dois números", {
  d <- df_exemplo()
  err <- tryCatch(tr_group_summarise(d, "regiao", "receita, media", "sum(valor)"),
                  error = identity)
  expect_equal(class(err)[[1]], "tr_data_error_mismatched_names")
  expect_match(conditionMessage(err), "2 nome\\(s\\) para 1 resumo\\(s\\)")

  expect_error(tr_group_summarise(d, "regiao", "receita", "sum(valor), mean(valor)"),
               class = "tr_data_error_mismatched_names")
})

# Nome repetido sai com o número de colunas certo e a conta errada: o segundo
# resumo sobrescreve o primeiro sem dizer nada.
test_that("nome de resumo repetido, ou igual à coluna de agrupamento, aborta", {
  d <- df_exemplo()
  expect_error(tr_group_summarise(d, "regiao", "x, x", "sum(valor), mean(valor)"),
               class = "tr_data_error_name_collision")
  expect_error(tr_group_summarise(d, "regiao", "regiao", "sum(valor)"),
               class = "tr_data_error_name_collision")
})

# Vírgula sobrando é acidente de digitação, e `.as_cols()` já a ignora no campo
# de colunas. Divergir entre os dois campos seria ter dois contratos para a
# mesma tecla. Só de vírgula, porém, é campo em branco com disfarce.
test_that("vírgula sobrando é ignorada; campo só de vírgula é branco", {
  d <- df_exemplo()
  out <- tr_group_summarise(d, "", "soma, ", "sum(valor), ")
  expect_equal(names(out), "soma")

  err <- tryCatch(tr_group_summarise(d, "", " , ", "sum(valor)"), error = identity)
  expect_equal(class(err)[[1]], "tr_data_error_blank_param")
  expect_match(conditionMessage(err), "name")
  expect_equal(class(tryCatch(tr_group_summarise(d, "", "soma", " , "),
                              error = identity))[[1]], "tr_data_error_blank_param")
})

# O erro de sintaxe ecoa o que o usuário digitou. O embrulho `list(...)` é
# mecanismo interno, e vê-lo na mensagem manda procurar parêntese alheio.
test_that("resumo com sintaxe quebrada culpa o campo e ecoa o texto do usuário", {
  d <- df_exemplo()
  err <- tryCatch(tr_group_summarise(d, "", "soma", "sum(valor"), error = identity)
  expect_equal(class(err)[[1]], "tr_data_error_bad_expr")
  expect_match(conditionMessage(err), "sum(valor", fixed = TRUE)
  expect_false(grepl("list(", conditionMessage(err), fixed = TRUE))
})
