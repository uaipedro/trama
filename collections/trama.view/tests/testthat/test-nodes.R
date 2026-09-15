# Os cinco primeiros nós pelo NÍVEL 1: chamada de função R comum, sem registro e sem
# motor. Todo teste daqui passa por `ggplot_build()` porque CONSTRUIR um ggplot
# quase nunca falha — quem falha é desenhar. Um teste que só confere
# `expect_s3_class(p, "ggplot")` passa alegremente num gráfico que não imprime.

# A cor desligada se confere no DESENHO, não no `mapping`. Hoje as duas coisas
# concordam (`.tr_view_mapa()` deixa a estética de fora do objeto, então
# `names(p$mapping)` de fato não traz "colour") — mas o que a coleção promete
# ao usuário é o gráfico sem cor, e é isso que esta função pergunta. Conferir
# pelo mapeamento seria conferir o meio em vez do fim: passaria por qualquer
# reescrita futura que montasse a estética de outro jeito e desenhasse errado.
tem_escala_cor <- function(p, aes_nome = "colour") {
  b <- ggplot2::ggplot_build(p)
  aes_nome %in% unlist(lapply(b$plot$scales$scales, function(s) s$aesthetics))
}

test_that("cada nó devolve um ggplot que DESENHA, com a dimensão pendurada", {
  d <- df_exemplo()
  ps <- list(
    tr_points(d, x = "valor", y = "qtd"),
    tr_line(d, x = "valor", y = "qtd"),
    tr_bars(d, x = "regiao", y = "valor"),
    tr_histogram(d, x = "valor"),
    tr_boxplot(d, x = "regiao", y = "valor")
  )
  for (p in ps) {
    expect_s3_class(p, "ggplot")
    expect_length(attr(p, "tr_view_dim"), 2L)
    expect_gte(length(p$layers), 1L)
    expect_no_error(ggplot2::ggplot_build(p))
  }
})

test_that("eixo em branco ABORTA; cor em branco DESLIGA", {
  d <- df_exemplo()
  expect_error(tr_points(d, x = "", y = "qtd"), class = "tr_view_error_blank_param")
  expect_error(tr_points(d, x = "valor", y = ""), class = "tr_view_error_blank_param")
  expect_error(tr_histogram(d, x = ""), class = "tr_view_error_blank_param")
  expect_error(tr_boxplot(d, x = "regiao", y = ""), class = "tr_view_error_blank_param")
  sem_cor <- tr_points(d, x = "valor", y = "qtd", cor = "")
  com_cor <- tr_points(d, x = "valor", y = "qtd", cor = "regiao")
  expect_false(tem_escala_cor(sem_cor))
  expect_true(tem_escala_cor(com_cor))
  expect_length(unique(ggplot2::ggplot_build(sem_cor)$data[[1]]$colour), 1L)
  expect_gt(length(unique(ggplot2::ggplot_build(com_cor)$data[[1]]$colour)), 1L)
})

test_that("cor desligada vale para os nós que pintam por 'fill' também", {
  d <- df_exemplo()
  expect_false(tem_escala_cor(tr_bars(d, x = "regiao", y = "valor"), "fill"))
  expect_true(tem_escala_cor(tr_bars(d, x = "regiao", y = "valor", cor = "produto"), "fill"))
  expect_false(tem_escala_cor(tr_histogram(d, x = "valor"), "fill"))
  expect_true(tem_escala_cor(tr_histogram(d, x = "valor", cor = "regiao"), "fill"))
  expect_false(tem_escala_cor(tr_boxplot(d, x = "regiao", y = "valor"), "fill"))
})

test_that("coluna inexistente é recusada, não ignorada", {
  d <- df_exemplo()
  expect_error(tr_points(d, x = "valro", y = "qtd"), class = "tr_view_error_unknown_column")
  expect_error(tr_points(d, x = "valor", y = "qtd", cor = "regaio"),
               class = "tr_view_error_unknown_column")
  expect_error(tr_bars(d, x = "regiao", y = "valro"), class = "tr_view_error_unknown_column")
})

test_that("barras sem Y conta as linhas, em vez de falhar", {
  p <- tr_bars(df_exemplo(), x = "regiao", y = "")
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  expect_equal(sum(b$data[[1]]$y), 6)
  expect_equal(p$labels$y, "contagem")
})

test_that("boxplot sem X vira uma caixa só, da amostra inteira", {
  p <- tr_boxplot(df_exemplo(), x = "", y = "valor")
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[1]]), 1L)
})

test_that("os cosméticos passam pelo funil e o enum ainda protege", {
  d <- df_exemplo()
  p <- tr_points(d, x = "valor", y = "qtd", aspecto = "1:1", titulo = "T",
                 rotulo_x = "X", rotulo_y = "Y", legenda = "nenhuma")
  expect_equal(attr(p, "tr_view_dim"), c(8, 8))
  expect_equal(p$labels$title, "T")
  expect_equal(p$labels$x, "X")
  expect_equal(p$labels$y, "Y")
  expect_error(tr_points(d, x = "valor", y = "qtd", tema = "escurro"),
               class = "tr_view_error_bad_option")
  # Pelo card o `fn` recebe a definição resolvida, não o nome.
  q <- tr_points(d, x = "valor", y = "qtd", tema = trama::tr_theme("claro"))
  expect_equal(q$theme$plot.background$fill, "#ffffff")
  expect_error(tr_points(d, x = "valor", y = "qtd", aspecto = "9:16"),
               class = "tr_view_error_bad_option")
})
