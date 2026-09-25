# Figura: painel (view/combine) e gravação (view/save).
#
# O painel se testa pelo gtable que o patchwork monta, e não pelo objeto: a
# etiqueta só existe depois do layout, e é ela que o leitor do artigo vê.

dois_graficos <- function() {
  d <- df_exemplo()
  a <- tr_points(d, "qtd", "valor", cor = "regiao", titulo = "um", tema = "escuro")
  b <- tr_bars(d, "regiao", titulo = "dois", tema = "claro")
  list(a = a, b = b)
}

# Rótulo do grob de nome `nome` no layout do patchwork (`tag-1`, `title-2`).
rotulo_grob <- function(g, nome) {
  achar <- function(z) {
    if (!is.null(z$label)) return(z$label)
    for (k in c(z$children, z$grobs)) { r <- achar(k); if (length(r)) return(r) }
    character()
  }
  i <- which(g$layout$name == nome)
  if (!length(i)) return(character())
  achar(g$grobs[[i[[1]]]])
}

test_that("as etiquetas seguem a ordem das entradas", {
  g <- dois_graficos()
  pw <- patchwork::patchworkGrob(tr_combine(list(g$a, g$b)))
  expect_equal(c(rotulo_grob(pw, "title-1"), rotulo_grob(pw, "tag-1")), c("um", "A"))
  expect_equal(c(rotulo_grob(pw, "title-2"), rotulo_grob(pw, "tag-2")), c("dois", "B"))
  # Invertida a entrada, inverte o painel: a ordem é a da lista, e só dela.
  pw <- patchwork::patchworkGrob(tr_combine(list(g$b, g$a), etiquetas = "1, 2, 3"))
  expect_equal(c(rotulo_grob(pw, "title-1"), rotulo_grob(pw, "tag-1")), c("dois", "1"))
  expect_length(rotulo_grob(patchwork::patchworkGrob(tr_combine(list(g$a, g$b), etiquetas = "nenhuma")),
                            "tag-1"), 0L)
})

test_that("pelo motor, a ordem dos painéis é a ordem em que as arestas foram ligadas", {
  reg <- view_registry(); s <- trama::tr_store(tempfile())
  f <- trama::tr_flow(reg) |>
    trama::tr_add("d", "data/example", dataset = "mtcars") |>
    trama::tr_add("b", "view/histogram", x = "hp", titulo = "segundo", from = "d") |>
    trama::tr_add("a", "view/points", x = "wt", y = "mpg", titulo = "primeiro", from = "d") |>
    # `a` ligado ANTES de `b`, embora criado depois: vale a ordem da ligação.
    trama::tr_add("p", "view/combine", from = c("a", "b"))
  p <- trama::tr_value(f$doc, "p", registry = reg, store = s)
  pw <- patchwork::patchworkGrob(p)
  expect_equal(rotulo_grob(pw, "title-1"), "primeiro")
  expect_equal(rotulo_grob(pw, "tag-2"), "B")
})

test_that("o tema do painel vale para todos os painéis, e para o fundo entre eles", {
  g <- dois_graficos()
  p <- tr_combine(list(g$a, g$b), tema = "claro")
  fundo <- trama::tr_theme("claro")$fundo
  for (i in seq_along(p)) {
    expect_equal(ggplot2::calc_element("plot.background", ggplot2::complete_theme(p[[i]]$theme))$fill,
                 fundo, info = i)
  }
  expect_equal(p$patches$annotation$theme$plot.background$fill, fundo)
})

test_that("legenda comum coleta, e colunas são respeitadas", {
  g <- dois_graficos()
  p <- tr_combine(list(g$a, g$a), colunas = 1, legenda_comum = TRUE)
  expect_equal(p$patches$layout$guides, "collect")
  expect_equal(p$patches$layout$ncol, 1L)
  # Coletadas, as duas legendas iguais viram UMA caixa no nível do painel, no
  # lugar das caixas por painel (`guide-box-right-1`, `-2`...).
  nomes <- patchwork::patchworkGrob(p)$layout$name
  expect_equal(grep("^guide-box", nomes, value = TRUE), "guide-box")
  # 0 é automático: o patchwork decide, e o layout não fixa colunas.
  expect_null(tr_combine(list(g$a, g$b), colunas = 0)$patches$layout$ncol)
})

test_that("título vira título do painel, e o preview renderiza na proporção", {
  g <- dois_graficos()
  p <- tr_combine(list(g$a, g$b), titulo = "Figura 1", aspecto = "2:1")
  expect_equal(p$patches$annotation$title, "Figura 1")
  expect_true(inherits(p, "ggplot"))
  dir <- tempfile(); dir.create(dir)
  ctx <- list(file = function(e) file.path(dir, paste0("pv.", e)))
  art <- view_plot_type()$preview(p, ctx)
  expect_equal(dim(png::readPNG(art$files$png))[1:2], c(800L, 1600L))
})

test_that("painel de painel funciona, e entrada que não é gráfico é recusada", {
  g <- dois_graficos()
  interno <- tr_combine(list(g$a, g$b))
  expect_s3_class(tr_combine(list(interno, g$a), colunas = 1), "patchwork")
  expect_error(tr_combine(list(g$a, df_exemplo())), class = "tr_view_error_not_a_plot")
  expect_error(tr_combine(list(g$a), etiquetas = "I, II"), class = "tr_view_error_bad_option")
})
