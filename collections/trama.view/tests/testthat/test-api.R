# A costura exportada é CONTRATO com outras coleções: se ela divergir do que
# os nós da própria `view` usam, o gráfico de série passa a ter outra cara
# que o gráfico de dispersão, e ninguém nota até pôr os dois lado a lado.

test_that("tr_view_props é o mesmo funil dos nós da view", {
  ps <- tr_view_props(x = trama::tr_param("cols", "", label = "X", example = "a"))
  expect_equal(names(ps),
               c("x", "aspecto", "tema", "titulo", "rotulo_x", "rotulo_y", "legenda"))
})

test_that("tr_view_finish pendura a dimensão que o preview lê", {
  p <- tr_view_finish(ggplot2::ggplot(df_exemplo()), aspecto = "1:1")
  expect_equal(attr(p, "tr_view_dim"), c(8, 8))
  expect_error(tr_view_finish(ggplot2::ggplot(df_exemplo()), aspecto = "5:1"),
               class = "tr_view_error_bad_option")
})

test_that("tr_view_render grava o PNG que o tipo view/plot grava", {
  skip_if_not_installed("png")
  dir <- tempfile(); dir.create(dir)
  ctx <- list(file = function(e) file.path(dir, paste0("pv.", e)))
  p <- tr_view_finish(ggplot2::ggplot(df_exemplo(), ggplot2::aes(valor, qtd)) +
                        ggplot2::geom_point(), aspecto = "2:1")
  art <- tr_view_render(p, ctx)
  expect_equal(art$renderer, "trama/image")
  dims <- dim(png::readPNG(art$files$png))
  expect_equal(c(dims[[2]], dims[[1]]), c(1600, 800))
})

test_that("a seção de aparência exportada é a que os nós anexam", {
  expect_identical(tr_view_help_appearance(), .TR_VIEW_AJUDA_APARENCIA)
})
