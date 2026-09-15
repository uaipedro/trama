# O guard mora no `store` porque ele é o funil único por onde todo valor entra
# no artefato — cobre qualquer nó que declare `view/plot`, hoje e amanhã, sem
# que cada `fn` precise lembrar. Mesma lição que o `data/table` aprendeu depois
# de deixar um `lm` virar tabela de zero colunas com card verde.

# `tr_points()` só chega na Task 3.1; o tipo não depende de nó nenhum, então o
# teste monta o gráfico com as peças que já existem.
plot_exemplo <- function(aspecto = "16:9") {
  .tr_view_acabar(
    ggplot2::ggplot(df_exemplo(), ggplot2::aes(x = .data[["valor"]], y = .data[["qtd"]])) +
      ggplot2::geom_point(),
    aspecto = aspecto, tema = "escuro", titulo = "", rotulo_x = "", rotulo_y = "",
    legenda = "direita")
}

test_that("store do view/plot recusa objeto que não é ggplot, classificado", {
  st <- view_plot_type()$store
  p <- tempfile(fileext = ".rds")
  for (x in list(1:10, df_exemplo(), stats::lm(mpg ~ cyl, datasets::mtcars))) {
    expect_error(st(x, p), class = "tr_view_error_not_a_plot")
    e <- tryCatch(st(x, p), error = identity)
    expect_equal(class(e)[[1]], "tr_view_error_not_a_plot")
    expect_match(conditionMessage(e), class(x)[[1]], fixed = TRUE)
  }
  expect_false(file.exists(p))
})

test_that("store/restore devolve um ggplot desenhável, com a dimensão junto", {
  ty <- view_plot_type()
  p <- plot_exemplo("4:3")
  f <- tempfile(fileext = ".rds")
  ty$store(p, f)
  de_volta <- ty$restore(f)
  expect_s3_class(de_volta, "ggplot")
  expect_equal(attr(de_volta, "tr_view_dim"), c(8, 6))
  # A prova de que o objeto restaurado não depende de um ambiente que não
  # existe mais: desenhar num device descartável. O device é o `ragg`, e não o
  # `grDevices::png`, pelo mesmo motivo que `R/type.R` dá pra usá-lo no
  # `preview`: o segundo depende de X11/quartz e falharia no servidor headless
  # onde a suíte roda — por um motivo que nada tem a ver com o que se testa aqui.
  ragg::agg_png(tempfile(fileext = ".png")); on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(de_volta))
})

test_that("preview grava um PNG na proporção pedida e aponta pro trama/image", {
  skip_if_not_installed("png")
  ty <- view_plot_type()
  p <- plot_exemplo("16:9")
  dir <- tempfile(); dir.create(dir)
  ctx <- list(file = function(e) file.path(dir, paste0("pv.", e)))
  art <- ty$preview(p, ctx)
  expect_equal(art$renderer, "trama/image")
  expect_true(file.exists(art$files$png))
  dims <- dim(png::readPNG(art$files$png))
  expect_equal(round(dims[[2]] / dims[[1]], 2), round(16 / 9, 2))
  expect_equal(dims[[2]], 1600)
})

test_that("preview em retrato mantém o lado MAIOR em 1600, não a largura", {
  # Largura e altura trocadas passam despercebidas em tudo que é quase
  # quadrado: só o retrato prova que o lado maior é o que manda.
  skip_if_not_installed("png")
  ty <- view_plot_type()
  dir <- tempfile(); dir.create(dir)
  ctx <- list(file = function(e) file.path(dir, paste0("pv.", e)))
  art <- ty$preview(plot_exemplo("3:4"), ctx)
  dims <- dim(png::readPNG(art$files$png))
  expect_equal(dims[[1]], 1600)             # altura
  expect_equal(dims[[2]], 1200)             # largura
})

test_that("summary alimenta a vista 'resumo' embutida do núcleo", {
  s <- view_plot_type()$summary(plot_exemplo())
  expect_true(all(c("proporcao", "camadas", "linhas") %in% names(s)))
  expect_equal(s$camadas, 1L)
  expect_equal(s$linhas, nrow(df_exemplo()))
})

# Conferir só os NOMES do summary deixava passar o valor errado, e deixou: com
# `%.0f`, o 16:9 (8 x 4,5 pol) saía "8 x 4 pol", porque o arredondamento é para
# par. O resumo mentia sobre a forma da imagem desenhada ao lado dele no card.
test_that("a proporção do resumo não perde a altura fracionária", {
  resumo <- function(a) view_plot_type()$summary(plot_exemplo(a))$proporcao
  expect_equal(resumo("16:9"), "8 x 4.5 pol")
  expect_equal(resumo("4:3"),  "8 x 6 pol")
  expect_equal(resumo("3:4"),  "6 x 8 pol")
})
