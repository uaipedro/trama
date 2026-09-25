# Camadas (view/reference, view/fit_line, view/annotate). O desenho se confere
# no PNG; aqui ficam as CONTAS (a equação e o IC têm de ser os do `lm`) e as
# recusas, que são o que um olho não pega.

disperso_mtcars <- function(...) {
  d <- mtcars; d$am <- factor(d$am, labels = c("auto", "manual"))
  tr_points(d, "wt", "mpg", ...)
}

camada_de <- function(p, geom) {
  b <- ggplot2::ggplot_build(p)
  i <- which(vapply(p$layers, function(l) inherits(l$geom, geom), logical(1)))
  b$data[i]
}

test_that("valores: vírgula decimal, ';' entre valores, e recusa do que não é número", {
  expect_equal(.tr_view_numeros("10; 2,5 ;30", "valor"), c(10, 2.5, 30))
  expect_equal(.tr_view_numeros(c(1, 2), "valor"), c(1, 2))
  expect_length(.tr_view_numeros("", "ate", vazio = TRUE), 0L)
  expect_error(.tr_view_numeros("", "valor"), class = "tr_view_error_blank_param")
  expect_error(.tr_view_numeros("10; dez", "valor"), "dez", class = "tr_view_error_not_numeric")
  expect_error(.tr_view_um_numero("1; 2", "x"), class = "tr_view_error_bad_option")
})

test_that("a camada preserva tema, proporção e rótulos do gráfico de entrada", {
  p <- disperso_mtcars(tema = "claro", aspecto = "4:3", titulo = "t")
  for (q in list(tr_reference(p, valor = "20"), tr_fit_line(p), tr_annotate(p, "3", "30", "a"))) {
    expect_equal(attr(q, "tr_view_dim"), attr(p, "tr_view_dim"))
    expect_identical(q$theme, p$theme)
    expect_equal(q$labels$title, "t")
    expect_gt(length(q$layers), length(p$layers))
    expect_no_error(ggplot2::ggplot_build(q))
  }
})

test_that("painel na entrada é recusado, com o nome do nó", {
  p <- disperso_mtcars()
  pw <- tr_combine(list(p, p))
  expect_error(tr_reference(pw), "view/reference", class = "tr_view_error_panel")
  expect_error(tr_fit_line(pw), class = "tr_view_error_panel")
  expect_error(tr_annotate(pw, "1", "1", "a"), class = "tr_view_error_panel")
  expect_error(tr_reference(mtcars), class = "tr_view_error_not_a_plot")
})

test_that("referência: várias linhas, faixa de um valor só, texto por valor", {
  p <- disperso_mtcars(painel = "am")
  h <- camada_de(tr_reference(p, valor = "15; 25"), "GeomHline")[[1]]
  # Duas linhas em cada um dos dois painéis: a referência vale para todos.
  expect_equal(sort(unique(h$yintercept)), c(15, 25))
  expect_equal(nrow(h), 4L)
  expect_error(tr_reference(p, valor = "15; 25", ate = "30"), class = "tr_view_error_bad_option")
  expect_error(tr_reference(p, "diagonal", ate = "3"), class = "tr_view_error_bad_option")
  expect_error(tr_reference(p, valor = "1; 2; 3", texto = "a; b"), class = "tr_view_error_bad_option")
  expect_error(tr_reference(tr_points(mtcars, "wt", "mpg", log = "Y"), "diagonal"),
               class = "tr_view_error_bad_option")
})

test_that("reta ajustada: a equação impressa é a do lm, e a faixa é o IC da média", {
  q <- tr_fit_line(disperso_mtcars(), "linear", confianca = 0.9)
  fit <- stats::lm(mpg ~ wt, mtcars)
  txt <- camada_de(q, "GeomText")[[1]]$label
  b <- stats::coef(fit)
  esperado <- sprintf("ŷ = %s - %s·x   R² = %s", .tr_view_fmt(b[[1]]), .tr_view_fmt(abs(b[[2]])),
                      .tr_view_fmt(summary(fit)$r.squared, 3L))
  expect_equal(txt, esperado)
  expect_match(txt, "37,29 - 5,344", fixed = TRUE)
  fita <- camada_de(q, "GeomRibbon")[[1]]
  ic <- stats::predict(fit, data.frame(wt = fita$x), interval = "confidence", level = 0.9)
  expect_equal(fita$ymin, unname(ic[, "lwr"]), tolerance = 1e-8)
  expect_equal(fita$ymax, unname(ic[, "upr"]), tolerance = 1e-8)
})

test_that("quadrática: coeficientes crus (I(x^2)), não os do polinômio ortogonal", {
  q <- tr_fit_line(disperso_mtcars(), "quadrática")
  fit <- stats::lm(mpg ~ wt + I(wt^2), mtcars)
  expect_equal(unname(attr(q, "tr_view_ajustes")$tudo$coeficientes), unname(stats::coef(fit)))
  expect_match(camada_de(q, "GeomText")[[1]]$label, "·x²", fixed = TRUE)
})

test_that("um ajuste por painel e por cor, e cada equação no seu painel", {
  d <- iris
  q <- tr_fit_line(tr_points(d, "Sepal.Length", "Petal.Length", cor = "Species"))
  expect_length(attr(q, "tr_view_ajustes"), 3L)
  so <- stats::lm(Petal.Length ~ Sepal.Length, d[d$Species == "setosa", ])
  expect_equal(unname(attr(q, "tr_view_ajustes")$setosa$coeficientes), unname(stats::coef(so)))
  # Sem 'Uma por cor', um ajuste só.
  expect_length(attr(tr_fit_line(tr_points(d, "Sepal.Length", "Petal.Length", cor = "Species"),
                                 por_cor = FALSE), "tr_view_ajustes"), 1L)
  q <- tr_fit_line(disperso_mtcars(painel = "am"))
  txt <- camada_de(q, "GeomText")[[1]]
  expect_equal(sort(unique(txt$PANEL)), factor(1:2))
  loe <- tr_fit_line(disperso_mtcars(), "loess")
  expect_length(camada_de(loe, "GeomText"), 0L)
})

test_that("reta ajustada recusa gráfico sem X e Y numéricos, e confiança fora de (0, 1)", {
  expect_error(tr_fit_line(tr_boxplot(iris, "Species", "Sepal.Length")), class = "tr_view_error_not_numeric")
  expect_error(tr_fit_line(tr_histogram(mtcars, "mpg")), class = "tr_view_error_not_numeric")
  expect_error(tr_fit_line(disperso_mtcars(), confianca = 95), class = "tr_view_error_bad_option")
  # A Linha também serve: é X e Y numéricos de colunas.
  expect_no_error(tr_fit_line(tr_line(mtcars, "wt", "mpg")))
})

test_that("com log no Y, o ajuste é na escala desenhada", {
  q <- tr_fit_line(disperso_mtcars(log = "Y"))
  fit <- stats::lm(log10(mpg) ~ wt, mtcars)
  expect_equal(unname(attr(q, "tr_view_ajustes")$tudo$coeficientes), unname(stats::coef(fit)))
  expect_match(camada_de(q, "GeomText")[[1]]$label, "^log\\(ŷ\\)")
})

test_that("anotação: números validados, seta só com os dois pontos", {
  p <- disperso_mtcars()
  expect_error(tr_annotate(p, "a", "1", "t"), class = "tr_view_error_not_numeric")
  expect_error(tr_annotate(p, "1", "1", ""), class = "tr_view_error_blank_param")
  expect_error(tr_annotate(p, "1", "1", "t", seta_x = "2"), class = "tr_view_error_bad_option")
  q <- tr_annotate(p, "3,5", "30", "t", seta_x = "5", seta_y = "10")
  expect_true(inherits(q$layers[[length(q$layers) - 1L]]$geom, "GeomSegment"))
  expect_equal(camada_de(q, "GeomText")[[1]]$x, 3.5)
})

test_that("pelo motor: disperso -> reta -> referência -> painel", {
  reg <- view_registry(); s <- trama::tr_store(tempfile())
  f <- trama::tr_flow(reg) |>
    trama::tr_add("d", "data/example", dataset = "mtcars") |>
    trama::tr_add("g", "view/points", x = "wt", y = "mpg", from = "d") |>
    trama::tr_add("r", "view/fit_line", from = "g") |>
    trama::tr_add("h", "view/reference", valor = "20", from = "r") |>
    trama::tr_add("p", "view/combine", from = c("h", "g"))
  p <- trama::tr_value(f$doc, "p", registry = reg, store = s)
  expect_s3_class(p, "patchwork")
})

test_that("o texto das camadas segue o Texto (pt) do view/save", {
  q <- .tr_view_texto(tr_fit_line(disperso_mtcars()), 9)
  expect_equal(unique(camada_de(q, "GeomText")[[1]]$size), 9 * .8 / ggplot2::.pt)
})
