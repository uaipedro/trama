# O segundo lote: eixo em log, rótulos nas barras, e os seis nós de
# ranking, pareamento e observações cruas.

df_pos <- function() {
  data.frame(
    g = rep(c("a", "b", "c"), each = 8),
    s = rep(c("m", "f"), 12),
    v = c(1:8, 11:18, 101:108),
    w = seq(.5, 12, length.out = 24),
    stringsAsFactors = FALSE
  )
}

test_that("eixo em log põe a escala, e recusa zero, negativo e texto", {
  d <- df_pos()
  p <- tr_points(d, "v", "w", log = "ambos")
  trans <- vapply(p$scales$scales, function(s) s$get_transformation()$name, "")
  expect_setequal(trans, c("log-10", "log-10"))
  expect_no_error(ggplot2::ggplot_build(tr_histogram(d, "v", log = TRUE)))
  expect_no_error(ggplot2::ggplot_build(tr_strip(d, "g", "v", log = TRUE)))
  d0 <- d; d0$v[[1]] <- 0
  err <- tryCatch(tr_boxplot(d0, "g", "v", log = TRUE), error = identity)
  expect_s3_class(err, "tr_view_error_not_positive")
  expect_match(conditionMessage(err), "1 valor")
  expect_error(tr_line(d, "g", "v", log = "X"), class = "tr_view_error_not_numeric")
  expect_error(tr_points(d, "v", "w", log = "x"), class = "tr_view_error_bad_option")
})

test_that("o tema pinta as marcas sem cor, e não o preto do ggplot", {
  escuro <- trama::tr_theme("escuro")
  b <- ggplot2::ggplot_build(tr_points(df_pos(), "v", "w", tema = escuro))
  expect_equal(unique(b$data[[1]]$colour), escuro$texto)
})

test_that("rótulos nas barras escrevem o número da barra, em cada posição", {
  d <- df_pos()
  pilha <- tr_bars(d, "g", "", "s", rotulos = TRUE)
  expect_length(pilha$layers, 2L)
  expect_setequal(pilha$data$rotulo, "4")
  soma <- tr_bars(d, "g", "v", rotulos = TRUE)
  expect_equal(soma$data$rotulo[soma$data$g == "c"], "836")
  prop <- tr_bars(d, "g", "", "s", posicao = "proporção", rotulos = TRUE)
  expect_setequal(prop$data$rotulo, "50%")
  expect_equal(.tr_view_numero(c(1.234, 5)), c("1,23", "5,00"))
  expect_equal(.tr_view_numero(c(12.34, 50)), c("12,3", "50,0"))
  expect_equal(.tr_view_numero(c(20, 3)), c("20", "3"))
  expect_no_error(ggplot2::ggplot_build(tr_bars(d, "g", "v", "s", posicao = "lado a lado",
                                                rotulos = TRUE, deitar = TRUE)))
})

test_that("disperso com rótulos exige o rótulo e escreve uma camada de texto", {
  d <- df_pos()
  expect_error(tr_labels(d, "v", "w", ""), class = "tr_view_error_blank_param")
  p <- tr_labels(d, "v", "w", "g", evitar = FALSE)
  expect_s3_class(p$layers[[2]]$geom, "GeomText")
  expect_false(p$layers[[2]]$geom_params$check_overlap)
})

test_that("faixa de pontos: colmeia não sobrepõe, é determinística e só mexe no X", {
  d <- data.frame(g = "a", v = rep(5, 6))
  p <- tr_strip(d, "g", "v", resumo = "nenhum")
  expect_equal(length(unique(p$data$posicao_x)), 6L)
  expect_equal(p$data$posicao_x, tr_strip(d, "g", "v", resumo = "nenhum")$data$posicao_x)
  b <- ggplot2::ggplot_build(p)$data[[1]]
  expect_true(all(b$y == 5))
  expect_true(all(abs(b$x - 1) <= .45))
  med <- tr_strip(df_pos(), "g", "v", resumo = "mediana")
  seg <- ggplot2::ggplot_build(med)$data[[2]]
  expect_equal(sort(seg$y), c(4.5, 14.5, 104.5))
  expect_error(tr_strip(df_pos(), "g", "v", estilo = "jitter"), class = "tr_view_error_bad_option")
})

test_that("pontos ordenados: maior em cima, soma por categoria", {
  p <- tr_dotplot(df_pos(), "g", "v")
  expect_equal(levels(p$data$g), c("a", "b", "c"))
  expect_equal(p$data$v[p$data$g == "c"], sum(101:108))
  pir <- tr_dotplot(df_pos(), "g", "", estilo = "pirulito")
  expect_s3_class(pir$layers[[1]]$geom, "GeomSegment")
  expect_equal(unique(pir$data$contagem), 8L)
})

test_that("halteres: condição única para, e a ordem é pela diferença", {
  d <- data.frame(cat = rep(c("x", "y", "z"), 2), ano = rep(c("2023", "2024"), each = 3),
                  r = c(10, 10, 10, 11, 30, 5))
  p <- tr_dumbbell(d, "cat", "ano", "r")
  # Diferenças: x +1, y +20, z -5 — o último nível (em cima) é o maior.
  expect_equal(levels(p$data$cat), c("z", "x", "y"))
  expect_no_error(ggplot2::ggplot_build(p))
  expect_error(tr_dumbbell(d[d$ano == "2023", ], "cat", "ano", "r"), class = "tr_view_error_levels")
  expect_error(tr_dumbbell(d, "cat", "", "r"), class = "tr_view_error_blank_param")
})

test_that("pareamento recusa par repetido, em vez de tirar média", {
  d <- data.frame(u = rep(1:4, 2), fase = rep(c("antes", "depois"), each = 4), a = 1:8)
  p <- tr_paired(d, "fase", "a", "u")
  expect_length(p$layers, 4L)
  expect_no_error(ggplot2::ggplot_build(tr_paired(d, "fase", "a", "u", cor = "fase")))
  rep <- rbind(d, d[1, ])
  err <- tryCatch(tr_paired(rep, "fase", "a", "u"), error = identity)
  expect_s3_class(err, "tr_view_error_not_unique")
  expect_match(conditionMessage(err), "1 par")
  expect_error(tr_paired(d[d$fase == "antes", ], "fase", "a", "u"), class = "tr_view_error_levels")
})

test_that("Pareto: ordem decrescente, acumulado até 100% e referência", {
  d <- data.frame(tipo = c(rep("a", 5), rep("b", 3), rep("c", 2)))
  p <- tr_pareto(d, "tipo")
  expect_equal(levels(p$data$tipo), c("a", "b", "c"))
  expect_equal(p$data$acumulado_pct, c(.5, .8, 1))
  expect_s3_class(p$layers[[4]]$geom, "GeomHline")
  expect_length(tr_pareto(d, "tipo", referencia = 0)$layers, 3L)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_error(tr_pareto(data.frame(t = c("a", "b"), v = c(1, -1)), "t", "v"),
               class = "tr_view_error_not_positive")
  expect_error(tr_pareto(d, "tipo", referencia = 120), class = "tr_view_error_bad_option")
})
