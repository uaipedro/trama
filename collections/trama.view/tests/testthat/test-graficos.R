# Os oito nós que vieram depois dos cinco, e os params acrescentados aos cinco.
# Mesma doutrina de `test-nodes.R`: tudo passa por `ggplot_build()`, porque
# construir um ggplot quase nunca falha — quem falha é desenhar.

df_grupos <- function() {
  set.seed(42)
  data.frame(
    grupo = rep(c("a", "b", "c"), each = 20),
    sub   = rep(c("m", "f"), 30),
    t     = rep(1:10, 6),
    v     = c(rnorm(20, 10), rnorm(20, 14, 2), rexp(20, .2)),
    w     = rnorm(60),
    stringsAsFactors = FALSE
  )
}

desenha <- function(p) {
  expect_s3_class(p, "ggplot")
  expect_length(attr(p, "tr_view_dim"), 2L)
  expect_no_error(b <- ggplot2::ggplot_build(p))
  invisible(ggplot2::ggplot_build(p))
}

test_that("cada nó novo desenha, e sobrevive ao rds", {
  d <- df_grupos()
  ps <- list(
    tr_area(d, "t", "v", "grupo"), tr_bin2d(d, "v", "w"),
    tr_heatmap(d, "grupo", "sub", "v", rotulos = TRUE),
    tr_density(d, "v", "grupo"), tr_violin(d, "grupo", "v", "sub", pontos = TRUE),
    tr_violin(d, "", "v"), tr_ecdf(d, "v", "grupo"), tr_qq(d, "v", "grupo"),
    tr_means(d, "grupo", "v", "sub")
  )
  for (p in ps) {
    desenha(p)
    f <- tempfile(fileext = ".rds")
    saveRDS(p, f)
    desenha(readRDS(f))
  }
})

test_that("os defaults dos params novos não mudam o desenho dos cinco", {
  # Um fluxo gravado antes dos params novos tem de renderizar igual: uma camada
  # só, sem painel, sem coord girada.
  d <- df_grupos()
  ps <- list(tr_points(d, "v", "w"), tr_line(d, "t", "v"), tr_bars(d, "grupo", "v"),
             tr_histogram(d, "v"), tr_boxplot(d, "grupo", "v"))
  for (p in ps) {
    expect_length(p$layers, 1L)
    expect_s3_class(p$facet, "FacetNull")
    expect_false(inherits(p$coordinates, "CoordFlip"))
  }
})

test_that("painel divide em facetas, e coluna errada é recusada", {
  d <- df_grupos()
  p <- tr_points(d, "v", "w", painel = "grupo")
  expect_s3_class(p$facet, "FacetWrap")
  expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 3L)
  expect_error(tr_histogram(d, "v", painel = "gurpo"), class = "tr_view_error_unknown_column")
  # O painel das médias precisa entrar nas chaves da agregação, senão a
  # coluna não existiria na tabela desenhada.
  expect_equal(nrow(ggplot2::ggplot_build(tr_means(d, "grupo", "v", painel = "sub"))$layout$layout), 2L)
})

test_that("tendência acrescenta a linha, e só com valor aceito", {
  d <- df_grupos()
  p <- tr_points(d, "v", "w", tendencia = "linear")
  expect_length(p$layers, 2L)
  expect_s3_class(p$layers[[2]]$geom, "GeomSmooth")
  desenha(tr_points(d, "v", "w", cor = "grupo", tendencia = "suave"))
  expect_error(tr_points(d, "v", "w", tendencia = "reta"), class = "tr_view_error_bad_option")
})

test_that("barras: proporção soma 1, lado a lado SOMA, ordenar e deitar", {
  d <- df_grupos()
  b <- ggplot2::ggplot_build(tr_bars(d, "grupo", "", "sub", posicao = "proporção"))$data[[1]]
  expect_equal(as.vector(tapply(b$ymax, b$x, max)), c(1, 1, 1))

  lado <- tr_bars(d, "grupo", "v", "sub", posicao = "lado a lado")
  somas <- stats::aggregate(v ~ grupo + sub, d, sum)
  expect_equal(nrow(lado$data), nrow(somas))
  expect_equal(sort(ggplot2::ggplot_build(lado)$data[[1]]$y), sort(somas$v))

  ord <- tr_bars(d, "grupo", "v", ordenar = TRUE)
  totais <- tapply(d$v, d$grupo, sum)
  expect_equal(levels(ord$data$grupo), names(sort(totais, decreasing = TRUE)))
  # Deitado, a maior vai para cima: o último nível é o de cima do eixo.
  deit <- tr_bars(d, "grupo", "v", ordenar = TRUE, deitar = TRUE)
  expect_equal(utils::tail(levels(deit$data$grupo), 1L), names(which.max(totais)))
  expect_s3_class(deit$coordinates, "CoordFlip")
  expect_error(tr_bars(d, "grupo", posicao = "empilhado"), class = "tr_view_error_bad_option")
})

test_that("histograma sobreposto não empilha; boxplot com observações não duplica extremos", {
  d <- df_grupos()
  h <- tr_histogram(d, "v", "grupo", posicao = "sobrepor")
  expect_s3_class(h$layers[[1]]$position, "PositionIdentity")
  bx <- tr_boxplot(d, "grupo", "v", pontos = TRUE)
  expect_length(bx$layers, 2L)
  expect_true(is.na(bx$layers[[1]]$geom_params$outlier_gp$shape))
  desenha(tr_boxplot(d, "grupo", "v", "sub", pontos = TRUE))
})

test_that("mapa de calor conta sem valor e soma com valor", {
  d <- df_grupos()
  conta <- tr_heatmap(d, "grupo", "sub")$data
  expect_equal(sum(conta$contagem), nrow(d))
  expect_equal(nrow(conta), 6L)
  soma <- tr_heatmap(d, "grupo", "sub", "v")$data
  ref <- stats::aggregate(v ~ grupo + sub, d, sum)
  m <- merge(soma, ref, by = c("grupo", "sub"))
  expect_equal(m$v.x, m$v.y)
  expect_length(tr_heatmap(d, "grupo", "sub", rotulos = TRUE)$layers, 2L)
  expect_error(tr_heatmap(d, "grupo", "sub", "sub"), class = "tr_view_error_not_numeric")
})

test_that("médias: o IC é o do t.test, e grupo de uma linha sai sem barra", {
  d <- df_grupos()
  r <- tr_means(d, "grupo", "v")$data
  ic <- stats::t.test(d$v[d$grupo == "b"])$conf.int
  expect_equal(r$inferior[r$grupo == "b"], ic[[1]])
  expect_equal(r$superior[r$grupo == "b"], ic[[2]])
  ep <- tr_means(d, "grupo", "v", barra = "erro padrão")$data
  vb <- d$v[d$grupo == "b"]
  expect_equal(ep$superior[ep$grupo == "b"] - ep$media[ep$grupo == "b"], sd(vb) / sqrt(20))

  um <- rbind(d, data.frame(grupo = "z", sub = "m", t = 1L, v = 3, w = 0))
  rz <- tr_means(um, "grupo", "v")$data
  expect_true(is.na(rz$inferior[rz$grupo == "z"]))
  desenha(tr_means(um, "grupo", "v"))

  expect_error(tr_means(d, "grupo", "sub"), class = "tr_view_error_not_numeric")
  expect_error(tr_means(d, "", "v"), class = "tr_view_error_blank_param")
  expect_error(tr_means(d, "grupo", "v", barra = "IC 99%"), class = "tr_view_error_bad_option")
})

test_that("a agregação preserva o tipo e a ordem dos níveis da chave", {
  d <- df_grupos()
  d$grupo <- factor(d$grupo, levels = c("c", "a", "b"))
  r <- tr_means(d, "grupo", "v")$data
  expect_s3_class(r$grupo, "factor")
  expect_equal(levels(r$grupo), c("c", "a", "b"))
})

test_that("densidade recusa suavidade que não é positiva; violino sem grupo tem cor", {
  d <- df_grupos()
  expect_error(tr_density(d, "v", suavidade = 0), class = "tr_view_error_bad_option")
  expect_error(tr_density(d, "", suavidade = 1), class = "tr_view_error_blank_param")
  # Sem grupo de cor, o preenchimento é a primeira cor da paleta, e não o
  # papel do tema — que no claro deixaria o violino invisível.
  claro <- trama::tr_theme("claro")
  b <- ggplot2::ggplot_build(tr_violin(d, "grupo", "v", tema = claro))
  expect_equal(unique(b$data[[1]]$fill), claro$paleta[[1]])
})

test_that("pelo motor, os dezenove nós rodam com os params do card", {
  # O nível 1 não prova que a declaração bate com o `fn`: um param declarado
  # com nome que o `fn` não aceita só falha aqui, quando o plano o repassa.
  reg <- view_registry(); s <- trama::tr_store(tempfile())
  f <- trama::tr_flow(reg) |>
    trama::tr_add("d", "data/generate",
                  expr = "data.frame(g = rep(c('a', 'b'), 15), t = rep(1:15, 2), v = rnorm(30), w = rnorm(30))")
  obrig <- list(
    points = list(x = "v", y = "w"), line = list(x = "t", y = "v"), area = list(x = "t", y = "w"),
    bin2d = list(x = "v", y = "w"), heatmap = list(x = "g", y = "t"),
    histogram = list(x = "v"), density = list(x = "v"), boxplot = list(y = "v"),
    violin = list(y = "v"), ecdf = list(x = "v"), qq = list(y = "v"),
    bars = list(x = "g"), means = list(x = "g", y = "v"),
    labels = list(x = "v", y = "w", rotulo = "g"), strip = list(y = "v"),
    dotplot = list(x = "g"), dumbbell = list(x = "t", cor = "g", y = "w"),
    paired = list(x = "g", y = "v", unidade = "t"), pareto = list(x = "g"))
  for (nm in names(obrig)) {
    f <- do.call(trama::tr_add, c(list(f, nm, paste0("view/", nm)), obrig[[nm]], list(from = "d")))
  }
  ev <- list()
  trama::tr_run(f$doc, registry = reg, store = s, on_event = function(e) ev[[length(ev) + 1]] <<- e)
  feitos <- vapply(Filter(function(e) identical(e$type, "done"), ev), `[[`, "", "node")
  expect_setequal(feitos, c("d", names(obrig)))
})
