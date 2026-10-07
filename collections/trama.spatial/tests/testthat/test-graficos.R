# Os dois gráficos e os dois cards.
#
# Um ggplot é construído de forma preguiçosa: `expect_s3_class(g, "ggplot")`
# passa mesmo quando o desenho falha no instante em que alguém o desenha. Por
# isso todo teste aqui DESENHA (`desenha()` grava o PNG, que força build e
# gtable) e confere algo real: o dado por trás da camada, o número de camadas,
# o tipo de coordenada, a cor da célula.

gx <- new.env()
fx <- function(dataset = "milho_se") {
  if (is.null(gx[[dataset]])) {
    p <- tr_spatial_example(dataset)
    m <- tr_spatial_variogram_fit(tr_spatial_variogram(p))
    gx[[dataset]] <- list(p = p, m = m, s = tr_spatial_kriging(p, m, resolucao = 20L))
  }
  gx[[dataset]]
}

# Grava o PNG pelo funil da `view` e devolve o caminho; falha se algo der erro
# ou aviso no build ou no desenho. Confere a assinatura do PNG, e não só que o
# arquivo existe.
desenha <- function(g) {
  pv <- expect_no_warning(trama.view::tr_view_render(g, ctx_tmp()))
  f <- pv$files$png
  expect_true(file.exists(f))
  expect_equal(readBin(f, "raw", 8L), as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
  f
}

# Largura e altura do PNG, lidas do cabeçalho IHDR (big-endian, bytes 17 a 24).
dim_png <- function(f) {
  b <- as.integer(readBin(f, "raw", 24L))[17:24]
  c(sum(b[1:4] * 256^(3:0)), sum(b[5:8] * 256^(3:0)))
}

geoms <- function(g) unname(vapply(g$layers, function(l) class(l$geom)[[1]], ""))

# `coord_equal()` no ggplot2 4 é um CoordCartesian com `ratio` 1; sem ele, `ratio` é NULL.
escala_igual <- function(g) identical(g$coordinates$ratio, 1)

# 8 pontos, sem borda e sem CRS (as duas coisas que o construtor permite omitir).
mini <- function(z = c(3, 9, 4, 1, 7, 5, 8, 2), borda = NULL, unidade = "") {
  d <- data.frame(leste = c(0, 10, 20, 5, 15, 25, 8, 18), norte = c(0, 6, 2, 12, 14, 9, 20, 4), v = z)
  tr_spatial_coordinates(d, x = "leste", y = "norte", variavel = "v", unidade = unidade,
                         borda = borda)
}

# ---- painel exploratório ---------------------------------------------------------

test_that("os cinco painéis desenham, e o completo compõe os quatro", {
  p <- fx()$p
  for (pn in c("completo", "mapa", "x", "y", "histograma")) desenha(tr_spatial_explore(p, painel = pn))
  # O completo é um patchwork de QUATRO gráficos; o que `tr_combine` guarda é o
  # primeiro como base e os outros três em `patches`.
  g <- tr_spatial_explore(p)
  expect_s3_class(g, "patchwork")
  expect_length(g$patches$plots, 3L)
  # Cada painel avulso é um ggplot de verdade, com a dimensão pendurada.
  expect_equal(attr(tr_spatial_explore(p, "x"), "tr_view_dim"), c(8, 8))
  expect_equal(attr(tr_spatial_explore(p, "x", aspecto = "4:3"), "tr_view_dim"), c(8, 6))
})

test_that("o postplot põe o quartil em tamanho E cor, em quatro classes de tamanho parecido", {
  p <- fx()$p
  g <- tr_spatial_explore(p, "mapa")
  b <- ggplot2::ggplot_build(g)
  i <- which(geoms(g) == "GeomPoint")
  expect_length(i, 1L)
  ld <- b$data[[i]]
  z <- p$dados[[p$variavel]]
  expect_equal(nrow(ld), nrow(p$dados))
  # Quatro cores e quatro tamanhos, em correspondência um a um: um canal só
  # (cor sem tamanho, ou o contrário) faria um dos dois contar menos de quatro.
  expect_length(unique(ld$fill), 4L)
  expect_length(unique(ld$size), 4L)
  expect_equal(nrow(unique(ld[, c("fill", "size")])), 4L)
  cls <- as.integer(factor(ld$size))   # tamanho cresce com a classe
  expect_true(all(abs(table(cls) - nrow(ld) / 4) <= 2))
  # As classes são faixas ordenadas de z, sem se sobrepor: é o que "quartil" quer dizer.
  mx <- tapply(z, cls, max); mn <- tapply(z, cls, min)
  expect_true(all(mn[-1] >= mx[-4]))
  # Lê em tons de cinza: a luminância sobe com a classe, então tamanho e brilho
  # concordam e o gráfico sobrevive à impressão em preto e branco.
  rgb <- grDevices::col2rgb(tapply(ld$fill, cls, `[`, 1L))
  lum <- as.vector(c(0.299, 0.587, 0.114) %*% rgb)
  expect_true(all(diff(lum) > 10))
  # Uma legenda só: cor e tamanho têm o mesmo título e as mesmas classes.
  expect_equal(b$plot$scales$get_scales("fill")$name, b$plot$scales$get_scales("size")$name)
})

test_that("o mapa dos pontos desenha a borda onde há e só onde há, e tem escala igual", {
  p <- fx()$p
  g <- tr_spatial_explore(p, "mapa")
  expect_equal(geoms(g), c("GeomPath", "GeomPoint"))
  b <- ggplot2::ggplot_build(g)
  # Os vértices da borda, na ordem e nos eixos certos (x e y trocados falhariam).
  expect_equal(b$data[[1]]$x, p$borda[, 1])
  expect_equal(b$data[[1]]$y, p$borda[, 2])
  # coord_equal: a razão entre os eixos é 1, e o painel tem a proporção dos dados.
  expect_true(escala_igual(g))
  pp <- b$layout$panel_params[[1]]
  expect_equal(g$coordinates$aspect(pp), diff(pp$y.range) / diff(pp$x.range))
  # Sem borda, sem a camada, e sem erro.
  sem <- tr_spatial_explore(mini(), "mapa")
  expect_equal(geoms(sem), "GeomPoint")
  expect_true(escala_igual(sem))
  desenha(sem)
})

test_that("os painéis contra as coordenadas NÃO têm escala igual, e mostram o eixo certo", {
  # coord_equal numa nuvem de valor contra coordenada esmagaria o gráfico: a
  # regra é para o mapa. Um painel que herdasse a regra falha aqui.
  p <- fx()$p
  gx_ <- tr_spatial_explore(p, "x"); gy_ <- tr_spatial_explore(p, "y")
  expect_false(escala_igual(gx_))
  expect_false(escala_igual(gy_))
  expect_equal(ggplot2::layer_data(gx_, 1L)$x, p$coords[, 1])
  expect_equal(ggplot2::layer_data(gx_, 1L)$y, p$dados[[p$variavel]])
  expect_equal(ggplot2::layer_data(gy_, 1L)$x, p$coords[, 2])
  expect_equal(ggplot2::layer_data(gy_, 1L)$y, p$dados[[p$variavel]])
  # O histograma conta todos os pontos.
  h <- tr_spatial_explore(p, "histograma")
  expect_equal(sum(ggplot2::layer_data(h, 1L)$count), nrow(p$dados))
})

test_that("o painel aguenta variável constante, sem borda, sem CRS e sem unidade", {
  # Valores idênticos: os quatro cortes coincidem e `cut()` recusaria.
  p <- mini(z = rep(5, 8))
  expect_null(p$borda)
  expect_false(inherits(p$crs, "crs"))
  for (pn in c("completo", "mapa", "x", "y", "histograma")) desenha(tr_spatial_explore(p, pn))
  ld <- ggplot2::layer_data(tr_spatial_explore(p, "mapa"), 1L)
  expect_length(unique(ld$fill), 1L)
  expect_equal(nrow(ld), 8L)
  # Uma classe só NA LEGENDA também: `cut()` com um único corte o leria como "cinco
  # intervalos" e a legenda mostraria cinco classes, quatro delas vazias. Conferir só
  # a cor dos pontos não pega isso.
  gc <- ggplot2::ggplot_build(tr_spatial_explore(p, "mapa"))$plot
  expect_equal(gc$scales$get_scales("fill")$get_limits(), "5")
  expect_equal(gc$scales$get_scales("size")$get_limits(), "5")
  # Sem unidade o eixo não diz "(NA)".
  lab <- ggplot2::get_labs(tr_spatial_explore(p, "mapa"))
  expect_equal(lab$x, "leste")
  expect_equal(ggplot2::get_labs(tr_spatial_explore(mini(unidade = "m"), "mapa"))$x, "leste (m)")
  # Empates parciais: classes se fundem em vez de dar erro.
  q <- mini(z = c(1, 1, 1, 1, 1, 1, 2, 3))
  desenha(tr_spatial_explore(q))
  expect_lt(length(unique(ggplot2::layer_data(tr_spatial_explore(q, "mapa"), 1L)$fill)), 4L)
})

test_that("painel fora do conjunto é erro de opção, antes de desenhar", {
  p <- fx()$p
  expect_error(tr_spatial_explore(p, painel = "sorte"), class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_explore(p, painel = c("x", "y")), class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_explore(list()), class = "tr_spatial_error_not_points")
})

test_that("o painel completo de um conjunto real desenha", {
  desenha(tr_spatial_explore(fx("milho_pr")$p))
})

# ---- mapa da superfície ----------------------------------------------------------

test_that("o mapa do predito e o do erro-padrão são dois mapas, cada um com o seu dado", {
  x <- fx()
  gp <- tr_spatial_map(x$s, "predito"); ge <- tr_spatial_map(x$s, "erro-padrao")
  desenha(gp); desenha(ge)
  expect_equal(gp$data$valor, x$s$grade$predito)
  expect_equal(ge$data$valor, x$s$grade$erro_padrao)
  # Se os dois saíssem iguais, o mapa honesto seria o enfeite.
  expect_false(isTRUE(all.equal(gp$data$valor, ge$data$valor)))
  expect_equal(geoms(gp)[1], "GeomRaster")
  expect_match(gp$scales$get_scales("fill")$name, x$p$variavel, fixed = TRUE)
  expect_match(ge$scales$get_scales("fill")$name, "erro-padrão", fixed = TRUE)
  # O raster cobre exatamente as células da grade, nas coordenadas delas.
  ld <- ggplot2::layer_data(gp, 1L)
  expect_equal(nrow(ld), nrow(x$s$grade))
  expect_equal(sort(unique(ld$x)), sort(unique(x$s$grade[[x$p$coord_cols[[1]]]])))
  # coord_equal sempre.
  for (g in list(gp, ge)) {
    expect_true(escala_igual(g))
    pp <- ggplot2::ggplot_build(g)$layout$panel_params[[1]]
    expect_equal(g$coordinates$aspect(pp), diff(pp$y.range) / diff(pp$x.range))
  }
})

test_that("isolinhas e pontos entram e saem, com o dado certo", {
  x <- fx()
  base <- tr_spatial_map(x$s)
  expect_equal(geoms(base), c("GeomRaster", "GeomPath", "GeomPoint"))
  expect_equal(nrow(ggplot2::layer_data(base, 3L)), nrow(x$p$dados))
  expect_equal(ggplot2::layer_data(base, 3L)$x, x$p$coords[, 1])
  com <- tr_spatial_map(x$s, isolinhas = TRUE)
  expect_equal(geoms(com), c("GeomRaster", "GeomContour", "GeomPath", "GeomPoint"))
  desenha(com)
  # As isolinhas têm linhas de fato: a camada que não calcula nada passaria no `geoms`.
  expect_gt(nrow(ggplot2::layer_data(com, 2L)), 0L)
  sem <- tr_spatial_map(x$s, pontos = FALSE)
  expect_equal(geoms(sem), c("GeomRaster", "GeomPath"))
  desenha(sem)
  # Sem borda: o mapa cai de uma camada e continua desenhando.
  p <- fx()$p
  p["borda"] <- list(NULL)
  s <- tr_spatial_kriging(p, x$m, resolucao = 12L)
  expect_equal(geoms(tr_spatial_map(s, isolinhas = TRUE)), c("GeomRaster", "GeomContour", "GeomPoint"))
  desenha(tr_spatial_map(s, isolinhas = TRUE))
})

test_that("célula sem predição fica cinza, e não transparente, com isolinhas e erro-padrão", {
  x <- fx()
  s <- tr_spatial_kriging(x$p, x$m, resolucao = 20L, dist_max = 8000)
  nas <- is.na(s$grade$predito)
  # Pré-condição: sem isto o teste passaria sem testar nada.
  expect_gt(sum(nas), 0L)
  expect_lt(sum(nas), nrow(s$grade))
  for (m in c("predito", "erro-padrao")) {
    g <- tr_spatial_map(s, mostrar = m, isolinhas = TRUE)
    desenha(g)
    fill <- ggplot2::layer_data(g, 1L)$fill
    expect_equal(sum(fill == "grey60"), sum(nas))
    expect_false(anyNA(fill))
  }
})

test_that("mostrar fora do conjunto é erro de opção", {
  expect_error(tr_spatial_map(fx()$s, mostrar = "sorte"), class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_map(list()), class = "tr_spatial_error_not_a_surface")
})

# ---- gráfico do modelo -----------------------------------------------------------

# Semivariância teórica, escrita à mão a partir dos parâmetros do modelo:
# esférico  g(h) = c0 + c (1,5 h/a - 0,5 (h/a)^3) para h < a, c0 + c depois;
# exponencial g(h) = c0 + c (1 - exp(-h/a)). Independe do gstat.
curva_teorica <- function(m, h) {
  r <- h / m$alcance
  m$pepita + m$contribuicao * switch(m$familia,
    esferico = ifelse(r < 1, 1.5 * r - 0.5 * r^3, 1),
    exponencial = 1 - exp(-r))
}

test_that("o gráfico do modelo desenha o empírico e uma curva que é a do modelo", {
  x <- fx()
  for (fam in c("esferico", "exponencial")) {
    m <- tr_spatial_variogram_fit(tr_spatial_variogram(x$p), familia = fam)
    g <- .tr_spatial_plot_modelo(m)
    desenha(g)
    expect_equal(geoms(g), c("GeomPoint", "GeomLine", "GeomHline", "GeomBlank"))
    emp <- ggplot2::layer_data(g, 1L)
    expect_equal(emp$x, m$variograma$tabela$u)
    expect_equal(emp$y, m$variograma$tabela$gamma)
    cv <- ggplot2::layer_data(g, 2L)
    # A curva do gráfico é a fórmula do modelo, ponto a ponto (relativo, 1e-9):
    # alcance prático no lugar do alcance, ou pepita esquecida, não passam.
    expect_equal(cv$y, curva_teorica(m, cv$x), tolerance = 1e-9)
    expect_equal(max(cv$x), max(emp$x), tolerance = 1e-9)
    expect_equal(ggplot2::layer_data(g, 3L)$yintercept, m$patamar)
  }
})

test_that("o gráfico do modelo aguenta uma classe só e a falta do variograma empírico", {
  m <- fx()$m
  um <- m
  um$variograma$tabela <- um$variograma$tabela[1L, ]
  g1 <- .tr_spatial_plot_modelo(um)
  desenha(g1)
  expect_equal(nrow(ggplot2::layer_data(g1, 1L)), 1L)
  # Modelo montado à mão: sem empírico, só a curva e o patamar, até 1,2 alcance prático.
  sem <- m
  sem["variograma"] <- list(NULL)
  g2 <- .tr_spatial_plot_modelo(sem)
  desenha(g2)
  expect_equal(geoms(g2), c("GeomLine", "GeomHline", "GeomBlank"))
  expect_equal(max(ggplot2::layer_data(g2, 1L)$x), 1.2 * m$alcance_pratico, tolerance = 1e-9)
  expect_error(.tr_spatial_plot_modelo(list()), class = "tr_spatial_error_not_a_model")
})

# ---- os cards --------------------------------------------------------------------

test_that("os cards do modelo e da superfície emitem PNG de 1600 px, não o preview de dados", {
  x <- fx()
  tipos <- trama_collection()$types
  por_id <- function(id) tipos[[which(vapply(tipos, function(t) t$id, "") == id)]]
  pm_ <- por_id("spatial/model")$preview(x$m, ctx_tmp())
  ps_ <- por_id("spatial/surface")$preview(x$s, ctx_tmp())
  for (pv in list(pm_, ps_)) {
    expect_equal(pv$renderer, "trama/image")
    expect_null(pv$data)
    expect_true(file.exists(pv$files$png))
  }
  dm <- dim_png(pm_$files$png); ds <- dim_png(ps_$files$png)
  # O lado maior é 1600 nos dois; o modelo sai 4:3 e a superfície quadrada.
  expect_equal(max(dm), 1600); expect_equal(dm[[1]] / dm[[2]], 4 / 3, tolerance = 1e-3)
  expect_equal(ds, c(1600, 1600))
  # Um modelo montado à mão também tem card.
  sem <- x$m; sem["variograma"] <- list(NULL)
  expect_true(file.exists(por_id("spatial/model")$preview(sem, ctx_tmp())$files$png))
})

# ---- os nós ----------------------------------------------------------------------

test_that("os nós de gráfico registram, na ordem, sem rotulo, e rodam no motor", {
  nos <- trama_collection()$nodes
  ex <- Filter(function(n) n$id == "spatial/explore", nos)[[1]]
  mp <- Filter(function(n) n$id == "spatial/map", nos)[[1]]
  cosm <- c("aspecto", "tema", "titulo", "rotulo_x", "rotulo_y", "legenda")
  expect_equal(names(ex$params), c("painel", cosm))
  expect_equal(names(mp$params), c("mostrar", "isolinhas", "pontos", cosm))
  expect_equal(ex$category, "espacial_explorar")
  expect_equal(mp$category, "espacial_predizer")
  expect_equal(names(ex$inputs), "pontos")
  expect_equal(names(mp$inputs), "superficie")
  expect_equal(ex$version, 1L); expect_equal(mp$version, 1L)
  expect_equal(ex$params$aspecto$default, "1:1")
  expect_equal(mp$params$aspecto$default, "1:1")
  expect_false("rotulo" %in% c(names(ex$params), names(mp$params)))
  # A ajuda do mapa diz por que o erro-padrão é pedido no mesmo bloco.
  expect_match(mp$help, "par honesto", fixed = TRUE)
  expect_match(mp$help, "erro-padrao", fixed = TRUE)
  reg <- spatial_registry()
  fl <- trama::tr_flow(reg) |>
    trama::tr_add("p", "spatial/example", dataset = "milho_se") |>
    trama::tr_add("e", "spatial/explore", painel = "mapa", from = "p") |>
    trama::tr_add("v", "spatial/variogram", from = "p") |>
    trama::tr_add("m", "spatial/variogram_fit", from = "v") |>
    trama::tr_add("k", "spatial/kriging", resolucao = 12L, from = c("p", "m")) |>
    trama::tr_add("mp", "spatial/map", mostrar = "erro-padrao", from = "k")
  ge <- rodar(fl, "e"); gm <- rodar(fl, "mp")
  expect_s3_class(ge, "ggplot"); expect_s3_class(gm, "ggplot")
  desenha(ge); desenha(gm)
  expect_match(gm$scales$get_scales("fill")$name, "erro-padrão", fixed = TRUE)
  expect_equal(attr(gm, "tr_view_dim"), c(8, 8))
})
