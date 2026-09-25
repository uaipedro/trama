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

# `patchworkGrob` mede texto e precisa de um device aberto; sem este, o R abre
# o `pdf()` padrão e deixa um `Rplots.pdf` na pasta dos testes.
pw_grob <- function(p) {
  ragg::agg_png(tempfile(fileext = ".png")); on.exit(grDevices::dev.off(), add = TRUE)
  patchwork::patchworkGrob(p)
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
  pw <- pw_grob(tr_combine(list(g$a, g$b)))
  expect_equal(c(rotulo_grob(pw, "title-1"), rotulo_grob(pw, "tag-1")), c("um", "A"))
  expect_equal(c(rotulo_grob(pw, "title-2"), rotulo_grob(pw, "tag-2")), c("dois", "B"))
  # Invertida a entrada, inverte o painel: a ordem é a da lista, e só dela.
  pw <- pw_grob(tr_combine(list(g$b, g$a), etiquetas = "1, 2, 3"))
  expect_equal(c(rotulo_grob(pw, "title-1"), rotulo_grob(pw, "tag-1")), c("dois", "1"))
  expect_length(rotulo_grob(pw_grob(tr_combine(list(g$a, g$b), etiquetas = "nenhuma")),
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
  pw <- pw_grob(p)
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
  nomes <- pw_grob(p)$layout$name
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

# Largura e altura de um TIFF, lidas do cabeçalho (tags 256 e 257), para não
# depender do pacote `tiff` só por dois números.
dim_tiff <- function(f) {
  b <- readBin(f, "raw", file.size(f))
  end <- if (identical(rawToChar(b[1:2]), "II")) "little" else "big"
  u <- function(at, n) readBin(b[at + seq_len(n)], "integer", size = n, endian = end, signed = n == 4)
  ifd <- u(4, 4); n <- u(ifd, 2); v <- c()
  for (i in seq_len(n) - 1L) {
    e <- ifd + 2 + 12 * i; tag <- u(e, 2); tipo <- u(e + 2, 2)
    if (tag %in% c(256, 257)) v[as.character(tag)] <- if (tipo == 3) u(e + 8, 2) else u(e + 8, 4)
  }
  unname(c(v[["256"]], v[["257"]]))
}

test_that("PNG e TIFF saem com mm x dpi / 25,4 pixels, arredondado", {
  d <- tempfile(); dir.create(d)
  a <- tr_points(df_exemplo(), "qtd", "valor", aspecto = "4:3")
  expect_identical(tr_save(a, file.path(d, "f1.png"), "png", largura_mm = 170, altura_mm = 100), a)
  # 170 mm a 300 dpi = 2007,87 px: o arquivo tem 2008, e não os 2007 do truncamento.
  expect_equal(dim(png::readPNG(file.path(d, "f1.png")))[2:1], round(c(170, 100) * 300 / 25.4))
  tr_save(a, file.path(d, "f2"), "tiff", largura_mm = 85, altura_mm = 60, dpi = 600)
  expect_equal(dim_tiff(file.path(d, "f2.tiff")), round(c(85, 60) * 600 / 25.4))
})

test_that("altura 0 deriva da proporção do gráfico", {
  d <- tempfile(); dir.create(d)
  a <- tr_points(df_exemplo(), "qtd", "valor", aspecto = "4:3")
  tr_save(a, file.path(d, "f.png"), "png", largura_mm = 120, dpi = 300)
  expect_equal(dim(png::readPNG(file.path(d, "f.png")))[2:1],
               round(c(120, 120 * 3 / 4) * 300 / 25.4))
})

# A caixa da página de um PDF. O cairo guarda os objetos num fluxo comprimido
# (`/ObjStm`), então se procura primeiro no texto cru e depois dentro de cada
# fluxo descomprimido.
pdf_mediabox <- function(f) {
  b <- readBin(f, "raw", file.size(f))
  textos <- list(b)
  ini <- grepRaw("stream\r?\n", b, all = TRUE); fim <- grepRaw("endstream", b, all = TRUE)
  ini <- setdiff(ini, fim + 3L)
  for (k in seq_along(fim)) {
    corpo <- b[(ini[[k]] + 7L):(fim[[k]] - 2L)]
    x <- tryCatch(memDecompress(corpo, "gzip"), error = function(e) NULL)
    if (!is.null(x)) textos[[length(textos) + 1L]] <- x
  }
  for (x in textos) {
    t <- rawToChar(x[x != as.raw(0)])
    m <- regmatches(t, regexpr("/MediaBox *\\[[^]]*\\]", t, useBytes = TRUE))
    if (length(m)) return(as.numeric(regmatches(m, gregexpr("[0-9.]+", m))[[1]]))
  }
  stop("PDF sem MediaBox: ", f)
}

test_that("PDF sai com a página em polegadas do tamanho pedido", {
  d <- tempfile(); dir.create(d); f <- file.path(d, "f.pdf")
  tr_save(tr_points(df_exemplo(), "qtd", "valor"), f, "pdf", largura_mm = 85, altura_mm = 60)
  nums <- pdf_mediabox(f)
  # Tolerância de 1 pt (0,35 mm): o cairo escreve a caixa em pontos inteiros
  # (240,94 pt sai 240). É a troca por fonte embutida, que as revistas pedem.
  expect_equal(nums[3:4], c(85, 60) / 25.4 * 72, tolerance = 1.01 / 240)
})

test_that("pasta inexistente, extensão trocada e caminho em branco", {
  a <- tr_points(df_exemplo(), "qtd", "valor")
  err <- tryCatch(tr_save(a, file.path(tempfile(), "sub", "f.png")), condition = identity)
  expect_s3_class(err, "tr_view_error_missing_dir")
  expect_match(conditionMessage(err), "pasta de destino não existe")
  expect_error(tr_save(a, file.path(tempdir(), "f.png"), "pdf"), class = "tr_view_error_bad_option")
  # Em branco é "desligado", como nos gravadores da `data`: repassa sem gravar.
  expect_identical(tr_save(a, ""), a)
})

test_that("pelo motor, dados -> dois gráficos -> painel -> arquivo", {
  reg <- view_registry(); s <- trama::tr_store(tempfile())
  d <- tempfile(); dir.create(d); f <- file.path(d, "figura1.png")
  fl <- trama::tr_flow(reg) |>
    trama::tr_add("d", "data/example", dataset = "mtcars") |>
    trama::tr_add("a", "view/points", x = "wt", y = "mpg", from = "d") |>
    trama::tr_add("b", "view/histogram", x = "hp", from = "d") |>
    trama::tr_add("p", "view/combine", aspecto = "2:1", from = c("a", "b")) |>
    trama::tr_add("s", "view/save", path = f, largura_mm = 170, from = "p")
  trama::tr_run(fl$doc, registry = reg, store = s)
  expect_equal(dim(png::readPNG(f))[2:1], round(c(170, 85) * 300 / 25.4))
})
