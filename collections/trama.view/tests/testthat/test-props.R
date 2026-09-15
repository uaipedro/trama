test_that("a proporção vira polegadas com o lado maior fixo em 8", {
  expect_equal(.tr_view_dim("16:9"), c(8, 4.5))
  expect_equal(.tr_view_dim("4:3"),  c(8, 6))
  expect_equal(.tr_view_dim("1:1"),  c(8, 8))
  expect_equal(.tr_view_dim("3:4"),  c(6, 8))
  expect_equal(.tr_view_dim("2:1"),  c(8, 4))
})

test_that("proporção fora do conjunto é erro classificado", {
  expect_error(.tr_view_dim("16:10"), class = "tr_view_error_bad_option")
})

test_that("nome de tema que não existe, no console, é erro classificado", {
  err <- expect_error(.tr_view_tema("escurro"), class = "tr_view_error_bad_option")
  expect_match(conditionMessage(err), "claro")
})

test_that("o tema aceita a definição resolvida e o nome", {
  expect_s3_class(.tr_view_tema(trama::tr_theme("claro")), "theme")
  expect_s3_class(.tr_view_tema("claro"), "theme")
  expect_s3_class(.tr_view_tema("padrão"), "theme")
  t <- trama::tr_theme("claro"); t$fundo <- "#123456"
  expect_equal(.tr_view_tema(t)$plot.background$fill, "#123456")
  # Resolvido com `ausente` desenha com o padrão: avisar é do card, não do fn.
  expect_s3_class(.tr_view_tema(trama::tr_theme("sumiu")), "theme")
})

test_that("base e fonte do tema viram ggplot", {
  t <- trama::tr_theme("escuro")
  expect_s3_class(.tr_view_tema(t)$panel.border, "element_blank")
  t$base <- "bw"; t$fonte <- "mono"
  th <- .tr_view_tema(t)
  expect_s3_class(th$panel.border, "element_rect")
  expect_equal(th$panel.border$colour, t$eixos)
  expect_equal(th$text$family, "mono")
  t$base <- "classic"
  expect_equal(.tr_view_tema(t)$axis.line$colour, t$eixos)
})

cores_de <- function(p) ggplot2::ggplot_build(p)$data[[1]]$colour
acabar <- function(p, tema) .tr_view_acabar(p, "16:9", tema, "", "", "", "direita")

test_that("mapeamento discreto recebe a paleta do tema", {
  t <- trama::tr_theme("claro")
  p <- ggplot2::ggplot(df_exemplo(), ggplot2::aes(valor, qtd, colour = regiao)) + ggplot2::geom_point()
  expect_true(t$paleta[[1]] %in% cores_de(acabar(p, t)))
  # Mapeamento só na camada também conta; `fill` idem.
  q <- ggplot2::ggplot(df_exemplo()) + ggplot2::geom_col(ggplot2::aes(regiao, valor, fill = produto))
  expect_true(t$paleta[[1]] %in% ggplot2::ggplot_build(acabar(q, t))$data[[1]]$fill)
})

test_that("escala explícita do gráfico não é trocada pela paleta", {
  t <- trama::tr_theme("claro")
  p <- ggplot2::ggplot(df_exemplo(), ggplot2::aes(valor, qtd, colour = regiao)) + ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = c(norte = "#000001", sul = "#000002"))
  expect_setequal(unique(cores_de(acabar(p, t))), c("#000001", "#000002"))
})

test_that("mapeamento contínuo recebe a escala contínua do tema", {
  t <- trama::tr_theme("claro")
  p <- ggplot2::ggplot(df_exemplo(), ggplot2::aes(valor, qtd, colour = valor)) + ggplot2::geom_point()
  padrao <- cores_de(p)
  vir <- cores_de(acabar(p, t))
  expect_false(identical(vir, padrao))
  expect_true("#440154" %in% substr(vir, 1, 7) || "#440154FF" %in% vir)
  t$continua <- "divergente"
  expect_false(identical(cores_de(acabar(p, t)), vir))
  for (cont in c("magma", "cividis", "azuis")) {
    t$continua <- cont
    expect_s3_class(acabar(p, t), "ggplot")
    expect_length(cores_de(acabar(p, t)), 6L)
  }
})

test_that("acabar pendura a dimensão no objeto, e ela sobrevive ao saveRDS", {
  p <- .tr_view_acabar(ggplot2::ggplot(df_exemplo()), aspecto = "4:3", tema = "escuro",
                       titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita")
  expect_s3_class(p, "ggplot")
  expect_equal(attr(p, "tr_view_dim"), c(8, 6))
  f <- tempfile(fileext = ".rds"); saveRDS(p, f)
  expect_equal(attr(readRDS(f), "tr_view_dim"), c(8, 6))
})

# O `+` posterior é o caso que o Task 3.1 encosta: todo `fn` chama `.tr_view_acabar()`
# por último, mas nada no ggplot promete que somar uma camada preserve atributo de
# terceiro. Se um dia deixar de preservar, é aqui que se descobre, e não numa imagem
# fora de forma que ninguém nota.
test_that("a dimensão sobrevive a um `+` depois do acabamento", {
  p <- .tr_view_acabar(ggplot2::ggplot(df_exemplo()), "4:3", "escuro",
                       titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita")
  expect_equal(attr(p + ggplot2::labs(title = "depois"), "tr_view_dim"), c(8, 6))
})

test_that("rótulo e título em branco são DESLIGADO, não erro", {
  p <- .tr_view_acabar(ggplot2::ggplot(df_exemplo()), "16:9", "escuro",
                       titulo = "", rotulo_x = "", rotulo_y = "", legenda = "nenhuma")
  expect_null(p$labels$title)
  # E o contraponto, senão o `expect_null` acima passaria mesmo se `labs()` nunca
  # fosse aplicado: com título preenchido, o título TEM de estar lá.
  q <- .tr_view_acabar(ggplot2::ggplot(df_exemplo()), "16:9", "escuro",
                       titulo = "  Receita  ", rotulo_x = "Região", rotulo_y = "",
                       legenda = "nenhuma")
  expect_equal(q$labels$title, "  Receita  ")
  expect_equal(q$labels$x, "Região")
  expect_null(q$labels$y)
})

test_that("legenda fora do conjunto é erro classificado", {
  expect_error(
    .tr_view_acabar(ggplot2::ggplot(df_exemplo()), "16:9", "escuro",
                    titulo = "", rotulo_x = "", rotulo_y = "", legenda = "esquerda"),
    class = "tr_view_error_bad_option")
})

test_that("os props comuns entram DEPOIS dos próprios do gráfico", {
  ps <- .tr_view_props(x = trama::tr_param("cols", "", label = "Eixo X"))
  expect_equal(names(ps)[[1]], "x")
  expect_true(all(c("aspecto", "tema", "titulo", "rotulo_x", "rotulo_y", "legenda")
                  %in% names(ps)))
})
