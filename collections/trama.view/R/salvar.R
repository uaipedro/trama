# `view/save`: a figura no disco, no formato do periódico. Mora num arquivo
# próprio, e não em `figura.R`, porque é o único nó da coleção que não desenha:
# as regras dele (caminho, device, unidade) são as dos gravadores da `data`.

# SVG fica de fora enquanto o `svglite` não for dependência: o `grDevices::svg`
# depende de cairo e desenha texto como contorno, o que tira do SVG a única
# vantagem que ele teria sobre o PDF (texto editável no Inkscape).
.TR_VIEW_FORMATOS <- c("pdf", "png", "tiff")

#' Grava a figura no disco, no tamanho e na resolução do periódico.
#'
#' Mesmo contrato dos gravadores da coleção `data`: REPASSA o gráfico adiante,
#' caminho em branco é "desligado" (card recém-arrastado não pinta de
#' vermelho), e grava em toda execução.
#'
#' Tamanho em MILÍMETROS porque é a unidade das instruções aos autores (uma
#' coluna ~85 mm, página ~170 mm). Altura 0 segue a proporção escolhida no
#' gráfico: quem ajustou o aspecto olhando o card recebe no arquivo a mesma
#' forma, só maior.
#'
#' `texto_pt` existe porque o tema é calibrado para o CARD (base 13 pt num
#' desenho de 8 pol) e a fonte é absoluta: gravado a 170 mm, o texto sairia
#' com 13–16 pt impressos, e num painel os títulos se atropelam. O periódico
#' pede 8–10 pt impressos; trocar só o `text` base basta, porque título,
#' eixos, legenda e etiqueta derivam dele por `rel()`.
#'
#' PNG e TIFF saem pelo `ragg`, pelo mesmo motivo do preview (não depende de
#' X11); TIFF com LZW, que as revistas aceitam e que corta o arquivo a uma
#' fração. PDF pelo `cairo_pdf`, que embute a fonte e escreve acento sem
#' depender da codificação do device `pdf()`.
#' @param grafico um ggplot (ou painel de [tr_combine()]).
#' @param path arquivo de saída; sem extensão, recebe a do `formato`.
#' @param formato `"pdf"`, `"png"` ou `"tiff"`.
#' @param largura_mm largura em milímetros.
#' @param altura_mm altura em milímetros; `0` deriva da proporção do gráfico.
#' @param dpi resolução de PNG e TIFF (o PDF é vetorial e a ignora).
#' @param texto_pt tamanho do texto base, em pontos, na figura gravada; `0`
#'   mantém o do tema.
#' @return o próprio `grafico`, para seguir no fluxo.
#' @export
tr_save <- function(grafico, path = "", formato = "png", largura_mm = 170, altura_mm = 0,
                    dpi = 300, texto_pt = 9, .ctx = NULL) {
  if (!length(path) || is.na(path[[1]]) || !nzchar(trimws(path[[1]]))) return(grafico)
  if (!formato %in% .TR_VIEW_FORMATOS) .tr_view_option("formato", formato, .TR_VIEW_FORMATOS)
  if (!is.null(.ctx)) path <- .ctx$path(path)
  ext <- tolower(tools::file_ext(path))
  if (!nzchar(ext)) {
    path <- paste0(path, ".", formato)
  } else if (!identical(ext, formato) && !(formato == "tiff" && ext == "tif")) {
    # Extensão que contradiz o formato é erro, e não "o formato vence": um
    # `figura.png` com bytes de PDF dentro abre quebrado em todo visualizador.
    rlang::abort(sprintf("Param 'path': o arquivo termina em .%s, mas o formato escolhido é %s.",
                         ext, formato), class = "tr_view_error_bad_option")
  }
  if (!dir.exists(dirname(path))) {
    rlang::abort(sprintf("Param 'path': a pasta de destino não existe: %s.", dirname(path)),
                 class = "tr_view_error_missing_dir")
  }
  largura <- .tr_view_medida(largura_mm, "largura_mm")
  altura <- suppressWarnings(as.numeric(altura_mm))
  if (!length(altura) || is.na(altura[[1]]) || altura[[1]] <= 0) {
    d <- attr(grafico, "tr_view_dim") %||% c(8, 4.5)
    altura <- largura * d[[2]] / d[[1]]
  } else {
    altura <- altura[[1]]
  }
  dpi <- .tr_view_medida(dpi, "dpi")
  # O objeto que SEGUE no fluxo é o original: o card continua no tamanho do card.
  grafico_out <- grafico
  grafico <- .tr_view_texto(grafico, texto_pt)
  if (formato == "pdf") {
    dev <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
    ggplot2::ggsave(path, plot = grafico, width = largura, height = altura, units = "mm",
                    device = dev)
  } else {
    # Raster em PIXELS inteiros, e não pelo `ggsave`: ele passa polegadas ao
    # device, e o `ragg` TRUNCA o produto — 170 mm a 300 dpi são 2007,87 px e
    # saíam 2007. Arredondar aqui é o que faz o arquivo ter o tamanho que a
    # conta da revista (mm x dpi / 25,4) promete. `res` grava o dpi no
    # cabeçalho, que é o que o sistema de submissão lê.
    px <- round(c(largura, altura) * dpi / 25.4)
    dev <- switch(formato, png = ragg::agg_png,
                  tiff = function(...) ragg::agg_tiff(..., compression = "lzw"))
    dev(path, width = px[[1]], height = px[[2]], units = "px", res = dpi)
    tryCatch(print(grafico), finally = grDevices::dev.off())
  }
  grafico_out
}

#' O texto base em pontos; `0`, vazio ou NA mantém o tema.
#'
#' `&` no painel, porque `+` só atingiria o último gráfico dele.
#' @noRd
.tr_view_texto <- function(grafico, texto_pt) {
  pt <- suppressWarnings(as.numeric(texto_pt))
  if (!length(pt) || is.na(pt[[1]]) || pt[[1]] <= 0) return(grafico)
  # `geom = element_geom(fontsize)` leva junto o texto DESENHADO pelas camadas
  # (equação, anotação, rótulo da referência), que lê o tamanho do tema.
  .tr_view_texto_em(grafico, ggplot2::theme(text = ggplot2::element_text(size = pt[[1]]),
                                            geom = ggplot2::element_geom(fontsize = pt[[1]])))
}

#' Desce o tema até DENTRO dos painéis aninhados. O `&` não atravessa o
#' `wrap_elements()` com que `tr_combine()` embrulha um painel interno (o
#' patchwork interno fica guardado em `attr(, "grobs")`), e sem esta descida o
#' painel externo encolhia e o interno ficava no tamanho do card.
#' @noRd
.tr_view_texto_em <- function(g, tx) {
  if (inherits(g, "wrapped_patch")) {
    gr <- attr(g, "grobs")
    for (k in names(gr)) if (inherits(gr[[k]], "ggplot")) gr[[k]] <- .tr_view_texto_em(gr[[k]], tx)
    attr(g, "grobs") <- gr
    return(g)
  }
  if (!inherits(g, "patchwork")) return(g + tx)
  g$patches$plots <- lapply(g$patches$plots, .tr_view_texto_em, tx = tx)
  g & tx
}

#' Número positivo, ou erro que diz qual campo.
#' @noRd
.tr_view_medida <- function(x, param) {
  v <- suppressWarnings(as.numeric(x))
  if (!length(v) || is.na(v[[1]]) || v[[1]] <= 0) {
    rlang::abort(sprintf("Param '%s': precisa ser um número maior que zero, não '%s'.",
                         param, paste(as.character(x), collapse = ", ")),
                 class = "tr_view_error_bad_option")
  }
  v[[1]]
}

.tr_view_no_salvar <- function(P, G) {
  # Não declara os cosméticos: gravar não redesenha. Um tema aqui seria um
  # segundo lugar para decidir a aparência, e o card de origem mostraria uma
  # figura diferente da que foi para o disco.
  trama::tr_node("view/save", fn = tr_save, label = "Salvar figura",
    category = "figura", description = "Grava o gráfico em PDF, PNG ou TIFF no tamanho do periódico.",
    icon = trama::tr_icon("save"),
    inputs = list(grafico = G), outputs = list(out = G),
    params = list(
      path = P("path", "", label = "Arquivo", example = "figuras/figura1.pdf"),
      formato = trama::tr_param_enum("png", .TR_VIEW_FORMATOS, label = "Formato"),
      largura_mm = trama::tr_param_num(170, min = 20, max = 500, step = 1, label = "Largura (mm)"),
      altura_mm = trama::tr_param_num(0, min = 0, max = 500, step = 1, label = "Altura (mm)"),
      dpi = trama::tr_when(trama::tr_param_num(300, min = 72, max = 1200, step = 1, label = "Resolução (dpi)"),
                           formato = c("png", "tiff")),
      texto_pt = trama::tr_param_num(9, min = 0, max = 14, step = 0.5, label = "Texto (pt)")),
    pure = FALSE, volatile = TRUE,
    fingerprint = function(params, ctx) ctx$path(params$path),
    help = "## Descrição

Grava o gráfico (ou o painel) num arquivo, no tamanho e na resolução que as
instruções aos autores pedem, e o REPASSA adiante. Caminho relativo é resolvido
a partir da pasta do projeto; a pasta tem que existir. Caminho em branco não
grava nada: o card recém-arrastado não é erro.

O tamanho é em milímetros porque é assim que as revistas o pedem: uma coluna
tem por volta de 85 mm, a página inteira por volta de 170 mm. O texto sai no
tamanho IMPRESSO de **Texto (pt)**, qualquer que seja a largura: o periódico
pede 8–10 pt na página, e é isso que o padrão 9 entrega. O tema do card é
calibrado para a tela, e gravado a 170 mm daria letras de 13–16 pt, com
títulos se sobrepondo num painel.

O nó grava em TODA execução do fluxo, mesmo que nada tenha mudado: um arquivo
apagado à mão volta na próxima execução.

## Parâmetros

- **Arquivo** — onde gravar. Sem extensão, recebe a do formato; extensão que
  contradiz o formato para o nó.
- **Formato** — `pdf` (vetorial, o preferido para gráfico de linhas e pontos),
  `png` ou `tiff` (TIFF com compressão LZW, o que muitas revistas exigem).
- **Largura (mm)** — padrão 170, a largura de página comum.
- **Altura (mm)** — `0` (padrão) segue a proporção escolhida no gráfico.
- **Resolução (dpi)** — para PNG e TIFF: 300 (padrão) para figura colorida,
  600 ou 1000 para desenho de linhas. O PDF ignora.
- **Texto (pt)** — tamanho do texto base na figura gravada, padrão 9 (de 6 a
  14). Título e etiquetas saem um pouco maiores, eixos um pouco menores, na
  proporção do tema. `0` mantém o tamanho do tema. O card não muda.

## Valor

O próprio gráfico, sem mudança, para seguir no fluxo. No console,
`tr_save(p, \"figura1.pdf\", formato = \"pdf\", largura_mm = 85)`.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"ensaio.csv\") |>
  tr_add(\"box\", \"view/boxplot\", x = \"tratamento\", y = \"resposta\",
         tema = \"clássico\", from = \"ler\") |>
  tr_add(\"grava\", \"view/save\", path = \"figuras/figura1.tiff\",
         formato = \"tiff\", largura_mm = 85, dpi = 600, from = \"box\")
```

## Veja também

`view/combine` para montar o painel antes de gravar; `data/write_csv` para
gravar a tabela que deu origem à figura.")
}
