# Figura: o que vem DEPOIS do gráfico, quando ele vai para um artigo ou tese.
#
# Os outros nós da coleção respondem "que gráfico mostra isto?". Estes dois
# respondem a pergunta seguinte, a de quem escreve: juntar dois ou três
# gráficos num painel com etiquetas A, B, C — que é como a figura aparece no
# periódico — e gravar a figura na largura e na resolução que a revista pede.
# Os dois recebem e devolvem `view/plot`, e é isso que os deixa encadear:
# painel de painéis, painel que vai para o disco, gráfico solto que vai para o
# disco sem passar pelo painel.

.TR_VIEW_ETIQUETAS <- c("nenhuma", "A, B, C", "a, b, c", "1, 2, 3")

#' Junta gráficos num painel com etiquetas.
#'
#' O painel é um `patchwork`, que HERDA de ggplot: passa pelo `store` do tipo
#' `view/plot` sem guard novo, e o preview o desenha pelo mesmo
#' [tr_view_render()]. O que muda em relação ao funil comum
#' ([tr_view_finish()]) é o operador: `+ tema` num patchwork só atinge o
#' ÚLTIMO painel, então o tema e os rótulos entram com `&`, que desce a todos.
#' Sem isso, o painel sairia com um tema por painel — o de cada nó de origem —
#' e a figura do artigo com três fundos diferentes.
#'
#' O tema vai também no `plot_annotation(theme = )`: é ele que pinta o fundo
#' ENTRE os painéis e atrás do título. Sem isso, no tema escuro sobram frestas
#' brancas entre os gráficos.
#'
#' A ordem dos painéis (e portanto das etiquetas) é a ordem das entradas, que o
#' núcleo fixa pelo `index` da aresta: a primeira ligada é o A.
#' @param graficos lista de ggplots (ou um só).
#' @param colunas quantos painéis por linha; `0` deixa o patchwork decidir.
#' @param etiquetas `"nenhuma"`, `"A, B, C"`, `"a, b, c"` ou `"1, 2, 3"`.
#' @param legenda_comum `TRUE` junta legendas iguais numa só (`guides = "collect"`).
#' @param aspecto,tema,titulo,rotulo_x,rotulo_y,legenda os cosméticos de
#'   [tr_view_props()]; o título vira o título do painel inteiro, e rótulo
#'   preenchido vale para todos os painéis.
#' @return um patchwork (que é um ggplot) com o atributo `tr_view_dim`.
#' @export
tr_combine <- function(graficos, colunas = 0, etiquetas = "A, B, C", legenda_comum = FALSE,
                       aspecto = "16:9", tema = "padrão", titulo = "", rotulo_x = "",
                       rotulo_y = "", legenda = "direita") {
  if (inherits(graficos, "ggplot")) graficos <- list(graficos)
  if (!length(graficos) || !all(vapply(graficos, function(g) inherits(g, "ggplot"), logical(1)))) {
    rlang::abort("Param 'graficos': o painel precisa de pelo menos um gráfico, e só de gráficos.",
                 class = "tr_view_error_not_a_plot")
  }
  if (!etiquetas %in% .TR_VIEW_ETIQUETAS) .tr_view_option("etiquetas", etiquetas, .TR_VIEW_ETIQUETAS)
  if (!legenda %in% .TR_VIEW_LEGENDAS) .tr_view_option("legenda", legenda, .TR_VIEW_LEGENDAS)
  # 0, vazio ou NA = automático: o patchwork escolhe uma grade quase quadrada,
  # que é o que se quer até o autor ter opinião.
  ncol <- suppressWarnings(as.integer(colunas))
  ncol <- if (!length(ncol) || is.na(ncol[[1]]) || ncol[[1]] <= 0L) NULL else ncol[[1]]
  tag <- switch(etiquetas, nenhuma = NULL, "A, B, C" = "A", "a, b, c" = "a", "1, 2, 3" = "1")
  pos <- switch(legenda, direita = "right", abaixo = "bottom", nenhuma = "none")
  th <- .tr_view_tema(tema) + ggplot2::theme(legend.position = pos)

  p <- patchwork::wrap_plots(graficos, ncol = ncol,
                             guides = if (isTRUE(legenda_comum)) "collect" else "auto")
  p <- p & th
  if (nzchar(trimws(rotulo_x))) p <- p & ggplot2::labs(x = rotulo_x)
  if (nzchar(trimws(rotulo_y))) p <- p & ggplot2::labs(y = rotulo_y)
  p <- p + patchwork::plot_annotation(
    title = if (nzchar(trimws(titulo))) titulo else NULL, tag_levels = tag, theme = th)
  attr(p, "tr_view_dim") <- .tr_view_dim(aspecto)
  p
}

.tr_view_nos_figura <- function(P, G) {
  list(
    trama::tr_node("view/combine", fn = tr_combine, label = "Painel",
      category = "figura", description = "Junta gráficos num painel com etiquetas A, B, C.",
      icon = trama::tr_icon("layout-grid"),
      inputs = list(graficos = trama::tr_port(G, multiple = TRUE)), outputs = list(out = G),
      params = .tr_view_props(
        colunas = trama::tr_param_num(0, min = 0, max = 6, step = 1, label = "Colunas"),
        etiquetas = trama::tr_param_enum("A, B, C", .TR_VIEW_ETIQUETAS, label = "Etiquetas"),
        legenda_comum = trama::tr_param_bool(FALSE, label = "Legenda comum")),
      help = paste0("## Descrição

Junta dois ou mais gráficos numa figura só, lado a lado ou em grade, com uma
etiqueta em cada painel (A, B, C). É a figura com vários painéis do artigo e da
tese, montada no fluxo em vez de num editor de imagem — e por isso refeita
sozinha quando o dado muda.

A ORDEM dos painéis é a ordem em que os gráficos foram ligados à entrada: o
primeiro ligado é o A. Para trocar a ordem, desligue e religue.

O tema do painel vale para TODOS os gráficos, qualquer que seja o tema que
cada um tinha no próprio card: uma figura de artigo com um fundo por painel
não passa na revisão. O mesmo vale para **Rótulo do X** e **Rótulo do Y**:
preenchidos, trocam o rótulo de todos os painéis; em branco, cada painel
mantém o seu. O **Título** vira o título da figura inteira, acima dos painéis.

## Parâmetros

- **Colunas** — quantos painéis por linha. `0` (padrão) deixa uma grade quase
  quadrada; `1` empilha tudo; o número de gráficos põe tudo numa linha.
- **Etiquetas** — `A, B, C` (padrão), `a, b, c`, `1, 2, 3` ou `nenhuma`.
- **Legenda comum** — ligado, legendas IGUAIS em vários painéis viram uma só,
  na posição de **Legenda**. Legendas diferentes continuam separadas.

## Valor

Um gráfico (um `patchwork`, que é um ggplot), que pode seguir para
`view/save` ou entrar em outro painel. No console,
`tr_combine(list(p1, p2), colunas = 2)`.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"ensaio.csv\") |>
  tr_add(\"a\", \"view/boxplot\", x = \"tratamento\", y = \"resposta\", from = \"ler\") |>
  tr_add(\"b\", \"view/points\", x = \"dose\", y = \"resposta\", from = \"ler\") |>
  tr_add(\"fig\", \"view/combine\", colunas = 2, tema = \"clássico\",
         aspecto = \"2:1\", from = c(\"a\", \"b\"))
```

## Veja também

`view/save` para gravar o painel no tamanho do periódico; `view/points`,
`view/boxplot` e os outros gráficos da coleção como painéis.", .TR_VIEW_AJUDA_APARENCIA))
  )
}
