# Os cosméticos que TODO nó de gráfico declara igual, declarados uma vez.
#
# Sem isto, seis params se repetiriam em cinco nós — e, pior, se repetiriam nas
# cinco páginas de `help`, que é onde mora o custo real de uma coleção. Mesma
# figura do `.tr_data_file_print` da coleção `data`: a coisa que uma família
# inteira de nós declara idêntica não é declarada cinco vezes.

.TR_VIEW_ASPECTOS <- c("16:9", "4:3", "1:1", "3:4", "2:1")
.TR_VIEW_LEGENDAS <- c("direita", "abaixo", "nenhuma")

#' Params cosméticos comuns, sempre DEPOIS dos próprios do gráfico.
#'
#' A ordem importa e não é estética: o card desenha os params na ordem de
#' declaração, então o que decide o que o gráfico AFIRMA (eixos, cor) fica em
#' cima e o que decide como ele PARECE fica embaixo.
#'
#' `aspecto` é enum, e não número com slider, por duas razões independentes.
#' A boa: proporção não é contínua — ninguém quer 1,73:1, quer paisagem,
#' quadrado ou retrato, nas proporções que já são convenção. A prática: o
#' widget de número do núcleo comita a cada passo do controle, e cada passo
#' aqui é um PNG renderizado.
#' @noRd
.tr_view_props <- function(...) {
  P <- trama::tr_param
  c(list(...), list(
    aspecto  = trama::tr_param_enum("16:9", .TR_VIEW_ASPECTOS, label = "Proporção"),
    tema     = trama::tr_param_theme(),
    titulo   = P("text", "", label = "Título", example = "Receita por região"),
    rotulo_x = P("text", "", label = "Rótulo do X", example = "Região"),
    rotulo_y = P("text", "", label = "Rótulo do Y", example = "Receita (R$)"),
    legenda  = trama::tr_param_enum("direita", .TR_VIEW_LEGENDAS, label = "Legenda")
  ))
}

#' O param "Painéis por", comum aos nós da `view` que o aceitam.
#'
#' Não mora em `.tr_view_props()` porque aqueles seis são CONTRATO das outras
#' coleções: o `fn` de todo nó que os usa precisa aceitá-los, e um sétimo
#' quebraria cada um deles. Declarado uma vez aqui, entra por nó, antes dos
#' cosméticos — é param do desenho, não da aparência.
#' @noRd
.tr_view_painel_param <- function() {
  trama::tr_param_col("", label = "Painéis por", role = "categorica", example = "regiao")
}

#' Proporção -> polegadas, com o LADO MAIOR fixo em 8.
#'
#' A resolução não é param de propósito: 8 polegadas a 200 dpi dão 1600 px no
#' lado maior, sempre. Expor os dois faria dois botões que interagem, e a frase
#' que rege o desenho ("o aspecto é o que importa") deixaria de ser verdade.
#' Lado maior fixo, e não largura fixa, porque com largura fixa o retrato
#' cresceria sem limite: `3:4` viraria 8x10,67 pol.
#' @noRd
.tr_view_dim <- function(aspecto) {
  if (!aspecto %in% .TR_VIEW_ASPECTOS) .tr_view_option("aspecto", aspecto, .TR_VIEW_ASPECTOS)
  r <- as.numeric(strsplit(aspecto, ":", fixed = TRUE)[[1]])
  if (r[[1]] >= r[[2]]) c(8, 8 * r[[2]] / r[[1]]) else c(8 * r[[1]] / r[[2]], 8)
}

#' O tema, como objeto de ggplot.
#'
#' Os campos moram no projeto (`trama.json`), e o núcleo só os valida e
#' resolve; transformá-los em ggplot é o trabalho desta função, e de nenhuma
#' outra. O padrão dos embutidos continua `escuro`, mas não mais porque o card
#' é escuro — o app agora pode ser claro. Continua porque o tema do gráfico é
#' do PROJETO e o do app é preferência de quem usa: o padrão não tem como seguir
#' o app sem que o mesmo documento renderize diferente para duas pessoas. E
#' mudá-lo recalcularia todo gráfico de todo documento existente. Quem quer
#' claro declara `tema_padrao: "claro"` no projeto.
#'
#' Duas entradas, um caminho: pelo card, o plano já resolveu e `tema` chega
#' como lista; no console, chega um nome. O nome passa por `trama::tr_theme()`,
#' e nome que não existe é ERRO aqui — quem digita `"escurro"` no console quer
#' saber, e não receber o escuro calado. Já a lista com `ausente` desenha
#' normalmente: o documento não quebra por cosmético, e avisar é do card.
#'
#' Cada cor é aplicada nos elementos que a base pinta. As bases `bw` e
#' `classic` ganham borda e linha de eixo na cor `eixos`: com o preto delas,
#' um fundo escuro esconderia justamente o que as distingue da `minimal`.
#'
#' `geom = element_geom(...)` é o que pinta as marcas SEM mapeamento de cor: no
#' ggplot2 4.0 a cor padrão de ponto, linha, barra e caixa sai de `ink`,
#' `paper` e `accent` do tema, e o `theme_minimal` traz tinta preta e papel
#' branco. Sem isto, no tema escuro o disperso sem cor saía com pontos pretos
#' sobre fundo quase preto, e o boxplot com caixas brancas acesas. Tinta é a cor
#' do texto, papel é o fundo, e o destaque (a linha de tendência, por exemplo) é
#' a primeira cor da paleta.
#'
#' A paleta vai no TEMA (`palette.*`, ggplot2 >= 4.0), e não como escala
#' somada: escala explícita do gráfico (a `scale_fill_gradient2` da
#' correlação, o `scale_colour_manual` do ACF) vence sozinha, e o ggplot é
#' quem decide se o mapeamento é discreto ou contínuo — inclusive para
#' `after_stat()`, que nenhuma inspeção do `aes` enxergaria. Paleta discreta
#' com menos cores que níveis repete as cores, em vez de pintar `NA`.
#' @noRd
.tr_view_tema <- function(tema) {
  t <- trama::tr_theme(tema)
  if (!is.list(tema) && isTRUE(t$ausente)) .tr_view_option("tema", tema, trama::tr_theme_names())
  base <- switch(t$base, minimal = ggplot2::theme_minimal, bw = ggplot2::theme_bw,
                 classic = ggplot2::theme_classic)
  fundo <- ggplot2::element_rect(fill = t$fundo, colour = NA)
  paleta <- t$paleta
  discreta <- function(n) rep_len(paleta, n)
  continua <- .tr_view_continua(t)
  th <- base(base_size = t$tamanho, base_family = t$fonte) + ggplot2::theme(
    plot.background = fundo, panel.background = fundo,
    legend.background = fundo, legend.key = fundo,
    panel.grid = ggplot2::element_line(colour = t$grade),
    text = ggplot2::element_text(colour = t$texto),
    axis.text = ggplot2::element_text(colour = t$eixos),
    strip.text = ggplot2::element_text(colour = t$texto),
    geom = ggplot2::element_geom(ink = t$texto, paper = t$fundo, accent = t$paleta[[1]]),
    palette.colour.discrete = discreta, palette.fill.discrete = discreta,
    palette.colour.continuous = continua, palette.fill.continuous = continua)
  switch(t$base,
    bw = th + ggplot2::theme(panel.border = ggplot2::element_rect(colour = t$eixos, fill = NA),
                             axis.ticks = ggplot2::element_line(colour = t$eixos)),
    # `classic` não tem grade: o `panel.grid` comum acima a traria de volta, e
    # o resultado seria um `bw` sem borda em vez do clássico.
    classic = th + ggplot2::theme(axis.line = ggplot2::element_line(colour = t$eixos),
                                  axis.ticks = ggplot2::element_line(colour = t$eixos),
                                  panel.grid = ggplot2::element_blank()),
    th)
}

#' A escala contínua do tema, como vetor de cores que o ggplot interpola.
#'
#' `divergente` passa pela cor dos `eixos` no meio, e não por um cinza fixo:
#' cinza claro fixo acenderia no escuro, cinza escuro fixo acenderia no claro.
#' Não é a cor da `grade`, que parecia a escolha natural ("o meio some no
#' fundo"): num gráfico de pontos, some o PONTO — o valor do meio da faixa
#' ficava invisível no tema escuro. As pontas azul e
#' vermelha são as mesmas da correlação da `multi`. O meio fica no meio da
#' FAIXA dos dados, não no zero: centrar no zero é decisão do gráfico, que
#' declara `scale_*_gradient2(midpoint = 0)` e vence o tema.
#' @noRd
.tr_view_continua <- function(t) {
  switch(t$continua,
    viridis = scales::pal_viridis(option = "D")(9),
    magma   = scales::pal_viridis(option = "A")(9),
    cividis = scales::pal_viridis(option = "E")(9),
    azuis   = scales::pal_brewer("seq", "Blues")(9),
    divergente = c("#2563eb", t$eixos, "#dc2626"))
}

#' O funil por onde todo nó de gráfico sai.
#'
#' Aplica tema, rótulos e legenda, e PENDURA a dimensão no objeto como
#' atributo. O atributo existe porque o `preview` do tipo recebe só o VALOR —
#' ele não vê os params do nó. Sem ele, o tipo não teria como saber em que
#' proporção renderizar, e a escolha do usuário morreria entre o `fn` e a
#' imagem. Atributo sobrevive ao `saveRDS`, e um ggplot com atributo a mais
#' continua um ggplot comum no console.
#'
#' Título e rótulo em branco são DESLIGADO, não erro: é a doutrina do param
#' vazio da casa, e aqui o comportamento honesto do vazio existe — o ggplot cai
#' no nome da coluna, que é a legenda certa em quase todo caso.
#' @noRd
.tr_view_acabar <- function(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda) {
  if (!legenda %in% .TR_VIEW_LEGENDAS) .tr_view_option("legenda", legenda, .TR_VIEW_LEGENDAS)
  p <- p + .tr_view_tema(tema) + ggplot2::theme(
    legend.position = switch(legenda, direita = "right", abaixo = "bottom", nenhuma = "none"))
  if (nzchar(trimws(titulo)))   p <- p + ggplot2::labs(title = titulo)
  if (nzchar(trimws(rotulo_x))) p <- p + ggplot2::labs(x = rotulo_x)
  if (nzchar(trimws(rotulo_y))) p <- p + ggplot2::labs(y = rotulo_y)
  attr(p, "tr_view_dim") <- .tr_view_dim(aspecto)
  p
}
