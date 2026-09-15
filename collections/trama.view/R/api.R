# A costura da `view`, exportada para OUTRAS coleções.
#
# Uma coleção que desenha gráfico (séries temporais, experimentos) tem duas
# saídas ruins: declarar um segundo tipo de gráfico, com outro preview e outro
# tema, e o usuário passa a ter dois tipos de card para a mesma coisa; ou
# copiar tema, proporção e ggsave daqui, e as cópias divergem na primeira
# mudança de paleta. Exportar o funil é a terceira saída: o gráfico de outra
# coleção sai no MESMO tipo `view/plot`, com o mesmo tema, a mesma proporção e
# a mesma seção de ajuda — e mudar qualquer um deles aqui muda todos.
#
# São invólucros finos sobre os internos, e não renomeação deles, para que o
# contrato público tenha nome em inglês como o resto das funções exportadas
# (`tr_points`, `tr_view_errors`) sem forçar a troca de nome no código que já
# existe.

#' Os seis params cosméticos de todo gráfico, DEPOIS dos próprios.
#'
#' Para nó de outra coleção que declara saída `view/plot`: os params do nó
#' vêm em `...`, e os cosméticos são acrescentados no fim, na ordem que o card
#' desenha. O `fn` do nó precisa aceitar `aspecto`, `tema`, `titulo`,
#' `rotulo_x`, `rotulo_y` e `legenda` e repassá-los a [tr_view_finish()].
#' @param ... params próprios do nó, nomeados.
#' @return lista nomeada de `tr_param`.
#' @export
tr_view_props <- function(...) .tr_view_props(...)

#' Aplica tema, rótulos, legenda e proporção a um ggplot.
#'
#' É o funil por onde sai todo gráfico da `view`. Pendura a dimensão no
#' objeto (atributo `tr_view_dim`), que é o que o preview do tipo `view/plot`
#' lê para renderizar na proporção escolhida — sem passar por aqui, um
#' ggplot guardado como `view/plot` sai sempre em 16:9.
#' @param p um ggplot.
#' @param aspecto,titulo,rotulo_x,rotulo_y,legenda os cosméticos de
#'   [tr_view_props()].
#' @param tema o nome de um tema (`"padrão"`, `"claro"`...), como no console,
#'   ou a definição já resolvida que o `fn` de nó recebe do plano (ver
#'   [trama::tr_theme()]). Nome que não existe é erro.
#' @return o ggplot acabado.
#' @export
tr_view_finish <- function(p, aspecto = "16:9", tema = "padrão", titulo = "",
                           rotulo_x = "", rotulo_y = "", legenda = "direita") {
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Grava o PNG HD de um ggplot e devolve o preview `trama/image`.
#'
#' Para o `preview` de um TIPO de outra coleção cujo card é um gráfico (uma
#' previsão, uma decomposição): o mesmo PNG de 1600 px no lado maior que o
#' `view/plot` grava, com o mesmo lightbox. Passe o gráfico por
#' [tr_view_finish()] antes, ou ele sai em 16:9 sem tema.
#' @param p um ggplot.
#' @param ctx o contexto do preview (precisa de `ctx$file()`).
#' @return um `tr_preview`.
#' @export
tr_view_render <- function(p, ctx) {
  d <- attr(p, "tr_view_dim") %||% c(8, 4.5)
  f <- ctx$file("png")
  # `ragg::agg_png` e não o device padrão: o `grDevices::png` depende de
  # X11/quartz e simplesmente não existe em servidor headless, que é onde
  # um worker roda. `dpi = 200` com lado maior de 8 pol dá os 1600 px.
  # Sem `bg =`: o fundo do PNG é o `plot.background` do tema, e todo tema de
  # `.tr_view_tema()` pinta o seu (medido: o canto do escuro é #11151c com
  # alfa 1). Passar `bg = "transparent"` não fazia nada — desde o ggplot2
  # 3.4 esse JÁ é o default do `ggsave`, então nem o tema futuro que não
  # pinte fundo precisaria dele. Argumento que não muda nada é pior que
  # ausente: o próximo leitor gasta tempo procurando o efeito.
  ggplot2::ggsave(f, plot = p, width = d[[1]], height = d[[2]], units = "in",
                  dpi = 200, device = ragg::agg_png)
  trama::tr_preview("trama/image", files = list(png = f))
}

#' A seção "Aparência" da ajuda, comum a todo gráfico.
#'
#' Para anexar ao `help` de nó de outra coleção que usa [tr_view_props()]:
#' os seis params são os mesmos, então a página que os explica também é.
#' @return string markdown.
#' @export
tr_view_help_appearance <- function() .TR_VIEW_AJUDA_APARENCIA
