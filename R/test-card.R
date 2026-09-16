#' A seção de ajuda que explica o card de teste de hipótese.
#'
#' O card `trama/test` é do núcleo e desenhado igual em toda coleção: régua do
#' p-valor com estrelas quando há p-valor, três pontinhos de nível quando o
#' teste só tem tabela de valores críticos. A explicação mora aqui, escrita UMA
#' vez, pelo mesmo motivo do componente: quinze páginas com quinze cópias
#' divergiriam na primeira mudança do renderer (`inst/www/runtime.js`,
#' `VereditoTeste`; regras em `inst/www/teste.js`).
#'
#' Os cortes vão por extenso ("1 em mil"), e não "0,001": a coleção `series`
#' confere que todo decimal escrito na ajuda de um teste existe como literal no
#' código do bloco, e o corte do CARD não é do bloco.
#'
#' Uma coleção que emite um tipo de teste registra o id dele apontando para o
#' renderer do núcleo (`registerRenderer("x/test", getRenderer("trama/test"))`)
#' e cola esta seção na ajuda de cada bloco de teste.
#' @return string markdown, começando por um título de terceiro nível.
#' @export
tr_help_test_card <- function() {
  .tr_msg("test_card.ajuda")
}
