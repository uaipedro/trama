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
  "
### Como ler o card do teste

O topo diz o teste, as estrelas e a hipótese nula (**H0**). No meio, o número
grande e o **selo** da decisão ao nível de 5%: preenchido quando rejeita H0,
vazado quando não rejeita. Embaixo, a conclusão em uma linha.

**Quando o teste tem p-valor**, o número grande é o p-valor (abaixo de 1 em mil,
escrito em potência de dez) e embaixo dele vem a **régua**:

- a régua é o p-valor em escala logarítmica: quanto MAIS COMPRIDA a barra,
  MENOR o p-valor e mais forte a evidência contra H0. A ponta marca onde o
  p-valor está;
- as marcas são os cortes de 10%, 5%, 1% e 1 em mil; a barra enche de vez
  abaixo de 1 em dez mil;
- as **estrelas** são as do `summary()` do R: `***` abaixo de 1 em mil, `**`
  abaixo de 1%, `*` abaixo de 5%, `.` abaixo de 10% e `ns` acima. A cor da barra
  fica mais forte a cada estrela; sem estrela, cinza.

**Quando o teste só tem tabela de valores críticos** (sem p-valor), o número
grande é a estatística, e no lugar da régua vêm **três pontinhos**, dos cortes
de 10%, 5% e 1%:

- **preenchido** — a estatística passa do valor crítico daquele nível, na cauda
  que o teste usa;
- **na cor de destaque** — a decisão a 5% é rejeitar H0; **cinza** — não é: um
  ponto cinza preenchido é um teste que vence só o corte de 10%;
- **vazio** — não passa daquele corte.

Quantos pontos acendem diz a FOLGA da decisão, não uma decisão diferente.

A vista **detalhe** traz o registro inteiro: estatística, graus de liberdade,
p-valor ou valores críticos, o tamanho do efeito com o intervalo de 95%
desenhado contra a referência (quando o teste tem um), as partes de um teste
conjunto, a nota e a fonte.

Não rejeitar H0 não é provar H0: com poucas observações o teste deixa de
rejeitar por falta de poder. A conclusão diz \"não há evidência\", e não \"é
igual\", por isso.
"
}
