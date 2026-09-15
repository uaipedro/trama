# Nós de saída como nós ricos, com opções de salvar

> **Estado: ideia parada.** Levantada em 2026-09-09, durante o sprint da coleção
> `data`, ao decidir o comportamento de cache dos gravadores. Registrada pra não
> se perder; não é plano de implementação.

## A ideia

Hoje um nó de saída (`data/write_csv`, `data/write_rds`, `data/write_parquet`)
é uma caixa quase cega: recebe a tabela, grava no caminho do param, repassa
adiante. O card dele mostra o mesmo preview de tabela de qualquer outro nó —
não mostra nada sobre a *saída*, que é a razão dele existir.

A ideia é que o nó de salvar/exportar seja um **nó de visualização**: mostra o
que vai ser gravado e oferece as opções de gravação ali, no card, em vez de
espremê-las em campos de texto soltos.

## Por que isso não é só cosmético

O gravador é o único nó da coleção cujo resultado interessante **não está na
tabela que ele devolve**. Ele devolve a entrada intacta; o que ele produz de
fato é um efeito no disco. Então o card dele hoje mostra a coisa errada — a
tabela que já estava visível no nó anterior — e esconde a coisa certa: o
arquivo foi escrito? quando? com quantas linhas? qual o tamanho? existe?

Um card que respondesse isso resolveria de vez a confusão que motivou esta
nota: sem ele, não há como saber, olhando a tela, se o arquivo no disco
corresponde ao que o fluxo calculou.

## O que ficaria no card

Rascunho, não decidido:

- estado do arquivo: existe, quando foi escrito, tamanho, linhas
- botão de gravar agora (a gravação deixa de ser só efeito colateral da execução)
- escolha de formato ali, em vez de um nó por formato — `csv`/`rds`/`parquet`
  viram opção de um nó `data/export` só, e a paleta encolhe em vez de crescer
- opções por formato (separador, decimal, compressão) aparecendo conforme o
  formato escolhido

## As perguntas em aberto

**Um nó `export` com formato como param, ou um nó por formato?** É exatamente a
mesma pergunta que o desenho da coleção `distribution` deixou aberta ("um nó
por mecanismo, ou um nó por relação?"). Aqui pesa a favor do nó único que os
formatos compartilham quase todos os params; pesa contra que o `fn` deixa de
ter semântica fixa e passa a despachar sobre um param.

**Botão no card exige o quê do núcleo?** O front hoje manda ops (`set_param`,
`connect`, `move`) e recebe eventos de execução. "Executar este nó agora" não é
uma op de documento — é um comando. Pode ser que o transporte já dê conta
(`tr_rerun` existe), mas o alvo por nó não.

**Como o card sabe o estado do arquivo?** O preview é produzido pelo `preview`
do TIPO, e o tipo aqui é `data/table` — que não sabe nada sobre o arquivo. Ou o
gravador ganha tipo de saída próprio, ou o preview do nó passa a poder
sobrescrever o do tipo. A segunda opção mexe num dos cinco contratos do núcleo.

## Relação com o que já foi decidido

Os gravadores ficaram `volatile = TRUE` no sprint da coleção `data`: rodar o
fluxo sempre grava, porque um sink cujo arquivo sumiu não fez o trabalho dele.
Um card que mostrasse o estado do arquivo tornaria essa decisão *visível* em vez
de implícita — e talvez permitisse relaxá-la, já que a pessoa veria na tela que
o arquivo não está lá.
