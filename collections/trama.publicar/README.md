# trama.publicar

Coleção do trama que publica um fluxo: o nó `publicar/publish` monta um pacote
[RO-Crate](https://w3id.org/ro/crate/1.1) com o fluxo, os itens ligados e uma
página Quarto em que os modelos preveem no navegador (WebR). A ação
**Visualizar** do card mostra a prévia local. Desenho em
`docs/visao-publicar-e-github.md`.

## Termos do `ro-crate-metadata.json`

Além do schema.org e do [FAIR4ML](https://w3id.org/fair4ml), o pacote usa dois
termos próprios, com o prefixo `trama:`:

- <a id="contemDadosTreino"></a>`trama:contemDadosTreino` — `true` no arquivo
  de modelo que carrega linhas do treino (vetores de suporte da SVM, folhas de
  uma linha da floresta de regressão). Só aparece quando quem publicou ligou
  "Aceito publicar dados de treino embutidos"; sem isso o modelo não entra.
- <a id="dadosTreinoEmbutidos"></a>`trama:dadosTreinoEmbutidos` — o texto que
  diz o que sobra do treino nesse arquivo.
