## Descrição

Monta o material suplementar do fluxo: um pacote [RO-Crate](https://w3id.org/ro/crate/1.1) com o fluxo, os itens ligados (tabelas, figuras, modelos) e uma página onde quem visita vê os resultados e usa os modelos no próprio navegador, via WebR.

Ligue à entrada **Itens** o que quer publicar. Tabela, gráfico e modelo entram direto: o fio converte cada um num item de publicação. O modelo entra **reduzido**, só com o que prevê, sem os dados de treino; classe de modelo que não sabe se reduzir não entra, e o card diz por quê.

Rodar o fluxo nunca publica. O bloco só confere e prepara; o botão **Visualizar** do card monta a pasta do pacote em `.trama/publicar/`, renderiza a página com o Quarto instalado e a abre numa aba, exatamente como sairia.

## Parâmetros

- **Título**, **Descrição**, **Palavras-chave** (separadas por vírgula) — vão para a página, o `README.md`, o `CITATION.cff` e o `ro-crate-metadata.json`.
- **Autores (ORCID)** — `Nome Sobrenome (0000-0000-0000-0000)`, separados por `;`. O ORCID é conferido pelo dígito verificador.
- **Licença** — do conteúdo (padrão CC-BY-4.0). **Licença do código** — do `flow.json` e do código R (padrão MIT).
- **Repositório** — nome ou endereço do repositório da publicação (usado no envio ao GitHub).
- **Próxima versão** — menor, correção ou maior.
- **Layout da página** — a ordem dos blocos: cabeçalho, textos em markdown, os itens (pelo card de origem) e o diagrama do fluxo. Vazio, a página tem o cabeçalho, cada item na ordem dos fios e o diagrama. Item ligado que fica fora do layout vai só para o pacote.

## Valor

O material do pacote: itens, metadados, layout e versões. O card mostra a conferência (itens, tamanhos, avisos e erros) e o botão Visualizar.

## Exemplos

Ligue a tabela de dados, o gráfico de dispersão e o `models/lm` a Publicar, preencha título e autores, e aperte Visualizar: a página mostra a tabela, a figura e um formulário que prevê com o modelo no navegador.

## Veja também

`models/lm`, `models/glm` e os modelos da coleção ml entram reduzidos (`tr_models_strip()`); FIGS ainda não se publica.
