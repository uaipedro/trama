---
title: Ler CSV
description: Traga um arquivo CSV para o fluxo e transforme seus registros em uma tabela pronta para análise.
section: colecoes
collection: dados
node: data/read_csv
category: fonte
order: 2
related: [data/summary, data/group_summarise]
---

## O que o bloco faz

`data/read_csv` lê um arquivo de texto delimitado e cria uma tabela no fluxo.
Cada linha do arquivo se torna uma linha da tabela, e cada campo se torna uma
coluna. O tipo das colunas é reconhecido a partir dos valores encontrados.

O caminho pode ser relativo à pasta do projeto, como `dados/vendas.csv`, ou
absoluto. A tabela resultante pode seguir para blocos de inspeção, limpeza e
análise.

## Quando usar

Use **Ler CSV** quando os dados estiverem num arquivo de texto cujas colunas
são separadas por um caractere, como vírgula ou ponto e vírgula. Planilhas
exportadas em português costumam usar ponto e vírgula como separador.

## Configuração

**Arquivo** indica o caminho do CSV. **Separador** informa o caractere entre
campos: `,`, `;`, tabulação ou `|`. **Marcas de faltante** lista outros textos
que representam ausência, separados por vírgula, como `NA, -, sem dado`.

> **Antes de continuar**
>
> Células vazias já são reconhecidas como faltantes. Use **Marcas de faltante**
> para acrescentar códigos usados no arquivo, como `-` ou `sem dado`; assim,
> esses registros também ficam disponíveis para a contagem e o tratamento de
> ausências nas etapas seguintes.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

arquivo <- tempfile(fileext = ".csv")
writeLines(c("regiao;valor", "Norte;120", "Sul;NA"), arquivo)
tr_flow(reg) |>
  tr_add("ler", "data/read_csv", path = arquivo, delim = ";", na = "NA") |>
  tr_add("perfil", "data/summary", from = "ler")
```

A tabela lida tem duas linhas, com `regiao` como texto e `valor` como número;
`NA` é reconhecido como faltante. O perfil expõe essa ausência na coluna
`faltantes`.

## Como interpretar

O leitor infere o tipo de cada coluna pelo conteúdo. Números com formato
brasileiro, como `1.234,56`, podem ser reconhecidos como texto; confira o perfil
e converta a coluna para número com o bloco **Converter tipo** quando
necessário.

O fluxo registra caminho, tamanho e data de modificação do arquivo. Ao salvar
alterações no CSV, a leitura é atualizada e as etapas seguintes são recalculadas
a partir da nova tabela.
