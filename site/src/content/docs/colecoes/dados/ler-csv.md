---
title: Ler CSV
description: Inicie um fluxo com uma tabela delimitada armazenada no projeto.
section: colecoes
collection: dados
node: data/read_csv
category: fonte
order: 2
related: [data/summary, data/filter]
---

## Finalidade

`data/read_csv` lê um arquivo de texto delimitado e produz uma tabela. Caminhos
relativos são resolvidos a partir da pasta do projeto.

## Configuração

**Arquivo** recebe o caminho, como `dados/vendas.csv`. **Separador** informa o
caractere entre campos; planilhas exportadas em português costumam usar `;`.
**Marcas de faltante** acrescenta textos que também significam ausência, como
`NA, -, sem dado`.

> **Antes de continuar**
>
> Uma célula vazia já é tratada como faltante. O campo de marcas serve para
> valores escritos que, naquele arquivo, também representam ausência; ele não
> converte todos os textos vazios em um valor novo.

## Resultado

O bloco devolve uma tabela com uma linha por linha do arquivo. Cada coluna tem
um tipo deduzido do conteúdo. Número escrito como `1.234,56` pode chegar como
texto e ser convertido depois com `data/convert`.

```r
tr_flow(reg) |>
  tr_add("ler", "data/read_csv", path = "dados/vendas.csv",
         delim = ";", na = "NA, -, sem dado") |>
  tr_add("perfil", "data/summary", from = "ler")
```

## Condições de leitura

O nó confere a impressão digital do arquivo. Se o CSV for alterado no disco, o
fluxo recalcula a partir da leitura em vez de reutilizar um resultado anterior.
