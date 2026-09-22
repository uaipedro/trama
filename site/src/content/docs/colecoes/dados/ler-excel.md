---
title: Ler Excel
description: Leia uma aba de uma pasta de trabalho como tabela.
section: colecoes
collection: dados
node: data/read_excel
category: fonte
order: 7
related: [data/clean_names, data/summary]
---

## O que o bloco faz

Lê a aba indicada de um arquivo Excel e a entrega como tabela. Os nomes e tipos das colunas vêm do conteúdo da planilha.

## Quando usar

Quando a origem é uma planilha Excel com uma aba que contém registros.

## Configuração

**Arquivo** (`path`) indica a pasta de trabalho. **Planilha** (`sheet`) aceita o nome da aba ou sua posição numérica, como `"1"`; o pacote opcional `readxl` executa a leitura.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

arquivo <- readxl::readxl_example("datasets.xlsx")
tr_flow(reg) |>
  tr_add("ler", "data/read_excel", path = arquivo, sheet = "mtcars") |>
  tr_add("perfil", "data/summary", from = "ler")
```

## Como interpretar

O resultado contém as linhas da aba escolhida. Siga com `data/clean_names` quando cabeçalhos têm espaços ou acentos.
