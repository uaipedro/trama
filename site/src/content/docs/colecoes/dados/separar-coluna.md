---
title: Separar coluna
description: Separe uma coluna de texto em várias pelo separador.
section: colecoes
collection: dados
node: data/separate
category: transformar
order: 23
related: [data/unite, data/convert]
---

## O que o bloco faz

Parte uma coluna de texto (`parcela_A_1`) em várias (`local`, `tratamento`, `repeticao`) pelo **Separador**, literal. As colunas novas entram no lugar da original. Linha com número de partes diferente do número de nomes para o bloco, nomeando a linha.

## Configuração

**Coluna** (`variavel`), **Novas colunas** (`nomes`, separadas por vírgula), **Separador** (`separador`) e **Remover a original** (`remover`).

## Exemplo

```r
tr_add("sep", "data/separate", variavel = "codigo", nomes = "local, tratamento, repeticao", separador = "_", from = "ler")
```
