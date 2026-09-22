---
title: Um caminho pelos dados
description: Leia, conheça e transforme tabelas em uma ordem que preserve as decisões da análise.
section: colecoes
collection: dados
order: 1
related: [data/read_csv, data/summary, data/filter, data/group_summarise]
---

## O trabalho antes da tabela final

Uma tabela raramente chega pronta para a pergunta. A coleção Dados organiza as
operações em uma ordem de trabalho: trazer o arquivo, conhecer o que ele contém,
tratar ausências ou tipos inadequados e então transformar ou resumir.

| Pergunta | Bloco | Resultado |
| --- | --- | --- |
| O que veio no arquivo? | `data/summary` | Uma linha por coluna |
| Quais linhas devem permanecer? | `data/filter` | Subconjunto de linhas |
| Qual medida existe por grupo? | `data/group_summarise` | Uma linha por grupo |

> **Antes de continuar**
>
> “Tabela” aqui significa um conjunto de linhas e colunas. Uma linha costuma
> representar uma observação; uma coluna, uma característica observada ou
> calculada. O significado específico depende da análise, não do formato.

## Uma sequência verificável

Depois da leitura, use **Resumo** antes de aplicar transformações. Tipos,
faltantes e valores distintos informam se uma coluna deve ser convertida,
limpa ou preservada como está. A tabela final mantém o caminho que a produziu.

```r
tr_flow(reg) |>
  tr_add("ler", "data/read_csv", path = "dados/vendas.csv", delim = ";") |>
  tr_add("perfil", "data/summary", from = "ler") |>
  tr_add("filtrar", "data/filter", expr = "valor > 0", from = "ler")
```
