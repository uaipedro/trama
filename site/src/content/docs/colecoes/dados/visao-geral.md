---
title: Um caminho pelos dados
description: Leia, inspecione, transforme e grave tabelas em uma ordem que preserve as decisões da análise.
section: colecoes
collection: dados
order: 1
related: [data/read_csv, data/summary, data/filter, data/group_summarise]
---

## Da fonte à saída

A coleção `trama.data` fornece tabelas ao fluxo, descreve sua estrutura, prepara valores, transforma linhas e colunas, combina fontes e grava resultados. A sequência depende da pergunta, mas uma inspeção após a leitura ajuda a escolher conversões e tratamentos de faltantes com base no conteúdo observado.

| Etapa | Nós | Decisão |
| --- | --- | --- |
| Ler ou criar | `data/read_csv`, `data/read_excel`, `data/read_json`, `data/read_rds`, `data/read_parquet`, `data/example`, `data/generate` | Escolha o formato da fonte ou produza dados simulados. |
| Inspecionar | `data/summary`, `data/get_dupes` | Verifique tipos, faltantes, extremos e chaves repetidas. |
| Preparar | `data/clean_names`, `data/remove_empty`, `data/distinct`, `data/rename`, `data/drop_na`, `data/replace_na`, `data/convert` | Ajuste nomes, duplicidades, ausências e tipos segundo o significado dos dados. |
| Transformar | `data/filter`, `data/mutate`, `data/select`, `data/arrange`, `data/slice_head`, `data/pivot_longer`, `data/pivot_wider` | Selecione registros, calcule colunas e ajuste a forma da tabela. |
| Resumir e combinar | `data/group_summarise`, `data/join`, `data/bind_rows` | Calcule medidas por grupo ou relacione/empilhe tabelas. |
| Processar por etapas | `data/to_stream`, `data/from_stream` | Delimite uma região que processa lotes em sequência. |
| Gravar | `data/write_csv`, `data/write_rds`, `data/write_parquet` | Escolha formato de intercâmbio, persistência R ou armazenamento colunar. |

## Inspeção orienta a preparação

`data/summary` apresenta uma linha por coluna, com tipo, faltantes, valores distintos, extremos e exemplo. No CSV, células vazias são faltantes; **Marcas de faltante** acrescenta códigos como `-` ou `n/d`. `data/convert` ajusta uma coluna que chegou como texto, enquanto `data/drop_na` e `data/replace_na` representam decisões diferentes sobre ausências.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("ler", "data/read_csv", path = "dados/vendas.csv",
         delim = ";", na = "NA, -, n/d") |>
  tr_add("perfil", "data/summary", from = "ler") |>
  tr_add("faltantes", "data/arrange", cols = "faltantes", desc = TRUE,
         from = "perfil")
```

## Linhas, medidas e forma

`data/filter` mantém registros conforme uma condição; `data/mutate` calcula valores para cada linha. `data/group_summarise` produz uma linha por combinação de grupos. `data/pivot_longer` empilha colunas em linhas e `data/pivot_wider` transforma categorias em colunas. Antes de espalhar dados, confira se as chaves identificam um único valor.

`data/join` relaciona duas tabelas por chave e pode multiplicar linhas quando há chaves repetidas em ambos os lados. `data/bind_rows` acrescenta registros de tabelas, alinhando colunas pelos nomes. Essas operações respondem a formatos de entrada distintos.

## Saída e lotes

Os nós `data/write_*` gravam a tabela e a repassam ao fluxo. CSV favorece intercâmbio, RDS preserva classes do R e Parquet usa formato colunar. `data/to_stream` e `data/from_stream` delimitam uma região de processamento por lotes; os nós intermediários operam sobre cada lote.
