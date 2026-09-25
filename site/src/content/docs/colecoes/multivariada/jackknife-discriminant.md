---
title: Jackknife da discriminante
description: Estima estabilidade de correlações canônicas, autovalores ou coeficientes padronizados da LDA.
section: colecoes
collection: multivariada
node: multi/jackknife_discriminant
related: [multi/discriminant, multi/discriminant_functions, multi/confusion]
---

## O que o bloco faz

O bloco `multi/jackknife_discriminant` refaz LDA sem cada observação e estima incerteza das correlações canônicas, autovalores ou coeficientes padronizados. O bloco recebe `multi/lda`.

## Quando usar

Use **Jackknife da discriminante** para verificar se observações individuais alteram os parâmetros das funções discriminantes.

## Configuração

- **Estatística** — `correlação canônica` (padrão), `autovalores` ou `coeficientes padronizados`.
- **Tabela** — `resumo` (padrão) ou `pseudovalores`.
- **Confiança do intervalo** (`confianca`) — 0,95 por padrão, entre 0,5 e 0,999. Até a versão 1 do bloco o param se chamava `nivel`; fluxo salvo com `nivel` abre migrado.
- **Grupo (apagar-um-grupo)** — em branco (padrão), tira uma linha por vez. Com dados em conglomerados (várias linhas do mesmo talhão, animal ou lote), informe a coluna do conglomerado: cada réplica tira o grupo inteiro e o erro padrão usa o número de grupos G no lugar de n, com intervalo t(G − 1) — a variância JK1 de amostragem (Shao & Tu, 1995; Kott, 2001). Viés, corrigida e pseudovalores com G só valem com grupos do mesmo tamanho: com tamanhos diferentes saem NA, o intervalo centra na estimativa da amostra toda e a coluna `nota` diz por quê. Com menos de 5 grupos o bloco avisa (o EP tem poucos graus de liberdade). Os pseudovalores saem um por grupo.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "iris") |>
  tr_add("lda", "multi/discriminant", grupo = "Species", from = "dados") |>
  tr_add("jk", "multi/jackknife_discriminant", from = "lda")
```

O resumo apresenta variabilidade das funções discriminantes ao retirar cada observação.

## Como interpretar

A tabela resumo estima variabilidade; pseudovalores mostram retiradas influentes. O bloco requer modelo linear. Para estabilidade do acerto, use a validação cruzada de `multi/confusion`.

## Veja também

- [`Discriminante`](/trama/colecoes/multivariada/discriminant/)
- [`Funções discriminantes`](/trama/colecoes/multivariada/discriminant-functions/)
- [`Matriz de confusão`](/trama/colecoes/multivariada/confusion/)
