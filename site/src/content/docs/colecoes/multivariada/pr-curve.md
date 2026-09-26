---
title: Curva precisão-revocação
description: Traça precisão contra revocação em todos os cortes e resume pela precisão média (AP), com validação cruzada.
section: colecoes
collection: multivariada
node: multi/pr_curve
related: [multi/roc, multi/confusion, multi/logistic]
---

## O que o bloco faz

O bloco `multi/pr_curve` plota, para cada corte de probabilidade, a revocação (fração dos positivos que a regra pega) contra a precisão (fração dos casos chamados de positivos que são positivos). Resume a curva pela precisão média (AP, Σ ganho de revocação × precisão) e pela área com a interpolação de Davis & Goadrich (2006), e marca a prevalência do positivo, que é a precisão de uma regra ao acaso. O bloco recebe `multi/classifier`.

## Quando usar

Use **Curva precisão-revocação** quando o grupo de interesse é raro: a ROC dilui os falsos positivos no grupo grande, e a precisão mostra quantos alarmes são falsos (Saito & Rehmsmeier, 2015).

## Configuração

- **Validação** — `cruzada` (padrão, deixa uma observação fora) ou `resubstituição`.
- **Aspecto**, **Tema**, **Título**, **Rótulo X**, **Rótulo Y** e **Legenda** — controlam a apresentação.

Com dois grupos, o segundo nível é o positivo, como na `multi/roc`. Com três ou mais, uma curva por grupo contra os outros, com AP e prevalência de cada na legenda.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "pima") |>
  tr_add("log", "multi/logistic", grupo = "diabetes", from = "dados") |>
  tr_add("pr", "multi/pr_curve", from = "log")
```

No `pima`, com as probabilidades de deixa-um-fora da logística com os sete preditores, AP = 0,721 e área de Davis & Goadrich = 0,719, contra o acaso de 0,333 (um terço tem diabetes).

## Como interpretar

Leia a AP contra a linha do acaso, não contra 0,5. AP e área interpolada estimam a mesma área de formas diferentes; reporte uma delas nomeada. Curvas de tabelas com prevalências diferentes não se comparam.

## Veja também

- [`Curva ROC`](/trama/colecoes/multivariada/roc/)
- [`Matriz de confusão`](/trama/colecoes/multivariada/confusion/)
- [`Regressão logística`](/trama/colecoes/multivariada/logistic/)
