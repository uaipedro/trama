---
title: Bootstrap
description: "Reamostra as linhas e reajusta o modelo: erro-padrão, intervalos percentil e BCa e estabilidade do sinal de coeficientes, médias ou diferenças."
section: colecoes
collection: modelos
node: models/bootstrap
category: resumir
related: [models/permutation, models/emmeans, models/coefficients]
---

## O que o bloco faz

`models/bootstrap` faz o bootstrap de casos: sorteia as linhas do ajuste com reposição, reajusta o modelo e recalcula a quantidade escolhida, tantas vezes quanto o número de **Reamostras**. O espalhamento das estimativas obtidas mede a incerteza delas. Recebe um `models/fit` e devolve um gráfico, uma tabela e a distribuição completa.

A quantidade pode ser os **coeficientes** do modelo, as **médias** marginais de um fator (as mesmas do `models/emmeans`) ou as **diferenças** entre todos os pares de médias. Nas médias e nas diferenças, a reamostragem é feita dentro de cada nível do fator, para que o número de repetições de cada tratamento fique fixo, como no experimento. Reamostras que perdem um nível ou ficam com posto incompleto são descartadas, e a tabela informa quantas foram.

## Quando usar

Quando a pergunta é quão estável é uma estimativa, e os intervalos teóricos dependem de pressupostos que não se sustentam bem, como normalidade dos erros com amostra pequena. Para perguntar se há efeito, o bloco indicado é o `models/permutation`.

## Configuração

- **Quantidade** — `coeficientes`, `médias` ou `diferenças`.
- **Fator das médias** — em branco, usa o primeiro fator do modelo.
- **Reamostrar dentro de** — estratos da reamostragem; em branco, o fator das médias (médias e diferenças) ou nenhum (coeficientes).
- **Reamostras** — o número B de reamostras (padrão 1999).
- **Confiança** — o nível dos intervalos.

O resultado depende do sorteio; a semente é a do card, e o mesmo card repete a mesma distribuição.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "PlantGrowth") |>
  tr_add("ajuste", "models/anova_dic", resposta = "weight", tratamento = "group", from = "dados") |>
  tr_add("boot", "models/bootstrap", quantidade = "diferenças", from = "ajuste")
```

## Como interpretar

A tabela traz, para cada quantidade, a estimativa, o viés e o erro-padrão bootstrap, o intervalo **percentil** e o **BCa**, que corrige viés e assimetria. O BCa fica instável com poucas reamostras: use 1999 ou mais. Nos coeficientes, `mesmo_sinal` é a fração de reamostras em que o coeficiente manteve o sinal da estimativa; perto de 1, o sentido do efeito é estável. A saída `distribuicao` tem um valor por reamostra e pode ir para um `view/histogram` ou para um `data/group_summarise`, se outro quantil interessar.
