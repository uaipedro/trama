---
title: GLS (erro correlacionado)
description: "Ajusta mínimos quadrados generalizados (nlme::gls): AR(1), simetria composta ou não estruturada no erro, variância por nível."
section: colecoes
collection: modelos
node: models/gls
category: ajustar
related: [models/lmer, models/anova_split_plot, models/compare]
---

## O que o bloco faz

`models/gls` ajusta uma regressão ou ANOVA por mínimos quadrados generalizados (`nlme::gls`), com o erro correlacionado dentro de cada grupo: AR(1), simetria composta ou não estruturada. Opcionalmente, cada nível de uma coluna tem variância própria (`varIdent`). A saída é `models/fit`.

## Quando usar

Em medidas repetidas no tempo: o mesmo animal, planta ou parcela medido várias vezes. A análise de parcela subdividida no tempo supõe esfericidade, que equivale a simetria composta. Com o GLS, essa estrutura vira uma escolha e pode ser comparada com outras.

## Configuração

Informe a fórmula dos efeitos fixos, a estrutura de correlação, o grupo (a unidade medida várias vezes) e, de preferência, o tempo, que ordena as ocasiões. Sem tempo, vale a ordem das linhas. **Variância por** é opcional.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("sono", "models/example", dataset = "sleepstudy") |>
  tr_add("gls", "models/gls", formula = "Reaction ~ Days", correlacao = "ar1",
         grupo = "Subject", tempo = "Days", from = "sono")
```

No `sleepstudy`, o AR(1) estima phi = 0,80 entre dias consecutivos e uma inclinação de 10,47 ms por dia (EP 1,70). Com simetria composta, a correlação é 0,59 e o AIC sobe de 1747,2 para 1794,5: o AR(1) descreve melhor esses dados.

## Como interpretar

Coeficientes e quadro são testes de Wald com gl n − p. Os resíduos do diagnóstico são os normalizados, já sem a correlação. Duas estruturas com o mesmo número de parâmetros (AR(1) e simetria composta) não são aninhadas: compare-as pelo AIC. Estruturas aninhadas (simetria composta dentro da não estruturada) vão ao `models/compare`. Validação: o bloco reproduz o `nlme` no `Ovary` (AR(1), phi 0,7532, logLik −780,7273), a igualdade entre simetria composta e o misto de intercepto aleatório, e o modelo não estruturado com variância por idade do `Orthodont`.
