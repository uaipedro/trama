---
title: Regressão logística
description: Ajusta logística binária para dois grupos ou multinomial para três ou mais grupos.
section: colecoes
collection: multivariada
node: multi/logistic
related: [models/coefficients, models/plot_coefficients, models/roc]
---

## O que o bloco faz

O bloco `multi/logistic` ajusta logística binária para dois grupos ou multinomial para três ou mais e devolve um classificador com probabilidades previstas. O bloco recebe `data/table`.

## Quando usar

Use **Regressão logística** para modelar a probabilidade de uma classe em função de medidas observadas.

## Configuração

- **Resposta** — classe conhecida.
- **Preditores** — colunas numéricas; em branco, usa as numéricas menos a Resposta.
- **Corte (binária)** — 0,5 por padrão (intervalo 0,01–0,99); aplica-se à logística binária, para prever o segundo nível.
- **Método** — `ml` (máxima verossimilhança, padrão) ou `firth` (verossimilhança penalizada de Firth, só com dois grupos).

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "pima") |>
  tr_add("log", "multi/logistic", resposta = "diabetes", preditores = "glicose, imc, pedigree", from = "dados")
```

O classificador estima as probabilidades de diabetes para cada linha de `pima`.

## Como interpretar

Resposta escolhe a coluna do grupo; Preditores seleciona as variáveis numéricas; Corte define o limiar para o segundo grupo no caso binário e não se aplica à multinomial. Separação completa pode tornar coeficientes não finitos. Com `metodo = "firth"` eles ficam finitos e os intervalos do `models/coefficients` passam a ser perfilados.

### Firth com quase separação

Nos `vinhos`, só os cultivares B e C (71 e 48), com álcool, flavonoides e intensidade de cor, os dois grupos se separam e a ML não dá coeficientes. Com `metodo = "firth"`, o coeficiente dos flavonoides é −5,23 (IC 95% perfilado de −16,69 a −1,86; p = 0,00004) e o do álcool 8,35 (de 0,48 a 28,49; p = 0,033). Os intervalos são longos e assimétricos: é o que a pouca informação de dados quase separados permite dizer.

```r
tr_flow(reg) |>
  tr_add("v", "multi/example", dataset = "vinhos") |>
  tr_add("bc", "data/filter", expr = 'cultivar != "A"', from = "v") |>
  tr_add("lg", "multi/logistic", resposta = "cultivar", preditores = "alcool, flavonoides, intensidade_cor",
         metodo = "firth", from = "bc") |>
  tr_add("rc", "models/coefficients", exponenciar = TRUE, from = "lg")
```

## Veja também

- [`Coeficientes`](/trama/colecoes/modelos/coefficients/) (com `exponenciar`, as razões de chances)
- [`Gráfico dos coeficientes`](/trama/colecoes/modelos/plot-coefficients/) (com `exponenciar`, as razões de chances)
- [`Curva ROC`](/trama/colecoes/modelos/roc/)
