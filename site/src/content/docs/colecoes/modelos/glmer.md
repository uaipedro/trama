---
title: GLM misto
description: "Ajusta um GLM misto (lme4::glmer), binomial ou Poisson, com efeito por observação opcional."
section: colecoes
collection: modelos
node: models/glmer
category: ajustar
related: [models/glm, models/lmer, models/compare, models/random_effects]
---

## O que o bloco faz

`models/glmer` ajusta um modelo linear generalizado com efeitos aleatórios (`lme4::glmer`, máxima verossimilhança pela aproximação de Laplace). A resposta é binomial (0/1, ou sucessos em n tentativas com `cbind(sucessos, fracassos)`) ou Poisson (contagem). A saída é `models/fit`.

## Quando usar

Quando a resposta não é normal e as observações vêm em grupos (rebanho, bloco, ninhada, sujeito). Com superdispersão, ligue **Efeito por observação**: ele soma `(1 | .obs)`, e a variância que sobra além da binomial ou da Poisson vira um componente de variância (Harrison 2014).

## Configuração

Informe a fórmula com pelo menos um termo aleatório, ou resposta, efeitos fixos e grupo pelo atalho. Escolha a família.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("carrapatos", "models/example", dataset = "grouseticks") |>
  tr_add("gm", "models/glmer", resposta = "TICKS", fixos = "YEAR", grupo = "BROOD",
         familia = "poisson", nivel_obs = TRUE, from = "carrapatos")
```

Nos carrapatos de `grouseticks`, o efeito por observação tem variância 0,30 e a ninhada 1,52. Comparado no `models/compare` ao modelo sem ele, o AIC cai de 2038,6 para 1844,3 (razão de verossimilhança 196,3): a contagem é superdispersa.

## Como interpretar

Os coeficientes estão na escala da ligação (logit, log), com z de Wald; o `models/coefficients` os exponencia. As médias do `models/emmeans` são as do grupo típico (efeito aleatório zero), não médias populacionais. Não há resíduo normal a testar, e o quadro é de Wald, tipo II ou III. No exemplo `cbpp` do lme4, o bloco reproduz a saída publicada: interceptos −1,3983, −0,9919, −1,1282 e −1,5797, variância do rebanho 0,4123 e AIC 194,1.
