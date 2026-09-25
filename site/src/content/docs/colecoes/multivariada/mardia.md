---
title: Normalidade multivariada (Mardia)
description: Testa a normalidade multivariada pela assimetria e pela curtose de Mardia, na tabela toda ou dentro de cada grupo.
section: colecoes
collection: multivariada
node: multi/mardia
related: [multi/box_m, multi/discriminant, multi/kmo_bartlett]
---

## O que o bloco faz

O bloco `multi/mardia` calcula a assimetria b₁,ₚ e a curtose b₂,ₚ multivariadas de Mardia (1970), com o teste qui-quadrado da assimetria (também com a correção de amostra pequena de Mardia, 1974) e o teste normal da curtose. O bloco recebe `data/table` e devolve uma tabela.

## Quando usar

Use **Normalidade multivariada (Mardia)** antes da discriminante, do M de Box, da fatorial por máxima verossimilhança e da esfericidade de Bartlett, que supõem normalidade multivariada. Testar cada variável sozinha não basta.

## Configuração

- **Variáveis** — colunas numéricas; em branco, todas as numéricas menos o grupo.
- **Grupo (opcional)** — testa dentro de cada grupo, como a discriminante supõe.
- **Confiança** — a leitura rejeita quando p < 1 − confiança (padrão 0,95).

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("i", "multi/example", dataset = "iris") |>
  tr_add("m", "multi/mardia", grupo = "Species", from = "i")
```

No iris, com as quatro medidas e 50 flores por espécie, nenhum grupo rejeita a 5%: em setosa a assimetria é b₁,ₚ = 3,080 (χ² = 25,66 com 20 gl, p = 0,177; corrigida, p = 0,113) e a curtose b₂,ₚ = 26,54 (z = 1,29, p = 0,195). Em virginica a assimetria corrigida chega mais perto (p = 0,098).

Os p são assintóticos (qui-quadrado e normal com n grande) e pouco confiáveis em n pequeno. Com menos de 20 linhas na tabela ou num grupo o bloco avisa e aponta a linha de amostra pequena; 20 é a regra do pacote MVN para trocar para essa correção (código de `MVN::mardia`), não um limiar estabelecido na literatura.

## Como interpretar

Na normal multivariada, b₁,ₚ = 0 e b₂,ₚ = p(p + 2) (24 com quatro variáveis). Rejeitar qualquer das duas é evidência contra a normalidade. Não rejeitar não prova normalidade: com poucas linhas o teste tem pouco poder; com muitas, rejeita desvios pequenos.

## Veja também

- [`M de Box`](/trama/colecoes/multivariada/box-m/)
- [`Discriminante`](/trama/colecoes/multivariada/discriminant/)
- [`KMO e Bartlett`](/trama/colecoes/multivariada/kmo-bartlett/)
