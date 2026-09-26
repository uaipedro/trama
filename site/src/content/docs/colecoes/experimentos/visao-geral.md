---
title: Planejar, simular e analisar um experimento
description: Declare o delineamento, componha a resposta termo a termo, analise por contrastes e confira o erro tipo I da análise por simulação.
section: colecoes
collection: experimentos
order: 1
related: [experiments/design, experiments/effect, experiments/error, experiments/contrasts, experiments/view, experiments/power]
---

## A ideia

A coleção `experiments` trata o experimento em três momentos: **planejar** (declarar o delineamento, sortear e compor a resposta que ele vai ter), **analisar** (contrastes, Box-Cox, superfície de resposta) e **avaliar** (poder e teste de aleatorização). As ANOVAs continuam em `trama.models`: o plano, ligado a elas, vira tabela.

| Categoria | Blocos | Resultado |
| --- | --- | --- |
| Planejar | `experiments/design`, `experiments/view`, `experiments/effect`, `experiments/error` | plano sorteado, vistas do plano, resposta simulada |
| Analisar | `experiments/contrasts`, `experiments/boxcox`, `experiments/response_surface` | contrastes com SQ, λ sugerido, modelo de superfície |
| Avaliar | `experiments/power`, `experiments/randomization_test` | taxa de rejeição com IC, p-valor por re-sorteio |

## Delineamento declarativo

`experiments/design` recebe uma estrutura (DIC, DBC, quadrado latino, fatorial, parcela subdividida, faixas, BIB, fracionado, composto central, medidas repetidas, crossover…) e os fatores, e sorteia a alocação. O plano guarda para cada fator o papel, a unidade que o recebe, o escopo do sorteio e a semente. `experiments/view` mostra o mesmo plano em cinco abas: **mapa** (o croqui), **hierarquia** (quem é unidade de quê), **combinações**, **ordem** de execução e **componentes** da resposta.

## Resposta composta termo a termo

Antes de existir dado, a resposta é montada por uma cadeia de `experiments/effect`, um termo por card: intercepto, efeito fixo (por nível ou por contraste), efeito aleatório, interação, termo quantitativo ou covariável. Cada termo vira uma coluna `.ef_<nome>` com o seu valor verdadeiro. `experiments/error` soma os termos, sorteia o resíduo (normal, Poisson, binomial ou gama) e escreve a coluna de resposta. A aba **componentes** do `experiments/view` desenha essa soma, e a análise pode ser conferida contra o que foi declarado.

## Tudo é contraste

Um efeito fixo pode ser declarado pelos contrastes que ele contém, com a magnitude de cada um (polinomiais, Helmert, controle ou digitados). `experiments/contrasts` faz o caminho de volta: abre o SQ do tratamento em uma linha por contraste, com estimativa, erro padrão, F e p, e confere se os SQ somam o do tratamento (só um conjunto completo e ortogonal soma; `controle`, cada tratamento contra o controle, não é ortogonal). Os mesmos conjuntos e coeficientes servem aos dois lados, então a estimativa devolvida está na mesma escala da magnitude declarada.

## O caso da parcela subdividida

Numa parcela subdividida, a irrigação é sorteada às parcelas e a variedade às subparcelas dentro delas: há dois erros. Se a parcela tem variação própria (o termo aleatório `bloco:parcela`), a irrigação precisa ser testada contra o erro de parcela. A análise ingênua — o fatorial em blocos, `models/anova_factorial` — a testa contra o resíduo das subparcelas.

Medimos isso com `experiments/power`, com a irrigação **sem efeito** no modelo declarado (4 blocos, 2 irrigações, 3 variedades; bloco com sd 3, erro de parcela com sd 4, resíduo com sd 1), em 1000 réplicas:

| Modelo verdadeiro | Análise | Taxa de rejeição de H0 (IC 95%) |
| --- | --- | --- |
| com erro de parcela | ingênua (`models/anova_factorial`) | 40,8% (37,7–43,9%) |
| com erro de parcela | parcela subdividida (`models/anova_split_plot`) | 5,6% (4,8–6,5%), em 3000 réplicas¹ |
| sem erro de parcela | ingênua | 6,2% (4,8–7,9%) |
| sem erro de parcela | parcela subdividida | 4,1% (3,0–5,5%) |

¹ Em 1000 réplicas de uma semente saiu 7,2%; as 3000 (três sementes: 7,2%, 4,9% e 4,7%) dão 5,6%, e o teste binomial exato de "taxa = 5%" não rejeita (p = 0,13). O F da subdividida é o do `aov` com `Error(bloco:parcela)` (diferença de p < 1e-14), exato sob o modelo; a coluna `p_binomial` do `experiments/power` faz essa conferência.

Com erro de parcela, a análise ingênua rejeita uma H0 verdadeira em cerca de quatro de cada dez experimentos. O exemplo `exemplos/delineamentos` monta os dois casos lado a lado: plano → efeitos → erro → as duas ANOVAs → contrastes das variedades → componentes.

```r
library(trama)

reg <- tr_registry()
for (p in c("trama.data", "trama.view", "trama.models", "trama.experiments")) tr_use(p, registry = reg)

tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "parcela_subdividida",
         fatores = "irrigacao: baixa, alta; variedade: A, B, C", repeticoes = 4L) |>
  tr_add("mu", "experiments/effect", tipo = "intercepto", valor = 50, from = "plano") |>
  tr_add("bloco", "experiments/effect", tipo = "aleatorio", fator = "bloco", sd = 3, from = "mu") |>
  tr_add("irr", "experiments/effect", tipo = "fixo", fator = "irrigacao",
         efeitos = "baixa = 0, alta = 0", from = "bloco") |>
  tr_add("var", "experiments/effect", tipo = "fixo", fator = "variedade",
         conjunto = "controle", controle = "A", magnitudes = "0.5, 2.5", from = "irr") |>
  tr_add("parc", "experiments/effect", tipo = "aleatorio", fator = "bloco:parcela", sd = 4, from = "var") |>
  tr_add("y", "experiments/error", resposta = "producao", sd = 1, from = "parc") |>
  tr_add("poder", "experiments/power", analise = "models/anova_factorial",
         parametros = "fatores = irrigacao, variedade; bloco = bloco",
         termo = "irrigacao", replicas = 1000L, from = "y")
```

Sem `analise`, `experiments/power` usa a análise que `experiments/error` sugeriu para o plano — aqui, `models/anova_split_plot`.
