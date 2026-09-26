---
title: "Efeito"
description: "Soma um termo à resposta simulada: intercepto, fixo (por nível ou por contraste), aleatório, interação, quantitativo ou covariável."
section: colecoes
collection: experimentos
node: experiments/effect
category: "Planejar"
related: [experiments/error, experiments/view, experiments/contrasts]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Soma **um** termo à resposta que o experimento vai ter, antes de ela existir.
A contribuição de cada unidade fica numa coluna própria, `.ef_<nome>`, e o
termo fica no plano com o seu **valor verdadeiro** — é contra ele que a
análise se confere depois. Encadeie um `experiments/effect` por termo e feche
com `experiments/error`. O plano que sai é o mesmo tipo que entra
(`experiments/plan`).

O **Tipo** diz quais params são lidos:

| Tipo | Lê | Contribuição |
|---|---|---|
| intercepto | Valor | μ em toda unidade |
| fixo | Fator, Efeitos **ou** Declarar por + Magnitudes | τᵢ do nível |
| aleatorio | Fator (`bloco` ou `bloco:parcela`), sd | um N(0, sd²) sorteado por nível (ou combinação) |
| interacao | Fator (`a:b`), Efeitos por célula **ou** Declarar por + Magnitudes | (τβ)ᵢⱼ da célula |
| quantitativo | Fator (numérico, codificado ou com Doses), Coeficientes | b₁x + b₂x² + … ; com `x1:x2`, b·x₁x₂ |
| covariavel | Fator (a coluna), Inclinação, Média, sd | β(x − média) |

### Por contraste

Com **Declarar por** = `polinomiais`, `helmert`, `controle` ou `digitados`, o
efeito fixo vem de um conjunto de contrastes e da **magnitude** de cada um. Os
conjuntos são os mesmos do `experiments/contrasts`, com os mesmos coeficientes
inteiros (para quatro doses igualmente espaçadas: linear `-3 -1 1 3`,
quadrático `1 -1 -1 1`, cúbico `-1 3 -3 1`).

**Escala:** a magnitude é o **valor do contraste** nesses coeficientes,
Σ cᵢ τᵢ — a mesma estimativa que o `experiments/contrasts` devolve com o
mesmo conjunto. "Linear = 4" com `-3 -1 1 3` dá τ = 4·c/20 =
(−0,6; −0,2; 0,2; 0,6). A conversão é τ = Lᵀ(LLᵀ)⁻¹m, que para contrastes
ortogonais é Σⱼ mⱼcⱼ/Σcⱼ²; os efeitos somam zero. Contraste não citado em
**Magnitudes** vale 0. A tabela da conversão (contraste, coeficientes,
magnitude, Σc², conferência Σcτ) fica em `plano$termos` e sai no `print` do
plano.

Na **interação**, `polinomiais` ou `helmert` são aplicados a cada fator e as
magnitudes são dos **produtos** (`Linear:alta vs baixa = 2`); os efeitos de
célula saem com margens nulas.

### Validação contra o plano

O fator tem de ser coluna do plano (a combinação `bloco:parcela` só existe
onde há parcela); o efeito por nível cobre todos os níveis, e a tabela da
interação, todas as células presentes no plano; o nome do termo não se repete;
nenhum termo depois de `experiments/error`. Efeito aleatório com um nível por
unidade avisa que se confunde com o resíduo.

## Pressupostos

Nenhum sobre dados: é simulação. O que se declara é o modelo verdadeiro; o
efeito aleatório é normal de média zero; o fixo é "soma zero" quando declarado
por contraste (e o que se digitar, quando por nível).

## Parâmetros

- **Tipo** — ver a tabela.
- **Fator(es)** — coluna(s) do plano: `irrigacao`, `bloco:parcela`, `dose:irrigacao`.
- **Valor (intercepto)**.
- **Efeitos por nível ou célula** — `baixa = 0, alta = 2`; na interação
  `baixa:A = 1, baixa:B = 0, ...`, com os níveis na ordem do Fator.
- **Declarar por** — `por nível` ou um conjunto de contrastes.
- **Magnitudes dos contrastes** — `linear = 4, quadratico = 1, cubico = 0`
  (nome sem caixa e sem acento) ou só os números, na ordem dos contrastes.
- **Doses** — valores numéricos dos níveis, na ordem (polinomiais e
  quantitativo). Com dois fatores: `dose: 0, 50, 100; irrigacao: 0, 1`.
- **Controle** — o nível controle (conjunto `controle`).
- **Contrastes (digitados)** — sintaxe do `models/linear_hypothesis`.
- **Desvio-padrão** — do efeito aleatório; ou da covariável, quando ela é
  gerada.
- **Coeficientes (quantitativo)** — `b1` ou `b1, b2` (linear, quadrática…), na
  escala de x (a codificada, no composto central).
- **Inclinação**, **Média (covariável)** — se a coluna da covariável está vazia
  (`NA`), ela é gerada normal com essa média e o sd; a contribuição é
  inclinação · (x − média).
- **Nome do termo** — em branco, vem do fator (`bloco_parcela`).

## Valor

O plano (`experiments/plan`) com a coluna `.ef_<nome>` nas unidades e o termo
em `plano$termos` (tipo, parâmetros, valor verdadeiro, conversão). A aba
`componentes` do `experiments/view` o desenha.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "dic", fatores = "dose: 0, 50, 100, 150") |>
  tr_add("mu", "experiments/effect", tipo = "intercepto", valor = 20, from = "plano") |>
  tr_add("dose", "experiments/effect", tipo = "fixo", fator = "dose", conjunto = "polinomiais",
         magnitudes = "linear = 4, quadratico = 1, cubico = 0", from = "mu")
```

## Referências

- Montgomery, D. C. *Design and Analysis of Experiments*. 9. ed. Hoboken:
  Wiley, 2017. (Contrastes ortogonais e polinômios ortogonais, cap. 3.)
- Gelman, A.; Hill, J. *Data Analysis Using Regression and
  Multilevel/Hierarchical Models*. Cambridge: Cambridge University Press, 2007.
  (Simulação de dados falsos para conferir a análise, cap. 8.)

## Veja também

`experiments/error` para fechar a resposta; `experiments/view` (aba
`componentes`); `experiments/contrasts` para recuperar o contraste declarado.

### O sorteio

O bloco é **estocástico**: a semente é do card, e não da sessão. O mesmo
documento sorteia sempre os mesmos valores; trocar a semente sorteia outros.
A semente fica no plano (no termo ou em `plano$resposta`). A semente do console
(`set.seed()`) não é tocada. Para repetir a cadeia inteira com uma semente só,
use `tr_experiments_simulate()` no console.

