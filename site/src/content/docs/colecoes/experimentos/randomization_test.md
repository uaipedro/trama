---
title: "Teste de aleatorização"
description: "Re-sorteia a alocação pela receita do delineamento, mantendo a resposta de cada unidade, e situa o F observado na distribuição do sorteio."
section: colecoes
collection: experimentos
node: experiments/randomization_test
category: "Avaliar"
related: [experiments/design, experiments/power, experiments/contrasts, models/anova_table]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

O **teste de aleatorização** de Fisher: a justificativa do teste vem do
sorteio que foi de fato feito, e não da normalidade. Sob a hipótese nula
**exata** — o tratamento não muda a resposta de nenhuma unidade —, cada
unidade teria dado a mesma resposta com qualquer outra alocação que o sorteio
poderia ter produzido. O bloco então re-sorteia a alocação **pela mesma
receita** do `experiments/design` (`tr_experiments_randomize`), mantém a
resposta de cada unidade, recalcula a estatística e situa a observada nessa
distribuição.

O conjunto de re-sorteios é o das alocações **admissíveis** do delineamento,
não o de todas as permutações: no DBC o tratamento só troca dentro do bloco;
na parcela subdividida a parcela só troca de lugar com parcela do mesmo bloco
e a subparcela dentro da parcela; no quadrado latino sai outro quadrado. A
tabela repete o escopo de cada fator.

### Estatística

O F do **Termo** no quadro de ANOVA da análise (a do plano, ou outra), ou o F
(1 gl, bilateral) de uma linha de contraste do `experiments/contrasts`.

### p-valor

- **exato** — enumera todas as alocações admissíveis (DIC, DBC e fatorial
  neles, até 100 000) e dá a proporção com estatística ≥ a observada; a
  observada está entre elas.
- **Monte Carlo** — com R re-sorteios e b deles ≥ a observada,
  p = (b + 1)/(R + 1), que nunca é zero e tem nível exato (Phipson e Smyth,
  2010).
- **automático** — exato quando a estrutura é enumerável e há até
  **Re-sorteios** alocações; Monte Carlo fora disso.

Empates contam como "≥" (tolerância relativa de 10⁻⁸). A tabela traz também
o p do quadro (o do F de Snedecor): em dados normais os dois ficam perto
(Pitman, 1938).

## Pressupostos

Só a aleatorização: que a alocação observada saiu do sorteio descrito no plano
e que, sob H0, a resposta de cada unidade não depende do tratamento que ela
recebeu. Não pede normalidade nem variâncias iguais. A hipótese testada é a
nula exata (efeito zero em toda unidade), mais forte que a igualdade de
médias.

## Parâmetros

- **Resposta** — em branco, a do plano. Com **dados** ligado, a tabela precisa
  de `unidade` (a do plano) e da resposta; se trouxer as colunas de
  tratamento, elas têm de ser a alocação do plano.

- **Análise (nó de models)** — em branco, a do plano (`plano$analise`, a que o
  `experiments/error` sugeriu). Outro id (`models/anova_factorial`) troca a
  análise; os params dela vêm de **Params da análise**.
- **Params da análise** — `nome = valor; nome = valor`, sobrescrevendo os do
  plano. A resposta é posta sozinha quando o nó tem `resposta`.
- **Termo testado** — a linha do quadro de ANOVA (`models/anova_table`, SQ
  tipo I) cujo p conta: `irrigacao`, `irrigacao:variedade`. Em branco, o
  primeiro fator de tratamento do plano.
- **Contraste: conjunto** — `nenhum` testa o F do termo; um conjunto do
  `experiments/contrasts` testa o F (1 gl) de uma linha dele, com o **Termo**
  como fator contrastado.
- **Contraste: linha** — o nome da linha (`linear`, `quadratico`).
- **Contrastes (digitados)**, **Controle**, **Doses** — como no
  `experiments/contrasts`.

- **Re-sorteios** — R do Monte Carlo (999 é o usual).
- **Método** — `automático`, `monte carlo` ou `exato`.

## Valor

`out`: o histograma da estatística sob re-sorteio, com a observada marcada.
`tabela`: `teste`, `estatistica`, `observado`, `p_valor`, `metodo`,
`alocacoes` (avaliadas), `admissiveis` (quantas há, quando enumerável),
`maiores_ou_iguais`, `falhas`, `p_parametrico` (o do quadro), `analise` e
`escopo`. `distribuicao`: a estatística de cada alocação.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "dbc", fatores = "t: A, B", repeticoes = 5L) |>
  tr_add("mu", "experiments/effect", tipo = "intercepto", valor = 10, from = "plano") |>
  tr_add("t", "experiments/effect", tipo = "fixo", fator = "t", efeitos = "A = 0, B = 1", from = "mu") |>
  tr_add("y", "experiments/error", sd = 1, from = "t") |>
  tr_add("ta", "experiments/randomization_test", from = "y")
```

## Referências

- Fisher, R. A. *The Design of Experiments*. Edinburgh: Oliver and Boyd,
  1935. (Cap. III: os dados de Darwin em *Zea mays*.) (edição a
  conferir no catálogo)
- Pitman, E. J. G. Significance tests which may be applied to samples from
  any populations. *Supplement to the Journal of the Royal Statistical
  Society*, v. 4, n. 1, p. 119–130, 1937. DOI: 10.2307/2984124.
- Pitman, E. J. G. Significance tests which may be applied to samples from
  any populations. III. The analysis of variance test. *Biometrika*, v. 29,
  n. 3/4, p. 322–335, 1938. DOI: 10.2307/2332008.
- Edgington, E. S.; Onghena, P. *Randomization Tests*. 4. ed. Boca Raton:
  Chapman & Hall/CRC, 2007. (ISBN a conferir no catálogo)
- Hinkelmann, K.; Kempthorne, O. *Design and Analysis of Experiments*, v. 1.
  2. ed. Hoboken: Wiley, 2008. (Aleatorização e análise pela
  aleatorização.) (a conferir no catálogo)
- Phipson, B.; Smyth, G. K. Permutation p-values should never be zero.
  *Statistical Applications in Genetics and Molecular Biology*, v. 9, n. 1,
  2010. DOI: 10.2202/1544-6115.1585.
- Hothorn, T.; Hornik, K.; van de Wiel, M. A.; Zeileis, A. Implementing a
  class of permutation tests: the coin package. *Journal of Statistical
  Software*, v. 28, n. 8, 2008. DOI: 10.18637/jss.v028.i08. (O oráculo dos
  testes.)

## Veja também

`experiments/design` (a receita do sorteio), `experiments/power`,
`experiments/contrasts`, `models/anova_table`.

### Aparência (comum a todos os gráficos)

- **Proporção** — a forma da imagem: `16:9` e `2:1` para paisagem, `1:1` para
  quadrado, `4:3` e `3:4` para o que vai numa página. A imagem sai sempre com
  1600 px no lado maior; o card só a escala, e um clique a abre em tela cheia.
  Mudar a proporção RECOMPUTA o gráfico — é o único param de aparência que
  muda mesmo o desenho, porque o ggplot recoloca a legenda e remede os rótulos.
  Arrastar a alça do card, não: aquilo é tamanho, não proporção.
- **Tema** — `padrão` segue o tema padrão do projeto; os temas (fundo, cores,
  fonte, paleta) são do projeto, e não do gráfico. Sem temas próprios valem os
  embutidos, com `escuro` como padrão; `claro` e `clássico` servem bem ao que
  sai no relatório. O tema dos gráficos não segue o claro/escuro do editor, que
  é preferência de cada pessoa: para mudar todos de uma vez, troque o tema
  padrão em ⚙ Configurações. A paleta só entra onde o
  gráfico não escolheu cores por conta própria.
- **Título**, **Rótulo do X**, **Rótulo do Y** — em branco, o gráfico usa o
  nome da coluna, que costuma ser a legenda certa.
- **Legenda** — `nenhuma` quando a cor já está explicada no título.


### O sorteio

O bloco é **estocástico** no Monte Carlo: a semente é do card, e não da
sessão; a mesma semente dá os mesmos re-sorteios e o mesmo p. O exato não
sorteia. A semente do console (`set.seed()`) não é tocada.

