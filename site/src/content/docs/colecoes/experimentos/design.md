---
title: "Delineamento"
description: "Declara o delineamento (fatores, blocos, unidades) e sorteia a alocação."
section: colecoes
collection: experimentos
node: experiments/design
category: "Planejar"
related: [experiments/view, models/anova_split_plot]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Declara um experimento e sorteia a alocação dos tratamentos às unidades. O
delineamento é uma descrição só, preenchida aqui; o que se aprende está em
VER o resultado em `experiments/view`: quem é unidade de quê e em que nível
cada fator foi sorteado.

Para cada fator o plano registra o **papel** (tratamento, bloco, tempo,
covariável observada), a **unidade de atribuição**, o **escopo do sorteio**
(entre quais unidades) e o **mecanismo** (livre, restrito, em estágios ou sem
sorteio, com a justificativa). Bloco, linha, coluna, local e tempo não são
sorteados: são fontes de variação controladas ou observadas.

As estruturas, e como cada uma sorteia:

- **dic** — inteiramente casualizado: os t tratamentos, r vezes cada, sorteados
  entre todas as parcelas.
- **dbc** — blocos casualizados: cada bloco recebe todos os tratamentos uma
  vez, em ordem sorteada dentro do bloco, independente dos outros blocos.
- **dql** — quadrado latino t × t, pelo procedimento de Fisher & Yates:
  sorteia um quadrado padrão (1ª linha e 1ª coluna em ordem) entre todos os
  da ordem t — 1, 1, 4, 56 e 9408 para t = 2 a 6 —, depois permuta as linhas,
  as colunas e a associação letra → tratamento. Até t = 6 o quadrado sai
  uniforme entre TODOS os quadrados latinos t × t. De t = 7 em diante os
  padrões são milhões (16.942.080 com t = 7) e o quadrado vem de t³ passos da
  cadeia de Jacobson & Matthews (1996), cuja distribuição limite é a uniforme:
  o sorteio é APROXIMADAMENTE uniforme, e o plano avisa. Cada tratamento fica
  uma vez em cada linha e em cada coluna.
- **fatorial** — as combinações de 2 ou mais fatores como tratamentos, em DIC
  ou DBC (**Base**).
- **confundimento** — 2^k em blocos incompletos: os efeitos em **Confundir com
  blocos** (e as suas interações generalizadas) ficam confundidos com blocos.
  Cada combinação vai ao bloco dado pela paridade das letras do efeito; a
  ordem dos blocos em cada repetição e das parcelas em cada bloco é sorteada.
- **fracionado** — 2^(k−p): os fatores básicos em ordem padrão e os demais
  pelos **Geradores**. O plano traz a relação de definição, a resolução e os
  aliases dos efeitos principais e das interações duplas. A ordem das corridas
  é sorteada; a coluna `padrao` guarda a ordem padrão. As colunas dos fatores
  saem SEMPRE codificadas, −1 e +1, mesmo com níveis nomeados: −1 é o 1º nível
  declarado e +1 o 2º (`A: baixo, alto` dá −1 = baixo). Os nomes ficam no
  metadado (`fatores$niveis`, e a tabela `extras$codificacao`).
- **composto_central** — porção fatorial, 2k pontos axiais em ±α e os
  **Pontos centrais**. A porção fatorial é o 2^k completo ou, com
  **Geradores**, a fração 2^(k−p) que eles definem, que precisa ter
  resolução V ou mais (com 5 fatores, `E = ABCD` dá 16 pontos em vez de 32).
  α rotacional = n_F^(1/4), com n_F os pontos da porção fatorial (2 com
  n_F = 16), ou 1 (face). Escala codificada; a ordem das corridas é sorteada.
- **parcela_subdividida** — o 1º fator na parcela, sorteado no bloco (ou entre
  todas as parcelas, com Base `dic`); o 2º na subparcela, sorteado DENTRO de
  cada parcela: dois estágios, dois erros.
- **faixas** — o 1º fator em faixas horizontais e o 2º em faixas verticais,
  cada um sorteado no bloco, independentemente.
- **bib** — blocos incompletos balanceados de k = **Parcelas por bloco**. Do
  menor λ admissível (r = λ(t−1)/(k−1) e b = rt/k inteiros, b ≥ t) para
  cima, procura: BIB cíclico (conjunto de diferenças, b = t); família de 2
  ou 3 blocos iniciais desenvolvidos mod t ou mod t − 1 com um ponto fixo;
  uma pequena tabela de BIBs conferidos ((10, 4), b = 15); busca exaustiva
  com limite de passos (b ≤ 60); e, com k > t/2, o complementar do BIB de
  t − k. Ex.: t = 6, k = 3 dá 10 blocos (r = 5, λ = 2); 9 e 3 dão 12; 8 e 4
  dão 14; 10 e 4 dão 15. Sem nada menor, usa todos os C(t, k) blocos, com
  aviso, ou recusa se passam de 300. Nem sempre o b achado é o menor que
  existe: as construções são finitas. O plano traz b, r, λ, a construção e o
  fator de eficiência λt/(rk). Sorteia os rótulos, a ordem dos blocos e a
  posição no bloco.
- **medidas_repetidas** — os tratamentos sorteados aos indivíduos (DIC entre
  indivíduos) e cada indivíduo medido em todos os **Tempos**, na ordem do tempo.
  A análise sugerida, `(1 | individuo)`, supõe a mesma correlação entre
  quaisquer dois tempos do indivíduo (simetria composta, que implica a
  esfericidade). Quando a correlação cai com a distância no tempo — o AR(1)
  que `experiments/error` simula —, esse modelo não a representa, e os testes
  do tempo e da interação ficam liberais; aí vale um modelo com estrutura de
  covariância no tempo.
- **crossover** — sequências de Williams: com t par, um quadrado; com t ímpar,
  o quadrado e o espelho (2t sequências). Cada tratamento é precedido por cada
  outro o mesmo número de vezes, o balanço para o efeito residual. As
  sequências são sorteadas aos indivíduos. A coluna `residual` é o
  tratamento do período anterior (`nenhum` no 1º período), e a análise
  sugerida é `~ periodo + trat + residual + (1 | individuo)`. Como `nenhum`
  coincide com o 1º período, a matriz do modelo tem uma coluna redundante: o
  `lme4` avisa e a descarta, e o efeito residual fica com t − 1 graus de
  liberdade.
- **grupos** — o mesmo delineamento (DIC, DBC ou fatorial) em cada um dos
  **Locais**, com sorteio independente por local. Na análise sugerida cada
  termo de tratamento — efeitos principais e todas as interações — ganha a
  sua interação aleatória com o local (`(1 | local:A)`, `(1 | local:A:B)`...),
  para que nenhum seja testado contra o resíduo dentro do local.

Recusas (card vermelho): fator com nome de coluna estrutural, número errado de
fatores, estrutura sem grau de liberdade para o resíduo (quadrado latino 2 × 2,
DBC de um bloco), gerador malformado, efeito principal confundido com blocos.
Avisos (no plano, sem bloquear): interação dupla confundida, resolução III,
modelo sem resíduo no fracionado, BIB não reduzido, quadrado latino com
t ≥ 7 (aproximadamente uniforme).

## Parâmetros

- **Estrutura** — uma das treze acima.
- **Fatores** — `nome: nível, nível; nome: nível, ...`. `dose: 4` cria os
  níveis 1 a 4. No fracionado e no composto central basta o nome (escala
  −1/+1); as letras A, B, C… dos geradores seguem a ordem declarada.
- **Repetições** — repetições do DIC, blocos do DBC, da parcela subdividida e
  das faixas, réplicas da matriz no confundimento, no fracionado e no composto
  central, cópias do BIB, indivíduos por tratamento (medidas repetidas) ou por
  sequência (crossover), blocos por local (grupos). 0 usa o padrão da
  estrutura; no quadrado latino é ignorado.
- **Base** — `dbc` ou `dic`: fatorial, parcela subdividida e grupos.
- **Confundir com blocos** — efeitos em letras, `ABC` ou `ABC; ABD`.
- **Geradores do fracionado** — `D = ABC; E = -ABD`. No composto central,
  definem a fração da porção fatorial (resolução V ou mais).
- **α do composto central**, **Pontos centrais**.
- **Parcelas por bloco (BIB)** — k.
- **Tempos (medidas repetidas)** — `0, 30, 60`.
- **Locais (grupos)**.
- **Colunas da grade** — largura da grade no DIC, no fracionado e no composto
  central (0 = quase quadrada).
- **Covariáveis observadas** — nomes de colunas a medir (saem vazias, `NA`).

## Valor

Um plano (`experiments/plan`): a tabela de unidades, uma linha por unidade de
observação, já na ordem de execução, com as colunas estruturais (`bloco`,
`parcela`, `subparcela`, `linha`, `coluna`, `individuo`, `tempo`, `periodo`,
`sequencia`, `local`...) e uma coluna por fator; mais o metadado do plano
(fatores, hierarquia, geometria, semente, a análise sugerida). Ligado num bloco
da `data`, vira essa tabela — e, com uma coluna de resposta, vai direto a
`models/anova_dic`, `models/anova_dbc`, `models/anova_dql`,
`models/anova_factorial`, `models/anova_split_plot`, `models/lm` ou
`models/lmer`. O card mostra o croqui (o mapa).

## Exemplos

```r
tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "parcela_subdividida",
         fatores = "irrigacao: baixa, alta; variedade: A, B, C", repeticoes = 4L) |>
  tr_add("mapa", "experiments/view", aba = "hierarquia", from = "plano")
```

## Veja também

`experiments/view` para o mapa, a hierarquia, as combinações e a ordem;
`models/anova_split_plot` e as demais ANOVAs para analisar.

### O sorteio

O bloco é **estocástico**: a semente é do card, e não da sessão. O mesmo
documento sorteia sempre a mesma alocação — o croqui que vai a campo amanhã é
o de hoje —, e trocar a semente sorteia outra. O plano guarda a semente, o
gerador e a versão da coleção, que é o que basta para reproduzir o sorteio. A
semente do console (`set.seed()`) não é tocada.


### Referências

- Banzatto, D. A.; Kronka, S. N. *Experimentação agrícola*. 4. ed. Jaboticabal:
  FUNEP, 2006.
- Montgomery, D. C. *Design and Analysis of Experiments*. 9. ed. Hoboken:
  Wiley, 2017.
- Cochran, W. G.; Cox, G. M. *Experimental Designs*. 2. ed. New York: Wiley,
  1957.
- Box, G. E. P.; Hunter, J. S.; Hunter, W. G. *Statistics for Experimenters*.
  2. ed. Hoboken: Wiley, 2005.
- Box, G. E. P.; Wilson, K. B. On the experimental attainment of optimum
  conditions. *Journal of the Royal Statistical Society B*, 13(1), 1–38
  (discussão, 38–45), 1951. doi:10.1111/j.2517-6161.1951.tb00067.x
- Williams, E. J. Experimental designs balanced for the estimation of residual
  effects of treatments. *Australian Journal of Scientific Research A*, 2(2),
  149–168, 1949. doi:10.1071/CH9490149
- Fisher, R. A.; Yates, F. *Statistical Tables for Biological, Agricultural
  and Medical Research*. 6. ed. Edinburgh: Oliver and Boyd, 1963 (introdução
  às tabelas de quadrados latinos; a conferir a página).
- Jacobson, M. T.; Matthews, P. Generating uniformly distributed random Latin
  squares. *Journal of Combinatorial Designs*, 4(6), 405–437, 1996.
  doi:10.1002/(SICI)1520-6610(1996)4:6<405::AID-JCD3>3.0.CO;2-J
- Bose, R. C. On the construction of balanced incomplete block designs.
  *Annals of Eugenics*, 9(4), 353–399, 1939.
  doi:10.1111/j.1469-1809.1939.tb02219.x
- OEIS A000315: número de quadrados latinos reduzidos de ordem n.
  https://oeis.org/A000315
- Bailey, R. A. *Design of Comparative Experiments*. Cambridge: Cambridge
  University Press, 2008.

