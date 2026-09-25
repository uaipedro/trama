# Fontes dos testes da coleção `series`

Este documento registra de onde vem cada teste de hipótese da coleção
`trama.series`, para citação em publicação. Ele é conferido por teste: a
varredura de `collections/trama.series/tests/testthat/test-catalogo.R` exige que
todo bloco de teste da `series` (saída `data/test`) tenha a sua linha na tabela abaixo, com o
campo `fonte` escrito exatamente como o bloco o devolve, e que toda linha da
tabela nomeie um bloco que existe.

## Crédito: a dissertação que guiou a escolha dos testes

A escolha e a formulação dos testes não paramétricos de tendência, dos testes de
sazonalidade e do teste de quebra estrutural seguem a síntese de:

> PAIVA, Denise de Assis. **Estudo de testes para tendência em séries
> temporais**. 2020. Dissertação (Mestrado em Estatística e Experimentação
> Agropecuária) — Universidade Federal de Lavras, Lavras, 2020. Orientadora:
> Profa. Dra. Thelma Sáfadi.

Seções e equações da dissertação usadas na implementação:

| seção | teste | equações | bloco |
|---|---|---|---|
| 3.3.1 | Pettitt | 3.8 a 3.12 | `series/pettitt` |
| 3.3.2 | Run | 3.13 a 3.15 | `series/runs` |
| 3.3.3 | Mann-Kendall | 3.16 a 3.21 (3.20 é a correção de empates) | `series/mann_kendall` |
| 3.3.4 | Cox-Stuart | 3.22 | `series/cox_stuart` (`pareamento = "metades"`) |
| 3.3.5.1 e 3.3.5.2 | Dickey-Fuller e Dickey-Fuller Aumentado | 3.23 a 3.34 | `series/adf` |
| 3.3.5.3 | Zivot-Andrews | 3.35 a 3.37, Tabela 3.2 | `series/zivot_andrews` |
| 3.5.1 | Kruskal-Wallis | 3.38 a 3.40 | `series/seasonality_kw` |
| 3.5.2 | Fisher | 3.41 e 3.42 | `series/periodicity_fisher` |

A dissertação também é a origem de três ressalvas que as páginas de ajuda
repetem: Pettitt detecta ruptura e não tendência (NIEL et al., 1998, §3.3.1);
ADF e Phillips-Perron viesam sob quebra estrutural (MARGARIDO, 2001, §3.3.5.3);
e o Run deixou de detectar tendências que Pettitt e Mann-Kendall encontraram nas
mesmas séries (BACK, 2001, revisado no capítulo 2).

## Referência de cada bloco

> **Conferir antes de publicar.** As referências marcadas "referência padrão"
> (KPSS, Phillips-Perron, Ljung-Box, Box-Pierce) e as da seção "Outras fontes"
> (Hyndman & Athanasopoulos, Cleveland et al., Hyndman & Khandakar, Hyndman et
> al.) foram escritas de memória e **não foram conferidas** contra os artigos.
> As marcadas "sim" foram copiadas da lista de referências da dissertação. A
> edição de Hyndman & Athanasopoulos citada pelo código ("cap. 4") é suposta.

A coluna `fonte` é o texto que o bloco devolve no campo `fonte` e que aparece na
vista `detalhe` do card. A referência completa está no formato ABNT e, quando
consta da lista de referências da dissertação, foi copiada de lá.

| bloco | fonte | referência completa | na dissertação? |
|---|---|---|---|
| `series/adf` | Dickey & Fuller (1979, 1981) | DICKEY, D. A.; FULLER, W. A. Distribution of the estimators for autoregressive time series with a unit root. Journal of the American statistical association, EUA, v. 74, n. 366a, p. 427–431, 1979. DICKEY, D. A.; FULLER, W. A. Likelihood ratio statistics for autoregressive time series with a unit root. Econometrica: Journal of the Econometric Society, New York, v. 49, n. 4, p. 1057–1072, 1981. | sim |
| `series/kpss` | Kwiatkowski et al. (1992) | KWIATKOWSKI, D.; PHILLIPS, P. C. B.; SCHMIDT, P.; SHIN, Y. Testing the null hypothesis of stationarity against the alternative of a unit root: how sure are we that economic time series have a unit root? Journal of Econometrics, v. 54, n. 1–3, p. 159–178, 1992. | **não** — referência padrão |
| `series/phillips_perron` | Phillips & Perron (1988) | PHILLIPS, P. C. B.; PERRON, P. Testing for a unit root in time series regression. Biometrika, v. 75, n. 2, p. 335–346, 1988. | **não** — referência padrão |
| `series/zivot_andrews` | Zivot & Andrews (1992) | ZIVOT, E.; ANDREWS, D. W. K. Further evidence on the great crash, the oil-price shock, and the unit-root hypothesis. Journal of Business & Economic Statistics, Alexandria, v. 10, n. 3, p. 25–44, 1992. | sim |
| `series/ljung_box` | Ljung & Box (1978) | LJUNG, G. M.; BOX, G. E. P. On a measure of lack of fit in time series models. Biometrika, v. 65, n. 2, p. 297–303, 1978. | **não** — referência padrão |
| `series/box_pierce` | Box & Pierce (1970) | BOX, G. E. P.; PIERCE, D. A. Distribution of residual autocorrelations in autoregressive-integrated moving average time series models. Journal of the American Statistical Association, v. 65, n. 332, p. 1509–1526, 1970. | **não** — referência padrão |
| `series/f_global` | Morettin & Toloi (2006) | MORETTIN, P. A.; TOLOI, C. M. Análise de Séries Temporais. 2. ed. São Paulo: Edgard Blucher, 2006. 564 p. | a obra sim; o F de regressão não é tratado na dissertação |
| `series/f_seasonal` | Morettin & Toloi (2006) | MORETTIN, P. A.; TOLOI, C. M. Análise de Séries Temporais. 2. ed. São Paulo: Edgard Blucher, 2006. 564 p. | a obra sim; o F de regressão não é tratado na dissertação |
| `series/f_trend` | Morettin & Toloi (2006) | MORETTIN, P. A.; TOLOI, C. M. Análise de Séries Temporais. 2. ed. São Paulo: Edgard Blucher, 2006. 564 p. | a obra sim; o F de regressão não é tratado na dissertação |
| `series/mann_kendall` | Mann (1945) | MANN, H. B. Nonparametric tests against trend. Econometric Society, New Haven, v. 13, n. 3, p. 245–259, 1945. | sim |
| `series/cox_stuart` | Cox & Stuart (1955) | COX, D. R.; STUART, A. Some quick sign tests for trend in location and dispersion. Biometrika, Oxford, v. 42, n. 1/2, p. 80–95, 1955. | sim |
| `series/runs` | Wald & Wolfowitz (1940) | WALD, A.; WOLFOWITZ, J. On a test whether two samples are from the same population. The Annals of Mathematical Statistics, Beachwood, v. 11, n. 2, p. 147–162, 1940. | sim |
| `series/pettitt` | Pettitt (1979) | PETTITT, A. A non-parametric approach to the change-point problem. Journal of the Royal Statistical Society, Malden, v. 28, n. 2, p. 126–135, 1979. | sim |
| `series/seasonality_kw` | Morettin & Toloi (2006) | MORETTIN, P. A.; TOLOI, C. M. Análise de Séries Temporais. 2. ed. São Paulo: Edgard Blucher, 2006. 564 p. | sim (§3.5.1) |
| `series/periodicity_fisher` | Morais (2012) | MORAIS, T. S. T. d. Estudo temporal do nível médio do mar em diferentes oceanos. 2012. 110 p. Dissertação (Mestrado em Estatística e Experimentação Agropecuária) — Universidade Federal de Lavras, Lavras, 2012. | sim (§3.5.2) |

Observações sobre a tabela:

- Com `pareamento = "metades"` o `series/cox_stuart` segue a formulação da
  dissertação (§3.3.4), e não a de Cox & Stuart (1955). O campo `fonte` continua
  "Cox & Stuart (1955)", e a `nota` do bloco acrescenta "formulação da
  dissertação (Paiva, 2020), não a original". Quem reportar esse caminho deve
  citar as duas.
- A eq. 3.22 do Cox-Stuart vem, na dissertação, de WACKERLY, D.; MENDENHALL, W.;
  SCHEAFFER, R. L. Mathematical Statistics with Applications. 7. ed. EUA:
  Cengage Learning, 2014. 939 p.
- A dissertação atribui o `trend::mk.test` a HIPEL, K. W.; MCLEOD, A. I. Time
  series modelling of water resources and environmental systems. [S.l.]:
  Elsevier, 1994. v. 45. 1012 p. O bloco cita o artigo original (Mann, 1945); a
  variância com correção de empates é a da eq. 3.20.

Referências das ressalvas citadas acima, como constam da dissertação:

- BACK, Á. J. Aplicação de análise estatística para identificação de tendências
  climáticas. Pesquisa Agropecuária Brasileira, Brasília, v. 36, n. 5, p.
  717–726, 2001.
- MARGARIDO, M. A. Aplicação de testes de raiz unitária com quebra estrutural em
  séries econômicas no Brasil na década de 90. Informações econômicas, São
  Paulo, v. 31, n. 4, p. 7–22, 2001.
- NIEL, H. L. et al. Variabilité climatique et statistiques: Etude par simulation
  de la puissance et de la robustesse de quelques tests utilisés pour vérifier
  l'homogénéité de chroniques. Revue des sciences de l'eau, Quebec, v. 11, n. 3,
  p. 383–408, 1998.

## Software

Usados pelos blocos:

- **urca** — `ur.df` (ADF), `ur.kpss` (KPSS) e `ur.za` (Zivot-Andrews). Citado
  na dissertação como: PFAFF, B. Analysis of Integrated and Cointegrated Time
  Series with R. Second. New York: Springer, 2008. ISBN 0-387-27960-1.
- **stats** (R base) — `PP.test` (Phillips-Perron), `Box.test` (Ljung-Box e
  Box-Pierce), `kruskal.test`, `spec.pgram` (periodograma do Fisher), `lm` e
  `anova` (os três F), `binom.test` (Cox-Stuart exato). R Core Team. R: A
  Language and Environment for Statistical Computing. Vienna, Austria: R
  Foundation for Statistical Computing. Disponível em:
  <https://www.R-project.org/>. (A dissertação cita a edição de 2019; citar a
  versão efetivamente usada.)

Usados **só para conferência** na suíte de testes (`Suggests`), nunca pelos
blocos — Mann-Kendall, Cox-Stuart, Run e Pettitt são implementados em R base e
conferidos numericamente contra eles:

- **trend** — POHLERT, T. trend: Non-Parametric Trend Tests and Change-Point
  Detection. [S.l.], 2018. R package version 1.1.1. Disponível em:
  <https://CRAN.R-project.org/package=trend>. (Citado na dissertação; a
  conferência roda com a versão 1.1.8.)
- **randtests** — CAEIRO, F.; MATEUS, A. randtests: Testing randomness in R.
  [S.l.], 2014. R package version 1.0. Disponível em:
  <https://CRAN.R-project.org/package=randtests>. (Citado na dissertação; a
  conferência roda com a versão 1.0.2.)

## Onde a implementação se afasta das fontes

Todas as divergências abaixo foram medidas. Elas têm o mesmo formato: a prosa
de uma fonte e o código de outra discordam num detalhe que quase nunca muda o
veredito, e por isso passam despercebidas.

### 1. Cox-Stuart: terços ou metades

Cox & Stuart (1955) pareiam **terços**: o primeiro terço contra o último, com o
miolo descartado. A dissertação (§3.3.4) pareia **metades**: cada observação
contra a que está meia série adiante. O bloco oferece as duas no param
`pareamento`, com `terços` como padrão — por ser a formulação original e por ser
a que tem referência conferível (`trend::cs.test`).

Medido com `set.seed(42); x <- 1:60 + rnorm(60, sd = 5)`:

| pareamento | pares | M | Z | p |
|---|---|---|---|---|
| terços | 20 | 20 | 4.472136 | 7.744216e-06 (idêntico ao `trend::cs.test`) |
| metades | 30 | 30 | 5.477226 | 4.320463e-08 |

**Em série de tamanho ímpar, as metades do bloco também não são as da
dissertação.** A dissertação usa c = (N+1)/2 e pareia Z(i) com Z(i+c), deixando
de fora a observação do MEIO; o bloco usa `floor(n/2)` e pareia `x[i]` com
`x[i + floor(n/2)]`, deixando de fora a ÚLTIMA. O número de pares é o mesmo, os
pares não. Medido com `x <- 0.05 * (1:61) + rnorm(61, sd = 2)`:

| semente | bloco (M, Z, p) | dissertação (M, Z, p) |
|---|---|---|
| 42 | 19, 1.460593, 0.144127 | 17, 0.7302967, 0.4652088 |
| 1 | 22, 2.556039, 0.01058714 | 23, 2.921187, 0.003487005 |
| 7 | 19, 1.460593, 0.144127 | 19, 1.460593, 0.144127 |

Nas três sementes a decisão a 5% coincide. Em N par as duas formulações são
idênticas. (Achado desta revisão; o código ainda não foi alterado.)

### 2. Cox-Stuart: a fronteira entre binomial exata e aproximação normal

A dissertação (§3.3.4) manda a binomial para n ≤ 20 pares e a normal para
n > 20. O bloco usa a binomial exata para **k < 20** pares, e a normal a partir
de 20. A borda é nossa, tirada da prosa da dissertação e deslocada em um par
para que a reta com ruído de 60 pontos — que sai com exatamente 20 pares em
terços — seja conferida contra o `trend` pela mesma aproximação.

O `trend::cs.test` **não tem ramo exato nenhum** e não serve de evidência sobre
onde pôr a borda: usa a normal sempre (com 6 pares e M = 6, dá p = 0.04122683
contra a binomial exata 0.03125). Além disso ele padroniza por n/6 e n/12 — os
momentos de n/3 pares — enquanto forma `ceiling(n/3)` pares e descarta empates;
aplica correção de continuidade para n ≤ 30; e toma `max(table(sign))`, de modo
que a estatística dele é sempre positiva e não carrega direção. O bloco
padroniza pelos pares que sobraram e guarda o sinal. Os dois só coincidem
quando n é múltiplo de 3, nada empata, k ≥ 20 (o que em terços já põe n acima
de 30, fora da correção de continuidade) e a série sobe.

### 3. Phillips-Perron: a borda da tabela de p-valores

O `stats::PP.test` interpola o p-valor na tabela
`tablep <- c(0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99)` e, fora dela,
prende o valor na borda (`approx(rule = 2)`) sem aviso. A tabela vai de 0.01 a
**0.99**, e não a 0.1. O nó antigo da coleção (`series/stationarity`, removido)
marcava como truncado todo p ≥ 0.1 — errado: um passeio aleatório
(`set.seed(1); cumsum(rnorm(200))`) sai com p = 0.6249, interpolado. O bloco
marca a truncagem só em p ≤ 0.01 ou p ≥ 0.99.

### 4. Zivot-Andrews: a janela de busca da quebra

O `urca::ur.za` varre **todos** os candidatos a quebra (`idx <- 1:(n - 1)`),
inclusive cortes que deixam um segmento de uma observação. O teste como Zivot &
Andrews (1992) o publicaram busca a quebra em aproximadamente [0.15n, 0.85n], e
é essa a tabela de valores críticos que o `urca` devolve. O bloco recorta a
estatística a partir de `z@tstats` para a janela [0.15n, 0.85n].

O motivo é a **data** da quebra, e não o tamanho do teste. Aparar muda a taxa
de rejeição sob H0 em no máximo 1 ponto percentual; sem aparar, a quebra
publicada cai fora da janela numa fração das amostras sob passeio aleatório, e o
card imprimiria "quebra na observação 1". O sobretamanho em série curta
**persiste** com a janela — é de amostra finita, não da falta de aparagem.

Medições registradas (passeio aleatório, modelo `both`, 5% nominal, 500
repetições):

| medição | n | varredura completa | janela [0.15n, 0.85n] | quebra fora da janela |
|---|---|---|---|---|
| Phase 4 do plano | 20 | 13.8% | 13.8% | — |
| Phase 4 do plano | 100 | 7.0% | 6.7% | — |
| Phase 4 do plano | não informado | — | — | 3% a 17% |
| comentário do código (`R/testar.R`) | 40 | — | perto de 9% | — |
| comentário do código (`R/testar.R`) | 100 | — | 7% a 8% | — |
| comentário do código (`R/testar.R`) | 20 a 100 | — | — | 4% a 14% |
| refeita nesta revisão, `lag = 0`, `set.seed(2026)` | 20 | 12.8% | 12.8% | 4.0% |
| refeita nesta revisão, `lag = 0`, `set.seed(2026)` | 40 | 8.6% | 8.4% | 5.8% |
| refeita nesta revisão, `lag = 0`, `set.seed(2026)` | 100 | 5.0% | 5.0% | 6.6% |

A diferença entre varrer tudo e aparar ficou em no máximo 1 ponto em todas as
medições. O excesso de rejeição em n = 40 (8% a 9%) se reproduziu; em n = 100 a
medição refeita com `lag = 0` não reproduziu os 7% (deu 5.0%, dentro do ruído de
500 repetições, cujo erro-padrão é de cerca de 1 ponto). O bloco avisa na nota
abaixo de 40 observações e recusa abaixo de 20.

### Duas correções à nossa própria orientação, que o usuário encontraria

**Kruskal-Wallis e o logaritmo.** Uma primeira versão do plano dizia que o
Kruskal-Wallis não rejeita no `AirPassengers` por a sazonalidade ser
multiplicativa, e que tirar o log resolveria. Errado: o teste é de postos e o
log é monotônico, não altera posto nenhum. Medido: H = 11.1484, p = 0.4309159
no bruto e no logado, idênticos. A causa é a **tendência** — os postos altos
ficam todos nos anos finais e se espalham por todos os meses. Removendo-a,
`diff(AirPassengers)` dá H = 119.2025, p = 2.622853e-20. A ajuda do bloco aponta
`series/diff` e avisa que o log é inerte.

**Pettitt detecta ruptura, não tendência.** Rejeitar H0 no Pettitt é concluir
que houve um ponto de mudança, e não que a série sobe ou desce — ressalva que a
própria dissertação faz (§3.3.1), citando Niel et al. (1998). A conclusão do
bloco nomeia a observação da mudança e não fala em direção.

## Outras fontes citadas nas páginas de ajuda

Não são blocos de teste (`data/test`), e não estão na dissertação; são as referências
padrão dos métodos que a coleção já citava no código:

- **Força da tendência e da sazonalidade** (resumo do `series/decompose` e do `series/stl`) —
  HYNDMAN, R. J.; ATHANASOPOULOS, G. Forecasting: principles and practice. 3.
  ed. Melbourne: OTexts, 2021. Disponível em: <https://otexts.com/fpp3/>.
  (O código cita "cap. 4", que na 3ª edição é *Time series features*.)
- **Decomposição STL** (`series/stl`) — CLEVELAND, R. B.; CLEVELAND, W. S.;
  McRAE, J. E.; TERPENNING, I. STL: a seasonal-trend decomposition procedure
  based on Loess. Journal of Official Statistics, v. 6, n. 1, p. 3–73, 1990.
- **ARIMA automático** (`series/arima` com `automatico`) — HYNDMAN, R. J.;
  KHANDAKAR, Y. Automatic time series forecasting: the forecast package for R.
  Journal of Statistical Software, v. 27, n. 3, p. 1–22, 2008.
- **ETS** (`series/ets`) — HYNDMAN, R. J.; KOEHLER, A. B.; ORD, J. K.; SNYDER,
  R. D. Forecasting with exponential smoothing: the state space approach.
  Berlin: Springer, 2008.
- **Regra de defasagens do Ljung-Box e do Box-Pierce** (10 sem ciclo, dois
  ciclos com ciclo, no máximo n/5) — Hyndman & Athanasopoulos, citado acima.
