---
title: Modelos
description: "Ajuste modelos e delineamentos, resuma efeitos e confira pressupostos com saídas encadeáveis."
section: colecoes
collection: modelos
related: [models/example, models/lm]
---

## Organização da coleção

A coleção `models` recebe tabelas `data/table` e produz ajustes (`models/fit`), tabelas de efeitos, testes, médias ajustadas ou gráficos. Os grupos seguem as perguntas da análise: carregar dados, ajustar, resumir o ajuste, comparar médias, verificar pressupostos e testar sem modelo.

Um fluxo comum começa com uma tabela, ajusta o modelo que corresponde ao desenho e conecta o resultado a resumos, comparações ou diagnósticos. Os blocos de ANOVA convertem tratamento, bloco, linha e coluna em fatores; `models/lm` e `models/glm` aceitam fórmulas do R; `models/lmer` representa agrupamentos por efeitos aleatórios.

## Fluxo reproduzível

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "milho_dbc") |>
  tr_add("ajuste", "models/anova_dbc", resposta = "producao",
         tratamento = "hibrido", bloco = "bloco", from = "dados") |>
  tr_add("medias", "models/emmeans", especs = "hibrido", from = "ajuste") |>
  tr_add("pares", "models/pairwise", from = "medias")
```

`milho_dbc` simula cinco híbridos em quatro blocos; H3 foi construído com produção maior. Compare médias e contrastes com esse efeito conhecido antes de aplicar o fluxo a outro conjunto.

## Blocos

### Fonte

- [Exemplo de modelos](/trama/colecoes/modelos/example/) carrega tabelas incluídas na coleção.

### Ajustar

- [Regressão linear](/trama/colecoes/modelos/lm/) ajusta respostas contínuas.
- [Modelo linear generalizado](/trama/colecoes/modelos/glm/) ajusta respostas binárias, contagens ou contínuas positivas.
- [Modelo misto](/trama/colecoes/modelos/lmer/) modela dependência entre observações agrupadas.
- [RLS online](/trama/colecoes/modelos/rls/) atualiza o ajuste ponto a ponto em fluxo.

### ANOVA

- [ANOVA · DIC](/trama/colecoes/modelos/anova-dic/), [ANOVA · DBC](/trama/colecoes/modelos/anova-dbc/) e [ANOVA · DQL](/trama/colecoes/modelos/anova-dql/) correspondem a diferentes restrições de casualização.
- [ANOVA · fatorial](/trama/colecoes/modelos/anova-factorial/) avalia fatores e interações.
- [ANOVA · parcela subdividida](/trama/colecoes/modelos/anova-split-plot/) usa dois estratos de erro.
- [Regressão de doses](/trama/colecoes/modelos/dose-response/) desdobra um tratamento quantitativo em componentes polinomiais e ajusta a curva.

### Resumir

- [Quadro da ANOVA](/trama/colecoes/modelos/anova-table/), [Coeficientes](/trama/colecoes/modelos/coefficients/), [Gráfico dos coeficientes](/trama/colecoes/modelos/plot-coefficients/), [Gráfico de regressão](/trama/colecoes/modelos/plot-regression/), [Medidas de ajuste](/trama/colecoes/modelos/fit-stats/), [Tamanho de efeito (ANOVA)](/trama/colecoes/modelos/effect-size/), [Efeitos aleatórios](/trama/colecoes/modelos/random-effects/), [Resíduos](/trama/colecoes/modelos/residuals/), [Diagnóstico dos resíduos](/trama/colecoes/modelos/plot-diagnostics/), [Gráfico de lagarta](/trama/colecoes/modelos/plot-caterpillar/), [Comparar modelos](/trama/colecoes/modelos/compare/), [Teste dos aleatórios](/trama/colecoes/modelos/random-test/) e [Importância](/trama/colecoes/modelos/importance/).

### Prever e avaliar

- [Prever](/trama/colecoes/modelos/predict/) aplica o modelo a dados novos ou ao treino (resubstituição ou validação cruzada).
- [Matriz de confusão](/trama/colecoes/modelos/confusion/), [Curva ROC](/trama/colecoes/modelos/roc/) e [Avaliar previsões](/trama/colecoes/modelos/evaluate/) medem o acerto de qualquer modelo — no treino, em dados novos ou numa tabela já prevista.

### Médias

- [Médias ajustadas](/trama/colecoes/modelos/emmeans/), [Comparações de médias](/trama/colecoes/modelos/pairwise/), [Contrastes (F)](/trama/colecoes/modelos/linear-hypothesis/), [Duncan](/trama/colecoes/modelos/duncan/), [Waller-Duncan](/trama/colecoes/modelos/waller-duncan/), [Scott-Knott](/trama/colecoes/modelos/scott-knott/) e [Gráfico de médias](/trama/colecoes/modelos/plot-means/).

### Pressupostos

- [Normalidade dos resíduos](/trama/colecoes/modelos/shapiro-residuals/), [Levene](/trama/colecoes/modelos/levene/), [Bartlett](/trama/colecoes/modelos/bartlett/), [Breusch-Pagan](/trama/colecoes/modelos/breusch-pagan/) e [Aditividade de Tukey](/trama/colecoes/modelos/tukey-additivity/).

### Testes

- [t para duas amostras](/trama/colecoes/modelos/t-test/), [t pareado](/trama/colecoes/modelos/paired-t/), [t para uma amostra](/trama/colecoes/modelos/one-sample-t/), [Wilcoxon-Mann-Whitney](/trama/colecoes/modelos/wilcoxon/), [Kruskal-Wallis](/trama/colecoes/modelos/kruskal/), [Dunn](/trama/colecoes/modelos/dunn/), [Tamanho de efeito (dois grupos)](/trama/colecoes/modelos/cohen-d/), [Qui-quadrado](/trama/colecoes/modelos/chisq/), [Exato de Fisher](/trama/colecoes/modelos/fisher-exact/), [Teste de correlação](/trama/colecoes/modelos/cor-test/) e [Shapiro-Wilk](/trama/colecoes/modelos/shapiro/).
