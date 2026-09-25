# As declarações das abas Fonte, Ajustar e ANOVA.

.tr_models_nos_fonte <- function() {
  list(
    trama::tr_node("models/example", fn = tr_models_example, label = "Exemplo de modelos",
      category = "modelo_fonte", icon = trama::tr_icon("database"),
      description = "Carrega um conjunto de dados escolhido para ensinar um modelo ou delineamento.",
      outputs = list(out = "data/table"),
      params = list(dataset = trama::tr_param_enum("PlantGrowth", .TR_MODELS_EXEMPLOS, label = "Conjunto")),
      help = .tr_models_ajuda(r"---[
Carrega um conjunto de dados pensado para as técnicas desta coleção — um por
delineamento ou modelo.

### Experimentos

- **PlantGrowth** — peso seco de plantas (`weight`) sob controle e dois
  tratamentos (`group`). DIC com 10 repetições: o caso de livro da ANOVA de um
  fator.
- **milho_dbc** — SIMULADO. 5 híbridos (`hibrido`) em 4 blocos (`bloco`),
  resposta `producao` em t/ha. O H3 foi construído 1,2 t/ha acima do H1, e o H5
  0,4 acima: o Tukey tem de separar o H3 dos demais.
- **racao_dql** — SIMULADO. Quadrado latino 5 × 5: rações (`racao`) em cinco
  períodos (`periodo`) e cinco lotes (`lote`), resposta `ganho_peso`. A R5 foi
  construída 3 unidades acima, e o período tem tendência crescente.
- **ToothGrowth** — comprimento de odontoblastos (`len`) de cobaias, com dois
  suplementos (`supp`) e três doses (`dose`). Fatorial 2 × 3 com interação.
- **warpbreaks** — quebras de fio (`breaks`) por lã (`wool`) e tensão
  (`tension`). Fatorial com interação forte.
- **npk** — ervilha em blocos (`block`), fatorial 2³ de N, P e K (`yield`).
- **aveia** — o experimento de Yates (`MASS::oats`): parcela subdividida com
  `variedade` na parcela, `nitrogenio` na subparcela, 6 blocos, `producao`.

### Outros modelos

- **sleepstudy** — tempo de reação (`Reaction`) de 18 pessoas (`Subject`) ao
  longo de 10 dias de privação de sono (`Days`). O exemplo do `lme4`.
- **InsectSprays** — contagem de insetos (`count`) sob 6 inseticidas (`spray`).
  Contagem com variância que cresce com a média: GLM Poisson.
- **mtcars** — 32 carros, consumo (`mpg`) e 10 características, com o nome em
  `modelo`. Regressão múltipla; `am` e `vs` servem para GLM binomial.
- **cars** — distância de frenagem (`dist`) pela velocidade (`speed`).
]---", r"---[
- **Conjunto** — qual conjunto carregar.
]---", r"---[
Uma tabela (`data/table`).
]---", r"---[
tr_flow(reg) |>
  tr_add("milho", "models/example", dataset = "milho_dbc") |>
  tr_add("dbc", "models/anova_dbc", resposta = "producao", tratamento = "hibrido",
         bloco = "bloco", from = "milho")
]---", r"---[
`models/anova_dic` e `models/anova_dbc` para os experimentos; `models/lm` e
`models/lmer` para os outros; `data/example` para os demais conjuntos do R.
]---"))
  )
}

#' O texto comum sobre faltantes, repetido nos blocos que ajustam.
#' @noRd
.tr_models_ajuda_faltantes <- function() {
  "
### Faltantes

Linhas com faltante nas colunas do modelo ficam FORA do ajuste, e o card diz
quantas (`n = 38 (2 fora)`). As outras colunas da tabela não contam.
"
}

.tr_models_nos_ajustar <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; B <- trama::tr_param_bool
  Fm <- "models/fit"; T <- "data/table"
  list(
    trama::tr_node("models/lm", 
      pressupostos = .tr_models_doc("models/lm")$pressupostos,
      referencias = .tr_models_doc("models/lm")$referencias,
      fn = tr_models_lm, label = "Regressão linear",
      category = "modelo_ajustar", icon = trama::tr_icon("chart-scatter"),
      description = "Ajusta um modelo linear (lm) pelas colunas ou por uma fórmula digitada.",
      inputs = list(dados = T), outputs = list(out = Fm),
      params = list(
        resposta = P("cols", "", label = "Resposta", example = "mpg"),
        preditores = P("cols", "", label = "Preditores", example = "wt, hp"),
        formula = P("expr", "", label = "Fórmula (vence as colunas)", example = "mpg ~ wt * hp + I(wt^2)")),
      help = .tr_models_ajuda(paste0(r"---[
Ajusta um modelo linear por mínimos quadrados (`stats::lm`).

### Colunas ou fórmula

Há dois jeitos de dizer o modelo:

- **Resposta** e **Preditores** — o atalho: `mpg` com `wt, hp` vira
  `mpg ~ wt + hp`, só efeitos principais.
- **Fórmula** — qualquer fórmula do R. Quando preenchida, ela VENCE, e os
  campos de coluna são ignorados. É onde entram interação (`a * b`, `a:b`),
  polinômio (`poly(x, 2)` ou `I(x^2)`), transformação (`log(y) ~ x`) e fator
  explícito (`factor(dose)`).

### Número é número

Colunas de texto viram fator; colunas numéricas ficam numéricas — `dose` com
0,5/1/2 entra como reta, com 1 grau de liberdade. Se a dose é tratamento, diga
na fórmula (`len ~ factor(dose)`) ou use os blocos da aba ANOVA, que fazem essa
conversão sozinhos.
]---", .tr_models_ajuda_faltantes()), r"---[
- **Resposta** — coluna numérica da resposta.
- **Preditores** — colunas dos preditores, separadas por vírgula. Em branco, só
  o intercepto.
- **Fórmula** — fórmula do R; preenchida, vence os dois campos acima.
]---", r"---[
Um modelo (`models/fit`). O card mostra, na vista **ajuste**, R², R² ajustado e
o F global com a régua do p-valor; na vista **efeitos**, cada coeficiente com a
sua régua. Ligado a um nó da coleção `data`, vira a tabela de coeficientes.
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("reg", "models/lm", formula = "mpg ~ wt + hp", from = "carros")
]---", r"---[
`models/coefficients` e `models/fit_stats` para ler o ajuste;
`models/plot_diagnostics` e `models/shapiro_residuals` para os resíduos;
`models/compare` para testar um termo; `models/glm` para resposta que não é
contínua.
]---")),

    trama::tr_node("models/glm", 
      pressupostos = .tr_models_doc("models/glm")$pressupostos,
      referencias = .tr_models_doc("models/glm")$referencias,
      fn = tr_models_glm, label = "Modelo linear generalizado",
      category = "modelo_ajustar", icon = trama::tr_icon("chart-spline"),
      description = "Ajusta um GLM (binomial, Poisson, gama...) pelas colunas ou por uma fórmula.",
      inputs = list(dados = T), outputs = list(out = Fm),
      params = list(
        resposta = P("cols", "", label = "Resposta", example = "count"),
        preditores = P("cols", "", label = "Preditores", example = "spray"),
        formula = P("expr", "", label = "Fórmula (vence as colunas)", example = "am ~ wt + hp"),
        familia = E("poisson", .TR_MODELS_FAMILIAS, label = "Família")),
      help = .tr_models_ajuda(paste0(r"---[
Ajusta um modelo linear generalizado (`stats::glm`): a resposta segue uma
distribuição da família escolhida, e a média se liga aos preditores por uma
função de ligação.

| família | resposta | ligação |
|---|---|---|
| `binomial` | 0/1, sim/não | logit |
| `poisson` | contagem | log |
| `quasipoisson` | contagem com variância maior que a média | log |
| `quasibinomial` | sucessos em n tentativas (`cbind(sucessos, fracassos)`) com variância maior que a binomial | logit |
| `gama` | contínua positiva, assimétrica | log |
| `gaussiana` | contínua (o mesmo que `models/lm`) | identidade |

As colunas e a fórmula funcionam como no `models/lm`: a fórmula, quando
preenchida, vence.

### Superdispersão

Numa Poisson a variância é igual à média. Quando o desvio residual é muito
maior que os graus de liberdade do resíduo, a variância é maior, os erros
padrão ficam pequenos demais e os p-valores, otimistas.
]---", .tr_models_ajuda_faltantes()), r"---[
- **Resposta**, **Preditores**, **Fórmula** — como no `models/lm`.
- **Família** — a distribuição da resposta (tabela acima).
]---", r"---[
Um modelo (`models/fit`). O card mostra o desvio explicado e o teste de razão de
verossimilhança contra o modelo só com intercepto.
]---", r"---[
tr_flow(reg) |>
  tr_add("insetos", "models/example", dataset = "InsectSprays") |>
  tr_add("glm", "models/glm", resposta = "count", preditores = "spray", familia = "poisson",
         from = "insetos")
]---", r"---[
`models/coefficients` com exponenciar para razão de taxas ou de chances;
`models/anova_table` para o quadro de desvio; `models/emmeans` para as médias na
escala da resposta.
]---")),

    trama::tr_node("models/lmer", 
      pressupostos = .tr_models_doc("models/lmer")$pressupostos,
      referencias = .tr_models_doc("models/lmer")$referencias,
      fn = tr_models_lmer, label = "Modelo misto",
      category = "modelo_ajustar", icon = trama::tr_icon("layers"),
      description = "Ajusta um modelo linear misto (lme4), com p-valores por Satterthwaite (lmerTest).",
      inputs = list(dados = T), outputs = list(out = Fm),
      params = list(
        formula = P("expr", "", label = "Fórmula", example = "Reaction ~ Days + (Days | Subject)"),
        resposta = P("cols", "", label = "Resposta (sem fórmula)", example = "Reaction"),
        fixos = P("cols", "", label = "Efeitos fixos (sem fórmula)", example = "Days"),
        grupo = P("cols", "", label = "Grupo aleatório (sem fórmula)", example = "Subject"),
        reml = B(TRUE, label = "REML")),
      help = .tr_models_ajuda(paste0(r"---[
Ajusta um modelo linear misto: efeitos FIXOS, que se quer estimar (dose,
tratamento, tempo), e efeitos ALEATÓRIOS, que são uma amostra de uma população
maior e só interessam pela variância (bloco, animal, pessoa, local).

### A fórmula

Os termos aleatórios vão entre parênteses, na sintaxe do `lme4`:

- `(1 | bloco)` — intercepto aleatório por bloco.
- `(Days | Subject)` — intercepto e inclinação de `Days` aleatórios por pessoa,
  correlacionados.
- `(1 | bloco:parcela)` — o erro da parcela numa parcela subdividida.

Sem fórmula, **Resposta**, **Efeitos fixos** e **Grupo** montam
`resposta ~ fixos + (1 | grupo)`.

### Os p-valores

O `lme4` não dá p-valor de propósito: os graus de liberdade de um misto não são
óbvios. O ajuste aqui é o do `lmerTest`, que os aproxima por Satterthwaite — é
o que o quadro, os coeficientes e o `models/emmeans` usam.

### REML

Ligado (padrão), as variâncias são estimadas por máxima verossimilhança
restrita, sem o viés da ML. Para comparar modelos com efeitos FIXOS diferentes,
o `models/compare` reajusta por ML sozinho.
]---", .tr_models_ajuda_faltantes()), r"---[
- **Fórmula** — com pelo menos um termo aleatório.
- **Resposta**, **Efeitos fixos**, **Grupo aleatório** — o atalho sem fórmula.
- **REML** — máxima verossimilhança restrita (padrão) ou ML.
]---", r"---[
Um modelo (`models/fit`). O card mostra o R² marginal (só efeitos fixos) e o
condicional (fixos e aleatórios), de Nakagawa & Schielzeth.
]---", r"---[
tr_flow(reg) |>
  tr_add("sono", "models/example", dataset = "sleepstudy") |>
  tr_add("misto", "models/lmer", formula = "Reaction ~ Days + (Days | Subject)", from = "sono")
]---", r"---[
`models/random_effects` para as variâncias; `models/random_test` para testar os
termos aleatórios; `models/anova_table` e `models/coefficients` para os fixos.
]---"))
  )
}

.tr_models_nos_anova <- function() {
  P <- trama::tr_param
  Fm <- "models/fit"; T <- "data/table"
  ajuda_fator <- "
### As colunas do desenho viram fator

Tratamento, bloco, linha e coluna entram como FATOR mesmo quando são números:
dose 50/100/150 é tratamento com 2 graus de liberdade, e não uma reta. Para
ajustar a dose como regressão, use o `models/lm`.
"
  list(
    trama::tr_node("models/anova_dic", 
      pressupostos = .tr_models_doc("models/anova_dic")$pressupostos,
      referencias = .tr_models_doc("models/anova_dic")$referencias,
      fn = tr_models_anova_dic, label = "ANOVA · DIC",
      category = "modelo_anova", icon = trama::tr_icon("layout-grid"),
      description = "Análise de variância de um delineamento inteiramente casualizado.",
      inputs = list(dados = T), outputs = list(out = Fm),
      params = list(
        resposta = P("cols", "", label = "Resposta", example = "weight"),
        tratamento = P("cols", "", label = "Tratamento", example = "group")),
      help = .tr_models_ajuda(paste0(r"---[
ANOVA do delineamento inteiramente casualizado (DIC): os tratamentos foram
sorteados nas parcelas sem restrição nenhuma. Modelo `resposta ~ tratamento`.

Serve quando as parcelas são homogêneas — vasos em casa de vegetação, placas
em laboratório. Se houve bloco, use o `models/anova_dbc`: ignorar o bloco joga
a variação dele no resíduo e esconde o efeito do tratamento.
]---", ajuda_fator, .tr_models_ajuda_faltantes()), r"---[
- **Resposta** — coluna numérica.
- **Tratamento** — coluna do tratamento.
]---", r"---[
Um modelo (`models/fit`). O card mostra R², CV e a régua do F do tratamento; a
vista **efeitos** é o quadro.
]---", r"---[
tr_flow(reg) |>
  tr_add("plantas", "models/example", dataset = "PlantGrowth") |>
  tr_add("dic", "models/anova_dic", resposta = "weight", tratamento = "group", from = "plantas")
]---", r"---[
`models/anova_table` para o quadro completo; `models/emmeans` para o Tukey;
`models/shapiro_residuals` e `models/levene` para os pressupostos;
`models/kruskal` como alternativa não paramétrica.
]---")),

    trama::tr_node("models/anova_dbc", 
      pressupostos = .tr_models_doc("models/anova_dbc")$pressupostos,
      referencias = .tr_models_doc("models/anova_dbc")$referencias,
      fn = tr_models_anova_dbc, label = "ANOVA · DBC",
      category = "modelo_anova", icon = trama::tr_icon("grid-3x3"),
      description = "Análise de variância de um delineamento em blocos casualizados.",
      inputs = list(dados = T), outputs = list(out = Fm),
      params = list(
        resposta = P("cols", "", label = "Resposta", example = "producao"),
        tratamento = P("cols", "", label = "Tratamento", example = "hibrido"),
        bloco = P("cols", "", label = "Bloco", example = "bloco")),
      help = .tr_models_ajuda(paste0(r"---[
ANOVA do delineamento em blocos casualizados (DBC): cada bloco recebe todos os
tratamentos, sorteados dentro dele. Modelo `resposta ~ bloco + tratamento`.

O bloco tira do resíduo a variação entre áreas (fertilidade, declive, dia de
colheita). O F do bloco não é o objetivo do experimento, mas diz se valeu a
pena bloquear: bloco não significativo é experimento que poderia ter sido DIC.
]---", ajuda_fator, .tr_models_ajuda_faltantes()), r"---[
- **Resposta** — coluna numérica.
- **Tratamento** — coluna do tratamento.
- **Bloco** — coluna do bloco.
]---", r"---[
Um modelo (`models/fit`). O card mostra R², CV e a régua do F do tratamento.
]---", r"---[
tr_flow(reg) |>
  tr_add("milho", "models/example", dataset = "milho_dbc") |>
  tr_add("dbc", "models/anova_dbc", resposta = "producao", tratamento = "hibrido",
         bloco = "bloco", from = "milho") |>
  tr_add("tukey", "models/emmeans", especs = "hibrido", from = "dbc")
]---", r"---[
`models/anova_table`; `models/emmeans` para o Tukey com letras;
`models/tukey_additivity`; `models/anova_factorial` quando o tratamento é uma
combinação de fatores.
]---")),

    trama::tr_node("models/anova_dql", 
      pressupostos = .tr_models_doc("models/anova_dql")$pressupostos,
      referencias = .tr_models_doc("models/anova_dql")$referencias,
      fn = tr_models_anova_dql, label = "ANOVA · DQL",
      category = "modelo_anova", icon = trama::tr_icon("columns-3"),
      description = "Análise de variância de um delineamento em quadrado latino.",
      inputs = list(dados = T), outputs = list(out = Fm),
      params = list(
        resposta = P("cols", "", label = "Resposta", example = "ganho_peso"),
        tratamento = P("cols", "", label = "Tratamento", example = "racao"),
        linha = P("cols", "", label = "Linha", example = "periodo"),
        coluna = P("cols", "", label = "Coluna", example = "lote")),
      help = .tr_models_ajuda(paste0(r"---[
ANOVA do quadrado latino (DQL): duas restrições de casualização cruzadas —
período e animal, linha e coluna do galpão —, com cada tratamento uma vez em
cada linha e em cada coluna. Modelo `resposta ~ linha + coluna + tratamento`.

Linhas, colunas e tratamentos têm de ter o MESMO número de níveis; fora disso o
bloco recusa, porque o ajuste sairia e o quadro seria de outro delineamento.
]---", ajuda_fator, .tr_models_ajuda_faltantes()), r"---[
- **Resposta** — coluna numérica.
- **Tratamento** — coluna do tratamento.
- **Linha**, **Coluna** — as duas restrições.
]---", r"---[
Um modelo (`models/fit`).
]---", r"---[
tr_flow(reg) |>
  tr_add("racao", "models/example", dataset = "racao_dql") |>
  tr_add("dql", "models/anova_dql", resposta = "ganho_peso", tratamento = "racao",
         linha = "periodo", coluna = "lote", from = "racao")
]---", r"---[
`models/anova_dbc` para uma restrição só; `models/emmeans`; `models/anova_table`.
]---")),

    trama::tr_node("models/anova_factorial", 
      pressupostos = .tr_models_doc("models/anova_factorial")$pressupostos,
      referencias = .tr_models_doc("models/anova_factorial")$referencias,
      fn = tr_models_anova_factorial, label = "ANOVA · fatorial",
      category = "modelo_anova", icon = trama::tr_icon("grid-2x2"),
      description = "Análise de variância de um fatorial com 2 ou 3 fatores, em DIC ou em blocos.",
      inputs = list(dados = T), outputs = list(out = Fm),
      params = list(
        resposta = P("cols", "", label = "Resposta", example = "len"),
        fatores = P("cols", "", label = "Fatores (2 ou 3)", example = "supp, dose"),
        bloco = P("cols", "", label = "Bloco (opcional)", example = "block")),
      help = .tr_models_ajuda(paste0(r"---[
ANOVA de um experimento fatorial: os tratamentos são as combinações de 2 ou 3
fatores. Modelo com todas as interações — `resposta ~ A * B` ou
`resposta ~ bloco + A * B * C`. Com **Bloco** em branco, o fatorial é em DIC.

### Leia a interação primeiro

Interação significativa quer dizer que o efeito de um fator DEPENDE do nível do
outro. Nesse caso a média de um fator somada sobre o outro esconde o que
aconteceu, e o que se compara é o desdobramento: no `models/emmeans`, as médias
de `A` com `B` em **Por**. Sem interação, cada fator se lê sozinho.
]---", ajuda_fator, .tr_models_ajuda_faltantes()), r"---[
- **Resposta** — coluna numérica.
- **Fatores** — 2 ou 3 colunas, separadas por vírgula.
- **Bloco** — coluna do bloco; em branco, DIC.
]---", r"---[
Um modelo (`models/fit`). A vista **efeitos** mostra os fatores e as interações
com a régua de cada um.
]---", r"---[
tr_flow(reg) |>
  tr_add("dentes", "models/example", dataset = "ToothGrowth") |>
  tr_add("fat", "models/anova_factorial", resposta = "len", fatores = "supp, dose", from = "dentes") |>
  tr_add("desdobra", "models/emmeans", especs = "dose", por = "supp", from = "fat")
]---", r"---[
`models/anova_table` (tipo II ou III no desbalanceado); `models/emmeans` para o
desdobramento; `models/anova_split_plot` quando um fator está na parcela e o
outro na subparcela.
]---")),

    trama::tr_node("models/anova_split_plot", 
      pressupostos = .tr_models_doc("models/anova_split_plot")$pressupostos,
      referencias = .tr_models_doc("models/anova_split_plot")$referencias,
      fn = tr_models_anova_split_plot, label = "ANOVA · parcela subdividida",
      category = "modelo_anova", icon = trama::tr_icon("square-split-horizontal"),
      description = "Análise de variância de parcelas subdivididas em blocos, com os erros (a) e (b).",
      inputs = list(dados = T), outputs = list(out = Fm),
      params = list(
        resposta = P("cols", "", label = "Resposta", example = "producao"),
        parcela = P("cols", "", label = "Fator da parcela", example = "variedade"),
        subparcela = P("cols", "", label = "Fator da subparcela", example = "nitrogenio"),
        bloco = P("cols", "", label = "Bloco", example = "bloco")),
      help = .tr_models_ajuda(paste0(r"---[
ANOVA de parcelas subdivididas em blocos: um fator é sorteado nas parcelas
grandes (irrigação, variedade, preparo do solo) e o outro nas subparcelas dentro
de cada uma.

### Dois erros

A parcela é a unidade do primeiro fator, e a subparcela a do segundo. Por isso o
quadro tem dois resíduos:

- **Resíduo (a)** — bloco × parcela. Testa o bloco e o fator da parcela.
- **Resíduo (b)** — o de dentro. Testa a subparcela e a interação.

Um `lm` comum testaria o fator da parcela contra o resíduo (b), com mais graus
de liberdade do que existem: p-valor pequeno demais, o engano clássico. O CV
sai para os dois erros.

### Por dentro

O quadro é o do `aov` com `Error(bloco/parcela)`. As médias e comparações usam
o misto equivalente `(1 | bloco:parcela)`, que dá os mesmos F no balanceado e
erros padrão corretos para cada comparação; os pressupostos usam os resíduos do
erro (b). Para dados desbalanceados, ajuste esse misto no `models/lmer`.
]---", ajuda_fator, .tr_models_ajuda_faltantes()), r"---[
- **Resposta** — coluna numérica.
- **Fator da parcela** — o fator sorteado nas parcelas grandes.
- **Fator da subparcela** — o fator sorteado dentro delas.
- **Bloco** — coluna do bloco.
]---", r"---[
Um modelo (`models/fit`). A vista **efeitos** mostra o quadro com os dois
resíduos.
]---", r"---[
tr_flow(reg) |>
  tr_add("aveia", "models/example", dataset = "aveia") |>
  tr_add("split", "models/anova_split_plot", resposta = "producao", parcela = "variedade",
         subparcela = "nitrogenio", bloco = "bloco", from = "aveia") |>
  tr_add("quadro", "models/anova_table", from = "split")
]---", r"---[
`models/anova_table`; `models/emmeans`; `models/random_effects` para a variância
entre parcelas; `models/lmer` para o caso desbalanceado.
]---"))
  )
}
