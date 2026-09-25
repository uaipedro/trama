# As declarações das abas Resumir e Médias.

.tr_models_nos_resumir <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; B <- trama::tr_param_bool
  Fm <- "models/fit"; EF <- "models/effects"; TE <- "data/test"; T <- "data/table"
  list(
    trama::tr_node("models/anova_table", fn = tr_models_anova_table, label = "Quadro da ANOVA",
      category = "modelo_resumir", icon = trama::tr_icon("sheet"),
      description = "O quadro da análise de variância, com SQ tipo I, II ou III e a régua do p-valor por termo.",
      inputs = list(modelo = Fm), outputs = list(out = EF),
      params = list(tipo_sq = E("I", .TR_MODELS_TIPOS_SQ, label = "Soma de quadrados")),
      help = .tr_models_ajuda(r"---[
O quadro da ANOVA de qualquer modelo da coleção: graus de liberdade, somas e
quadrados médios, F e p-valor por termo, com CV e média geral no rodapé.

### Tipo de soma de quadrados

- **I (sequencial)** — cada termo ajustado pelos que vêm ANTES dele na fórmula.
  A ordem importa. É o quadro dos livros de experimentação.
- **II** — cada termo ajustado por todos os outros que não o contêm. Testa os
  efeitos principais sem supor que a interação é zero, e é o que se recomenda
  quando não há interação.
- **III** — cada termo ajustado por todos os outros, inclusive as interações. É
  o do SAS. O bloco reajusta com contrastes de soma zero antes, que é o que faz
  o tipo III testar o efeito na MÉDIA dos outros fatores, e não num nível de
  referência.

No experimento balanceado os três coincidem. Só diferem quando falta parcela.

### Por modelo

- `lm` e delineamentos: F.
- GLM: desvio, com F nas famílias de dispersão estimada e qui-quadrado na
  binomial e Poisson.
- Misto: F com gl do denominador por Satterthwaite (coluna `gl_den`).
- Parcela subdividida: os dois resíduos; só o tipo I.
]---", r"---[
- **Soma de quadrados** — `I`, `II` ou `III`.
]---", r"---[
Um quadro de efeitos (`models/effects`). A vista **significância** mostra cada
termo com a régua do p-valor e as estrelas; a vista **tabela**, o quadro inteiro.
Ligado a um nó da coleção `data`, vira a tabela.
]---", r"---[
tr_flow(reg) |>
  tr_add("dentes", "models/example", dataset = "ToothGrowth") |>
  tr_add("fat", "models/anova_factorial", resposta = "len", fatores = "supp, dose", from = "dentes") |>
  tr_add("quadro", "models/anova_table", tipo_sq = "III", from = "fat")
]---", r"---[
`models/anova_dbc` e os outros delineamentos; `models/emmeans` para comparar as
médias depois do F; `models/coefficients` para os coeficientes.
]---", teste = TRUE)),

    trama::tr_node("models/coefficients", fn = tr_models_coefficients, label = "Coeficientes",
      category = "modelo_resumir", icon = trama::tr_icon("variable"),
      description = "Estimativa, erro padrão, estatística, p-valor e intervalo de confiança de cada coeficiente.",
      inputs = list(modelo = Fm), outputs = list(out = EF),
      params = list(exponenciar = B(FALSE, label = "Exponenciar (GLM)"),
                    escala = trama::tr_param_enum("unidade", .TR_MODELS_ESCALAS, label = "Escala"),
                    confianca = trama::tr_param_num(0.95, min = 0.5, max = 0.999, step = 0.01, label = "Confiança")),
      help = .tr_models_ajuda(r"---[
Os coeficientes do modelo, um por linha: estimativa, erro padrão, estatística
(t ou z), p-valor e intervalo de confiança (95% no padrão; as colunas levam o
nível no nome: `li_95`, `ls_95`, ou `li_90`, `ls_90` a 90%).

- `lm`: t, com intervalo exato.
- GLM: z (ou t nas famílias de dispersão estimada), com intervalo de Wald, na
  escala da ligação. **Exponenciar** devolve `exp()` da estimativa e do
  intervalo: razão de chances na binomial, razão de taxas na Poisson.
- Misto: só os efeitos fixos, com gl de Satterthwaite.

### Fator vira contraste

Um fator com k níveis vira k − 1 coeficientes, cada um a diferença para o
PRIMEIRO nível. O p-valor de `sprayC` é o de C contra A, e não o de C contra
todos. Para comparar níveis, use o `models/emmeans`.

### Escala

**unidade** (padrão) é o coeficiente de sempre: quanto muda a resposta (ou o
log-odds, no GLM) quando a preditora sobe UMA unidade. **desvio padrão**
multiplica estimativa, erro padrão e intervalo pelo desvio padrão da coluna —
quanto muda quando a preditora sobe um DP —, o que deixa comparáveis
preditoras em unidades diferentes (cm e kg). A estatística e o p-valor não
mudam. Numa dummy de fator o DP é o da coluna 0/1, e a leitura fica estranha:
prefira a escala da unidade para fatores.

Com **exponenciar** e desvio padrão juntos, sai a razão de chances por DP.

### Classificadores de outras coleções

Uma logística multinomial (da `multi`) tem um coeficiente por classe para cada
preditora: a tabela ganha a coluna `grupo`, e a régua mostra `classe · termo`.

Na parcela subdividida os coeficientes misturam os dois erros, e o bloco recusa.
]---", r"---[
- **Exponenciar** — só no GLM.
- **Escala** — `unidade` (padrão) ou `desvio padrão` da preditora.
- **Confiança** — o nível do intervalo (padrão 0,95).
]---", r"---[
Um quadro de efeitos (`models/effects`), com a régua por coeficiente.
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("logit", "models/glm", formula = "am ~ wt", familia = "binomial", from = "carros") |>
  tr_add("coef", "models/coefficients", exponenciar = TRUE, from = "logit")
]---", r"---[
`models/lm`; `models/fit_stats` para as medidas de ajuste; `models/emmeans` para
comparar níveis de um fator.
]---", teste = TRUE)),

    trama::tr_node("models/effect_size", fn = tr_models_effect_size, label = "Tamanho de efeito (ANOVA)",
      category = "modelo_resumir", icon = trama::tr_icon("ruler"),
      description = "Eta², eta² parcial e ômega² de cada termo da ANOVA: quanto da variação cada um explica.",
      inputs = list(modelo = Fm), outputs = list(out = T),
      params = list(tipo_sq = E("I", .TR_MODELS_TIPOS_SQ, label = "Soma de quadrados")),
      help = .tr_models_ajuda(r"---[
O p-valor do quadro diz SE um termo tem efeito; o tamanho de efeito diz de
QUANTO. Uma linha por termo, calculada do quadro da ANOVA com o erro de cada
termo (na parcela subdividida, o erro (a) para a parcela e o (b) para o resto;
a coluna `erro` diz qual).

- **eta2** — SQ do termo / SQ total: a fração da variação total que o termo
  explica. Diminui quando o modelo ganha termos.
- **eta2_parcial** — SQ / (SQ + SQ do erro): o termo contra o próprio erro. É o
  que o SPSS mostra, e o que se compara entre experimentos com delineamentos
  diferentes. Não soma 1 entre os termos.
- **omega2** — (SQ − gl · QM do erro) / (SQ total + QM do erro): o eta²
  corrigido do viés para cima em amostra pequena. Sai negativo quando F < 1:
  leia como zero.

Referências de Cohen (1988) para eta²: 0,01 pequeno, 0,06 médio, 0,14 grande —
régua genérica, que a área de cada um deve substituir quando tiver a sua.
]---", r"---[
- **Soma de quadrados** — `I`, `II` ou `III`, como no `models/anova_table`.
]---", r"---[
Uma tabela (`data/table`): `termo`, `gl`, `eta2`, `eta2_parcial`, `omega2` e
`erro`.
]---", r"---[
tr_flow(reg) |>
  tr_add("milho", "models/example", dataset = "milho_dbc") |>
  tr_add("dbc", "models/anova_dbc", resposta = "producao", tratamento = "hibrido",
         bloco = "bloco", from = "milho") |>
  tr_add("efeito", "models/effect_size", from = "dbc")
]---", r"---[
`models/anova_table` para o quadro; `models/cohen_d` para dois grupos.
]---")),

    trama::tr_node("models/fit_stats", fn = tr_models_fit_stats, label = "Medidas de ajuste",
      category = "modelo_resumir", icon = trama::tr_icon("gauge"),
      description = "R², R² ajustado, R² marginal e condicional, CV, AIC, BIC e log-verossimilhança, em colunas fixas.",
      inputs = list(modelo = Fm), outputs = list(out = T),
      help = .tr_models_ajuda(r"---[
Uma linha com as medidas de ajuste do modelo. As colunas são SEMPRE as mesmas,
com NA onde a medida não existe, para que três modelos ligados num
`data/bind_rows` virem a tabela de comparação.

- `r2`, `r2_ajustado` — `lm` e delineamentos.
- `r2_marginal`, `r2_condicional` — misto (Nakagawa & Schielzeth 2013): a
  variância explicada só pelos fixos, e pelos fixos e aleatórios juntos.
- `desvio_explicado` — GLM: 1 − desvio residual / desvio nulo.
- `sigma` — desvio padrão residual.
- `cv_pct` — coeficiente de variação (100 · √QM do resíduo / média). No
  experimento, é a medida de precisão que as revistas pedem.
- `aic`, `bic`, `log_verossimilhanca` — para comparar modelos NÃO aninhados
  (menor AIC, melhor), ajustados nas mesmas linhas.
]---", r"---[
Nenhum.
]---", r"---[
Uma tabela (`data/table`) de uma linha.
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("reg", "models/lm", formula = "mpg ~ wt + hp", from = "carros") |>
  tr_add("medidas", "models/fit_stats", from = "reg")
]---", r"---[
`models/compare` para modelos aninhados; `data/bind_rows` para juntar as medidas
de vários modelos.
]---")),

    trama::tr_node("models/random_effects", fn = tr_models_random_effects, label = "Efeitos aleatórios",
      category = "modelo_resumir", icon = trama::tr_icon("dices"),
      description = "Os componentes de variância de um modelo misto, com a proporção de cada um.",
      inputs = list(modelo = Fm), outputs = list(out = T),
      help = .tr_models_ajuda(r"---[
Os componentes de variância de um misto: a variância e o desvio padrão de cada
termo aleatório e do resíduo, as correlações entre intercepto e inclinação, e a
`proporcao` que cada intercepto e o resíduo representam da soma deles.

A proporção do intercepto de um grupo é a correlação intraclasse (ICC): o
quanto duas observações do mesmo bloco, animal ou pessoa se parecem. Não é
calculada para inclinações, cuja variância está em outra escala.

Aceita também a parcela subdividida: o componente `bloco:parcela` é a variância
entre parcelas, o erro (a).
]---", r"---[
Nenhum.
]---", r"---[
Uma tabela (`data/table`) com `grupo`, `componente`, `variancia`,
`desvio_padrao`, `correlacao` e `proporcao`.
]---", r"---[
tr_flow(reg) |>
  tr_add("sono", "models/example", dataset = "sleepstudy") |>
  tr_add("misto", "models/lmer", formula = "Reaction ~ Days + (Days | Subject)", from = "sono") |>
  tr_add("var", "models/random_effects", from = "misto")
]---", r"---[
`models/lmer`; `models/random_test` para testar se um termo aleatório é
necessário.
]---")),

    trama::tr_node("models/residuals", fn = tr_models_residuals, label = "Resíduos",
      category = "modelo_resumir", icon = trama::tr_icon("chart-scatter"),
      description = "A tabela do ajuste com os valores ajustados, os resíduos e os resíduos padronizados.",
      inputs = list(modelo = Fm), outputs = list(out = T),
      help = .tr_models_ajuda(r"---[
Devolve as linhas usadas no ajuste com três colunas à direita:

- `ajustado` — o valor previsto pelo modelo.
- `residuo` — observado − ajustado (no GLM, o resíduo de desvio).
- `residuo_padronizado` — o resíduo dividido pelo seu erro padrão.
  Padronizado acima de 3 em módulo é observação para conferir no caderno de
  campo.

Se a tabela já tem uma coluna com um desses nomes, a nova ganha o sufixo
`_modelo`, em vez de sobrescrever.

Na parcela subdividida os resíduos são os do erro (b). É a tabela para desenhar
o que o `models/plot_diagnostics` não desenha — resíduo por bloco num
`view/points`, por exemplo — ou para filtrar os discrepantes num `data/filter`.
]---", r"---[
Nenhum.
]---", r"---[
Uma tabela (`data/table`).
]---", r"---[
tr_flow(reg) |>
  tr_add("milho", "models/example", dataset = "milho_dbc") |>
  tr_add("dbc", "models/anova_dbc", resposta = "producao", tratamento = "hibrido",
         bloco = "bloco", from = "milho") |>
  tr_add("res", "models/residuals", from = "dbc") |>
  tr_add("grandes", "data/filter", expr = "abs(residuo_padronizado) > 2", from = "res")
]---", r"---[
`models/plot_diagnostics`; `models/shapiro_residuals`; `view/points`.
]---")),

    trama::tr_node("models/plot_diagnostics", role = "avaliacao", fn = tr_models_plot_diagnostics, label = "Diagnóstico dos resíduos",
      category = "modelo_resumir", icon = trama::tr_icon("microscope"),
      description = "Quatro painéis: resíduos × ajustados, Q-Q normal, escala-locação e histograma.",
      inputs = list(modelo = Fm), outputs = list(out = "view/plot"),
      params = .tr_models_props(.aspecto = "1:1"),
      help = .tr_models_ajuda(r"---[
Os quatro gráficos que se olham antes de acreditar num quadro:

- **resíduos × ajustados** — deve ser uma nuvem sem forma em torno do zero.
  Funil é variância que cresce com a média (transforme a resposta, ou use um
  GLM); curva é termo faltando.
- **Q-Q normal** — os resíduos padronizados contra os quantis da normal. Pontos
  na diagonal são normalidade; caudas que fogem são assimetria ou discrepantes.
- **escala-locação** — a raiz do resíduo padronizado em módulo: a linha subindo
  é o mesmo funil, mais fácil de ver.
- **histograma** — a forma da distribuição.

Com 10 ou mais observações, uma curva suavizada (loess) ajuda a ver tendência.
O gráfico diz COMO o pressuposto falhou; os testes (`models/shapiro_residuals`,
`models/levene`) dizem SE falhou — e, com muitas observações, rejeitam desvios
que não importam. Olhe os dois.
]---", r"---[
Só os de aparência (abaixo).
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("insetos", "models/example", dataset = "InsectSprays") |>
  tr_add("dic", "models/anova_dic", resposta = "count", tratamento = "spray", from = "insetos") |>
  tr_add("diag", "models/plot_diagnostics", from = "dic")
]---", r"---[
`models/residuals` para a tabela; `models/shapiro_residuals`, `models/levene` e
`models/breusch_pagan` para os testes.
]---", grafico = TRUE)),

    trama::tr_node("models/plot_caterpillar", fn = tr_models_plot_caterpillar, label = "Gráfico de lagarta",
      category = "modelo_resumir", icon = trama::tr_icon("list-ordered"),
      description = "Os efeitos aleatórios previstos de um misto, um por nível do grupo, com intervalo e a faixa do desvio padrão.",
      inputs = list(modelo = Fm), outputs = list(out = "view/plot"),
      params = .tr_models_props(
        grupo = P("cols", "", label = "Grupo aleatório", example = "Subject"),
        intervalo = E("IC", .TR_MODELS_INTERVALOS, label = "Intervalo"),
        confianca = trama::tr_param_num(0.95, min = 0.5, max = 0.999, step = 0.01, label = "Confiança (IC)"),
        faixa_desvio = B(TRUE, label = "Faixa de ±1 desvio padrão"),
        ordenar = B(TRUE, label = "Ordenar pelo efeito"),
        .aspecto = "3:4", .legenda = "abaixo"),
      help = .tr_models_ajuda(r"---[
O gráfico de lagarta (*caterpillar plot*): cada nível do grupo aleatório — cada
sujeito, bloco, parcela — é uma linha, com o efeito previsto (o BLUP, quanto
aquele nível se afasta da média do modelo) e o seu intervalo. Ordenados, os
pontos formam a "lagarta". Um painel por termo aleatório: com `(Days | Subject)`,
um para o intercepto e outro para a inclinação, na MESMA ordem de sujeitos, para
que se veja se quem começa alto também sobe mais rápido.

### Intervalo

A largura das barras vem da variância condicional do `lme4`:

- **IC** — intervalo com a **Confiança** escolhida: 1,96 erros padrão a 0,95,
  1,64 a 0,90, 2,58 a 0,99.
- **± 1 EP**, **± 2 EP** — com muitos níveis próximos, 1 EP mostra a ordem sem
  virar um borrão de barras sobrepostas.

Em cor de destaque os níveis cujo intervalo não cruza o zero; em cinza, os que
cruzam.

### Faixa do desvio padrão

A faixa sombreada vai de −1 a +1 desvio padrão do componente de variância — o
MESMO número da coluna `desvio_padrao` do `models/random_effects`. É o que torna
o gráfico comparável com o resultado: a variância do grupo é a largura típica
da lagarta, e um nível cujo intervalo sai inteiro da faixa é atípico para aquele
grupo.

O BLUP é encolhido em direção a zero (tanto mais quanto menos dados o nível
tem): o gráfico mostra o que o modelo prevê, e não a média crua de cada nível.

Aceita também a parcela subdividida, pelo misto equivalente (grupo
`bloco:parcela`).
]---", r"---[
- **Grupo aleatório** — o fator de agrupamento; em branco, o primeiro do modelo.
- **Intervalo** — a largura das barras.
- **Faixa de ±1 desvio padrão** — sombrear o desvio padrão do componente.
- **Ordenar pelo efeito** — ordenar os níveis pelo primeiro termo.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("sono", "models/example", dataset = "sleepstudy") |>
  tr_add("misto", "models/lmer", formula = "Reaction ~ Days + (Days | Subject)", from = "sono") |>
  tr_add("lagarta", "models/plot_caterpillar", intervalo = "± 1 EP", from = "misto")
]---", r"---[
`models/random_effects` para os desvios padrão; `models/random_test` para testar
os termos; `models/lmer`.
]---", grafico = TRUE)),

    trama::tr_node("models/compare", role = "avaliacao", fn = tr_models_compare, label = "Comparar modelos",
      category = "modelo_resumir", icon = trama::tr_icon("git-compare"),
      description = "Dois modelos aninhados: os termos a mais melhoram o ajuste? (F ou razão de verossimilhança)",
      inputs = list(modelo = Fm, outro = Fm), outputs = list(out = TE),
      help = .tr_models_ajuda(r"---[
Compara dois modelos ANINHADOS — o menor tem todos os seus termos no maior — e
testa se os termos a mais melhoram o ajuste. Não importa em que entrada vai
cada um: o bloco descobre qual é o menor.

- `lm`: F de modelos aninhados.
- GLM: qui-quadrado da diferença de desvios (F nas famílias de dispersão
  estimada).
- Misto: razão de verossimilhança, com os dois reajustados por máxima
  verossimilhança (REML não compara efeitos fixos diferentes).

Os dois modelos têm de usar as MESMAS linhas: se um tem uma coluna com
faltante que o outro não usa, a comparação mediria os dados, e o bloco recusa.
Para modelos não aninhados, compare o AIC em `models/fit_stats`.
]---", r"---[
Nenhum. Entradas: **modelo** e **outro**.
]---", r"---[
Um teste (`data/test`), com a régua do p-valor.
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("m1", "models/lm", formula = "mpg ~ wt", from = "carros") |>
  tr_add("m2", "models/lm", formula = "mpg ~ wt + hp", from = "carros") |>
  tr_add("cmp", "models/compare", from = "m1") |>
  tr_link("m2", "cmp:outro")
]---", r"---[
`models/fit_stats` para o AIC; `models/random_test` para os termos aleatórios de
um misto; `models/anova_table` para os termos um a um.
]---", teste = TRUE)),

    trama::tr_node("models/random_test", role = "avaliacao", fn = tr_models_random_test, label = "Teste dos aleatórios",
      category = "modelo_resumir", icon = trama::tr_icon("shuffle"),
      description = "Razão de verossimilhança para cada termo aleatório de um modelo misto.",
      inputs = list(modelo = Fm), outputs = list(out = EF),
      help = .tr_models_ajuda(r"---[
Tira cada termo aleatório do misto, um de cada vez, e testa por razão de
verossimilhança se o ajuste piora (`lmerTest::ranova`). Uma inclinação aleatória
`(Days | Subject)` é reduzida a `(1 | Subject)`; um intercepto `(1 | bloco)` sai.

A hipótese nula põe a variância na FRONTEIRA (zero), onde a distribuição
qui-quadrado não vale exatamente: o p-valor é conservador — maior do que
deveria. Um termo que sai significativo aqui é seguro.
]---", r"---[
Nenhum.
]---", r"---[
Um quadro de efeitos (`models/effects`), uma linha por termo, com a régua.
]---", r"---[
tr_flow(reg) |>
  tr_add("sono", "models/example", dataset = "sleepstudy") |>
  tr_add("misto", "models/lmer", formula = "Reaction ~ Days + (Days | Subject)", from = "sono") |>
  tr_add("aleat", "models/random_test", from = "misto")
]---", r"---[
`models/random_effects` para o tamanho das variâncias; `models/compare` para
comparar duas estruturas escolhidas à mão.
]---", teste = TRUE))
  )
}

.tr_models_nos_medias <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; B <- trama::tr_param_bool
  N <- trama::tr_param_num
  list(
    trama::tr_node("models/emmeans", fn = tr_models_emmeans, label = "Médias ajustadas",
      category = "modelo_medias", icon = trama::tr_icon("chart-column"),
      description = "Médias ajustadas (emmeans) com intervalo de confiança e letras de comparação (Tukey e outros).",
      inputs = list(modelo = "models/fit"), outputs = list(out = "models/emm"),
      params = list(
        especs = P("cols", "", label = "Médias de", example = "hibrido"),
        por = P("cols", "", label = "Por (desdobramento)", example = "supp"),
        ajuste = E("tukey", .TR_MODELS_AJUSTES, label = "Ajuste das letras"),
        confianca = N(0.95, min = 0.5, max = 0.999, step = 0.01, label = "Confiança"),
        escala = E("resposta", c("resposta", "ligação"), label = "Escala (GLM)")),
      help = .tr_models_ajuda(r"---[
As médias ajustadas pelo modelo (`emmeans`), com erro padrão, intervalo de
confiança e as LETRAS: médias que compartilham uma letra não diferem ao nível
escolhido. "a" é o grupo da maior média.

### Médias ajustadas, e não médias da tabela

A média ajustada é a que o modelo prevê para cada nível, fazendo a média sobre
os outros fatores com peso igual. No balanceado é a média da tabela; quando falta
parcela, ou há covariável, ela corrige o que a média crua mistura. É a LSMEANS do
SAS, e é a que o Tukey compara.

### Desdobramento

**Médias de** diz os fatores comparados; **Por**, os fatores de condição. Com
`dose` em Médias de e `supp` em Por, as doses são comparadas DENTRO de cada
suplemento, com letras separadas — o desdobramento de uma interação. Com
`supp, dose` em Médias de, as seis combinações entram na mesma comparação.

### Letras

Pelo algoritmo de inserir e absorver (Piepho 2004) sobre as comparações par a
par com o ajuste escolhido: `tukey` (padrão), `bonferroni`, `holm`, `sidak` ou
`nenhum` (o t sem correção, que infla o erro com muitas médias).

O fator tem de ser FATOR no modelo: nos blocos de ANOVA isso é automático; no
`models/lm`, use `factor()` na fórmula.
]---", r"---[
- **Médias de** — 1 a 3 fatores.
- **Por** — fatores de condição (opcional).
- **Ajuste das letras** — a correção das comparações.
- **Confiança** — o nível de confiança do intervalo; as letras usam
  alfa = 1 − confiança (padrão 0,95, letras a 5%).
- **Escala (GLM)** — `resposta` (contagens, proporções) ou `ligação` (log,
  logit). Não muda nada em modelo gaussiano.
]---", r"---[
Médias ajustadas (`models/emm`). O card é o gráfico das médias com o intervalo e
as letras; a vista **resumo** diz o ajuste. Ligado a um nó da coleção `data`,
vira a tabela com as letras em `grupo` — pronta para o artigo.
]---", r"---[
tr_flow(reg) |>
  tr_add("milho", "models/example", dataset = "milho_dbc") |>
  tr_add("dbc", "models/anova_dbc", resposta = "producao", tratamento = "hibrido",
         bloco = "bloco", from = "milho") |>
  tr_add("tukey", "models/emmeans", especs = "hibrido", ajuste = "tukey", from = "dbc")
]---", r"---[
`models/pairwise` para os p-valores de cada par e o Dunnett; `models/plot_means`
para o gráfico com título e proporção; `models/anova_factorial` para o
desdobramento.
]---")),

    trama::tr_node("models/pairwise", fn = tr_models_pairwise, label = "Comparações de médias",
      category = "modelo_medias", icon = trama::tr_icon("git-compare"),
      description = "Todas as diferenças entre pares, ou cada tratamento contra um controle (Dunnett), com p-valor ajustado.",
      inputs = list(medias = "models/emm"), outputs = list(out = "models/effects"),
      params = list(
        metodo = E("todos os pares", c("todos os pares", "contra controle"), label = "Comparar"),
        controle = P("text", "", label = "Controle", example = "ctrl"),
        ajuste = E("tukey", c(.TR_MODELS_AJUSTES, "dunnett"), label = "Ajuste")),
      help = .tr_models_ajuda(r"---[
As comparações que estão por trás das letras, uma por linha: a diferença, o erro
padrão, o intervalo e o p-valor AJUSTADO, com a régua.

- **todos os pares** — k(k − 1)/2 diferenças. Com `tukey`, é o teste de Tukey.
- **contra controle** — cada tratamento menos o **Controle**. Com `dunnett`, é o
  teste de Dunnett, que tem mais poder que o Tukey quando só essas comparações
  interessam. Pede médias de UM fator.

Com **Por** preenchido no `models/emmeans`, as comparações são feitas dentro de
cada condição, e a condição aparece no nome da linha.

No GLM com médias na escala da resposta, a diferença vira RAZÃO (A / C): no log,
diferença é razão.
]---", r"---[
- **Comparar** — `todos os pares` ou `contra controle`.
- **Controle** — o nível do controle, escrito como na tabela.
- **Ajuste** — `tukey`, `bonferroni`, `holm`, `sidak`, `nenhum` ou `dunnett` (só
  contra controle).
]---", r"---[
Um quadro de efeitos (`models/effects`).
]---", r"---[
tr_flow(reg) |>
  tr_add("plantas", "models/example", dataset = "PlantGrowth") |>
  tr_add("dic", "models/anova_dic", resposta = "weight", tratamento = "group", from = "plantas") |>
  tr_add("medias", "models/emmeans", especs = "group", from = "dic") |>
  tr_add("dunnett", "models/pairwise", metodo = "contra controle", controle = "ctrl",
         ajuste = "dunnett", from = "medias")
]---", r"---[
`models/emmeans`; `models/plot_means`.
]---", teste = TRUE)),

    trama::tr_node("models/linear_hypothesis", fn = tr_models_linear_hypothesis, label = "Contrastes (F)",
      category = "modelo_medias", icon = trama::tr_icon("divide"),
      description = "Teste F da hipótese linear geral: você escreve os contrastes, nas médias de um fator ou nos coeficientes.",
      inputs = list(modelo = "models/fit"), outputs = list(out = "data/test"),
      params = list(
        hipoteses = P("expr", "", label = "Contrastes (um por linha, ou separados por ;)",
                      example = "ctrl vs trat: 2 -1 -1; trt1 vs trt2: trt1 - trt2"),
        fator = P("cols", "", label = "Fator (em branco: coeficientes)", example = "group")),
      help = .tr_models_ajuda(r"---[
Testa a hipótese linear geral **H0: Lβ = c** — todos os contrastes digitados ao
mesmo tempo, com um F só. Cada linha escrita é uma linha de L; o F tem tantos
graus de liberdade no numerador quanto contrastes.

É o teste dos **contrastes planejados**: em vez de comparar todos os pares,
pergunta-se exatamente o que o experimento foi desenhado para responder.

### Nas médias de um fator (Fator preenchido)

Cada contraste combina as médias ajustadas dos níveis, e há dois jeitos de
escrevê-lo:

- **números**, um por nível, na ordem dos níveis: `2 -1 -1` (controle contra a
  média dos dois tratamentos), `0 1 -1`, `-3 -1 1 3` (tendência linear de 4
  doses igualmente espaçadas).
- **pelos nomes dos níveis**: `ctrl - (trt1 + trt2) / 2`, `H3 - H1`. Nível cujo
  nome não é simples vai entre crases: `` `0.6cwt` - `0.0cwt` ``.

Um rótulo antes de dois-pontos dá nome ao contraste — `linear: -3 -1 1 3` —, e
é por ele que o contraste aparece no detalhe.

Os contrastes de verdade somam zero; se algum não somar, a nota avisa (continua
sendo uma função estimável válida, só não é contraste). Contrastes ortogonais —
os coeficientes de dois deles multiplicados e somados dão zero — dividem a soma
de quadrados do tratamento em partes independentes.

Funciona em todo modelo da coleção: no delineamento, no GLM (qui-quadrado, na
escala da ligação), no misto e na parcela subdividida.

### Nos coeficientes (Fator em branco)

As hipóteses são escritas com os nomes dos coeficientes, que aparecem no
`models/coefficients`, na sintaxe do `car::linearHypothesis`:

- `wt = 0; hp = 0` — os dois coeficientes são zero juntos.
- `sprayB = sprayF` — dois coeficientes iguais.
- `wt = 2 * hp`, `x1 + x2 = 1`.

No `lm` o teste é F; no GLM de família binomial ou Poisson, qui-quadrado de
Wald; no misto, F com gl de Satterthwaite (só com lado direito zero).

### O detalhe

A vista **detalhe** traz cada contraste com a sua estimativa e o p-valor
individual (nas médias). Rejeitada a hipótese conjunta, é ali que se vê qual
contraste a derrubou.

Contrastes linearmente dependentes — um que é combinação dos outros — são
recusados: não acrescentam hipótese, e o F ganharia um grau de liberdade que não
existe.
]---", r"---[
- **Contrastes** — um por linha ou separados por `;`, cada um com rótulo
  opcional (`nome: ...`).
- **Fator** — a coluna-fator cujas médias os contrastes combinam. Em branco, as
  hipóteses são sobre os coeficientes.
]---", r"---[
Um teste (`data/test`), com a régua do p-valor da hipótese conjunta.
]---", r"---[
tr_flow(reg) |>
  tr_add("plantas", "models/example", dataset = "PlantGrowth") |>
  tr_add("dic", "models/anova_dic", resposta = "weight", tratamento = "group", from = "plantas") |>
  tr_add("contr", "models/linear_hypothesis", fator = "group",
         hipoteses = "ctrl vs trat: 2 -1 -1; trt1 vs trt2: trt1 - trt2", from = "dic")
]---", r"---[
`models/pairwise` para todos os pares; `models/emmeans` para as médias que os
contrastes combinam; `models/coefficients` para os nomes dos coeficientes;
`models/compare` quando a hipótese é tirar termos do modelo.
]---", teste = TRUE)),

    trama::tr_node("models/duncan", fn = tr_models_duncan, label = "Duncan",
      category = "modelo_medias", icon = trama::tr_icon("chart-column"),
      description = "Teste de Duncan (amplitude múltipla): letras de agrupamento das médias.",
      inputs = list(modelo = "models/fit"), outputs = list(out = "models/emm"),
      params = list(
        tratamento = P("cols", "", label = "Tratamento", example = "hibrido"),
        confianca = N(0.95, min = 0.5, max = 0.999, step = 0.01, label = "Confiança")),
      help = .tr_models_ajuda(r"---[
O teste de Duncan agrupa as médias do tratamento por amplitudes múltiplas
(`agricolae::duncan.test`). O nível de proteção cresce com o número de médias
entre as duas comparadas, e por isso ele separa mais que o Tukey — mais poder, e
mais falsos positivos quando há muitas médias. É comum nas revistas de ciências
agrárias; quando não houver tradição a seguir, o Tukey do `models/emmeans` é o
mais conservador.

Usa o QM e os gl do resíduo do modelo. Na parcela subdividida, o fator da
parcela é testado com o erro (a) e o da subparcela com o (b), e a nota diz qual.
Com dois ou três fatores em **Tratamento**, as combinações entram juntas.

As médias são as da tabela: no desbalanceado elas diferem das ajustadas, e a
nota avisa.
]---", r"---[
- **Tratamento** — o fator (ou até 3, separados por vírgula).
- **Confiança** — padrão 0,95: as letras saem a alfa = 1 − confiança (5%).
]---", r"---[
Médias com letras (`models/emm`): o card é o gráfico com as letras, e a tabela
sai pelo adaptador, como no `models/emmeans`.
]---", r"---[
tr_flow(reg) |>
  tr_add("milho", "models/example", dataset = "milho_dbc") |>
  tr_add("dbc", "models/anova_dbc", resposta = "producao", tratamento = "hibrido",
         bloco = "bloco", from = "milho") |>
  tr_add("duncan", "models/duncan", tratamento = "hibrido", from = "dbc")
]---", r"---[
`models/waller_duncan`; `models/emmeans` para o Tukey; `models/plot_means`.
]---")),

    trama::tr_node("models/waller_duncan", fn = tr_models_waller_duncan, label = "Waller-Duncan",
      category = "modelo_medias", icon = trama::tr_icon("chart-column"),
      description = "Teste de Waller-Duncan (bayesiano, razão K): letras de agrupamento das médias.",
      inputs = list(modelo = "models/fit"), outputs = list(out = "models/emm"),
      params = list(
        tratamento = P("cols", "", label = "Tratamento", example = "hibrido"),
        k = N(100, min = 2, max = 10000, step = 50, label = "Razão K")),
      help = .tr_models_ajuda(r"---[
O teste de Waller-Duncan agrupa as médias por uma regra bayesiana
(`agricolae::waller.test`). No lugar do alfa entra a **razão K**: quanto um erro
tipo I custa a mais que um tipo II.

| K | equivale, grosso modo, a |
|---|---|
| 50 | 10% |
| 100 | 5% |
| 500 | 1% |

A diferença crítica depende do F do tratamento: quando o quadro mostra
evidência forte (F grande), o teste separa mais; quando o F é pequeno, fica
conservador e tende a não separar nada. É o que o torna diferente de Tukey e
Duncan, que usam a mesma régua qualquer que seja o F. A nota traz a diferença
crítica usada.

Erro, parcela subdividida, combinações e médias da tabela: como no
`models/duncan`.
]---", r"---[
- **Tratamento** — o fator (ou até 3).
- **Razão K** — 100 (padrão, ~5%), 500 (~1%), 50 (~10%).
]---", r"---[
Médias com letras (`models/emm`).
]---", r"---[
tr_flow(reg) |>
  tr_add("aveia", "models/example", dataset = "aveia") |>
  tr_add("split", "models/anova_split_plot", resposta = "producao", parcela = "variedade",
         subparcela = "nitrogenio", bloco = "bloco", from = "aveia") |>
  tr_add("waller", "models/waller_duncan", tratamento = "nitrogenio", from = "split")
]---", r"---[
`models/duncan`; `models/emmeans`; `models/anova_table` para o F que o teste usa.
]---")),

    trama::tr_node("models/scott_knott", fn = tr_models_scott_knott, label = "Scott-Knott",
      category = "modelo_medias", icon = trama::tr_icon("chart-column"),
      description = "Teste de Scott-Knott: agrupa as médias em grupos sem sobreposição.",
      inputs = list(modelo = "models/fit"), outputs = list(out = "models/emm"),
      params = list(
        tratamento = P("cols", "", label = "Tratamento", example = "hibrido"),
        confianca = N(0.95, min = 0.5, max = 0.999, step = 0.01, label = "Confiança")),
      help = .tr_models_ajuda(r"---[
O teste de Scott & Knott (1974) parte as médias ordenadas em grupos por
divisões sucessivas: entre todos os cortes possíveis, escolhe o que deixa a
maior soma de quadrados entre os dois lados e testa, por uma razão com
distribuição qui-quadrado, se o corte é real. Se for, cada lado é partido de
novo; se não, as médias daquele lado formam um grupo.

A diferença para Tukey e Duncan é que os grupos **não se sobrepõem**: cada
média tem UMA letra, nunca "ab". Com muitos tratamentos (cultivares, linhagens,
clones) é o que deixa a tabela legível, e por isso é o teste de agrupamento mais
usado nas revistas brasileiras de ciências agrárias.

Usa o QM e os gl do resíduo do modelo; na parcela subdividida, o erro (a) para
o fator da parcela e o (b) para o da subparcela, como no `models/duncan`. As
médias são as da tabela: no desbalanceado, a nota avisa.
]---", r"---[
- **Tratamento** — o fator (ou até 3, separados por vírgula).
- **Confiança** — padrão 0,95: os cortes são testados a alfa = 1 − confiança (5%).
]---", r"---[
Médias com letras (`models/emm`): o card é o gráfico com as letras, e a tabela
sai pelo adaptador, como no `models/emmeans`.
]---", r"---[
tr_flow(reg) |>
  tr_add("milho", "models/example", dataset = "milho_dbc") |>
  tr_add("dbc", "models/anova_dbc", resposta = "producao", tratamento = "hibrido",
         bloco = "bloco", from = "milho") |>
  tr_add("sk", "models/scott_knott", tratamento = "hibrido", from = "dbc")
]---", r"---[
`models/duncan`; `models/emmeans` para o Tukey; `models/plot_means`.
]---")),

    trama::tr_node("models/plot_means", fn = tr_models_plot_means, label = "Gráfico de médias",
      category = "modelo_medias", icon = trama::tr_icon("chart-no-axes-column"),
      description = "Médias ajustadas com intervalo de confiança e as letras de comparação.",
      inputs = list(medias = "models/emm"), outputs = list(out = "view/plot"),
      params = .tr_models_props(letras = B(TRUE, label = "Letras")),
      help = .tr_models_ajuda(r"---[
O gráfico das médias ajustadas: ponto na média, barra no intervalo de confiança
e as letras acima. É o mesmo desenho do card do `models/emmeans`, com título,
rótulos e proporção escolhidos.

Com dois fatores em **Médias de**, o segundo vira a cor; com **Por**, um painel
por condição.

O intervalo de cada média não é o teste da diferença: dois intervalos que se
tocam ainda podem vir de médias diferentes. Quem diz se diferem são as letras.
]---", r"---[
- **Letras** — escrever as letras acima dos intervalos.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("dentes", "models/example", dataset = "ToothGrowth") |>
  tr_add("fat", "models/anova_factorial", resposta = "len", fatores = "supp, dose", from = "dentes") |>
  tr_add("medias", "models/emmeans", especs = "dose, supp", from = "fat") |>
  tr_add("graf", "models/plot_means", titulo = "Comprimento por dose", from = "medias")
]---", r"---[
`models/emmeans`; `models/pairwise`.
]---", grafico = TRUE))
  )
}
