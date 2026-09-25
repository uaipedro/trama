# As declarações das abas Pressupostos e Testes.

.tr_models_nos_pressupostos <- function() {
  E <- trama::tr_param_enum
  Fm <- "models/fit"; TE <- "models/test"
  exemplo_dbc <- function(no, extra = "") sprintf(r"---[
tr_flow(reg) |>
  tr_add("milho", "models/example", dataset = "milho_dbc") |>
  tr_add("dbc", "models/anova_dbc", resposta = "producao", tratamento = "hibrido",
         bloco = "bloco", from = "milho") |>
  tr_add("teste", "%s", %sfrom = "dbc")
]---", no, extra)
  list(
    trama::tr_node("models/shapiro_residuals", 
      pressupostos = .tr_models_doc("models/shapiro_residuals")$pressupostos,
      referencias = .tr_models_doc("models/shapiro_residuals")$referencias,
      fn = tr_models_shapiro_residuals, label = "Normalidade dos resíduos",
      category = "modelo_pressupostos", icon = trama::tr_icon("chart-area"),
      description = "Shapiro-Wilk nos resíduos do modelo: os erros são normais?",
      inputs = list(modelo = Fm), outputs = list(out = TE),
      help = .tr_models_ajuda(r"---[
Testa se os RESÍDUOS do modelo têm distribuição normal (Shapiro-Wilk).

O pressuposto da ANOVA e da regressão é sobre o erro, e não sobre a resposta: a
produção de um DBC com blocos muito diferentes pode ser bimodal sem nada de
errado. Por isso o teste é nos resíduos — para a coluna crua, `models/shapiro`.

- Parcela subdividida: resíduos do erro (b).
- Misto: resíduos condicionais.
- GLM: o bloco recusa. Normalidade não é pressuposto de um GLM.

Com muitas observações o teste rejeita desvios pequenos que não afetam a ANOVA;
com poucas, não rejeita nada. Leia junto do Q-Q em `models/plot_diagnostics`.
]---", r"---[
Nenhum.
]---", r"---[
Um teste (`models/test`).
]---", exemplo_dbc("models/shapiro_residuals"), r"---[
`models/plot_diagnostics`; `models/levene`; `models/shapiro` para uma coluna.
]---", teste = TRUE)),

    trama::tr_node("models/levene", 
      pressupostos = .tr_models_doc("models/levene")$pressupostos,
      referencias = .tr_models_doc("models/levene")$referencias,
      fn = tr_models_levene, label = "Levene",
      category = "modelo_pressupostos", icon = trama::tr_icon("scale"),
      description = "Levene (Brown-Forsythe): as variâncias dos resíduos são iguais entre os tratamentos?",
      inputs = list(modelo = Fm), outputs = list(out = TE),
      params = list(centro = E("mediana", c("mediana", "média"), label = "Centro")),
      help = .tr_models_ajuda(r"---[
Testa a homogeneidade das variâncias entre os grupos: faz uma ANOVA dos desvios
absolutos dos resíduos em relação ao centro de cada grupo.

Os grupos são as combinações dos TRATAMENTOS (sem o bloco) nos delineamentos, e
as colunas-fator dos efeitos fixos nos modelos de fórmula. Sem fator, use o
`models/breusch_pagan`.

- **mediana** (padrão) — a versão de Brown-Forsythe, robusta à falta de
  normalidade. É a recomendada.
- **média** — o Levene original.

Não se aplica a GLM (a variância acompanha a média por construção) nem a misto.
]---", r"---[
- **Centro** — `mediana` ou `média`.
]---", r"---[
Um teste (`models/test`).
]---", exemplo_dbc("models/levene"), r"---[
`models/bartlett`; `models/breusch_pagan`; `models/plot_diagnostics`.
]---", teste = TRUE)),

    trama::tr_node("models/bartlett", 
      pressupostos = .tr_models_doc("models/bartlett")$pressupostos,
      referencias = .tr_models_doc("models/bartlett")$referencias,
      fn = tr_models_bartlett, label = "Bartlett",
      category = "modelo_pressupostos", icon = trama::tr_icon("scale"),
      description = "Bartlett: as variâncias dos resíduos são iguais entre os tratamentos?",
      inputs = list(modelo = Fm), outputs = list(out = TE),
      help = .tr_models_ajuda(r"---[
Testa a homogeneidade das variâncias entre os grupos (os mesmos do
`models/levene`), pelo teste de Bartlett.

É mais poderoso que o Levene quando os resíduos são normais.
]---", r"---[
Nenhum.
]---", r"---[
Um teste (`models/test`).
]---", exemplo_dbc("models/bartlett"), r"---[
`models/levene`; `models/shapiro_residuals`.
]---", teste = TRUE)),

    trama::tr_node("models/breusch_pagan", 
      pressupostos = .tr_models_doc("models/breusch_pagan")$pressupostos,
      referencias = .tr_models_doc("models/breusch_pagan")$referencias,
      fn = tr_models_breusch_pagan, label = "Breusch-Pagan",
      category = "modelo_pressupostos", icon = trama::tr_icon("chart-spline"),
      description = "Breusch-Pagan (Koenker): a variância dos resíduos depende dos preditores?",
      inputs = list(modelo = Fm), outputs = list(out = TE),
      help = .tr_models_ajuda(r"---[
Testa se a variância dos resíduos é constante, regredindo o quadrado dos
resíduos nos preditores do modelo. Se os preditores explicam o tamanho do
resíduo, há heterocedasticidade.

É o teste de variância para preditor CONTÍNUO, onde não há grupos para o
Levene. A versão é a studentizada de Koenker (n · R²), que não pressupõe
normalidade — a mesma do `lmtest::bptest`.

Não se aplica a GLM nem a misto.
]---", r"---[
Nenhum.
]---", r"---[
Um teste (`models/test`).
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "cars") |>
  tr_add("reg", "models/lm", resposta = "dist", preditores = "speed", from = "carros") |>
  tr_add("bp", "models/breusch_pagan", from = "reg")
]---", r"---[
`models/levene` para grupos; `models/plot_diagnostics` (painel escala-locação);
`models/glm` com família gama quando a variância cresce com a média.
]---", teste = TRUE)),

    trama::tr_node("models/tukey_additivity", 
      pressupostos = .tr_models_doc("models/tukey_additivity")$pressupostos,
      referencias = .tr_models_doc("models/tukey_additivity")$referencias,
      fn = tr_models_tukey_additivity, label = "Aditividade de Tukey",
      category = "modelo_pressupostos", icon = trama::tr_icon("equal"),
      description = "Teste de não aditividade de Tukey: bloco e tratamento interagem?",
      inputs = list(modelo = Fm), outputs = list(out = TE),
      help = .tr_models_ajuda(r"---[
O DBC supõe que o efeito do tratamento é o mesmo em todo bloco — que os efeitos
SOMAM. Com uma repetição por bloco não dá para estimar a interação
bloco × tratamento, mas dá para testar uma forma dela: o teste de um grau de
liberdade de Tukey acrescenta o quadrado do valor ajustado ao modelo e vê se ele
é significativo.

Rejeitar quer dizer que os efeitos se MULTIPLICAM (o tratamento bom rende
proporcionalmente mais no bloco bom). Uma transformação — o log, quase sempre —
costuma devolver a aditividade.

Aplica-se a `models/anova_dbc`, a `models/anova_factorial` com bloco (as
combinações de fatores como tratamento) e a `models/anova_dql`.
]---", r"---[
Nenhum.
]---", r"---[
Um teste (`models/test`).
]---", exemplo_dbc("models/tukey_additivity"), r"---[
`models/anova_dbc`; `models/plot_diagnostics`.
]---", teste = TRUE))
  )
}

.tr_models_nos_testes <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; B <- trama::tr_param_bool
  N <- trama::tr_param_num
  T <- "data/table"; TE <- "models/test"
  ALT <- function() E("bilateral", .TR_MODELS_ALTERNATIVAS, label = "Alternativa")
  ajuda_alt <- "- **Alternativa** — `bilateral` (diferentes), `menor` ou `maior` (o primeiro em relação ao segundo)."
  list(
    trama::tr_node("models/t_test", 
      pressupostos = .tr_models_doc("models/t_test")$pressupostos,
      referencias = .tr_models_doc("models/t_test")$referencias,
      fn = tr_models_t_test, label = "t para duas amostras",
      category = "modelo_testes", icon = trama::tr_icon("equal-not"),
      description = "t de Welch (ou de Student): as médias de dois grupos independentes são iguais?",
      inputs = list(dados = T), outputs = list(out = TE),
      params = list(
        resposta = P("cols", "", label = "Resposta", example = "len"),
        grupo = P("cols", "", label = "Grupo (2 níveis)", example = "supp"),
        variancias_iguais = B(FALSE, label = "Variâncias iguais (Student)"),
        alternativa = ALT()),
      help = .tr_models_ajuda(r"---[
Compara as médias de DOIS grupos independentes. A coluna do grupo tem de ter
exatamente dois níveis; para mais, `models/anova_dic`.

Por padrão é o t de **Welch**, que não pressupõe variâncias iguais e perde quase
nada quando elas são. O t de **Student** clássico só vale a pena quando se sabe
que as variâncias são iguais.

O detalhe mostra a DIFERENÇA de médias (primeiro nível − segundo, na ordem
alfabética ou dos níveis do fator) com o intervalo de 95% — o tamanho do efeito,
que o p-valor não diz.
]---", paste(r"---[
- **Resposta** — coluna numérica.
- **Grupo** — coluna com dois níveis.
- **Variâncias iguais** — liga o t de Student.
]---", ajuda_alt), r"---[
Um teste (`models/test`).
]---", r"---[
tr_flow(reg) |>
  tr_add("dentes", "models/example", dataset = "ToothGrowth") |>
  tr_add("t", "models/t_test", resposta = "len", grupo = "supp", from = "dentes")
]---", r"---[
`models/wilcoxon` (não paramétrico); `models/paired_t` para medidas pareadas;
`models/anova_dic` para mais de dois grupos.
]---", teste = TRUE)),

    trama::tr_node("models/paired_t", 
      pressupostos = .tr_models_doc("models/paired_t")$pressupostos,
      referencias = .tr_models_doc("models/paired_t")$referencias,
      fn = tr_models_paired_t, label = "t pareado",
      category = "modelo_testes", icon = trama::tr_icon("link"),
      description = "t pareado: a média das diferenças entre duas medidas na mesma unidade é zero?",
      inputs = list(dados = T), outputs = list(out = TE),
      params = list(
        antes = P("cols", "", label = "Primeira medida", example = "antes"),
        depois = P("cols", "", label = "Segunda medida", example = "depois"),
        alternativa = ALT()),
      help = .tr_models_ajuda(r"---[
Compara duas medidas feitas na MESMA unidade — antes e depois, lado esquerdo e
direito, dois métodos na mesma amostra. Testa se a média das diferenças
(primeira − segunda) é zero.

As duas medidas ficam em duas colunas da mesma linha. Se estão empilhadas numa
coluna só, com outra dizendo o momento, passe por um `data/pivot_wider` antes.

Parear tira a variação entre unidades: é por isso que detecta diferenças que o
`models/t_test` com as mesmas colunas não detectaria.
]---", paste(r"---[
- **Primeira medida**, **Segunda medida** — colunas numéricas.
]---", ajuda_alt), r"---[
Um teste (`models/test`), com a média das diferenças e o intervalo de 95%.
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("pareado", "models/paired_t", antes = "drat", depois = "wt", from = "carros")
]---", r"---[
`models/t_test` para grupos independentes; `models/lmer` para mais de duas
medidas na mesma unidade.
]---", teste = TRUE)),

    trama::tr_node("models/one_sample_t", 
      pressupostos = .tr_models_doc("models/one_sample_t")$pressupostos,
      referencias = .tr_models_doc("models/one_sample_t")$referencias,
      fn = tr_models_one_sample_t, label = "t para uma amostra",
      category = "modelo_testes", icon = trama::tr_icon("target"),
      description = "t para uma amostra: a média da coluna é igual a um valor de referência?",
      inputs = list(dados = T), outputs = list(out = TE),
      params = list(
        coluna = P("cols", "", label = "Coluna", example = "weight"),
        mu = N(0, label = "Valor de referência"),
        alternativa = ALT()),
      help = .tr_models_ajuda(r"---[
Testa se a média de uma coluna é igual a um valor de referência — a meta de
produção, o valor do rótulo, o padrão da norma.
]---", paste(r"---[
- **Coluna** — coluna numérica.
- **Valor de referência** — o valor da hipótese nula.
]---", ajuda_alt), r"---[
Um teste (`models/test`), com a média e o intervalo de 95%.
]---", r"---[
tr_flow(reg) |>
  tr_add("plantas", "models/example", dataset = "PlantGrowth") |>
  tr_add("t1", "models/one_sample_t", coluna = "weight", mu = 5, from = "plantas")
]---", r"---[
`models/t_test`; `models/shapiro` para conferir a normalidade da coluna.
]---", teste = TRUE)),

    trama::tr_node("models/wilcoxon", 
      pressupostos = .tr_models_doc("models/wilcoxon")$pressupostos,
      referencias = .tr_models_doc("models/wilcoxon")$referencias,
      fn = tr_models_wilcoxon, label = "Wilcoxon-Mann-Whitney",
      category = "modelo_testes", icon = trama::tr_icon("list-ordered"),
      description = "Wilcoxon-Mann-Whitney: dois grupos independentes têm a mesma locação? (não paramétrico)",
      inputs = list(dados = T), outputs = list(out = TE),
      params = list(
        resposta = P("cols", "", label = "Resposta", example = "len"),
        grupo = P("cols", "", label = "Grupo (2 níveis)", example = "supp"),
        alternativa = ALT()),
      help = .tr_models_ajuda(r"---[
A alternativa não paramétrica ao `models/t_test`: compara os POSTOS dos dois
grupos, e não as médias. Não pressupõe normalidade e resiste a discrepantes.

O efeito no detalhe é a diferença de locação de Hodges-Lehmann — a mediana das
diferenças entre um valor de cada grupo —, com o intervalo de 95%. Com empates
o p-valor é o da aproximação normal, e a nota avisa.
]---", paste(r"---[
- **Resposta** — coluna numérica.
- **Grupo** — coluna com dois níveis.
]---", ajuda_alt), r"---[
Um teste (`models/test`).
]---", r"---[
tr_flow(reg) |>
  tr_add("dentes", "models/example", dataset = "ToothGrowth") |>
  tr_add("w", "models/wilcoxon", resposta = "len", grupo = "supp", from = "dentes")
]---", r"---[
`models/t_test`; `models/kruskal` para mais de dois grupos.
]---", teste = TRUE)),

    trama::tr_node("models/kruskal", 
      pressupostos = .tr_models_doc("models/kruskal")$pressupostos,
      referencias = .tr_models_doc("models/kruskal")$referencias,
      fn = tr_models_kruskal, label = "Kruskal-Wallis",
      category = "modelo_testes", icon = trama::tr_icon("list-ordered"),
      description = "Kruskal-Wallis: dois ou mais grupos vêm da mesma distribuição? (não paramétrico)",
      inputs = list(dados = T), outputs = list(out = TE),
      params = list(
        resposta = P("cols", "", label = "Resposta", example = "count"),
        grupo = P("cols", "", label = "Grupo", example = "spray")),
      help = .tr_models_ajuda(r"---[
A alternativa não paramétrica ao `models/anova_dic`: compara os postos de dois
ou mais grupos. Útil quando os resíduos da ANOVA não são normais e nenhuma
transformação resolve.

Rejeitar diz que ALGUM grupo difere, não qual. Não há bloco: para um DBC com
dados não normais, considere transformar a resposta ou um `models/glm`.
]---", r"---[
- **Resposta** — coluna numérica.
- **Grupo** — coluna dos grupos.
]---", r"---[
Um teste (`models/test`).
]---", r"---[
tr_flow(reg) |>
  tr_add("insetos", "models/example", dataset = "InsectSprays") |>
  tr_add("kw", "models/kruskal", resposta = "count", grupo = "spray", from = "insetos")
]---", r"---[
`models/anova_dic`; `models/wilcoxon` para dois grupos.
]---", teste = TRUE)),

    trama::tr_node("models/chisq", version = 2L,
      pressupostos = .tr_models_doc("models/chisq")$pressupostos,
      referencias = .tr_models_doc("models/chisq")$referencias,
      fn = tr_models_chisq, label = "Qui-quadrado",
      category = "modelo_testes", icon = trama::tr_icon("table"),
      description = "Qui-quadrado de independência entre duas colunas categóricas.",
      inputs = list(dados = T), outputs = list(out = TE),
      params = list(
        linha = P("cols", "", label = "Linha", example = "wool"),
        coluna = P("cols", "", label = "Coluna", example = "tension"),
        correcao = B(FALSE, label = "Correção de Yates (2 × 2)")),
      help = .tr_models_ajuda(r"---[
Testa se duas variáveis categóricas são independentes, a partir da tabela de
contingência que o bloco monta contando as linhas da tabela.

Quando mais de 20% das caselas têm esperado abaixo de 5, a nota avisa e aponta o
`models/fisher_exact`. O detalhe mostra o menor esperado.

A **Correção de Yates** só age em tabelas 2 × 2 e vem desligada: ela deixa o
teste conservador (p-valor maior que o nominal). Com esperados pequenos, o
caminho é o `models/fisher_exact`, não a correção.
]---", r"---[
- **Linha**, **Coluna** — as duas colunas categóricas.
- **Correção de Yates** — correção de continuidade no 2 × 2 (desligada por
  padrão).
]---", r"---[
Um teste (`models/test`).
]---", r"---[
tr_flow(reg) |>
  tr_add("fios", "models/example", dataset = "warpbreaks") |>
  tr_add("qui", "models/chisq", linha = "wool", coluna = "tension", from = "fios")
]---", r"---[
`models/fisher_exact` para contagens pequenas; `data/group_summarise` para ver as contagens.
]---", teste = TRUE)),

    trama::tr_node("models/fisher_exact", 
      pressupostos = .tr_models_doc("models/fisher_exact")$pressupostos,
      referencias = .tr_models_doc("models/fisher_exact")$referencias,
      fn = tr_models_fisher_exact, label = "Exato de Fisher",
      category = "modelo_testes", icon = trama::tr_icon("table"),
      description = "Teste exato de Fisher: duas colunas categóricas são independentes?",
      inputs = list(dados = T), outputs = list(out = TE),
      params = list(
        linha = P("cols", "", label = "Linha", example = "am"),
        coluna = P("cols", "", label = "Coluna", example = "vs")),
      help = .tr_models_ajuda(r"---[
O teste de independência EXATO, sem a aproximação do qui-quadrado: vale com
qualquer contagem. É o que se usa quando o `models/chisq` avisa que há esperados
pequenos.

Numa tabela 2 × 2 o detalhe mostra a razão de chances com o intervalo de 95%.
Tabelas grandes com muitas linhas podem estourar a memória do cálculo exato; aí o
card fica vermelho e o qui-quadrado é a saída.
]---", r"---[
- **Linha**, **Coluna** — as duas colunas categóricas.
]---", r"---[
Um teste (`models/test`).
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("fisher", "models/fisher_exact", linha = "am", coluna = "vs", from = "carros")
]---", r"---[
`models/chisq`; `models/glm` binomial para modelar a associação com mais
variáveis.
]---", teste = TRUE)),

    trama::tr_node("models/cor_test", 
      pressupostos = .tr_models_doc("models/cor_test")$pressupostos,
      referencias = .tr_models_doc("models/cor_test")$referencias,
      fn = tr_models_cor_test, label = "Teste de correlação",
      category = "modelo_testes", icon = trama::tr_icon("trending-up"),
      description = "Correlação de Pearson, Spearman ou Kendall entre duas colunas, com o teste de que ela é zero.",
      inputs = list(dados = T), outputs = list(out = TE),
      params = list(
        x = P("cols", "", label = "X", example = "wt"),
        y = P("cols", "", label = "Y", example = "mpg"),
        metodo = E("pearson", c("pearson", "spearman", "kendall"), label = "Método")),
      help = .tr_models_ajuda(r"---[
Estima a correlação entre duas colunas numéricas e testa se ela é zero.

- **pearson** — associação LINEAR. Com intervalo de confiança.
- **spearman** — associação monotônica, pelos postos; resiste a discrepantes.
- **kendall** — também por postos, mais estável com amostras pequenas e empates.

O p-valor diz se há correlação; o `r` no detalhe diz de quanto. Com 500
observações, r = 0,1 é significativo e explica 1% da variação.
]---", r"---[
- **X**, **Y** — colunas numéricas.
- **Método** — `pearson`, `spearman` ou `kendall`.
]---", r"---[
Um teste (`models/test`).
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("cor", "models/cor_test", x = "wt", y = "mpg", metodo = "spearman", from = "carros")
]---", r"---[
`models/lm` para modelar a relação; `view/points` para vê-la.
]---", teste = TRUE)),

    trama::tr_node("models/shapiro", 
      pressupostos = .tr_models_doc("models/shapiro")$pressupostos,
      referencias = .tr_models_doc("models/shapiro")$referencias,
      fn = tr_models_shapiro, label = "Shapiro-Wilk",
      category = "modelo_testes", icon = trama::tr_icon("chart-area"),
      description = "Shapiro-Wilk: uma coluna tem distribuição normal?",
      inputs = list(dados = T), outputs = list(out = TE),
      params = list(coluna = P("cols", "", label = "Coluna", example = "weight")),
      help = .tr_models_ajuda(r"---[
Testa se os valores de UMA coluna vêm de uma distribuição normal.

Para o pressuposto de uma ANOVA ou regressão, não é este: o que precisa ser
normal são os resíduos, e a coluna crua mistura os efeitos dos tratamentos. Use
o `models/shapiro_residuals`.

Aceita de 3 a 5000 valores.
]---", r"---[
- **Coluna** — coluna numérica.
]---", r"---[
Um teste (`models/test`).
]---", r"---[
tr_flow(reg) |>
  tr_add("plantas", "models/example", dataset = "PlantGrowth") |>
  tr_add("sw", "models/shapiro", coluna = "weight", from = "plantas")
]---", r"---[
`models/shapiro_residuals`; `view/histogram`.
]---", teste = TRUE))
  )
}
