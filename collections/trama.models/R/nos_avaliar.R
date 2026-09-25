# As declarações dos nós que medem a previsão: confusão, ROC, métricas e a
# importância das preditoras.
#
# Confusão, ROC e métricas têm as DUAS entradas opcionais (ver `R/avaliar.R`):
# a ajuda de cada uma repete o parágrafo dos três modos, porque é a primeira
# coisa que quem abre o card precisa saber — o que ligar.

.tr_models_ajuda_modos <- function() {
  "
### O que ligar

As duas entradas são opcionais, mas uma tem de estar ligada:

- **só `modelo`** — avalia no TREINO, pela **validação**: `cruzada` (padrão;
  cada linha prevista por um modelo ajustado sem ela) ou `resubstituição` (o
  modelo completo prevê as linhas que o ajustaram — otimista).
- **`modelo` e `dados`** — prevê `dados` aqui e compara com a resposta do
  modelo, que a tabela precisa ter (o teste de uma divisão treino/teste, por exemplo).
  A validação é ignorada.
- **só `dados`** — modo tabela: a tabela já traz a resposta real e a prevista
  (de um `models/predict`, ou de fora), e os params `resposta`/`predito`
  dizem quais colunas são.
"
}

.tr_models_nos_avaliar <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum
  Fm <- "models/fit"; T <- "data/table"
  opc <- function(tipo) trama::tr_port(tipo, required = FALSE)
  validacao <- E(.TR_MODELS_VALIDACAO_PADRAO, .TR_MODELS_VALIDACOES, label = "Validação (só modelo)")
  resposta <- P("cols", "", label = "Resposta (modo tabela)", example = "am")
  predito <- P("cols", "previsto", label = "Previsto (modo tabela)", example = "previsto")
  positiva <- P("text", "", label = "Classe positiva", example = "1")
  list(
    trama::tr_node("models/confusion", fn = tr_models_confusion, label = "Matriz de confusão",
      category = "modelo_avaliar", icon = trama::tr_icon("table"),
      description = "Classe real × classe prevista, com o acerto de cada classe e o geral.",
      inputs = list(modelo = opc(Fm), dados = opc(T)), outputs = list(out = T),
      params = list(validacao = validacao, resposta = resposta, predito = predito),
      help = .tr_models_ajuda(paste0(r"---[
Conta, para cada classe REAL, em qual classe o modelo pôs cada caso. A
diagonal são os acertos; fora dela, as confusões. Ao lado, o total da classe,
os acertos e a taxa de acerto; a última linha (`real = total`) é o geral.
]---", .tr_models_ajuda_modos(), r"---[
### Por que cruzada

A taxa de acerto por resubstituição é otimista: cada caso ajudou a desenhar a
regra que o classifica. A cruzada é a estimativa honesta de quanto o modelo
acerta num caso novo, sem precisar separar teste. Leia a taxa de cada classe,
e não só a geral: com classes desbalanceadas, prever sempre a maior já acerta
muito.

Serve a todo classificador que viaja em `models/fit`: o GLM binomial daqui, a
discriminante e a logística da `multi`, as árvores da `ml`. Modelo de
regressão recusa — use `models/evaluate`.
]---"), r"---[
- **Validação (só modelo)** — `cruzada` (padrão) ou `resubstituição`.
- **Resposta**, **Previsto** — só no modo tabela: as colunas da classe real e
  da prevista (padrão `previsto`, a do `models/predict`).
]---", r"---[
Uma tabela (`data/table`) no formato largo: `real`, uma coluna por classe
prevista, `total`, `acertos`, `taxa_acerto`, e a linha `total`. Para o formato
longo (real, previsto, n), um `data/pivot_longer` nas colunas das classes.
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("logit", "models/glm", formula = "am ~ wt", familia = "binomial", from = "carros") |>
  tr_add("cv", "models/confusion", validacao = "cruzada", from = "logit")
]---", r"---[
`models/roc` para todos os cortes; `models/evaluate` para acurácia, kappa e
F1; `models/predict` para ver os casos.
]---")),

    trama::tr_node("models/roc", fn = tr_models_roc, label = "Curva ROC",
      category = "modelo_avaliar", icon = trama::tr_icon("chart-line"),
      description = "Sensibilidade × especificidade em todos os cortes, com a AUC.",
      inputs = list(modelo = opc(Fm), dados = opc(T)), outputs = list(out = "view/plot"),
      params = .tr_models_props(
        validacao = validacao, positiva = positiva, resposta = resposta,
        probabilidade = P("cols", "", label = "Probabilidade (modo tabela)", example = "prob_1"),
        .aspecto = "1:1"),
      help = .tr_models_ajuda(paste0(r"---[
Para cada corte de probabilidade possível, a fração dos positivos que a regra
pega (**sensibilidade**) contra a fração dos negativos que ela alarma à toa
(**1 − especificidade**). Um classificador inútil anda na diagonal tracejada;
um perfeito sobe reto até o canto superior esquerdo.

A **AUC** (área sob a curva) é a probabilidade de um positivo sorteado ter
probabilidade prevista maior que a de um negativo sorteado: 0,5 é moeda, 1 é
perfeito. Não depende do corte, e compara modelos melhor que a taxa de acerto
quando as classes são desbalanceadas.

- **Duas classes** — uma curva; a positiva é a **Classe positiva**, ou o
  SEGUNDO nível se vazia. O ponto rosa é o corte do modelo (o `corte` da
  logística da `multi`; 0,5 nos demais).
- **Três ou mais** — uma curva por classe, ela contra todas as outras, com a
  AUC de cada na legenda. Com **Classe positiva** preenchida, só a curva dela.
]---", .tr_models_ajuda_modos(), r"---[
No modo tabela a curva lê a coluna **Probabilidade**; vazia, a
`prob_<positiva>` que o `models/predict` escreve (e, com três ou mais classes
sem positiva, todas as `prob_<nível>`).
]---"), r"---[
- **Validação (só modelo)** — `cruzada` (padrão) ou `resubstituição`.
- **Classe positiva** — vazia = o segundo nível.
- **Resposta**, **Probabilidade** — só no modo tabela.
]---", r"---[
Um gráfico (`view/plot`), com a AUC no subtítulo (ou na legenda, uma por
classe).
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("logit", "models/glm", formula = "am ~ wt", familia = "binomial", from = "carros") |>
  tr_add("roc", "models/roc", validacao = "cruzada", from = "logit")
]---", r"---[
`models/confusion` para o acerto num corte; `models/evaluate` para as métricas.
]---", grafico = TRUE)),

    trama::tr_node("models/evaluate", fn = tr_models_evaluate, label = "Avaliar previsões",
      category = "modelo_avaliar", icon = trama::tr_icon("gauge"),
      description = "Erro de previsão (regressão) ou acerto, kappa e F1 (classificação), numa tabela de métricas.",
      inputs = list(modelo = opc(Fm), dados = opc(T)), outputs = list(out = T),
      params = list(validacao = validacao, positiva = positiva, resposta = resposta, predito = predito),
      help = .tr_models_ajuda(paste0(r"---[
Mede quanto a previsão erra, numa linha por métrica:

- **Regressão** — `mae` (erro absoluto médio), `rmse` (raiz do erro
  quadrático médio, na unidade da resposta) e `r2` de PREVISÃO (1 − SQ do erro
  / SQ total nas linhas avaliadas). Na cruzada ou em dados novos o `r2` pode
  ser negativo: o modelo prevê pior que a média.
- **Classificação** — `accuracy` (acerto), `balanced_accuracy` (média do
  acerto por classe observada), `macro_f1`, `kappa` de Cohen (o acerto além do
  que o acaso daria) e, com duas classes, `sensitivity` e `specificity` da
  **Classe positiva** (vazia = o segundo nível).

Os nomes e as contas são os do bloco de avaliação da coleção `ml`, para que
os números batam.
Diferente de `models/fit_stats`, que descreve o AJUSTE no treino, este bloco
mede a PREVISÃO.
]---", .tr_models_ajuda_modos(), r"---[
No modo tabela, resposta ou previsto categóricos (texto, fator, lógico) fazem
classificação; dois números, regressão. Linhas sem real ou sem previsto ficam
fora, e `n` diz quantas contaram.
]---"), r"---[
- **Validação (só modelo)** — `cruzada` (padrão) ou `resubstituição`.
- **Classe positiva** — a das `sensitivity`/`specificity`.
- **Resposta**, **Previsto** — só no modo tabela.
]---", r"---[
Uma tabela (`data/table`): `metrica`, `valor`, `n`.
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("reg", "models/lm", formula = "mpg ~ wt + hp", from = "carros") |>
  tr_add("cv", "models/evaluate", validacao = "cruzada", from = "reg")
]---", r"---[
`models/fit_stats` para o ajuste no treino; `models/confusion` para as
confusões entre classes.
]---")),

    trama::tr_node("models/importance", fn = tr_models_importance_table, label = "Importância",
      category = "modelo_resumir", icon = trama::tr_icon("chart-bar"),
      description = "Quanto cada preditora pesa no modelo, da maior para a menor.",
      inputs = list(modelo = Fm), outputs = list(out = T),
      help = .tr_models_ajuda(r"---[
Uma linha por preditora (ou coeficiente), da mais para a menos importante. A
coluna `medida` diz qual régua foi usada, e ela muda com o modelo:

- `lm` e GLM — **|t|** (ou |z|) de cada coeficiente: não depende da escala da
  preditora, e o intercepto fica de fora. Um fator vira uma linha por
  contraste.
- Árvores da `ml` — redução de impureza (ganho no XGBoost).

Não compare importâncias de medidas diferentes, e não leia sinal nem causa:
preditoras correlacionadas repartem a importância entre si. O misto e a
parcela subdividida recusam.
]---", r"---[
Sem params.
]---", r"---[
Uma tabela (`data/table`): `termo`, `importancia`, `medida`.
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("reg", "models/lm", formula = "mpg ~ wt + hp + qsec", from = "carros") |>
  tr_add("imp", "models/importance", from = "reg")
]---", r"---[
`models/coefficients` com escala `desvio padrão` para o efeito comparável de
cada preditora.
]---"))
  )
}
