# A declaração do nó da predição.

.tr_models_nos_prever <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; N <- trama::tr_param_num
  Fm <- "models/fit"; T <- "data/table"
  list(
    trama::tr_node("models/predict", fn = tr_models_predict, label = "Prever",
      category = "modelo_avaliar", icon = trama::tr_icon("target"),
      description = "Aplica um modelo já ajustado a uma tabela nova (ou ao próprio treino), e devolve a previsão de cada linha.",
      inputs = list(modelo = Fm, dados = trama::tr_port(T, required = FALSE)), outputs = list(out = T),
      params = list(validacao = E("resubstituição", .TR_MODELS_VALIDACOES, label = "Validação (sem dados)"),
                    intervalo = E("nenhum", .TR_MODELS_PREVER_INTERVALOS, label = "Intervalo"),
                    confianca = N(0.95, min = 0.5, max = 0.999, step = 0.01, label = "Confiança")),
      help = .tr_models_ajuda(r"---[
Aplica um modelo já ajustado (`models/fit`) a uma tabela NOVA, e devolve as
mesmas colunas de `dados` com a previsão anexada. Sem `dados` ligada, prevê as
linhas do próprio ajuste.

### Sem `dados`: o treino, por resubstituição ou cruzada

- **resubstituição** (padrão) — o modelo completo prevê as linhas que o
  ajustaram. É o `fitted()`: otimista, porque cada linha ajudou a se prever.
- **cruzada** — cada linha é prevista por um modelo ajustado SEM ela
  (deixa-um-fora; no `lm` pela fórmula exata do PRESS, sem reajustar). É o
  que se espera numa linha nova. O misto recusa: tirar uma linha quebra a
  estrutura dos grupos.

Com `dados` ligada a validação é ignorada: uma tabela nova nunca esteve no
ajuste.

### O modelo tem que já ter visto essas colunas

Uma coluna preditora ausente em `dados`, ou um fator com um nível que o ajuste
nunca viu, param aqui com o NOME da coluna — não com o erro cru do R, que fala
de linhas da matriz de design ou de contrastes e não diz qual coluna foi.

### Classificação: classe e probabilidades

Num modelo que classifica — o GLM **binomial** de resposta 0/1, lógica ou
fator de dois níveis, e os classificadores da `multi` e da `ml` — `previsto` é
a CLASSE (corte 0,5 no GLM), e cada classe ganha uma coluna `prob_<nível>`
(`prob_0`, `prob_1`; espaço e hífen do nível viram `_`).

**Mudou**: até a Fase 4, o GLM binomial devolvia em `previsto` a
probabilidade numérica. Ela agora está em `prob_<segundo nível>` (`prob_1`
para uma resposta 0/1); `previsto` é a classe. Um fluxo que lia `previsto`
como número passa a ler `prob_1`. Binomial de proporção (com pesos) continua
prevendo a taxa em `previsto`, como antes.

Nas outras famílias `previsto` sai na escala da RESPOSTA (contagem, média) —
não na escala da ligação (log), que é o default de `predict.glm()`.

### Intervalo

Só em `models/lm`: **confiança**, em torno da média prevista; **predição**, em
torno de uma observação nova (mais largo, porque soma a variância do erro). O
GLM não tem intervalo fechado em `predict.glm()`, e o misto não tem erro
padrão de predição fechado — os dois recusam com **confiança**/**predição**.
Sem `dados`, o intervalo só existe por resubstituição.

### Dentro de uma região de fluxo

Este nó não sabe nada sobre fluxo — é um nó comum, de dois inputs, puro. Ligado
depois de um `data/to_stream`, a região o eleva ponto a ponto sozinha: ele
prevê UM ponto por passo, sem mudar uma linha de código. É o achado que
motivou a Fase 7 — a peça que faltava para o caso de uso do desenho era esta,
e não uma peça com memória.
]---", r"---[
- **Validação (sem dados)** — `resubstituição` (padrão) ou `cruzada`; só vale sem `dados`.
- **Intervalo** — `nenhum`, `confiança` ou `predição` (só em `models/lm`).
- **Confiança** — o nível do intervalo (padrão 0,95); ignorada com `nenhum`.
]---", r"---[
Uma tabela (`data/table`): as colunas de `dados` (ou do treino), mais
`previsto`, `prob_<nível>` na classificação, e `li`, `ls` quando há intervalo.
Colunas com esses nomes que já existiam são substituídas.
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("reg", "models/lm", formula = "mpg ~ wt", from = "carros") |>
  tr_add("prever", "models/predict", from = c("reg", "carros")) |>
  tr_add("logit", "models/glm", formula = "am ~ wt", familia = "binomial", from = "carros") |>
  tr_add("cv", "models/predict", validacao = "cruzada", from = "logit")
]---", r"---[
`models/lm`, `models/glm`, `models/lmer` para ajustar; `models/confusion`,
`models/roc` e `models/evaluate` para medir o acerto; `data/to_stream` e
`data/from_stream` para prever ponto a ponto dentro de uma região de fluxo.
]---")
    )
  )
}
