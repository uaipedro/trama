# A declaração do nó da predição.

.tr_models_nos_prever <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; N <- trama::tr_param_num
  Fm <- "models/fit"; T <- "data/table"
  list(
    trama::tr_node("models/predict", fn = tr_models_predict, label = "Prever",
      category = "modelo_resumir", icon = trama::tr_icon("target"),
      description = "Aplica um modelo já ajustado a uma tabela nova, e devolve a previsão de cada linha.",
      inputs = list(modelo = Fm, dados = T), outputs = list(out = T),
      params = list(intervalo = E("nenhum", .TR_MODELS_PREVER_INTERVALOS, label = "Intervalo"),
                    confianca = N(0.95, min = 0.5, max = 0.999, step = 0.01, label = "Confiança")),
      help = .tr_models_ajuda(r"---[
Aplica um modelo já ajustado (`models/fit`) a uma tabela NOVA, e devolve as
mesmas colunas de `dados` com a previsão anexada.

### O modelo tem que já ter visto essas colunas

Uma coluna preditora ausente em `dados`, ou um fator com um nível que o ajuste
nunca viu, param aqui com o NOME da coluna — não com o erro cru do R, que fala
de linhas da matriz de design ou de contrastes e não diz qual coluna foi.

### Intervalo

Só em `models/lm`: **confiança**, em torno da média prevista; **predição**, em
torno de uma observação nova (mais largo, porque soma a variância do erro). O
GLM não tem intervalo fechado em `predict.glm()`, e o misto não tem erro
padrão de predição fechado — os dois recusam com **confiança**/**predição**.

Em `models/glm`, `previsto` sai na escala da RESPOSTA (probabilidade,
contagem) — não na escala da ligação (log-odds, log), que é o default de
`predict.glm()`. É uma escolha deste card, para não exigir que quem lê a
tabela desfaça o `logit`/`log` de cabeça. Não há um param para pedir a escala
da ligação: se você precisa dela, chame `predict.glm()` diretamente sobre
`modelo$ajuste`.

### Dentro de uma região de fluxo

Este nó não sabe nada sobre fluxo — é um nó comum, de dois inputs, puro. Ligado
depois de um `data/to_stream`, a região o eleva ponto a ponto sozinha: ele
prevê UM ponto por passo, sem mudar uma linha de código. É o achado que
motivou a Fase 7 — a peça que faltava para o caso de uso do desenho era esta,
e não uma peça com memória.
]---", r"---[
- **Intervalo** — `nenhum`, `confiança` ou `predição` (só em `models/lm`).
- **Confiança** — o nível do intervalo (padrão 0,95); ignorada com `nenhum`.
]---", r"---[
Uma tabela (`data/table`): as colunas de `dados`, mais `previsto` (e `li`,
`ls` quando há intervalo).
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("reg", "models/lm", formula = "mpg ~ wt", from = "carros") |>
  tr_add("prever", "models/predict", from = c("reg", "carros"))
]---", r"---[
`models/lm`, `models/glm`, `models/lmer` para ajustar; `data/to_stream` e
`data/from_stream` para prever ponto a ponto dentro de uma região de fluxo.
]---")
    )
  )
}
