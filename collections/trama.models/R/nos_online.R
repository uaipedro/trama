# O primeiro nó com memória de verdade da coleção: mínimos quadrados
# recursivos (RLS). `init`/`step` em base R, sem dependência nova.
#
# A resposta à Tarefa 7.3: um `models/lm` ajusta TUDO de uma vez, e não serve
# de propósito dentro de uma região — teria que reajustar a cada ponto, o que
# nem é o que "ponto a ponto" quer dizer. O RLS é o oposto: recebe UM ponto,
# atualiza os coeficientes com ele, e esquece o ponto — o estado inteiro é
# `theta` (os coeficientes) e `P` (a covariância deles), não a tabela.

#' O estado inicial do RLS: prior difusa, nenhum ponto visto ainda.
#'
#' `theta` começa em 0 — sempre, independente de `lambda`. É `P` quem carrega
#' a incerteza sobre esse chute, e `P0 = lambda * I`: quanto maior `lambda`,
#' mais incerto o estado antes do primeiro ponto. Essa incerteza é o que faz
#' `step_rls()` convergir para os MESMOS coeficientes do `lm()` depois de `n`
#' pontos (ver lá) — um `lambda` pequeno é um prior INFORMATIVO não declarado:
#' os primeiros pontos puxariam `theta` para 0 mais do que os dados pedem, e
#' só se dissolveria aos poucos.
#'
#' Com menos pontos que coeficientes, o sistema é subdeterminado — e é
#' exatamente aí que a prior sustenta a conta. Sem ela, `P` teria direções sem
#' NENHUMA informação, e o denominador da atualização (`1 + x'Px`) se
#' aproximaria de 1 numa direção que `x` não excita e a atualização faria
#' `theta` andar só no que os dados de fato pediram — o que é o comportamento
#' CERTO (ridge-like: parado nas direções ainda não visitadas), e é a prior
#' difusa que garante que a conta nunca divide por perto de zero.
#'
#' `preditores` em branco (`""`) é "as demais colunas do ponto, na ordem em
#' que aparecem nele": o param existiria só para repetir o que a tabela de
#' origem já diz, e repetir é o que deixa os dois divergirem quando a tabela
#' ganha uma coluna. Resolvido no PRIMEIRO passo (`step_rls()`), porque é lá
#' que o primeiro ponto chega — `init()` só recebe params (o contrato do
#' núcleo: ver `stream-driver.R`), nunca um ponto.
#' @noRd
init_rls <- function(preditores = "", resposta = "y", lambda = 1e4) {
  list(theta = NULL, P = NULL, preditores = .tr_models_split(preditores),
       resposta = resposta, lambda = .tr_models_num(lambda, "lambda", min = 1e-12),
       n = 0L)
}

#' Um passo do RLS: atualiza `theta`/`P` com UM ponto, na forma Sherman-Morrison.
#'
#' `out` é `data.frame(previsto, real, residuo)` — o previsto é o valor que o
#' modelo ANTES deste ponto já daria para ele (a predição a priori, o que se
#' pede de um filtro online); `residuo = real - previsto`.
#'
#' Sem `passo`: `data/from_stream` (Fase 6) ACRESCENTA uma coluna `passo` — o
#' índice do ponto, o eixo x de todo gráfico — e, se o ponto já trouxer uma
#' coluna de mesmo nome, ela é SUBSTITUÍDA em silêncio (a colisão de nomes
#' achada na Fase 6). Se `out` declarasse `passo`, a mesma informação moraria
#' em dois lugares e a cópia daqui seria descartada sem aviso — o índice é de
#' quem monta o fluxo, não do nó.
#'
#' A forma numericamente estável: a atualização ingênua de `P`
#' (`P - P x x' P / (1 + x'Px)`) perde simetria depois de milhares de passos
#' por erro de arredondamento acumulado, e `P` deixa de ser positiva definida
#' — a covariância "negativa" que trava filtros de Kalman em produção. A
#' simetrização (`(P + t(P)) / 2`) a cada passo é o remédio padrão e barato:
#' mede-se `test-rls.R` ("P não degenera em 1000 passos") com assimetria 0 e
#' autovalores positivos depois de mil pontos — ver o relatório da Tarefa 7.2.
#' @noRd
step_rls <- function(state, dados) {
  if (is.null(state$theta)) {
    nomes <- state$preditores
    if (!length(nomes)) nomes <- setdiff(names(dados), state$resposta)
    p <- length(nomes) + 1L
    state$preditores <- nomes
    state$theta <- rep(0, p)
    state$P <- diag(state$lambda, p)
  }

  faltam <- setdiff(c(state$preditores, state$resposta), names(dados))
  if (length(faltam)) {
    .tr_models_abort("tr_models_error_unknown_column",
                     "'models/rls': o ponto não tem a coluna '%s'. Colunas do ponto: %s.",
                     paste(faltam, collapse = "', '"), paste(names(dados), collapse = ", "))
  }

  x <- c(1, as.numeric(unlist(dados[1L, state$preditores, drop = TRUE])))
  y <- as.numeric(dados[[state$resposta]][[1L]])

  P <- state$P; theta <- state$theta
  previsto <- as.numeric(sum(x * theta))
  Px <- as.vector(P %*% x)
  denom <- 1 + as.numeric(sum(x * Px))
  ganho <- Px / denom
  residuo <- y - previsto

  theta <- theta + ganho * residuo
  P <- P - outer(ganho, Px)
  P <- (P + t(P)) / 2  # ver o cabeçalho: sem isto P degenera em poucos milhares de passos

  state$theta <- theta; state$P <- P; state$n <- state$n + 1L
  list(state = state, out = data.frame(previsto = previsto, real = y, residuo = residuo))
}

#' Os coeficientes do estado atual, nomeados como `coef(lm())`.
#' @noRd
coef_rls <- function(state) {
  theta <- state$theta
  names(theta) <- c("(Intercept)", state$preditores)
  theta
}

#' O `fn` de `models/rls` — nunca roda de verdade.
#'
#' Um nó `online` só existe DENTRO de uma região (`node.R` recusa `step` sem
#' entrada de fluxo), e é o `step` quem o driver chama a cada passo — `fn` fica
#' de fora do caminho de execução. Existe porque `tr_node()` o exige sempre
#' (é o nível 1 da API), e os formais espelham os PARAMS um a um, com os
#' MESMOS defaults do spec: é o que "todo param do spec existe no fn com o
#' mesmo default" cobra de cada nó da coleção (`test-catalogo.R`), a mesma
#' garantia que evita spec e fn discordando em silêncio em qualquer outro nó.
#' @export
tr_models_rls_fn <- function(dados, resposta = "y", preditores = "", lambda = 1e4) dados

.tr_models_nos_online <- function() {
  P <- trama::tr_param; N <- trama::tr_param_num
  T <- "data/table"
  list(
    trama::tr_node("models/rls", fn = tr_models_rls_fn, label = "RLS online",
      category = "modelo_ajustar", icon = trama::tr_icon("activity"),
      description = "Regressão linear que aprende ponto a ponto: mínimos quadrados recursivos, dentro de uma região de fluxo.",
      # As DUAS portas de fluxo, entrada e saída: sem a de SAÍDA, o núcleo
      # classifica este nó como COLAPSO (`.tr_stream_regions()`: "recebe fluxo
      # por declaração e devolve valor comum" é exatamente a forma de "entrada
      # de fluxo declarada, saída não" — a mesma forma de um nó com memória que
      # continua emitindo ponto a ponto). Sem `stream = TRUE` aqui, a região
      # parava em `models/rls` e `data/from_stream` nunca virava membro — a
      # propagação achada rodando o exemplo desta ajuda.
      inputs = list(dados = trama::tr_port(T, stream = TRUE)),
      outputs = list(out = trama::tr_port(T, stream = TRUE)),
      params = list(
        resposta = P("cols", "y", label = "Resposta", example = "y"),
        preditores = P("cols", "", label = "Preditores (em branco = as demais colunas do ponto)", example = "x"),
        lambda = N(1e4, min = 1e-6, label = "Prior difusa (lambda)")),
      init = init_rls, step = step_rls,
      help = .tr_models_ajuda(r"---[
Regressão linear que atualiza os coeficientes UM PONTO por vez, em vez de
reajustar a tabela inteira: é o nó com memória desta coleção, para dentro de
uma região de fluxo (`data/to_stream` → ... → `data/from_stream`).

### O mesmo resultado do `lm()`, algebricamente

Depois de ver `n` pontos, o `models/rls` chega EXATAMENTE aos coeficientes que
`models/lm` chegaria ajustando a tabela inteira de uma vez — não uma
aproximação: é a mesma conta de mínimos quadrados, refeita de forma
incremental. Nenhum filtro por gradiente (SGD) tem essa propriedade.

### A prior difusa

`lambda` é o quanto o modelo desconfia de si mesmo ANTES do primeiro ponto —
quanto maior, mais rápido ele se comporta como o `lm()` batch. Pequeno demais,
os primeiros pontos pesam mais do que deviam e o viés leva um tempo para
sumir.

### Resposta e preditoras

**Resposta** é o nome da coluna-alvo no ponto (`y`, por padrão). **Preditores**
em branco usa as demais colunas do ponto, na ordem em que chegam — é assim
porque o ponto já vem do `data/to_stream` com as colunas da tabela de origem,
e repetir a lista aqui só criaria uma segunda lista para desatualizar quando a
tabela ganhasse uma coluna.
]---", r"---[
- **Resposta** — a coluna-alvo, no ponto.
- **Preditores** — em branco, todas as outras colunas do ponto.
- **Prior difusa (lambda)** — quanto maior, mais rápida a convergência para o
  `lm()` batch; menor pesa mais nos primeiros pontos.
]---", r"---[
Uma tabela (`data/table`) de UMA linha por passo: `previsto` (antes de ver o
ponto), `real` e `residuo`. Ligado a `data/from_stream`, vira o histórico —
uma linha por ponto, com a coluna `passo`.
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("entra", "data/to_stream", lote = 1L, from = "carros") |>
  tr_add("rls", "models/rls", resposta = "mpg", preditores = "wt", from = "entra") |>
  tr_add("sai", "data/from_stream", from = "rls")
]---", r"---[
`models/lm` para o ajuste em lote; `data/to_stream` e `data/from_stream`, a
fronteira da região de fluxo.
]---")
    )
  )
}
