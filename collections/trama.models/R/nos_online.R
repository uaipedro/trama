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
#' `step_rls()` chegar PERTO dos coeficientes do `lm()` depois de `n` pontos
#' (ver lá — a exatidão é só no limite `lambda → ∞`) — um `lambda` pequeno é
#' um prior INFORMATIVO não declarado: os primeiros pontos puxam `theta` para
#' 0 mais do que os dados pedem, e o viés correspondente NÃO se dissolve
#' sozinho — fica.
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
#'
#' `min = 1e-6` no `lambda`, e não `1e-12`: o mesmo número do param do spec
#' (`:137`), de propósito — ter duas faixas para o mesmo bound (o card recusa
#' um valor que a função aceitaria) é o achado Minor 6 da revisão da Fase 7.
#' `1e-6` é o bound que importa aqui: um `lambda` menor que isso é uma prior
#' que já não é "difusa" — `P0` nasce perto de singular e devolve exatamente
#' o modo de falha que a prior existe para evitar.
#' @noRd
init_rls <- function(resposta = "y", preditores = "", lambda = 1e6) {
  list(theta = NULL, P = NULL, preditores = .tr_models_split(preditores),
       resposta = resposta, lambda = .tr_models_num(lambda, "lambda", min = 1e-6),
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
#' A simetrização de `P` (`(P + t(P)) / 2`, abaixo): NÃO é remédio para uma
#' assimetria observada — é seguro barato contra uma que esta forma
#' específica não produz. `Px <- P %*% x` e `outer(ganho, Px)` é
#' `outer(Px/denom, Px)`, ou seja a célula `(i, j)` é `Px_i * Px_j / denom` e
#' a `(j, i)` é `Px_j * Px_i / denom` — a mesma multiplicação de ponto
#' flutuante, comutativa, nos dois lados. Esta forma de posto 1 é
#' algebricamente auto-simétrica; não HÁ erro de arredondamento assimétrico
#' para acumular.
#'
#' Medido removendo a linha de propósito (1000 e 10000 passos, 1 e 3
#' preditores, e um caso de x na escala de 1e6): a assimetria não CRESCE de
#' 1000 para 10000 passos (mesma ordem de grandeza nos dois), fica em torno de
#' 1e-18 a 1e-13 mesmo sem a linha, e `P` nunca perde definição positiva —
#' com ou sem simetrização, os autovalores são os mesmos. Nenhuma das duas
#' metades da alegação antiga ("perde simetria com o tempo", "vira
#' covariância negativa") se confirmou.
#'
#' A linha fica mesmo assim: é seguro contra uma mudança FUTURA na forma da
#' atualização (uma reparametrização, um termo de esquecimento) que deixe de
#' ter essa simetria embutida — não contra o que este código faz hoje.
#' `test-rls.R` ("P não degenera em 1000 passos") mede exatamente isso: `P`
#' simétrica e positiva definida depois de mil pontos, o que já era verdade
#' antes da linha existir.
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
  P <- (P + t(P)) / 2  # ver o cabeçalho: proteção contra mudança futura na forma, não contra deriva observada

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
tr_models_rls_fn <- function(dados, resposta = "y", preditores = "", lambda = 1e6) dados

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
        lambda = N(1e6, min = 1e-6, label = "Prior difusa (lambda)")),
      init = init_rls, step = step_rls,
      help = .tr_models_ajuda(r"---[
Regressão linear que atualiza os coeficientes UM PONTO por vez, em vez de
reajustar a tabela inteira: é o nó com memória desta coleção, para dentro de
uma região de fluxo (`data/to_stream` → ... → `data/from_stream`).

### O mesmo resultado do `lm()`, no limite

Depois de ver `n` pontos, o `models/rls` chega PERTO dos coeficientes que
`models/lm` chegaria ajustando a tabela inteira de uma vez — a mesma conta de
mínimos quadrados, refeita de forma incremental, e não um filtro por
gradiente (SGD): nenhum SGD tem essa propriedade nem de longe. Mas a
igualdade é exata só quando `lambda → ∞` — com `lambda` finito sobra um viés
da ordem de `1/lambda`, e esse viés NÃO desaparece com mais pontos: é
permanente, não transitório. No default (`1e6`), medido com 1 preditor e
n=200 o maior coeficiente erra por volta de `2e-8`; com preditoras colineares
(correlação perto de 1) ou em escalas muito diferentes entre si, o mesmo
`lambda` erra ordens de grandeza mais — é a conta ficando mal-condicionada,
não o RLS relaxando a exatidão de propósito.

### A prior difusa

`lambda` é o quanto o modelo desconfia de si mesmo ANTES do primeiro ponto —
quanto maior, mais perto ele fica do `lm()` batch (ver acima). Pequeno demais,
os primeiros pontos pesam mais do que deviam e o viés PERMANECE, não só
demora a sumir. Mas maior não é sempre melhor: a partir de mais ou menos
`1e6` quem passa a dominar é o condicionamento numérico de `P0 = lambda * I`,
e subir mais além disso pode PIORAR o resultado (medido: com 3 preditoras, o
erro a `lambda = 1e12` saiu dezenas de vezes maior que a `lambda = 1e6`) — não
existe "quanto maior, melhor" sem teto.

### Resposta e preditoras

**Resposta** é o nome da coluna-alvo no ponto (`y`, por padrão). **Preditores**
em branco usa as demais colunas do ponto, na ordem em que chegam — é assim
porque o ponto já vem do `data/to_stream` com as colunas da tabela de origem,
e repetir a lista aqui só criaria uma segunda lista para desatualizar quando a
tabela ganhasse uma coluna.
]---", r"---[
- **Resposta** — a coluna-alvo, no ponto.
- **Preditores** — em branco, todas as outras colunas do ponto.
- **Prior difusa (lambda)** — quanto maior (até ~1e6), mais perto do `lm()`
  batch; menor deixa um viés permanente nos coeficientes. Acima de ~1e6 o
  condicionamento numérico piora, não melhora.
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
