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
#' `min = 1e-6` no `lambda`, e não `1e-12`: é o mesmo número do param do spec,
#' e essa é a ÚNICA razão. Ter duas faixas para o mesmo bound — o card recusando
#' um valor que a função aceita — é a divergência que se está fechando aqui.
#'
#' O que este bound NÃO faz é separar o que funciona do que não funciona, e
#' vale dizer porque a versão anterior deste comentário afirmava que fazia:
#' medido, `lambda = 1e-6` (valor que o bound ACEITA) já devolve um modelo
#' inútil — coeficientes na casa de 1e-4 onde o `lm()` dá 2 e 3 —, e `P0 =
#' 1e-6 * I` não é "perto de singular": o número de condição dele é 1, ele é só
#' pequeno. Prior pequena demais não é instabilidade numérica, é viés enorme,.
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
#' A simetrização de `P` (`(P + t(P)) / 2`, abaixo). Este comentário já esteve
#' errado duas vezes em direções opostas, então aqui vai só o que foi medido,
#' removendo a linha de propósito.
#'
#' A assimetria NÃO cresce com os passos: mesma ordem de grandeza em 1000 e em
#' 10000, entre 6e-18 (um preditor) e 1e-10 (três) a `lambda = 1e6`; a `1e4` o piso é mais baixo ainda. Não é zero — `ganho` é
#' `Px/denom`, arredondado ELEMENTO A ELEMENTO antes de multiplicar, então a
#' célula `(i, j)` é `fl(Px_i/denom) * Px_j` e a `(j, i)` é
#' `fl(Px_j/denom) * Px_i`: dois produtos diferentes, e sem a linha
#' `identical(P, t(P))` é FALSE. O que a forma de posto 1 garante é assimetria
#' LIMITADA, não ausente — e é a não-acumulação que importa aqui.
#'
#' E a linha não é só seguro para o futuro — mas aqui o que se pode afirmar é
#' menos do que três rodadas de comentário tentaram afirmar, e vale registrar o
#' porquê, porque é o tipo de número que parece fácil de medir e não é.
#'
#' Três medições independentes do "menor autovalor de `P` sem a linha" deram
#' três respostas: −23.8 (configuração não registrada), negativo em 32 de 40
#' sementes com três preditoras na escala de 1e6, e ZERO negativos em 40
#' sementes na mesma escala, num desenho de dados um pouco diferente. Não é
#' desleixo de ninguém: a quantidade depende do desenho de um jeito que nenhuma
#' das três mediu o bastante para fixar.
#'
#' E há uma razão técnica para a divergência ser fácil: SEM a linha `P` não é
#' simétrica, e aí "o menor autovalor" nem é bem definido — `eigen(symmetric =
#' TRUE)` (o natural de se escrever para uma covariância) usa só um triângulo,
#' isto é, mede a matriz SIMETRIZADA, exatamente o que a linha produziria; com
#' assimetria grande, `eigen(symmetric = FALSE)` pode devolver valores
#' complexos. Dois medidores podem estar medindo objetos diferentes sem notar.
#'
#' O que as três medições CONCORDAM, e é o que fica aqui:
#'   - sem a linha, `identical(P, t(P))` é FALSE (assimetria real, medida por
#'     todos);
#'   - a assimetria não cresce com os passos;
#'   - com a linha, `P` é simétrica bit a bit por construção (a soma em IEEE é
#'     comutativa), e isso é garantia, não sorte.
#'
#' A linha fica por isso: custa uma operação, torna `P` simétrica por definição
#' em vez de por acidente, e remove de vez a pergunta ambígua "o menor autovalor
#' de uma matriz que não é simétrica". Se alguém quiser afirmar um número de
#' autovalor aqui, meça-o antes, e diga em que configuração.
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
  P <- (P + t(P)) / 2  # ver o cabeçalho: `P` simétrica por definição, não por acidente

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

`lambda` é o quanto o modelo desconfia de si mesmo ANTES do primeiro ponto.
Pequeno demais, os primeiros pontos pesam mais do que deviam e o viés
PERMANECE, não só demora a sumir.

Maior é melhor **até um ponto que depende dos seus dados**, e o default não é
esse ponto: é um valor seguro. Medido com 3 preditoras bem escaladas, o erro
continua caindo de `1e6` até algo em torno de `1e8` (chegou a ~7e-11, cem
vezes melhor que o default) e só então o condicionamento numérico de
`P0 = lambda * I` passa a dominar — a `1e12` o erro voltou a ser dezenas de
vezes PIOR que a `1e6`. Onde exatamente fica o ótimo muda com os dados, e é
por isso que o default é conservador em vez de agressivo.

**A exceção que inverte o conselho:** com preditoras em escalas muito
diferentes entre si, subir `lambda` PIORA. Medido, uma coluna na escala de 1e5
contra as outras em torno de 1: erro da ordem de 1e-6 a `lambda = 1e4` e de 1e-5 a 1e-4 a
`lambda = 1e6` — dezenas de vezes pior no valor maior na mediana, e em algumas
amostras o sinal chega a inverter. Se as colunas não estão
na mesma ordem de grandeza, o conserto é escalá-las (`data/mutate`), não mexer
no `lambda`.

### Resposta e preditoras

**Resposta** é o nome da coluna-alvo no ponto (`y`, por padrão). **Preditores**
em branco usa as demais colunas do ponto, na ordem em que chegam — é assim
porque o ponto já vem do `data/to_stream` com as colunas da tabela de origem,
e repetir a lista aqui só criaria uma segunda lista para desatualizar quando a
tabela ganhasse uma coluna.
]---", r"---[
- **Resposta** — a coluna-alvo, no ponto.
- **Preditores** — em branco, todas as outras colunas do ponto.
- **Prior difusa (lambda)** — menor deixa um viés permanente nos coeficientes;
  maior aproxima do `lm()` batch até um ótimo que depende dos dados (medido
  em torno de 1e8 com preditoras bem escaladas, e o lugar muda com os dados), e daí em diante o
  condicionamento piora. Com colunas em escalas muito diferentes, subir
  `lambda` piora: escale as colunas em vez disso.
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
