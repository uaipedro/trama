#' O driver da região de fluxo: o laço em lockstep, dentro de UMA unidade.
#'
#' A região é uma unidade como qualquer outra do ponto de vista do executor — o
#' que muda é só quem sabe executá-la. Entra chave, sai chave; nada de valor
#' pesado atravessa fronteira de processo. A tese de `worker.R` vale aqui sem
#' emenda.
#'
#' A ordem topológica vem do PLANO (`region$order`), não daqui: o driver só
#' caminha nela. Recalculá-la no worker criaria uma segunda verdade sobre a
#' ordem — e é a ordem do plano que entrou na chave da região.
#'
#' ## O contrato da FONTE
#'
#' O `fn` da fonte roda UMA vez, antes do laço, e devolve a **sequência finita
#' de pontos como uma LISTA** — um elemento por passo, na ordem em que o driver
#' vai andar. `length()` da lista é o número de passos da região, e `list()` é a
#' região de zero pontos.
#'
#' O driver não fatia nada: não sabe o que é uma linha de uma tabela, de um
#' raster ou de um `sf`, e não vai saber. Quem fatia é a fonte, que é o nó de
#' domínio — `data/to_stream` (Fase 6) lê os params `por`, coluna de ordenação
#' e tamanho do lote e devolve a lista já fatiada. Pôr o fatiamento aqui
#' significaria o núcleo conhecendo `data/table`, que é exatamente o que a
#' Decisão 4 do desenho evitou.
#'
#' `NULL` vale como zero pontos, e é o ÚNICO outro valor aceito. Não é frouxura:
#' uma fonte com a entrada opcional solta (`data/to_stream` sem tabela ligada,
#' um `s/fonte` de teste) devolve o próprio default, que é `NULL` — recusar
#' quebraria um grafo que a validação da região aceita. E `length(NULL)` já é 0,
#' então o driver não trata dois casos: normaliza para `list()` e segue. O que
#' NÃO passa é o valor não fatiado (a tabela inteira, um escalar): esse é o
#' esquecimento de verdade do autor da fonte, e é dele que o erro alto protege.
#'
#' Fonte com mais de uma saída devolve lista nomeada pelas portas, como todo nó
#' de várias saídas (`.tr_store_outputs`). Aí:
#'   - porta declarada `stream = TRUE` → a lista de pontos;
#'   - porta comum → um valor só, CONSTANTE em todos os passos.
#' Duas portas de fluxo com contagens de pontos diferentes não têm semântica de
#' lockstep, e param aqui — é o mesmo motivo de `tr_error_stream_multi_source`.
#' E `NULL` vale como zero pontos aqui também: numa fonte de várias saídas ele
#' é lido como "todas as portas vazias", e não como lista de portas faltando.
#'
#' ## O contrato do COLAPSO
#'
#' O colapso NÃO é elevado ponto a ponto: o `fn` dele roda **uma vez, depois do
#' laço**, e recebe a lista com o valor de todos os passos em cada porta
#' alimentada de DENTRO da região. O artefato que ele devolve é o histórico
#' (Decisão 6).
#'
#' "Alimentada de dentro", e não "porta de fluxo": o que decide é de onde vem a
#' aresta, não como a porta foi declarada. Uma porta COMUM do colapso ligada a
#' um nó da região também acumula — se a fonte devolve `resumo = "x"` numa porta
#' comum e o colapso a consome, ele recebe `list("x", "x", "x")`, uma cópia por
#' passo, e não a constante. É consistente (a mesma regra do laço, aplicada ao
#' colapso), mas se lê ao contrário do parágrafo da fonte acima, e quem escreve
#' um `from_stream(x, resumo)` cai nisso primeiro: pega `n` cópias e ou quebra,
#' ou faz `resumo[[1]]` e esconde o mal-entendido. Só porta vinda de FORA da
#' região é constante.
#'
#' O esboço do plano fazia o contrário — aplicava o colapso por ponto e o driver
#' acumulava os retornos. Não fecha: o driver teria que juntar N pedaços num
#' valor único do tipo DECLARADO da saída, e juntar é conhecimento de domínio
#' (`rbind` de tabela, `c()` de vetor, mosaico de raster). O núcleo não tem esse
#' conhecimento, e o palpite genérico (uma `list()` de N pedaços) produziria um
#' artefato que não é do tipo que a porta promete — errado em silêncio, e
#' descoberto lá longe, no nó de gráfico que consome o histórico. Com o colapso
#' recebendo o fluxo inteiro, `data/from_stream` (Fase 6) faz o `rbind`, e é ele
#' o dono de `data/table`.
#'
#' Pelo mesmo motivo é o colapso, e não o driver, que decide o que é o histórico
#' VAZIO de zero pontos: o núcleo não sabe construir o valor vazio de um tipo
#' que não conhece. Com zero pontos, `init` roda, `step` nunca é chamado, e o
#' colapso é chamado uma vez com a lista vazia.
#'
#' Cada colapso da região deixa o próprio histórico na própria saída, pelo mapa
#' `region$outputs` — e a região roda UMA vez para todos eles.
#'
#' ## Dentro da região, todo valor é do PASSO
#'
#' A porta de um membro alimentada de DENTRO da região (`from = "internal"`)
#' recebe o valor daquele passo; alimentada de FORA (`from = "external"`) recebe
#' o artefato do store, lido UMA vez antes do laço e igual em todos os passos —
#' é a "constante vinda de fora da região" do desenho. Ler por passo seriam N
#' mil leituras do mesmo artefato, e o que vem de fora da região é justamente o
#' mais pesado do fluxo (o modelo treinado).
#'
#' No colapso a mesma distinção decide o que é acumulado: porta alimentada de
#' dentro acumula (é fluxo), porta alimentada de fora é constante.
#'
#' ## O que `.seed` significa dentro da região
#'
#' A regra de QUEM recebe é a mesma de um nó solto: recebe quem DECLARA `.seed`
#' entre os formais (`.tr_wants_seed()`, em `node.R`) — e a função relevante é o
#' `fn` do membro elevado, da fonte e do colapso, e o `step` do membro com
#' memória. O que MUDA é o valor:
#'
#'   - FONTE e COLAPSO recebem a própria seed do documento
#'     (`region$nodes[[id]]$seed`), sem derivação. O `fn` dos dois roda UMA vez,
#'     antes e depois do laço, e é o mesmo `fn` que roda solto no nível 1 da API:
#'     passar outra coisa faria o MESMO nó se comportar diferente dentro e fora
#'     de uma região. "A seed desta invocação" não tem o que variar quando existe
#'     uma invocação só.
#'   - MEMBRO ELEVADO e `step` de membro COM MEMÓRIA recebem uma seed DERIVADA de
#'     (id do membro, seed do membro, índice do passo) — `.tr_region_step_seed()`.
#'     Aí sim há uma invocação por ponto, e é a elevação que cria a pergunta "a
#'     seed de qual invocação?". Com a seed crua repetida em todos os passos, um
#'     nó de ruído no meio do fluxo faria `set.seed()` reiniciar o RNG a cada
#'     ponto e sortearia o MESMO valor n vezes: um ruído que é uma constante, e
#'     um histórico que nada denuncia.
#'   - `init` NÃO recebe `.seed`, e `node.R` recusa o formal na declaração: ele
#'     não roda num run retomado (o estado vem do checkpoint), então um `init`
#'     que sorteasse daria história diferente na retomada.
#'
#' Por que derivar por HASH e não por `seed + i`: a chave da região já inclui a
#' seed de um membro estocástico (`.tr_region_key()`), então a chave promete que
#' o histórico depende dela — e antes desta derivação o histórico dependia, de
#' fato, do RNG global do processo que pegou a unidade. Com `seed + i` a promessa
#' voltaria a ser falsa de outro jeito: o membro A com seed 1 no passo 2 colide
#' com o membro B com seed 2 no passo 1, e dois membros que o autor do fluxo
#' escreveu como independentes sorteiam idêntico. A identidade do membro entra no
#' hash exatamente por isso.
#'
#' E por que a derivação tem que ser PURA e estável: o checkpoint (Tarefa 5.2)
#' não guarda o estado do RNG de propósito — restaurar `.Random.seed` num daemon
#' vazaria para a próxima unidade que rodasse ali. A seed de cada passo sendo
#' função só de (membro, passo), o run retomado refaz cada passo com a mesma seed
#' que o ininterrupto usaria, e o histórico sai byte a byte igual. Pelo mesmo
#' motivo ela não pode depender de locale nem de ordem de iteração: a derivação
#' roda no DAEMON, e uma que divergisse entre coordenador e daemon faria adoção e
#' retomada produzirem outra história sob a mesma chave (é a razão de
#' `sort(method = "radix")` em `hash.R`, aplicada aqui).
#' @noRd
.tr_run_region <- function(unit, registry, store, ctx_extra = NULL) {
  rg <- unit$region
  specs <- lapply(stats::setNames(rg$order, rg$order),
                  function(id) tr_get_node(rg$nodes[[id]]$node_type, registry))

  # `.ctx` da região: a unidade pede (`wants_ctx = TRUE` em `plan.R`) e quem
  # honra é ESTE driver, não o bloco de `.tr_run_unit()` — o despacho pra cá
  # continua sendo a primeira coisa lá, por um motivo que não mudou (ver
  # `worker.R`). Nenhum membro recebe este ctx: nó elevado tem `.ctx` PROIBIDO
  # (Fase 2) e `init`/`step` não o tomam. O usuário é o laço, que publica
  # progresso e o parcial de cada nó pelo store — o mesmo canal, e o mesmo
  # arquivo, de um nó longo comum.
  ctx <- if (isTRUE(unit$wants_ctx)) .tr_make_ctx(unit, store, ctx_extra) else NULL

  # Cadência de publicação, em segundos. O laço roda uma vez por PONTO: publicar
  # em todo passo de uma região de dez mil pontos escreveria o JSON de progresso
  # dez mil vezes, e o coordenador só o lê a cada ~50ms — a maioria esmagadora
  # das escritas nunca seria lida. Com o estrangulamento o custo é no máximo
  # `1/publish_every` publicações por segundo (cada uma: uma escrita do JSON por
  # membro publicável), INDEPENDENTE do número de pontos.
  #
  # Não é param do documento de propósito (Decisão 9): cadência é estado de
  # sessão e não conteúdo do grafo — se entrasse no documento, mudar a cadência
  # mudaria a chave da região e recomputaria o fluxo inteiro. Chega por
  # `ctx_extra`, que atravessa a fronteira de processo junto com a chamada; a
  # Tarefa 5.3 é que a põe no arquivo de controle do store, e até lá o default
  # vale sem que arquivo nenhum precise existir.
  cadencia <- ctx_extra$publish_every %||% 0.1

  # Cadência do CHECKPOINT, em PASSOS (não em segundos, ao contrário da de
  # publicação): o que o checkpoint protege é trabalho, e trabalho aqui se conta
  # em pontos. "Perdi no máximo 99 passos" é uma garantia que o autor do fluxo
  # entende; "perdi no máximo 2 segundos" depende de quanto cada ponto demora.
  #
  # Chega por `ctx_extra` pelo mesmo motivo de `publish_every` (Decisão 9): é
  # botão operacional, não conteúdo do grafo — como param do documento, mudar a
  # cadência mudaria a chave e recomputaria o fluxo inteiro, que é exatamente o
  # que esta tarefa existe pra evitar.
  #
  # Default 100, e o que isso custa: cada checkpoint serializa o acumulador
  # INTEIRO (o histórico até aqui), então o custo é `n/100` escritas de um
  # acumulador que cresce — em dez mil pontos, 100 escritas, e ~50 vezes o
  # tamanho do histórico final em bytes no disco ao longo do run. Num histórico
  # de dez megabytes é meio gigabyte de escrita; num de um gigabyte é cinquenta,
  # e aí o número tem que subir. O outro lado é o que a Fase 4 mediu: sem
  # checkpoint, morrer no passo 9.999 custa os 9.999. 100 é o meio honesto, e
  # `checkpoint_every = 0` desliga.
  #
  # Valor torto (NA, texto, negativo) DESLIGA em vez de abortar, pelo mesmo
  # motivo do `tryCatch` no tipo do parcial mais abaixo: uma região que roda hoje
  # não pode parar de rodar por causa de um botão de operação. Desligado é o
  # comportamento de antes desta tarefa, que é o pior aceitável; abortar o laço
  # de dez mil pontos por causa de um `checkpoint_every` mal digitado não é.
  ckpt_cada <- suppressWarnings(as.integer(ctx_extra$checkpoint_every %||% 100L))
  if (length(ckpt_cada) != 1L || is.na(ckpt_cada) || ckpt_cada < 0L) ckpt_cada <- 0L

  # 1. As entradas COMUNS da região, lidas UMA vez. `.tr_load_ref()` é o mesmo
  # de um nó qualquer: o adaptador da aresta roda lá, e a impressão dele já
  # entrou na chave da região (plan.R). Escrever um segundo carregador aqui
  # criaria duas verdades sobre como um input vira valor.
  ext <- lapply(unit$inputs, function(ref) {
    # `unit$inputs[[nm]]` é UMA ref em porta simples e uma LISTA de refs em
    # porta variádica. O `pos` da fonte externa indexa dentro da porta, então
    # normalizar tudo para lista aqui é o que faz `pos` sempre valer.
    if (!is.null(ref$key)) list(.tr_load_ref(ref, registry, store))
    else lapply(ref, function(r) .tr_load_ref(r, registry, store))
  })

  # `t0` só agora: `.tr_run_unit()` cronometra o `do.call` do `fn` e não a
  # leitura dos inputs, e o `duration` do handle tem que significar a mesma
  # coisa nos dois — é o número que o front mostra no card. Subir esta linha
  # para antes do `ext` não quebra teste nenhum de resultado (o histórico sai
  # idêntico), então o que a mantém no lugar é o teste de `duration` em
  # `test-stream-driver.R`, com um `restore` lento: sem ele, a região passaria a
  # medir a leitura do artefato de fora — o modelo treinado — e o card diria que
  # o laço de dez mil pontos levou o tempo de carregar um arquivo.
  t0 <- Sys.time()

  # 2. A fonte roda uma vez e entrega os pontos.
  fonte <- rg$source[[1]]
  fs <- specs[[fonte]]
  if (any(vapply(rg$nodes[[fonte]]$inputs, .tr_region_interna, logical(1)))) {
    # A fonte roda ANTES do laço: não existe "valor do passo" pra ela consumir.
    # Hoje é inalcançável (uma aresta de dentro da região pra fonte fecharia
    # ciclo), e é rede de segurança pra um furo futuro na detecção — sem ela o
    # driver leria `NULL` e a região rodaria com pontos errados, calada.
    rlang::abort(sprintf(
      paste0("A fonte '%s' da região de fluxo '%s' recebe valor de dentro da própria região. ",
             "A fonte roda antes do laço e não tem passo de onde ler."), fonte, rg$id),
      class = "tr_error_stream_bad_source")
  }
  # A seed da fonte é a DELA, sem derivação: o `fn` roda uma vez (ver o
  # cabeçalho). Uma fonte que sorteia quais pontos emitir tem que emitir os
  # mesmos dentro e fora da região.
  saida <- do.call(fs$fn, .tr_region_args(
    fonte, rg, specs, registry, ext, list(),
    seed = if (.tr_wants_seed(fs$fn)) rg$nodes[[fonte]]$seed))
  # "NULL vale como zero pontos" (ver o cabeçalho) só valia para a fonte de UMA
  # saída: com mais de uma, `.tr_region_route()` exige a lista nomeada pelas
  # portas e abortava `tr_error_bad_output` ANTES da normalização lá embaixo.
  # Quem paga é justamente a fonte que o contrato prevê — `data/to_stream` com a
  # tabela desligada devolve o próprio default, que é NULL —, e a região morria
  # com um erro sobre "não devolveu as portas" num grafo que a validação ACEITA.
  # NULL da fonte é "todas as portas vazias", que é a única leitura sensata: a
  # porta de fluxo fica sem pontos e a porta comum fica sem valor.
  #
  # Normalizado AQUI e não dentro de `.tr_region_route()` porque lá a regra vale
  # também para os MEMBROS, e um membro de duas saídas que devolve NULL num
  # passo é erro de verdade: aceitá-lo daria NULL calado em cada porta.
  if (is.null(saida) && length(fs$outputs) > 1L) {
    saida <- stats::setNames(vector("list", length(fs$outputs)), names(fs$outputs))
  }
  vals <- .tr_region_route(saida, fonte, fs)

  fluxos <- names(fs$outputs)[vapply(fs$outputs, function(p) isTRUE(p$stream), logical(1))]
  for (pn in fluxos) {
    # `NULL` -> zero pontos (ver o cabeçalho). Normalizado AQUI e uma vez só,
    # pra que o laço e o colapso não tenham que saber que existem duas formas.
    if (is.null(vals[[pn]])) vals[pn] <- list(list())
    # `!is.null(dim())` além de `!is.list()` porque um `data.frame` É uma lista: com
    # `!is.list()` sozinho, a fonte que esquece de fatiar e devolve a TABELA
    # INTEIRA passa pela guarda, e o driver anda nela como uma lista de colunas
    # — três colunas viram um fluxo de três pontos, o `fn` elevado recebe o
    # vetor inteiro de cada coluna e a região TERMINA, com um histórico de três
    # linhas que vai pro store e é servido do cache pra sempre. É o esquecimento
    # mais provável do autor de uma fonte, e é o modo de falha calado contra o
    # qual este arquivo inteiro foi escrito.
    #
    # O teste é "isto é um RETÂNGULO?", não "isto tem classe?". `dim()` pega
    # `data.frame`, `tbl_df`, `sf` e `data.table` pelo mesmo caminho, SEM o
    # núcleo conhecer tipo de domínio nenhum — é essa a propriedade que importa,
    # e é por isso que a guarda não é uma lista de classes proibidas.
    #
    # A versão anterior usava `is.object()`, e isso recusava fatiamento
    # LEGÍTIMO: `dplyr::group_split()` — a forma mais idiomática de partir uma
    # tabela por coluna, que é exatamente o que `data/to_stream(por = )` faz —
    # devolve um `vctrs_list_of`, que tem classe e não tem `dim`. A fonte havia
    # fatiado certo e ouvia "quem fatia é a fonte". Recusar o certo com a
    # mensagem errada é pior que não recusar.
    if (!is.list(vals[[pn]]) || !is.null(dim(vals[[pn]]))) {
      rlang::abort(sprintf(
        paste0("A fonte '%s' (%s) devolveu %s na porta de fluxo '%s', e o driver espera uma ",
               "LISTA de pontos — um elemento por passo, `list()` ou NULL se não houver nenhum. ",
               "Uma lista de pontos não tem dimensão; quem fatia é a fonte."),
        fonte, fs$id, class(vals[[pn]])[[1]], pn),
        class = "tr_error_stream_bad_source")
    }
  }
  ns <- vapply(fluxos, function(pn) length(vals[[pn]]), integer(1))
  if (length(unique(ns)) > 1L) {
    rlang::abort(sprintf(
      paste0("A fonte '%s' devolveu contagens de pontos diferentes nas portas de fluxo (%s). ",
             "Lockstep de sequências de tamanhos diferentes não tem semântica definida."),
      fonte, paste(sprintf("%s: %d", names(ns), ns), collapse = ", ")),
      class = "tr_error_stream_bad_source")
  }
  n <- if (length(ns)) ns[[1]] else 0L

  # 3. O CHECKPOINT, se houver um sob esta chave. Lido aqui — depois de `n` e
  # ANTES de `init` — por duas razões: `n` é o que valida o checkpoint (ver
  # `.tr_ckpt_read()`), e retomar não pode rodar `init`, cujo resultado seria
  # descartado em seguida pelo estado gravado. Um `init` que aloca caro pagaria
  # duas vezes por run retomado.
  #
  # Um checkpoint de um run cuja chave MUDOU nunca é lido, por construção: o
  # diretório se chama pela chave, e chave diferente é caminho diferente. O que
  # sobra é o diretório órfão, e disso cuida a varredura de `tr_store_gc()`.
  ck <- if (ckpt_cada > 0) .tr_ckpt_read(store, unit$key, n) else NULL
  inicio <- if (is.null(ck)) 0L else ck$i

  # `init` roda UMA vez, antes do laço, e recebe só os params que declara —
  # `node.R` valida `init` como função de params, e só de params.
  estado <- list()
  if (is.null(ck)) {
    for (id in rg$order) {
      if (!isTRUE(rg$nodes[[id]]$online)) next
      ini <- specs[[id]]$init
      # `estado[id] <- list(v)`, e não `estado[[id]] <- v`: um `init` que devolve
      # NULL (estado que só nasce no primeiro passo) REMOVERIA a entrada, e o
      # `step` receberia o estado de outro nó — ou nenhum.
      estado[id] <- list(do.call(ini, rg$nodes[[id]]$params[names(formals(ini))]))
    }
  } else {
    # É aqui que a Decisão 8 se paga: o estado é explícito e serializável porque
    # `init`/`step` o devolvem como VALOR. Uma closure com o estado no ambiente
    # não teria como voltar do disco, e este arquivo inteiro teria que recomeçar
    # do passo 1 a cada morte do worker.
    #
    # Contrapartida que o autor do nó carrega: estado que não sobrevive a um
    # `saveRDS` (ponteiro externo, conexão aberta, ambiente com referência a
    # processo) volta quebrado, e o driver não tem como detectar isso. Estado de
    # nó com memória é valor de R, não recurso.
    estado <- ck$estado
  }

  # 4. O laço. O histórico de cada porta de colapso alimentada de dentro cresce
  # aqui; `vector("list", n)` porque o número de passos é conhecido — `c()` por
  # passo copiaria a lista inteira a cada ponto (quadrático em 10 mil pontos).
  acc <- list()
  for (cid in rg$collapse) {
    portas <- Filter(function(pn) .tr_region_interna(rg$nodes[[cid]]$inputs[[pn]]),
                     names(rg$nodes[[cid]]$inputs))
    for (pn in portas) acc[[cid]][pn] <- list(vector("list", n))
  }
  # O acumulado do checkpoint substitui a pré-alocação. A FORMA casa por
  # construção — quais colapsos, quais portas e de onde cada porta é alimentada
  # entraram todos na chave da região —, e o COMPRIMENTO é o que
  # `.tr_ckpt_read()` confere contra `n`.
  if (!is.null(ck)) acc <- ck$hist

  # Quem tem parcial pra publicar, e com QUAL tipo. O colapso não entra: o `fn`
  # dele só roda depois do laço, e o artefato dele sai por chave de store como o
  # de qualquer nó. O tipo é o da PRIMEIRA porta de saída do membro no registro
  # — é o valor que ele acabou de produzir, e é o mesmo critério que
  # `.tr_run_unit()` usa pra `unit$partial_type`. Vem do registro, e não de
  # `region$nodes`, porque é o registro que tem `preview`; o plano só carrega o
  # id do tipo.
  #
  # `tryCatch` no tipo: tipo de porta interna nunca foi exigido no registro
  # local (a chave da região não guarda a impressão dele, justamente porque nada
  # dela vai ao disco), e abortar aqui faria uma região que RODA hoje parar de
  # rodar por causa da barra de progresso. Sem tipo, o nó publica sem preview.
  publicaveis <- list()
  for (id in if (is.null(ctx)) character() else rg$order) {
    if (identical(rg$nodes[[id]]$role, "collapse")) next
    pn <- names(specs[[id]]$outputs)
    if (length(pn) == 0) next
    publicaveis[[length(publicaveis) + 1L]] <- list(
      id = id, de = paste0(id, ":", pn[[1]]),
      tipo = tryCatch(tr_get_type(specs[[id]]$outputs[[pn[[1]]]]$type, registry),
                      error = function(e) NULL))
  }
  # `-Inf` e não `Sys.time()`: o piso é o passo 1. Com o relógio de agora, uma
  # região de poucos pontos rápidos terminaria sem publicar nada e o card
  # ficaria em branco do começo ao fim, que é exatamente o sintoma que esta
  # tarefa existe pra tirar.
  ultima <- -Inf

  # `seq.int(inicio + 1L, n)` só quando há passo a dar: com `inicio == n` (o run
  # morreu DEPOIS do laço, num colapso) ele contaria para trás e o laço rodaria
  # o fluxo ao contrário. `integer()` é "nada a fazer", e o colapso roda direto.
  for (i in if (inicio < n) seq.int(inicio + 1L, n) else integer()) {
    # Os valores DO PASSO, por `nó:porta`. Zerado a cada passo de propósito: um
    # membro que lesse o valor do passo anterior (porque o produtor não rodou
    # neste) andaria com dado velho em silêncio, e a porta ausente erra alto no
    # `fn` do consumidor.
    cur <- list()
    for (pn in names(vals)) {
      cur[paste0(fonte, ":", pn)] <-
        list(if (pn %in% fluxos) vals[[pn]][[i]] else vals[[pn]])
    }

    for (id in rg$order) {
      if (identical(id, fonte)) next
      m <- rg$nodes[[id]]; spec <- specs[[id]]
      # A seed DESTE passo, para quem declara `.seed`. A função relevante é o
      # `step` do membro com memória e o `fn` do elevado — no colapso esta
      # chamada só ACUMULA portas, e o `fn` dele (que leva a seed crua) roda
      # depois do laço. Sem isto, um membro que força `.seed` morria com
      # "argumento ausente" e um que só o declara rodava calado com o RNG global
      # do daemon, sob uma chave que promete depender da seed.
      f_seed <- if (identical(m$role, "collapse")) NULL
                else if (isTRUE(m$online)) spec$step else spec$fn
      args <- .tr_region_args(
        id, rg, specs, registry, ext, cur,
        seed = if (.tr_wants_seed(f_seed)) .tr_region_step_seed(id, m$seed, i))

      if (identical(m$role, "collapse")) {
        # O colapso só ACUMULA aqui; o `fn` dele roda depois do laço.
        for (pn in names(acc[[id]])) acc[[id]][[pn]][i] <- list(args[[pn]])
        next
      }

      if (isTRUE(m$online)) {
        # Nó COM MEMÓRIA: `step(state, <inputs/params que declara>)`. Só os
        # formais declarados entram — `node.R` garante que são um subconjunto de
        # inputs e params, e passar o resto faria `do.call` falhar num nó que
        # declara menos do que tem.
        st <- spec$step
        formais <- setdiff(names(formals(st)), "state")
        sargs <- c(list(state = estado[[id]]), args[intersect(formais, names(args))])
        r <- .tr_region_call(st, sargs, id, i, n, rg$id)
        # `step` devolve SEMPRE `list(state =, out =)` (Decisão 8), e `node.R`
        # validou a DECLARAÇÃO — não o retorno, que é código que só roda aqui.
        # Sem esta checagem, um `step` que devolve o ponto direto viraria
        # `state = NULL` no passo seguinte, e o histórico sairia plausível e
        # errado.
        if (!is.list(r) || !all(c("state", "out") %in% names(r))) {
          rlang::abort(sprintf(
            paste0("O 'step' de '%s' devolveu %s no passo %d da região de fluxo '%s'. ",
                   "'step' devolve sempre list(state = , out = )."),
            id, if (is.list(r)) paste(names(r), collapse = ", ") else class(r)[[1]], i, rg$id),
            class = "tr_error_bad_step")
        }
        estado[id] <- list(r$state)
        valor <- r$out
      } else {
        # Nó PURO ELEVADO: o `fn` do catálogo, aplicado ao ponto do passo. É a
        # elevação automática do desenho — com ela, `online` passa a significar
        # só "eu carrego estado entre os pontos".
        valor <- .tr_region_call(spec$fn, args, id, i, n, rg$id)
      }
      saidas <- .tr_region_route(valor, id, spec)
      for (pn in names(saidas)) cur[paste0(id, ":", pn)] <- list(saidas[[pn]])
    }

    # Publicado DEPOIS do passo inteiro, e nunca no meio: no meio, o mapa
    # misturaria o valor deste passo nos nós que já correram com o do passo
    # anterior nos que não — um retrato que nunca existiu, e plausível.
    #
    # O valor do nó é o do PASSO, e é o mesmo para nó elevado e nó com memória:
    # o que o membro acabou de emitir (`out`, no caso do com memória — não o
    # `state`). É o que desce pra jusante naquele passo, então é o que o card
    # tem que mostrar; o histórico não existe antes do colapso.
    # `is.null(ctx)` PRIMEIRO, e o relógio só depois: sem ctx não se paga nem o
    # `Sys.time()` por ponto num laço que roda dez mil vezes.
    if (!is.null(ctx)) {
      agora <- as.numeric(Sys.time())
      if (agora - ultima >= cadencia) {
        ultima <- agora
        ctx$progress(i / n, sprintf("ponto %d de %d", i, n))
        for (pb in publicaveis) ctx$partial_node(pb$id, cur[[pb$de]], pb$tipo)
      }
    }

    # O checkpoint, DEPOIS do passo inteiro: gravar no meio guardaria um
    # `estado` de alguns membros já no passo `i` e de outros ainda no `i-1`, e a
    # retomada continuaria de um retrato que nunca existiu — plausível, e errado
    # em silêncio. O `i` gravado é sempre um passo COMPLETO, e é isso que faz o
    # checkpoint nunca envenenar a retomada de um run que falhou: o passo que
    # quebrou não está nele, e é refeito.
    #
    # `i < n` porque o último passo não vale uma escrita do acumulador inteiro:
    # o colapso roda em seguida e o diretório sai. Se o colapso é que falha, a
    # retomada volta ao último múltiplo da cadência e refaz no máximo
    # `ckpt_cada` passos — barato, contra uma serialização do histórico inteiro
    # em TODO run que termina bem.
    if (ckpt_cada > 0 && i < n && i %% ckpt_cada == 0) {
      .tr_ckpt_write(store, unit$key, i, n, estado, acc)
    }
  }

  # 5. Cada colapso roda UMA vez, com o fluxo inteiro.
  valores <- lapply(stats::setNames(rg$collapse, rg$collapse), function(cid) {
    # Seed crua, como a da fonte e pelo mesmo motivo: o `fn` do colapso roda uma
    # vez, com o fluxo inteiro, e é o mesmo `fn` de fora da região.
    args <- .tr_region_args(cid, rg, specs, registry, ext, list(), acc = acc[[cid]],
                            seed = if (.tr_wants_seed(specs[[cid]]$fn)) rg$nodes[[cid]]$seed)
    # O colapso é nomeado no erro porque o evento `failed` da região sai sob
    # `u$node` — o PRIMEIRO colapso. Numa região de dois, a falha do segundo
    # apareceria no card do primeiro sem uma palavra sobre de quem foi.
    tryCatch(do.call(specs[[cid]]$fn, args), error = function(cnd) {
      rlang::abort(sprintf(
        "O colapso '%s' falhou ao montar o histórico de %d ponto(s) da região de fluxo '%s': %s",
        cid, n, rg$id, conditionMessage(cnd)),
        class = "tr_error_stream_collapse", parent = cnd)
    })
  })
  # `duration` é o tempo da REGIÃO INTEIRA — fonte, laço e todos os colapsos —,
  # e é o MESMO número gravado no handle de cada saída de cada colapso. Numa
  # região de dois colapsos o front mostra o mesmo tempo nos dois cards, e isso
  # é de propósito: a região é uma unidade e rodou uma vez, então não existe "o
  # tempo do colapso 2" para atribuir a ele. Fica escrito aqui porque o card não
  # diz, e sem isto alguém somaria os dois e concluiria o dobro do tempo real.
  #
  # Numa região RETOMADA de checkpoint isto mede só a tentativa que terminou —
  # os passos vindos do disco não estão aqui, e o card vai dizer menos tempo do
  # que o fluxo custou de fato. É a leitura certa do número que o executor
  # cronometra ("quanto durou este `fn`"), e somar as tentativas exigiria pôr
  # tempo acumulado no checkpoint: aí `duration` passaria a significar duas
  # coisas diferentes na região e no nó comum.
  duration <- as.numeric(Sys.time() - t0, units = "secs")

  # Gravar por `.tr_store_outputs()` é o que faz o `store`/`preview`/`summary` do
  # tipo ser o mesmo caminho de um nó comum. Um caminho paralelo aqui faria o
  # artefato da região divergir do de todo o resto sem que nenhum teste de tipo
  # percebesse.
  handles <- list()
  for (cid in rg$collapse) {
    hs <- .tr_store_outputs(.tr_region_sub_unit(unit, cid),
                            valores[[cid]], registry, store, duration)
    # Renomeados para o nome da saída NA REGIÃO ("<colapso>:<porta>" quando há
    # mais de um colapso). É assim que `unit$outputs` e o `u$handles` do caminho
    # `cached` (plan.R) estão nomeados, e o front faz `handles.out || primeiro
    # valor` (`editor.js`). Com o nome de PORTA cru, dois colapsos dariam duas
    # chaves "out" no mesmo evento `done` — chave repetida no JSON, e o `done`
    # divergindo do `cached` do MESMO grafo.
    mapa <- rg$outputs[[cid]]
    nomes <- if (is.null(names(hs))) unname(mapa) else unname(mapa)[match(names(hs), names(mapa))]
    handles <- c(handles, stats::setNames(hs, nomes))
  }

  # A unidade concluiu: o diretório sai, e o artefato final o substitui. DEPOIS
  # dos handles, e não antes: entre apagar o checkpoint e gravar o artefato há
  # uma janela em que a chave não tem nem um nem outro, e um worker morto nela
  # custaria o run inteiro. Nesta ordem o pior caso é um diretório órfão.
  #
  # É o ÚNICO caminho que apaga. Falha no `step`, falha no colapso e worker
  # morto guardam o checkpoint de propósito: o que está nele são passos
  # completos de uma computação determinística sob uma chave que não mudou —
  # jogá-lo fora custaria os 9.999 pontos bons por causa do erro no 10.000º, que
  # é a situação que esta tarefa existe pra atender. Quem apaga o que sobra é
  # `tr_store_gc()` (por idade) e `tr_bust()` (porque lá o código mudou sem a
  # chave mudar, e retomar serviria um histórico metade velho).
  .tr_ckpt_clear(store, unit$key)
  handles
}

#' O diretório da região no store: `<store>/stream/<chave-da-unidade>/`.
#'
#' Pela chave da UNIDADE, que é a mesma com que `.tr_make_ctx()` nomeia o
#' arquivo de progresso e com que o `collect()` do scheduler faz o poll — e não
#' pelas chaves de SAÍDA, que são `hash(chave da unidade, porta)` e são várias
#' por região. Um diretório por unidade é o que a Tarefa 5.3 vai encontrar para
#' pôr o arquivo de controle (pause/step/tempo) ao lado do checkpoint.
#' @noRd
.tr_stream_dir <- function(store, key) file.path(store$root, "stream", key)
.tr_ckpt_path  <- function(store, key) file.path(.tr_stream_dir(store, key), "ckpt.rds")

#' Grava o checkpoint por RDS CRU — e essa palavra é a condição inteira.
#'
#' A chave da região NÃO inclui a impressão digital dos tipos das portas
#' INTERNAS (ver `.tr_region_key()` em `hash.R`), e o raciocínio que autoriza
#' isso é: nada de dentro da região atravessa o `store`/`restore` de um tipo. No
#' instante em que este arquivo gravar estado interior PELO TIPO, o código
#' daquele tipo passa a ser insumo do histórico que sai sob esta chave, e tem
#' que entrar na chave no MESMO commit — senão editar o `store` de um tipo
#' interno serve, do cache, um histórico que aquele código não produziria mais.
#' `saveRDS` do estado e do acumulado mantém a condição de pé.
#'
#' `.tr_atomic()` pela mesma razão do artefato (`store.R`): o worker pode morrer
#' no meio da escrita, e meio `ckpt.rds` sob uma chave válida é
#' indistinguível de um bom — para sempre. Aqui é pior que no artefato, porque a
#' retomada leria estado truncado e seguiria como se fosse estado.
#'
#' Dois workers na mesma chave (duas sessões sobre o mesmo store) não se
#' corrompem: cada escrita é atômica e cada checkpoint é um PREFIXO da mesma
#' computação determinística, então qualquer um dos dois serve para retomar.
#' @noRd
.tr_ckpt_write <- function(store, key, i, n, estado, hist) {
  dir.create(.tr_stream_dir(store, key), recursive = TRUE, showWarnings = FALSE)
  .tr_atomic(store, .tr_ckpt_path(store, key), function(tmp) {
    saveRDS(list(i = i, n = n, estado = estado, hist = hist), tmp)
  })
  invisible(TRUE)
}

#' Lê o checkpoint da chave, ou `NULL` se não houver um utilizável.
#'
#' Ilegível conta como AUSENTE, nunca como exceção — o mesmo argumento de
#' `tr_store_handle()`: um arquivo truncado não pode deixar uma região
#' permanentemente não-rodável, e o custo de cair aqui é um run do zero.
#'
#' `n` é conferido porque a FONTE é isenta da liftabilidade e pode ser impura:
#' um `to_stream` que lê arquivo devolve 300 pontos hoje e 250 amanhã sob a
#' mesma chave. Retomar com um acumulador de outro tamanho misturaria duas
#' leituras num histórico do tamanho certo — plausível, e errado.
#' @noRd
.tr_ckpt_read <- function(store, key, n) {
  p <- .tr_ckpt_path(store, key)
  if (!file.exists(p)) return(NULL)
  ck <- tryCatch(readRDS(p), error = function(e) NULL)
  if (!is.list(ck) || !all(c("i", "n", "estado", "hist") %in% names(ck))) return(NULL)
  if (!identical(as.integer(ck$n), as.integer(n))) return(NULL)
  if (!is.numeric(ck$i) || ck$i < 1L || ck$i > n) return(NULL)
  ck$i <- as.integer(ck$i)
  ck
}

#' @noRd
.tr_ckpt_clear <- function(store, key) {
  unlink(.tr_stream_dir(store, key), recursive = TRUE)
  invisible(TRUE)
}

#' Chama o `fn`/`step` de um membro e, se ele explodir, nomeia o nó e o PASSO.
#'
#' O passo entra na mensagem porque é o único jeito de o autor do nó reproduzir:
#' "a região falhou" num fluxo de dez mil pontos não diz onde olhar. A causa
#' original vai como `parent` e no texto, pelo mesmo motivo — sem ela o autor
#' recebe "falhou no passo 3" e nada sobre o quê.
#' @noRd
.tr_region_call <- function(f, args, id, i, n, region_id) {
  tryCatch(do.call(f, args), error = function(cnd) {
    rlang::abort(sprintf("Nó '%s' falhou no passo %d de %d da região de fluxo '%s': %s",
                         id, i, n, region_id, conditionMessage(cnd)),
                 class = "tr_error_stream_step", parent = cnd)
  })
}

#' Argumentos de um membro num passo: input por input, na forma que o `fn`
#' espera.
#'
#' `acc` presente = é o colapso, depois do laço: a porta alimentada de dentro
#' recebe a lista de TODOS os passos em vez do valor de um. É o único ponto em
#' que interna e externa mudam de significado, e é o contrato do colapso.
#'
#' `seed` é o valor de `.seed` desta invocação, ou `NULL` para não passar o
#' argumento. QUEM decide é o chamador, porque a função relevante muda com o
#' papel do membro (o `step` do com memória, o `fn` dos outros) e o valor muda
#' com ele também (derivado por passo no elevado, cru na fonte e no colapso) —
#' ver o cabeçalho. Aqui só se monta a lista de argumentos.
#' @noRd
.tr_region_args <- function(id, rg, specs, registry, ext, cur, acc = NULL, seed = NULL) {
  m <- rg$nodes[[id]]; spec <- specs[[id]]
  args <- list()
  for (pn in names(spec$inputs)) {
    srcs <- m$inputs[[pn]]
    # Porta opcional solta não é passada, e o `fn` cai no próprio default —
    # idêntico a `.tr_run_unit()`. Passar `NULL` faria o `fn` usar NULL em vez
    # do default declarado, o que é OUTRA coisa.
    if (length(srcs) == 0) next
    v <- if (!is.null(acc[[pn]])) {
      acc[[pn]]
    } else {
      vs <- lapply(srcs, function(s) {
        if (identical(s$from, "external")) return(ext[[s$input]][[s$pos]])
        val <- cur[[paste0(s$node, ":", s$port)]]
        # O adaptador da aresta INTERNA roda por passo, aqui, e não uma vez: o
        # valor que ele converte é o ponto, que é outro a cada passo.
        if (is.null(s$adapter)) return(val)
        ad <- tr_adapter_for(s$adapter$from, s$adapter$to, registry)
        if (is.null(ad)) {
          rlang::abort(sprintf("Adaptador %s -> %s não está registrado neste worker.",
                               s$adapter$from, s$adapter$to),
                       class = "tr_error_unknown_adapter")
        }
        ad$fn(val)
      })
      if (isTRUE(spec$inputs[[pn]]$multiple)) vs else vs[[1]]
    }
    # `args[[pn]] <- NULL` REMOVE o elemento; `args[pn] <- list(v)` atribui. Um
    # nó que legitimamente devolve NULL num passo (filtro sem linha) faria o
    # consumidor falhar com "argumento ausente, sem padrão" — ver
    # `.tr_run_unit()`.
    args[pn] <- list(v)
  }
  for (nm in names(m$params)) args[nm] <- list(m$params[[nm]])
  # Depois dos params, e nunca antes: `.seed` é nome reservado (`node.R`), então
  # param nenhum pode sobrescrevê-lo.
  if (!is.null(seed)) args$.seed <- seed
  args
}

#' A seed de UMA invocação de um membro elevado: derivada do membro e do passo.
#'
#' Três propriedades, e cada uma fecha um furo (o raciocínio está no cabeçalho
#' deste arquivo):
#'   - DETERMINÍSTICA: `rlang::hash()` dos mesmos três valores dá o mesmo dígito
#'     em qualquer processo, sem tocar em `.Random.seed`. É o mesmo hash com que
#'     o plano monta a chave da região, então "estável entre coordenador e
#'     daemon" já é condição de vida do pacote inteiro — não é suposição nova.
#'     Nada aqui depende de locale nem de ordem de iteração: a lista é posicional
#'     e o id de nó é restrito a `[A-Za-z0-9_.-]` (`.tr_check_node_id()`).
#'   - PURA no passo: função só de (id, seed, i), e não do histórico do RNG. É o
#'     que faz a retomada de checkpoint sair idêntica ao run ininterrupto.
#'   - SEM COLISÃO entre pares (membro, passo): a identidade do membro entra no
#'     hash, então `A` com seed 1 no passo 2 e `B` com seed 2 no passo 1 não
#'     caem no mesmo valor — que é o que `seed + i` faria.
#'
#' Sete dígitos hex como em `.tr_new_seed()`: cabe em inteiro de R (máximo
#' 0xFFFFFFF), e `set.seed()` aceita um inteiro único.
#' @noRd
.tr_region_step_seed <- function(id, seed, i) {
  # Mistura PRÓPRIA, e não `rlang::hash()`, por uma assimetria que só existe
  # aqui. A chave de cache também sai de um hash, mas lá uma mudança de hash
  # muda a CHAVE: todo mundo recomputa, e isso é desperdício, não erro. Aqui
  # seria o contrário — a chave ficaria idêntica e o HISTÓRICO mudaria, ou seja
  # o artefato cacheado passaria a discordar do que o mesmo documento produz
  # agora. É o único ponto do sistema em que trocar de hash serve resultado
  # errado em silêncio, e é justamente a promessa que o tempo de passo existe
  # para sustentar ("replayável amanhã e por outra pessoa", Decisão 1).
  #
  # Mesmo precedente do `codetools`, que saiu por licença e virou implementação
  # própria (`hash.R`): quando a estabilidade de terceiro é condição de
  # correção, a conta é nossa. Aqui o custo é seis linhas de inteiro.
  #
  # Mistura de 32 bits em aritmética EXATA. R não tem inteiro sem sinal, e as
  # duas saídas óbvias falham: `bitwAnd`/`bitwShiftR` coagem para inteiro de 32
  # bits com sinal e devolvem NA acima de 2^31, e multiplicar dois valores de 32
  # bits em `double` estoura os 2^53 de exatidão. Então tudo passa por
  # `mult32()`, que parte um dos fatores em metades de 16 bits para que nenhum
  # produto intermediário passe de 2^48 — exato em dupla precisão, e idêntico em
  # qualquer plataforma, versão de R ou locale.
  M <- 4294967296             # 2^32
  mult32 <- function(a, b) {
    ah <- a %/% 65536; al <- a %% 65536
    ((ah * b) %% 65536 * 65536 + al * b) %% M
  }
  mistura <- function(x) {
    x <- mult32(x, 3432918353)
    x <- (x %/% 131072 + x %% 131072 * 32768) %% M   # >>17 e <<15, aritmético
    mult32(x, 461845907)
  }
  acc <- 2166136261           # semente FNV, só para não começar em zero
  # O id entra byte a byte: é o que separa `A` com seed 1 no passo 2 de `B` com
  # seed 2 no passo 1 — a colisão que `seed + i` produz.
  for (b in utf8ToInt(id)) acc <- mistura((acc + b) %% M)
  acc <- mistura((acc + as.integer(seed) %% M) %% M)
  acc <- mistura((acc + as.integer(i)) %% M)
  # Sete dígitos hex de largura, como `.tr_new_seed()`: cabe em inteiro de R.
  as.integer(acc %% 268435456)
}

#' Roteia o valor devolvido por um membro para as portas dele — a mesma regra de
#' `.tr_store_outputs()`, só que o destino é o passo e não o store.
#'
#' Duas verdades sobre "como um `fn` mapeia retorno em portas" divergiriam: um
#' nó de duas saídas funcionaria fora da região e daria `NULL` numa porta dentro
#' dela, calado. Mantê-las iguais é o que faz a elevação automática ser
#' automática.
#' @noRd
.tr_region_route <- function(value, id, spec) {
  ports <- names(spec$outputs)
  if (length(ports) == 0) return(list())
  if (length(ports) == 1L) return(stats::setNames(list(value), ports))
  if (!is.list(value) || !all(ports %in% names(value))) {
    rlang::abort(sprintf("'%s' (%s) declara as saídas %s mas devolveu %s.", id, spec$id,
                         paste(ports, collapse = ", "),
                         if (is.list(value)) paste(names(value), collapse = ", ") else class(value)[[1]]),
                 class = "tr_error_bad_output")
  }
  value[ports]
}

#' A unidade vista por UM colapso: as saídas dele, nomeadas pelas PORTAS dele.
#'
#' `.tr_store_outputs()` indexa `unit$outputs` por nome de PORTA, e numa região
#' de vários colapsos o nome ali é `"<colapso>:<porta>"`. Sem renomear, o
#' colapso gravaria sob `NULL` — ou, pior, sob a chave do outro colapso.
#' O mapa vem de `region$outputs`, que é onde `tr_plan()` gravou a regra de
#' nomenclatura: re-derivá-la aqui criaria a segunda verdade de sempre.
#' @noRd
.tr_region_sub_unit <- function(unit, cid) {
  mapa <- unit$region$outputs[[cid]]
  ports <- names(mapa)
  # Índice por POSIÇÃO: no colapso sem porta de saída o nome é `""`, e
  # `unit$outputs[""]` em R não devolve o elemento de nome vazio — devolve NA.
  unit$outputs <- stats::setNames(unit$outputs[match(unname(mapa), names(unit$outputs))],
                                  ports)
  com_porta <- ports[nzchar(ports)]
  unit$output_types <- stats::setNames(unit$output_types[unname(mapa)[nzchar(ports)]],
                                       com_porta)
  unit
}

#' Alguma fonte desta porta vem de DENTRO da região? É o que decide "valor do
#' passo" contra "constante lida uma vez", e no colapso "acumula" contra "não".
#' @noRd
.tr_region_interna <- function(srcs) {
  length(srcs) > 0 &&
    any(vapply(srcs, function(s) identical(s$from, "internal"), logical(1)))
}
