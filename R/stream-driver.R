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
#' @noRd
.tr_run_region <- function(unit, registry, store, ctx_extra = NULL) {
  rg <- unit$region
  specs <- lapply(stats::setNames(rg$order, rg$order),
                  function(id) tr_get_node(rg$nodes[[id]]$node_type, registry))

  # `.ctx` da região: a unidade pede (`wants_ctx = TRUE` em `plan.R`), e nada
  # honra o pedido — o despacho de `.tr_run_unit()` vem pra cá ANTES do bloco
  # que constrói o ctx, e este driver não constrói nenhum. Nenhum membro o
  # receberia de todo jeito: nó elevado tem `.ctx` PROIBIDO (Fase 2) e
  # `init`/`step` não o tomam. Quem vai usá-lo é o driver, na Fase 5, pra
  # publicar o parcial de cada nó e ler comandos pelo store; até lá, a região
  # não publica progresso e `tr_progress()` na chave dela devolve NULL. O
  # `ctx_extra` entra na assinatura mesmo sem uso porque descartá-lo no despacho
  # de `.tr_run_unit()` seria uma perda silenciosa quando a Fase 5 o usar.

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
  saida <- do.call(fs$fn, .tr_region_args(fonte, rg, specs, registry, ext, list()))
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

  # 3. `init` roda UMA vez, antes do laço, e recebe só os params que declara —
  # `node.R` valida `init` como função de params, e só de params.
  estado <- list()
  for (id in rg$order) {
    if (!isTRUE(rg$nodes[[id]]$online)) next
    ini <- specs[[id]]$init
    # `estado[id] <- list(v)`, e não `estado[[id]] <- v`: um `init` que devolve
    # NULL (estado que só nasce no primeiro passo) REMOVERIA a entrada, e o
    # `step` receberia o estado de outro nó — ou nenhum.
    estado[id] <- list(do.call(ini, rg$nodes[[id]]$params[names(formals(ini))]))
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

  for (i in seq_len(n)) {
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
      args <- .tr_region_args(id, rg, specs, registry, ext, cur)

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
  }

  # 5. Cada colapso roda UMA vez, com o fluxo inteiro.
  valores <- lapply(stats::setNames(rg$collapse, rg$collapse), function(cid) {
    args <- .tr_region_args(cid, rg, specs, registry, ext, list(), acc = acc[[cid]])
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
  handles
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
#' @noRd
.tr_region_args <- function(id, rg, specs, registry, ext, cur, acc = NULL) {
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
  args
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
