#' Executa UMA unidade. É o que roda dentro do worker.
#'
#' Auto-contido de propósito: recebe a unidade (serializável — não carrega
#' `fn` nem ambiente), resolve o nó no registro LOCAL, lê os inputs do store,
#' grava as saídas e devolve os handles. Nunca toca no documento.
#'
#' Nada de valor pesado atravessa fronteira de processo: entra chave, sai
#' chave. É a tese inteira do desenho num lugar só.
#' @noRd
.tr_run_unit <- function(unit, registry, store, ctx_extra = NULL) {
  # Região de fluxo é uma unidade como qualquer outra do ponto de vista do
  # executor — o que muda é só quem sabe executá-la (`R/stream-driver.R`). O
  # despacho é a PRIMEIRA coisa porque `unit$node_type` de uma região é
  # "trama/stream_region", que não é nó de coleção nenhuma: `tr_get_node()`
  # abortaria aqui e o `collect()` gravaria esse erro sob TODA chave de saída da
  # região. A chave não muda depois, e nenhum `tr_plan()` recalcula um handle
  # que existe — o erro sobreviveria pra sempre.
  if (identical(unit$kind, "stream_region")) {
    return(.tr_run_region(unit, registry, store, ctx_extra))
  }

  spec <- tr_get_node(unit$node_type, registry)

  args <- list()
  for (pn in names(spec$inputs)) {
    ref <- unit$inputs[[pn]]
    if (is.null(ref)) next   # porta opcional solta: o `fn` cai no próprio default
    v <- if (isTRUE(spec$inputs[[pn]]$multiple)) {
      lapply(ref, function(r) .tr_load_ref(r, registry, store))
    } else {
      .tr_load_ref(ref, registry, store)
    }
    # `args[[pn]] <- NULL` REMOVE o elemento. Um nó que legitimamente devolve
    # NULL (filtro sem resultado, leitura opcional) faria o consumidor falhar
    # com "argumento ausente, sem padrão" — o "erro longe da causa" que o
    # projeto inteiro se propõe a evitar. `args[pn] <- list(v)` atribui.
    args[pn] <- list(v)
  }
  for (nm in names(unit$params)) args[nm] <- list(unit$params[[nm]])
  if (isTRUE(unit$wants_seed)) args$.seed <- unit$seed
  if (isTRUE(unit$wants_ctx)) {
    ports <- names(unit$output_types)
    unit$partial_type <- if (length(ports)) tr_get_type(unit$output_types[[ports[[1]]]], registry) else NULL
    args$.ctx <- .tr_make_ctx(unit, store, ctx_extra)
  }

  t0 <- Sys.time()
  value <- do.call(spec$fn, args)
  duration <- as.numeric(Sys.time() - t0, units = "secs")

  .tr_store_outputs(unit, value, registry, store, duration)
}

#' Lê um input do store e aplica o adaptador da aresta, se houver.
#'
#' O adaptador roda AQUI, no worker, e não como nó do grafo — é a razão de ele
#' não aparecer como caixa na tela. A impressão digital dele já entrou na
#' chave do consumidor (plan.R), então o resultado adaptado é cacheável.
#' @noRd
.tr_load_ref <- function(ref, registry, store) {
  ty <- tr_get_type(ref$type, registry)
  v <- tr_store_get(store, ref$key, ty)
  if (is.null(ref$adapter)) return(v)
  ad <- tr_adapter_for(ref$adapter$from, ref$adapter$to, registry)
  if (is.null(ad)) {
    rlang::abort(sprintf("Adaptador %s -> %s não está registrado neste worker.",
                         ref$adapter$from, ref$adapter$to),
                 class = "tr_error_unknown_adapter")
  }
  ad$fn(v)
}

#' Roteia o valor devolvido pelo `fn` para as portas de saída.
#' @noRd
.tr_store_outputs <- function(unit, value, registry, store, duration = NA_real_) {
  ports <- names(unit$output_types)
  if (length(ports) == 0) {
    # Nó terminal sem saída: grava um marcador, pra que a unidade tenha
    # presença no store e o plano possa dizer "já rodou".
    ty <- tr_type("trama/marker")
    return(list(tr_store_put(store, unit$outputs[[1]], TRUE, ty,
                             node_type = unit$node_type, collections = unit$collections,
                             duration = duration)))
  }
  if (length(ports) == 1L) {
    ty <- tr_get_type(unit$output_types[[ports]], registry)
    return(stats::setNames(list(
      tr_store_put(store, unit$outputs[[ports]], value, ty,
                   node_type = unit$node_type, collections = unit$collections,
                   duration = duration)), ports))
  }
  # Múltiplas saídas: o `fn` devolve uma lista nomeada pelas portas. Erro alto
  # e cedo se faltar alguma — senão a porta ficaria sem artefato e o consumidor
  # falharia com "chave ausente", longe da causa.
  if (!is.list(value) || !all(ports %in% names(value))) {
    rlang::abort(
      sprintf("'%s' declara as saídas %s mas devolveu %s.", unit$node_type,
              paste(ports, collapse = ", "),
              if (is.list(value)) paste(names(value), collapse = ", ") else class(value)[1]),
      class = "tr_error_bad_output"
    )
  }
  stats::setNames(lapply(ports, function(pn) {
    ty <- tr_get_type(unit$output_types[[pn]], registry)
    tr_store_put(store, unit$outputs[[pn]], value[[pn]], ty,
                 node_type = unit$node_type, collections = unit$collections,
                 duration = duration)
  }), ports)
}

#' `.ctx` — progresso e resultado parcial de dentro de um nó longo.
#'
#' Publica pelo STORE, não por socket. O desenho independente propunha um
#' canal `nanonext` push/pull; usar o store faz o mesmo trabalho sem
#' maquinaria nova, funciona igual nos dois executores, e mantém a tese ("o
#' store é o protocolo") em vez de abrir um segundo caminho de comunicação.
#' Custo: o coordenador descobre por polling, não por push — latência de
#' fração de segundo, irrelevante pra barra de progresso.
#' @noRd
.tr_make_ctx <- function(unit, store, extra = NULL) {
  dir <- file.path(store$root, "progress")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, paste0(unit$key, ".json"))
  c(list(
    node = unit$node, key = unit$key, seed = unit$seed,
    root = store$project_root,
    path = function(p) tr_store_path(store, p),
    progress = function(fraction, msg = NULL) {
      cur <- tr_progress(store, unit$key) %||% list()
      cur$fraction <- fraction; cur$message <- msg; cur$at <- as.numeric(Sys.time())
      jsonlite::write_json(cur, path, auto_unbox = TRUE, null = "null", digits = NA)
      invisible(TRUE)
    },
    partial = function(value) {
      # Parcial NÃO vai para a chave real: lá, `tr_store_has()` diria "pronto"
      # e o plano seguinte pularia a unidade. Vai como campo do progresso,
      # já em forma de preview (o worker tem o tipo; o coordenador não tem o
      # valor), e o handle final substitui quando o nó termina.
      ty <- unit$partial_type
      art <- if (is.null(ty) || is.null(ty$preview)) NULL else
        tryCatch(ty$preview(value, list(file = function(e) file.path(store$root, "tmp", paste0(unit$key, "-partial.", e)))),
                 error = function(e) list(renderer = "trama/error", data = list(message = conditionMessage(e))))
      cur <- tr_progress(store, unit$key) %||% list(fraction = NULL, message = NULL)
      cur$partial <- list(key = unit$key, preview = art, partial = TRUE)
      cur$at <- as.numeric(Sys.time())
      jsonlite::write_json(cur, path, auto_unbox = TRUE, null = "null", digits = NA)
      invisible(TRUE)
    },
    file = function(ext) file.path(store$root, "tmp", paste0(.tr_entropy_hex(12L), ".", ext))
  ), extra)
}

#' Lê o progresso publicado por `.ctx$progress()`/`.ctx$partial()` pra uma chave, ou `NULL` se a unidade não publicou nada (ou já terminou — o scheduler limpa ao concluir).
#' @export
tr_progress <- function(store, key) {
  p <- file.path(store$root, "progress", paste0(key, ".json"))
  if (!file.exists(p)) return(NULL)
  tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) NULL)
}
