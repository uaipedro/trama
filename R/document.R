#' Documento: o grafo que o usuário montou.
#'
#' Semântica de valor — toda op devolve um documento novo, nunca muta. É o que
#' torna undo/redo um log de ops e o que permite comparar duas revisões.
#'
#' **Duas camadas no mesmo arquivo, deliberadamente:**
#'   - semântica (`nodes`, `edges`, `seed`) entra na chave de cache;
#'   - apresentação (`ui`) nunca entra.
#' Se posição e parâmetro morassem no mesmo lugar, todo caminho de
#' invalidação precisaria saber ignorar posição — o insumo pagava esse preço
#' com uma função dedicada a comparar "só o que afeta o cálculo".
#' @export
tr_doc <- function() {
  structure(list(
    format = 1L, rev = 0L,
    collections = list(),
    nodes = list(), edges = list(),
    ui = list(positions = list(), sizes = list(), views = list(),
              frames = list(), modes = list(), soltos = list(),
              notes = list(), ocultos = list())
  ), class = "tr_doc")
}

#' Ops que mexem SÓ em apresentação. Todo o resto é semântico.
#'
#' A lista é essa direção de propósito. Enumerar as ops semânticas (e tratar o
#' resto como cosmético) faria o custo de esquecer uma op nova ser exatamente
#' o bug que `R/session.R` diz ter erradicado: edição aceita, revisão avança,
#' preview não atualiza, nenhum erro. Enumerando as cosméticas, o custo de
#' esquecer vira "recomputou à toa" — barulhento e inofensivo.
.tr_presentation_ops <- c("rename", "move", "resize", "set_view",
                          "add_frame", "update_frame", "remove_frame",
                          "reorder_frames", "set_mode", "set_solto",
                          "set_preview_oculto",
                          "add_note", "update_note", "remove_note")

#' Uma op é SEMÂNTICA se não for puramente de apresentação — só ops semânticas disparam re-execução. Um `batch` é semântico se qualquer op dentro dele for.
#' @export
tr_op_semantic <- function(op) {
  if (identical(op$op, "batch")) return(any(vapply(op$ops, tr_op_semantic, logical(1))))
  !(op$op %in% .tr_presentation_ops)
}

#' Ops cuja resposta leva o documento inteiro de volta ao front.
#'
#' O critério é o do comentário em `R/transport.R`: o front não reimplementa a
#' semântica de uma op só pra saber desenhar o resultado. Estrutura de grafo
#' entra, e também criar, apagar e reordenar frame (o front não conhece o id
#' que o servidor materializou, nem o número de ordem). Arrastar, redimensionar
#' e recolher ficam de fora: o gesto já atualizou a tela.
#' @noRd
.tr_doc_echo_ops <- c("add_node", "remove_node", "connect", "disconnect",
                      "add_frame", "remove_frame", "reorder_frames",
                      "add_note", "remove_note")

.tr_op_echoes_doc <- function(op) {
  if (identical(op$op, "batch")) return(any(vapply(op$ops, .tr_op_echoes_doc, logical(1))))
  isTRUE(op$op %in% .tr_doc_echo_ops)
}

.tr_edge_key <- function(e) {
  paste0(e$from$node, ":", e$from$port, "->", e$to$node, ":", e$to$port,
         "#", as.integer(e$index %||% 1L))
}

.tr_node_or_abort <- function(doc, id) {
  if (!is.character(id) || length(id) != 1L) {
    rlang::abort("Op sem referência de nó válida.", class = "tr_error_bad_op")
  }
  n <- doc$nodes[[id]]
  if (is.null(n)) rlang::abort(sprintf("Nó '%s' não existe.", id), class = "tr_error_unknown_node")
  n
}

#' Ops chegam de JSON de um cliente: forma nunca é presumida.
#'
#' Sem isso, uma op malformada produz `simpleError` do interpretador
#' ("attempt to select less than one element") — mensagem inútil pro usuário e
#' classe inútil pro front, que usa `class` pra decidir o que mostrar.
#' @noRd
.tr_require <- function(op, fields) {
  if (!is.list(op) || is.null(op$op) || !is.character(op$op) || length(op$op) != 1L) {
    rlang::abort("Op malformada: campo 'op' ausente ou inválido.", class = "tr_error_bad_op")
  }
  missing <- fields[vapply(fields, function(f) is.null(op[[f]]), logical(1))]
  if (length(missing) > 0) {
    rlang::abort(sprintf("Op '%s' sem campo(s) obrigatório(s): %s.",
                         op$op, paste(missing, collapse = ", ")),
                 class = "tr_error_bad_op")
  }
  invisible(TRUE)
}

.tr_scalar_num <- function(x, what) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x)) {
    rlang::abort(sprintf("%s deve ser um número.", what), class = "tr_error_bad_op")
  }
  as.numeric(x)
}

#' Aplica uma op e devolve `list(doc, op)` — o documento novo E a op
#' NORMALIZADA (com `id` e `seed` materializados, `index` resolvido).
#'
#' Devolver a op normalizada não é detalhe: é o que faz o log ser replayável.
#' Ecoar a op recebida (que pode não ter id nem seed, porque o cliente deixa o
#' servidor materializar) faria o undo por replay reconstruir o documento com
#' ids e seeds DIFERENTES — a identidade estável evaporaria no primeiro undo,
#' e todo cache a jusante junto.
#' @noRd
.tr_apply <- function(doc, op, registry = .tr_default_registry) {
  res <- .tr_op_fn(op$op)(doc, op, registry)
  res$doc$rev <- res$doc$rev + 1L
  res
}

#' Nome de op -> função. Separado de `.tr_apply` porque o `batch` despacha as
#' ops de dentro SEM avançar a revisão a cada uma: o lote inteiro é uma
#' revisão e um passo de undo.
#'
#' O nome é checado ANTES do `switch()`: com um número, o `switch()` escolhe
#' por posição (`op = 5` virava `rename`, `op = 100` um `simpleError`), e com
#' uma lista nem chega a despachar. Ausente também cai aqui — é op malformada,
#' não op desconhecida; `tr_error_unknown_op` fica pra texto que não é op.
#' @noRd
.tr_op_fn <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    rlang::abort("Op malformada: campo 'op' ausente ou inválido.", class = "tr_error_bad_op")
  }
  switch(name,
    add_node = .tr_op_add_node, remove_node = .tr_op_remove_node,
    set_param = .tr_op_set_param, set_seed = .tr_op_set_seed,
    rename = .tr_op_rename, move = .tr_op_move, resize = .tr_op_resize,
    set_view = .tr_op_set_view,
    add_frame = .tr_op_add_frame, update_frame = .tr_op_update_frame,
    remove_frame = .tr_op_remove_frame, reorder_frames = .tr_op_reorder_frames,
    set_mode = .tr_op_set_mode, set_solto = .tr_op_set_solto,
    set_preview_oculto = .tr_op_set_preview_oculto,
    add_note = .tr_op_add_note, update_note = .tr_op_update_note,
    remove_note = .tr_op_remove_note,
    connect = .tr_op_connect, disconnect = .tr_op_disconnect,
    batch = .tr_op_batch,
    rlang::abort(sprintf("Op desconhecida: '%s'.", name),
                 class = "tr_error_unknown_op")
  )
}

#' Várias ops como UMA: atômica, uma revisão, um passo de undo.
#'
#' Atomicidade vem de graça da semântica de valor: as ops de dentro trabalham
#' numa cópia local, e se uma aborta a cópia é descartada — o documento de
#' quem chamou nunca foi tocado. A mensagem ganha a posição da op que falhou,
#' mas a classe continua a dela, que é o que o front usa pra decidir o que
#' mostrar.
#'
#' Batch dentro de batch é recusado: não compra nada que um lote plano não
#' compre, e abriria recursão sem fundo pra um cliente com bug.
#' @noRd
.tr_op_batch <- function(doc, op, registry) {
  .tr_require(op, "ops")
  ops <- op$ops
  # Três defeitos distintos, três mensagens: juntá-los fazia `ops = "move"`
  # dizer "Batch vazio." e `ops = list(1, 2)` falar de batch aninhado.
  if (!is.list(ops) || length(ops) == 0L) {
    rlang::abort("'ops' deve ser uma lista não vazia.", class = "tr_error_bad_op")
  }
  n <- length(ops)
  for (i in seq_len(n)) {
    inner <- ops[[i]]
    if (!is.list(inner)) {
      rlang::abort(sprintf("op %d de %d não é um objeto.", i, n), class = "tr_error_bad_op")
    }
    if (identical(inner$op, "batch")) {
      rlang::abort(sprintf("op %d de %d: batch não pode conter batch.", i, n),
                   class = "tr_error_bad_op")
    }
    # O nome só entra na mensagem se for um texto: o handler que reembala o
    # erro não pode ele mesmo quebrar no `sprintf()` com uma `op` malformada.
    nome <- if (is.character(inner$op) && length(inner$op) == 1L && !is.na(inner$op)) inner$op else "?"
    res <- tryCatch(.tr_op_fn(inner$op)(doc, inner, registry), error = function(e) {
      rlang::abort(sprintf("op %d de %d (%s): %s", i, n, nome, conditionMessage(e)),
                   class = class(e)[[1]])
    })
    doc <- res$doc
    ops[[i]] <- res$op
  }
  op$ops <- ops
  list(doc = doc, op = op)
}

#' Conveniência (console, testes, replay). A superfície do transporte é
#' `tr_submit()` — ela é que carrega a proteção de revisão. Chamar isto
#' direto a partir do servidor reintroduziria a corrida do insumo.
#' @export
tr_doc_apply <- function(doc, op, registry = .tr_default_registry) {
  .tr_apply(doc, op, registry)$doc
}

.tr_op_add_node <- function(doc, op, registry) {
  .tr_require(op, "type")
  spec <- tr_get_node(op$type, registry)
  id <- op$id %||% .tr_new_id()
  .tr_check_node_id(id)
  if (!is.null(doc$nodes[[id]])) {
    rlang::abort(sprintf("Id de nó já existe: '%s'.", id), class = "tr_error_duplicate_id")
  }
  # Template (ou op) gravado numa versão velha do nó traz `type_version`: os
  # params sobem pela cadeia de migração ANTES da checagem de desconhecidos,
  # senão o renome de um param recusaria o template inteiro.
  if (!is.null(op$type_version)) {
    migrados <- .tr_migrate_params(spec, op$params, op$type_version, node = id)
    if (!is.null(migrados)) op$params <- migrados
    op$type_version <- NULL
  }
  unknown <- setdiff(names(op$params %||% list()), names(spec$params))
  if (length(unknown) > 0) {
    rlang::abort(sprintf("Param desconhecido em '%s': %s.", op$type, paste(unknown, collapse = ", ")),
                 class = "tr_error_unknown_param")
  }
  params <- op$params %||% list()
  for (nm in names(params)) params[[nm]] <- .tr_check_param_value(spec$params[[nm]], params[[nm]], nm)

  seed <- if (is.null(op$seed)) .tr_new_seed() else .tr_check_seed(op$seed)
  doc$nodes[[id]] <- list(
    type = op$type, type_version = spec$version,
    label = op$label %||% spec$label, params = params, seed = seed
  )
  if (!is.null(op$position)) {
    doc$ui$positions[[id]] <- c(.tr_scalar_num(op$position[[1]], "position.x"),
                                .tr_scalar_num(op$position[[2]], "position.y"))
  }
  # Adicionar é O(1): só o nó novo pode introduzir coleção. Recalcular o
  # manifesto inteiro aqui tornaria `add_node` O(n) e a montagem de um grafo
  # grande O(n²) — medido, é o que domina o custo de colar muitos nós.
  cid <- .tr_collection_of(op$type)
  if (is.null(doc$collections[[cid]])) {
    doc$collections[[cid]] <- registry$collections[[cid]]$version %||% "unknown"
  }
  op$id <- id; op$seed <- seed; op$params <- params
  list(doc = doc, op = op)
}

.tr_op_remove_node <- function(doc, op, registry) {
  .tr_require(op, "node"); .tr_node_or_abort(doc, op$node)
  doc$nodes[[op$node]] <- NULL
  doc$ui$positions[[op$node]] <- NULL
  doc$ui$sizes[[op$node]] <- NULL
  doc$ui$views[[op$node]] <- NULL
  doc$ui$modes[[op$node]] <- NULL
  doc$ui$soltos[[op$node]] <- NULL
  doc$ui$ocultos[[op$node]] <- NULL
  doc$edges <- Filter(function(e) e$from$node != op$node && e$to$node != op$node, doc$edges)
  # Sem isto o documento continuaria declarando dependência de uma coleção
  # cujo último nó acabou de sair — e exigiria instalá-la pra abrir.
  doc <- .tr_sync_collections(doc, registry)
  list(doc = doc, op = op)
}

#' `collections` reflete o que o documento REALMENTE usa. É o manifesto que
#' permite dizer "abri e falta a coleção X, versão Y" em vez de "tipo de nó
#' desconhecido". Recalculado só na remoção — que é a única op capaz de fazer
#' uma coleção deixar de ser usada.
#' @noRd
.tr_sync_collections <- function(doc, registry) {
  used <- unique(vapply(doc$nodes, function(n) .tr_collection_of(n$type), ""))
  keep <- list()
  for (cid in used) {
    keep[[cid]] <- doc$collections[[cid]] %||% registry$collections[[cid]]$version %||% "unknown"
  }
  doc$collections <- keep
  doc
}

.tr_op_set_param <- function(doc, op, registry) {
  .tr_require(op, c("node", "name"))
  n <- .tr_node_or_abort(doc, op$node)
  spec <- tr_get_node(n$type, registry)
  pspec <- spec$params[[op$name]]
  if (is.null(pspec)) {
    rlang::abort(sprintf("Param '%s' não existe em '%s'.", op$name, n$type),
                 class = "tr_error_unknown_param")
  }
  if (!is.null(op$origem) && !identical(op$origem, "sugestao")) {
    rlang::abort(sprintf("'origem' desconhecida em set_param: %s.", paste(format(op$origem), collapse = ", ")),
                 class = "tr_error_bad_op")
  }
  doc$nodes[[op$node]]$params[[op$name]] <- .tr_check_param_value(pspec, op$value, op$name)
  # `sugeridos` marca os params cujo valor veio da sugestão automática, não
  # de uma escolha. É o que deixa o front refazer a sugestão quando a entrada
  # muda sem nunca pisar no que alguém escolheu: qualquer set_param sem
  # `origem` é escolha e tira a marca. Mora na op (e não num campo à parte)
  # porque o undo é replay do log — a marca volta junto com o valor.
  sug <- setdiff(as.character(doc$nodes[[op$node]]$sugeridos), op$name)
  if (identical(op$origem, "sugestao")) sug <- c(sug, op$name)
  doc$nodes[[op$node]]$sugeridos <- if (length(sug)) sug else NULL
  list(doc = doc, op = op)
}

#' `sugeridos` só pode citar params que o nó TEM: depois de migração que
#' renomeia/remove param, ou de edição à mão, um nome velho ficaria marcado
#' para sempre. Ausente (documento antigo) e vazio são a mesma coisa: campo
#' nenhum.
#' @noRd
.tr_sugeridos_limpos <- function(n) {
  sug <- intersect(as.character(unlist(n$sugeridos)), names(n$params))
  n$sugeridos <- if (length(sug)) sug else NULL
  n
}

.tr_op_set_seed <- function(doc, op, registry) {
  .tr_require(op, c("node", "value")); .tr_node_or_abort(doc, op$node)
  doc$nodes[[op$node]]$seed <- .tr_check_seed(op$value)
  list(doc = doc, op = op)
}

#' `as.integer("abc")` devolve `NA` com um mero *warning* — que `tr_submit`
#' não captura, porque só trata `error`. Um seed `NA` viajaria pro documento e
#' só explodiria no executor, longe da causa.
#' @noRd
.tr_check_seed <- function(x) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x)) {
    rlang::abort("Seed deve ser um inteiro.", class = "tr_error_bad_op")
  }
  as.integer(x)
}

.tr_op_rename <- function(doc, op, registry) {
  .tr_require(op, c("node", "label")); .tr_node_or_abort(doc, op$node)
  doc$nodes[[op$node]]$label <- as.character(op$label)[[1]]
  list(doc = doc, op = op)
}

.tr_op_move <- function(doc, op, registry) {
  .tr_require(op, c("node", "x", "y")); .tr_node_or_abort(doc, op$node)
  # `c(NULL, NULL)` produz vetor vazio, e atribuir vazio a `positions[[node]]`
  # APAGA a chave — mover sem coordenadas removeria a posição em silêncio.
  doc$ui$positions[[op$node]] <- c(.tr_scalar_num(op$x, "x"), .tr_scalar_num(op$y, "y"))
  list(doc = doc, op = op)
}

# Não há clamp de mínimo aqui de propósito: o piso de 240x132 é política de
# apresentação e mora no CSS (`min-width`/`min-height`), onde já é aplicado a
# documento escrito à mão. Duplicar o número em R faria dois lugares para
# manter e nenhum ganho.
.tr_op_resize <- function(doc, op, registry) {
  .tr_require(op, c("node", "w", "h")); .tr_node_or_abort(doc, op$node)
  doc$ui$sizes[[op$node]] <- c(.tr_scalar_num(op$w, "w"), .tr_scalar_num(op$h, "h"))
  list(doc = doc, op = op)
}

# Guarda o ID da vista, nunca o índice: uma coleção que reordena suas vistas
# mudaria em silêncio a vista escolhida de todo documento salvo. Se o id não
# existir mais, o front-end cai na primeira vista — a política é dele.
.tr_op_set_view <- function(doc, op, registry) {
  .tr_require(op, c("node", "view")); .tr_node_or_abort(doc, op$node)
  doc$ui$views[[op$node]] <- as.character(op$view)[[1]]
  list(doc = doc, op = op)
}

# --- Frames -------------------------------------------------------------------
# Frame é APRESENTAÇÃO pura: um retângulo com título, proporção e ordem de
# slide, em `ui.frames`, fora da chave de cache. O pertencimento de um card a
# um frame é geométrico e calculado pelo front no começo do arrasto — nunca
# gravado. Por isso nenhuma op de nó precisa saber que frames existem.

#' Proporções que o front sabe travar. Fechado de propósito: uma proporção
#' nova é uma linha aqui e uma no `ASPECTS` de `inst/www/geometria.js`.
#' @noRd
.tr_aspects <- c("livre", "16:9", "4:3", "1:1", "A4")
.tr_frame_fields <- c("x", "y", "w", "h", "title", "aspect", "color")

.tr_frame_or_abort <- function(doc, id) {
  if (!is.character(id) || length(id) != 1L) {
    rlang::abort("Op sem referência de frame válida.", class = "tr_error_bad_op")
  }
  f <- doc$ui$frames[[id]]
  if (is.null(f)) rlang::abort(sprintf("Frame '%s' não existe.", id), class = "tr_error_unknown_frame")
  f
}

.tr_pos_num <- function(x, what) {
  v <- .tr_scalar_num(x, what)
  if (v <= 0) rlang::abort(sprintf("%s deve ser positivo.", what), class = "tr_error_bad_op")
  v
}

.tr_scalar_chr <- function(x, what) {
  if (length(x) != 1L || !is.character(x) || is.na(x)) {
    rlang::abort(sprintf("%s deve ser um texto.", what), class = "tr_error_bad_op")
  }
  x
}

.tr_check_aspect <- function(x) {
  x <- .tr_scalar_chr(x, "aspect")
  if (!x %in% .tr_aspects) {
    rlang::abort(sprintf("Proporção desconhecida: '%s'. Use uma de: %s.",
                         x, paste(.tr_aspects, collapse = ", ")),
                 class = "tr_error_bad_op")
  }
  x
}

.tr_frame_field <- function(k, v) {
  switch(k,
    x = , y = .tr_scalar_num(v, k),
    w = , h = .tr_pos_num(v, k),
    title = , color = .tr_scalar_chr(v, k),
    aspect = .tr_check_aspect(v))
}

# `id`, `order`, `title`, `aspect` e `color` são materializados E ecoados, como
# o `id` e o `seed` de `add_node`: o undo por replay tem que reconstruir o
# mesmo frame, na mesma posição da sequência.
.tr_op_add_frame <- function(doc, op, registry) {
  .tr_require(op, c("x", "y", "w", "h"))
  id <- op$id %||% .tr_new_id()
  .tr_check_node_id(id)
  if (!is.null(doc$ui$frames[[id]])) {
    rlang::abort(sprintf("Id de frame já existe: '%s'.", id), class = "tr_error_duplicate_id")
  }
  orders <- vapply(doc$ui$frames, function(f) as.numeric(f$order %||% 0), numeric(1))
  order <- if (is.null(op$order)) {
    if (length(orders)) max(orders) + 1 else 1
  } else {
    # O schema diz inteiro >= 1. Fora disso, `as.integer()` aceitaria ordem
    # negativa calado, e uma grande demais viraria NA com um warning.
    o <- .tr_scalar_num(op$order, "order")
    if (o < 1 || o > .Machine$integer.max) {
      rlang::abort("order deve ser um inteiro >= 1.", class = "tr_error_bad_op")
    }
    o
  }
  order <- as.integer(order)
  f <- list(
    x = .tr_frame_field("x", op$x), y = .tr_frame_field("y", op$y),
    w = .tr_frame_field("w", op$w), h = .tr_frame_field("h", op$h),
    title = if (is.null(op$title)) sprintf("Frame %d", order) else .tr_frame_field("title", op$title),
    aspect = if (is.null(op$aspect)) "16:9" else .tr_frame_field("aspect", op$aspect),
    color = if (is.null(op$color)) "azul" else .tr_frame_field("color", op$color),
    order = order
  )
  doc$ui$frames[[id]] <- f
  op$id <- id; op$order <- order
  op$title <- f$title; op$aspect <- f$aspect; op$color <- f$color
  list(doc = doc, op = op)
}

# Patch parcial: só os campos que vieram mudam. Arrastar manda `x, y`;
# redimensionar manda `x, y, w, h`; o menu manda `aspect, h` ou `color`.
.tr_op_update_frame <- function(doc, op, registry) {
  .tr_require(op, "frame")
  f <- .tr_frame_or_abort(doc, op$frame)
  given <- intersect(.tr_frame_fields, names(op))
  if (length(given) == 0L) {
    rlang::abort("update_frame sem nenhum campo para mudar.", class = "tr_error_bad_op")
  }
  for (k in given) f[[k]] <- .tr_frame_field(k, op[[k]])
  doc$ui$frames[[op$frame]] <- f
  list(doc = doc, op = op)
}

.tr_op_remove_frame <- function(doc, op, registry) {
  .tr_require(op, "frame"); .tr_frame_or_abort(doc, op$frame)
  doc$ui$frames[[op$frame]] <- NULL
  list(doc = doc, op = op)
}

# Lista COMPLETA, e não "mova o X pra posição k": a op fica idempotente e o
# replay não depende da ordem em que os frames foram criados.
.tr_op_reorder_frames <- function(doc, op, registry) {
  .tr_require(op, "frames")
  ids <- as.character(unlist(op$frames))
  have <- names(doc$ui$frames) %||% character()
  if (length(ids) != length(have) || anyDuplicated(ids) > 0L || !setequal(ids, have)) {
    rlang::abort("reorder_frames precisa listar cada frame exatamente uma vez.",
                 class = "tr_error_bad_op")
  }
  for (i in seq_along(ids)) doc$ui$frames[[ids[[i]]]]$order <- i
  list(doc = doc, op = op)
}

# --- Notas -----------------------------------------------------------------
# Blocos de apresentação, irmãos do frame: retângulo, sem porta e sem execução.
# Moram em `ui.notes`, fora da chave de cache, e as ops são cosméticas — mudar
# o texto de uma nota não pode invalidar o cache de gráfico nenhum. Não há
# `reorder_notes`: frame tem ordem porque é slide, e nota não é slide, ela
# ENTRA num (pelo mesmo pertencimento geométrico dos cards).

.tr_note_kinds   <- c("markdown", "imagem")
# Tamanho de nascença de cada kind quando a op não traz `w`/`h`: um parágrafo
# curto legível e uma imagem em 16:11. Espelhado em `NOTA_TAMANHO`
# (inst/www/geometria.js), e `tests/js/geometria.test.mjs` confere os dois.
.tr_note_tamanho <- list(markdown = c(280, 160), imagem = c(320, 220))
.tr_note_escalas <- c("letreiro", "nota")
.tr_note_fundos  <- c("nenhum", "cartao")
.tr_note_fits    <- c("contain", "cover")
.tr_note_fields  <- c("x", "y", "w", "h", "kind", "text", "escala", "fundo",
                      "color", "src", "fit")

.tr_note_or_abort <- function(doc, id) {
  if (!is.character(id) || length(id) != 1L) {
    rlang::abort("Op sem referência de nota válida.", class = "tr_error_bad_op")
  }
  n <- doc$ui$notes[[id]]
  if (is.null(n)) {
    rlang::abort(sprintf("Nota '%s' não existe.", id), class = "tr_error_unknown_note")
  }
  n
}

.tr_check_enum <- function(x, what, vals) {
  x <- .tr_scalar_chr(x, what)
  if (!x %in% vals) {
    rlang::abort(sprintf("%s desconhecido: '%s'. Use um de: %s.",
                         what, x, paste(vals, collapse = ", ")),
                 class = "tr_error_bad_op")
  }
  x
}

# `src` é relativo à pasta `imagens/` do projeto, que é a ÚNICA servida ao
# navegador (`tr_ui()`). As três recusas são as mesmas de `.tr_check_asset()`
# em R/collection.R, pelos mesmos motivos: `..` sai da pasta pensada pra isso;
# caminho absoluto funcionaria numa máquina e em nenhuma outra; e a barra
# invertida é separador só no Windows, então o mesmo documento acharia
# arquivos diferentes conforme quem o abre.
.tr_check_src <- function(x) {
  x <- .tr_scalar_chr(x, "src")
  if (grepl("^(/|~|[A-Za-z]:)", x)) {
    rlang::abort(sprintf("src tem que ser relativo à pasta imagens/ do projeto (\"%s\" é absoluto).", x),
                 class = "tr_error_bad_op")
  }
  if (any(strsplit(x, "/", fixed = TRUE)[[1]] == "..")) {
    rlang::abort(sprintf("src não pode ter '..' (\"%s\").", x), class = "tr_error_bad_op")
  }
  if (grepl("\\", x, fixed = TRUE)) {
    rlang::abort(sprintf("src usa barra invertida (\"%s\"); separe as pastas com '/'.", x),
                 class = "tr_error_bad_op")
  }
  x
}

.tr_note_field <- function(k, v) {
  switch(k,
    x = , y = .tr_scalar_num(v, k),
    w = , h = .tr_pos_num(v, k),
    text = , color = .tr_scalar_chr(v, k),
    kind   = .tr_check_enum(v, "kind", .tr_note_kinds),
    escala = .tr_check_enum(v, "escala", .tr_note_escalas),
    fundo  = .tr_check_enum(v, "fundo", .tr_note_fundos),
    fit    = .tr_check_enum(v, "fit", .tr_note_fits),
    src    = .tr_check_src(v))
}

# `id`, `kind`, `escala`, `fundo`, `color` e `fit` são materializados E ecoados,
# como em `add_frame`: o undo por replay tem que reconstruir a mesma nota.
# `color = "nenhuma"` é o padrão porque bloco de texto quer ser texto, e não
# um retângulo colorido: a cor é acento que se escolhe, não ponto de partida.
.tr_op_add_note <- function(doc, op, registry) {
  .tr_require(op, c("x", "y", "kind"))
  id <- op$id %||% .tr_new_id()
  .tr_check_node_id(id)
  if (!is.null(doc$ui$notes[[id]])) {
    rlang::abort(sprintf("Id de nota já existe: '%s'.", id), class = "tr_error_duplicate_id")
  }
  kind <- .tr_note_field("kind", op$kind)
  op$w <- op$w %||% .tr_note_tamanho[[kind]][[1]]
  op$h <- op$h %||% .tr_note_tamanho[[kind]][[2]]
  n <- list(
    x = .tr_note_field("x", op$x), y = .tr_note_field("y", op$y),
    w = .tr_note_field("w", op$w), h = .tr_note_field("h", op$h),
    kind = kind,
    escala = if (is.null(op$escala)) "nota" else .tr_note_field("escala", op$escala),
    fundo = if (is.null(op$fundo)) "cartao" else .tr_note_field("fundo", op$fundo),
    color = if (is.null(op$color)) "nenhuma" else .tr_note_field("color", op$color)
  )
  # Campo de um kind não existe no outro: nota de markdown com `fit` (ou
  # imagem com `text`) guardaria valor que ninguém lê e que a próxima pessoa
  # a ler o JSON tentaria entender.
  if (kind == "markdown") {
    n$text <- if (is.null(op$text)) "" else .tr_note_field("text", op$text)
  } else {
    n$src <- if (is.null(op$src)) "" else .tr_note_field("src", op$src)
    n$fit <- if (is.null(op$fit)) "contain" else .tr_note_field("fit", op$fit)
  }
  doc$ui$notes[[id]] <- n
  op$id <- id; op$kind <- kind
  op$escala <- n$escala; op$fundo <- n$fundo; op$color <- n$color
  if (kind == "markdown") op$text <- n$text else { op$src <- n$src; op$fit <- n$fit }
  list(doc = doc, op = op)
}

# Patch parcial, como `update_frame`: arrastar manda `x, y`; redimensionar
# manda os quatro; editar manda `text`; o menu manda `color`, `fundo`,
# `escala` ou `fit`. `kind` NÃO entra: trocar o tipo de um bloco existente
# trocaria também quais campos ele tem, e o caminho honesto é apagar e criar.
.tr_op_update_note <- function(doc, op, registry) {
  .tr_require(op, "note")
  n <- .tr_note_or_abort(doc, op$note)
  given <- setdiff(intersect(.tr_note_fields, names(op)), "kind")
  if (length(given) == 0L) {
    rlang::abort("update_note sem nenhum campo para mudar.", class = "tr_error_bad_op")
  }
  for (k in given) n[[k]] <- .tr_note_field(k, op[[k]])
  doc$ui$notes[[op$note]] <- n
  list(doc = doc, op = op)
}

.tr_op_remove_note <- function(doc, op, registry) {
  .tr_require(op, "note"); .tr_note_or_abort(doc, op$note)
  doc$ui$notes[[op$note]] <- NULL
  list(doc = doc, op = op)
}

# Modos de exibição do card. `completo` é o padrão e a ausência já diz isso:
# só entra no documento o card que desvia dele. O editor oferece três: `mini`,
# `completo` e `solto` (card em mini, preview destacado flutuando no canvas
# como uma caixa de imagem, geometria em `ui$soltos`). `params` e `preview`
# ficam aceitos só por documento antigo: o front lê os dois como `completo`,
# e `preview` significa ainda "params começam recolhidos".
.tr_modes <- c("mini", "params", "preview", "completo", "solto")

.tr_op_set_mode <- function(doc, op, registry) {
  .tr_require(op, c("node", "modo")); .tr_node_or_abort(doc, op$node)
  m <- op$modo
  if (!is.character(m) || length(m) != 1L || !m %in% .tr_modes) {
    rlang::abort(sprintf("modo deve ser um de: %s.", paste(.tr_modes, collapse = ", ")),
                 class = "tr_error_bad_op")
  }
  doc$ui$modes[[op$node]] <- if (identical(m, "completo")) NULL else m
  list(doc = doc, op = op)
}

# Geometria do preview destacado de um card em modo `solto`: c(x, y, w, h).
# Mora num mapa próprio, e não em `sizes`, porque o card continua existindo
# (em mini) e tem talhe seu. Não exige que o modo seja `solto`: o front guarda
# a caixa pra quando o card voltar a soltar, e a ordem das ops num batch fica
# livre. É apresentação pura, como `resize`.
.tr_op_set_solto <- function(doc, op, registry) {
  .tr_require(op, c("node", "x", "y", "w", "h")); .tr_node_or_abort(doc, op$node)
  doc$ui$soltos[[op$node]] <- c(.tr_scalar_num(op$x, "x"), .tr_scalar_num(op$y, "y"),
                                .tr_scalar_num(op$w, "w"), .tr_scalar_num(op$h, "h"))
  list(doc = doc, op = op)
}

# Preview escondido no card: só a faixa some, o nó continua executando (é
# apresentação, como `set_mode`). Mapa à parte e não um modo novo porque
# ocultar é ortogonal a mini/completo: quem volta de mini pro completo quer
# reencontrar o preview como o deixou. Só `TRUE` entra no documento; a
# ausência já diz "visível".
.tr_op_set_preview_oculto <- function(doc, op, registry) {
  .tr_require(op, c("node", "oculto")); .tr_node_or_abort(doc, op$node)
  o <- op$oculto
  if (!is.logical(o) || length(o) != 1L || is.na(o)) {
    rlang::abort("oculto deve ser TRUE ou FALSE.", class = "tr_error_bad_op")
  }
  doc$ui$ocultos[[op$node]] <- if (o) TRUE else NULL
  list(doc = doc, op = op)
}

# Documento gravado antes dos modos guardava três chaves soltas por card.
# `mini` manda sobre as outras (era assim que o card desenhava); sem ele, o
# que sobra aberto decide, e os dois recolhidos davam um card só com cabeçalho,
# que é o mini de agora.
.tr_mode_from_fold <- function(f) {
  if (isTRUE(f$mini)) return("mini")
  pv <- !isFALSE(f$preview); pm <- !isFALSE(f$params)
  if (pv && pm) "completo" else if (pv) "preview" else if (pm) "params" else "mini"
}

.tr_op_connect <- function(doc, op, registry) {
  .tr_require(op, c("from_node", "from_port", "to_node", "to_port"))
  from <- .tr_node_or_abort(doc, op$from_node)
  to   <- .tr_node_or_abort(doc, op$to_node)
  out_port <- tr_get_node(from$type, registry)$outputs[[op$from_port]]
  in_port  <- tr_get_node(to$type, registry)$inputs[[op$to_port]]
  if (is.null(out_port) || is.null(in_port)) {
    rlang::abort(sprintf("Porta inexistente: %s.%s -> %s.%s.",
                         op$from_node, op$from_port, op$to_node, op$to_port),
                 class = "tr_error_unknown_port")
  }
  if (!tr_compatible(out_port$type, in_port$type, registry)) {
    rlang::abort(sprintf("Tipos incompatíveis: %s (%s) -> %s (%s).",
                         op$from_port, out_port$type, op$to_port, in_port$type),
                 class = "tr_error_type_mismatch")
  }

  # Porta não-variádica aceita uma aresta só: reconectar substitui, que é o
  # gesto esperado ao arrastar um cabo pra uma porta ocupada.
  if (!isTRUE(in_port$multiple)) {
    doc$edges <- Filter(function(e) !(e$to$node == op$to_node && e$to$port == op$to_port), doc$edges)
    index <- 1L
  } else {
    # `index` ordena as entradas de uma porta variádica — é a razão de a porta
    # variádica existir desde o começo. Atribuído aqui (próximo livre) quando
    # o cliente não escolhe, senão duas origens entrariam ambas como 1 e a
    # ordem viraria indefinida — não-determinismo de resultado no executor.
    used <- vapply(Filter(function(e) e$to$node == op$to_node && e$to$port == op$to_port,
                          doc$edges), function(e) as.integer(e$index %||% 1L), integer(1))
    index <- if (is.null(op$index)) (if (length(used)) max(used) + 1L else 1L) else as.integer(op$index)
  }

  edge <- list(from = list(node = op$from_node, port = op$from_port),
               to = list(node = op$to_node, port = op$to_port), index = index)
  if (any(vapply(doc$edges, function(e) identical(.tr_edge_key(e), .tr_edge_key(edge)), logical(1)))) {
    rlang::abort("Aresta já existe.", class = "tr_error_duplicate_edge")
  }
  .tr_check_no_cycle(doc, op$from_node, op$to_node)
  doc$edges <- c(doc$edges, list(edge))
  op$index <- index
  list(doc = doc, op = op)
}

.tr_op_disconnect <- function(doc, op, registry) {
  .tr_require(op, c("from_node", "from_port", "to_node", "to_port"))
  matches <- function(e) {
    e$from$node == op$from_node && e$from$port == op$from_port &&
      e$to$node == op$to_node && e$to$port == op$to_port &&
      (is.null(op$index) || identical(as.integer(e$index %||% 1L), as.integer(op$index)))
  }
  n_before <- length(doc$edges)
  doc$edges <- Filter(function(e) !matches(e), doc$edges)
  if (length(doc$edges) == n_before) {
    rlang::abort("Aresta não existe.", class = "tr_error_unknown_edge")
  }
  list(doc = doc, op = op)
}

#' Lista de adjacência num environment, não num list.
#'
#' `succ[[k]] <- c(succ[[k]], v)` sobre uma lista R copia a lista a cada
#' inserção, o que torna a MONTAGEM da adjacência O(E²) — era o que dominava o
#' custo do `connect` em grafo grande (11s em 900 arestas), não a travessia.
#' Environment tem inserção O(1).
#' @noRd
.tr_succ <- function(doc) {
  e <- new.env(hash = TRUE, parent = emptyenv(), size = length(doc$edges) + 1L)
  for (edge in doc$edges) {
    k <- edge$from$node
    assign(k, c(mget(k, envir = e, ifnotfound = list(character()))[[1]], edge$to$node), envir = e)
  }
  e
}

#' Ciclo checado na EDIÇÃO, não na execução: erro na edição é localizável
#' ("essa aresta fecha um ciclo"); erro na execução aparece longe da causa.
#'
#' Incremental e iterativo, por dois motivos medidos. Um DFS global a cada
#' `connect` custava O(V·E): 200 nós levavam ~5,7s e 400 levavam ~38s, o que
#' já é intolerável na edição. E a versão recursiva estourava a pilha do R
#' (derrubando a sessão inteira do coordenador, sem erro tratável) por volta
#' de 3000 nós de profundidade. Aqui: a aresta nova só pode fechar ciclo se
#' `to_node` já alcança `from_node`, então basta uma travessia a partir de
#' `to_node`, com pilha explícita. 200 nós passaram a levar 0,49s.
#'
#' Continua O(E) por `connect` — a checagem de duplicata, o `Filter` e a cópia
#' da lista de arestas são todos lineares, e a lista é imutável de propósito.
#' Isso é ~2,5ms por aresta em 200 nós e ~10ms em 900: irrelevante na edição,
#' que é uma aresta por vez. Só vira quadrático em carga em massa — que entra
#' por `tr_doc_parse()`, não pelas ops, e portanto não paga esse custo.
#' @noRd
.tr_check_no_cycle <- function(doc, from_node, to_node) {
  if (identical(from_node, to_node)) {
    rlang::abort(sprintf("Ciclo detectado: %s -> %s.", from_node, from_node),
                 class = "tr_error_cycle")
  }
  succ <- .tr_succ(doc)
  seen <- character(); stack <- to_node
  while (length(stack) > 0) {
    cur <- stack[[length(stack)]]; stack <- stack[-length(stack)]
    if (cur %in% seen) next
    seen <- c(seen, cur)
    if (identical(cur, from_node)) {
      rlang::abort(sprintf("Ciclo detectado: a aresta %s -> %s fecha um ciclo.", from_node, to_node),
                   class = "tr_error_cycle")
    }
    stack <- c(stack, mget(cur, envir = succ, ifnotfound = list(character()))[[1]])
  }
  invisible(TRUE)
}

#' Checagem global de ciclo — para documento vindo de JSON cru, que nunca
#' passou pelas ops. Iterativa pelo mesmo motivo acima.
#' @noRd
.tr_find_cycle <- function(doc) {
  succ <- .tr_succ(doc)
  state <- stats::setNames(rep("new", length(doc$nodes)), names(doc$nodes))
  for (root in names(doc$nodes)) {
    if (state[[root]] != "new") next
    stack <- list(list(id = root, kids = mget(root, envir = succ, ifnotfound = list(character()))[[1]]))
    state[[root]] <- "open"
    while (length(stack) > 0) {
      top <- stack[[length(stack)]]
      if (length(top$kids) == 0) {
        state[[top$id]] <- "done"; stack[[length(stack)]] <- NULL; next
      }
      kid <- top$kids[[1]]
      stack[[length(stack)]]$kids <- top$kids[-1]
      if (!(kid %in% names(state))) next         # aresta pendurada: outro problema
      if (state[[kid]] == "open") return(c(top$id, kid))
      if (state[[kid]] == "new") {
        state[[kid]] <- "open"
        stack[[length(stack) + 1]] <- list(id = kid, kids = mget(kid, envir = succ, ifnotfound = list(character()))[[1]])
      }
    }
  }
  NULL
}

#' Nós sem aresta de saída — os alvos default de execução.
#' @export
tr_doc_terminals <- function(doc) {
  with_out <- unique(vapply(doc$edges, function(e) e$from$node, ""))
  setdiff(names(doc$nodes), with_out)
}

#' @export
print.tr_doc <- function(x, ...) {
  cat(sprintf("<tr_doc> rev %d | %d nó(s), %d aresta(s)\n", x$rev, length(x$nodes), length(x$edges)))
  if (length(x$collections)) {
    cat("  coleções:", paste(sprintf("%s@%s", names(x$collections), unlist(x$collections)), collapse = ", "), "\n")
  }
  for (id in names(x$nodes)) {
    n <- x$nodes[[id]]
    cat(sprintf("  %-18s %-22s %s\n", id, n$type, n$label %||% ""))
  }
  invisible(x)
}

#' @export
print.tr_registry <- function(x, ...) {
  cat(sprintf("<tr_registry> %d coleção(ões), %d tipo(s), %d nó(s), %d adaptador(es)\n",
              length(x$collections), length(x$types), length(x$nodes), length(x$adapters)))
  for (c in x$collections) cat(sprintf("  %s@%s\n", c$id, c$version))
  invisible(x)
}
