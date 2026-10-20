#' Layout por etapas: o fluxo arrumado como narrativa, do lado do servidor.
#'
#' O agente (MCP, `trama-agente`) não tem mouse: para pôr blocos num frame,
#' prender uma nota a um resultado ou arrumar o fluxo inteiro ele precisa de
#' alguém que calcule as posições. Tudo aqui é função pura de documento e
#' medidas e devolve OPS comuns do documento (`move`, `update_frame`,
#' `update_note`, `set_solto`, `add_frame`, `add_note`, `add_grupo`): o canal
#' de controle as manda como um batch pelo mesmo `aplicar()` do editor, e um
#' Ctrl+Z desfaz o gesto inteiro. Nenhuma op nova, nenhum campo novo no
#' documento.
#'
#' As regras de pertencimento são as do editor (`inst/www/geometria.js`):
#' card ou nota pertence ao MENOR frame que o contém inteiro; frame que
#' contém outro frame é "externo" (prancheta) e não vira etapa. Nota presa a
#' um bloco é um grupo (`ui.grupos`) com a nota e o bloco: anda junto no
#' editor e, aqui, fica logo abaixo do bloco.
#'
#' Altura de card depende de params, portas e preview, e só o navegador sabe.
#' O editor manda as medidas (`tr_medidas`, ver `medidasCards` em
#' `geometria.js`); sem elas vale a estimativa de `rectOf()`.
#' @noRd
NULL

# Espelhos de `geometria.js` (CARD_W, CARD_H, PREVIEW_H, FRAME_HEAD,
# FRAME_PAD) e os espaços do layout: entre colunas e linhas os do dagre do
# Organizar (`ranksep`, `nodesep`), entre faixas e entre frames, maiores, para
# a etapa se ler como um bloco.
.tr_lay <- list(card_w = 240, card_h = 190, preview_h = 132, head = 44, pad = 32,
                col = 90, lin = 40, banda = 80, frame = 160, nota = 16,
                frame_w = 640, frame_h = 400, nota_w = 320)

# Retângulos ---------------------------------------------------------------

.tr_lay_num <- function(x) suppressWarnings(as.numeric(unlist(x)))

#' Largura e altura de um card: medida do editor, senão a estimativa de
#' `rectOf()` (a altura declarada em `ui.sizes` é a do preview).
#' @noRd
.tr_lay_card_wh <- function(doc, id, medidas = NULL) {
  m <- .tr_lay_num(medidas[[id]])
  if (length(m) >= 2L && all(is.finite(m[1:2])) && all(m[1:2] > 0)) return(m[1:2])
  sz <- .tr_lay_num(doc$ui$sizes[[id]])
  w <- max(.tr_lay$card_w, if (length(sz) >= 1L && is.finite(sz[[1]])) sz[[1]] else 0)
  ph <- if (length(sz) >= 2L && is.finite(sz[[2]])) sz[[2]] else .tr_lay$preview_h
  c(w, .tr_lay$card_h + max(0, ph - .tr_lay$preview_h))
}

#' `list(cards, frames, notes)`, cada um id -> c(x, y, w, h). Card sem posição
#' fica com x e y `NA`: não pertence a frame nenhum.
#' @noRd
.tr_lay_rects <- function(doc, medidas = NULL) {
  cards <- lapply(stats::setNames(nm = names(doc$nodes)), function(id) {
    p <- .tr_lay_num(doc$ui$positions[[id]])
    xy <- if (length(p) >= 2L) p[1:2] else c(NA_real_, NA_real_)
    c(xy, .tr_lay_card_wh(doc, id, medidas))
  })
  quatro <- function(f) c(.tr_lay_num(f$x), .tr_lay_num(f$y), .tr_lay_num(f$w), .tr_lay_num(f$h))
  list(cards = cards,
       frames = lapply(doc$ui$frames %||% list(), quatro),
       notes = lapply(doc$ui$notes %||% list(), quatro))
}

.tr_lay_dentro <- function(a, b) {
  !anyNA(a) && a[[1]] >= b[[1]] && a[[2]] >= b[[2]] &&
    a[[1]] + a[[3]] <= b[[1]] + b[[3]] && a[[2]] + a[[4]] <= b[[2]] + b[[4]]
}

#' Quem é de qual frame, pela regra de `donos()` (geometria.js): externos fora,
#' o menor frame que contém o item, empate pela ordem de slide.
#'
#' Uma diferença deliberada: o editor pede o card INTEIRO dentro; aqui basta o
#' canto de cima do card (240x190, o card sem preview). O agente põe o bloco
#' no frame antes de ele rodar, e o gráfico que chega depois estica o card
#' para fora da borda: pela régua do editor ele sairia da etapa no próximo
#' layout. Com o canto, ele fica, e o layout aumenta o frame até caber — e aí
#' o editor concorda. Mesma folga para nota (o piso de `NOTA_MIN`).
#' @return `list(externos, etapas, card, nota)`: `etapas` são os frames que
#'   não são externos, na ordem de slide; `card` e `nota` são id -> frame.
#' @noRd
.tr_lay_donos <- function(doc, rects) {
  fr <- rects$frames
  ids <- names(fr) %||% character()
  area <- vapply(fr, function(r) r[[3]] * r[[4]], 0)
  ordem <- vapply(ids, function(id) as.numeric(doc$ui$frames[[id]]$order %||% 0), 0)
  externos <- ids[vapply(ids, function(f) any(vapply(setdiff(ids, f), function(g) {
    area[[g]] < area[[f]] && .tr_lay_dentro(fr[[g]], fr[[f]])
  }, TRUE)), TRUE)]
  cand <- setdiff(ids, externos)
  cand <- cand[order(area[cand], ordem[cand])]
  dono <- function(rs, piso) {
    out <- character()
    for (id in names(rs)) {
      r <- rs[[id]]
      canto <- c(r[1:2], min(r[[3]], piso[[1]]), min(r[[4]], piso[[2]]))
      for (f in cand) if (.tr_lay_dentro(canto, fr[[f]])) { out[[id]] <- f; break }
    }
    out
  }
  etapas <- setdiff(ids, externos)
  list(externos = externos, etapas = etapas[order(ordem[etapas], etapas)],
       card = dono(rects$cards, c(.tr_lay$card_w, .tr_lay$card_h)),
       nota = dono(rects$notes, c(160, 80)))
}

#' Nota presa a bloco: nota num grupo que tem card fica com o PRIMEIRO card do
#' grupo (ordem dos membros). Nota -> card.
#' @noRd
.tr_lay_anexos <- function(doc) {
  out <- character()
  for (g in doc$ui$grupos %||% list()) {
    m <- as.character(unlist(g))
    cards <- m[m %in% names(doc$nodes)]
    if (!length(cards)) next
    for (n in m[m %in% names(doc$ui$notes)]) out[[n]] <- cards[[1]]
  }
  out
}

#' O card como peça do layout: o card, o preview solto dele (o envelope dos
#' dois, como no Organizar) e as notas presas, empilhadas logo abaixo.
#' Deslocamentos relativos ao canto da peça.
#' @noRd
.tr_lay_peca <- function(doc, id, rects, anexos) {
  r <- rects$cards[[id]]
  so <- .tr_lay_num(doc$ui$soltos[[id]])
  base <- if (length(so) == 4L && !anyNA(r)) {
    x <- min(r[[1]], so[[1]]); y <- min(r[[2]], so[[2]])
    list(w = max(r[[1]] + r[[3]], so[[1]] + so[[3]]) - x,
         h = max(r[[2]] + r[[4]], so[[2]] + so[[4]]) - y,
         card = c(r[[1]] - x, r[[2]] - y), solto = c(so[[1]] - x, so[[2]] - y), solto_wh = so[3:4])
  } else list(w = r[[3]], h = r[[4]], card = c(0, 0))
  notas <- list()
  y <- base$h
  for (n in names(anexos)[anexos == id]) {
    nr <- rects$notes[[n]]
    y <- y + .tr_lay$nota
    notas[[n]] <- c(0, y)
    base$w <- max(base$w, nr[[3]])
    y <- y + nr[[4]]
  }
  base$h <- y
  base$notas <- notas
  base
}

# Uma etapa ----------------------------------------------------------------

#' Arruma as peças de uma etapa da esquerda para a direita.
#'
#' Coluna = caminho mais longo desde as fontes da etapa (só ligações de
#' dentro). Duas faixas: em cima o ramo principal (o que leva o fluxo para
#' fora da etapa e seus ancestrais), embaixo o resto (gráficos, testes,
#' tabelas de diagnóstico); sem nada saindo da etapa, faixa única. Numa coluna
#' as peças se empilham pela média da altura dos pais (o que reduz
#' cruzamentos) e, no empate, pela ordem de antes; cada uma tenta ficar na
#' altura do primeiro pai, para uma corrente sair reta. Quem não tem pai na
#' etapa (as entradas dela) segue a altura dos pais na etapa anterior: os
#' testes da etapa 2 saem na ordem das séries da etapa 1. Notas da etapa abrem
#' a etapa numa coluna própria à esquerda.
#' @param pecas id -> `list(w, h)`.
#' @param de,para Ligações dentro da etapa.
#' @param principal Ids do ramo principal.
#' @param ordem0 id -> chave da ordem de antes.
#' @param intro Lista nomeada nota -> c(w, h), na ordem.
#' @param fora id -> altura dos pais de fora da etapa (`NA` sem pai de fora).
#' @return `list(pos = id -> c(x, y), intro = nota -> c(x, y), w, h)`, relativo
#'   ao canto do conteúdo.
#' @noRd
.tr_lay_etapa <- function(pecas, de = character(), para = character(), principal = character(),
                          ordem0 = NULL, intro = list(), fora = NULL) {
  ids <- names(pecas) %||% character()
  ordem0 <- ordem0 %||% stats::setNames(seq_along(ids), ids)
  fora <- fora %||% stats::setNames(rep(NA_real_, length(ids)), ids)
  pais <- lapply(stats::setNames(nm = ids), function(id) unique(de[para == id]))
  # Caminho mais longo: Kahn sobre as ligações de dentro (o fluxo é DAG).
  rank <- stats::setNames(rep(0L, length(ids)), ids)
  falta <- vapply(pais, length, 0L)
  fila <- ids[falta == 0L]
  while (length(fila)) {
    n <- fila[[1]]; fila <- fila[-1]
    for (f in unique(para[de == n])) {
      rank[[f]] <- max(rank[[f]], rank[[n]] + 1L)
      falta[[f]] <- falta[[f]] - 1L
      if (falta[[f]] == 0L) fila <- c(fila, f)
    }
  }
  faixa <- if (length(principal) && !all(ids %in% principal)) {
    ifelse(ids %in% principal, 1L, 2L)
  } else rep(1L, length(ids))
  names(faixa) <- ids

  x0 <- 0
  intro_pos <- list(); yi <- 0
  for (n in names(intro)) {
    intro_pos[[n]] <- c(0, yi)
    yi <- yi + intro[[n]][[2]] + .tr_lay$lin
    x0 <- max(x0, intro[[n]][[1]] + .tr_lay$col)
  }
  cols <- sort(unique(rank))
  larg <- vapply(cols, function(r) max(vapply(pecas[ids[rank == r]], function(p) p$w, 0)), 0)
  xcol <- stats::setNames(x0 + c(0, cumsum(larg + .tr_lay$col))[seq_along(cols)], cols)

  y <- stats::setNames(rep(NA_real_, length(ids)), ids)
  h <- vapply(pecas, function(p) p$h, 0)
  topo <- 0
  for (b in sort(unique(faixa))) {
    for (r in cols) {
      aqui <- ids[rank == r & faixa == b]
      if (!length(aqui)) next
      bary <- vapply(aqui, function(n) {
        ps <- pais[[n]][!is.na(y[pais[[n]]])]
        if (length(ps)) mean(y[ps] + h[ps] / 2) else NA_real_
      }, 0)
      aqui <- aqui[order(is.na(bary), bary, is.na(fora[aqui]), fora[aqui], ordem0[aqui], aqui)]
      cur <- topo
      for (n in aqui) {
        ps <- pais[[n]][!is.na(y[pais[[n]]])]
        alvo <- if (length(ps)) y[ps][order(y[ps])][[1]] else cur
        y[[n]] <- max(cur, alvo)
        cur <- y[[n]] + h[[n]] + .tr_lay$lin
      }
    }
    na_faixa <- ids[faixa == b]
    topo <- max(y[na_faixa] + h[na_faixa]) + .tr_lay$banda
  }
  pos <- lapply(stats::setNames(nm = ids), function(n) c(xcol[[as.character(rank[[n]])]], y[[n]]))
  w <- max(c(0, vapply(ids, function(n) pos[[n]][[1]] + pecas[[n]]$w, 0),
             vapply(names(intro), function(n) intro[[n]][[1]], 0)))
  hh <- max(c(0, vapply(ids, function(n) y[[n]] + h[[n]], 0),
              vapply(names(intro), function(n) intro_pos[[n]][[2]] + intro[[n]][[2]], 0)))
  list(pos = pos, intro = intro_pos, w = w, h = hh)
}

#' Ramo principal de uma etapa: quem liga para fora dela e os ancestrais
#' desses dentro dela.
#' @noRd
.tr_lay_principal <- function(ids, de, para) {
  sai <- unique(de[de %in% ids & !(para %in% ids)])
  dentro <- de %in% ids & para %in% ids
  d <- de[dentro]; p <- para[dentro]
  alcance <- sai
  repeat {
    novos <- setdiff(d[p %in% alcance], alcance)
    if (!length(novos)) break
    alcance <- c(alcance, novos)
  }
  alcance
}

.tr_lay_arestas <- function(doc) {
  list(de = vapply(doc$edges, function(e) e$from$node, ""),
       para = vapply(doc$edges, function(e) e$to$node, ""))
}

#' Ordem de antes: de cima para baixo, depois da esquerda para a direita; sem
#' posição, por último, na ordem do documento.
#' @noRd
.tr_lay_ordem0 <- function(rects) {
  ids <- names(rects$cards)
  y <- vapply(rects$cards, function(r) r[[2]], 0); x <- vapply(rects$cards, function(r) r[[1]], 0)
  o <- order(is.na(y), y, x, seq_along(ids))
  stats::setNames(match(seq_along(ids), o), ids)
}

#' Peças, ligações e ramo principal de um conjunto de cards, e o resultado
#' da etapa já com o deslocamento de cada parte (card, solto, notas presas).
#' @noRd
.tr_lay_montar <- function(doc, rects, cards, intro_notas, anexos, ordem0, ys = NULL) {
  ar <- .tr_lay_arestas(doc)
  pecas <- lapply(stats::setNames(nm = cards), .tr_lay_peca, doc = doc, rects = rects, anexos = anexos)
  dentro <- ar$de %in% cards & ar$para %in% cards
  intro <- lapply(stats::setNames(nm = intro_notas), function(n) rects$notes[[n]][3:4])
  # Altura dos pais de fora: a nova, se já foram arrumados (`ys`), senão a de agora.
  ys <- ys %||% list()
  y_de <- function(p) (ys[[p]] %||% rects$cards[[p]])[[2]]
  fora <- vapply(cards, function(id) {
    ps <- unique(ar$de[ar$para == id & !(ar$de %in% cards)])
    v <- vapply(ps, y_de, 0)
    if (length(v) && any(!is.na(v))) mean(v, na.rm = TRUE) else NA_real_
  }, 0)
  et <- .tr_lay_etapa(pecas, ar$de[dentro], ar$para[dentro],
                      .tr_lay_principal(cards, ar$de, ar$para), ordem0[cards], intro, fora)
  et$pecas <- pecas
  et
}

#' Põe o resultado de uma etapa no canto (ox, oy) do canvas, em `novo`.
#' @noRd
.tr_lay_colocar <- function(novo, et, ox, oy) {
  for (id in names(et$pos)) {
    p <- et$pos[[id]] + c(ox, oy); pc <- et$pecas[[id]]
    novo$cards[[id]] <- p + pc$card
    if (!is.null(pc$solto)) novo$soltos[[id]] <- c(p + pc$solto, pc$solto_wh)
    for (n in names(pc$notas)) novo$notes[[n]] <- p + pc$notas[[n]]
  }
  for (n in names(et$intro)) novo$notes[[n]] <- et$intro[[n]] + c(ox, oy)
  novo
}

#' Tamanho do frame para um conteúdo: folga, cabeçalho e, com proporção
#' travada, a dimensão curta cresce até ela (a regra de `fitAspect`).
#' @noRd
.tr_lay_frame_wh <- function(cw, ch, aspect) {
  w <- ceiling(cw + 2 * .tr_lay$pad); h <- ceiling(ch + 2 * .tr_lay$pad + .tr_lay$head)
  ra <- c(`16:9` = 16 / 9, `4:3` = 4 / 3, `1:1` = 1, A4 = 1 / sqrt(2))[aspect %||% "livre"]
  if (!is.na(ra)) {
    if (w / h < ra) w <- ceiling(h * ra) else h <- ceiling(w / ra)
  }
  c(w, h)
}

# Fluxo inteiro --------------------------------------------------------------

#' O layout por etapas do fluxo inteiro.
#'
#' Cada frame (não externo) é uma etapa, e as etapas vão lado a lado, da
#' esquerda para a direita, na ordem de slide, alinhadas pelo topo. O frame
#' toma o tamanho do conteúdo (frame vazio fica como está). Cards e notas fora
#' de frame formam uma faixa abaixo das etapas (ou o fluxo todo, sem frames).
#' Externos envolvem de novo os frames que continham. Começa no canto de cima
#' à esquerda do que já existe, então arrumar duas vezes não muda nada.
#' @return `list(cards, frames, notes, soltos)` com as posições novas.
#' @noRd
.tr_lay_etapas <- function(doc, medidas = NULL) {
  rects <- .tr_lay_rects(doc, medidas)
  donos <- .tr_lay_donos(doc, rects)
  anexos <- .tr_lay_anexos(doc)
  ordem0 <- .tr_lay_ordem0(rects)
  presos <- names(anexos)
  todos <- c(rects$cards, rects$frames, rects$notes)
  xs <- vapply(todos, function(r) r[[1]], 0); ys <- vapply(todos, function(r) r[[2]], 0)
  ox <- if (any(is.finite(xs))) min(xs, na.rm = TRUE) else 0
  oy <- if (any(is.finite(ys))) min(ys, na.rm = TRUE) else 0

  novo <- list(cards = list(), frames = list(), notes = list(), soltos = list())
  x <- ox; base <- oy
  for (f in donos$etapas) {
    cards <- names(donos$card)[donos$card == f]
    notas <- setdiff(names(donos$nota)[donos$nota == f], presos)
    notas <- notas[order(vapply(notas, function(n) rects$notes[[n]][[2]], 0))]
    fr <- rects$frames[[f]]
    if (!length(cards) && !length(notas)) {
      wh <- fr[3:4]
    } else {
      et <- .tr_lay_montar(doc, rects, cards, notas, anexos, ordem0, novo$cards)
      wh <- .tr_lay_frame_wh(et$w, et$h, doc$ui$frames[[f]]$aspect)
      novo <- .tr_lay_colocar(novo, et, x + .tr_lay$pad, oy + .tr_lay$head + .tr_lay$pad)
    }
    novo$frames[[f]] <- c(x, oy, wh)
    x <- x + wh[[1]] + .tr_lay$frame
    base <- max(base, oy + wh[[2]] + .tr_lay$frame)
  }
  soltos <- setdiff(names(rects$cards), names(donos$card))
  notas <- setdiff(names(rects$notes), c(names(donos$nota), presos))
  if (length(soltos) || length(notas)) {
    notas <- notas[order(vapply(notas, function(n) rects$notes[[n]][[2]], 0))]
    et <- .tr_lay_montar(doc, rects, soltos, notas, anexos, ordem0, novo$cards)
    novo <- .tr_lay_colocar(novo, et, ox, if (length(donos$etapas)) base else oy)
  }
  # Externos do menor para o maior: o de fora já vê o de dentro refeito.
  area <- function(f) rects$frames[[f]][[3]] * rects$frames[[f]][[4]]
  for (e in donos$externos[order(vapply(donos$externos, area, 0))]) {
    dentro <- Filter(function(g) g != e && .tr_lay_dentro(rects$frames[[g]], rects$frames[[e]]),
                     names(rects$frames))
    rs <- lapply(dentro, function(g) novo$frames[[g]] %||% rects$frames[[g]])
    if (!length(rs)) next
    x0 <- min(vapply(rs, `[[`, 0, 1)) - .tr_lay$pad
    y0 <- min(vapply(rs, `[[`, 0, 2)) - .tr_lay$pad - .tr_lay$head
    x1 <- max(vapply(rs, function(r) r[[1]] + r[[3]], 0)) + .tr_lay$pad
    y1 <- max(vapply(rs, function(r) r[[2]] + r[[4]], 0)) + .tr_lay$pad
    novo$frames[[e]] <- c(x0, y0, x1 - x0, y1 - y0)
  }
  novo
}

#' Ops que levam o documento a `novo`; só o que mudou, inteiro.
#' @noRd
.tr_lay_ops <- function(doc, novo, medidas = NULL) {
  rects <- .tr_lay_rects(doc, medidas)
  ops <- list()
  add <- function(op) ops[[length(ops) + 1L]] <<- op
  for (id in names(novo$cards)) {
    p <- round(novo$cards[[id]]); r <- rects$cards[[id]]
    if (anyNA(r[1:2]) || any(p != round(r[1:2]))) add(list(op = "move", node = id, x = p[[1]], y = p[[2]]))
  }
  for (id in names(novo$soltos)) {
    s <- round(novo$soltos[[id]]); v <- round(.tr_lay_num(doc$ui$soltos[[id]]))
    if (length(v) != 4L || any(s != v)) {
      add(list(op = "set_solto", node = id, x = s[[1]], y = s[[2]], w = s[[3]], h = s[[4]]))
    }
  }
  for (id in names(novo$frames)) {
    f <- round(novo$frames[[id]]); r <- round(rects$frames[[id]])
    op <- list(op = "update_frame", frame = id)
    if (any(f[1:2] != r[1:2])) { op$x <- f[[1]]; op$y <- f[[2]] }
    if (f[[3]] != r[[3]]) op$w <- f[[3]]
    if (f[[4]] != r[[4]]) op$h <- f[[4]]
    if (length(op) > 2L) add(op)
  }
  for (id in names(novo$notes)) {
    p <- round(novo$notes[[id]]); r <- round(rects$notes[[id]])
    if (any(p != r[1:2])) add(list(op = "update_note", note = id, x = p[[1]], y = p[[2]]))
  }
  ops
}

# Um frame -------------------------------------------------------------------

#' Rearruma o conteúdo de UM frame onde ele está (o canto fica, o tamanho
#' segue o conteúdo) e abre espaço se ele cresceu: o que estava à direita
#' anda para a direita, o que estava embaixo (na largura dele) anda para
#' baixo, cada frame com o que tem dentro. Sem isso, o frame que cresce
#' engoliria o vizinho — pertencimento é geométrico.
#' @param incluir,excluir Cards que entram ou saem do frame; quem sai vai para
#'   uma linha logo abaixo dele.
#' @param incluir_notas Notas que entram na abertura do frame.
#' @noRd
.tr_lay_frame <- function(doc, frame, medidas = NULL, incluir = character(), excluir = character(),
                          incluir_notas = character()) {
  rects <- .tr_lay_rects(doc, medidas)
  if (is.null(rects$frames[[frame]])) rlang::abort(sprintf("Frame '%s' não existe.", frame))
  donos <- .tr_lay_donos(doc, rects)
  anexos <- .tr_lay_anexos(doc)
  presos <- names(anexos)
  cards <- setdiff(union(names(donos$card)[donos$card == frame], incluir), excluir)
  notas <- union(setdiff(names(donos$nota)[donos$nota == frame], presos), incluir_notas)
  notas <- notas[order(vapply(notas, function(n) rects$notes[[n]][[2]], 0))]
  fr <- rects$frames[[frame]]
  novo <- list(cards = list(), frames = list(), notes = list(), soltos = list())
  if (length(cards) || length(notas)) {
    et <- .tr_lay_montar(doc, rects, cards, notas, anexos, .tr_lay_ordem0(rects))
    wh <- .tr_lay_frame_wh(et$w, et$h, doc$ui$frames[[frame]]$aspect)
    novo <- .tr_lay_colocar(novo, et, fr[[1]] + .tr_lay$pad, fr[[2]] + .tr_lay$head + .tr_lay$pad)
  } else wh <- fr[3:4]
  novo$frames[[frame]] <- c(fr[1:2], wh)

  # Quem sai: uma linha logo abaixo do frame novo.
  x <- fr[[1]]
  for (id in intersect(excluir, names(donos$card)[donos$card == frame])) {
    pc <- .tr_lay_peca(doc, id, rects, anexos)
    novo <- .tr_lay_colocar(novo, list(pos = stats::setNames(list(c(0, 0)), id), intro = list(),
                                       pecas = stats::setNames(list(pc), id)),
                            x, fr[[2]] + wh[[2]] + .tr_lay$col)
    x <- x + pc$w + .tr_lay$col
  }

  dw <- wh[[1]] - fr[[3]]; dh <- wh[[2]] - fr[[4]]
  if (dw > 0 || dh > 0) {
    mexidos <- c(cards, excluir, notas, frame)
    unid <- .tr_lay_unidades(doc, rects, donos, anexos)
    for (u in unid) {
      if (any(c(u$id, u$cards, u$notas) %in% mexidos)) next
      r <- u$rect
      sx <- if (dw > 0 && r[[1]] >= fr[[1]] + fr[[3]]) dw else 0
      sy <- if (dh > 0 && sx == 0 && r[[2]] >= fr[[2]] + fr[[4]] &&
                r[[1]] < fr[[1]] + wh[[1]] && r[[1]] + r[[3]] > fr[[1]]) dh else 0
      if (sx == 0 && sy == 0) next
      novo <- .tr_lay_deslocar(novo, doc, rects, u, c(sx, sy))
    }
  }
  novo
}

#' O que anda junto quando se abre espaço: cada frame (não externo) com seus
#' cards, notas e o que está preso aos cards; cada card solto; cada nota
#' solta. Externo não anda (envolveria a etapa que cresceu).
#' @noRd
.tr_lay_unidades <- function(doc, rects, donos, anexos) {
  presos <- names(anexos)
  com_anexos <- function(cards) names(anexos)[anexos %in% cards]
  us <- lapply(donos$etapas, function(f) {
    cards <- names(donos$card)[donos$card == f]
    list(id = f, rect = rects$frames[[f]], cards = cards,
         notas = union(names(donos$nota)[donos$nota == f], com_anexos(cards)))
  })
  livres <- setdiff(names(rects$cards), names(donos$card))
  livres <- livres[!vapply(livres, function(id) anyNA(rects$cards[[id]]), TRUE)]
  us <- c(us, lapply(livres, function(id) list(id = NULL, rect = rects$cards[[id]], cards = id,
                                                notas = com_anexos(id))))
  notas <- setdiff(names(rects$notes), c(names(donos$nota), presos))
  c(us, lapply(notas, function(n) list(id = NULL, rect = rects$notes[[n]], cards = character(), notas = n)))
}

.tr_lay_deslocar <- function(novo, doc, rects, u, d) {
  if (!is.null(u$id)) novo$frames[[u$id]] <- c(rects$frames[[u$id]][1:2] + d, rects$frames[[u$id]][3:4])
  for (id in u$cards) {
    novo$cards[[id]] <- rects$cards[[id]][1:2] + d
    so <- .tr_lay_num(doc$ui$soltos[[id]])
    if (length(so) == 4L) novo$soltos[[id]] <- c(so[1:2] + d, so[3:4])
  }
  for (n in u$notas) novo$notes[[n]] <- rects$notes[[n]][1:2] + d
  novo
}

# Mover ----------------------------------------------------------------------

#' Mover como o arrasto do editor: card leva o preview solto; frame leva tudo
#' o que contém inteiro (cards, notas, frames); quem está num grupo leva o
#' grupo. Cada item anda uma vez; o que foi pedido explicitamente vence.
#' @param itens Lista de `list(id, x, y)` (absoluto) ou `list(id, dx, dy)`.
#' @noRd
.tr_lay_mover <- function(doc, itens, medidas = NULL) {
  rects <- .tr_lay_rects(doc, medidas)
  tipo <- function(id) {
    if (!is.null(doc$nodes[[id]])) "card" else if (!is.null(doc$ui$frames[[id]])) "frame"
    else if (!is.null(doc$ui$notes[[id]])) "nota"
    else rlang::abort(sprintf("Não há bloco, frame ou nota com id '%s'.", id))
  }
  rect <- function(id) switch(tipo(id), card = rects$cards[[id]], frame = rects$frames[[id]],
                              nota = rects$notes[[id]])
  delta <- list()
  for (it in itens) {
    id <- as.character(it$id %||% "")
    r <- rect(id)
    d <- if (!is.null(it$x) || !is.null(it$y)) {
      if (is.null(it$x) || is.null(it$y)) rlang::abort(sprintf("'%s': passe x e y juntos.", id))
      c(.tr_scalar_num(it$x, "x"), .tr_scalar_num(it$y, "y")) - (if (anyNA(r[1:2])) c(0, 0) else r[1:2])
    } else c(.tr_scalar_num(it$dx %||% 0, "dx"), .tr_scalar_num(it$dy %||% 0, "dy"))
    delta[[id]] <- d
  }
  pedidos <- names(delta)
  seguir <- function(id, d) if (is.null(delta[[id]])) delta[[id]] <<- d
  for (id in pedidos) {
    d <- delta[[id]]
    if (tipo(id) == "frame") {
      fr <- rects$frames[[id]]
      for (k in c("cards", "notes", "frames")) for (o in names(rects[[k]])) {
        if (o != id && .tr_lay_dentro(rects[[k]][[o]], fr)) seguir(o, d)
      }
    }
  }
  for (id in names(delta)) for (g in doc$ui$grupos %||% list()) {
    m <- as.character(unlist(g))
    if (id %in% m) for (o in m) seguir(o, delta[[id]])
  }
  ops <- list()
  for (id in names(delta)) {
    d <- delta[[id]]; r <- rect(id)
    base <- if (anyNA(r[1:2])) c(0, 0) else r[1:2]
    p <- round(base + d)
    ops[[length(ops) + 1L]] <- switch(tipo(id),
      card = list(op = "move", node = id, x = p[[1]], y = p[[2]]),
      frame = list(op = "update_frame", frame = id, x = p[[1]], y = p[[2]]),
      nota = list(op = "update_note", note = id, x = p[[1]], y = p[[2]]))
    so <- .tr_lay_num(doc$ui$soltos[[id]])
    if (tipo(id) == "card" && length(so) == 4L) {
      s <- round(c(so[1:2] + d, so[3:4]))
      ops[[length(ops) + 1L]] <- list(op = "set_solto", node = id, x = s[[1]], y = s[[2]], w = s[[3]], h = s[[4]])
    }
  }
  ops
}

# Notas ----------------------------------------------------------------------

#' Altura que um markdown pede numa largura, na escala "nota": ~6,3 px por
#' caractere (medido no editor), linhas de 20 px, título maior, mais o
#' respiro do corpo. Só a
#' nascença: quem quiser outra altura passa `h`.
#' @noRd
.tr_lay_nota_h <- function(texto, w = .tr_lay$nota_w) {
  linhas <- strsplit(texto %||% "", "\n", fixed = TRUE)[[1]]
  if (!length(linhas)) linhas <- ""
  cabe <- max(10, floor((w - 28) / 6.3))
  h <- sum(vapply(linhas, function(l) {
    if (!nzchar(trimws(l))) return(10)
    titulo <- grepl("^#{1,6}\\s", l)
    n <- max(1, ceiling(nchar(l) / (if (titulo) cabe * 0.75 else cabe)))
    n * (if (titulo) 28 else 20)
  }, 0))
  max(80, ceiling(h + 28))
}

#' Ponto livre à direita de tudo o que existe, alinhado pelo topo dos frames
#' (ou de tudo, sem frames). Onde frame e nota novos nascem.
#' @noRd
.tr_lay_a_direita <- function(rects) {
  todos <- Filter(function(r) !anyNA(r), c(rects$cards, rects$frames, rects$notes))
  if (!length(todos)) return(c(0, 0))
  x <- max(vapply(todos, function(r) r[[1]] + r[[3]], 0)) + .tr_lay$frame
  topo <- if (length(rects$frames)) rects$frames else todos
  c(x, min(vapply(topo, `[[`, 0, 2)))
}

# Comandos do canal de controle --------------------------------------------
# Cada um recebe o fluxo com gravador (`fl$ops`, ver `.tr_flow_apply`) e as
# medidas do editor, e devolve o fluxo com as ops acrescentadas. O canal manda
# tudo como um batch: um passo de desfazer por gesto.

.tr_lay_aplicar <- function(fl, ops) {
  for (op in ops) fl <- .tr_flow_apply(fl, op)
  fl
}

.tr_lay_ultimo_id <- function(fl) fl$ops[[length(fl$ops)]]$id

.tr_lay_nos <- function(fl, nos) {
  nos <- as.character(unlist(nos))
  falta <- setdiff(nos, names(fl$doc$nodes))
  if (length(falta)) rlang::abort(sprintf("Nó desconhecido: '%s'.", falta[[1]]))
  nos
}

.tr_lay_frame_existe <- function(fl, frame) {
  if (!is.character(frame) || length(frame) != 1L || is.null(fl$doc$ui$frames[[frame]])) {
    rlang::abort(sprintf("Frame desconhecido: '%s'. Veja os frames em 'state'.", paste(frame, collapse = ",")))
  }
  frame
}

.tr_lay_cmd_organizar <- function(fl, medidas = NULL) {
  .tr_lay_aplicar(fl, .tr_lay_ops(fl$doc, .tr_lay_etapas(fl$doc, medidas), medidas))
}

.tr_lay_cmd_mover <- function(fl, itens, medidas = NULL) {
  if (!length(itens)) rlang::abort("Nada para mover.")
  .tr_lay_aplicar(fl, .tr_lay_mover(fl$doc, itens, medidas))
}

.tr_lay_cmd_rearrumar <- function(fl, frame, medidas = NULL, ...) {
  .tr_lay_aplicar(fl, .tr_lay_ops(fl$doc, .tr_lay_frame(fl$doc, frame, medidas, ...), medidas))
}

#' `frame add|set|rm|put|take`. Frame novo nasce à direita de tudo, com
#' proporção livre (a etapa tem o tamanho do conteúdo) e, com blocos, já os
#' traz para dentro arrumados.
#' @noRd
.tr_lay_cmd_frame <- function(fl, m, medidas = NULL) {
  acao <- m$acao %||% ""
  if (acao != "add") .tr_lay_frame_existe(fl, m$frame)
  switch(acao,
    add = {
      p <- .tr_lay_a_direita(.tr_lay_rects(fl$doc, medidas))
      nos <- .tr_lay_nos(fl, m$nodes)
      fl <- .tr_flow_apply(fl, Filter(Negate(is.null), list(
        op = "add_frame", id = m$id, x = p[[1]], y = p[[2]], w = .tr_lay$frame_w, h = .tr_lay$frame_h,
        title = m$title, color = m$color, aspect = m$aspect %||% "livre")))
      if (length(nos)) fl <- .tr_lay_cmd_rearrumar(fl, .tr_lay_ultimo_id(fl), medidas, incluir = nos)
      fl
    },
    set = {
      campos <- Filter(Negate(is.null), list(title = m$title, color = m$color, aspect = m$aspect,
                                             w = m$w, h = m$h))
      if (!length(campos) && is.null(m$order)) rlang::abort("Nada para mudar no frame.")
      if (length(campos)) fl <- .tr_flow_apply(fl, c(list(op = "update_frame", frame = m$frame), campos))
      if (!is.null(m$order)) {
        fs <- fl$doc$ui$frames
        ids <- names(fs)[order(vapply(fs, function(f) as.numeric(f$order %||% 0), 0))]
        ids <- setdiff(ids, m$frame)
        k <- min(max(1L, as.integer(m$order)), length(ids) + 1L)
        fl <- .tr_flow_apply(fl, list(op = "reorder_frames", frames = append(ids, m$frame, k - 1L)))
      }
      fl
    },
    rm = .tr_flow_apply(fl, list(op = "remove_frame", frame = m$frame)),
    put = .tr_lay_cmd_rearrumar(fl, m$frame, medidas, incluir = .tr_lay_nos(fl, m$nodes)),
    take = .tr_lay_cmd_rearrumar(fl, m$frame, medidas, excluir = .tr_lay_nos(fl, m$nodes)),
    rlang::abort(sprintf("Ação de frame desconhecida: '%s'. Use add, set, rm, put ou take.", acao)))
}

#' `note add|set|rm`. Nota presa a bloco (`node`) nasce logo abaixo dele, num
#' grupo com ele; presa a frame (`frame`), entra na abertura do frame. Nos
#' dois casos o frame se rearruma para caber.
#' @noRd
.tr_lay_cmd_nota <- function(fl, m, medidas = NULL) {
  acao <- m$acao %||% ""
  if (acao %in% c("set", "rm") &&
      (!is.character(m$note) || length(m$note) != 1L || is.null(fl$doc$ui$notes[[m$note]]))) {
    rlang::abort(sprintf("Nota desconhecida: '%s'. Veja as notas em 'state'.", paste(m$note, collapse = ",")))
  }
  dono_frame <- function(fl, id, tipo) {
    rects <- .tr_lay_rects(fl$doc, medidas)
    d <- .tr_lay_donos(fl$doc, rects)
    unname(d[[tipo]][id])
  }
  switch(acao,
    add = {
      if (!is.null(m$node) && !is.null(m$frame)) rlang::abort("Prenda a nota a um bloco OU a um frame, não aos dois.")
      texto <- m$text %||% ""
      w <- m$w %||% .tr_lay$nota_w
      h <- m$h %||% .tr_lay_nota_h(texto, w)
      rects <- .tr_lay_rects(fl$doc, medidas)
      p <- if (!is.null(m$node)) {
        no <- .tr_lay_nos(fl, m$node)
        anexos <- .tr_lay_anexos(fl$doc)
        r <- rects$cards[[no]]
        if (anyNA(r)) c(0, 0) else {
          fundo <- r[[2]] + r[[4]]
          for (n in names(anexos)[anexos == no]) fundo <- max(fundo, rects$notes[[n]][[2]] + rects$notes[[n]][[4]])
          c(r[[1]], fundo + .tr_lay$nota)
        }
      } else if (!is.null(m$frame)) {
        fr <- rects$frames[[.tr_lay_frame_existe(fl, m$frame)]]
        c(fr[[1]] + .tr_lay$pad, fr[[2]] + .tr_lay$head + .tr_lay$pad)
      } else if (!is.null(m$x) && !is.null(m$y)) c(m$x, m$y) else .tr_lay_a_direita(rects)
      fl <- .tr_flow_apply(fl, Filter(Negate(is.null), list(
        op = "add_note", id = m$id, kind = "markdown", x = p[[1]], y = p[[2]], w = w, h = h,
        text = texto, color = m$color, escala = m$escala, fundo = m$fundo)))
      id <- .tr_lay_ultimo_id(fl)
      if (!is.null(m$node)) {
        no <- m$node
        velho <- Filter(function(g) no %in% as.character(unlist(fl$doc$ui$grupos[[g]])),
                        names(fl$doc$ui$grupos %||% list()))
        membros <- c(no, id)
        gid <- NULL
        if (length(velho)) {
          gid <- velho[[1]]
          membros <- c(no, setdiff(as.character(unlist(fl$doc$ui$grupos[[gid]])), no), id)
          fl <- .tr_flow_apply(fl, list(op = "remove_grupo", grupo = gid))
        }
        fl <- .tr_flow_apply(fl, Filter(Negate(is.null), list(op = "add_grupo", id = gid, membros = membros)))
        f <- dono_frame(fl, no, "card")
        if (length(f) && !is.na(f)) fl <- .tr_lay_cmd_rearrumar(fl, f, medidas)
      } else if (!is.null(m$frame)) {
        fl <- .tr_lay_cmd_rearrumar(fl, m$frame, medidas, incluir_notas = id)
      }
      fl
    },
    set = {
      n <- fl$doc$ui$notes[[m$note]]
      campos <- Filter(Negate(is.null), list(text = m$text, color = m$color, w = m$w, h = m$h,
                                             x = m$x, y = m$y, escala = m$escala, fundo = m$fundo))
      if (!length(campos)) rlang::abort("Nada para mudar na nota.")
      if (!is.null(m$text) && is.null(m$h)) {
        campos$h <- max(as.numeric(n$h), .tr_lay_nota_h(m$text, m$w %||% as.numeric(n$w)))
      }
      fl <- .tr_flow_apply(fl, c(list(op = "update_note", note = m$note), campos))
      cresceu <- !is.null(campos$h) && campos$h != n$h || !is.null(campos$w) && campos$w != n$w
      if (cresceu && is.null(m$x)) {
        anexos <- .tr_lay_anexos(fl$doc)
        f <- if (m$note %in% names(anexos)) dono_frame(fl, anexos[[m$note]], "card") else {
          # A nota cresceu e talvez já não caiba no frame: o dono é o de antes.
          d <- .tr_lay_donos(fl$doc, .tr_lay_rects(fl$doc, medidas))
          dentro <- Filter(function(fr) {
            r <- .tr_lay_rects(fl$doc, medidas)$frames[[fr]]
            .tr_lay_dentro(c(n$x, n$y, n$w, n$h), r)
          }, d$etapas)
          if (length(dentro)) dentro[[1]] else NA
        }
        if (length(f) && !is.na(f)) {
          fl <- .tr_lay_cmd_rearrumar(fl, f, medidas, incluir_notas = if (!m$note %in% names(anexos)) m$note else character())
        }
      }
      fl
    },
    rm = .tr_flow_apply(fl, list(op = "remove_note", note = m$note)),
    rlang::abort(sprintf("Ação de nota desconhecida: '%s'. Use add, set ou rm.", acao)))
}

#' Frames e notas como o agente precisa ler: com quem está dentro de cada
#' frame e a quem cada nota está presa. Entra no `state`.
#' @noRd
.tr_lay_resumo <- function(doc, medidas = NULL) {
  rects <- .tr_lay_rects(doc, medidas)
  d <- .tr_lay_donos(doc, rects)
  anexos <- .tr_lay_anexos(doc)
  fs <- doc$ui$frames %||% list()
  ids <- names(fs)[order(vapply(fs, function(f) as.numeric(f$order %||% 0), 0))]
  frames <- lapply(ids, function(id) {
    f <- fs[[id]]
    list(id = id, title = f$title, order = f$order, color = f$color, aspect = f$aspect,
         rect = round(rects$frames[[id]]), externo = id %in% d$externos,
         nodes = as.list(names(d$card)[d$card == id]),
         notes = as.list(names(d$nota)[d$nota == id]))
  })
  notas <- lapply(names(doc$ui$notes %||% list()), function(id) {
    n <- doc$ui$notes[[id]]
    Filter(Negate(is.null), list(id = id, kind = n$kind, text = n$text, rect = round(rects$notes[[id]]),
                                 frame = if (!is.na(d$nota[id])) unname(d$nota[id]),
                                 node = if (!is.na(anexos[id])) unname(anexos[id])))
  })
  list(frames = frames, notes = notas)
}
