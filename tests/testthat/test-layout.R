# Layout por etapas (R/layout.R): frames, notas e posição montados pelo
# servidor para o agente, sempre como ops comuns do documento. O que se trava
# aqui: pertencimento geométrico igual ao do editor, layout determinístico e
# idempotente, frame que cresce sem engolir o vizinho, nota presa que anda
# com o bloco, e um passo de desfazer por gesto.

# a, b -> s -> g (gráfico, lateral);  s -> t -> u.
fluxo_lay <- function() {
  fl <- tr_flow(test_registry())
  fl <- tr_add(fl, "a", "t/const", position = c(0, 0))
  fl <- tr_add(fl, "b", "t/const", position = c(0, 900))
  fl <- tr_add(fl, "s", "t/add", from = c("a", "b"), position = c(2000, 300))
  fl <- tr_add(fl, "g", "t/show", from = "s", position = c(5000, 0))
  fl <- tr_add(fl, "t", "t/add", from = c("s", "s"), position = c(800, 3000))
  fl <- tr_add(fl, "u", "t/show", from = "t", position = c(7000, 7000))
  fl$ops <- list()
  fl
}

aplicar_cmd <- function(fl, f, ...) f(fl, ...)
rect_card <- function(doc, id) .tr_lay_rects(doc)$cards[[id]]
rect_frame <- function(doc, id) .tr_lay_rects(doc)$frames[[id]]
no_frame <- function(doc) .tr_lay_donos(doc, .tr_lay_rects(doc))$card

etapas <- function(medidas = NULL) {
  fl <- fluxo_lay()
  fl <- .tr_lay_cmd_frame(fl, list(acao = "add", id = "f1", title = "1. Dados", nodes = list("a", "b", "s", "g")),
                          medidas)
  .tr_lay_cmd_frame(fl, list(acao = "add", id = "f2", title = "2. Modelo", nodes = list("t", "u")), medidas)
}

test_that("frame add com blocos: frame à direita de tudo, blocos inteiros dentro", {
  fl <- fluxo_lay()
  antes <- .tr_lay_rects(fl$doc)
  dir <- max(vapply(antes$cards, function(r) r[[1]] + r[[3]], 0))
  fl <- .tr_lay_cmd_frame(fl, list(acao = "add", id = "f1", title = "1. Dados", nodes = list("a", "b")))
  f <- fl$doc$ui$frames$f1
  expect_equal(f$title, "1. Dados")
  expect_equal(f$aspect, "livre")
  # Nasce à direita de tudo o que existia: não engole card nenhum por acaso.
  expect_gt(f$x, dir)
  expect_equal(unname(no_frame(fl$doc)[c("a", "b")]), c("f1", "f1"))
  expect_false("s" %in% names(no_frame(fl$doc)))
  # Ops comuns: nada que o editor não conheça.
  expect_true(all(vapply(fl$ops, `[[`, "", "op") %in% c("add_frame", "move", "update_frame")))
})

test_that("organizar: etapas lado a lado na ordem, principal em cima, lateral embaixo", {
  fl <- .tr_lay_cmd_organizar(etapas())
  d <- fl$doc
  f1 <- rect_frame(d, "f1"); f2 <- rect_frame(d, "f2")
  expect_equal(f1[[2]], f2[[2]])                       # alinhados pelo topo
  expect_lt(f1[[1]] + f1[[3]], f2[[1]])                # f1 antes de f2, sem sobrepor
  expect_equal(unname(no_frame(d)[c("a", "b", "s", "g", "t", "u")]), rep(c("f1", "f2"), c(4, 2)))
  a <- rect_card(d, "a"); b <- rect_card(d, "b"); s <- rect_card(d, "s"); g <- rect_card(d, "g")
  # Fontes empilhadas na mesma coluna; s na coluna seguinte, na altura de a.
  expect_equal(a[[1]], b[[1]])
  expect_gt(b[[2]], a[[2]] + a[[4]])
  expect_gt(s[[1]], a[[1]] + a[[3]])
  expect_equal(s[[2]], a[[2]])
  # g não leva o fluxo adiante: faixa de baixo, abaixo de todo o ramo principal.
  expect_gt(g[[2]], max(b[[2]] + b[[4]], s[[2]] + s[[4]]))
  # Em f2 não há principal (nada sai dele): faixa única, corrente reta.
  t <- rect_card(d, "t"); u <- rect_card(d, "u")
  expect_equal(t[[2]], u[[2]])
  expect_gt(u[[1]], t[[1]] + t[[3]])
})

test_that("entradas de uma etapa seguem a ordem dos pais na etapa anterior", {
  fl <- fluxo_lay()
  fl <- tr_add(fl, "p", "t/show", from = "b", position = c(9000, -500))
  fl <- tr_add(fl, "q", "t/show", from = "a", position = c(9000, 500))
  fl <- .tr_lay_cmd_frame(fl, list(acao = "add", id = "f1", title = "1", nodes = list("a", "b")))
  fl <- .tr_lay_cmd_frame(fl, list(acao = "add", id = "f2", title = "2", nodes = list("p", "q")))
  d <- .tr_lay_cmd_organizar(fl)$doc
  expect_lt(rect_card(d, "a")[[2]], rect_card(d, "b")[[2]])
  # p estava acima de q, mas o pai de q (a) vem antes do pai de p (b).
  expect_lt(rect_card(d, "q")[[2]], rect_card(d, "p")[[2]])
})

test_that("organizar é determinístico e idempotente", {
  um <- .tr_lay_cmd_organizar(etapas())
  dois <- .tr_lay_cmd_organizar(etapas())
  expect_identical(um$doc$ui$positions, dois$doc$ui$positions)
  expect_identical(um$doc$ui$frames, dois$doc$ui$frames)
  de_novo <- um; de_novo$ops <- list()
  expect_length(.tr_lay_cmd_organizar(de_novo)$ops, 0L)
})

test_that("a ordem dos frames decide a ordem das colunas", {
  fl <- etapas()
  fl <- .tr_lay_cmd_frame(fl, list(acao = "set", frame = "f2", order = 1))
  expect_equal(fl$doc$ui$frames$f2$order, 1L)
  d <- .tr_lay_cmd_organizar(fl)$doc
  expect_lt(rect_frame(d, "f2")[[1]], rect_frame(d, "f1")[[1]])
})

test_that("medida do editor vale mais que a estimativa", {
  m <- list(g = list(500, 900))
  sem <- .tr_lay_cmd_organizar(etapas())$doc
  com <- .tr_lay_cmd_organizar(etapas(m), medidas = m)$doc
  expect_gt(rect_frame(com, "f1")[[4]], rect_frame(sem, "f1")[[4]])
  expect_true(.tr_lay_dentro(c(rect_card(com, "g")[1:2], 500, 900), rect_frame(com, "f1")))
})

test_that("card que cresce depois de entrar (o gráfico chegou) continua na etapa", {
  fl <- etapas()
  # g esticou para fora da borda de f1, como um preview que chega depois.
  m <- list(g = list(900, 2500))
  expect_false(.tr_lay_dentro(c(rect_card(fl$doc, "g")[1:2], 900, 2500), rect_frame(fl$doc, "f1")))
  expect_equal(unname(.tr_lay_donos(fl$doc, .tr_lay_rects(fl$doc, m))$card["g"]), "f1")
  d <- .tr_lay_cmd_organizar(fl, medidas = m)$doc
  expect_true(.tr_lay_dentro(c(rect_card(d, "g")[1:2], 900, 2500), rect_frame(d, "f1")))
})

test_that("nota no frame abre a etapa, à esquerda dos blocos", {
  fl <- .tr_lay_cmd_nota(etapas(), list(acao = "add", id = "n1", frame = "f1",
                                        text = "## Dados\nO que se testa e por quê."))
  d <- fl$doc
  n <- d$ui$notes$n1
  expect_equal(n$kind, "markdown")
  expect_true(.tr_lay_dentro(c(n$x, n$y, n$w, n$h), rect_frame(d, "f1")))
  expect_lt(n$x + n$w, min(vapply(c("a", "b", "s"), function(id) rect_card(d, id)[[1]], 0)))
  # Continua lá depois de organizar o fluxo inteiro.
  d2 <- .tr_lay_cmd_organizar(fl)$doc
  n2 <- d2$ui$notes$n1
  expect_true(.tr_lay_dentro(c(n2$x, n2$y, n2$w, n2$h), rect_frame(d2, "f1")))
  expect_equal(.tr_lay_resumo(d2)$notes[[1]]$frame, "f1")
})

test_that("nota presa ao bloco: grupo com ele, logo abaixo, anda junto", {
  fl <- .tr_lay_cmd_nota(etapas(), list(acao = "add", id = "n2", node = "s", text = "p = 0,41"))
  d <- fl$doc
  expect_equal(unname(.tr_lay_anexos(d)["n2"]), "s")
  expect_true(any(vapply(d$ui$grupos, function(g) setequal(unlist(g), c("s", "n2")), TRUE)))
  s <- rect_card(d, "s"); n <- d$ui$notes$n2
  expect_equal(n$x, s[[1]])
  expect_gte(n$y, s[[2]] + s[[4]])
  # A etapa se rearrumou para a nota caber.
  expect_true(.tr_lay_dentro(c(n$x, n$y, n$w, n$h), rect_frame(d, "f1")))
  # Segunda nota no mesmo bloco: mesmo grupo, abaixo da primeira.
  fl <- .tr_lay_cmd_nota(fl, list(acao = "add", id = "n3", node = "s", text = "outra"))
  expect_gt(fl$doc$ui$notes$n3$y, fl$doc$ui$notes$n2$y)
  expect_length(fl$doc$ui$grupos, 1L)
  # Mover o bloco leva as notas.
  fl$ops <- list()
  fl <- .tr_lay_cmd_mover(fl, list(list(id = "s", dx = 10, dy = 20)))
  expect_equal(fl$doc$ui$notes$n2$x, n$x + 10)
  expect_equal(fl$doc$ui$notes$n2$y, n$y + 20)
  expect_setequal(vapply(fl$ops, function(o) o$node %||% o$note, ""), c("s", "n2", "n3"))
})

test_that("editar nota com texto maior faz a nota e o frame crescerem", {
  fl <- .tr_lay_cmd_nota(etapas(), list(acao = "add", id = "n1", frame = "f1", text = "curta"))
  h0 <- fl$doc$ui$notes$n1$h
  longo <- paste(rep("Uma linha comprida de explicação sobre o teste.", 12), collapse = "\n")
  fl <- .tr_lay_cmd_nota(fl, list(acao = "set", note = "n1", text = longo))
  n <- fl$doc$ui$notes$n1
  expect_gt(n$h, h0)
  expect_true(.tr_lay_dentro(c(n$x, n$y, n$w, n$h), rect_frame(fl$doc, "f1")))
  expect_error(.tr_lay_cmd_nota(fl, list(acao = "set", note = "nada", text = "x")), "desconhecida")
})

test_that("mover frame leva o que está dentro; absoluto e relativo", {
  fl <- etapas(); fl$ops <- list()
  f <- rect_frame(fl$doc, "f2"); t <- rect_card(fl$doc, "t")
  fl <- .tr_lay_cmd_mover(fl, list(list(id = "f2", x = f[[1]] + 100, y = f[[2]] - 50)))
  expect_equal(rect_card(fl$doc, "t")[1:2], t[1:2] + c(100, -50))
  expect_equal(unname(no_frame(fl$doc)[c("t", "u")]), c("f2", "f2"))
  fl <- .tr_lay_cmd_mover(fl, list(list(id = "a", x = 5, y = 6)))
  expect_equal(rect_card(fl$doc, "a")[1:2], c(5, 6))
  expect_error(.tr_lay_cmd_mover(fl, list(list(id = "zz", x = 1, y = 1))), "zz")
  expect_error(.tr_lay_cmd_mover(fl, list(list(id = "a", x = 1))), "x e y")
})

test_that("frame que cresce abre espaço: o vizinho anda com o que tem dentro", {
  fl <- etapas()
  f1 <- rect_frame(fl$doc, "f1"); f2 <- rect_frame(fl$doc, "f2"); t <- rect_card(fl$doc, "t")
  expect_lt(f1[[1]], f2[[1]])
  # Põe em f1 um card novo bem largo: f1 cresce para a direita.
  fl <- tr_add(fl, "w", "t/const", position = c(-5000, -5000))
  doc <- fl$doc; doc$ui$sizes$w <- c(2000, 132); fl$doc <- doc
  fl <- .tr_lay_cmd_frame(fl, list(acao = "put", frame = "f1", nodes = list("w")))
  g1 <- rect_frame(fl$doc, "f1"); g2 <- rect_frame(fl$doc, "f2")
  expect_gt(g1[[3]], f1[[3]])
  expect_lt(g1[[1]] + g1[[3]], g2[[1]])
  expect_equal(rect_card(fl$doc, "t")[[1]] - t[[1]], g2[[1]] - f2[[1]])
  expect_equal(unname(no_frame(fl$doc)[c("w", "t", "u")]), c("f1", "f2", "f2"))
})

test_that("frame take tira o bloco e ele fica fora, abaixo", {
  fl <- .tr_lay_cmd_frame(etapas(), list(acao = "take", frame = "f1", nodes = list("g")))
  expect_false("g" %in% names(no_frame(fl$doc)))
  expect_gt(rect_card(fl$doc, "g")[[2]], sum(rect_frame(fl$doc, "f1")[c(2, 4)]))
  # E organizar põe os soltos numa faixa abaixo das etapas.
  d <- .tr_lay_cmd_organizar(fl)$doc
  base <- max(vapply(c("f1", "f2"), function(f) sum(rect_frame(d, f)[c(2, 4)]), 0))
  expect_gt(rect_card(d, "g")[[2]], base)
})

test_that("recusas claras: frame, nó, ação", {
  fl <- etapas()
  expect_error(.tr_lay_cmd_frame(fl, list(acao = "put", frame = "nada", nodes = list("a"))), "Frame desconhecido")
  expect_error(.tr_lay_cmd_frame(fl, list(acao = "put", frame = "f1", nodes = list("zz"))), "zz")
  expect_error(.tr_lay_cmd_frame(fl, list(acao = "voar", frame = "f1")), "desconhecida")
  expect_error(.tr_lay_cmd_nota(fl, list(acao = "add", text = "x", node = "a", frame = "f1")), "OU")
  expect_error(.tr_lay_cmd_frame(fl, list(acao = "set", frame = "f1")), "Nada")
})

test_that("prancheta: externo não vira etapa e volta a envolver as células", {
  fl <- etapas()
  f1 <- rect_frame(fl$doc, "f1"); f2 <- rect_frame(fl$doc, "f2")
  x0 <- f1[[1]] - 100; y0 <- f1[[2]] - 200
  fl <- .tr_flow_apply(fl, list(op = "add_frame", id = "ext", x = x0, y = y0,
                                w = f2[[1]] + f2[[3]] + 100 - x0, h = 6000, aspect = "livre"))
  d <- .tr_lay_cmd_organizar(fl)$doc
  expect_true("ext" %in% .tr_lay_donos(d, .tr_lay_rects(d))$externos)
  e <- rect_frame(d, "ext")
  expect_true(.tr_lay_dentro(rect_frame(d, "f1"), e))
  expect_true(.tr_lay_dentro(rect_frame(d, "f2"), e))
})

test_that("resumo para o state: frames na ordem com blocos e notas", {
  fl <- .tr_lay_cmd_nota(etapas(), list(acao = "add", id = "n1", frame = "f2", text = "abre"))
  r <- .tr_lay_resumo(fl$doc)
  expect_equal(vapply(r$frames, `[[`, "", "id"), c("f1", "f2"))
  expect_setequal(unlist(r$frames[[1]]$nodes), c("a", "b", "s", "g"))
  expect_equal(unlist(r$frames[[2]]$notes), "n1")
  expect_equal(r$notes[[1]]$text, "abre")
})

test_that("altura da nota sai do texto", {
  expect_gte(.tr_lay_nota_h(""), 80)
  expect_gt(.tr_lay_nota_h(paste(rep("palavra", 200), collapse = " ")), .tr_lay_nota_h("curta"))
  expect_gt(.tr_lay_nota_h("# Título\nlinha\nlinha"), .tr_lay_nota_h("linha\nlinha"))
})
