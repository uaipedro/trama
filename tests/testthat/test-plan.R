chain <- function(reg) build(reg, list(
  list(op = "add_node", type = "t/const", id = "c", params = list(v = 10)),
  list(op = "add_node", type = "t/inc",   id = "i", params = list(by = 5)),
  list(op = "connect", from_node = "c", from_port = "out", to_node = "i", to_port = "x")
))

test_that("plano é pull-based: só o fecho do alvo entra", {
  reg <- store_registry()
  doc <- chain(reg)
  doc <- tr_doc_apply(doc, list(op = "add_node", type = "t/const", id = "solto"), reg)

  p <- tr_plan(doc, targets = "i", registry = reg)
  expect_setequal(names(p$units), c("c", "i"))   # `solto` fora do caminho
  expect_equal(p$targets, "i")

  # Sem alvo explícito, os terminais: `i` e `solto`.
  expect_setequal(names(tr_plan(doc, registry = reg)$units), c("c", "i", "solto"))
})

test_that("ordem é topológica: input antes do consumidor", {
  reg <- store_registry()
  p <- tr_plan(chain(reg), registry = reg)
  expect_lt(which(names(p$units) == "c"), which(names(p$units) == "i"))
})

test_that("mudar param muda a chave só do nó e do que depende dele", {
  reg <- store_registry()
  doc <- chain(reg)
  a <- tr_plan(doc, registry = reg)
  b <- tr_plan(tr_doc_apply(doc, list(op = "set_param", node = "i", name = "by", value = 6), reg),
               registry = reg)
  expect_equal(a$keys$c, b$keys$c)     # upstream intocado
  expect_false(identical(a$keys$i, b$keys$i))

  # E mudar o upstream muda os dois.
  cc <- tr_plan(tr_doc_apply(doc, list(op = "set_param", node = "c", name = "v", value = 11), reg),
                registry = reg)
  expect_false(identical(a$keys$c, cc$keys$c))
  expect_false(identical(a$keys$i, cc$keys$i))
})

test_that("renomear e mover NÃO mudam chave nenhuma", {
  reg <- store_registry()
  doc <- chain(reg)
  a <- tr_plan(doc, registry = reg)
  doc <- tr_doc_apply(doc, list(op = "rename", node = "i", label = "Outro nome"), reg)
  doc <- tr_doc_apply(doc, list(op = "move", node = "i", x = 99, y = 99), reg)
  expect_equal(tr_plan(doc, registry = reg)$keys, a$keys)
})

test_that("seed só entra na chave de nó estocástico", {
  reg <- store_registry()
  doc <- chain(reg)
  a <- tr_plan(doc, registry = reg)
  doc2 <- tr_doc_apply(doc, list(op = "set_seed", node = "c", value = 999L), reg)
  expect_equal(tr_plan(doc2, registry = reg)$keys$c, a$keys$c)  # t/const não é estocástico
})

test_that("chave é estável através de salvar e reabrir", {
  reg <- store_registry()
  doc <- chain(reg)
  a <- tr_plan(doc, registry = reg)
  b <- tr_plan(tr_doc_parse(tr_doc_json(doc)), registry = reg)
  expect_equal(a$keys, b$keys)
})

test_that("porta variádica: ordem vem do index, não da ordem de edição", {
  reg <- store_registry()
  mk <- function(order_ab) {
    doc <- build(reg, list(
      list(op = "add_node", type = "t/const", id = "a", params = list(v = 1)),
      list(op = "add_node", type = "t/const", id = "b", params = list(v = 2)),
      list(op = "add_node", type = "t/sum",   id = "s")))
    for (nd in order_ab) {
      doc <- tr_doc_apply(doc, list(op = "connect", from_node = nd, from_port = "out",
                                    to_node = "s", to_port = "xs",
                                    index = if (nd == "a") 1L else 2L), reg)
    }
    doc
  }
  # Mesmo `index`, ordem de edição invertida -> mesma chave.
  expect_equal(tr_plan(mk(c("a", "b")), registry = reg)$keys$s,
               tr_plan(mk(c("b", "a")), registry = reg)$keys$s)

  # Trocar os índices -> chave diferente (a ordem importa de verdade).
  doc <- mk(c("a", "b"))
  doc$edges[[1]]$index <- 2L; doc$edges[[2]]$index <- 1L
  expect_false(identical(tr_plan(doc, registry = reg)$keys$s,
                         tr_plan(mk(c("a", "b")), registry = reg)$keys$s))

  # E a ordem chega nos inputs do plano.
  u <- tr_plan(mk(c("a", "b")), registry = reg)$units$s
  expect_equal(vapply(u$inputs$xs, function(r) r$node, ""), c("a", "b"))
})

test_that("cache hit é decidido no plano, sem tocar em worker", {
  reg <- store_registry(); s <- tmp_store()
  doc <- chain(reg)
  p1 <- tr_plan(doc, registry = reg, store = s)
  expect_length(tr_plan_pending(p1), 2)

  tr_store_put(s, p1$units$c$outputs$out, list(v = 10), tr_get_type("t/box", reg), node_type = "t/const")
  p2 <- tr_plan(doc, registry = reg, store = s)
  expect_true(p2$units$c$cached)
  expect_false(p2$units$i$cached)
  expect_equal(unname(vapply(tr_plan_pending(p2), function(u) u$node, "")), "i")
})

test_that("erro a montante bloqueia o jusante em vez de reexecutar o mundo", {
  reg <- store_registry(); s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/const", id = "c"),
    list(op = "add_node", type = "t/boom",  id = "b"),
    list(op = "add_node", type = "t/inc",   id = "i"),
    list(op = "connect", from_node = "c", from_port = "out", to_node = "b", to_port = "x"),
    list(op = "connect", from_node = "b", from_port = "out", to_node = "i", to_port = "x")))
  p <- tr_plan(doc, registry = reg, store = s)
  tr_store_put_error(s, p$units$b$outputs$out, "explodiu", node_type = "t/boom")

  p2 <- tr_plan(doc, registry = reg, store = s)
  expect_true(p2$units$b$failed)
  expect_equal(p2$units$i$blocked_by, "b")
  expect_equal(unname(vapply(tr_plan_pending(p2), function(u) u$node, "")), "c")
  expect_setequal(vapply(tr_plan_blocked(p2), function(u) u$node, ""), c("b", "i"))
})

test_that("nó impuro leva o fingerprint externo na chave", {
  reg <- store_registry()
  f <- tempfile(); writeLines("1", f)
  doc <- build(reg, list(list(op = "add_node", type = "t/read", id = "r",
                              params = list(path = f))))
  k1 <- tr_plan(doc, registry = reg)$keys$r
  Sys.sleep(1.1); writeLines("2", f)          # mesmo caminho, conteúdo novo
  expect_false(identical(tr_plan(doc, registry = reg)$keys$r, k1))
})

test_that("nó volátil nunca repete chave", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/box")),
                       nodes = list(tr_node("t/now", fn = function() Sys.time(),
                                            description = "Devolve o horário atual.",
                                            outputs = list(out = "t/box"), volatile = TRUE))),
         registry = reg)
  doc <- build(reg, list(list(op = "add_node", type = "t/now", id = "n")))
  expect_false(identical(tr_plan(doc, registry = reg)$keys$n,
                         tr_plan(doc, registry = reg)$keys$n))
})

test_that("adaptador entra na chave do consumidor", {
  mk_reg <- function(fn) {
    r <- tr_registry()
    tr_use(tr_collection(id = "t",
      types = list(tr_type("t/a"), tr_type("t/b")),
      nodes = list(tr_node("t/src", fn = function() 1, description = "Devolve o número um.",
                           outputs = list(out = "t/a")),
                   tr_node("t/dst", fn = function(x) x, inputs = list(x = "t/b"),
                           description = "Repassa a entrada, que precisa chegar no outro tipo.",
                           outputs = list(out = "t/b"))),
      adapters = list(tr_adapter("t/a", "t/b", fn))), registry = r)
    r
  }
  doc_for <- function(r) build(r, list(
    list(op = "add_node", type = "t/src", id = "s"),
    list(op = "add_node", type = "t/dst", id = "d"),
    list(op = "connect", from_node = "s", from_port = "out", to_node = "d", to_port = "x")))

  r1 <- mk_reg(function(x) x * 1); r2 <- mk_reg(function(x) x * 2)
  k1 <- tr_plan(doc_for(r1), registry = r1)$keys
  k2 <- tr_plan(doc_for(r2), registry = r2)$keys
  expect_equal(k1$s, k2$s)                       # a origem não muda
  expect_false(identical(k1$d, k2$d))            # o consumidor sim
  expect_equal(tr_plan(doc_for(r1), registry = r1)$units$d$inputs$x$adapter$from, "t/a")
})

test_that("tr_plan_keys dá o conjunto keep do gc", {
  reg <- store_registry()
  p <- tr_plan(chain(reg), registry = reg)
  expect_setequal(tr_plan_keys(p), c(p$units$c$outputs$out, p$units$i$outputs$out))
})

# --- Regressões da revisão de E3 -------------------------------------------

test_that("porta de origem entra na chave: duas saídas não colidem", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/box")),
    nodes = list(
      tr_node("t/split", fn = function() list(a = 1, b = 2),
              description = "Devolve um número em cada uma das duas saídas.",
              outputs = list(a = "t/box", b = "t/box")),
      tr_node("t/take", fn = function(x) x, inputs = list(x = "t/box"),
              description = "Repassa a caixa que recebe.",
              outputs = list(out = "t/box")))), registry = reg)
  mk <- function(port) build(reg, list(
    list(op = "add_node", type = "t/split", id = "s"),
    list(op = "add_node", type = "t/take",  id = "k"),
    list(op = "connect", from_node = "s", from_port = port, to_node = "k", to_port = "x")))
  ka <- tr_plan(mk("a"), registry = reg)$units$k$key
  kb <- tr_plan(mk("b"), registry = reg)$units$k$key
  expect_false(identical(ka, kb))

  # E cada saída tem artefato próprio no store.
  u <- tr_plan(mk("a"), registry = reg)$units$s
  expect_setequal(names(u$outputs), c("a", "b"))
  expect_false(identical(u$outputs$a, u$outputs$b))
})

test_that("chave não depende da locale", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/box")),
    nodes = list(tr_node("t/p", fn = function(B, a) B, outputs = list(out = "t/box"),
                         description = "Devolve o param B e ignora o a.",
                         params = list(B = tr_param_num(1), a = tr_param_num(2))))), registry = reg)
  doc <- build(reg, list(list(op = "add_node", type = "t/p", id = "n")))
  k1 <- withr::with_collate("C", tr_plan(doc, registry = reg)$keys$n)
  k2 <- withr::with_collate("en_US.UTF-8", tr_plan(doc, registry = reg)$keys$n)
  expect_equal(k1, k2)
})

test_that("mudar o preview do TIPO invalida a chave", {
  mk_reg <- function(prev) {
    r <- tr_registry()
    tr_use(tr_collection(id = "t",
      types = list(tr_type("t/box", preview = prev)),
      nodes = list(tr_node("t/c", fn = function() 1, description = "Devolve a constante um.",
                           outputs = list(out = "t/box")))),
      registry = r)
    r
  }
  r1 <- mk_reg(function(x, ctx) tr_preview("t/box", data = list(cor = "vermelho")))
  r2 <- mk_reg(function(x, ctx) tr_preview("t/box", data = list(cor = "azul")))
  d <- function(r) build(r, list(list(op = "add_node", type = "t/c", id = "n")))
  expect_false(identical(tr_plan(d(r1), registry = r1)$keys$n,
                         tr_plan(d(r2), registry = r2)$keys$n))
})

test_that("param não-escalar tem chave estável através de salvar e reabrir", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/box")),
    nodes = list(tr_node("t/sel", fn = function(cols) cols, outputs = list(out = "t/box"),
                         description = "Devolve as colunas escolhidas no param.",
                         params = list(cols = tr_param("cols", c("x", "y")))))), registry = reg)
  # Como chega do cliente: array JSON com simplifyVector=FALSE, ou seja lista.
  doc <- build(reg, list(list(op = "add_node", type = "t/sel", id = "n",
                              params = list(cols = list("x", "y")))))
  expect_equal(tr_plan(doc, registry = reg)$keys$n,
               tr_plan(tr_doc_parse(tr_doc_json(doc)), registry = reg)$keys$n)
})

test_that("porta obrigatória solta invalida a unidade em vez de enfileirá-la", {
  reg <- store_registry()
  doc <- build(reg, list(list(op = "add_node", type = "t/inc", id = "i")))
  p <- tr_plan(doc, registry = reg)
  expect_equal(p$units$i$invalid, "missing_required_input:x")
  expect_length(tr_plan_pending(p), 0)
  expect_length(tr_plan_blocked(p), 1)
})

test_that("porta opcional solta não invalida — o fn cai no próprio default", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/box")),
    nodes = list(tr_node("t/opt", fn = function(x = NULL) x, outputs = list(out = "t/box"),
                         description = "Repassa a entrada opcional, ou NULL se ela estiver solta.",
                         inputs = list(x = tr_port("t/box", required = FALSE))))), registry = reg)
  p <- tr_plan(build(reg, list(list(op = "add_node", type = "t/opt", id = "o"))), registry = reg)
  expect_length(p$units$o$invalid, 0)
  expect_length(tr_plan_pending(p), 1)
})

test_that("plano é serializável: não carrega fn nem environment", {
  reg <- store_registry()
  u <- tr_plan(chain(reg), registry = reg)$units$i
  expect_null(u$spec)
  expect_equal(u$node_type, "t/inc")
  expect_false(u$wants_ctx)
  expect_type(jsonlite::toJSON(u$outputs, auto_unbox = TRUE), "character")
})

test_that("fingerprint é memoizado por coleção e zerado ao recarregar", {
  reg <- store_registry()
  doc <- chain(reg)
  invisible(tr_plan(doc, registry = reg))
  t2 <- system.time(for (i in 1:20) tr_plan(doc, registry = reg))[["elapsed"]]
  expect_lt(t2, 2)
  expect_gt(length(ls(reg$prints)), 0)
})
