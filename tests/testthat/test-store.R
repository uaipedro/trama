test_that("put/get faz round-trip usando o store/restore do TIPO", {
  s <- tmp_store(); reg <- store_registry()
  ty <- tr_get_type("t/box", reg)
  h <- tr_store_put(s, "k1", list(v = 42), ty, node_type = "t/const")
  expect_true(tr_store_has(s, "k1"))
  expect_equal(tr_store_get(s, "k1", ty)$v, 42)
  expect_equal(h$summary$v, 42)
  expect_equal(h$preview$renderer, "t/box")
  expect_gt(h$bytes, 0)
})

test_that("handle é escrito por último — objeto órfão nunca vira chave válida", {
  s <- tmp_store(); reg <- store_registry()
  ty <- tr_get_type("t/box", reg)
  tr_store_put(s, "k1", list(v = 1), ty)
  unlink(file.path(s$root, "handles", "k1.json"))
  expect_false(tr_store_has(s, "k1"))   # objeto existe, mas a chave não vale
})

test_that("nada de parcial fica visível: escrita é atômica", {
  s <- tmp_store(); reg <- store_registry()
  ty <- tr_get_type("t/box", reg)
  ty$store <- function(x, path) stop("worker morreu no meio")
  expect_error(tr_store_put(s, "k1", list(v = 1), ty))
  expect_false(tr_store_has(s, "k1"))
  expect_length(list.files(file.path(s$root, "objects")), 0)
})

test_that("erro é valor: fica no store e é legível", {
  s <- tmp_store(); reg <- store_registry()
  tr_store_put_error(s, "k1", "explodiu", class = "simpleError", node_type = "t/boom")
  h <- tr_store_handle(s, "k1")
  expect_true(tr_handle_failed(h))
  expect_equal(h$error$message, "explodiu")
  expect_error(tr_store_get(s, "k1", tr_get_type("t/box", reg)), class = "tr_error_failed_key")
})

test_that("gc preserva o alcançável e remove o resto por idade", {
  s <- tmp_store(); reg <- store_registry(); ty <- tr_get_type("t/box", reg)
  tr_store_put(s, "keep", list(v = 1), ty)
  tr_store_put(s, "drop", list(v = 2), ty)
  # `drop` envelhecido à força
  h <- tr_store_handle(s, "drop"); h$created <- as.numeric(Sys.time()) - 30 * 86400
  jsonlite::write_json(h, file.path(s$root, "handles", "drop.json"), auto_unbox = TRUE)

  expect_equal(tr_store_gc(s, keep = "keep", max_age_days = 7), 1L)
  expect_true(tr_store_has(s, "keep"))
  expect_false(tr_store_has(s, "drop"))
  expect_length(list.files(file.path(s$root, "objects")), 1)
})

# --- Regressões da revisão de E3 -------------------------------------------

test_that("caminho de preview no handle é relativo, mesmo com regex no root", {
  s <- tmp_store(); reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/img", ext = "rds",
    preview = function(x, ctx) {
      p <- ctx$file("png"); writeLines("x", p); tr_preview("t/img", files = c(png = p))
    })), nodes = list()), registry = reg)
  ty <- tr_get_type("t/img", reg)
  h <- tr_store_put(s, "k1", list(1), ty)
  expect_false(grepl("^/", h$preview$files[["png"]]))
  expect_true(file.exists(file.path(s$root, h$preview$files[["png"]])))

  .tr_drop_key(s, "k1")
  expect_false(file.exists(file.path(s$root, "previews", "k1.png")))
})

# O teste acima olha o handle EM MEMÓRIA, que é onde o bug não aparecia: o
# estrago acontecia na travessia do JSON, e por isso passou despercebido até a
# primeira coleção a produzir imagem de verdade. Aqui a asserção é sobre o
# arquivo lido de volta do disco, que é o que o front recebe.
test_that("files do preview atravessa o JSON como objeto, com o nome intacto", {
  s <- tmp_store(); reg <- tr_registry()
  tr_use(tr_collection(id = "t2", types = list(tr_type("t2/img", ext = "rds",
    preview = function(x, ctx) {
      p <- ctx$file("png"); writeLines("x", p)
      tr_preview("trama/image", files = list(png = p))
    })), nodes = list()), registry = reg)
  tr_store_put(s, "k9", list(1), tr_get_type("t2/img", reg))

  h <- tr_store_handle(s, "k9")
  expect_type(h$preview$files, "list")
  expect_named(h$preview$files, "png")
  expect_match(h$preview$files$png, "\\.png$")
  expect_true(file.exists(file.path(s$root, h$preview$files$png)))
})

test_that("falha de escrita aborta em vez de deixar handle órfão", {
  s <- tmp_store(); ty <- tr_get_type("t/box", store_registry())
  ty$store <- function(x, path) invisible(NULL)   # não escreve nada
  expect_error(tr_store_put(s, "k1", list(v = 1), ty), class = "tr_error_store_write")
  expect_false(tr_store_has(s, "k1"))
})

test_that("handle sem objeto não conta como cache, e get falha classificado", {
  s <- tmp_store(); reg <- store_registry(); ty <- tr_get_type("t/box", reg)
  tr_store_put(s, "k1", list(v = 1), ty)
  unlink(file.path(s$root, "objects", "k1.rds"))
  expect_false(tr_store_has(s, "k1"))
  expect_error(tr_store_get(s, "k1", ty), class = "tr_error_missing_object")
})

test_that("handle corrompido não trava o gc nem o bust", {
  s <- tmp_store(); reg <- store_registry(); ty <- tr_get_type("t/box", reg)
  tr_store_put(s, "ok", list(v = 1), ty)
  writeLines("{ nao e json", file.path(s$root, "handles", "lixo.json"))
  expect_silent(n <- tr_store_gc(s, keep = "ok", max_age_days = 0))
  expect_gte(n, 1L)
  expect_true(tr_store_has(s, "ok"))
})

test_that("tr_bust invalida por coleção e remove handle ilegível", {
  s <- tmp_store(); reg <- store_registry(); ty <- tr_get_type("t/box", reg)
  tr_store_put(s, "a", list(v = 1), ty, node_type = "t/const")
  tr_store_put(s, "b", list(v = 2), ty, node_type = "outra/coisa")
  expect_equal(tr_bust(s, collection = "t"), 1L)
  expect_false(tr_store_has(s, "a"))
  expect_true(tr_store_has(s, "b"))
})

test_that("tr_bust casa por QUALQUER coleção do handle", {
  # A unidade que não é um nó — a região de fluxo — não tem uma coleção só, e o
  # `node_type` dela ("trama/stream_region") lê como coleção "trama". Sem o
  # conjunto no handle, `tr_bust()` pela coleção que de fato produziu o artefato
  # não o alcança, e o cache sobrevive ao upgrade que a válvula existe pra
  # invalidar.
  s <- tmp_store(); reg <- store_registry(); ty <- tr_get_type("t/box", reg)
  tr_store_put(s, "regiao", list(v = 1), ty, node_type = "trama/stream_region",
               collections = c("dados", "modelos"))
  expect_equal(tr_bust(s, collection = "trama"), 0L)
  expect_true(tr_store_has(s, "regiao"))
  # Qualquer uma das duas alcança: uma região mistura coleções, e bustar a do
  # membro tem que valer tanto quanto bustar a do colapso.
  expect_equal(tr_bust(s, collection = "modelos"), 1L)
  expect_false(tr_store_has(s, "regiao"))

  # E o handle de ERRO leva o conjunto pelo mesmo caminho.
  tr_store_put_error(s, "ruim", "explodiu", node_type = "trama/stream_region",
                     collections = c("dados", "modelos"))
  expect_equal(tr_bust(s, collection = "dados"), 1L)
})

test_that("regravar a mesma chave é idempotente", {
  s <- tmp_store(); reg <- store_registry(); ty <- tr_get_type("t/box", reg)
  tr_store_put(s, "k", list(v = 1), ty)
  h <- tr_store_put(s, "k", list(v = 1), ty)
  expect_equal(tr_store_get(s, "k", ty)$v, 1)
  expect_length(list.files(file.path(s$root, "objects")), 1)
  expect_length(list.files(file.path(s$root, "tmp")), 0)
})

test_that("schema: papéis, ordem e distintos de uma tabela", {
  sc <- trama:::.tr_df_schema(iris)
  expect_equal(vapply(sc$colunas, `[[`, "", "nome"), names(iris))
  expect_equal(vapply(sc$colunas, `[[`, "", "papel"), c(rep("numerica", 4), "categorica"))
  expect_equal(sc$colunas[[5]]$n_distintos, 3L)
  expect_false(sc$truncado); expect_false(sc$amostra)
  df <- data.frame(d = as.Date("2020-01-01") + 0:1, t = as.POSIXct("2020-01-01", tz = "UTC") + 0:1,
                   l = c(TRUE, NA))
  df$lst <- list(1, "a")
  sc <- trama:::.tr_df_schema(df)
  expect_equal(vapply(sc$colunas, `[[`, "", "papel"), c("tempo", "tempo", "categorica", "outra"))
  expect_true(sc$colunas[[3]]$tem_na)
  expect_null(trama:::.tr_df_schema(list(a = 1)))
  expect_null(trama:::.tr_df_schema(1:3))
})

test_that("schema: tetos de colunas e de linhas", {
  larga <- as.data.frame(matrix(1, nrow = 1, ncol = 501))
  sc <- trama:::.tr_df_schema(larga)
  expect_true(sc$truncado); expect_length(sc$colunas, 500)
  longa <- data.frame(x = seq_len(10001))
  sc <- trama:::.tr_df_schema(longa)
  expect_true(sc$amostra); expect_equal(sc$colunas[[1]]$n_distintos, 10000L)
})

test_that("schema vai no handle e tabela de 1 coluna sai como array no JSON", {
  s <- tmp_store()
  ty <- tr_type("t/df", version = 1L)
  tr_store_put(s, "k1", data.frame(a = 1:3), ty)
  raw <- jsonlite::read_json(file.path(s$root, "handles", "k1.json"), simplifyVector = FALSE)
  expect_type(raw$schema$colunas, "list")
  expect_null(names(raw$schema$colunas))
  expect_equal(raw$schema$colunas[[1]]$nome, "a")
  expect_equal(raw$schema$colunas[[1]]$papel, "numerica")
  expect_match(paste(readLines(file.path(s$root, "handles", "k1.json")), collapse = ""),
               '"colunas":\\[\\{', fixed = FALSE)
  h <- tr_store_put(s, "k2", list(v = 1), tr_get_type("t/box", store_registry()))
  expect_null(h$schema)
})
