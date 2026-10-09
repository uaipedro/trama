# Galeria: itens, sincronização e captura. A coleção é local a este arquivo:
# um tipo de imagem (renderer `trama/image`, que re-renderiza em alta quando
# recebe `ctx$qualidade`), um de tabela (outro renderer) e um de imagem que
# falha só na exportação, para provar que um erro não derruba os outros.

galeria_setup <- function() {
  log <- new.env(parent = emptyenv()); log$qualidade <- list()
  img <- tr_type("g/img", label = "Imagem", preview = function(x, ctx) {
    if (!is.null(ctx$qualidade)) {
      log$qualidade[[length(log$qualidade) + 1L]] <- ctx$qualidade
      if (isTRUE(x$falha)) stop("sem memória para a alta qualidade")
    }
    f <- ctx$file("png")
    writeBin(c(as.raw(c(0x89, 0x50, 0x4e, 0x47)), charToRaw(paste0("v=", x$v))), f)
    tr_preview("trama/image", files = list(png = f))
  })
  tab <- tr_type("g/tab", label = "Tabela",
                 preview = function(x, ctx) tr_preview("trama/tabela", data = list(n = x$n)))
  reg <- test_registry()
  tr_use(tr_collection(
    id = "g", version = "1.0.0", label = "Galeria de teste",
    types = list(img, tab),
    nodes = list(
      tr_node("g/const", fn = function(v) list(v = v, falha = FALSE),
              description = "Imagem de teste.", outputs = list(out = "g/img"),
              params = list(v = tr_param_num(1))),
      tr_node("g/fragil", fn = function(v) list(v = v, falha = TRUE),
              description = "Imagem que não exporta.", outputs = list(out = "g/img"),
              params = list(v = tr_param_num(1))),
      tr_node("g/tabela", fn = function(n) list(n = n), description = "Tabela de teste.",
              outputs = list(out = "g/tab"), params = list(n = tr_param_num(1))),
      tr_node("g/puro", fn = function(x) x, description = "Passa a imagem adiante.",
              inputs = list(x = "g/img"), outputs = list(out = "g/img"))
    )
  ), registry = reg)
  list(reg = reg, log = log)
}

# Documento com os cards dados, e handles gravados de verdade no store
# (sem executor): é o que `tr_store_put` deixa pronto para a galeria.
galeria_doc <- function(reg, store, cards, conexoes = list()) {
  doc <- tr_doc()
  handles <- list()
  for (cd in cards) {
    doc <- tr_doc_apply(doc, c(list(op = "add_node", type = cd$type, id = cd$id),
                               cd[setdiff(names(cd), c("type", "id", "value", "key"))]), reg)
    if (!is.null(cd$key)) {
      handles[[cd$id]] <- tr_store_put(store, cd$key, cd$value, tr_get_type(cd$tipo, reg),
                                       node_type = cd$type)
    }
  }
  for (cx in conexoes) doc <- tr_doc_apply(doc, c(list(op = "connect"), cx), reg)
  list(doc = doc, handles = handles)
}

galeria_png_b64 <- function() {
  jsonlite::base64_enc(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0, 0, 0, 0)))
}

test_that("imagem entra por padrão; tabela não", {
  root <- withr::local_tempdir(); store <- tmp_store(); s <- galeria_setup()
  m <- galeria_doc(s$reg, store, list(
    list(type = "g/const", id = "a", label = "Barras", v = 1, key = "ka", tipo = "g/img", value = list(v = 1)),
    list(type = "g/tabela", id = "t", label = "Resumo", n = 3, key = "kt", tipo = "g/tab", value = list(n = 3))))
  itens <- tr_galeria_itens(root, store, s$reg, m$doc, .tr_settings(list()), m$handles)
  expect_equal(vapply(itens, `[[`, "", "node"), "a")
  expect_equal(itens[[1]]$tipo, "imagem")
  expect_equal(itens[[1]]$arquivo, "barras.png")
})

test_that("FALSE tira a imagem da galeria", {
  root <- withr::local_tempdir(); store <- tmp_store(); s <- galeria_setup()
  m <- galeria_doc(s$reg, store, list(
    list(type = "g/const", id = "a", label = "Barras", key = "ka", tipo = "g/img", value = list(v = 1))))
  m$doc <- tr_doc_apply(m$doc, list(op = "set_galeria", node = "a", valor = FALSE), s$reg)
  expect_length(tr_galeria_itens(root, store, s$reg, m$doc, .tr_settings(list()), m$handles), 0L)
})

test_that("TRUE põe a tabela como captura e sem arquivo ela não fica pronta", {
  root <- withr::local_tempdir(); store <- tmp_store(); s <- galeria_setup()
  m <- galeria_doc(s$reg, store, list(
    list(type = "g/tabela", id = "t", label = "Resumo", n = 3, key = "kt", tipo = "g/tab", value = list(n = 3))))
  m$doc <- tr_doc_apply(m$doc, list(op = "set_galeria", node = "t", valor = TRUE), s$reg)
  itens <- tr_galeria_sincronizar(root, store, s$reg, m$doc, .tr_settings(list()), m$handles)
  expect_equal(itens[[1]]$tipo, "captura")
  expect_false(itens[[1]]$pronto)
  expect_false(file.exists(file.path(root, "gallery", "resumo.png")))
})

test_that("a ordem é topológica pelas arestas; empate pela posição x", {
  root <- withr::local_tempdir(); store <- tmp_store(); s <- galeria_setup()
  # `p` tem x menor que `c`, mas recebe a aresta de `c`: `p` só entra depois.
  # Entre os prontos (`c`, `z`), o de menor x sai primeiro.
  m <- galeria_doc(s$reg, store, list(
    list(type = "g/const", id = "c", label = "Origem", key = "kc", tipo = "g/img", value = list(v = 1), position = c(50, 0)),
    list(type = "g/puro", id = "p", label = "Destino", key = "kp", tipo = "g/img", value = list(v = 1), position = c(0, 0)),
    list(type = "g/const", id = "z", label = "Solto", key = "kz", tipo = "g/img", value = list(v = 1), position = c(20, 0))),
    conexoes = list(list(from_node = "c", from_port = "out", to_node = "p", to_port = "x")))
  itens <- tr_galeria_itens(root, store, s$reg, m$doc, .tr_settings(list()), m$handles)
  expect_equal(vapply(itens, `[[`, "", "node"), c("z", "c", "p"))
})

test_that("dpi e formato do settings chegam ao ctx$qualidade", {
  root <- withr::local_tempdir(); store <- tmp_store(); s <- galeria_setup()
  m <- galeria_doc(s$reg, store, list(
    list(type = "g/const", id = "a", label = "Barras", key = "ka", tipo = "g/img", value = list(v = 1))))
  ajuste <- .tr_settings(list(galeria = list(dpi = 150, formato = "png")))
  tr_galeria_sincronizar(root, store, s$reg, m$doc, ajuste, m$handles)
  q <- s$log$qualidade[[length(s$log$qualidade)]]
  expect_equal(q$dpi, 150)
  expect_equal(q$formato, "png")
})

test_that("sem mudança, rodar de novo não reescreve o arquivo", {
  root <- withr::local_tempdir(); store <- tmp_store(); s <- galeria_setup()
  m <- galeria_doc(s$reg, store, list(
    list(type = "g/const", id = "a", label = "Barras", key = "ka", tipo = "g/img", value = list(v = 1))))
  ajuste <- .tr_settings(list())
  p <- file.path(root, "gallery", "barras.png")
  it1 <- tr_galeria_sincronizar(root, store, s$reg, m$doc, ajuste, m$handles)
  mt1 <- file.mtime(p)
  Sys.sleep(1.1)
  it2 <- tr_galeria_sincronizar(root, store, s$reg, m$doc, ajuste, m$handles)
  expect_true(it1[[1]]$pronto)
  expect_true(it2[[1]]$pronto)
  expect_equal(file.mtime(p), mt1)
})

test_that("mudar a key reescreve o MESMO nome", {
  root <- withr::local_tempdir(); store <- tmp_store(); s <- galeria_setup()
  ajuste <- .tr_settings(list())
  m <- galeria_doc(s$reg, store, list(
    list(type = "g/const", id = "a", label = "Barras", key = "ka", tipo = "g/img", value = list(v = 1))))
  it1 <- tr_galeria_sincronizar(root, store, s$reg, m$doc, ajuste, m$handles)
  m2 <- galeria_doc(s$reg, store, list(
    list(type = "g/const", id = "a", label = "Barras", key = "ka2", tipo = "g/img", value = list(v = 2))))
  it2 <- tr_galeria_sincronizar(root, store, s$reg, m2$doc, ajuste, m2$handles)
  p <- file.path(root, "gallery", "barras.png")
  expect_equal(it2[[1]]$arquivo, it1[[1]]$arquivo)
  expect_false(identical(it2[[1]]$versao, it1[[1]]$versao))
  conteudo <- readBin(p, "raw", file.size(p))
  expect_match(rawToChar(conteudo[-(1:4)]), "v=2", fixed = TRUE)
  expect_equal(list.files(file.path(root, "gallery"), all.files = TRUE, no.. = TRUE),
               c(".galeria.json", "barras.png"))
})

test_that("desmarcar remove o arquivo do manifesto e preserva arquivo alheio", {
  root <- withr::local_tempdir(); store <- tmp_store(); s <- galeria_setup()
  ajuste <- .tr_settings(list())
  m <- galeria_doc(s$reg, store, list(
    list(type = "g/const", id = "a", label = "Barras", key = "ka", tipo = "g/img", value = list(v = 1))))
  tr_galeria_sincronizar(root, store, s$reg, m$doc, ajuste, m$handles)
  alheio <- file.path(root, "gallery", "notas.txt")
  writeLines("do usuário", alheio)
  m$doc <- tr_doc_apply(m$doc, list(op = "set_galeria", node = "a", valor = FALSE), s$reg)
  tr_galeria_sincronizar(root, store, s$reg, m$doc, ajuste, m$handles)
  expect_false(file.exists(file.path(root, "gallery", "barras.png")))
  expect_true(file.exists(alheio))
  expect_equal(readLines(alheio), "do usuário")
})

test_that("nome que colide com arquivo alheio cede lugar ao sufixo do id", {
  root <- withr::local_tempdir(); store <- tmp_store(); s <- galeria_setup()
  dir.create(file.path(root, "gallery"), recursive = TRUE)
  writeLines("alheio", file.path(root, "gallery", "barras.png"))
  m <- galeria_doc(s$reg, store, list(
    list(type = "g/const", id = "a", label = "Barras", key = "ka", tipo = "g/img", value = list(v = 1))))
  it <- tr_galeria_sincronizar(root, store, s$reg, m$doc, .tr_settings(list()), m$handles)
  expect_equal(it[[1]]$arquivo, "barras-a.png")
  expect_equal(readLines(file.path(root, "gallery", "barras.png")), "alheio")
})

test_that("erro de exportação de um item não derruba os outros", {
  root <- withr::local_tempdir(); store <- tmp_store(); s <- galeria_setup()
  m <- galeria_doc(s$reg, store, list(
    list(type = "g/fragil", id = "f", label = "Quebra", v = 1, key = "kf", tipo = "g/img", value = list(v = 1, falha = TRUE)),
    list(type = "g/const", id = "a", label = "Barras", key = "ka", tipo = "g/img", value = list(v = 1))))
  itens <- tr_galeria_sincronizar(root, store, s$reg, m$doc, .tr_settings(list()), m$handles)
  por_no <- stats::setNames(itens, vapply(itens, `[[`, "", "node"))
  expect_false(por_no$f$pronto)
  expect_match(por_no$f$erro, "sem memória")
  expect_true(por_no$a$pronto)
  expect_true(file.exists(file.path(root, "gallery", "barras.png")))
})

test_that("captura grava o PNG e fica pronta; imagem não aceita captura", {
  root <- withr::local_tempdir(); store <- tmp_store(); s <- galeria_setup()
  ajuste <- .tr_settings(list())
  m <- galeria_doc(s$reg, store, list(
    list(type = "g/tabela", id = "t", label = "Resumo", n = 3, key = "kt", tipo = "g/tab", value = list(n = 3)),
    list(type = "g/const", id = "a", label = "Barras", key = "ka", tipo = "g/img", value = list(v = 1))))
  m$doc <- tr_doc_apply(m$doc, list(op = "set_galeria", node = "t", valor = TRUE), s$reg)
  tr_galeria_sincronizar(root, store, s$reg, m$doc, ajuste, m$handles)
  item <- tr_galeria_captura(root, store, s$reg, m$doc, ajuste, m$handles, node = "t", key = "kt",
                             png = paste0("data:image/png;base64,", galeria_png_b64()))
  expect_true(item$pronto)
  p <- file.path(root, "gallery", "resumo.png")
  expect_true(file.exists(p))
  expect_equal(readBin(p, "raw", 4), as.raw(c(0x89, 0x50, 0x4e, 0x47)))
  # Próxima sincronização mantém a captura (não é reescrita nem sai).
  mt <- file.mtime(p)
  it <- tr_galeria_sincronizar(root, store, s$reg, m$doc, ajuste, m$handles)
  por_no <- stats::setNames(it, vapply(it, `[[`, "", "node"))
  expect_true(por_no$t$pronto)
  expect_equal(file.mtime(p), mt)
  # Resultado novo: a captura velha fica no disco, mas deixa de estar pronta.
  h2 <- m$handles; h2$t$key <- "kt2"
  it <- tr_galeria_sincronizar(root, store, s$reg, m$doc, ajuste, h2)
  por_no <- stats::setNames(it, vapply(it, `[[`, "", "node"))
  expect_false(por_no$t$pronto)
  expect_true(file.exists(p))
  expect_error(tr_galeria_captura(root, store, s$reg, m$doc, ajuste, m$handles, node = "a", key = "ka",
                                  png = galeria_png_b64()), class = "tr_error_galeria")
  expect_error(tr_galeria_captura(root, store, s$reg, m$doc, ajuste, m$handles, node = "t", key = "kt",
                                  png = jsonlite::base64_enc(charToRaw("nao e png"))),
               class = "tr_error_galeria")
})

test_that("nome é estável: card que chega depois com o mesmo rótulo leva o sufixo", {
  root <- withr::local_tempdir(); store <- tmp_store(); s <- galeria_setup()
  ajuste <- .tr_settings(list())
  um <- list(type = "g/const", id = "a", label = "Barras", key = "ka", tipo = "g/img", value = list(v = 1))
  m1 <- galeria_doc(s$reg, store, list(um))
  tr_galeria_sincronizar(root, store, s$reg, m1$doc, ajuste, m1$handles)
  expect_true(file.exists(file.path(root, "gallery", "barras.png")))
  # A cópia (ramo bifurcado) entra com o mesmo rótulo; o original não muda de nome.
  m2 <- galeria_doc(s$reg, store, list(
    list(type = "g/const", id = "b", label = "Barras", key = "kb", tipo = "g/img", value = list(v = 2)), um))
  it <- tr_galeria_sincronizar(root, store, s$reg, m2$doc, ajuste, m2$handles)
  por_no <- stats::setNames(it, vapply(it, `[[`, "", "node"))
  expect_equal(por_no$a$arquivo, "barras.png")
  expect_equal(por_no$b$arquivo, "barras-b.png")
})

test_that("formato jpeg não re-renderiza a cada sincronização; renomear só renomeia", {
  root <- withr::local_tempdir(); store <- tmp_store(); s <- galeria_setup()
  ajuste <- .tr_settings(list(galeria = list(formato = "jpeg")))
  m <- galeria_doc(s$reg, store, list(
    list(type = "g/const", id = "a", label = "Barras", key = "ka", tipo = "g/img", value = list(v = 1))))
  it1 <- tr_galeria_sincronizar(root, store, s$reg, m$doc, ajuste, m$handles)
  n1 <- length(s$log$qualidade)
  it2 <- tr_galeria_sincronizar(root, store, s$reg, m$doc, ajuste, m$handles)
  expect_equal(length(s$log$qualidade), n1)
  expect_equal(it2[[1]]$arquivo, it1[[1]]$arquivo)
  m$doc <- tr_doc_apply(m$doc, list(op = "rename", node = "a", label = "Colunas"), s$reg)
  it3 <- tr_galeria_sincronizar(root, store, s$reg, m$doc, ajuste, m$handles)
  expect_equal(length(s$log$qualidade), n1)
  expect_equal(tools::file_path_sans_ext(it3[[1]]$arquivo), "colunas")
  expect_true(file.exists(file.path(root, "gallery", it3[[1]]$arquivo)))
  expect_false(file.exists(file.path(root, "gallery", it1[[1]]$arquivo)))
})

test_that("pasta da galeria não sai do projeto nem cai na raiz", {
  for (ruim in c(".", "..", "../fora", "a/../..", "flows", ".trama/x", "./"))
    expect_error(.tr_settings(list(galeria = list(pasta = ruim))), class = "tr_error_bad_theme")
  expect_equal(.tr_settings(list(galeria = list(pasta = "figuras/finais")))$galeria$pasta, "figuras/finais")
  expect_equal(.tr_settings(list(galeria = list(pasta = "/tmp/fig")))$galeria$pasta, "/tmp/fig")
})
