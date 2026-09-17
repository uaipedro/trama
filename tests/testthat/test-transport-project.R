# A troca de projeto é a única operação do transporte que mexe em TUDO de uma
# vez: documento, store, log de undo e run em voo. O que se trava aqui é a ordem
# — validar antes de mexer — e o zeramento do undo.

# Projeto montado à mão, como em test-session.R: a coleção de teste não é pacote,
# então o registry não tem `package` e nenhum manifesto casaria com ele.
projeto_falso <- function(cols = character()) {
  root <- withr::local_tempdir("proj", .local_envir = parent.frame())
  tr_project_new(root, cols)
  reg <- tr_registry()
  for (p in cols) reg$collections[[p]] <- list(id = p, label = p, version = "1",
                                               js = NULL, css = NULL, package = p)
  tr_project_at(root, reg)
}

# `add_frame` é a op mais barata que o log aceita sem tipo de nó nenhum
# registrado: é de apresentação, então nem dispara execução.
op_frame <- function(seq, rev) {
  list(seq = seq, base_rev = rev,
       op = list(op = "add_frame", x = 0, y = 0, w = 100, h = 60, title = "f"))
}

test_that("abrir troca o projeto, manda project + document e zera o undo", {
  p1 <- projeto_falso(); p2 <- projeto_falso()

  shiny::testServer(tr_server(p1, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    tipos <- function() vapply(msgs, function(m) m$type, "")
    log_len <- function() length(session$env$log)

    session$setInputs(tr_ready = 1)
    # Uma op aplicada para o log ter o que perder.
    session$setInputs(tr_op = op_frame(1, rv_doc()$rev))
    expect_gt(log_len(), 0L)

    msgs <- list()
    session$setInputs(tr_project_open = list(seq = 1, path = p2$root))

    expect_equal(normalizePath(rv_project()$root), normalizePath(p2$root))
    expect_true(all(c("project", "document") %in% tipos()))
    # Reaplicar o log de um projeto sobre o documento de outro é o que
    # `.tr_undo_doc()` existe para impedir: trocar sem zerar deixaria um Ctrl+Z
    # armado com a história errada.
    expect_equal(log_len(), 0L)
  })
})

test_that("abrir com coleção faltando NÃO muda nada, e avisa nomeando-a", {
  p1 <- projeto_falso()
  outro <- withr::local_tempdir("q")
  tr_project_new(file.path(outro, "t"), "trama.terrain")

  shiny::testServer(tr_server(p1, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message

    session$setInputs(tr_ready = 1)
    msgs <- list()
    session$setInputs(tr_project_open = list(seq = 1, path = file.path(outro, "t")))

    # A parte que importa de "valida antes de mexer": continua sendo o de antes.
    expect_equal(normalizePath(rv_project()$root), normalizePath(p1$root))
    avisos <- Filter(function(m) identical(m$type, "warning"), msgs)
    expect_length(avisos, 1L)
    expect_match(avisos[[1]]$message, "trama.terrain", fixed = TRUE)
  })
})

test_that("browse devolve a listagem, marcando o que é projeto", {
  p1 <- projeto_falso()

  shiny::testServer(tr_server(p1, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message

    session$setInputs(tr_ready = 1)
    msgs <- list()
    session$setInputs(tr_browse = list(seq = 1, path = dirname(p1$root)))

    l <- Filter(function(m) identical(m$type, "listing"), msgs)
    expect_length(l, 1L)
    alvo <- Filter(function(e) identical(e$nome, basename(p1$root)), l[[1]]$entries)
    expect_length(alvo, 1L)
    expect_true(alvo[[1]]$projeto)
  })
})

test_that("criar projeto grava no disco e abre em seguida", {
  p1 <- projeto_falso()
  base <- withr::local_tempdir("base")

  shiny::testServer(tr_server(p1, autosave = FALSE), {
    session$sendCustomMessage <- function(type, message) invisible(NULL)
    session$setInputs(tr_ready = 1)
    session$setInputs(tr_project_new = list(seq = 1, path = base, nome = "novo"))

    expect_true(file.exists(file.path(base, "novo", "trama.json")))
    expect_equal(normalizePath(rv_project()$root), normalizePath(file.path(base, "novo")))
  })
})

# Criar sobre pasta ocupada falha no primeiro passo, e o segundo NÃO acontece:
# abrir um projeto que não foi criado agora seria abrir o de outra pessoa.
test_that("criar sobre pasta ocupada avisa e não troca de projeto", {
  p1 <- projeto_falso()
  base <- withr::local_tempdir("base")
  tr_project_new(file.path(base, "ja"), character())

  shiny::testServer(tr_server(p1, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    session$setInputs(tr_ready = 1)
    msgs <- list()
    session$setInputs(tr_project_new = list(seq = 1, path = base, nome = "ja"))

    expect_equal(normalizePath(rv_project()$root), normalizePath(p1$root))
    expect_true(any(vapply(msgs, function(m) identical(m$type, "warning"), TRUE)))
  })
})

# O nome vem do cliente: cru no `file.path`, `""` fazia a pasta NAVEGADA virar
# projeto e `"../x"` punha o projeto fora dela — os dois calados.
test_that("nome inválido avisa, não escreve nada e não troca de projeto", {
  p1 <- projeto_falso()
  base <- withr::local_tempdir("base")
  irmao <- file.path(dirname(base), "escapou")

  shiny::testServer(tr_server(p1, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    avisou <- function() any(vapply(msgs, function(m) identical(m$type, "warning"), TRUE))
    session$setInputs(tr_ready = 1)

    msgs <- list()
    session$setInputs(tr_project_new = list(seq = 1, path = base, nome = ""))
    expect_true(avisou())
    expect_equal(list.files(base, all.files = TRUE, no.. = TRUE), character())

    msgs <- list()
    session$setInputs(tr_project_new = list(seq = 2, path = base, nome = "../escapou"))
    expect_true(avisou())
    expect_false(dir.exists(irmao))

    expect_equal(normalizePath(rv_project()$root), normalizePath(p1$root))
  })
})

# Abrir pasta que não é projeto trocava de projeto com sucesso e deixava
# `.trama/` e `flows/` lá dentro. O diálogo só oferece "Abrir" para quem tem
# manifesto; o servidor não pode confiar nisso.
test_that("abrir pasta sem manifesto avisa, não troca e não deixa andaime", {
  p1 <- projeto_falso()
  fotos <- withr::local_tempdir("fotos")

  shiny::testServer(tr_server(p1, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    session$setInputs(tr_ready = 1)

    msgs <- list()
    session$setInputs(tr_project_open = list(seq = 1, path = fotos))

    avisos <- Filter(function(m) identical(m$type, "warning"), msgs)
    expect_length(avisos, 1L)
    expect_match(avisos[[1]]$message, "trama.json", fixed = TRUE)
    expect_equal(normalizePath(rv_project()$root), normalizePath(p1$root))
    expect_false(dir.exists(file.path(fotos, ".trama")))
    expect_false(dir.exists(file.path(fotos, "flows")))
  })
})

# Reabrir o que já está aberto rodava o GC sobre o próprio projeto e zerava o
# log: o documento sobrevivia (autosave) e o Ctrl+Z do usuário sumia calado.
test_that("reabrir o projeto já aberto preserva o log de undo", {
  p1 <- projeto_falso()

  shiny::testServer(tr_server(p1, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    log_len <- function() length(session$env$log)

    session$setInputs(tr_ready = 1)
    session$setInputs(tr_op = op_frame(1, rv_doc()$rev))
    n <- log_len()
    expect_gt(n, 0L)

    # Caminho NÃO normalizado, que é como ele pode chegar do cliente.
    msgs <- list()
    session$setInputs(tr_project_open = list(seq = 1, path = file.path(p1$root, ".")))

    expect_equal(log_len(), n)
    expect_equal(normalizePath(rv_project()$root), normalizePath(p1$root))
    expect_false(any(vapply(msgs, function(m) identical(m$type, "warning"), TRUE)))
  })
})

# A FORMA do JSON, e não o conteúdo: o diálogo do front itera `entries` e testa
# `parent` para decidir se desenha o "..". O estrago que este teste previne é o
# de `R/store.R:130-141` — uma lista de um elemento que colapsa em escalar e o
# outro lado recebe algo que não é array, sem erro em lugar nenhum.
test_that("a listagem desce com entries sempre em array e parent presente", {
  p1 <- projeto_falso()
  vazia <- withr::local_tempdir("vazia")
  uma <- withr::local_tempdir("uma")
  dir.create(file.path(uma, "so-esta"))

  shiny::testServer(tr_server(p1, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    json <- function(path) {
      msgs <<- list()
      session$setInputs(tr_browse = list(seq = 1, path = path))
      l <- Filter(function(m) identical(m$type, "listing"), msgs)
      as.character(jsonlite::toJSON(l[[1]], auto_unbox = TRUE, null = "null"))
    }
    session$setInputs(tr_ready = 1)

    j0 <- json(vazia)
    expect_match(j0, '"entries":[]', fixed = TRUE)
    expect_match(j0, '"parent":"', fixed = TRUE)

    j1 <- json(uma)
    expect_match(j1, '"entries":[{', fixed = TRUE)
    expect_match(j1, '"nome":"so-esta"', fixed = TRUE)
    # Uma entrada não pode virar objeto solto nem o vetor `["so-esta"]`.
    expect_no_match(j1, '"entries":{', fixed = TRUE)

    # Campo sumido e campo nulo são a mesma coisa em R e coisas diferentes no
    # front: sem `parent`, o diálogo não sabe se está na raiz do disco ou se o
    # servidor esqueceu de responder. `dirname("/")` é `"/"` no POSIX; em
    # Windows a raiz é outra coisa e o caso não se reproduz.
    skip_if_not(identical(dirname("/"), "/"))
    expect_match(json("/"), '"parent":null', fixed = TRUE)
  })
})

test_that("tr_themes grava no manifesto e reenvia; tema inválido não toca o arquivo", {
  p1 <- projeto_falso()
  f <- file.path(p1$root, "trama.json")

  shiny::testServer(tr_server(p1, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    tipos <- function() vapply(msgs, function(m) m$type, "")

    session$setInputs(tr_ready = 1)
    expect_true("themes" %in% tipos())

    msgs <- list()
    session$setInputs(tr_themes = list(temas = list(rel = list(fundo = "#ffffff")), tema_padrao = "rel",
                                       marca = TRUE, seq = 1))
    expect_equal(rv_project()$settings$tema_padrao, "rel")
    expect_equal(jsonlite::fromJSON(f)$tema_padrao, "rel")
    temas <- Filter(function(m) identical(m$type, "themes"), msgs)
    expect_equal(temas[[1]]$tema_padrao, "rel")

    antes <- readBin(f, "raw", file.size(f))
    msgs <- list()
    session$setInputs(tr_themes = list(temas = list(rel = list(fundo = "azul")), tema_padrao = "rel",
                                       marca = TRUE, seq = 2))
    expect_identical(readBin(f, "raw", file.size(f)), antes)
    expect_true(all(c("warning", "themes") %in% tipos()))
    expect_equal(rv_project()$settings$temas$rel$fundo, "#ffffff")

    # Sem `tema_padrao` (formato antigo `padrao` incluso) é recusa, não padrão
    # escolhido calado.
    msgs <- list()
    session$setInputs(tr_themes = list(temas = list(rel = list(fundo = "#000000")), padrao = "rel",
                                       marca = TRUE, seq = 3))
    expect_identical(readBin(f, "raw", file.size(f)), antes)
    expect_true(all(c("warning", "themes") %in% tipos()))
    expect_equal(rv_project()$settings$temas$rel$fundo, "#ffffff")
  })
})

test_that("`ctx_extra` de tr_server() chega ao executor do run", {
  # A outra porta dos ajustes do run (`tr_run()` é a primeira). O salto é de uma
  # linha, e é justamente por isso que ele precisa de teste: sem `ctx_extra` aqui
  # o editor roda com o default calado, que é o defeito que a Tarefa 5.4
  # conserta — um botão que existe na assinatura e não no caminho.
  root <- withr::local_tempdir("proj")
  tr_project_new(root, character())
  proj <- tr_project_at(root, store_registry())

  visto <- new.env(parent = emptyenv())
  espia <- tr_executor_sequential()
  submit0 <- espia$submit
  espia$submit <- function(unit, registry, store, ctx_extra = NULL) {
    visto$ctx_extra <- ctx_extra
    submit0(unit, registry, store, ctx_extra)
  }

  shiny::testServer(tr_server(proj, autosave = FALSE, executor = espia,
                              ctx_extra = list(checkpoint_every = 7)), {
    session$sendCustomMessage <- function(type, message) invisible(NULL)
    session$setInputs(tr_ready = 1)
    session$setInputs(tr_op = list(
      seq = 1, base_rev = rv_doc()$rev,
      op = list(op = "add_node", type = "t/const", id = "c", params = list(v = 1))))
  })

  expect_equal(visto$ctx_extra$checkpoint_every, 7)
})

test_that("tr_themes grava a marca junto; recusa não deixa o pedido pela metade", {
  p1 <- projeto_falso()
  f <- file.path(p1$root, "trama.json")

  shiny::testServer(tr_server(p1, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    tipos <- function() vapply(msgs, function(m) m$type, "")
    ultimo <- function() {
      t <- Filter(function(m) identical(m$type, "themes"), msgs)
      t[[length(t)]]
    }

    session$setInputs(tr_ready = 1)
    expect_true(ultimo()$marca)

    msgs <- list()
    session$setInputs(tr_themes = list(temas = list(rel = list()), tema_padrao = "rel",
                                       marca = FALSE, seq = 1))
    expect_false(rv_project()$settings$marca)
    expect_false(jsonlite::fromJSON(f)$marca)
    expect_false(ultimo()$marca)

    # Tema inválido no mesmo gesto: nem o tema nem a marca entram. Religar a
    # marca por causa de um tema recusado seria gravar metade do pedido.
    antes <- readBin(f, "raw", file.size(f))
    msgs <- list()
    session$setInputs(tr_themes = list(temas = list(rel = list(fundo = "azul")), tema_padrao = "rel",
                                       marca = TRUE, seq = 2))
    expect_identical(readBin(f, "raw", file.size(f)), antes)
    expect_true("warning" %in% tipos())
    expect_false(rv_project()$settings$marca)
    expect_false(ultimo()$marca)

    # Sem `marca` é recusa, como sem `tema_padrao`: o painel sempre sabe em que
    # posição a chave está, e assumir "ligada" religaria a marca de quem a
    # desligou.
    msgs <- list()
    session$setInputs(tr_themes = list(temas = list(rel = list(fundo = "#000000")), tema_padrao = "rel",
                                       seq = 3))
    expect_identical(readBin(f, "raw", file.size(f)), antes)
    expect_true("warning" %in% tipos())
    expect_false(ultimo()$marca)
  })
})
