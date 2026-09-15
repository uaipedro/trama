test_that("tr_icon aceita nome do conjunto e svg cru, um de cada vez", {
  a <- tr_icon("eraser")
  expect_equal(a$kind, "set")
  expect_equal(a$value, "eraser")

  b <- tr_icon(svg = "<path d='M3 3'/>")
  expect_equal(b$kind, "svg")
  expect_equal(b$value, "<path d='M3 3'/>")
})

test_that("tr_icon recusa nenhum argumento, ou os dois", {
  expect_error(tr_icon(), class = "tr_error_bad_icon")
  expect_error(tr_icon("eraser", svg = "<path/>"), class = "tr_error_bad_icon")
})

test_that("tr_icon recusa nome que não existe no sprite", {
  # 'filter' é a armadilha real: parece óbvio e NÃO existe no Lucide — o nome
  # certo é 'list-filter'. É o typo que este erro existe pra pegar.
  expect_error(tr_icon("filter"), class = "tr_error_unknown_icon")
  expect_error(tr_icon("nao-existe-mesmo"), class = "tr_error_unknown_icon")
  expect_silent(tr_icon("list-filter"))
})

test_that("tr_icon recusa nome e svg que não sejam string única", {
  expect_error(tr_icon(c("a", "b")), class = "tr_error_bad_icon")
  expect_error(tr_icon(svg = 42), class = "tr_error_bad_icon")
})

test_that("sprite presente mas ilegível é erro, não lista vazia", {
  # A distinção que importa: vazio significaria "todo nome é válido", e o typo
  # que tr_icon() existe pra pegar passaria batido em cada bloco do catálogo.
  vazio <- withr::local_tempfile(fileext = ".svg")
  writeLines("<svg><defs></defs></svg>", vazio)
  expect_error(.tr_icon_parse(vazio), class = "tr_error_bad_sprite")

  bom <- withr::local_tempfile(fileext = ".svg")
  writeLines('<svg><symbol id="ab"><path/></symbol><symbol id="cd"/></svg>', bom)
  expect_equal(.tr_icon_parse(bom), c("ab", "cd"))
})

test_that("sprite ausente degrada: validação não roda, em vez de recusar tudo", {
  # Instalação sem o asset não pode impedir uma coleção de carregar — o front
  # já sabe mostrar bloco sem ícone. É o oposto do sprite ILEGÍVEL acima.
  withr::local_environment(NULL)
  antigo <- .tr_icon_env$names
  withr::defer(.tr_icon_env$names <- antigo)
  .tr_icon_env$names <- character(0)

  expect_silent(tr_icon("nome-que-nao-existe-em-lugar-nenhum"))
  expect_equal(tr_icon("nome-que-nao-existe-em-lugar-nenhum")$kind, "set")
})

test_that("todo ícone declarado pelas coleções do repo existe no sprite", {
  # O teste que mais paga: pega o dia em que alguém atualizar o Lucide e um
  # ícone for renomeado — falha aqui em vez de virar buraco no card de quem
  # instalou. Lê o TEXTO das coleções em vez de carregá-las, porque carregar
  # `trama.data` arrastaria dplyr/readr pra dentro da suíte do núcleo, que é
  # exatamente o acoplamento que helper-collection.R:1-3 existe pra impedir —
  # e porque um `skip` por dependência ausente deixaria a checagem sumir em
  # silêncio num CI magro.
  raiz <- testthat::test_path("..", "..")
  fontes <- Sys.glob(file.path(raiz, "collections", "*", "R", "*.R"))
  skip_if(length(fontes) == 0, "coleções não estão presentes (pacote instalado)")

  txt <- unlist(lapply(fontes, readLines, warn = FALSE), use.names = FALSE)
  nomes <- regmatches(txt, gregexpr('tr_icon\\("[^"]+"', txt))
  nomes <- sub('^tr_icon\\("', "", unlist(nomes, use.names = FALSE))
  nomes <- unique(sub('"$', "", nomes))
  expect_gt(length(nomes), 0)

  desconhecidos <- setdiff(nomes, .tr_icon_names())
  expect_equal(desconhecidos, character(0))
})
