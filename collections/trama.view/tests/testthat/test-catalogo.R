# A varredura do catálogo, na mesma doutrina da coleção `data`: a ajuda é parte
# do nó, e não enfeite. Como o teste varre o registro inteiro em vez de listar
# nós um a um, nó novo nasce REPROVADO até ganhar a sua página — que é o ponto.

test_that("a coleção carrega sobre a data, e não sozinha", {
  expect_no_error(view_registry())
  # Sozinha, o núcleo recusa na porta: o tipo `data/table` não existe no
  # registro. A mensagem nomeia o tipo, que é o diagnóstico certo pra quem
  # esqueceu a coleção no `trama.json`. A dependência de ordem de carga fica
  # travada aqui por TESTE, e não por convenção escrita num comentário.
  reg <- trama::tr_registry()
  err <- tryCatch(trama::tr_use(trama_collection(), registry = reg), condition = identity)
  expect_s3_class(err, "tr_error_unknown_type")
  expect_match(conditionMessage(err), "data/table", fixed = TRUE)
})

test_that("todo nó tem help no formato, e todo campo digitável tem exemplo", {
  reg <- view_registry()
  digitaveis <- c("expr", "cols", "path", "text")
  nos <- Filter(function(n) startsWith(n$id, "view/"), reg$nodes)
  expect_length(nos, 24L)
  for (n in nos) {
    expect_true(!is.null(n$help) && nzchar(trimws(n$help)), info = n$id)
    # Formato curto da data e da view (c865c5c).
    expect_match(n$help, "## Uso principal", fixed = TRUE, info = n$id)
    expect_match(n$help, "## Exemplo curto", fixed = TRUE, info = n$id)
    expect_match(n$help, "## Usos relacionados", fixed = TRUE, info = n$id)
    # A seção de aparência é comparada INTEIRA, e não pelo título dela: um
    # `### Aparência` digitado à mão passaria numa busca por cabeçalho e
    # descreveria seis params que já mudaram de nome. O cabeçalho é convenção;
    # a constante é garantia — só passa quem colou `.TR_VIEW_AJUDA_APARENCIA`.
    # `view/save` não desenha, e as camadas herdam a aparência do gráfico de
    # entrada: nenhum dos quatro tem a seção nem os seis params.
    # No formato curto a seção de aparência fica no site; os seis params
    # cosméticos continuam cobrados no teste abaixo, na ordem.

    for (nm in names(n$params)) {
      p <- n$params[[nm]]
      if (!p$kind %in% digitaveis) next
      # Campo digitável sem exemplo é campo em branco na interface: o usuário
      # não tem como adivinhar que ali cabe um nome de coluna.
      expect_true(!is.null(p$example) && nzchar(p$example), info = paste(n$id, nm))
    }
  }
})

test_that("todo nó de gráfico declara os seis props cosméticos, na ordem", {
  # Ordem é interface: o card desenha os params na ordem de declaração, e o que
  # decide o que o gráfico AFIRMA tem que ficar acima do que decide como ele
  # PARECE. Um nó que declarasse os cosméticos à mão, ou fora do
  # `.tr_view_props()`, cairia aqui.
  reg <- view_registry()
  # `view/save` fica de fora: grava, não redesenha, e um tema ali seria um
  # segundo lugar para decidir a aparência da figura.
  # As camadas também: herdam a aparência do gráfico que recebem.
  sem <- c("view/save", "view/reference", "view/fit_line", "view/annotate")
  for (n in Filter(function(x) startsWith(x$id, "view/") && !x$id %in% sem, reg$nodes)) {
    nm <- names(n$params)
    comuns <- c("aspecto", "tema", "titulo", "rotulo_x", "rotulo_y", "legenda")
    expect_equal(utils::tail(nm, 6L), comuns, info = n$id)
    # Tema é do projeto: o param é o `theme` do núcleo, não um enum local.
    expect_equal(n$params$tema$kind, "theme", info = n$id)
    expect_equal(n$params$tema$default, formals(n$fn)$tema, info = n$id)
  }
})

test_that("toda referência cruzada da ajuda aponta pra nó que existe", {
  # As páginas citam outros nós em crase (`data/group_summarise`,
  # `view/points`). Renomear ou remover um nó não quebra nada em tempo de
  # execução: a citação simplesmente passa a mandar o usuário pra lugar nenhum,
  # em silêncio. O registro tem as DUAS coleções, então a checagem é barata.
  reg <- view_registry()
  conhecidos <- c(names(reg$nodes), names(reg$types))
  for (n in Filter(function(x) startsWith(x$id, "view/"), reg$nodes)) {
    citados <- regmatches(n$help, gregexpr("`[a-z][a-z0-9_.]*/[a-z0-9_]+`", n$help))[[1]]
    citados <- unique(gsub("`", "", citados, fixed = TRUE))
    expect_true(length(citados) > 0L, info = n$id)
    for (id in citados) expect_true(id %in% conhecidos, info = paste(n$id, "cita", id))
  }
})

test_that("o catálogo sai com a ajuda inteira", {
  # O `help` só serve se atravessar a serialização: é no JSON do catálogo que
  # a interface o lê.
  cat_json <- trama::tr_catalog_json(view_registry())
  expect_match(as.character(cat_json), "## Usos relacionados", fixed = TRUE)
})
