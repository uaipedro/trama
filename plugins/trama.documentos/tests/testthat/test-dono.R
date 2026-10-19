# As regras de dono, de ponta a ponta: projeto real, fluxo no store,
# documento no disco. Fora das regiões nada muda; região editada não é
# sobrescrita; região de nó removido fica órfã; a ordem é a do texto.

documento_com <- function(proj, ..., env = parent.frame()) {
  salvar_fluxo(proj, ...)
  tr_documento_criar("rel", proj, perfil = FALSE)
}

test_that("criar escreve o YAML do trama e o esqueleto do modelo, e mais nada", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab"))
  pasta <- dirname(qmd)
  expect_identical(basename(qmd), "rel.qmd")
  expect_true(file.exists(file.path(pasta, "_extensions/basico/_extension.yml")))
  expect_true(file.exists(file.path(pasta, "_brand.yml")))
  expect_false(file.exists(file.path(pasta, "_extensions/basico/template.qmd")))
  expect_identical(linhas(file.path(pasta, "_quarto.yml")), c("project:", "  output-dir: _saida"))
  y <- trama.documentos:::.td_yaml(linhas(qmd))
  expect_identical(names(y$format), c("basico-html", "basico-typst", "basico-docx"))
  expect_identical(y$trama$fluxo, "main")
  expect_null(y$author)
  # O corpo é o do esqueleto, igual.
  esq <- linhas(system.file("modelos/basico/template.qmd", package = "trama.documentos"))
  expect_identical(tail(linhas(qmd), 10), tail(esq, 10))
  expect_error(tr_documento_criar("rel", proj), class = "tr_documento_erro_nome")
  expect_error(tr_documento_criar("../fora", proj), class = "tr_documento_erro_nome")
  expect_identical(tr_documento_listar(proj)$nome, "rel")
})

test_that("inserir só acrescenta: o texto de antes fica byte a byte", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab"), no("txt", "tst/texto", valor = "oi"))
  antes <- linhas(qmd)
  tr_documento_inserir(qmd, "tab", proj)
  depois <- linhas(qmd)
  expect_identical(depois[seq_along(antes)], antes)
  expect_identical(depois[-seq_along(antes)][-1],
                   c(sprintf('::: {#trama-tab .trama trama-hash="%s"}', regiao(qmd, "tab")$hash),
                     "{{< include _trama/tab/tabela.md >}}", ":::"))
  pasta <- file.path(dirname(qmd), "_trama/tab")
  expect_setequal(list.files(pasta), c("dados.csv", "tabela.md", "trama.json"))
  expect_equal(utils::read.csv(file.path(pasta, "dados.csv")), data.frame(x = 1:3, y = (1:3) * 1.5))
  expect_identical(estado_de(qmd, proj), c(tab = "atualizada"))

  expect_error(tr_documento_inserir(qmd, "tab", proj), class = "tr_documento_erro_regiao_existe")
  expect_error(tr_documento_inserir(qmd, "nao-existe", proj), class = "tr_documento_erro_no")
})

test_that("inserir em linha, depois de região ou no topo; nunca no YAML, em região ou em código", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("a"), no("b"), no("c"), no("d"))
  l <- linhas(qmd)
  l <- c(l, "", "```{r}", "x <- 1", "```")
  writeLines(l, qmd)
  tr_documento_inserir(qmd, "a", proj)
  tr_documento_inserir(qmd, "b", proj, apos = 0)
  tr_documento_inserir(qmd, "c", proj, apos = "b")
  ordem <- vapply(trama.documentos:::.td_regioes(trama.documentos:::.td_ler(qmd)$linhas), `[[`, "", "chave")
  expect_identical(ordem, c("b", "c", "a"))
  # Primeira coisa depois do YAML.
  fim_yaml <- trama.documentos:::.td_fim_yaml(linhas(qmd))
  expect_match(linhas(qmd)[[fim_yaml + 2L]], "^::: \\{#trama-b ")

  bytes_antes <- bytes(qmd)
  chunk <- which(linhas(qmd) == "x <- 1")
  expect_error(tr_documento_inserir(qmd, "d", proj, apos = chunk), class = "tr_documento_erro_posicao")
  expect_error(tr_documento_inserir(qmd, "d", proj, apos = 2), class = "tr_documento_erro_posicao")
  expect_error(tr_documento_inserir(qmd, "d", proj, apos = regiao(qmd, "a")$inicio), class = "tr_documento_erro_posicao")
  expect_error(tr_documento_inserir(qmd, "d", proj, apos = "zzz"), class = "tr_documento_erro_posicao")
  expect_identical(bytes(qmd), bytes_antes)
})

test_that("editar o texto em volta e atualizar: o texto fica, a região muda", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab", n = 3L), no("txt", "tst/texto", valor = "um"))
  tr_documento_inserir(qmd, "tab", proj)
  tr_documento_inserir(qmd, "txt", proj)
  # A pessoa escreve antes, entre e depois das regiões, e põe um chunk seu.
  l <- linhas(qmd)
  i <- regiao(qmd, "txt")$inicio
  l <- c(l[1:(i - 1L)], "A Tabela @tbl-tab mostra **tudo**.", "", "```{r}", "summary(cars)", "```", "",
         l[i:length(l)], "", "## Discussão", "", "Texto final da pessoa.  ")
  writeLines(l, qmd)
  texto_antes <- fora(qmd)

  salvar_fluxo(proj, no("tab", n = 5L), no("txt", "tst/texto", valor = "dois"))
  expect_identical(estado_de(qmd, proj), c(tab = "desatualizada", txt = "desatualizada"))
  r <- tr_documento_atualizar(qmd, proj)
  expect_identical(r$estado, c("atualizada", "atualizada"))
  expect_identical(fora(qmd), texto_antes)
  expect_identical(estado_de(qmd, proj), c(tab = "atualizada", txt = "atualizada"))
  tab_md <- linhas(file.path(dirname(qmd), "_trama/tab/tabela.md"))
  expect_length(grep("^\\| [0-9]", tab_md), 5L)
  expect_true(any(grepl("dois", linhas(file.path(dirname(qmd), "_trama/txt/resultado.md")))))

  # De novo, sem mudança: nada a fazer, e o arquivo nem é regravado.
  b <- bytes(qmd)
  expect_identical(tr_documento_atualizar(qmd, proj)$estado, c("igual", "igual"))
  expect_identical(tr_documento_atualizar(qmd, proj, forcar = TRUE)$estado, c("igual", "igual"))
  expect_identical(bytes(qmd), b)
})

test_that("região editada à mão vira conflito e nunca é sobrescrita", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab", n = 3L), no("u", n = 2L))
  tr_documento_inserir(qmd, "tab", proj)
  tr_documento_inserir(qmd, "u", proj)
  editar(qmd, "{{< include _trama/tab/tabela.md >}}", "Minha versão da tabela, escrita à mão.")
  expect_identical(estado_de(qmd, proj), c(tab = "conflito", u = "atualizada"))

  pasta <- file.path(dirname(qmd), "_trama/tab")
  arquivos_antes <- lapply(list.files(pasta, full.names = TRUE), bytes)
  salvar_fluxo(proj, no("tab", n = 7L), no("u", n = 4L))
  b <- linhas(qmd)
  r <- tr_documento_atualizar(qmd, proj)
  expect_identical(r$estado, c("conflito", "atualizada"))
  conteudo <- regiao(qmd, "tab")$conteudo
  expect_identical(conteudo, "Minha versão da tabela, escrita à mão.")
  # Nem os arquivos da região em conflito mudam: o texto dela pode citá-los.
  expect_identical(lapply(list.files(pasta, full.names = TRUE), bytes), arquivos_antes)
  # Só as linhas da região `u` mudaram (se mudaram).
  depois <- linhas(qmd)
  ru <- regiao(qmd, "u")
  expect_identical(depois[-(ru$inicio:ru$fim)], b[-(ru$inicio:ru$fim)])

  # Espaço no fim, CRLF e linhas em branco na ponta não são edição.
  editar(qmd, "{{< include _trama/u/tabela.md >}}", "{{< include _trama/u/tabela.md >}}   ")
  expect_identical(estado_de(qmd, proj)[["u"]], "atualizada")
})

test_that("resolver: manter vira texto da pessoa, trazer descarta a edição, remover apaga", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("a"), no("b"), no("c"))
  for (n in c("a", "b", "c")) tr_documento_inserir(qmd, n, proj)
  for (n in c("a", "b")) editar(qmd, sprintf("{{< include _trama/%s/tabela.md >}}", n), sprintf("editado %s", n))
  expect_identical(estado_de(qmd, proj), c(a = "conflito", b = "conflito", c = "atualizada"))

  tr_documento_resolver(qmd, "a", "manter")
  expect_false(any(grepl("trama-a", linhas(qmd))))
  expect_true("editado a" %in% linhas(qmd))
  tr_documento_resolver(qmd, "b", "trazer", projeto = proj)
  expect_identical(regiao(qmd, "b")$conteudo, "{{< include _trama/b/tabela.md >}}")
  tr_documento_resolver(qmd, "c", "remover")
  expect_identical(estado_de(qmd, proj), c(b = "atualizada"))
  expect_false(any(grepl("trama-c|_trama/c/", linhas(qmd))))
  expect_error(tr_documento_resolver(qmd, "zzz", "manter"), class = "tr_documento_erro_regiao")

  # Limpar: `c` não é usada; `a` virou texto (a pasta foi congelada como
  # `a--mantida`), mas o texto não cita a pasta, então também sai; `b` fica.
  expect_setequal(tr_documento_limpar(qmd), c("a--mantida", "c"))
  expect_identical(list.files(file.path(dirname(qmd), "_trama")), "b")
})

test_that("bloco removido do fluxo deixa a região órfã, intocada", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("a"), no("b"))
  tr_documento_inserir(qmd, "a", proj)
  tr_documento_inserir(qmd, "b", proj)
  salvar_fluxo(proj, no("b", n = 9L))
  expect_identical(estado_de(qmd, proj), c(a = "orfa", b = "desatualizada"))
  ra <- regiao(qmd, "a")
  r <- tr_documento_atualizar(qmd, proj)
  expect_identical(r$estado, c("orfa", "atualizada"))
  expect_identical(regiao(qmd, "a")[c("inicio", "fim", "conteudo", "hash")], ra[c("inicio", "fim", "conteudo", "hash")])
  expect_true(dir.exists(file.path(dirname(qmd), "_trama/a")))
  expect_error(tr_documento_resolver(qmd, "a", "trazer", projeto = proj), class = "tr_documento_erro_regiao")
  # O nó volta: a região deixa de ser órfã.
  salvar_fluxo(proj, no("a"), no("b", n = 9L))
  expect_identical(estado_de(qmd, proj)[["a"]], "atualizada")
})

test_that("a pessoa move e apaga regiões; a ordem é a do texto", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("a", n = 1L), no("b", n = 2L), no("c", n = 3L))
  for (n in c("a", "b", "c")) tr_documento_inserir(qmd, n, proj)
  l <- linhas(qmd)
  ra <- regiao(qmd, "a"); rc <- regiao(qmd, "c")
  bloco_a <- l[ra$inicio:ra$fim]
  # Move `a` para o fim, depois de um parágrafo; apaga `c`.
  l <- l[-c(ra$inicio:ra$fim, rc$inicio:rc$fim)]
  l <- c(l, "Parágrafo novo.", "", bloco_a)
  writeLines(l, qmd)
  expect_identical(tr_documento_estado(qmd, proj)$regiao, c("b", "a"))

  salvar_fluxo(proj, no("a", n = 4L), no("b", n = 5L), no("c", n = 6L))
  texto <- fora(qmd)
  expect_identical(tr_documento_atualizar(qmd, proj)$regiao, c("b", "a"))
  expect_identical(fora(qmd), texto)
  expect_identical(tail(linhas(qmd), 3)[[2]], "{{< include _trama/a/tabela.md >}}")
})

test_that("documento CRLF: linhas novas em CRLF e o resto byte a byte", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab"))
  txt <- paste0(paste(linhas(qmd), collapse = "\r\n"), "\r\n")
  writeBin(charToRaw(enc2utf8(txt)), qmd)
  tr_documento_inserir(qmd, "tab", proj)
  novo <- rawToChar(bytes(qmd))
  expect_true(startsWith(novo, txt))
  expect_false(grepl("[^\r]\n", novo))
  salvar_fluxo(proj, no("tab", n = 8L))
  expect_identical(tr_documento_atualizar(qmd, proj)$estado, "atualizada")
  expect_true(startsWith(rawToChar(bytes(qmd)), txt))
})

test_that("região dentro de um callout da pessoa é atualizada; o callout fica", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab"))
  tr_documento_inserir(qmd, "tab", proj)
  l <- linhas(qmd)
  r <- regiao(qmd, "tab")
  l <- c(l[seq_len(r$inicio - 1L)], "::: {.callout-note}", "## Resultado principal", "",
         l[r$inicio:r$fim], "", "Comentário dentro do callout.", ":::")
  writeLines(l, qmd)
  texto <- fora(qmd)
  salvar_fluxo(proj, no("tab", n = 6L))
  expect_identical(tr_documento_atualizar(qmd, proj)$estado, "atualizada")
  expect_identical(fora(qmd), texto)
})

test_that("cerca quebrada: nada é escrito", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab"))
  tr_documento_inserir(qmd, "tab", proj)
  l <- linhas(qmd)
  writeLines(l[-length(l)], qmd)   # sem o ':::' final
  b <- bytes(qmd)
  expect_error(tr_documento_atualizar(qmd, proj), class = "tr_documento_erro_formato")
  expect_error(tr_documento_inserir(qmd, "tab", proj), class = "tr_documento_erro_formato")
  expect_identical(bytes(qmd), b)
})

test_that("documento salvo no editor durante a atualização: nada é gravado", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab"))
  tr_documento_inserir(qmd, "tab", proj)
  d <- trama.documentos:::.td_ler(qmd)
  cat("Linha que a pessoa acabou de salvar.\n", file = qmd, append = TRUE)
  b <- bytes(qmd)
  d$linhas <- c(d$linhas, "x")
  expect_error(trama.documentos:::.td_gravar(qmd, d), class = "tr_documento_erro_concorrente")
  expect_identical(bytes(qmd), b)
})

test_that("nó que falha: erro na região, documento intocado", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("f", "tst/falha"), no("tab"))
  expect_error(tr_documento_inserir(qmd, "f", proj), class = "tr_documento_erro_valor")
  tr_documento_inserir(qmd, "tab", proj)
  # O nó passa a falhar depois de inserido: atualizar marca erro e não toca.
  salvar_fluxo(proj, no("f", "tst/falha"), list(id = "tab", tipo = "tst/falha", params = list(n = 2L)))
  b <- bytes(qmd)
  r <- tr_documento_atualizar(qmd, proj)
  expect_identical(r$estado, "erro")
  expect_match(r$mensagem, "quebrou")
  expect_identical(bytes(qmd), b)
})

test_that("opções no cabeçalho são da pessoa: mudar trama-linhas refaz a região e o atributo fica", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab", n = 30L))
  tr_documento_inserir(qmd, "tab", proj, opcoes = list(linhas = 5))
  md <- function() linhas(file.path(dirname(qmd), "_trama/tab/tabela.md"))
  expect_length(grep("^\\| [0-9]", md()), 5L)
  r <- regiao(qmd, "tab")
  l <- linhas(qmd)
  l[[r$inicio]] <- sub('trama-linhas="5"', 'trama-linhas="12"', l[[r$inicio]])
  writeLines(l, qmd)
  expect_identical(estado_de(qmd, proj), c(tab = "desatualizada"))
  expect_identical(tr_documento_atualizar(qmd, proj)$estado, "atualizada")
  expect_length(grep("^\\| [0-9]", md()), 12L)
  expect_match(linhas(qmd)[[r$inicio]], 'trama-linhas="12"', fixed = TRUE)
  expect_identical(estado_de(qmd, proj), c(tab = "atualizada"))
})

test_that("porta que não é a primeira: região própria com trama-porta", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("d", "tst/duas", n = 3L))
  tr_documento_inserir(qmd, "d", proj, porta = "b")
  tr_documento_inserir(qmd, "d", proj)
  e <- tr_documento_estado(qmd, proj)
  expect_identical(e$regiao, c("d--b", "d"))
  expect_identical(e$porta, c("b", NA))
  expect_equal(utils::read.csv(file.path(dirname(qmd), "_trama/d--b/dados.csv"))$b, c(2, 4, 6))
  expect_error(tr_documento_inserir(qmd, "d", proj, porta = "zz"), class = "tr_documento_erro_no")
})

test_that("modelo com dados de treino: só com aceite, e o aceite fica no cabeçalho", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("m", "tst/modelo", n = 1L))
  b <- bytes(qmd)
  expect_error(tr_documento_inserir(qmd, "m", proj), class = "tr_documento_erro_dados_treino")
  expect_identical(bytes(qmd), b)
  expect_false(dir.exists(file.path(dirname(qmd), "_trama/m")))
  tr_documento_inserir(qmd, "m", proj, aceitar_dados_treino = TRUE)
  expect_identical(regiao(qmd, "m")$pares[["trama-dados-treino"]], "aceito")
  salvar_fluxo(proj, no("m", "tst/modelo", n = 2L))
  expect_identical(tr_documento_atualizar(qmd, proj)$estado, "atualizada")
  # Tirar o aceite do cabeçalho: a próxima atualização recusa.
  l <- linhas(qmd)
  r <- regiao(qmd, "m")
  l[[r$inicio]] <- sub(' trama-dados-treino="aceito"', "", l[[r$inicio]], fixed = TRUE)
  writeLines(l, qmd)
  salvar_fluxo(proj, no("m", "tst/modelo", n = 3L))
  r <- tr_documento_atualizar(qmd, proj)
  expect_identical(r$estado, "erro")
  expect_match(r$mensagem, "dados de treino")
})

test_that("projeto achado a partir do documento", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab"))
  tr_documento_inserir(qmd, "tab", proj)
  expect_identical(trama.documentos:::.td_raiz_de(qmd), normalizePath(proj$root))
  # Pelo trama.json, a coleção falsa não carrega: o nó não é órfão, só indisponível.
  expect_identical(estado_de(qmd, NULL), c(tab = "indisponivel"))
  expect_identical(tr_documento_atualizar(qmd)$estado, "indisponivel")
  fora_do_projeto <- withr::local_tempfile(fileext = ".qmd")
  writeLines("x", fora_do_projeto)
  expect_error(tr_documento_estado(fora_do_projeto), class = "tr_documento_erro_projeto")
})
