# Casos-limite das regras de dono, achados na revisão da fase 1.

documento_com <- function(proj, ...) {
  salvar_fluxo(proj, ...)
  tr_documento_criar("rel", proj, perfil = FALSE)
}

test_that("reescrever a região guarda as classes, o recuo e as aspas que a pessoa pôs", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab", n = 2L))
  tr_documento_inserir(qmd, "tab", proj)
  r <- regiao(qmd, "tab")
  l <- linhas(qmd)
  # A pessoa põe a região num item de lista, com uma classe e um atributo com aspas.
  cab <- sub("\\.trama ", ".trama .column-page ", l[[r$inicio]])
  cab <- sub("\\}$", " trama-nota='Diz \"oi\"'}", cab)
  l <- c(l[seq_len(r$inicio - 1L)], "1. Primeiro item:", "",
         paste0("    ", c(cab, l[(r$inicio + 1L):r$fim])), l[-seq_len(r$fim)])
  writeLines(l, qmd)
  expect_identical(estado_de(qmd, proj), c(tab = "desatualizada"))   # atributo novo = opção nova
  texto <- fora(qmd)
  salvar_fluxo(proj, no("tab", n = 3L))
  expect_identical(tr_documento_atualizar(qmd, proj)$estado, "atualizada")
  expect_identical(fora(qmd), texto)
  r <- regiao(qmd, "tab")
  cab <- linhas(qmd)[[r$inicio]]
  expect_match(cab, "^    ::: \\{#trama-tab \\.trama \\.column-page trama-hash=")
  expect_match(cab, "trama-nota='Diz \"oi\"'", fixed = TRUE)
  expect_identical(r$conteudo, "    {{< include _trama/tab/tabela.md >}}")
  expect_identical(r$pares[["trama-nota"]], 'Diz "oi"')
  expect_identical(linhas(qmd)[[r$fim]], "    :::")
  expect_identical(estado_de(qmd, proj), c(tab = "atualizada"))
})

test_that("manter congela a pasta: inserir o mesmo nó de novo não muda o texto que ficou com a pessoa", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab", n = 2L))
  tr_documento_inserir(qmd, "tab", proj)
  editar(qmd, "{{< include _trama/tab/tabela.md >}}",
         "Comentário meu. {{< include _trama/tab/tabela.md >}}")
  tr_documento_resolver(qmd, "tab", "manter")
  expect_true("Comentário meu. {{< include _trama/tab--mantida/tabela.md >}}" %in% linhas(qmd))
  congelada <- file.path(dirname(qmd), "_trama/tab--mantida/tabela.md")
  antes <- bytes(congelada)
  salvar_fluxo(proj, no("tab", n = 9L))
  tr_documento_inserir(qmd, "tab", proj)
  expect_identical(bytes(congelada), antes)
  expect_identical(tr_documento_limpar(qmd), character())   # a mantida é citada no texto
})

test_that("opção com quebra de linha e corpo com cerca solta são recusados sem escrever", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab"), no("q", "tst/quebrado"))
  b <- bytes(qmd)
  expect_error(tr_documento_inserir(qmd, "tab", proj, opcoes = list(linhas = "1\n:::")),
               class = "tr_documento_erro_opcoes")
  expect_error(tr_documento_inserir(qmd, "q", proj), class = "tr_documento_erro_parte")
  expect_identical(bytes(qmd), b)
  expect_false(dir.exists(file.path(dirname(qmd), "_trama/q")))
})

test_that("remover ou trazer uma região editada com cercas dentro pede confirmação", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab"))
  tr_documento_inserir(qmd, "tab", proj)
  r <- regiao(qmd, "tab")
  l <- linhas(qmd)
  l <- c(l[seq_len(r$inicio)], "::: callout-note", "Nota da pessoa.", ":::", l[(r$inicio + 1L):length(l)])
  writeLines(l, qmd)
  b <- bytes(qmd)
  expect_error(tr_documento_resolver(qmd, "tab", "remover"), "Nota da pessoa", class = "tr_documento_erro_confirmar")
  expect_error(tr_documento_resolver(qmd, "tab", "trazer", proj), class = "tr_documento_erro_confirmar")
  expect_identical(bytes(qmd), b)
  out <- tr_documento_resolver(qmd, "tab", "remover", confirmar = TRUE)
  expect_true("Nota da pessoa." %in% out$removidas)
  expect_false(any(grepl("trama-tab", linhas(qmd))))
})

test_that("opção igual ao padrão ou só o rótulo novo: o estado volta a 'atualizada'", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab", n = 3L))
  tr_documento_inserir(qmd, "tab", proj)
  r <- regiao(qmd, "tab")
  l <- linhas(qmd)
  l[[r$inicio]] <- sub("\\}$", ' trama-linhas="20"}', l[[r$inicio]])   # 20 é o padrão
  writeLines(l, qmd)
  expect_identical(estado_de(qmd, proj), c(tab = "desatualizada"))
  expect_identical(tr_documento_atualizar(qmd, proj)$estado, "igual")
  expect_identical(estado_de(qmd, proj), c(tab = "atualizada"))

  # O rótulo do card não muda a chave do store, mas muda a legenda.
  f <- trama::tr_flow(proj$registry) |> trama::tr_add("tab", "tst/tabela", n = 3L, label = "Vendas por mês")
  trama::tr_project_save(proj, trama::tr_flow_doc(f))
  expect_identical(estado_de(qmd, proj), c(tab = "desatualizada"))
  expect_identical(tr_documento_atualizar(qmd, proj)$estado, "atualizada")
  expect_identical(linhas(file.path(dirname(qmd), "_trama/tab/tabela.md"))[[1]], ": Vendas por mês {#tbl-tab}")
})

test_that("YAML com crases ou '...' recuado num bloco literal não confunde", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("a"), no("b"))
  l <- linhas(qmd)
  l <- c("---", "abstract: |", "  Exemplo:", "  ```", "  ...", "  fim", l[-1])
  writeLines(l, qmd)
  expect_identical(trama.documentos:::.td_fim_yaml(linhas(qmd)), which(linhas(qmd) == "---")[[2]])
  tr_documento_inserir(qmd, "a", proj, apos = 0)
  expect_identical(regiao(qmd, "a")$inicio, which(linhas(qmd) == "---")[[2]] + 2L)
  expect_identical(trama.documentos:::.td_fluxo_do(linhas(qmd)), "main")
})

test_that("bloco de código recuado num item de lista esconde as cercas de dentro", {
  l <- c("1. Item", "", "    ```markdown", "    ::: {#trama-x .trama}", "    ```", "", "Texto.")
  expect_length(trama.documentos:::.td_regioes(l), 0L)
  expect_false(trama.documentos:::.td_pode_inserir(l, 3))
  expect_true(trama.documentos:::.td_pode_inserir(l, 5))
})

test_that("região copiada à mão: a cópia é 'duplicada' e não é tocada", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("tab", n = 2L))
  tr_documento_inserir(qmd, "tab", proj)
  r <- regiao(qmd, "tab")
  l <- linhas(qmd)
  copia <- l[r$inicio:r$fim]
  copia[[2]] <- "Minha cópia."
  writeLines(c(l, "", copia), qmd)
  expect_identical(tr_documento_estado(qmd, proj)$estado, c("atualizada", "duplicada"))
  salvar_fluxo(proj, no("tab", n = 5L))
  expect_identical(tr_documento_atualizar(qmd, proj)$estado, c("atualizada", "duplicada"))
  expect_true("Minha cópia." %in% linhas(qmd))
})

test_that("limpar conta as regiões de todos os .qmd da pasta", {
  proj <- local_projeto()
  qmd <- documento_com(proj, no("a"), no("b"))
  tr_documento_inserir(qmd, "a", proj)
  outro <- file.path(dirname(qmd), "anexo.qmd")
  writeLines(c("---", "title: Anexo", "---", ""), outro)
  tr_documento_inserir(outro, "b", proj)
  expect_identical(tr_documento_limpar(qmd), character())
  expect_setequal(list.files(file.path(dirname(qmd), "_trama")), c("a", "b"))
})
