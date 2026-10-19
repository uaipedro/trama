# O painel Documentos sem navegador: o controlador recebe as ações do JS e
# responde com mensagens; `agendar` chama na hora (render em segundo plano
# vira espera curta).

controlador_de_teste <- function(proj, servir = function(dir, nome) paste0("trama-acao-", nome, "/"),
                                 agendar = function(f, seg) { Sys.sleep(seg); f() }, recarregar = NULL) {
  msgs <- list()
  ctl <- trama.documentos:::.td_controlador(
    enviar = function(m) msgs[[length(msgs) + 1L]] <<- m,
    agendar = agendar,
    ctx = list(project = function() proj, servir = servir, recarregar = recarregar))
  list(receber = ctl, msgs = function() msgs, tipos = function() vapply(msgs, `[[`, "", "tipo"),
       ultima = function(tipo = NULL) {
         m <- if (is.null(tipo)) msgs else Filter(function(x) identical(x$tipo, tipo), msgs)
         m[[length(m)]]
       })
}

test_that("contrato de plugin: JS e CSS existem no pacote, server é função", {
  e <- trama_plugin()
  expect_true(file.exists(system.file(e$js, package = "trama.documentos")))
  expect_true(file.exists(system.file(e$css, package = "trama.documentos")))
  expect_true(is.function(e$server))
  expect_true(isTRUE(as.logical(utils::packageDescription("trama.documentos", fields = "Config/trama/plugin"))))
})

test_that("listar, criar, inserir, estado e atualizar pelo painel", {
  proj <- local_projeto()
  salvar_fluxo(proj, no("tab", n = 3L))
  tr_documento_perfil_salvar("ufla", "Ana Lima")
  c <- controlador_de_teste(proj)
  c$receber(list(acao = "listar"))
  l <- c$ultima("lista")
  expect_length(l$documentos, 0L)
  expect_identical(l$perfil_padrao, "ufla")
  expect_true("basico" %in% vapply(l$modelos, `[[`, "", "nome"))

  c$receber(list(acao = "criar", nome = "rel", titulo = "Teste", modelo = "basico", perfil = "ufla"))
  expect_identical(c$ultima("criado")$documento, "documentos/rel/rel.qmd")
  doc <- "documentos/rel/rel.qmd"
  expect_identical(c$ultima("lista")$documentos[[1]]$caminho, doc)

  c$receber(list(acao = "inserir", documento = doc, no = "tab", opcoes = list(linhas = 2, completa = TRUE)))
  expect_identical(c$ultima("inserido")$regiao, "tab")
  e <- c$ultima("estado")
  expect_identical(e$regioes[[1]]$estado, "atualizada")
  expect_null(e$regioes[[1]]$porta)
  cab <- regiao(file.path(proj$root, doc), "tab")$pares
  expect_identical(cab[["trama-linhas"]], "2")
  expect_identical(cab[["trama-tabela"]], "completa")
  expect_identical(c$ultima("lista")$documentos[[1]]$resumo, list(atualizada = 1L))

  salvar_fluxo(proj, no("tab", n = 5L))
  c$receber(list(acao = "listar"))
  expect_identical(c$ultima("lista")$documentos[[1]]$resumo, list(desatualizada = 1L))
  c$receber(list(acao = "atualizar", documento = doc))
  expect_identical(c$ultima("atualizado")$resultado[[1]]$estado, "atualizada")
  expect_identical(c$ultima("estado")$regioes[[1]]$estado, "atualizada")
})

test_that("erros voltam como mensagem com a classe; caminho de fora é recusado", {
  proj <- local_projeto()
  salvar_fluxo(proj, no("m", "tst/modelo"))
  qmd <- tr_documento_criar("rel", proj, perfil = FALSE)
  c <- controlador_de_teste(proj)
  c$receber(list(acao = "inserir", documento = "documentos/rel/rel.qmd", no = "m"))
  e <- c$ultima()
  expect_identical(e$tipo, "erro")
  expect_identical(e$classe, "tr_documento_erro_dados_treino")
  c$receber(list(acao = "inserir", documento = "documentos/rel/rel.qmd", no = "m", aceitar = TRUE))
  expect_identical(c$ultima("inserido")$regiao, "m")

  for (ruim in c("../fora.qmd", "documentos/../x/y.qmd", "documentos/rel/../../trama.json", "/etc/passwd",
                 "documentos/.oculto/a.qmd")) {
    c$receber(list(acao = "estado", documento = ruim))
    expect_identical(c$ultima()$classe, "tr_documento_erro_painel", label = ruim)
  }
  c$receber(list(acao = "nao-existe"))
  expect_match(c$ultima()$mensagem, "desconhecida")
})

test_that("resolver pelo painel: confirmar vem do JS", {
  proj <- local_projeto()
  salvar_fluxo(proj, no("tab"))
  qmd <- tr_documento_criar("rel", proj, perfil = FALSE)
  tr_documento_inserir(qmd, "tab", proj)
  r <- regiao(qmd, "tab")
  l <- linhas(qmd)
  writeLines(c(l[seq_len(r$inicio)], "::: callout-note", "Nota.", ":::", l[(r$inicio + 1L):length(l)]), qmd)
  c <- controlador_de_teste(proj)
  doc <- "documentos/rel/rel.qmd"
  c$receber(list(acao = "resolver", documento = doc, regiao = "tab", como = "remover"))
  expect_identical(c$ultima()$classe, "tr_documento_erro_confirmar")
  c$receber(list(acao = "resolver", documento = doc, regiao = "tab", como = "remover", confirmar = TRUE))
  expect_identical(c$ultima("resolvido")$regiao, "tab")
  expect_length(c$ultima("estado")$regioes, 0L)
})

test_that("abrir no programa do sistema: documento, pasta e arquivo exportado", {
  proj <- local_projeto()
  salvar_fluxo(proj, no("tab"))
  qmd <- tr_documento_criar("rel", proj, perfil = FALSE)
  abertos <- character()
  withr::local_options(trama.documentos.abrir = function(caminho) abertos <<- c(abertos, caminho))
  c <- controlador_de_teste(proj)
  doc <- "documentos/rel/rel.qmd"
  c$receber(list(acao = "abrir", documento = doc, alvo = "documento"))
  c$receber(list(acao = "abrir", documento = doc, alvo = "pasta"))
  expect_identical(normalizePath(abertos), normalizePath(c(qmd, dirname(qmd))))
  c$receber(list(acao = "abrir", documento = doc, alvo = "saida", arquivo = "documentos/rel/_saida/../../../x.pdf"))
  expect_identical(c$ultima()$classe, "tr_documento_erro_painel")
  # Slides e dashboard moram em subpastas de `_saida/`; outra subpasta, não.
  for (sub in c("slides", "outra")) {
    dir.create(file.path(dirname(qmd), "_saida", sub), recursive = TRUE)
    file.create(file.path(dirname(qmd), "_saida", sub, "rel.html"))
  }
  c$receber(list(acao = "abrir", documento = doc, alvo = "saida", arquivo = "documentos/rel/_saida/slides/rel.html"))
  expect_identical(c$ultima()$tipo, "aberto")
  expect_identical(normalizePath(utils::tail(abertos, 1L)), normalizePath(file.path(dirname(qmd), "_saida", "slides", "rel.html")))
  c$receber(list(acao = "abrir", documento = doc, alvo = "saida", arquivo = "documentos/rel/_saida/outra/rel.html"))
  expect_identical(c$ultima()$classe, "tr_documento_erro_painel")
  c$receber(list(acao = "guia_quarto"))
  expect_match(c$ultima("guia")$passos[[1]]$link, "quarto.org")
})

test_that("sem Quarto, a lista avisa (o painel mostra o guia)", {
  proj <- local_projeto()
  withr::local_envvar(QUARTO_PATH = file.path(tempdir(), "nao-existe", "quarto"))
  c <- controlador_de_teste(proj)
  c$receber(list(acao = "listar"))
  expect_false(c$ultima("lista")$quarto)
  c$receber(list(acao = "visualizar", documento = "documentos/x/x.qmd"))
  expect_identical(c$ultima()$tipo, "erro")
})

test_that("aba Modelos: ações modelos_* vão para o ponto da fase 3", {
  proj <- local_projeto()
  c <- controlador_de_teste(proj)
  c$receber(list(acao = "modelos_listar"))
  expect_true("basico" %in% vapply(c$ultima("modelos")$modelos, `[[`, "", "nome"))
  c$receber(list(acao = "modelos_criar"))
  expect_match(c$ultima()$mensagem, "modelos desconhecida")
})

test_that("visualizar e exportar em segundo plano, servindo a prévia pelo editor", {
  skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")), "R CMD check")
  skip_if(!nzchar(trama.documentos:::.td_quarto()), "Quarto não instalado")
  proj <- local_projeto()
  salvar_fluxo(proj, no("tab"))
  qmd <- tr_documento_criar("rel", proj, perfil = FALSE)
  tr_documento_inserir(qmd, "tab", proj)
  servidas <- list()
  c <- controlador_de_teste(proj, servir = function(dir, nome) { servidas[[nome]] <<- dir; paste0("trama-acao-", nome, "/") })
  doc <- "documentos/rel/rel.qmd"
  c$receber(list(acao = "visualizar", documento = doc))
  expect_identical(c$ultima("ocupado")$tarefa, "previa")
  expect_identical(c$ultima("previa")$url, "trama-acao-documento-rel/rel.html")
  expect_identical(normalizePath(servidas[["documento-rel"]]), normalizePath(file.path(dirname(qmd), "_saida")))
  c$receber(list(acao = "exportar", documento = doc, formato = "html"))
  expect_identical(c$ultima("exportado")$arquivo, "documentos/rel/_saida/rel.html")
  # Render que falha volta como erro, e o painel solta o "ocupado".
  cat("\n{{< include _trama/nao-existe/x.md >}}\n", file = qmd, append = TRUE)
  c$receber(list(acao = "exportar", documento = doc, formato = "html"))
  expect_identical(c$ultima()$tipo, "erro")
  expect_identical(c$ultima()$classe, "tr_documento_erro_render")
})

test_that("a primeira lista da sessão migra o bloco Publicar, recarrega o editor e avisa; a segunda, não", {
  proj <- local_projeto()
  fluxo_com_publicar(proj)
  recargas <- 0L
  c <- controlador_de_teste(proj, recarregar = function() recargas <<- recargas + 1L)
  c$receber(list(acao = "listar"))
  m <- c$ultima("migrado")
  expect_identical(m$documentos[[1]]$documento, "documentos/consumo-e-peso/consumo-e-peso.qmd")
  expect_identical(recargas, 1L)
  expect_identical(c$ultima("lista")$documentos[[1]]$nome, "consumo-e-peso")
  c$receber(list(acao = "listar"))
  expect_identical(sum(c$tipos() == "migrado"), 1L)
  expect_identical(recargas, 1L)
})

test_that("publicar pelo painel: conferir, confirmar, enviar e acompanhar até a página no ar", {
  gh <- github_memoria()
  x <- doc_publicavel()
  tarefas <- list()
  c <- controlador_de_teste(x$proj, agendar = function(f, seg) tarefas[[length(tarefas) + 1L]] <<- f)
  doc <- "documentos/consumo/consumo.qmd"
  c$receber(list(acao = "publicar_conferir", documento = doc))
  p <- c$ultima("publicacao")
  expect_identical(p$titulo, "Consumo e peso")
  expect_identical(p$repo, "consumo-e-peso")
  expect_false(p$registrado)
  expect_length(p$erros, 0L)
  expect_null(p$envio)
  # Privado pede confirmação antes de tocar no GitHub.
  mudar_yaml(x$qmd, function(y) { y$trama$publicar$privado <- TRUE; y })
  c$receber(list(acao = "publicar", documento = doc, repositorio = ""))
  expect_match(c$ultima("publicar_confirmar")$texto, "plano pago")
  expect_length(gh$chamadas, 0L)
  c$receber(list(acao = "publicar", documento = doc, repositorio = "", confirmado = TRUE))
  e <- c$ultima("envio")
  expect_identical(e$estado$tag, "v0.1.0")
  expect_length(tarefas, 1L)
  # O acompanhamento agendado lê o GitHub e se reagenda até terminar.
  gh$runs <- list(list(id = 1, nome = "publicar", estado = "in_progress", conclusao = NA, url = "https://run/1"))
  tarefas[[1]]()
  expect_identical(c$ultima("envio")$estado$etapa, "actions")
  expect_length(tarefas, 2L)
  gh$runs[[1]]$estado <- "completed"; gh$runs[[1]]$conclusao <- "success"
  local_mocked_bindings(.td_pagina_no_ar = function(url) TRUE, .package = "trama.documentos")
  tarefas[[2]]()
  expect_identical(c$ultima("envio")$estado$etapa, "no_ar")
  expect_length(tarefas, 2L)
  # Reabrir mostra a publicação registrada e o último envio.
  c$receber(list(acao = "publicar_conferir", documento = doc))
  p <- c$ultima("publicacao")
  expect_true(p$registrado)
  expect_identical(p$ultima$versao, "0.1.0")
  expect_identical(p$envio$etapa, "no_ar")
})

test_that("bloqueio ao publicar volta como lista, sem tocar no GitHub", {
  gh <- github_memoria()
  x <- doc_publicavel()
  mudar_yaml(x$qmd, function(y) { y$author <- NULL; y })
  c <- controlador_de_teste(x$proj)
  c$receber(list(acao = "publicar", documento = "documentos/consumo/consumo.qmd"))
  expect_match(unlist(c$ultima("publicar_bloqueado")$erros), "autor", all = FALSE)
  expect_length(gh$chamadas, 0L)
})
