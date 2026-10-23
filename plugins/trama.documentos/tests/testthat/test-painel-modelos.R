# A aba Modelos do painel, sem navegador: as ações `modelos_*` do
# controlador e as mensagens que voltam.

controlador_modelos <- function(proj, abrir = function(caminho) NULL) {
  msgs <- list()
  withr::local_options(trama.documentos.abrir = abrir, .local_envir = parent.frame())
  ctl <- trama.documentos:::.td_controlador(
    enviar = function(m) msgs[[length(msgs) + 1L]] <<- m,
    agendar = function(f, seg) f(),
    ctx = list(project = function() proj, servir = function(dir, nome) paste0(nome, "/")))
  list(receber = ctl,
       ultima = function(tipo) {
         m <- Filter(function(x) identical(x$tipo, tipo), msgs)
         if (length(m)) m[[length(m)]]
       },
       tipos = function() vapply(msgs, `[[`, "", "tipo"))
}

base64 <- function(caminho) {
  list(nome = basename(caminho), conteudo = paste0("data:application/octet-stream;base64,",
                                                   jsonlite::base64_enc(readBin(caminho, "raw", file.info(caminho)$size))))
}

test_that("a lista do painel traz a galeria com validade, perfil e os formatos de cada documento", {
  proj <- local_projeto()
  tr_documento_criar("slides", proj, modelo = "slides", perfil = FALSE)
  c <- controlador_modelos(proj)
  c$receber(list(acao = "listar"))
  l <- c$ultima("lista")
  ufla <- Filter(function(m) m$nome == "ufla", l$modelos)[[1]]
  expect_identical(ufla$perfil, "aluno-ufla")
  expect_true(ufla$valido)
  expect_null(ufla$problema)
  expect_identical(l$documentos[[1]]$formatos, list("revealjs"))
  c$receber(list(acao = "modelos_listar"))
  expect_identical(length(c$ultima("modelos")$modelos), length(l$modelos))
})

test_that("detalhe, duplicar e editar pelo painel; formatos sempre como lista", {
  proj <- local_projeto()
  c <- controlador_modelos(proj)
  c$receber(list(acao = "modelos_detalhe", nome = "slides"))
  d <- c$ultima("modelo")$modelo
  expect_identical(d$formatos, list("revealjs"))
  expect_true(is.list(d$cores))

  c$receber(list(acao = "modelos_duplicar", de = "ufla", nome = "meu-tcc", titulo = "Meu TCC"))
  expect_identical(c$ultima("modelo_salvo")$nome, "meu-tcc")
  expect_identical(c$ultima("modelo")$modelo$titulo, "Meu TCC")
  expect_true("meu-tcc" %in% vapply(c$ultima("modelos")$modelos, `[[`, "", "nome"))

  logo <- withr::local_tempfile(fileext = ".svg")
  writeLines('<svg xmlns="http://www.w3.org/2000/svg" width="4" height="4"/>', logo)
  c$receber(list(acao = "modelos_editar", nome = "meu-tcc", formatos = list("typst"), cores = list(primaria = "#7b2d26"),
                 fontes = list(texto = "Lora"), capa = FALSE, perfil = "", logo = base64(logo)))
  expect_match(c$ultima("modelo_salvo")$mensagem, "Meu TCC")
  m <- tr_documento_modelo("meu-tcc")
  expect_identical(m$formatos, "typst")
  expect_identical(m$cores$primaria, "#7b2d26")
  expect_false(m$capa)
  expect_null(m$perfil)
  expect_identical(basename(m$logo), "logo.svg")
  expect_identical(unname(tools::md5sum(m$logo)), unname(tools::md5sum(logo)))
})

test_that("editar um embutido pelo painel avisa que virou cópia; voltar ao embutido desfaz", {
  proj <- local_projeto()
  c <- controlador_modelos(proj)
  c$receber(list(acao = "modelos_editar", nome = "basico", titulo = "Básico da casa"))
  expect_match(c$ultima("modelo_salvo")$mensagem, "cópia sua")
  expect_true(tr_documento_modelo("basico")$sobre_embutido)
  c$receber(list(acao = "modelos_remover", nome = "basico"))
  expect_match(c$ultima("modelo_salvo")$mensagem, "voltou ao embutido")
  expect_identical(tr_documento_modelo("basico")$titulo, "Básico")
  c$receber(list(acao = "modelos_remover", nome = "basico"))
  e <- c$ultima("erro")
  expect_identical(e$acao, "modelos_remover")
  expect_identical(e$classe, "tr_documento_erro_modelo")
})

test_that("exportar e importar pelo painel (o zip sobe em base64)", {
  proj <- local_projeto()
  abertos <- character()
  c <- controlador_modelos(proj, abrir = function(caminho) abertos <<- c(abertos, caminho))
  c$receber(list(acao = "modelos_exportar", nome = "consultoria"))
  z <- c$ultima("modelo_exportado")$arquivo
  expect_true(file.exists(z))
  expect_identical(basename(z), "consultoria.zip")
  c$receber(list(acao = "modelos_abrir", alvo = "exportados"))
  expect_identical(basename(abertos[[1]]), "exportados")

  copia <- file.path(withr::local_tempdir(), "minha-consultoria.zip")
  file.copy(z, copia)
  c$receber(list(acao = "modelos_importar", arquivo = base64(copia)))
  # O nome vem da pasta dentro do zip: a cópia do usuário por cima do embutido.
  expect_identical(c$ultima("modelo_salvo")$nome, "consultoria")
  expect_true(tr_documento_modelo("consultoria")$sobre_embutido)
  expect_match(c$ultima("modelo_salvo")$mensagem, "por cima do embutido")
  c$receber(list(acao = "modelos_importar", arquivo = base64(copia)))
  e <- c$ultima("erro")
  expect_identical(e$classe, "tr_documento_erro_modelo_existe")
  expect_identical(e$acao, "modelos_importar")
  c$receber(list(acao = "modelos_importar", arquivo = base64(copia), substituir = TRUE))
  expect_identical(c$ultima("modelo_salvo")$nome, "consultoria")
  c$receber(list(acao = "modelos_importar", arquivo = base64(copia), nome = "consultoria-2"))
  expect_identical(c$ultima("modelo_salvo")$nome, "consultoria-2")
  expect_identical(tr_documento_modelo("consultoria-2")$origem, "usuario")

  c$receber(list(acao = "modelos_abrir", nome = "consultoria"))
  expect_identical(basename(abertos[[2]]), "consultoria")
  c$receber(list(acao = "modelos_abrir", nome = "ufla"))
  expect_match(c$ultima("erro")$mensagem, "embutido")

  txt <- withr::local_tempfile(fileext = ".txt")
  writeLines("x", txt)
  c$receber(list(acao = "modelos_importar", arquivo = base64(txt)))
  expect_match(c$ultima("erro")$mensagem, ".zip")
  c$receber(list(acao = "modelos_importar", arquivo = list(nome = ".escondido.zip", conteudo = "AAAA")))
  expect_identical(c$ultima("erro")$classe, "tr_documento_erro_painel")
  c$receber(list(acao = "modelos_nao_existe"))
  expect_match(c$ultima("erro")$mensagem, "desconhecida")
})
