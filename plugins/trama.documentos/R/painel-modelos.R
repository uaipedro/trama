# Aba Modelos do painel Documentos: a galeria de modelos (fase 3, #24).
#
# Toda ação do JS cujo nome começa com `modelos_` chega aqui (ver
# `.td_controlador()`), com o mesmo `enviar` e o mesmo `ctx`; o estado da
# aba é o redutor de `inst/trama/modelos.js` e o desenho, `modelos-ui.js`.
# Depois de mudar a galeria, a lista vai de novo (`modelos`), que é também a
# do "novo documento".
#
# Arquivos (logo, CSL, zip para importar) chegam do navegador em base64,
# `{nome, conteudo}`: o navegador não dá o caminho do arquivo escolhido, e os
# arquivos de modelo são pequenos. São gravados num temporário com o nome
# original (a extensão decide o tipo) e passados à API da galeria, que
# confere tudo.

.TD_MAX_ARQUIVO <- 20 * 1024^2

.td_painel_modelos <- function(m, enviar, ctx) {
  nome <- function() .td_texto1(m$nome) %||% .td_erro("Qual modelo?", "tr_documento_erro_painel")
  lista <- function() enviar(list(tipo = "modelos", modelos = .td_modelos_json()))
  salvo <- function(nome, mensagem) {
    enviar(Filter(Negate(is.null), list(tipo = "modelo_salvo", nome = nome, mensagem = mensagem)))
    lista()
  }
  tmp <- tempfile("modelo-painel-")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  switch(m$acao,
    modelos_listar = lista(),
    modelos_detalhe = enviar(list(tipo = "modelo", modelo = .td_modelo_json(tr_documento_modelo(nome())))),
    modelos_duplicar = {
      d <- tr_documento_modelo_duplicar(.td_texto1(m$de) %||% "basico", nome(), titulo = .td_texto1(m$titulo))
      salvo(d$nome, sprintf("Modelo '%s' criado: ajuste-o abaixo.", d$titulo))
      enviar(list(tipo = "modelo", modelo = .td_modelo_json(d)))
    },
    modelos_editar = {
      era_embutido <- identical(tr_documento_modelo(nome())$origem, "embutido")
      arquivo <- function(x) if (is.null(x) || isFALSE(x)) x else .td_arquivo_do_navegador(x, tmp)
      e <- tr_documento_modelo_editar(
        nome(), titulo = .td_texto1(m$titulo), descricao = .td_texto1(m$descricao),
        formatos = if (!is.null(m$formatos)) as.character(unlist(m$formatos)),
        cores = if (length(m$cores)) lapply(m$cores, as.character),
        fontes = if (length(m$fontes)) lapply(m$fontes, as.character),
        logo = arquivo(m$logo), capa = if (is.logical(m$capa)) m$capa, csl = arquivo(m$csl),
        perfil = if (isFALSE(m$perfil) || identical(m$perfil, "")) FALSE else .td_texto1(m$perfil))
      salvo(e$nome, if (era_embutido) {
        sprintf(paste("Salvo como c\u{F3}pia sua de '%s', que vale para documentos novos; o embutido fica",
                      "intacto (\u{201C}Voltar ao embutido\u{201D} desfaz)."), e$titulo)
      } else sprintf("Modelo '%s' salvo. Vale para documentos novos.", e$titulo))
    },
    modelos_remover = {
      n <- nome()
      voltou <- isTRUE(tr_documento_modelo(n)$sobre_embutido)
      tr_documento_modelo_remover(n)
      salvo(if (voltou) n, if (voltou) sprintf("'%s' voltou ao embutido.", n) else sprintf("Modelo '%s' removido.", n))
    },
    modelos_exportar = {
      n <- nome()
      pasta <- .td_dir_usuario("exportados")
      dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
      z <- tr_documento_modelo_exportar(n, file.path(pasta, paste0(n, ".zip")))
      enviar(list(tipo = "modelo_exportado", nome = n, arquivo = z))
    },
    modelos_importar = {
      z <- .td_arquivo_do_navegador(m$arquivo, tmp)
      if (!grepl("\\.zip$", z, ignore.case = TRUE)) .td_erro("Escolha um arquivo .zip.", "tr_documento_erro_modelo")
      i <- tr_documento_modelo_importar(z, substituir = isTRUE(m$substituir))
      salvo(i$nome, sprintf("Modelo '%s' importado.", i$titulo))
    },
    modelos_abrir = {
      caminho <- if (identical(.td_texto1(m$alvo), "exportados")) .td_dir_usuario("exportados") else {
        d <- tr_documento_modelo(nome())
        if (!identical(d$origem, "usuario")) {
          .td_erro("Modelo embutido n\u{E3}o se edita no lugar: duplique ou edite (cria a sua c\u{F3}pia).",
                   "tr_documento_erro_modelo")
        }
        d$caminho
      }
      .td_abrir_externo(caminho)
      enviar(list(tipo = "aberto"))
    },
    enviar(list(tipo = "erro", acao = m$acao, mensagem = sprintf("A\u{E7}\u{E3}o de modelos desconhecida: '%s'.", m$acao))))
  invisible()
}

#' A galeria para o JS (a mesma da lista do painel).
#' @noRd
.td_modelos_json <- function() {
  md <- tr_documento_modelos()
  lapply(seq_len(nrow(md)), function(i) {
    Filter(Negate(is.null), list(
      nome = md$nome[i], titulo = md$titulo[i], formatos = md$formatos[i], origem = md$origem[i],
      descricao = if (!is.na(md$descricao[i])) md$descricao[i], perfil = if (!is.na(md$perfil[i])) md$perfil[i],
      sobre_embutido = md$sobre_embutido[i], valido = md$valido[i],
      problema = if (!is.na(md$problema[i])) md$problema[i]))
  })
}

#' O detalhe para o formulário: vetores viram listas (o JSON sai com
#' `auto_unbox`, e um formato só viraria texto em vez de lista).
#' @noRd
.td_modelo_json <- function(d) {
  d$formatos <- as.list(d$formatos)
  d$formatos_desligados <- as.list(d$formatos_desligados)
  d$problemas <- as.list(d$problemas)
  if (!length(d$cores)) d$cores <- structure(list(), names = character())
  if (!length(d$fontes)) d$fontes <- structure(list(), names = character())
  Filter(Negate(is.null), d)
}

#' Arquivo que veio do navegador (`{nome, conteudo}` em base64) num
#' temporário com o nome original.
#' @noRd
.td_arquivo_do_navegador <- function(x, pasta) {
  nome <- basename(.td_texto1(x$nome) %||% "")
  conteudo <- .td_texto1(x$conteudo) %||% ""
  if (!nzchar(nome) || !nzchar(conteudo) || startsWith(nome, ".")) {
    .td_erro("Arquivo vazio ou sem nome.", "tr_documento_erro_painel")
  }
  conteudo <- sub("^data:[^,]*,", "", conteudo)
  if (nchar(conteudo) > .TD_MAX_ARQUIVO * 4 / 3) .td_erro("Arquivo grande demais (m\u{E1}ximo 20 MB).", "tr_documento_erro_painel")
  bytes <- tryCatch(jsonlite::base64_dec(conteudo), error = function(e) {
    .td_erro("N\u{E3}o foi poss\u{ED}vel ler o arquivo enviado.", "tr_documento_erro_painel")
  })
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(pasta, nome)
  writeBin(bytes, f)
  f
}
