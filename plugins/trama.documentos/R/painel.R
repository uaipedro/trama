# As ações do painel Documentos. Sem Shiny: `enviar(mensagem)` fala com o
# JS, `agendar(f, segundos)` chama `f` mais tarde sem travar o R (no editor,
# `later`), e `ctx` é o do plugin (`project()`, `servir()`). Assim o
# controlador é testado inteiro sem navegador.
#
# Documentos viajam pelo caminho relativo à raiz do projeto
# (`documentos/<nome>/<nome>.qmd`), conferido antes de tocar o disco.

.td_controlador <- function(enviar, agendar, ctx) {
  projeto <- function() ctx$project()
  raiz <- function() projeto()$root
  doc <- function(m) .td_doc_rel(raiz(), m$documento)
  rel <- function(caminho) .td_relativo(raiz(), caminho)
  falhar <- function(acao, documento = NULL) function(e) {
    enviar(Filter(Negate(is.null), list(tipo = "erro", acao = acao, documento = documento,
                                        mensagem = conditionMessage(e), classe = class(e)[[1]])))
  }
  listar <- function() enviar(c(list(tipo = "lista"), .td_lista_painel(projeto())))
  # Migração do `trama.publicar` (retirado): na primeira lista da sessão,
  # o nó `publicar/publish` vira documento; o editor relê o fluxo e o painel
  # abre com o aviso.
  migrados <- character()
  migrar <- function() {
    if (raiz() %in% migrados) return(invisible())
    migrados <<- c(migrados, raiz())
    r <- tr_documento_migrar_publicar(raiz())
    if (!nrow(r)) return(invisible())
    if (is.function(ctx$recarregar)) ctx$recarregar()
    enviar(list(tipo = "migrado", documentos = .td_linhas_json(r)))
  }
  # Acompanhamento do envio ao GitHub: um passo a cada 5 segundos até a
  # página entrar no ar ou o workflow falhar (no máximo ~15 minutos). Cada
  # documento tem uma geração: envio novo (ou conferir de novo) recomeça, e o
  # laço velho para. Erro de rede passageiro tenta de novo; três seguidos param.
  geracoes <- list()
  acompanhar <- function(rel, novo = TRUE, g = NULL, passos = 0L, falhas = 0L) {
    if (novo) {
      g <- (geracoes[[rel]] %||% 0L) + 1L
      geracoes[[rel]] <<- g
    }
    if (!identical(geracoes[[rel]], g)) return(invisible())
    e <- tryCatch(tr_documento_acompanhar(file.path(raiz(), rel), raiz()), error = function(e) e)
    if (inherits(e, "error")) {
      if (falhas >= 2L) return(falhar("acompanhar", rel)(e))
      return(agendar(function() acompanhar(rel, FALSE, g, passos + 1L, falhas + 1L), 5))
    }
    if (is.null(e)) return(invisible())
    enviar(list(tipo = "envio", documento = rel, estado = e))
    if (e$etapa %in% c("no_ar", "falhou")) return(listar())
    if (passos >= 180L) return(invisible())
    agendar(function() acompanhar(rel, FALSE, g, passos + 1L, 0L), 5)
  }
  estado <- function(m) {
    d <- doc(m)
    e <- tr_documento_estado(d, projeto())
    enviar(list(tipo = "estado", documento = m$documento, regioes = .td_linhas_json(e)))
  }
  renderizar <- function(m, formato, depois, tarefa = formato) {
    d <- doc(m)
    enviar(list(tipo = "ocupado", documento = m$documento, tarefa = tarefa))
    .td_render_fundo(d, formato, agendar,
                     pronto = function(saida) tryCatch(depois(saida), error = falhar("render", m$documento)),
                     falhou = falhar("render", m$documento))
  }
  acoes <- list(
    listar = function(m) {
      tryCatch(migrar(), error = function(e) {
        enviar(list(tipo = "erro", acao = "migrar", mensagem = paste(
          "A migra\u{E7}\u{E3}o do bloco Publicar para documento falhou:", conditionMessage(e))))
      })
      listar()
    },
    estado = estado,
    criar = function(m) {
      perfil <- if (identical(m$perfil, "") || isFALSE(m$perfil)) FALSE else m$perfil
      qmd <- tr_documento_criar(.td_texto1(m$nome), projeto(), modelo = .td_texto1(m$modelo) %||% "basico",
                                perfil = perfil, titulo = .td_texto1(m$titulo))
      enviar(list(tipo = "criado", documento = rel(qmd)))
      listar()
    },
    atualizar = function(m) {
      enviar(list(tipo = "ocupado", documento = m$documento, tarefa = "atualizar"))
      r <- tr_documento_atualizar(doc(m), projeto())
      enviar(list(tipo = "atualizado", documento = m$documento, resultado = .td_linhas_json(r)))
      estado(m)
      listar()
    },
    inserir = function(m) {
      opcoes <- Filter(function(v) !is.null(v) && nzchar(as.character(v)), list(
        linhas = m$opcoes$linhas, tabela = if (isTRUE(m$opcoes$completa)) "completa"))
      r <- tr_documento_inserir(doc(m), .td_texto1(m$no), projeto(), porta = .td_texto1(m$porta),
                                opcoes = opcoes, aceitar_dados_treino = isTRUE(m$aceitar))
      enviar(list(tipo = "inserido", documento = m$documento, regiao = r$regiao, avisos = as.list(r$avisos)))
      estado(m)
      listar()
    },
    resolver = function(m) {
      r <- tr_documento_resolver(doc(m), .td_texto1(m$regiao), .td_texto1(m$como), projeto(),
                                 confirmar = isTRUE(m$confirmar))
      enviar(list(tipo = "resolvido", documento = m$documento, regiao = r$regiao))
      estado(m)
      listar()
    },
    # Slides e dashboard também são HTML: a prévia usa o primeiro que o
    # documento tem. Serve `_saida/` inteira: slides e dashboard ficam em
    # subpastas dela.
    visualizar = function(m) renderizar(m, .td_formato_previa(doc(m)), tarefa = "previa", function(saida) {
      d <- doc(m)
      nome <- paste0("documento-", gsub("[^A-Za-z0-9_-]", "-", basename(dirname(d))))
      url <- ctx$servir(file.path(dirname(d), "_saida"), nome)
      enviar(list(tipo = "previa", documento = m$documento, url = paste0(url, .td_saida_relativa(saida))))
    }),
    exportar = function(m) {
      formato <- match.arg(.td_texto1(m$formato) %||% "", c("html", "pdf", "docx", "revealjs", "dashboard"))
      renderizar(m, formato, function(saida) {
        enviar(list(tipo = "exportado", documento = m$documento, formato = formato, arquivo = rel(saida)))
      })
    },
    abrir = function(m) {
      d <- doc(m)
      alvo <- switch(.td_texto1(m$alvo) %||% "documento",
        documento = d,
        pasta = dirname(d),
        saida = .td_doc_saida(raiz(), m$arquivo),
        .td_erro("O que abrir? documento, pasta ou saida.", "tr_documento_erro_painel"))
      .td_abrir_externo(alvo)
      enviar(list(tipo = "aberto", documento = m$documento))
    },
    guia_quarto = function(m) enviar(list(tipo = "guia", passos = .td_guia_quarto())),
    # Publicar no GitHub (#25): conferir o que sai, publicar e acompanhar.
    publicar_conferir = function(m) {
      d <- doc(m)
      pub <- tr_documento_publicacao(d, projeto(), desatualizada = "aviso")
      e <- .td_envio_ler(raiz(), m$documento)
      enviar(c(list(tipo = "publicacao", documento = m$documento), .td_publicacao_json(pub, raiz(), m$documento),
               list(envio = e)))
      if (!is.null(e) && !e$etapa %in% c("no_ar", "falhou")) acompanhar(m$documento)
    },
    publicar = function(m) {
      d <- doc(m)
      enviar(list(tipo = "ocupado", documento = m$documento, tarefa = "publicar"))
      repo <- .td_texto1(m$repositorio)
      r <- tr_documento_publicar(d, projeto(), repositorio = if (is.null(repo) || !nzchar(trimws(repo))) NULL else trimws(repo),
                                 confirmado = isTRUE(m$confirmado))
      if (!is.null(r$bloqueios)) {
        return(enviar(list(tipo = "publicar_bloqueado", documento = m$documento, erros = r$bloqueios)))
      }
      if (!is.null(r$confirmar)) {
        return(enviar(list(tipo = "publicar_confirmar", documento = m$documento, texto = r$confirmar, repo = r$repo)))
      }
      enviar(list(tipo = "envio", documento = m$documento, estado = r))
      agendar(function() acompanhar(m$documento), 5)
    }
  )
  function(m) {
    acao <- .td_texto1(m$acao) %||% ""
    # Ações da aba Modelos (fase 3, #24) moram em `R/painel-modelos.R`.
    if (startsWith(acao, "modelos_")) {
      return(invisible(tryCatch(.td_painel_modelos(m, enviar, ctx), error = falhar(acao))))
    }
    f <- acoes[[acao]]
    if (is.null(f)) return(invisible(enviar(list(tipo = "erro", acao = acao, mensagem = sprintf("A\u{E7}\u{E3}o desconhecida: '%s'.", acao)))))
    tryCatch(f(m), error = falhar(acao, m$documento))
    invisible()
  }
}

#' A conferência da publicação para o painel: metadados, regiões, avisos,
#' bloqueios, o repositório (o do registro ou o sugerido pelo título) e a
#' última publicação.
#' @noRd
.td_publicacao_json <- function(pub, raiz, rel) {
  m <- pub$metadados
  reg <- .td_registro(raiz)[[rel]]
  list(titulo = m$titulo, autores = lapply(m$autores, function(a) a$nome), licenca = m$licenca,
       licenca_codigo = m$licenca_codigo, tipo_versao = m$tipo_versao, privado = isTRUE(m$privado),
       regioes = lapply(pub$regioes, function(r) list(chave = r$chave, titulo = r$titulo, tipo = r$tipo, bytes = r$bytes)),
       avisos = as.list(pub$avisos), erros = as.list(pub$erros),
       repo = reg$repo %||% .td_slug(m$titulo), registrado = !is.null(reg$repo),
       ultima = if (!is.null(reg$versao)) list(versao = reg$versao, pagina = reg$pagina, ultimo = reg$ultimo, data = reg$data))
}

#' O que o painel mostra ao abrir: documentos (com o resumo do estado),
#' modelos, perfis e se há Quarto.
#' @noRd
.td_lista_painel <- function(projeto) {
  docs <- tr_documento_listar(projeto)
  p <- tr_documento_perfis()
  list(
    documentos = lapply(seq_len(nrow(docs)), function(i) {
      e <- tryCatch(tr_documento_estado(docs$caminho[i], projeto), error = function(e) e)
      list(nome = docs$nome[i], caminho = .td_relativo(projeto$root, docs$caminho[i]),
           modelo = if (is.na(docs$modelo[i])) NULL else docs$modelo[i],
           formatos = as.list(tryCatch(tr_documento_formatos(docs$caminho[i]), error = function(e) character())),
           resumo = if (inherits(e, "error")) list(erro = conditionMessage(e)) else as.list(table(e$estado)))
    }),
    modelos = .td_modelos_json(),
    perfis = lapply(names(p$perfis), function(k) list(perfil = k, nome = p$perfis[[k]]$nome %||% k)),
    perfil_padrao = p$padrao,
    quarto = nzchar(.td_quarto()))
}

#' Caminho de documento vindo do navegador: `documentos/<nome>/<arquivo>.qmd`,
#' sem `..` nem nome oculto, e existente.
#' @noRd
.td_doc_rel <- function(raiz, rel) {
  rel <- .td_texto1(rel) %||% ""
  if (!grepl("^documentos/[^/.][^/]*/[^/.][^/]*\\.qmd$", rel)) {
    .td_erro(sprintf("Documento inv\u{E1}lido: '%s'.", rel), "tr_documento_erro_painel")
  }
  p <- file.path(raiz, rel)
  if (!file.exists(p)) .td_erro(sprintf("O documento '%s' n\u{E3}o existe mais.", rel), "tr_documento_erro_caminho")
  p
}

#' Arquivo exportado (em `documentos/<nome>/_saida/`, ou na subpasta dos
#' slides ou do dashboard), vindo do navegador.
#' @noRd
.td_doc_saida <- function(raiz, rel) {
  rel <- .td_texto1(rel) %||% ""
  padrao <- sprintf("^documentos/[^/.][^/]*/_saida/((%s)/)?[^/.][^/]*\\.(html|pdf|docx)$", paste(.TD_SUBPASTA, collapse = "|"))
  if (!grepl(padrao, rel) || !file.exists(file.path(raiz, rel))) {
    .td_erro(sprintf("Arquivo inv\u{E1}lido: '%s'.", rel), "tr_documento_erro_painel")
  }
  file.path(raiz, rel)
}

.td_relativo <- function(raiz, caminho) {
  r <- normalizePath(raiz, winslash = "/")
  c <- normalizePath(caminho, winslash = "/")
  if (startsWith(c, paste0(r, "/"))) substring(c, nchar(r) + 2L) else c
}

.td_texto1 <- function(x) if (is.null(x) || !length(x)) NULL else as.character(x)[[1]]

#' data.frame -> lista de linhas para o JSON (NA vira ausente).
#' @noRd
.td_linhas_json <- function(df) {
  lapply(seq_len(nrow(df)), function(i) Filter(function(v) !is.na(v), as.list(df[i, , drop = FALSE])))
}

#' Abre um arquivo ou pasta no programa padrão do sistema (o editor de texto
#' da pessoa, o leitor de PDF). Roda no R porque o R é o computador da
#' pessoa; o navegador não abre arquivo local. `options(trama.documentos.abrir
#' = function(caminho) ...)` troca o jeito (testes, ou um editor preferido).
#' @noRd
.td_abrir_externo <- function(caminho) {
  f <- getOption("trama.documentos.abrir")
  if (is.function(f)) return(invisible(f(caminho)))
  sis <- Sys.info()[["sysname"]]
  if (identical(sis, "Windows")) {
    shell.exec <- get("shell.exec", envir = baseenv())
    return(invisible(shell.exec(caminho)))
  }
  cmd <- if (identical(sis, "Darwin")) "open" else "xdg-open"
  processx::process$new(cmd, caminho, stdout = NULL, stderr = NULL, cleanup = FALSE)
  invisible(caminho)
}

#' Guia de instalação do Quarto, quando ele falta.
#' @noRd
.td_guia_quarto <- function() {
  list(
    list(titulo = "Baixe o Quarto",
         texto = "O Quarto transforma o documento em HTML, PDF e Word. Baixe o instalador do seu sistema na p\u{E1}gina oficial.",
         link = "https://quarto.org/docs/get-started/", rotulo = "Abrir quarto.org"),
    list(titulo = "Instale",
         texto = "Rode o instalador com as op\u{E7}\u{F5}es padr\u{E3}o. O PDF sai pelo Typst, que vem junto: n\u{E3}o precisa instalar LaTeX."),
    list(titulo = "Reabra o trama",
         texto = "Feche e abra o trama de novo para ele achar o Quarto. Sem Quarto, d\u{E1} para escrever e atualizar o documento; s\u{F3} a pr\u{E9}via e a exporta\u{E7}\u{E3}o esperam."))
}
