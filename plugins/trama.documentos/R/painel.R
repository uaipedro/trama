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
  estado <- function(m) {
    d <- doc(m)
    e <- tr_documento_estado(d, projeto())
    enviar(list(tipo = "estado", documento = m$documento, regioes = .td_linhas_json(e)))
  }
  renderizar <- function(m, formato, depois) {
    d <- doc(m)
    enviar(list(tipo = "ocupado", documento = m$documento, tarefa = if (formato == "html") "previa" else formato))
    .td_render_fundo(d, formato, agendar,
                     pronto = function(saida) tryCatch(depois(saida), error = falhar("render", m$documento)),
                     falhou = falhar("render", m$documento))
  }
  acoes <- list(
    listar = function(m) listar(),
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
    visualizar = function(m) renderizar(m, "html", function(saida) {
      nome <- paste0("documento-", gsub("[^A-Za-z0-9_-]", "-", basename(dirname(dirname(saida)))))
      url <- ctx$servir(dirname(saida), nome)
      enviar(list(tipo = "previa", documento = m$documento, url = paste0(url, basename(saida))))
    }),
    exportar = function(m) {
      formato <- match.arg(.td_texto1(m$formato) %||% "", c("html", "pdf", "docx"))
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
    guia_quarto = function(m) enviar(list(tipo = "guia", passos = .td_guia_quarto()))
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

#' O que o painel mostra ao abrir: documentos (com o resumo do estado),
#' modelos, perfis e se há Quarto.
#' @noRd
.td_lista_painel <- function(projeto) {
  docs <- tr_documento_listar(projeto)
  m <- tr_documento_modelos()
  p <- tr_documento_perfis()
  list(
    documentos = lapply(seq_len(nrow(docs)), function(i) {
      e <- tryCatch(tr_documento_estado(docs$caminho[i], projeto), error = function(e) e)
      list(nome = docs$nome[i], caminho = .td_relativo(projeto$root, docs$caminho[i]),
           modelo = if (is.na(docs$modelo[i])) NULL else docs$modelo[i],
           resumo = if (inherits(e, "error")) list(erro = conditionMessage(e)) else as.list(table(e$estado)))
    }),
    modelos = lapply(seq_len(nrow(m)), function(i) list(nome = m$nome[i], titulo = m$titulo[i],
                                                         formatos = m$formatos[i], origem = m$origem[i])),
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

#' Arquivo exportado (em `documentos/<nome>/_saida/`), vindo do navegador.
#' @noRd
.td_doc_saida <- function(raiz, rel) {
  rel <- .td_texto1(rel) %||% ""
  if (!grepl("^documentos/[^/.][^/]*/_saida/[^/.][^/]*\\.(html|pdf|docx)$", rel) || !file.exists(file.path(raiz, rel))) {
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
