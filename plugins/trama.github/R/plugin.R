#' Menu GitHub no editor
#'
#' Contrato de plugin do trama (ver `trama::tr_plugins()`): o editor
#' carrega `inst/trama/github.js` e liga o server abaixo a cada sessão. O canal
#' é próprio: o JS manda `input$trama_github` (`{acao}`) e o R responde com a
#' mensagem `trama_github` (`{tipo, ...}`).
#'
#' @return Lista com `js`, `css` e `server`.
#' @export
trama_plugin <- function() {
  list(js = "trama/github.js", css = "trama/github.css", server = .gh_servidor)
}

.gh_servidor <- function(input, output, session, ctx) {
  receber <- .gh_controlador(
    enviar = function(m) session$sendCustomMessage("trama_github", m),
    agendar = function(f, seg) later::later(function() if (!session$isClosed()) f(), max(seg, 1)))
  shiny::observeEvent(input$trama_github, receber(input$trama_github))
}

# Ações do menu. `enviar(mensagem)` fala com o JS; `agendar(f, segundos)`
# chama `f` mais tarde sem travar o R (no editor, `later`). A conferência do
# device flow vai se reagendando até dar certo, expirar ou ser cancelada; a
# `geracao` invalida conferências de um login cancelado ou recomeçado.
.gh_controlador <- function(enviar, agendar) {
  geracao <- 0L
  mandar_estado <- function() enviar(c(list(tipo = "estado"), .gh_estado()))
  falhar <- function(msg) {
    geracao <<- geracao + 1L
    enviar(list(tipo = "erro", mensagem = msg))
  }
  conferir <- function(g, p) {
    if (g != geracao) return(invisible())
    r <- tryCatch(tr_github_device_conferir(p), tr_github_erro = function(e) {
      list(estado = "erro", mensagem = conditionMessage(e))
    })
    switch(r$estado,
      pendente = {
        p$intervalo <- r$intervalo
        agendar(function() conferir(g, p), r$intervalo)
      },
      ok = {
        geracao <<- geracao + 1L
        tr_github_salvar_token(r$token)
        mandar_estado()
      },
      expirado = falhar("O código expirou antes da autorização. Tente de novo."),
      negado = falhar("A autorização foi recusada no GitHub."),
      falhar(r$mensagem %||% "O login não deu certo. Tente de novo."))
    invisible()
  }
  entrar <- function() {
    p <- tryCatch(tr_github_device_iniciar(), tr_github_erro = function(e) e)
    if (inherits(p, "tr_github_erro")) return(falhar(conditionMessage(p)))
    geracao <<- geracao + 1L
    g <- geracao
    enviar(list(tipo = "codigo", codigo = p$codigo, link = p$link,
                expira = as.numeric(difftime(p$expira_em, Sys.time(), units = "secs"))))
    agendar(function() conferir(g, p), p$intervalo)
  }
  function(m) {
    switch(m$acao %||% "",
      estado = mandar_estado(),
      entrar = entrar(),
      cancelar = { geracao <<- geracao + 1L; mandar_estado() },
      sair = { tr_github_sair(); mandar_estado() },
      guia = enviar(list(tipo = "guia", passos = tr_github_guia_conta())))
    invisible()
  }
}

# Estado da conexão para o menu. Confere a credencial no GitHub: um token
# vencido aparece como "não conectado", com o motivo.
.gh_estado <- function() {
  base <- list(login_disponivel = nzchar(tr_github_client_id(obrigatorio = FALSE)),
               persistente = tr_github_chaveiro()$persistente)
  tk <- tr_github_token()
  if (is.null(tk)) return(c(base, list(conectado = FALSE)))
  u <- tryCatch(tr_github_usuario(tk), tr_github_erro = function(e) e)
  if (inherits(u, "tr_github_erro")) {
    return(c(base, list(conectado = FALSE, fonte = attr(tk, "fonte"), erro = conditionMessage(u))))
  }
  c(base, list(conectado = TRUE, fonte = attr(tk, "fonte"), usuario = u))
}
