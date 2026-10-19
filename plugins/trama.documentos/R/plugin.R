#' Painel Documentos no editor
#'
#' Contrato de plugin do trama (ver `trama::tr_plugins()`): o editor carrega
#' `inst/trama/documentos.js` (o botão Documentos na barra e o item "Inserir
#' no documento…" no menu do card) e liga o server abaixo a cada sessão. O
#' canal é próprio: o JS manda `input$trama_documentos` (`{acao, ...}`) e o R
#' responde com a mensagem `trama_documentos` (`{tipo, ...}`).
#'
#' @return Lista com `js`, `css` e `server`.
#' @export
trama_plugin <- function() {
  list(js = "trama/documentos.js", css = "trama/documentos.css", server = .td_servidor)
}

.td_servidor <- function(input, output, session, ctx) {
  receber <- .td_controlador(
    enviar = function(m) session$sendCustomMessage("trama_documentos", m),
    agendar = function(f, seg) later::later(function() if (!session$isClosed()) f(), seg),
    ctx = ctx)
  shiny::observeEvent(input$trama_documentos, receber(input$trama_documentos))
}
