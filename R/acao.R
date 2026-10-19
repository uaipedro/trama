#' Ação de card: o editor pede, sobre um resultado gravado, uma das `acoes`
#' do tipo dele (`tr_type(acoes = )`).
#'
#' Fora da execução de propósito: rodar o fluxo é puro e cacheável, e o que
#' tem efeito no mundo (servir uma prévia, publicar) só acontece quando alguém
#' aperta o botão. A ação recebe o valor lido do store pela chave que o card
#' mostra, então age sobre exatamente o resultado que está na tela.
#'
#' `req` vem do navegador: `tipo`, `chave` e `acao` são conferidos antes de
#' tocar o disco (a chave vira caminho no store).
#' @return invisível; a resposta sai por `responder()`.
#' @noRd
.tr_acao_rodar <- function(req, proj, doc, flow, responder, servir) {
  tipo <- as.character(req$tipo %||% "")[1]
  chave <- as.character(req$chave %||% "")[1]
  acao <- as.character(req$acao %||% "")[1]
  recusa <- function(msg) rlang::abort(msg, class = "tr_error_bad_action")
  if (!grepl("^[A-Za-z0-9_.:-]+$", chave) || grepl("..", chave, fixed = TRUE)) recusa("Chave de resultado inválida.")
  ty <- proj$registry$types[[tipo]]
  if (is.null(ty)) recusa(sprintf("Tipo desconhecido: '%s'.", tipo))
  fn <- ty$acoes[[acao]]
  if (is.null(fn)) recusa(sprintf("O tipo '%s' não tem a ação '%s'.", tipo, acao))
  h <- tr_store_handle(proj$store, chave)
  if (is.null(h) || !identical(h$type, tipo)) recusa("O resultado não está mais no store: rode o fluxo de novo.")
  valor <- tr_store_get(proj$store, chave, ty)
  ctx <- list(doc = doc, flow = flow, raiz = proj$root, store = proj$store, registry = proj$registry,
              chave = chave, servir = servir, responder = responder, extra = req$extra)
  res <- fn(valor, ctx)
  if (!is.null(res)) responder(res)
  invisible()
}

#' Serve uma pasta pelo servidor do editor: `trama-acao-<nome>/`.
#'
#' Pelo próprio Shiny, e não por um servidor à parte: mesma origem do editor,
#' nenhuma porta nova, e a pasta sai do ar com ele. `nome` vira prefixo de
#' URL, então só letras, dígitos, `-` e `_`.
#' @noRd
.tr_acao_servir <- function(dir, nome) {
  if (!grepl("^[A-Za-z0-9_-]+$", nome)) {
    rlang::abort(sprintf("Nome de pasta servida inválido: '%s'.", nome), class = "tr_error_bad_action")
  }
  prefixo <- paste0("trama-acao-", nome)
  shiny::addResourcePath(prefixo, normalizePath(dir, mustWork = TRUE))
  paste0(prefixo, "/")
}
