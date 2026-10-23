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
  # O nó cujo resultado é este, achado pelo plano só quando a ação pede (a
  # chave é do conteúdo; o nó é o que o projeto usa para guardar o que é dele).
  no <- function() .tr_acao_no(doc, proj, chave)
  ctx <- list(doc = doc, flow = flow, raiz = proj$root, store = proj$store, registry = proj$registry,
              settings = proj$settings, chave = chave, no = no, servir = servir, responder = responder, extra = req$extra)
  res <- fn(valor, ctx)
  if (!is.null(res)) responder(res)
  invisible()
}

#' O id do nó que produziu a chave, pelo plano do documento atual (na raiz do
#' projeto, como o editor: quem lê arquivo resolve o caminho relativo a ela).
#' @noRd
.tr_acao_no <- function(doc, proj, chave) {
  antes <- setwd(proj$root)
  on.exit(setwd(antes))
  plano <- tr_plan(doc, registry = proj$registry, store = proj$store, settings = proj$settings)
  for (u in plano$units) if (chave %in% unlist(u$outputs)) return(u$node)
  NULL
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
