#' Mensagens do pacote, fora do código R.
#'
#' As mensagens do trama são em português e têm acento. Acento dentro de string
#' de código R é WARNING no `R CMD check` ("Portable packages must use only
#' ASCII characters in their R code"), e a saída que o manual oferece é escapar
#' cada letra (`"Coleção"`), o que deixa o fonte ilegível justamente
#' nas linhas que o usuário vai ler. Comentário acentuado NÃO é acusado — só
#' string —, então o catálogo resolve o aviso sem mexer em nada mais.
#'
#' O texto mora em `inst/mensagens/pt-BR.json`, um arquivo por língua, e o
#' código R fica ASCII. Em JSON, e não `.rda`, porque mensagem se revisa em
#' diff: um `.rda` seria um blob opaco no histórico.
#'
#' Lido no primeiro uso, não no `.onLoad`: sessão que nunca erra não paga a
#' leitura, e o arquivo é pequeno o bastante para a primeira falha não custar
#' nada perceptível.
#' @noRd
.tr_msgs <- new.env(parent = emptyenv())

#' Texto da mensagem `chave`, formatado com `...` por `sprintf()`.
#'
#' Chave ausente é ERRO, não silêncio nem a própria chave como texto: uma
#' mensagem faltando só aparece no dia em que o usuário erra, que é o pior
#' momento possível para descobrir. O teste que confere catálogo contra código
#' (em `test-mensagens.R`) existe pelo mesmo motivo.
#' @noRd
.tr_msg <- function(chave, ...) {
  if (is.null(.tr_msgs$catalogo)) .tr_msgs$catalogo <- .tr_msg_carrega()
  txt <- .tr_msgs$catalogo[[chave]]
  if (is.null(txt)) {
    rlang::abort(sprintf("Mensagem '%s' nao esta no catalogo de mensagens.", chave),
                 class = "tr_error_missing_message")
  }
  if (!...length()) return(txt)
  sprintf(txt, ...)
}

#' Lê o catálogo do `inst/` do pacote.
#'
#' `system.file()` e não caminho montado à mão: em desenvolvimento
#' (`pkgload::load_all()`) o pacote não está instalado, e é o `system.file()`
#' que o pkgload redireciona. A leitura é `simplifyVector = TRUE` porque o
#' arquivo é um objeto plano de chave para texto — o resultado é um vetor
#' nomeado de character, que é o que `[[` acima espera.
#' @noRd
.tr_msg_carrega <- function() {
  caminho <- system.file("mensagens", "pt-BR.json", package = "trama")
  if (!nzchar(caminho)) {
    rlang::abort("Catalogo de mensagens nao encontrado na instalacao do trama.",
                 class = "tr_error_missing_message")
  }
  as.list(jsonlite::fromJSON(caminho, simplifyVector = TRUE))
}
