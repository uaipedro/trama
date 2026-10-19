#' Endereços do GitHub
#'
#' `tr_github_api_url()` é a API REST (`https://api.github.com`) e
#' `tr_github_url()` é o site (`https://github.com`), onde moram as páginas do
#' device flow e do cadastro. Os dois mudam por opção (`trama.github.api_url`,
#' `trama.github.url`) ou variável de ambiente (`TRAMA_GITHUB_API_URL`,
#' `TRAMA_GITHUB_URL`): é assim que os testes apontam para um GitHub falso.
#' @return Uma URL sem barra no fim.
#' @export
tr_github_api_url <- function() {
  .gh_config("trama.github.api_url", "TRAMA_GITHUB_API_URL", "https://api.github.com")
}

#' @rdname tr_github_api_url
#' @export
tr_github_url <- function() {
  .gh_config("trama.github.url", "TRAMA_GITHUB_URL", "https://github.com")
}

# Opção do R > variável de ambiente > padrão. Vazio conta como ausente.
.gh_config <- function(opcao, env, padrao) {
  v <- getOption(opcao)
  if (is.null(v) || !nzchar(v)) v <- Sys.getenv(env)
  if (!nzchar(v)) v <- padrao
  sub("/+$", "", v)
}

# Requisição à API REST. Não lança erro HTTP sozinha: `.gh_chamar()` traduz o
# status para uma condição com mensagem em português.
.gh_req <- function(caminho, token = NULL, base = tr_github_api_url()) {
  req <- httr2::request(paste0(base, caminho))
  req <- httr2::req_headers(req, Accept = "application/vnd.github+json",
                            `X-GitHub-Api-Version` = "2022-11-28")
  req <- httr2::req_user_agent(req, "trama.github (https://github.com/uaipedro/trama)")
  if (!is.null(token)) req <- httr2::req_auth_bearer_token(req, token)
  httr2::req_error(req, is_error = function(resp) FALSE)
}

# Faz a chamada e devolve o JSON como lista (sem simplificar: blobs, árvores e
# refs são listas de objetos e o código que os lê espera `[[`).
.gh_chamar <- function(caminho, metodo = "GET", corpo = NULL, token = NULL,
                       base = tr_github_api_url()) {
  req <- httr2::req_method(.gh_req(caminho, token, base), metodo)
  if (!is.null(corpo)) req <- httr2::req_body_json(req, corpo, auto_unbox = TRUE, null = "null")
  resp <- .gh_executar(req)
  if (httr2::resp_status(resp) >= 400) .gh_erro_http(resp)
  if (httr2::resp_status(resp) == 204 || !length(httr2::resp_body_raw(resp))) return(invisible(NULL))
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

.gh_executar <- function(req) {
  tryCatch(httr2::req_perform(req), httr2_failure = function(e) {
    .gh_parar("tr_github_erro_rede",
              "Sem conexão com o GitHub. Confira a internet e tente de novo.",
              detalhe = conditionMessage(e))
  })
}

.gh_parar <- function(classe, mensagem, status = NA_integer_, detalhe = NULL) {
  stop(structure(class = c(classe, "tr_github_erro", "error", "condition"),
                 list(message = mensagem, call = NULL, status = status, detalhe = detalhe)))
}

# O GitHub explica o erro em `message` e, nos 422, em `errors[].message`.
.gh_erro_http <- function(resp) {
  status <- httr2::resp_status(resp)
  corpo <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) list())
  detalhe <- paste(c(corpo$message,
                     vapply(corpo$errors %||% list(), function(x) as.character(x$message %||% x$code %||% ""), "")),
                   collapse = ": ")
  limite <- identical(httr2::resp_header(resp, "x-ratelimit-remaining"), "0")
  switch(as.character(status),
    "401" = .gh_parar("tr_github_erro_credencial",
      "O GitHub não aceitou a credencial: ela expirou ou foi revogada. Entre de novo.", status, detalhe),
    "403" = if (limite) .gh_parar("tr_github_erro_limite",
      "O GitHub limitou as chamadas por enquanto. Espere alguns minutos e tente de novo.", status, detalhe)
      else .gh_parar("tr_github_erro_permissao",
      "Sem permissão no GitHub para isso. A credencial precisa dos escopos `repo` e `workflow`.", status, detalhe),
    "404" = .gh_parar("tr_github_erro_nao_encontrado",
      "Não encontrado no GitHub (ou a credencial não tem acesso).", status, detalhe),
    "409" = .gh_parar("tr_github_erro_conflito",
      "O GitHub recusou por conflito no repositório.", status, detalhe),
    "422" = .gh_parar("tr_github_erro_validacao",
      paste0("O GitHub recusou o pedido: ", detalhe), status, detalhe),
    .gh_parar("tr_github_erro_http",
      sprintf("O GitHub respondeu com erro %d: %s", status, detalhe), status, detalhe))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
