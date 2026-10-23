# Escopos pedidos no login: `repo` cria e escreve em repositórios, inclusive
# privados; `workflow` permite gravar `.github/workflows/`, que a publicação
# (fase 2) usa para gerar release e página.
.ESCOPOS <- c("repo", "workflow")

#' Login pelo device flow do GitHub
#'
#' O mesmo caminho do `gh auth login`: o trama pede um código ao GitHub, a
#' pessoa abre o link no navegador, digita o código e autoriza. Não precisa de
#' client secret nem de servidor de retorno.
#'
#' `tr_github_device_iniciar()` pede o código; `tr_github_device_conferir()`
#' pergunta uma vez se a pessoa já autorizou (o editor chama em intervalos sem
#' travar a tela); `tr_github_entrar()` faz tudo no console e guarda o token no
#' chaveiro.
#'
#' @param client_id Client ID do OAuth App (ver [tr_github_client_id()]).
#' @param escopos Escopos pedidos.
#' @return `tr_github_device_iniciar()`: lista com `codigo` (o que a pessoa
#'   digita), `link`, `device_code`, `intervalo` (segundos entre conferências)
#'   e `expira_em`.
#' @export
tr_github_device_iniciar <- function(client_id = tr_github_client_id(), escopos = .ESCOPOS) {
  r <- .gh_form("/login/device/code", list(client_id = client_id, scope = paste(escopos, collapse = " ")))
  if (!is.null(r$error)) .gh_erro_device(r)
  list(codigo = r$user_code, link = r$verification_uri, device_code = r$device_code,
       intervalo = as.numeric(r$interval %||% 5), client_id = client_id,
       expira_em = Sys.time() + as.numeric(r$expires_in %||% 900))
}

#' @param pedido O que `tr_github_device_iniciar()` devolveu.
#' @return `tr_github_device_conferir()`: lista com `estado` (`"pendente"`,
#'   `"ok"`, `"expirado"` ou `"negado"`), `intervalo` e, quando `"ok"`, `token`.
#' @rdname tr_github_device_iniciar
#' @export
tr_github_device_conferir <- function(pedido) {
  if (Sys.time() > pedido$expira_em) return(list(estado = "expirado", intervalo = pedido$intervalo))
  r <- .gh_form("/login/oauth/access_token", list(
    client_id = pedido$client_id, device_code = pedido$device_code,
    grant_type = "urn:ietf:params:oauth:grant-type:device_code"))
  if (!is.null(r$access_token)) return(list(estado = "ok", token = r$access_token, intervalo = pedido$intervalo))
  switch(r$error %||% "",
    authorization_pending = list(estado = "pendente", intervalo = pedido$intervalo),
    # O GitHub pede para ir mais devagar e diz o novo intervalo; sem ele, +5 s.
    slow_down = list(estado = "pendente", intervalo = as.numeric(r$interval %||% (pedido$intervalo + 5))),
    expired_token = list(estado = "expirado", intervalo = pedido$intervalo),
    access_denied = list(estado = "negado", intervalo = pedido$intervalo),
    .gh_erro_device(r))
}

#' @param abrir Abrir o link no navegador.
#' @param salvar Guardar o token no chaveiro.
#' @return `tr_github_entrar()`: o usuário conectado (ver [tr_github_usuario()]),
#'   invisível.
#' @rdname tr_github_device_iniciar
#' @export
tr_github_entrar <- function(abrir = interactive(), salvar = TRUE) {
  p <- tr_github_device_iniciar()
  message(sprintf("Abra %s e digite o código %s", p$link, p$codigo))
  if (abrir) utils::browseURL(p$link)
  intervalo <- p$intervalo
  repeat {
    Sys.sleep(intervalo)
    r <- tr_github_device_conferir(p)
    intervalo <- r$intervalo
    if (r$estado == "ok") break
    if (r$estado == "expirado") .gh_parar("tr_github_erro_login", "O código expirou antes da autorização. Tente de novo.")
    if (r$estado == "negado") .gh_parar("tr_github_erro_login", "A autorização foi recusada no GitHub.")
  }
  if (salvar) {
    tr_github_salvar_token(r$token)
    if (!tr_github_chaveiro()$persistente) {
      message("Este sistema não tem um chaveiro seguro disponível: o login vale até fechar o R.")
    }
  }
  u <- tr_github_usuario(r$token)
  message(sprintf("Conectado como %s.", u$login))
  invisible(u)
}

# As rotas do device flow são do site (github.com), não da API, e respondem
# 200 com `error` no corpo; pedindo JSON, o corpo vem em JSON.
.gh_form <- function(caminho, campos) {
  req <- httr2::request(paste0(tr_github_url(), caminho))
  req <- httr2::req_headers(req, Accept = "application/json")
  req <- httr2::req_user_agent(req, "trama.github (https://github.com/uaipedro/trama)")
  req <- do.call(httr2::req_body_form, c(list(req), campos))
  req <- httr2::req_error(.gh_paciencia(req), is_error = function(resp) FALSE)
  resp <- .gh_executar(req)
  if (httr2::resp_status(resp) >= 400) .gh_erro_http(resp)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

.gh_erro_device <- function(r) {
  msg <- switch(r$error,
    device_flow_disabled = "O app do trama no GitHub está sem o device flow habilitado.",
    incorrect_client_credentials = "O Client ID configurado não é de um app válido no GitHub.",
    paste0("O GitHub recusou o login: ", r$error_description %||% r$error))
  .gh_parar("tr_github_erro_login", msg, detalhe = r$error)
}
