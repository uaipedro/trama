#' Credencial do GitHub
#'
#' `tr_github_token()` procura, nesta ordem:
#'
#' 1. o token que o próprio trama guardou no chaveiro do sistema, no login;
#' 2. as variáveis de ambiente `GITHUB_PAT` e `GITHUB_TOKEN`;
#' 3. o `gh` (GitHub CLI), por `gh auth token`;
#' 4. o gerenciador de credenciais do git, pelo `gitcreds` (o mesmo que o
#'    `usethis` usa).
#'
#' O token nunca é gravado no fluxo, no projeto ou no pacote publicado: só no
#' chaveiro (`keyring`, serviço `trama-github`).
#'
#' @return O token (texto) com o atributo `fonte` (`"chaveiro"`, `"GITHUB_PAT"`,
#'   `"GITHUB_TOKEN"`, `"gh"` ou `"gitcreds"`), ou `NULL` se não houver.
#' @export
tr_github_token <- function() {
  fontes <- list(
    chaveiro = .gh_token_chaveiro,
    GITHUB_PAT = function() Sys.getenv("GITHUB_PAT"),
    GITHUB_TOKEN = function() Sys.getenv("GITHUB_TOKEN"),
    gh = .gh_token_gh_cli,
    gitcreds = .gh_token_gitcreds
  )
  for (nome in names(fontes)) {
    tk <- tryCatch(fontes[[nome]](), error = function(e) NULL)
    if (is.character(tk) && length(tk) == 1L && nzchar(tk)) return(structure(tk, fonte = nome))
  }
  NULL
}

# Token pedido por quem chama a API: o explícito ou o que `tr_github_token()`
# achar. Sem nenhum, erro que diz o que fazer.
.gh_token_ou_erro <- function(token = NULL) {
  token <- token %||% tr_github_token()
  if (is.null(token)) {
    .gh_parar("tr_github_erro_sem_credencial",
              "Você ainda não entrou no GitHub. Use \"Entrar\" no menu GitHub do editor ou `tr_github_entrar()`.")
  }
  as.vector(token)
}

.gh_token_gh_cli <- function() {
  gh <- Sys.which("gh")
  if (!nzchar(gh)) return(NULL)
  host <- sub("^https?://", "", tr_github_url())
  out <- suppressWarnings(tryCatch(
    system2(gh, c("auth", "token", "--hostname", host), stdout = TRUE, stderr = FALSE),
    error = function(e) character()))
  if (!is.null(attr(out, "status")) || !length(out)) return(NULL)
  trimws(out[[1]])
}

.gh_token_gitcreds <- function() {
  if (!nzchar(Sys.which("git"))) return(NULL)
  cred <- gitcreds::gitcreds_get(url = tr_github_url(), use_cache = FALSE)
  cred$password
}

# Chaveiro ---------------------------------------------------------------------

.SERVICO <- "trama-github"
.USUARIO <- "token"

# Backend do keyring. Sem Secret Service (Linux sem libsecret), o keyring cai
# no backend `env` (some ao fechar o R) ou, se configurado, no `file`, que pede
# senha no console e travaria o editor. Nos dois casos o trama usa `env` e
# avisa que o login não vai durar.
.gh_chaveiro <- function() {
  b <- tryCatch(suppressWarnings(keyring::default_backend()), error = function(e) NULL)
  if (is.null(b) || inherits(b, "backend_file")) b <- keyring::backend_env$new()
  b
}

#' Onde o token do trama fica guardado
#'
#' @return Lista com `backend` (nome do backend do `keyring`) e `persistente`
#'   (`FALSE` quando o token só vale enquanto o R estiver aberto).
#' @export
tr_github_chaveiro <- function() {
  b <- .gh_chaveiro()
  nome <- sub("^backend_", "", class(b)[1])
  list(backend = nome, persistente = !identical(nome, "env"))
}

.gh_token_chaveiro <- function() {
  tryCatch(.gh_chaveiro()$get(service = .SERVICO, username = .USUARIO),
           error = function(e) NULL)
}

#' Guarda e apaga o token do trama no chaveiro
#'
#' `tr_github_salvar_token()` é o que o login faz no fim; serve também para
#' colar um token criado à mão. `tr_github_sair()` apaga só o que o trama
#' guardou: `GITHUB_PAT`, `gh` e `gitcreds` são de outras ferramentas e
#' continuam valendo.
#' @param token Token do GitHub.
#' @export
tr_github_salvar_token <- function(token) {
  stopifnot(is.character(token), length(token) == 1L, nzchar(token))
  .gh_chaveiro()$set_with_value(service = .SERVICO, username = .USUARIO, password = token)
  invisible(tr_github_chaveiro())
}

#' @rdname tr_github_salvar_token
#' @export
tr_github_sair <- function() {
  b <- .gh_chaveiro()
  tinha <- !is.null(.gh_token_chaveiro())
  if (tinha) tryCatch(b$delete(service = .SERVICO, username = .USUARIO), error = function(e) NULL)
  invisible(tinha)
}
