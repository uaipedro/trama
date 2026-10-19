# Client ID do OAuth App "trama" no GitHub. PONTO ÚNICO: quando o app for
# registrado, o valor entra aqui. Client ID é público (vai no código de todo
# app que usa device flow, como o do `gh`); o client secret NUNCA entra no
# pacote, e o device flow não precisa dele.
.CLIENT_ID_TRAMA <- ""

#' Client ID do OAuth App usado no login
#'
#' Ordem: `getOption("trama.github.client_id")`, variável de ambiente
#' `TRAMA_GITHUB_CLIENT_ID` e, por fim, o Client ID do app "trama" fixado no
#' pacote. Só identifica o app; não é segredo.
#'
#' @param obrigatorio Se `TRUE`, erro claro quando não há nenhum configurado.
#' @return O Client ID, ou `""` quando não há e `obrigatorio = FALSE`.
#' @export
tr_github_client_id <- function(obrigatorio = TRUE) {
  id <- .gh_config("trama.github.client_id", "TRAMA_GITHUB_CLIENT_ID", .CLIENT_ID_TRAMA)
  if (!nzchar(id) && obrigatorio) {
    .gh_parar("tr_github_erro_client_id", paste(
      "O login pelo navegador ainda não está configurado neste trama (falta o Client ID",
      "do app no GitHub). Defina TRAMA_GITHUB_CLIENT_ID ou use uma credencial que você",
      "já tenha: GITHUB_PAT, `gh auth login` ou `usethis::create_github_token()`."))
  }
  id
}
