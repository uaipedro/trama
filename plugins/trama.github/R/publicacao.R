# O que a publicação (trama.publicar, #14) pede além de repositório, commit e
# tag: as versões já publicadas, a release de uma tag, o GitHub Pages em modo
# Actions, a regra do ambiente que deixa a tag publicar, e o acompanhamento do
# workflow que a tag dispara.

#' Tags de um repositório
#'
#' @inheritParams tr_github_commit
#' @return Vetor de nomes de tag (até 100, as mais recentes primeiro, na ordem
#'   da API).
#' @export
tr_github_tags <- function(repo, token = NULL) {
  r <- .gh_chamar(sprintf("/repos/%s/tags?per_page=100", repo), token = .gh_token_ou_erro(token))
  vapply(r, function(t) as.character(t$name), "")
}

#' Release de uma tag
#'
#' @param tag Nome da tag (`"v1.0.0"`).
#' @inheritParams tr_github_commit
#' @return `NULL` se a release ainda não existe; senão lista com `tag`, `url`
#'   e `arquivos` (nome -> URL de download).
#' @export
tr_github_release <- function(repo, tag, token = NULL) {
  r <- tryCatch(.gh_chamar(sprintf("/repos/%s/releases/tags/%s", repo, tag), token = .gh_token_ou_erro(token)),
                tr_github_erro_nao_encontrado = function(e) NULL)
  if (is.null(r)) return(NULL)
  arquivos <- vapply(r$assets %||% list(), function(a) as.character(a$browser_download_url), "")
  names(arquivos) <- vapply(r$assets %||% list(), function(a) as.character(a$name), "")
  list(tag = r$tag_name, url = r$html_url, arquivos = as.list(arquivos))
}

#' GitHub Pages em modo Actions
#'
#' `tr_github_pages()` lê a configuração (`NULL` se o Pages está desligado).
#' `tr_github_ativar_pages()` liga em modo Actions (o site sai do workflow,
#' não de uma branch) ou, já ligado, troca o modo para Actions. Repositório
#' privado só tem Pages em plano pago: o GitHub recusa, e o erro diz isso.
#'
#' @inheritParams tr_github_commit
#' @return Lista com `url` (endereço do site), `modo` (`"workflow"` ou
#'   `"legacy"`) e `estado`.
#' @export
tr_github_pages <- function(repo, token = NULL) {
  r <- tryCatch(.gh_chamar(sprintf("/repos/%s/pages", repo), token = .gh_token_ou_erro(token)),
                tr_github_erro_nao_encontrado = function(e) NULL)
  if (is.null(r)) NULL else list(url = r$html_url, modo = r$build_type %||% NA_character_,
                                 estado = r$status %||% NA_character_)
}

#' @rdname tr_github_pages
#' @export
tr_github_ativar_pages <- function(repo, token = NULL) {
  token <- .gh_token_ou_erro(token)
  caminho <- sprintf("/repos/%s/pages", repo)
  tryCatch(.gh_chamar(caminho, "POST", list(build_type = "workflow"), token),
    tr_github_erro_conflito = function(e) .gh_chamar(caminho, "PUT", list(build_type = "workflow"), token),
    tr_github_erro_validacao = function(e) {
      .gh_parar("tr_github_erro_pages",
                paste("O GitHub não ligou o Pages neste repositório. Repositório privado só tem",
                      "Pages em plano pago (o GitHub Education dá o Pro a estudantes e professores)."),
                e$status, e$detalhe)
    })
  tr_github_pages(repo, token)
}

#' Deixa uma tag publicar no ambiente do Pages
#'
#' O ambiente `github-pages`, que o GitHub cria ao ligar o Pages, só aceita
#' publicação da branch padrão. A publicação do trama sai de um workflow
#' disparado por tag (`vX.Y.Z`): sem uma regra para as tags, o deploy é
#' recusado. A regra é acrescentada, e regra igual já existente não é erro.
#'
#' @param ambiente Nome do ambiente.
#' @param padrao Padrão de nome das tags que podem publicar.
#' @inheritParams tr_github_commit
#' @return Invisível, `TRUE`.
#' @export
tr_github_ambiente_tags <- function(repo, ambiente = "github-pages", padrao = "v*", token = NULL) {
  token <- .gh_token_ou_erro(token)
  base <- sprintf("/repos/%s/environments/%s", repo, ambiente)
  .gh_chamar(base, "PUT", list(deployment_branch_policy = list(protected_branches = FALSE,
                                                              custom_branch_policies = TRUE)), token)
  tryCatch(.gh_chamar(paste0(base, "/deployment-branch-policies"), "POST",
                      list(name = padrao, type = "tag"), token),
           tr_github_erro_validacao = function(e) NULL,
           tr_github_erro_conflito = function(e) NULL)
  invisible(TRUE)
}

#' Execuções do GitHub Actions
#'
#' `tr_github_execucoes()` lista as execuções de um commit (o da tag);
#' `tr_github_falha()` diz em que job e etapa uma execução falhou.
#'
#' @param sha Commit (`head_sha`) cujas execuções se quer.
#' @param id Id da execução.
#' @inheritParams tr_github_commit
#' @return `tr_github_execucoes()`: lista de execuções, cada uma com `id`,
#'   `nome`, `estado` (`queued`, `in_progress`, `completed`...), `conclusao`
#'   (`success`, `failure`... ou `NA` enquanto roda) e `url`.
#'   `tr_github_falha()`: lista com `job`, `etapa` e `url` (do log), ou `NULL`.
#' @export
tr_github_execucoes <- function(repo, sha, token = NULL) {
  r <- .gh_chamar(sprintf("/repos/%s/actions/runs?head_sha=%s&per_page=20", repo, sha),
                  token = .gh_token_ou_erro(token))
  lapply(r$workflow_runs %||% list(), function(x) {
    list(id = x$id, nome = x$name %||% "", estado = x$status %||% NA_character_,
         conclusao = x$conclusion %||% NA_character_, url = x$html_url %||% "")
  })
}

#' @rdname tr_github_execucoes
#' @export
tr_github_falha <- function(repo, id, token = NULL) {
  r <- .gh_chamar(sprintf("/repos/%s/actions/runs/%s/jobs", repo, id), token = .gh_token_ou_erro(token))
  for (j in r$jobs %||% list()) {
    if (!identical(j$conclusion, "failure")) next
    etapa <- Filter(function(s) identical(s$conclusion, "failure"), j$steps %||% list())
    return(list(job = j$name %||% "", etapa = if (length(etapa)) etapa[[1]]$name else NA_character_,
                url = j$html_url %||% ""))
  }
  NULL
}
