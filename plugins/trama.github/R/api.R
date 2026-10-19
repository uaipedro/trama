# API interna que a publicação (trama.publicar) e o versionamento do projeto
# usam. Tudo pela API REST: não precisa de git instalado.

#' Usuário conectado
#'
#' @param token Token do GitHub; o padrão é o que [tr_github_token()] achar.
#' @return Lista com `login`, `nome`, `avatar`, `perfil` (URL) e `plano`.
#' @export
tr_github_usuario <- function(token = NULL) {
  u <- .gh_chamar("/user", token = .gh_token_ou_erro(token))
  list(login = u$login, nome = u$name %||% u$login, avatar = u$avatar_url,
       perfil = u$html_url, plano = u$plan$name %||% NA_character_)
}

#' Repositórios
#'
#' `tr_github_criar_repo()` cria um repositório já com um primeiro commit
#' (`auto_init`): a API de dados do Git recusa gravar num repositório vazio.
#' `tr_github_repo()` lê um repositório e devolve `NULL` se ele não existe.
#'
#' @param nome Nome do repositório (sem o dono).
#' @param privado Repositório privado (o padrão) ou público.
#' @param descricao Descrição curta, opcional.
#' @param dono Organização onde criar; `NULL` cria na conta de quem entrou.
#' @param repo Repositório como `"dono/nome"`.
#' @inheritParams tr_github_usuario
#' @return Lista com `repo` (`"dono/nome"`), `url`, `privado` e `branch`
#'   (a branch padrão).
#' @export
tr_github_criar_repo <- function(nome, privado = TRUE, descricao = NULL, dono = NULL, token = NULL) {
  stopifnot(is.character(nome), length(nome) == 1L, nzchar(nome))
  caminho <- if (is.null(dono)) "/user/repos" else sprintf("/orgs/%s/repos", dono)
  corpo <- list(name = nome, private = isTRUE(privado), auto_init = TRUE)
  if (!is.null(descricao)) corpo$description <- descricao
  r <- tryCatch(.gh_chamar(caminho, "POST", corpo, .gh_token_ou_erro(token)),
    tr_github_erro_validacao = function(e) {
      if (grepl("already exists", e$detalhe %||% "", fixed = TRUE)) {
        .gh_parar("tr_github_erro_repo_existe",
                  sprintf("Já existe um repositório chamado \"%s\" nessa conta. Escolha outro nome.", nome),
                  e$status, e$detalhe)
      }
      stop(e)
    })
  .gh_repo_info(r)
}

#' @rdname tr_github_criar_repo
#' @export
tr_github_repo <- function(repo, token = NULL) {
  r <- tryCatch(.gh_chamar(paste0("/repos/", repo), token = .gh_token_ou_erro(token)),
                tr_github_erro_nao_encontrado = function(e) NULL)
  if (is.null(r)) NULL else .gh_repo_info(r)
}

.gh_repo_info <- function(r) {
  list(repo = r$full_name, url = r$html_url, privado = isTRUE(r$private),
       branch = r$default_branch %||% "main")
}

#' Commit atômico pela API de dados do Git
#'
#' Grava arquivos num repositório sem git instalado: cria os blobs, monta a
#' árvore sobre a do último commit, cria o commit e só então move a branch.
#' Se a rede cair no meio, sobram objetos soltos que o GitHub descarta, e a
#' branch continua onde estava: repetir o envio é seguro.
#'
#' @param repo Repositório como `"dono/nome"`.
#' @param mensagem Mensagem do commit.
#' @param arquivos Arquivos locais: vetor nomeado `caminho no repositório =
#'   caminho no disco`.
#' @param conteudos Conteúdo em memória: lista nomeada `caminho no repositório =
#'   texto ou raw`.
#' @param apagar Caminhos a remover do repositório.
#' @param branch Branch; `NULL` usa a padrão do repositório.
#' @param manter_outros `TRUE` (padrão) mantém os arquivos que já estão no
#'   repositório; `FALSE` faz a árvore ter exatamente os arquivos enviados.
#' @param pular_iguais Não reenviar arquivo que o repositório já tem igual
#'   (compara o sha de blob do git, calculado aqui). Custa uma leitura da
#'   árvore e poupa o envio de tudo o que não mudou.
#' @inheritParams tr_github_usuario
#' @return Lista com `sha`, `url` e `mudou` (`FALSE` quando a árvore ficou
#'   igual e nenhum commit foi criado).
#' @export
tr_github_commit <- function(repo, mensagem, arquivos = character(), conteudos = list(),
                             apagar = character(), branch = NULL, manter_outros = TRUE,
                             pular_iguais = FALSE, token = NULL) {
  token <- .gh_token_ou_erro(token)
  if (length(arquivos) && is.null(names(arquivos))) stop("`arquivos` precisa de nomes (caminho no repositório).")
  if (length(conteudos) && is.null(names(conteudos))) stop("`conteudos` precisa de nomes (caminho no repositório).")
  if (length(apagar) && !manter_outros) stop("`apagar` só faz sentido com `manter_outros = TRUE`.")
  faltando <- arquivos[!file.exists(arquivos)]
  if (length(faltando)) stop("Arquivo não encontrado: ", paste(faltando, collapse = ", "))
  grandes <- arquivos[file.size(arquivos) > .LIMITE_ARQUIVO]
  if (length(grandes)) {
    .gh_parar("tr_github_erro_arquivo_grande", sprintf(
      "O GitHub recusa arquivo acima de 100 MB: %s", paste(names(grandes), collapse = ", ")))
  }

  base <- paste0("/repos/", repo, "/git")
  branch <- branch %||% tr_github_repo(repo, token)$branch %||% "main"
  pai <- .gh_chamar(sprintf("%s/ref/heads/%s", base, branch), token = token)$object$sha
  arvore_pai <- .gh_chamar(sprintf("%s/commits/%s", base, pai), token = token)$tree$sha

  conteudo <- c(lapply(arquivos, function(f) readBin(f, "raw", file.size(f))),
                lapply(conteudos, function(x) if (is.raw(x)) x else charToRaw(enc2utf8(paste(x, collapse = "\n")))))
  remotos <- if (pular_iguais) unlist(.gh_arvore(repo, arvore_pai, token)) else character()
  entradas <- Map(function(caminho, bytes) {
    sha <- if (length(remotos)) .gh_sha_blob(bytes)
    if (is.null(sha) || !sha %in% remotos) {
      sha <- .gh_chamar(paste0(base, "/blobs"), "POST",
                        list(content = jsonlite::base64_enc(bytes), encoding = "base64"), token)$sha
    }
    list(path = caminho, mode = "100644", type = "blob", sha = sha)
  }, names(conteudo), conteudo)
  # `sha: null` com `base_tree` remove o caminho da árvore.
  remocoes <- lapply(apagar, function(p) list(path = p, mode = "100644", type = "blob", sha = NULL))
  corpo <- list(tree = unname(c(entradas, remocoes)))
  if (manter_outros) corpo$base_tree <- arvore_pai
  arvore <- .gh_chamar(paste0(base, "/trees"), "POST", corpo, token)$sha
  if (identical(arvore, arvore_pai)) return(list(sha = pai, url = .gh_url_commit(repo, pai), mudou = FALSE))

  commit <- .gh_chamar(paste0(base, "/commits"), "POST",
                       list(message = mensagem, tree = arvore, parents = list(pai)), token)$sha
  # Só aqui a branch muda. Sem `force`: se alguém gravou na branch no meio do
  # caminho, o GitHub recusa em vez de apagar o trabalho alheio.
  tryCatch(.gh_chamar(sprintf("%s/refs/heads/%s", base, branch), "PATCH",
                      list(sha = commit, force = FALSE), token),
    tr_github_erro_validacao = function(e) {
      .gh_parar("tr_github_erro_concorrencia",
                "O repositório mudou enquanto o trama enviava. Nada foi perdido: tente de novo.",
                e$status, e$detalhe)
    })
  list(sha = commit, url = .gh_url_commit(repo, commit), mudou = TRUE)
}

.LIMITE_ARQUIVO <- 100 * 1024^2

# sha de blob do git: sha1 de "blob <tamanho>\0" seguido do conteúdo. É o
# endereço que o GitHub dá ao mesmo conteúdo, então dá para saber sem enviar.
.gh_sha_blob <- function(bytes) {
  paste(as.character(openssl::sha1(c(charToRaw(paste0("blob ", length(bytes))), as.raw(0L), bytes))))
}

# Arquivos de uma árvore (recursiva): lista nomeada caminho -> sha do blob.
.gh_arvore <- function(repo, arvore, token) {
  r <- .gh_chamar(sprintf("/repos/%s/git/trees/%s?recursive=1", repo, arvore), token = token)
  if (isTRUE(r$truncated)) {
    .gh_parar("tr_github_erro_arvore_grande",
              "O repositório tem arquivos demais para o trama ler de uma vez.")
  }
  blobs <- Filter(function(e) identical(e$type, "blob"), r$tree %||% list())
  stats::setNames(lapply(blobs, function(e) e$sha), vapply(blobs, function(e) e$path, ""))
}

.gh_url_commit <- function(repo, sha) sprintf("%s/%s/commit/%s", tr_github_url(), repo, sha)

#' Cria uma tag
#'
#' Tag já existente é recusada (erro de classe `tr_github_erro_tag_existe`):
#' versão publicada nunca muda.
#'
#' @param tag Nome da tag, por exemplo `"v1.0.0"`.
#' @param sha Commit marcado; `NULL` usa o último da branch padrão.
#' @param mensagem Com mensagem, cria tag anotada; sem, tag leve.
#' @inheritParams tr_github_commit
#' @return Lista com `tag`, `sha` (do commit) e `url`.
#' @export
tr_github_tag <- function(repo, tag, sha = NULL, mensagem = NULL, token = NULL) {
  token <- .gh_token_ou_erro(token)
  base <- paste0("/repos/", repo, "/git")
  existe <- tryCatch({ .gh_chamar(sprintf("%s/ref/tags/%s", base, tag), token = token); TRUE },
                     tr_github_erro_nao_encontrado = function(e) FALSE)
  if (existe) .gh_tag_existe(tag)
  if (is.null(sha)) {
    branch <- tr_github_repo(repo, token)$branch
    sha <- .gh_chamar(sprintf("%s/ref/heads/%s", base, branch), token = token)$object$sha
  }
  alvo <- sha
  if (!is.null(mensagem)) {
    alvo <- .gh_chamar(paste0(base, "/tags"), "POST",
                       list(tag = tag, message = mensagem, object = sha, type = "commit"), token)$sha
  }
  tryCatch(.gh_chamar(paste0(base, "/refs"), "POST", list(ref = paste0("refs/tags/", tag), sha = alvo), token),
           tr_github_erro_validacao = function(e) .gh_tag_existe(tag, e))
  list(tag = tag, sha = sha, url = sprintf("%s/%s/releases/tag/%s", tr_github_url(), repo, tag))
}

.gh_tag_existe <- function(tag, e = NULL) {
  .gh_parar("tr_github_erro_tag_existe",
            sprintf("A tag %s já existe. Versão publicada não muda: use a próxima.", tag),
            e$status %||% 422L, e$detalhe)
}
