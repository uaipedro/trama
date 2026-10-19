# Versionar o projeto (#15): cada "salvar versão" é um commit no repositório
# PRIVADO do projeto, separado do repositório de cada publicação. O commit é
# o retrato exato do que se escolheu versionar; restaurar traz esse retrato
# de volta, guardando antes o estado atual.
#
# A configuração mora no `trama.json`, chave `versionar`:
# `{repo, url, branch, dados}`. `dados` são os arquivos de dados que a pessoa
# marcou. Nada ali muda a cada versão: o histórico vem da API.

.CHAVE <- "versionar"

# Nunca vão: cache e estado do editor (`.trama/`, que tem o store e o token
# do canal de controle), git, sessão do R e segredos.
.NUNCA <- c("^\\.trama/", "^\\.git/", "^\\.Rproj\\.user/", "(^|/)\\.Rhistory$", "(^|/)\\.RData$",
            "(^|/)\\.Ruserdata$", "(^|/)\\.Renviron$", "(^|/)\\.env$", "\\.pem$", "\\.key$",
            "(^|/)id_(rsa|ed25519|ecdsa)", "^renv/(library|staging|local)/", "^\\.quarto/",
            "(^|/)\\.DS_Store$", "(^|/)Thumbs\\.db$")

# Sempre vão: o que descreve a análise. Fluxos, configuração, código,
# metadados e as imagens das notas do canvas.
.SEMPRE <- c("^trama\\.json$", "^flows/.*\\.json$", "^imagens/",
             "\\.(R|r|Rmd|rmd|qmd|py|sql)$",
             "\\.(md|cff|ya?ml|toml|Rproj)$",
             "(^|/)(DESCRIPTION|NAMESPACE|LICENSE[^/]*|\\.gitignore|\\.Rbuildignore)$")

.AVISO_TAMANHO <- 20 * 1024^2

# Nome de arquivo que sugere dado pessoal ou sigiloso.
.SENSIVEL <- paste0("(cpf|rg_|paciente|prontuario|cliente|pessoa|pessoal|pessoais|contato|email|e-mail|",
                    "telefone|endereco|salario|renda|saude|sigilo|sigiloso|confidencial|senha|segredo|",
                    "credencia|token|privad)")

#' Arquivos do projeto e o que vai numa versão
#'
#' Classifica os arquivos do projeto. Vão sempre (`tipo = "projeto"`):
#' `trama.json`, fluxos, código (R, Rmd, qmd, py, sql), metadados (md, cff,
#' yml, toml...) e as imagens das notas. São dados (`tipo = "dados"`) todos
#' os outros: ficam de fora até serem marcados. Não aparecem, e nunca vão:
#' `.trama/` (store, execuções, token do editor), `.git/`, arquivos de sessão
#' do R, `.Renviron`, `.env` e chaves.
#'
#' @param root Pasta do projeto.
#' @param dados Arquivos de dados marcados; o padrão é o que está salvo no
#'   `trama.json`.
#' @return Data frame com `caminho`, `tamanho` (bytes), `tipo`, `incluido`,
#'   `bloqueado` (acima de 100 MB: o GitHub recusa) e `aviso` (texto ou `NA`).
#' @export
tr_github_versionar_arquivos <- function(root = ".", dados = NULL) {
  raiz <- normalizePath(root, mustWork = TRUE)
  dados <- dados %||% unlist(tr_github_versionar_config(raiz)$dados) %||% character()
  caminhos <- list.files(raiz, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  caminhos <- gsub("\\\\", "/", caminhos)
  caminhos <- caminhos[!.gh_casa(caminhos, .NUNCA)]
  tamanho <- file.size(file.path(raiz, caminhos))
  tipo <- ifelse(.gh_casa(caminhos, .SEMPRE), "projeto", "dados")
  bloqueado <- tamanho > .LIMITE_ARQUIVO
  aviso <- rep(NA_character_, length(caminhos))
  aviso[tamanho > .AVISO_TAMANHO] <- "Arquivo grande: cada versão o envia de novo quando muda."
  sens <- tipo == "dados" & grepl(.SENSIVEL, tolower(basename(caminhos)))
  aviso[sens] <- "O nome sugere dados pessoais ou sigilosos: confira antes de versionar."
  aviso[bloqueado] <- "Acima de 100 MB: o GitHub recusa o arquivo."
  incluido <- (tipo == "projeto" | caminhos %in% dados) & !bloqueado
  out <- data.frame(caminho = caminhos, tamanho = tamanho, tipo = tipo, incluido = incluido,
                    bloqueado = bloqueado, aviso = aviso, stringsAsFactors = FALSE)
  out[order(out$tipo != "projeto", out$caminho), , drop = FALSE]
}

.gh_casa <- function(x, padroes) {
  Reduce(`|`, lapply(padroes, function(p) grepl(p, x)), logical(length(x)))
}

#' Configuração do versionamento do projeto
#'
#' `tr_github_versionar_config()` lê a chave `versionar` do `trama.json`
#' (`NULL` se o projeto nunca foi versionado). `tr_github_versionar_marcar()`
#' marca ou desmarca arquivos de dados para entrar nas próximas versões.
#'
#' @inheritParams tr_github_versionar_arquivos
#' @param caminhos Caminhos relativos à pasta do projeto.
#' @param incluir `TRUE` marca, `FALSE` desmarca.
#' @return A configuração (lista), invisível em `tr_github_versionar_marcar()`.
#' @export
tr_github_versionar_config <- function(root = ".") {
  trama::tr_project_config(root, .CHAVE)
}

#' @rdname tr_github_versionar_config
#' @export
tr_github_versionar_marcar <- function(root = ".", caminhos, incluir = TRUE) {
  cfg <- tr_github_versionar_config(root) %||% list()
  arq <- tr_github_versionar_arquivos(root, dados = character())
  fora <- setdiff(caminhos, arq$caminho[arq$tipo == "dados"])
  if (length(fora)) stop("Não é arquivo de dados do projeto: ", paste(fora, collapse = ", "))
  grandes <- intersect(caminhos, arq$caminho[arq$bloqueado])
  if (incluir && length(grandes)) {
    .gh_parar("tr_github_erro_arquivo_grande",
              sprintf("O GitHub recusa arquivo acima de 100 MB: %s", paste(grandes, collapse = ", ")))
  }
  atuais <- unlist(cfg$dados) %||% character()
  cfg$dados <- as.list(sort(if (incluir) union(atuais, caminhos) else setdiff(atuais, caminhos)))
  trama::tr_project_config_set(root, .CHAVE, cfg)
  invisible(cfg)
}

#' Repositório privado do projeto
#'
#' Cria o repositório privado onde ficam as versões do projeto e o registra
#' no `trama.json`. É outro repositório, nunca o de uma publicação: o
#' rascunho não vaza junto com o que se publica.
#'
#' @inheritParams tr_github_versionar_arquivos
#' @param nome Nome do repositório; o padrão vem do nome da pasta.
#' @inheritParams tr_github_usuario
#' @return A configuração gravada.
#' @export
tr_github_versionar_criar <- function(root = ".", nome = NULL, token = NULL) {
  cfg <- tr_github_versionar_config(root) %||% list()
  if (!is.null(cfg$repo)) {
    .gh_parar("tr_github_erro_ja_versionado",
              sprintf("Este projeto já guarda versões em %s.", cfg$repo))
  }
  nome <- nome %||% tr_github_versionar_nome(root)
  r <- tr_github_criar_repo(nome, privado = TRUE,
                            descricao = "Versões de um projeto do trama (privado).", token = token)
  cfg$repo <- r$repo
  cfg$url <- r$url
  cfg$branch <- r$branch
  cfg$dados <- cfg$dados %||% list()
  trama::tr_project_config_set(root, .CHAVE, cfg)
  invisible(cfg)
}

#' @rdname tr_github_versionar_criar
#' @return `tr_github_versionar_nome()`: nome sugerido (pasta em minúsculas,
#'   sem acento, com hífens).
#' @export
tr_github_versionar_nome <- function(root = ".") {
  n <- basename(normalizePath(root, mustWork = TRUE))
  n <- iconv(n, "UTF-8", "ASCII//TRANSLIT", sub = "")
  n <- gsub("[^a-z0-9._-]+", "-", tolower(n))
  n <- gsub("^-+|-+$", "", n)
  if (!nzchar(n)) "projeto-trama" else n
}

.gh_versionar_repo <- function(root) {
  cfg <- tr_github_versionar_config(root)
  if (is.null(cfg$repo)) {
    .gh_parar("tr_github_erro_sem_repositorio",
              "Este projeto ainda não guarda versões no GitHub. Crie o repositório privado primeiro.")
  }
  cfg
}

#' Salvar versão
#'
#' Grava no repositório privado do projeto um commit com exatamente os
#' arquivos que vão numa versão (ver [tr_github_versionar_arquivos()]).
#' Arquivo apagado do projeto, ou dado desmarcado, some da versão nova e
#' continua nas anteriores. Só o que mudou é enviado.
#'
#' @inheritParams tr_github_versionar_arquivos
#' @param mensagem Uma frase sobre o que mudou.
#' @inheritParams tr_github_usuario
#' @return Lista com `sha`, `url`, `mudou` (`FALSE` se nada mudou desde a
#'   última versão), `arquivos` e `bytes`.
#' @export
tr_github_salvar_versao <- function(root = ".", mensagem, token = NULL) {
  if (!is.character(mensagem) || length(mensagem) != 1L || !nzchar(trimws(mensagem))) {
    stop("Escreva uma frase sobre o que mudou.")
  }
  cfg <- .gh_versionar_repo(root)
  raiz <- normalizePath(root, mustWork = TRUE)
  arq <- tr_github_versionar_arquivos(raiz)
  vao <- arq[arq$incluido, , drop = FALSE]
  r <- tr_github_commit(cfg$repo, trimws(mensagem),
                        arquivos = stats::setNames(file.path(raiz, vao$caminho), vao$caminho),
                        branch = cfg$branch, manter_outros = FALSE, pular_iguais = TRUE, token = token)
  c(r, list(arquivos = nrow(vao), bytes = sum(vao$tamanho)))
}

#' Versões salvas
#'
#' @inheritParams tr_github_versionar_arquivos
#' @param n Quantas, das mais recentes.
#' @inheritParams tr_github_usuario
#' O primeiro commit do repositório, que o GitHub cria só com um README, não
#' é versão do projeto e não aparece.
#'
#' @return Data frame com `sha`, `mensagem` (primeira linha), `data`
#'   (POSIXct, UTC), `autor` e `url`, a mais recente primeiro.
#' @export
tr_github_versoes <- function(root = ".", n = 30, token = NULL) {
  cfg <- .gh_versionar_repo(root)
  r <- .gh_chamar(sprintf("/repos/%s/commits?sha=%s&per_page=%d", cfg$repo, cfg$branch %||% "main", as.integer(n)),
                  token = .gh_token_ou_erro(token))
  r <- Filter(function(x) length(x$parents) > 0, r)
  if (!length(r)) {
    return(data.frame(sha = character(), mensagem = character(), data = as.POSIXct(character(), tz = "UTC"),
                      autor = character(), url = character(), stringsAsFactors = FALSE))
  }
  data.frame(
    sha = vapply(r, function(x) x$sha, ""),
    mensagem = vapply(r, function(x) sub("\n.*", "", x$commit$message %||% ""), ""),
    data = as.POSIXct(vapply(r, function(x) x$commit$author$date %||% NA_character_, ""),
                      format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    autor = vapply(r, function(x) x$commit$author$name %||% "", ""),
    url = vapply(r, function(x) x$html_url %||% "", ""),
    stringsAsFactors = FALSE)
}

#' Restaurar uma versão
#'
#' Traz os arquivos do projeto de volta a como estavam numa versão salva.
#' Tudo é baixado antes de mexer em qualquer arquivo: se a rede cair, nada
#' muda. Antes de escrever, o estado atual de cada arquivo que vai ser
#' trocado ou removido é copiado para `.trama/restauracao/<data-hora>/`.
#' Arquivos que não vão em versão (`.trama/`, dados não marcados) não são
#' tocados. No `trama.json`, as chaves `versionar` e `publicacoes` ficam como
#' estão agora: a ligação com o GitHub não volta no tempo.
#'
#' @inheritParams tr_github_versionar_arquivos
#' @param sha Commit da versão (de [tr_github_versoes()]).
#' @inheritParams tr_github_usuario
#' @return Lista com `escritos`, `removidos` (caminhos) e `copia` (pasta com o
#'   estado anterior, ou `NA` se nada foi trocado).
#' @export
tr_github_restaurar <- function(root = ".", sha, token = NULL) {
  token <- .gh_token_ou_erro(token)
  cfg <- .gh_versionar_repo(root)
  raiz <- normalizePath(root, mustWork = TRUE)
  arvore <- .gh_chamar(sprintf("/repos/%s/git/commits/%s", cfg$repo, sha), token = token)$tree$sha
  remotos <- .gh_arvore(cfg$repo, arvore, token)
  caminhos <- names(remotos)
  # Caminho vindo de fora não escreve fora do projeto nem em `.trama/`.
  ruins <- grepl("(^|/)\\.\\.(/|$)", caminhos) | grepl("^/|^[A-Za-z]:", caminhos) | .gh_casa(caminhos, .NUNCA)
  remotos <- remotos[!ruins]
  if (!"trama.json" %in% names(remotos)) {
    .gh_parar("tr_github_erro_versao_invalida",
              "Essa versão não tem o trama.json: não é uma versão do projeto e não pode ser restaurada.")
  }

  # 1. Baixa o que difere do disco. Nenhum arquivo muda até aqui.
  locais <- file.path(raiz, names(remotos))
  igual <- vapply(seq_along(remotos), function(i) {
    file.exists(locais[i]) && identical(.gh_sha_blob(readBin(locais[i], "raw", file.size(locais[i]))), remotos[[i]])
  }, logical(1))
  baixar <- remotos[!igual]
  novos <- lapply(baixar, function(s) {
    b <- .gh_chamar(sprintf("/repos/%s/git/blobs/%s", cfg$repo, s), token = token)
    jsonlite::base64_dec(gsub("\n", "", b$content, fixed = TRUE))
  })

  # 2. Arquivo do projeto (fluxo, código, configuração) que a versão não tem
  #    sai. Dado nunca é apagado: no máximo, trocado pelo da versão.
  atuais <- tr_github_versionar_arquivos(raiz)
  remover <- setdiff(atuais$caminho[atuais$tipo == "projeto"], names(remotos))

  # 3. Cópia do estado atual do que vai ser trocado ou removido.
  tocados <- c(names(baixar)[file.exists(file.path(raiz, names(baixar)))], remover)
  copia <- NA_character_
  if (length(tocados)) {
    copia <- file.path(raiz, ".trama", "restauracao", format(Sys.time(), "%Y%m%d-%H%M%S"))
    for (p in tocados) {
      dir.create(dirname(file.path(copia, p)), recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(raiz, p), file.path(copia, p), overwrite = TRUE, copy.date = TRUE)
    }
  }

  # 4. Escreve. A ligação com o GitHub e o registro de publicações não voltam
  #    no tempo: valem as de agora.
  manter <- list(versionar = trama::tr_project_config(raiz, .CHAVE),
                 publicacoes = trama::tr_project_config(raiz, "publicacoes"))
  for (p in names(novos)) {
    destino <- file.path(raiz, p)
    dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(destino, ".trama-tmp")
    writeBin(novos[[p]], tmp)
    file.rename(tmp, destino)
  }
  if (length(remover)) unlink(file.path(raiz, remover))
  if (file.exists(file.path(raiz, "trama.json"))) {
    for (k in names(manter)) trama::tr_project_config_set(raiz, k, manter[[k]])
  }
  list(escritos = names(novos), removidos = remover, copia = copia)
}
