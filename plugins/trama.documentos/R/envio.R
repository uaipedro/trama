# Publicar no GitHub: o destino que leva o documento e o pacote reprodutível
# a um repositório próprio da publicação, com release e página.
#
# Portado do `trama.publicar` (`R/envio.R`), agora a partir de um documento.
# Publicar: confere os bloqueios, acha ou cria o repositório (um por
# documento, público por padrão), liga o Pages em modo Actions e deixa a tag
# publicar no ambiente dele, monta o pacote com a próxima versão, faz UM
# commit atômico (a árvore passa a ser exatamente o pacote + o workflow) e
# cria a tag `vX.Y.Z`, que dispara o workflow (release + Pages em `/vX.Y.Z/`
# e `/ultimo/`, acumulados na branch `paginas`). Acompanhar: lê o Actions, a
# release e a página, e diz em que etapa está. O estado do envio mora em
# `.trama/documentos/envios/`; o endereço da publicação, no registro do
# projeto (`tr_documento_publicacoes()`).

#' Nome de repositório a partir de um texto: minúsculas, sem acento, hífens.
#' @noRd
.td_slug <- function(x) {
  x <- iconv(tolower(x), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  substr(if (nzchar(x)) x else "publicacao", 1L, 60L)
}

#' O que vai para o repositório além do pacote: o workflow e os scripts dele.
#' @noRd
.td_arquivos_github <- function(citacao, pagina) {
  ler <- function(nome) {
    p <- system.file("github", nome, package = "trama.documentos")
    paste(readLines(p, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  }
  notas <- paste0(
    "Pacote reprodut\u{ED}vel gerado pelo trama: o documento (`index.qmd`), os resultados em `_trama/`, ",
    "o fluxo (`flow.json`) e `ro-crate-metadata.json`.\n\n",
    "- P\u{E1}gina desta vers\u{E3}o: ", pagina, "\n",
    "- Como citar: ", citacao, "\n")
  list(".github/workflows/publicar.yml" = ler("publicar.yml"),
       ".github/trama/conferir.py" = ler("conferir.py"),
       ".github/trama/paginas.sh" = ler("paginas.sh"),
       ".github/trama/notas.md" = notas,
       ".gitattributes" = ler("gitattributes"))
}

# ---- estado do envio ---------------------------------------------------------------

.td_envio_arquivo <- function(raiz, rel) {
  file.path(raiz, ".trama", "documentos", "envios", paste0(gsub("[^A-Za-z0-9_.-]", "_", rel), ".json"))
}

.td_envio_gravar <- function(raiz, estado) {
  p <- .td_envio_arquivo(raiz, estado$documento)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  writeLines(.td_json_txt(estado), p, useBytes = TRUE)
  estado
}

.td_envio_ler <- function(raiz, rel) {
  p <- .td_envio_arquivo(raiz, rel)
  if (!file.exists(p)) return(NULL)
  jsonlite::fromJSON(p, simplifyVector = FALSE)
}

# ---- publicar ----------------------------------------------------------------------

#' Publica o documento no GitHub
#'
#' Atualiza as regiões, confere ([tr_documento_publicacao()]), monta o pacote
#' ([tr_documento_pacote()]) com a próxima versão livre e o envia num commit
#' atômico ao repositório da publicação, com a tag `vX.Y.Z` que dispara o
#' workflow. A conta é a do `trama.github` (menu GitHub do editor, ou
#' `GITHUB_PAT`). Volta logo: o Actions leva um minuto ou dois, e
#' [tr_documento_acompanhar()] diz em que etapa está.
#'
#' Pede confirmação (devolve `confirmar`, sem tocar no GitHub) antes de criar
#' um repositório privado (Pages privado exige plano pago) e antes de enviar
#' a um repositório que já existe sem nenhuma versão `vX.Y.Z` (o envio
#' substitui o conteúdo dele). Bloqueios saem em `bloqueios`.
#'
#' @inheritParams tr_documento_inserir
#' @param repositorio `"nome"` ou `"dono/nome"`; `NULL` usa o que o documento
#'   já usou (registro do projeto) ou o título.
#' @param confirmado Já confirmado (ver acima).
#' @param atualizar Atualizar as regiões antes ([tr_documento_atualizar()]).
#' @return Lista: `bloqueios`, ou `confirmar` (texto) e `repo`, ou o estado
#'   do envio (`documento`, `repo`, `url`, `versao`, `tag`, `sha`, `pagina`,
#'   `ultimo`, `etapa` = `"enviado"`, `detalhe`).
#' @export
tr_documento_publicar <- function(documento, projeto = NULL, repositorio = NULL, confirmado = FALSE,
                                  atualizar = TRUE) {
  projeto <- .td_projeto(projeto, documento)
  raiz <- projeto$root
  rel <- .td_relativo(raiz, documento)
  if (isTRUE(atualizar)) tr_documento_atualizar(documento, projeto)
  pub <- tr_documento_publicacao(documento, projeto)
  if (length(pub$erros)) return(list(bloqueios = as.list(pub$erros)))
  token <- trama.github::tr_github_token()
  if (is.null(token)) {
    return(list(bloqueios = list("Voc\u{EA} ainda n\u{E3}o entrou no GitHub: use Entrar no menu GitHub do editor.")))
  }
  m <- pub$metadados
  usuario <- trama.github::tr_github_usuario(token)$login
  # O repositório: o pedido; senão o que este documento já usou (trocar o
  # título não abre outro repositório); senão o do título.
  registrado <- .td_registro(raiz)[[rel]]$repo
  alvo <- repositorio %||% registrado %||% .td_slug(m$titulo)
  if (!nzchar(alvo)) alvo <- .td_slug(m$titulo)
  partes <- strsplit(sub("^https?://github.com/", "", alvo), "/", fixed = TRUE)[[1]]
  dono <- if (length(partes) == 2L) partes[[1]] else usuario
  nome <- .td_slug(partes[[length(partes)]])
  repo <- paste0(dono, "/", nome)
  # Um repositório por documento, e nunca o repositório privado do projeto:
  # o envio troca a árvore inteira.
  reg <- .td_registro(raiz)
  outros <- Filter(function(p) identical(tolower(p$repo %||% ""), tolower(repo)), reg[names(reg) != rel])
  if (length(outros)) {
    return(list(bloqueios = list(sprintf(paste0("O reposit\u{F3}rio %s j\u{E1} \u{E9} o da publica\u{E7}\u{E3}o de %s. ",
                                                "Escolha outro nome de reposit\u{F3}rio para este documento."),
                                         repo, paste(names(outros), collapse = ", ")))))
  }
  privado_projeto <- tryCatch(trama.github::tr_github_versionar_config(raiz)$repo, error = function(e) NULL)
  if (identical(tolower(privado_projeto %||% ""), tolower(repo))) {
    return(list(bloqueios = list(sprintf(paste0("%s \u{E9} o reposit\u{F3}rio privado das vers\u{F5}es deste projeto: ",
                                                "a publica\u{E7}\u{E3}o vai para um reposit\u{F3}rio pr\u{F3}prio. ",
                                                "Escolha outro nome."), repo))))
  }
  info <- trama.github::tr_github_repo(repo, token)
  if (is.null(info)) {
    if (isTRUE(m$privado) && !isTRUE(confirmado)) {
      return(list(confirmar = paste0(
        "Vai criar o reposit\u{F3}rio privado ", repo, ". P\u{E1}gina de reposit\u{F3}rio privado s\u{F3} sai em ",
        "plano pago do GitHub (o GitHub Education d\u{E1} o Pro a estudantes e professores). Criar mesmo assim?"),
        repo = repo))
    }
    info <- trama.github::tr_github_criar_repo(nome, privado = isTRUE(m$privado), descricao = m$titulo,
                                              dono = if (identical(dono, usuario)) NULL else dono, token = token)
    tags <- character()
  } else {
    tags <- trama.github::tr_github_tags(repo, token)
    if (!any(grepl("^v[0-9]+\\.[0-9]+\\.[0-9]+$", tags)) && !isTRUE(confirmado)) {
      return(list(confirmar = paste0(
        "O reposit\u{F3}rio ", repo, " j\u{E1} existe e n\u{E3}o tem nenhuma vers\u{E3}o publicada pelo trama. ",
        "Publicar substitui todo o conte\u{FA}do dele pelo pacote. Continuar?"), repo = repo))
    }
  }
  pages <- trama.github::tr_github_pages(repo, token)
  if (is.null(pages) || !identical(pages$modo, "workflow")) pages <- trama.github::tr_github_ativar_pages(repo, token)
  trama.github::tr_github_ambiente_tags(repo, token = token)

  versao <- .td_proxima(m$tipo_versao, .td_ultima_versao(tags))
  tag <- paste0("v", versao)
  base_pages <- sub("/?$", "/", pages$url %||% sprintf("https://%s.github.io/%s/", tolower(dono), nome))
  pagina <- paste0(base_pages, tag, "/")
  dir <- file.path(raiz, ".trama", "documentos", paste0("envio-", gsub("[^A-Za-z0-9_-]", "-", basename(dirname(rel)))))
  tr_documento_pacote(documento, dir, projeto, versao = versao, repositorio = info$url)
  arqs <- .td_listar(dir)
  commit <- trama.github::tr_github_commit(
    repo, sprintf("Publica\u{E7}\u{E3}o %s", tag), arquivos = stats::setNames(file.path(dir, arqs), arqs),
    conteudos = .td_arquivos_github(.td_citacao(m, versao, info$url), pagina),
    manter_outros = FALSE, token = token)
  t <- tryCatch(trama.github::tr_github_tag(repo, tag, sha = commit$sha, mensagem = sprintf("Vers\u{E3}o %s", versao),
                                           token = token),
    tr_github_erro_tag_existe = function(e) {
      .td_erro(sprintf("A vers\u{E3}o %s j\u{E1} foi publicada e n\u{E3}o muda. A pr\u{F3}xima livre \u{E9} %s: publique de novo.",
                       tag, paste0("v", .td_proxima(m$tipo_versao, versao))), "tr_documento_erro_publicar")
    })
  estado <- .td_envio_gravar(raiz, list(
    documento = rel, repo = repo, url = info$url, versao = versao, tag = tag, sha = commit$sha,
    commit = commit$url, release = t$url, pagina = pagina, ultimo = paste0(base_pages, "ultimo/"),
    etapa = "enviado", detalhe = "Pacote enviado; esperando o GitHub Actions.",
    enviado_em = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")))
  .td_registrar(raiz, rel, list(repo = repo, url = info$url, pagina = base_pages, ultimo = estado$ultimo,
                                versao = versao, tag = tag, data = format(Sys.Date(), "%Y-%m-%d")))
  estado
}

#' A página já responde? (o Pages leva um tempo depois do deploy).
#' @noRd
.td_pagina_no_ar <- function(url) {
  r <- tryCatch(curl::curl_fetch_memory(url, handle = curl::new_handle(nobody = TRUE, timeout = 10)),
                error = function(e) NULL)
  !is.null(r) && r$status_code == 200L
}

#' Acompanha a última publicação do documento
#'
#' Um passo de leitura no GitHub: o workflow começou, terminou, falhou (em
#' que etapa)? A release saiu? A página responde? Grava e devolve o estado,
#' com `etapa` em `enviado`, `actions`, `release`, `no_ar` ou `falhou`.
#'
#' @inheritParams tr_documento_inserir
#' @return O estado do envio (ver [tr_documento_publicar()]), com `actions`,
#'   `release` e `log` quando houver; `NULL` se o documento nunca foi
#'   publicado deste projeto.
#' @export
tr_documento_acompanhar <- function(documento, projeto = NULL) {
  raiz <- if (inherits(projeto, "tr_project")) projeto$root else projeto %||% .td_raiz_de(documento)
  estado <- .td_envio_ler(raiz, .td_relativo(raiz, documento))
  if (is.null(estado) || estado$etapa %in% c("no_ar", "falhou")) return(estado)
  token <- trama.github::tr_github_token()
  repo <- estado$repo
  if (estado$etapa %in% c("enviado", "actions")) {
    runs <- Filter(function(r) identical(r$nome, "publicar"), trama.github::tr_github_execucoes(repo, estado$sha, token))
    if (!length(runs)) {
      estado$detalhe <- "Esperando o GitHub Actions come\u{E7}ar."
      return(.td_envio_gravar(raiz, estado))
    }
    run <- runs[[1]]
    estado$actions <- run$url
    if (!identical(run$estado, "completed")) {
      estado$etapa <- "actions"
      estado$detalhe <- "O GitHub Actions est\u{E1} conferindo o pacote e renderizando a p\u{E1}gina."
      return(.td_envio_gravar(raiz, estado))
    }
    if (!identical(run$conclusao, "success")) {
      f <- trama.github::tr_github_falha(repo, run$id, token)
      estado$etapa <- "falhou"
      estado$detalhe <- sprintf("O workflow falhou na etapa \"%s\".", f$etapa %||% run$conclusao)
      estado$log <- f$url %||% run$url
      return(.td_envio_gravar(raiz, estado))
    }
    estado$etapa <- "release"
  }
  if (identical(estado$etapa, "release")) {
    r <- trama.github::tr_github_release(repo, estado$tag, token)
    if (!is.null(r)) estado$release <- r$url
    if (.td_pagina_no_ar(estado$pagina)) {
      estado$etapa <- "no_ar"
      estado$detalhe <- "P\u{E1}gina no ar."
    } else {
      estado$detalhe <- "Release criada; o Pages ainda est\u{E1} pondo a p\u{E1}gina no ar."
    }
  }
  .td_envio_gravar(raiz, estado)
}
