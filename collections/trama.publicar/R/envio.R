# O envio ao GitHub (#14): as ações "publicar" e "acompanhar" do card.
#
# Publicar: confere os bloqueios, acha ou cria o repositório da publicação
# (um por publicação, público por padrão), liga o Pages em modo Actions e
# deixa a tag publicar no ambiente dele, monta o pacote com a próxima versão,
# faz UM commit atômico (a árvore passa a ser exatamente o pacote + o
# workflow) e cria a tag `vX.Y.Z`, que dispara o workflow. Acompanhar: lê o
# Actions, a release e a página, e diz em que etapa está. O estado mora em
# `.trama/publicar/envios/`, para o card retomar depois de recarregar.

#' Nome de repositório a partir de um texto: minúsculas, sem acento, hífens.
#' @noRd
.tr_publicar_slug <- function(x) {
  x <- iconv(tolower(x), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  substr(if (nzchar(x)) x else "publicacao", 1L, 60L)
}

#' Maior versão `vX.Y.Z` entre as tags (`"0.0.0"` sem nenhuma).
#' @noRd
.tr_publicar_ultima_versao <- function(tags) {
  v <- sub("^v", "", grep("^v[0-9]+\\.[0-9]+\\.[0-9]+$", tags, value = TRUE))
  if (!length(v)) return("0.0.0")
  as.character(max(numeric_version(v)))
}

#' O que vai para o repositório além do pacote: o workflow e os scripts dele.
#' @noRd
.tr_publicar_arquivos_github <- function(material, repo, tag, pagina) {
  ler <- function(nome) {
    p <- system.file("github", nome, package = "trama.publicar")
    paste(readLines(p, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  }
  notas <- paste0(
    "Pacote reprodut\u{ED}vel gerado pelo trama: `ro-crate-metadata.json`, o fluxo (`flow.json`), ",
    "os itens e a p\u{E1}gina.\n\n",
    "- P\u{E1}gina desta vers\u{E3}o: ", pagina, "\n",
    "- Como citar: ", .tr_publicar_citacao(material), "\n")
  list(".github/workflows/publicar.yml" = ler("publicar.yml"),
       ".github/trama/conferir.py" = ler("conferir.py"),
       ".github/trama/paginas.sh" = ler("paginas.sh"),
       ".github/trama/notas.md" = notas,
       ".gitattributes" = ler("gitattributes"))
}

.tr_publicar_estado_arquivo <- function(raiz, repo) {
  file.path(raiz, ".trama", "publicar", "envios", paste0(gsub("[^A-Za-z0-9_.-]", "_", repo), ".json"))
}

.tr_publicar_estado_gravar <- function(raiz, estado) {
  p <- .tr_publicar_estado_arquivo(raiz, estado$repo)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  writeLines(.tr_publicar_json_txt(estado), p, useBytes = TRUE)
  estado
}

.tr_publicar_estado_ler <- function(raiz, repo) {
  p <- .tr_publicar_estado_arquivo(raiz, repo)
  if (!file.exists(p)) return(NULL)
  jsonlite::fromJSON(p, simplifyVector = FALSE)
}

# ---- as publicações do projeto ---------------------------------------------------

#' Publicações do projeto
#'
#' Qual repositório cada nó `publicar` usa, a página, a última versão
#' enviada e quando. Mora no `trama.json` (chave `publicacoes`, por id do nó):
#' é configuração do PROJETO, não vai no fluxo compartilhável nem no pacote
#' publicado. Estado do Actions e versões ao vivo se leem do GitHub
#' (`trama.github`); isto é só o endereço de cada publicação.
#' @param root Pasta do projeto.
#' @return data.frame com `no`, `repo`, `url`, `pagina` (base do Pages),
#'   `ultimo`, `versao`, `tag` e `data`; zero linhas sem publicação.
#' @export
tr_publicar_publicacoes <- function(root = ".") {
  pubs <- trama::tr_project_config(root, "publicacoes") %||% list()
  cols <- c("repo", "url", "pagina", "ultimo", "versao", "tag", "data")
  if (!length(pubs)) {
    return(as.data.frame(stats::setNames(rep(list(character()), length(cols) + 1L), c("no", cols))))
  }
  linhas <- lapply(names(pubs), function(no) {
    p <- pubs[[no]]
    as.data.frame(c(list(no = no), stats::setNames(lapply(cols, function(k) as.character(p[[k]] %||% NA_character_)), cols)))
  })
  do.call(rbind, linhas)
}

#' Grava (ou atualiza) a publicação de um nó no `trama.json`.
#' @noRd
.tr_publicar_registrar <- function(raiz, no, estado, base_pages) {
  if (is.null(no) || !file.exists(file.path(raiz, "trama.json"))) return(invisible(NULL))
  pubs <- trama::tr_project_config(raiz, "publicacoes") %||% list()
  pubs[[no]] <- list(repo = estado$repo, url = estado$url, pagina = base_pages, ultimo = estado$ultimo,
                     versao = estado$versao, tag = estado$tag, data = format(Sys.Date(), "%Y-%m-%d"))
  trama::tr_project_config_set(raiz, "publicacoes", pubs)
  invisible(pubs[[no]])
}

#' A ação "publicar".
#'
#' Pede confirmação (devolve `confirmar`) antes de criar um repositório
#' privado (Pages privado exige plano pago) e antes de enviar a um repositório
#' que já existe sem nenhuma versão `vX.Y.Z` (o envio substitui o conteúdo).
#' Bloqueios saem em `bloqueios`, sem tocar no GitHub.
#' @noRd
.tr_publicar_publicar <- function(valor, ctx) {
  if (length(valor$erros)) return(list(bloqueios = valor$erros))
  if (!length(valor$itens)) return(list(bloqueios = list("Nenhum item para publicar.")))
  token <- trama.github::tr_github_token()
  if (is.null(token)) {
    return(list(bloqueios = list("Voc\u{EA} ainda n\u{E3}o entrou no GitHub: use Entrar no menu GitHub do editor.")))
  }
  confirmado <- isTRUE(ctx$extra$confirmado)
  m <- valor$metadados
  usuario <- trama.github::tr_github_usuario(token)$login
  # O repositório: o do param; senão o que este nó já usou (registrado no
  # projeto: trocar o título não abre outro repositório); senão o do título.
  no <- tryCatch(ctx$no(), error = function(e) NULL)
  registrado <- if (!is.null(no)) (trama::tr_project_config(ctx$raiz, "publicacoes") %||% list())[[no]]$repo
  alvo <- if (nzchar(m$repositorio)) m$repositorio else registrado %||% .tr_publicar_slug(m$titulo)
  partes <- strsplit(alvo, "/", fixed = TRUE)[[1]]
  dono <- if (length(partes) == 2L) partes[[1]] else usuario
  nome <- .tr_publicar_slug(partes[[length(partes)]])
  repo <- paste0(dono, "/", nome)
  info <- trama.github::tr_github_repo(repo, token)
  if (is.null(info)) {
    if (isTRUE(m$privado) && !confirmado) {
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
    if (!any(grepl("^v[0-9]+\\.[0-9]+\\.[0-9]+$", tags)) && !confirmado) {
      return(list(confirmar = paste0(
        "O reposit\u{F3}rio ", repo, " j\u{E1} existe e n\u{E3}o tem nenhuma vers\u{E3}o publicada pelo trama. ",
        "Publicar substitui todo o conte\u{FA}do dele pelo pacote. Continuar?"), repo = repo))
    }
  }
  pages <- trama.github::tr_github_pages(repo, token)
  if (is.null(pages) || !identical(pages$modo, "workflow")) pages <- trama.github::tr_github_ativar_pages(repo, token)
  trama.github::tr_github_ambiente_tags(repo, token = token)

  versao <- .tr_publicar_proxima(m$tipo_versao %||% "menor", .tr_publicar_ultima_versao(tags))
  tag <- paste0("v", versao)
  valor$metadados$versao <- versao
  if (!nzchar(valor$metadados$repositorio)) valor$metadados$repositorio <- info$url
  base_pages <- sub("/?$", "/", pages$url %||% sprintf("https://%s.github.io/%s/", tolower(dono), nome))
  pagina <- paste0(base_pages, tag, "/")
  dir <- file.path(ctx$raiz, ".trama", "publicar", paste0("envio-", nome))
  tr_publicar_montar(valor, dir, doc = ctx$doc, rotulos = .tr_publicar_rotulos(ctx$doc, ctx$registry),
                     dados_entrada = .tr_publicar_dados_entrada(ctx$doc, ctx$registry, ctx$store, ctx$raiz,
                                                                names(valor$itens), ctx$settings))
  rel <- .tr_publicar_listar(dir)
  arquivos <- stats::setNames(file.path(dir, rel), rel)
  commit <- trama.github::tr_github_commit(repo, sprintf("Publica\u{E7}\u{E3}o %s", tag), arquivos = arquivos,
                                           conteudos = .tr_publicar_arquivos_github(valor, repo, tag, pagina),
                                           manter_outros = FALSE, token = token)
  t <- tryCatch(trama.github::tr_github_tag(repo, tag, sha = commit$sha, mensagem = sprintf("Vers\u{E3}o %s", versao),
                                           token = token),
    tr_github_erro_tag_existe = function(e) {
      rlang::abort(sprintf("A vers\u{E3}o %s j\u{E1} foi publicada e n\u{E3}o muda. A pr\u{F3}xima livre \u{E9} %s: publique de novo.",
                           tag, paste0("v", .tr_publicar_proxima(m$tipo_versao %||% "menor", versao))),
                   class = "tr_publicar_error_tag")
    })
  estado <- .tr_publicar_estado_gravar(ctx$raiz, list(
    repo = repo, url = info$url, versao = versao, tag = tag, sha = commit$sha,
    commit = commit$url, release = t$url, pagina = pagina, ultimo = paste0(base_pages, "ultimo/"),
    etapa = "enviado", detalhe = "Pacote enviado; esperando o GitHub Actions.",
    enviado_em = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")))
  .tr_publicar_registrar(ctx$raiz, no, estado, base_pages)
  estado
}

#' A página já responde? (o Pages leva um tempo depois do deploy).
#' @noRd
.tr_publicar_pagina_no_ar <- function(url) {
  r <- tryCatch(curl::curl_fetch_memory(url, handle = curl::new_handle(nobody = TRUE, timeout = 10)),
                error = function(e) NULL)
  !is.null(r) && r$status_code == 200L
}

#' A ação "acompanhar": um passo de leitura no GitHub, e o estado novo.
#' @noRd
.tr_publicar_acompanhar <- function(valor, ctx) {
  repo <- as.character(ctx$extra$repo %||% "")[1]
  estado <- .tr_publicar_estado_ler(ctx$raiz, repo)
  if (is.null(estado)) rlang::abort(sprintf("Nenhum envio de '%s' neste projeto.", repo), class = "tr_publicar_error_envio")
  if (estado$etapa %in% c("no_ar", "falhou")) return(estado)
  token <- trama.github::tr_github_token()
  if (estado$etapa %in% c("enviado", "actions")) {
    runs <- Filter(function(r) identical(r$nome, "publicar"), trama.github::tr_github_execucoes(repo, estado$sha, token))
    if (!length(runs)) {
      estado$detalhe <- "Esperando o GitHub Actions come\u{E7}ar."
      return(.tr_publicar_estado_gravar(ctx$raiz, estado))
    }
    run <- runs[[1]]
    estado$actions <- run$url
    if (!identical(run$estado, "completed")) {
      estado$etapa <- "actions"
      estado$detalhe <- "O GitHub Actions est\u{E1} conferindo o pacote e renderizando a p\u{E1}gina."
      return(.tr_publicar_estado_gravar(ctx$raiz, estado))
    }
    if (!identical(run$conclusao, "success")) {
      f <- trama.github::tr_github_falha(repo, run$id, token)
      estado$etapa <- "falhou"
      estado$detalhe <- sprintf("O workflow falhou na etapa \"%s\".", f$etapa %||% run$conclusao)
      estado$log <- f$url %||% run$url
      return(.tr_publicar_estado_gravar(ctx$raiz, estado))
    }
    estado$etapa <- "release"
  }
  if (identical(estado$etapa, "release")) {
    rel <- trama.github::tr_github_release(repo, estado$tag, token)
    if (!is.null(rel)) estado$release <- rel$url
    if (.tr_publicar_pagina_no_ar(estado$pagina)) {
      estado$etapa <- "no_ar"
      estado$detalhe <- "P\u{E1}gina no ar."
    } else {
      estado$detalhe <- "Release criada; o Pages ainda est\u{E1} pondo a p\u{E1}gina no ar."
    }
  }
  .tr_publicar_estado_gravar(ctx$raiz, estado)
}
