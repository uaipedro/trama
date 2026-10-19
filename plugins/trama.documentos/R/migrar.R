# Migração do `trama.publicar` (retirado): o nó `publicar/publish` vira um
# documento do modelo Página web.
#
# O nó guardava a página nos params (título, autores, licenças, layout) e
# recebia os itens por arestas. Cada nó vira `documentos/<nome>/`: o YAML
# com os metadados, o corpo na ordem do layout (cabeçalho do modelo, textos,
# uma região por item, o diagrama), e o registro de publicações do
# `trama.json` passa da chave do nó para a do documento. O nó e as arestas
# dele saem do fluxo, e a coleção sai do `trama.json`. Roda ao abrir o
# projeto no editor (uma vez: depois não há mais nó) e avisa.

.TD_NO_PUBLICAR <- "publicar/publish"

#' Migra os nós `publicar/publish` do projeto para documentos
#'
#' Para cada nó `publicar/publish` (da coleção `trama.publicar`, que deixou
#' de existir) em qualquer fluxo do projeto, cria um documento do modelo
#' Página web com o que o nó tinha: título, resumo, palavras-chave, autores
#' (com ORCID), licenças, tipo de versão, repositório privado e aceite de
#' dados de treino no YAML; o corpo segue o layout do nó (cabeçalho, textos,
#' cada item ligado como região, o diagrama). O endereço da publicação já
#' feita passa a ser do documento ([tr_documento_publicacoes()]), e a próxima
#' publicação continua no mesmo repositório, na versão seguinte. Depois tira
#' o nó (e as arestas que chegavam nele) do fluxo e `trama.publicar` da lista
#' de coleções do `trama.json`.
#'
#' Item que não pôde virar região (o nó falha, o modelo carrega dados de
#' treino sem o aceite) vira uma nota no texto, para inserir de novo à mão.
#'
#' @param projeto Pasta do projeto (ou um `trama::tr_project()`).
#' @return data.frame com `fluxo`, `no`, `documento` (caminho relativo ao
#'   projeto), `regioes` (quantas entraram) e `avisos`; zero linhas se não
#'   havia nada a migrar.
#' @export
tr_documento_migrar_publicar <- function(projeto = ".") {
  raiz <- normalizePath(if (inherits(projeto, "tr_project")) projeto$root else projeto, mustWork = TRUE)
  colunas <- c("fluxo", "no", "documento", "regioes", "avisos")
  # A coleção sai antes de abrir o projeto: instalada (de uma versão velha),
  # ela entraria no registro e mudaria a chave do store das regiões novas.
  .td_tirar_colecao_publicar(raiz)
  alvos <- .td_fluxos_com_publicar(raiz)
  if (!length(alvos)) return(.td_quadro(list(), colunas))
  if (!inherits(projeto, "tr_project") || "trama.publicar" %in% projeto$collections) {
    projeto <- suppressMessages(trama::tr_project(raiz, create = FALSE))
  }
  pubs <- tryCatch(trama::tr_project_config(raiz, .TD_REGISTRO), error = function(e) NULL) %||% list()
  linhas <- list()
  for (a in alvos) {
    doc <- a$doc
    # Primeiro o fluxo sem os nós publicar e as arestas que chegavam neles:
    # as regiões novas se calculam (e ganham a chave do store) já nele.
    trama::tr_doc_write(.td_sem_nos(doc, a$nos), a$caminho)
    for (id in a$nos) {
      r <- .td_migrar_no(projeto, raiz, a$fluxo, doc, id, pubs[[id]])
      if (!is.null(pubs[[id]]) || !is.null(r$registro)) {
        pubs[[r$documento]] <- utils::modifyList(pubs[[id]] %||% list(), r$registro %||% list())
        pubs[[id]] <- NULL
      }
      linhas[[length(linhas) + 1L]] <- list(fluxo = a$fluxo, no = id, documento = r$documento,
                                            regioes = r$regioes, avisos = paste(r$avisos, collapse = " "))
    }
  }
  if (file.exists(file.path(raiz, "trama.json"))) {
    trama::tr_project_config_set(raiz, .TD_REGISTRO, if (length(pubs)) pubs else NULL)
  }
  .td_quadro(linhas, colunas)
}

#' Fluxos do projeto que têm nó `publicar/publish`, lidos do disco (a
#' coleção não precisa estar instalada).
#' @noRd
.td_fluxos_com_publicar <- function(raiz) {
  arqs <- sort(Sys.glob(file.path(raiz, "flows", "*.json")))
  out <- list()
  for (f in arqs) {
    doc <- tryCatch(trama::tr_doc_read(f), error = function(e) NULL)
    if (is.null(doc)) next
    nos <- names(doc$nodes)[vapply(doc$nodes, function(n) identical(n$type, .TD_NO_PUBLICAR), TRUE)]
    if (length(nos)) out[[length(out) + 1L]] <- list(fluxo = tools::file_path_sans_ext(basename(f)), caminho = f,
                                                     doc = doc, nos = nos)
  }
  out
}

#' Tira `trama.publicar` da lista de coleções do `trama.json`. A lista é
#' chave do núcleo (sem verbo para plugin); a gravação segue a dele: o resto
#' do arquivo como está, `collections` sempre como lista, troca atômica.
#' @noRd
.td_tirar_colecao_publicar <- function(raiz) {
  cfg_path <- file.path(raiz, "trama.json")
  if (!file.exists(cfg_path)) return(invisible())
  cfg <- jsonlite::fromJSON(cfg_path, simplifyVector = FALSE)
  cols <- as.character(unlist(cfg$collections))
  if (!"trama.publicar" %in% cols) return(invisible())
  cfg$collections <- I(setdiff(cols, "trama.publicar"))
  json <- enc2utf8(as.character(jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = TRUE, digits = NA)))
  tmp <- tempfile(".trama-", tmpdir = raiz, fileext = ".part")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(charToRaw(paste0(json, "\n")), tmp)
  file.rename(tmp, cfg_path)
  invisible()
}

.td_sem_nos <- function(doc, nos) {
  doc$nodes <- doc$nodes[setdiff(names(doc$nodes), nos)]
  doc$edges <- Filter(function(e) !e$to$node %in% nos && !e$from$node %in% nos, doc$edges)
  if (!is.null(doc$ui$positions)) doc$ui$positions <- doc$ui$positions[setdiff(names(doc$ui$positions), nos)]
  if (is.list(doc$collections)) doc$collections$publicar <- NULL
  doc
}

#' Um nó vira um documento. Devolve `documento`, `regioes`, `avisos` e
#' `registro` (o repositório pedido no param, se a publicação não tinha um).
#' @noRd
.td_migrar_no <- function(projeto, raiz, fluxo, doc, id, registro) {
  p <- doc$nodes[[id]]$params %||% list()
  txt <- function(k) {
    v <- p[[k]]
    if (is.character(v) && length(v) && !is.na(v[[1]])) trimws(v[[1]]) else ""
  }
  titulo <- txt("titulo") %|v|% (doc$nodes[[id]]$label %||% "Publica\u{E7}\u{E3}o")
  nome <- .td_nome_documento_livre(raiz, .td_slug(if (nzchar(txt("titulo"))) txt("titulo") else id))
  qmd <- tr_documento_criar(nome, raiz, modelo = "pagina", perfil = FALSE, titulo = titulo, fluxo = fluxo)
  rel <- .td_relativo(raiz, qmd)
  avisos <- character()

  # O YAML: o trama acabou de criar o documento, então ainda é dele.
  d <- .td_ler(qmd)
  f <- .td_fim_yaml(d$linhas)
  y <- .td_yaml_listas(d$linhas)
  corpo <- d$linhas[(f + 1L):length(d$linhas)]
  autores <- .td_autores_param(txt("autores"))
  if (length(autores)) y$author <- autores
  if (nzchar(txt("descricao"))) y$description <- txt("descricao") else y$description <- NULL
  chaves <- trimws(strsplit(txt("palavras_chave"), ",", fixed = TRUE)[[1]])
  y$keywords <- if (length(chaves[nzchar(chaves)])) as.list(chaves[nzchar(chaves)]) else NULL
  pub <- list(licenca = txt("licenca") %|v|% "CC-BY-4.0", "licenca-codigo" = txt("licenca_codigo") %|v|% "MIT",
              versao = txt("versao") %|v|% "menor")
  if (isTRUE(p$privado)) pub$privado <- TRUE
  if (isTRUE(p$aceitar_dados_treino)) pub[["aceitar-dados-treino"]] <- TRUE
  y$trama$publicar <- pub

  # O corpo: o do esqueleto até "## Resultados" é o cabeçalho (o setup do
  # knitr fica sempre); o resto segue o layout.
  i_res <- which(grepl("^## Resultados\\s*$", .td_sem_cr(corpo)))[1]
  cabeca <- if (is.na(i_res)) corpo else corpo[seq_len(i_res - 1L)]
  setup <- cabeca[grepl("_knitr.qmd", cabeca, fixed = TRUE)]
  itens <- .td_itens_publicar(doc, id)
  blocos <- .td_layout_migrar(txt("layout"), names(itens))
  if (!is.null(blocos$aviso)) avisos <- c(avisos, blocos$aviso)
  tem_cabecalho <- any(vapply(blocos$lista, function(b) identical(b$tipo, "cabecalho"), TRUE))
  inicio <- if (tem_cabecalho) cabeca else c("", setup, "")
  d$linhas <- c("---", .td_yaml_texto(y), "---", .td_aparar_fim(inicio))
  .td_gravar(qmd, d)
  .td_yaml_gravar(.td_variaveis(y, versao = "rascunho"), file.path(dirname(qmd), "_variables.yml"))

  acrescentar <- function(linhas) {
    dd <- .td_ler(qmd)
    dd$linhas <- c(.td_aparar_fim(dd$linhas), "", linhas)
    .td_gravar(qmd, dd)
  }
  n <- 0L
  for (b in blocos$lista) {
    switch(b$tipo,
      cabecalho = NULL,
      texto = acrescentar(.td_linhas(b$texto %||% "")),
      fluxo = acrescentar(c("## O fluxo", "",
                            "![O fluxo do trama que gerou este material.](fluxo.svg){fig-alt=\"Diagrama do fluxo\"}")),
      item = {
        it <- itens[[b$id]]
        rot <- b$titulo %||% doc$nodes[[it$no]]$label %||% projeto$registry$nodes[[doc$nodes[[it$no]]$type]]$label %||% it$no
        acrescentar(paste0("## ", rot))
        r <- tryCatch(tr_documento_inserir(qmd, it$no, projeto, porta = it$porta,
                                           aceitar_dados_treino = isTRUE(p$aceitar_dados_treino)),
                      error = function(e) e)
        if (inherits(r, "error")) {
          msg <- conditionMessage(r)
          avisos <- c(avisos, sprintf("'%s' n\u{E3}o entrou: %s", it$no, msg))
          acrescentar(sprintf("*(O resultado de `%s` n\u{E3}o p\u{F4}de ser inserido na migra\u{E7}\u{E3}o: %s Insira de novo pelo menu do card.)*",
                              it$no, gsub("[\r\n]+", " ", msg)))
        } else {
          n <- n + 1L
          avisos <- c(avisos, r$avisos)
        }
        if (nzchar(b$legenda %||% "")) acrescentar(b$legenda)
      })
  }
  repo <- txt("repositorio")
  list(documento = rel, regioes = n, avisos = avisos,
       registro = if (nzchar(repo) && is.null(registro$repo)) list(repo = sub("^https?://github.com/", "", repo)))
}

#' Os itens ligados ao nó, por id de origem e na ordem das arestas: `no` e
#' `porta` de onde vêm.
#' @noRd
.td_itens_publicar <- function(doc, id) {
  out <- list()
  for (e in doc$edges) {
    if (!identical(e$to$node, id) || is.null(doc$nodes[[e$from$node]])) next
    out[[e$from$node]] <- list(no = e$from$node, porta = e$from$port)
  }
  out
}

#' O layout do nó (JSON) como lista de blocos; vazio é o automático do
#' publicar: cabeçalho, cada item na ordem das arestas e o diagrama.
#' @noRd
.td_layout_migrar <- function(layout, ids) {
  auto <- c(list(list(tipo = "cabecalho")), lapply(ids, function(i) list(tipo = "item", id = i)),
            list(list(tipo = "fluxo")))
  if (!nzchar(trimws(layout))) return(list(lista = auto))
  l <- tryCatch(jsonlite::fromJSON(layout, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(l) || !is.null(names(l))) {
    return(list(lista = auto, aviso = "O layout do Publicar n\u{E3}o se leu: o documento segue o autom\u{E1}tico."))
  }
  l <- Filter(function(b) is.list(b) && (b$tipo %||% "") %in% c("cabecalho", "texto", "item", "fluxo") &&
                (!identical(b$tipo, "item") || (b$id %||% "") %in% ids), l)
  list(lista = l)
}

#' Autores no formato do param antigo: `Nome Sobrenome (0000-0000-0000-0000)`,
#' separados por `;` -> `author` do Quarto.
#' @noRd
.td_autores_param <- function(texto) {
  partes <- trimws(strsplit(texto %||% "", ";", fixed = TRUE)[[1]])
  partes <- partes[nzchar(partes)]
  lapply(partes, function(p) {
    orcid <- regmatches(p, regexpr("[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]", p))
    nome <- trimws(gsub("\\(?\\s*(https?://orcid.org/)?[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]\\s*\\)?", "", p))
    Filter(Negate(is.null), list(name = nome, orcid = if (length(orcid)) orcid))
  })
}

#' Nome de documento que ainda não existe (`x`, `x-2`, `x-3`…).
#' @noRd
.td_nome_documento_livre <- function(raiz, base) {
  nome <- base
  k <- 1L
  while (dir.exists(file.path(raiz, "documentos", nome))) {
    k <- k + 1L
    nome <- paste0(base, "-", k)
  }
  nome
}

.td_aparar_fim <- function(l) {
  cheias <- which(nzchar(trimws(.td_sem_cr(l))))
  if (length(cheias)) l[seq_len(cheias[[length(cheias)]])] else character()
}
