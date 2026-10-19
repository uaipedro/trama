# A montagem: material + o que só o editor sabe -> a pasta do pacote.
#
# Determinística dados os mesmos argumentos (a data entra como argumento): é
# a mesma função para a prévia local e, na fase de envio, para o que vai ao
# GitHub — o que se vê na prévia é o que sai.

.TR_PUBLICAR_QUARTO_LIVE <- "0.2.0"
.TR_PUBLICAR_ESQUEMA_FLUXO <- "https://github.com/uaipedro/trama/blob/main/inst/schema/document-v1.json"

#' Monta a pasta do pacote RO-Crate
#'
#' Grava `itens/<id>/`, `flow.json`, `fluxo.svg`, `dados-entrada.json`,
#' `ambiente.json`, `README.md`, `CITATION.cff`, `LICENSE`, a página
#' (`index.qmd` + a extensão quarto-live fixada em `_extensions/`) e, por
#' último, o `ro-crate-metadata.json`, que descreve todos os outros com
#' tamanho e sha256.
#' @param material O valor do nó `publicar` ([tr_publicar_material()]).
#' @param dir Pasta de destino (criada; o conteúdo anterior é apagado).
#' @param doc O documento do fluxo (`NULL` = sem `flow.json` nem diagrama).
#' @param rotulos Lista nomeada id do nó -> rótulo, para o diagrama.
#' @param dados_entrada Lista de descrições dos dados não ligados (ver
#'   `.tr_publicar_dados_entrada()`).
#' @param data Data da publicação (`Date`).
#' @return Invisível: `list(dir, arquivos)`.
#' @export
tr_publicar_montar <- function(material, dir, doc = NULL, rotulos = list(), dados_entrada = list(),
                               data = Sys.Date()) {
  if (!inherits(material, "tr_publicar_material")) {
    rlang::abort("tr_publicar_montar() recebe o valor do n\u{F3} publicar.", class = "tr_publicar_error_bad_item")
  }
  if (dir.exists(dir)) unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  escrever <- function(rel, conteudo) {
    p <- file.path(dir, rel)
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    if (is.raw(conteudo)) writeBin(conteudo, p) else writeLines(enc2utf8(conteudo), p, useBytes = TRUE)
    rel
  }
  for (id in names(material$itens)) {
    it <- material$itens[[id]]
    for (nm in names(it$arquivos)) escrever(file.path("itens", .tr_publicar_pasta(id), nm), it$arquivos[[nm]])
  }
  tem_fluxo <- !is.null(doc)
  if (tem_fluxo) {
    escrever("flow.json", as.character(trama::tr_doc_json(doc)))
    escrever("fluxo.svg", .tr_publicar_fluxo_svg(doc, rotulos))
  }
  escrever("dados-entrada.json", .tr_publicar_json_txt(list(
    nota = paste("Dados que o fluxo l\u{EA} e que n\u{E3}o foram ligados ao publicar: vai a descri\u{E7}\u{E3}o,",
                 "n\u{E3}o o dado. Para reproduzir, traga o arquivo com o mesmo sha256."),
    dados = dados_entrada)))
  escrever("ambiente.json", .tr_publicar_json_txt(material$ambiente))
  escrever("README.md", .tr_publicar_readme(material, dados_entrada, tem_fluxo))
  escrever("CITATION.cff", .tr_publicar_cff(material, data))
  escrever("LICENSE", .tr_publicar_licenca(material, data))
  escrever("index.qmd", tr_publicar_pagina(material, tem_fluxo = tem_fluxo, data = data))
  ext <- system.file("quarto", "_extensions", package = "trama.publicar")
  if (nzchar(ext)) file.copy(ext, dir, recursive = TRUE)
  escrever("ro-crate-metadata.json", .tr_publicar_json_txt(.tr_publicar_rocrate(material, dir, data, dados_entrada)))
  invisible(list(dir = dir, arquivos = .tr_publicar_listar(dir)))
}

#' Id de nó como nome de pasta: só o seguro em URL e em qualquer sistema.
#' @noRd
.tr_publicar_pasta <- function(id) gsub("[^A-Za-z0-9_.-]", "_", id)

.tr_publicar_json_txt <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA))
}

.tr_publicar_listar <- function(dir) {
  # `radix`: ordem de bytes, igual em qualquer locale (o snapshot e o crate).
  sort(list.files(dir, recursive = TRUE, all.files = FALSE), method = "radix")
}

.tr_publicar_sha256 <- function(path) digest::digest(file = path, algo = "sha256")

# ---- RO-Crate ----------------------------------------------------------------------

#' `encodingFormat` pela extensão (IANA).
#' @noRd
.tr_publicar_formato <- function(arq) {
  switch(tolower(tools::file_ext(arq)),
         csv = "text/csv", json = "application/json", png = "image/png", svg = "image/svg+xml",
         rds = "application/x-r-rds", md = "text/markdown", qmd = "text/markdown",
         cff = "application/x-yaml", yml = "application/x-yaml", lua = "text/x-lua",
         js = "text/javascript", css = "text/css", ojs = "text/javascript",
         "application/octet-stream")
}

.tr_publicar_licenca_url <- function(spdx) paste0("https://spdx.org/licenses/", spdx)

.tr_publicar_pessoa <- function(a) {
  id <- if (!is.null(a$orcid)) paste0("https://orcid.org/", a$orcid) else paste0("#", gsub("\\s+", "-", tolower(a$nome)))
  list("@id" = id, "@type" = "Person", name = a$nome)
}

#' O `ro-crate-metadata.json` (RO-Crate 1.1): o descritor, o dataset raiz,
#' as pessoas, as licenças e cada arquivo da pasta. Modelos com o
#' vocabulário FAIR4ML (`https://w3id.org/fair4ml#`).
#' @noRd
.tr_publicar_rocrate <- function(material, dir, data, dados_entrada) {
  m <- material$metadados
  arquivos <- setdiff(.tr_publicar_listar(dir), "ro-crate-metadata.json")
  # A extensão do Quarto é ferramenta, não dado: um Dataset só, com o nome.
  ext <- grepl("^_extensions/", arquivos)
  arquivos <- arquivos[!ext]
  pessoas <- lapply(m$autores, .tr_publicar_pessoa)
  descricao_item <- list()
  for (id in names(material$itens)) {
    it <- material$itens[[id]]
    for (nm in names(it$arquivos)) {
      descricao_item[[file.path("itens", .tr_publicar_pasta(id), nm)]] <- list(item = it, id = id, arquivo = nm)
    }
  }
  entidade <- function(rel) {
    p <- file.path(dir, rel)
    e <- list("@id" = rel, "@type" = "File", name = basename(rel),
              encodingFormat = .tr_publicar_formato(rel),
              contentSize = as.character(file.info(p)$size),
              sha256 = .tr_publicar_sha256(p))
    d <- descricao_item[[rel]]
    if (!is.null(d)) {
      md <- d$item$metadados
      e$name <- paste0(d$item$titulo, " \u{2014} ", d$arquivo)
      e$description <- md$descricao %||% NULL
      # Modelo: o arquivo do modelo reduzido é o `fair4ml:MLModel`; o motor e
      # o web.json são partes dele.
      if (identical(md$tipo_schema, "fair4ml:MLModel") && identical(d$arquivo, "modelo.rds")) {
        e[["@type"]] <- list("File", "fair4ml:MLModel")
        e <- c(e, Filter(Negate(is.null), md$propriedades %||% list()))
      } else if (!is.null(md$tipo_schema) && !identical(md$tipo_schema, "fair4ml:MLModel") &&
                 !identical(md$tipo_schema, "Dataset")) {
        e[["@type"]] <- list("File", md$tipo_schema)
      }
      if (identical(md$tipo_schema, "Dataset") && !is.null(md$propriedades$variableMeasured)) {
        e$variableMeasured <- md$propriedades$variableMeasured
      }
    }
    e
  }
  especiais <- list(
    "flow.json" = "O fluxo do trama que gerou tudo; abre no editor.",
    "fluxo.svg" = "Diagrama do fluxo.",
    "dados-entrada.json" = "Descri\u{E7}\u{E3}o e sha256 dos dados de entrada n\u{E3}o inclu\u{ED}dos.",
    "ambiente.json" = "Vers\u{F5}es de R, do trama, das cole\u{E7}\u{F5}es e dos pacotes.",
    "index.qmd" = "Fonte da p\u{E1}gina (Quarto + quarto-live).",
    "README.md" = "Como citar e reproduzir.", "CITATION.cff" = "Cita\u{E7}\u{E3}o.", "LICENSE" = "Licen\u{E7}as.")
  arquivos_ent <- lapply(arquivos, function(rel) {
    e <- entidade(rel)
    if (!is.null(especiais[[rel]])) e$description <- especiais[[rel]]
    if (rel == "flow.json") e$conformsTo <- list("@id" = .TR_PUBLICAR_ESQUEMA_FLUXO)
    e
  })
  tem_ext <- any(ext)
  partes <- c(lapply(arquivos, function(a) list("@id" = a)), if (tem_ext) list(list("@id" = "_extensions/")))
  raiz <- Filter(Negate(is.null), list(
    "@id" = "./", "@type" = "Dataset", name = m$titulo,
    description = if (nzchar(m$descricao)) m$descricao else m$titulo,
    datePublished = format(data, "%Y-%m-%d"), version = m$versao,
    license = list("@id" = .tr_publicar_licenca_url(m$licenca)),
    keywords = if (length(m$palavras_chave)) as.list(m$palavras_chave) else NULL,
    author = lapply(pessoas, function(p) list("@id" = p[["@id"]])),
    hasPart = partes))
  grafo <- c(
    list(list("@id" = "ro-crate-metadata.json", "@type" = "CreativeWork",
              conformsTo = list("@id" = "https://w3id.org/ro/crate/1.1"), about = list("@id" = "./"))),
    list(raiz),
    pessoas,
    list(list("@id" = .tr_publicar_licenca_url(m$licenca), "@type" = "CreativeWork", name = m$licenca),
         list("@id" = .tr_publicar_licenca_url(m$licenca_codigo), "@type" = "CreativeWork", name = m$licenca_codigo)),
    arquivos_ent,
    if ("flow.json" %in% arquivos) list(list("@id" = .TR_PUBLICAR_ESQUEMA_FLUXO, "@type" = "CreativeWork",
                                             name = "Documento do trama, formato 1 (JSON Schema)")),
    if (tem_ext) list(list("@id" = "_extensions/", "@type" = "Dataset",
                           name = paste0("Extens\u{E3}o quarto-live ", .TR_PUBLICAR_QUARTO_LIVE, " (r-wasm/quarto-live, MIT)"),
                           url = "https://github.com/r-wasm/quarto-live")))
  list("@context" = list("https://w3id.org/ro/crate/1.1/context", list(fair4ml = "https://w3id.org/fair4ml#")),
       "@graph" = grafo)
}

# ---- textos do pacote --------------------------------------------------------------

.tr_publicar_autores_txt <- function(autores) {
  paste(vapply(autores, function(a) if (is.null(a$orcid)) a$nome else sprintf("%s (https://orcid.org/%s)", a$nome, a$orcid), ""),
        collapse = "; ")
}

#' Citação em texto corrido: Autores (ano). Título. Versão X.
#' @noRd
.tr_publicar_citacao <- function(material, data = Sys.Date()) {
  m <- material$metadados
  sprintf("%s (%s). %s. Vers\u{E3}o %s.%s", paste(vapply(m$autores, function(a) a$nome, ""), collapse = "; "),
          format(data, "%Y"), m$titulo, m$versao,
          if (nzchar(m$repositorio)) paste0(" ", m$repositorio, ".") else "")
}

.tr_publicar_readme <- function(material, dados_entrada, tem_fluxo) {
  m <- material$metadados
  modelo <- .tr_publicar_modelo_texto("readme.md")
  dados <- if (length(dados_entrada)) {
    paste(vapply(dados_entrada, function(d) {
      sprintf("- `%s` (%s): %s linhas, %s colunas; sha256 `%s`", d$arquivo %||% d$no, d$rotulo %||% "",
              d$linhas %||% "?", length(d$colunas %||% list()), d$sha256 %||% "\u{2014}")
    }, ""), collapse = "\n")
  } else "Nenhum: tudo o que o fluxo l\u{EA} est\u{E1} no pacote."
  itens <- paste(vapply(names(material$itens), function(id) {
    it <- material$itens[[id]]
    sprintf("- `itens/%s/` \u{2014} %s (%s): %s", .tr_publicar_pasta(id), it$titulo, it$tipo,
            paste(names(it$arquivos), collapse = ", "))
  }, ""), collapse = "\n")
  .tr_publicar_preencher(modelo, list(
    titulo = m$titulo, descricao = m$descricao, autores = .tr_publicar_autores_txt(m$autores),
    versao = m$versao, licenca = m$licenca, licenca_codigo = m$licenca_codigo,
    citacao = .tr_publicar_citacao(material), itens = if (nzchar(itens)) itens else "Nenhum.",
    dados = dados, fluxo = if (tem_fluxo) "`flow.json`" else "(o fluxo n\u{E3}o foi inclu\u{ED}do)"))
}

.tr_publicar_cff <- function(material, data) {
  m <- material$metadados
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  autores <- vapply(m$autores, function(a) {
    partes <- strsplit(a$nome, "\\s+")[[1]]
    given <- paste(utils::head(partes, -1L), collapse = " ")
    fam <- utils::tail(partes, 1L)
    linhas <- c(sprintf("  - family-names: %s", q(fam)),
                if (nzchar(given)) sprintf("    given-names: %s", q(given)),
                if (!is.null(a$orcid)) sprintf("    orcid: %s", q(paste0("https://orcid.org/", a$orcid))))
    paste(linhas, collapse = "\n")
  }, "")
  paste(c(
    "cff-version: 1.2.0",
    sprintf("message: %s", q("Se usar este material, cite-o como abaixo.")),
    "type: dataset",
    sprintf("title: %s", q(m$titulo)),
    if (nzchar(m$descricao)) sprintf("abstract: %s", q(m$descricao)),
    "authors:", autores,
    sprintf("version: %s", q(m$versao)),
    sprintf("date-released: %s", q(format(data, "%Y-%m-%d"))),
    sprintf("license: %s", q(m$licenca)),
    if (length(m$palavras_chave)) c("keywords:", sprintf("  - %s", q(m$palavras_chave))),
    if (grepl("^https?://", m$repositorio)) sprintf("repository: %s", q(m$repositorio))
  ), collapse = "\n")
}

.tr_publicar_licenca <- function(material, data) {
  m <- material$metadados
  titulares <- paste(vapply(m$autores, function(a) a$nome, ""), collapse = ", ")
  .tr_publicar_preencher(.tr_publicar_modelo_texto("licenca.txt"), list(
    licenca = m$licenca, licenca_url = .tr_publicar_licenca_url(m$licenca),
    licenca_codigo = m$licenca_codigo, licenca_codigo_url = .tr_publicar_licenca_url(m$licenca_codigo),
    ano = format(data, "%Y"), titulares = titulares,
    texto_codigo = if (m$licenca_codigo == "MIT") {
      .tr_publicar_preencher(.tr_publicar_modelo_texto("mit.txt"), list(ano = format(data, "%Y"), titulares = titulares))
    } else
      sprintf("O texto integral est\u{E1} em %s.", .tr_publicar_licenca_url(m$licenca_codigo))))
}

#' Texto de `inst/modelos/` (UTF-8): os textos longos em português ficam
#' fora do código R.
#' @noRd
.tr_publicar_modelo_texto <- function(nome) {
  p <- system.file("modelos", nome, package = "trama.publicar")
  paste(readLines(p, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

#' `{{chave}}` -> valor.
#' @noRd
.tr_publicar_preencher <- function(modelo, valores) {
  for (k in names(valores)) modelo <- gsub(paste0("{{", k, "}}"), valores[[k]] %||% "", modelo, fixed = TRUE)
  modelo
}

# ---- diagrama do fluxo -------------------------------------------------------------

.tr_publicar_xml <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE); gsub("\"", "&quot;", x, fixed = TRUE)
}

#' O grafo desenhado, estático: caixas nas posições do editor, fios entre
#' elas. SVG puro, sem fonte externa.
#' @noRd
.tr_publicar_fluxo_svg <- function(doc, rotulos = list()) {
  ids <- names(doc$nodes)
  if (!length(ids)) return("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10\" height=\"10\"/>")
  pos <- lapply(ids, function(id) {
    p <- unlist(doc$ui$positions[[id]] %||% c(0, 0))
    c(as.numeric(p[[1]]), as.numeric(p[[2]]))
  })
  names(pos) <- ids
  xs <- vapply(pos, `[[`, 0, 1); ys <- vapply(pos, `[[`, 0, 2)
  larg <- 200; alt <- 44; margem <- 20
  ox <- min(xs) - margem; oy <- min(ys) - margem
  w <- max(xs) - ox + larg + margem; h <- max(ys) - oy + alt + margem
  rot <- function(id) rotulos[[id]] %||% doc$nodes[[id]]$label %||% doc$nodes[[id]]$type
  caixas <- vapply(ids, function(id) {
    x <- pos[[id]][1] - ox; y <- pos[[id]][2] - oy
    sprintf(paste0("<g><rect x=\"%.0f\" y=\"%.0f\" width=\"%d\" height=\"%d\" rx=\"8\" fill=\"#ffffff\" ",
                   "stroke=\"#64748b\"/><text x=\"%.0f\" y=\"%.0f\" font-family=\"sans-serif\" font-size=\"13\" ",
                   "fill=\"#0f172a\">%s</text><text x=\"%.0f\" y=\"%.0f\" font-family=\"monospace\" font-size=\"10\" ",
                   "fill=\"#64748b\">%s</text></g>"),
            x, y, larg, alt, x + 10, y + 19, .tr_publicar_xml(substr(rot(id), 1, 28)),
            x + 10, y + 35, .tr_publicar_xml(doc$nodes[[id]]$type))
  }, "")
  fios <- vapply(doc$edges, function(e) {
    a <- pos[[e$from$node]]; b <- pos[[e$to$node]]
    if (is.null(a) || is.null(b)) return("")
    x1 <- a[1] - ox + larg; y1 <- a[2] - oy + alt / 2; x2 <- b[1] - ox; y2 <- b[2] - oy + alt / 2
    meio <- (x1 + x2) / 2
    sprintf("<path d=\"M%.0f,%.0f C%.0f,%.0f %.0f,%.0f %.0f,%.0f\" fill=\"none\" stroke=\"#94a3b8\" stroke-width=\"1.5\"/>",
            x1, y1, meio, y1, meio, y2, x2, y2)
  }, "")
  paste0("<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 ", round(w), " ", round(h),
         "\" width=\"", round(w), "\" height=\"", round(h), "\" role=\"img\" aria-label=\"Diagrama do fluxo\">",
         paste(fios, collapse = ""), paste(caixas, collapse = ""), "</svg>")
}
