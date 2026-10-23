# O pacote reprodutível de um documento: a pasta que vai ao GitHub.
#
# Portado do `trama.publicar` (`R/montar.R` e a parte de dados de entrada de
# `R/previa.R`). Determinístico dados os mesmos argumentos (a data entra como
# argumento). O documento vai como `index.qmd`, com a pasta dele (modelo em
# `_extensions/`, `_brand.yml`, `_trama/` com os resultados materializados);
# somam-se o fluxo, o diagrama, a descrição dos dados de entrada, o ambiente,
# README, CITATION.cff, LICENSE e, por último, o `ro-crate-metadata.json`,
# que descreve todos os outros com tamanho e sha256.

.TD_QUARTO_LIVE <- "0.2.0"
# Termos próprios do pacote (a marca de dados de treino), num namespace do
# repositório: não há termo schema.org nem FAIR4ML para "o modelo contém
# linhas do treino".
.TD_VOCABULARIO <- "https://github.com/uaipedro/trama/blob/main/plugins/trama.documentos/README.md#"
.TD_ESQUEMA_FLUXO <- "https://github.com/uaipedro/trama/blob/main/inst/schema/document-v1.json"

#' Monta o pacote de publicação de um documento
#'
#' A pasta que "Publicar no GitHub" envia, e que se pode conferir antes:
#' o documento como `index.qmd`, a pasta dele (sem `_saida/` nem arquivos
#' ocultos; de `_trama/`, só o que o texto usa), `flow.json`, `fluxo.svg`,
#' `dados-entrada.json`, `ambiente.json`, `README.md`, `CITATION.cff`,
#' `LICENSE`, `_variables.yml` com a versão e a citação, e o
#' `ro-crate-metadata.json`. Bloqueios de [tr_documento_publicacao()] param
#' a montagem.
#'
#' @inheritParams tr_documento_inserir
#' @param dir Pasta de destino (criada; o conteúdo anterior é apagado).
#' @param versao Versão do pacote (`"1.2.0"`); `NULL`, `"rascunho"`.
#' @param repositorio Endereço do repositório da publicação (vai para a
#'   citação).
#' @param data Data da publicação (`Date`).
#' @return Invisível: `list(dir, arquivos, publicacao)`.
#' @export
tr_documento_pacote <- function(documento, dir, projeto = NULL, versao = NULL, repositorio = "", data = Sys.Date()) {
  projeto <- .td_projeto(projeto, documento)
  pub <- tr_documento_publicacao(documento, projeto)
  if (length(pub$erros)) {
    .td_erro(paste(c("O documento ainda n\u{E3}o pode ser publicado:", paste("-", pub$erros)), collapse = "\n"),
             "tr_documento_erro_publicar")
  }
  versao <- versao %||% "rascunho"
  documento <- normalizePath(documento)
  pasta_doc <- dirname(documento)
  d <- .td_ler(documento)
  y <- .td_yaml(d$linhas)
  doc <- trama::tr_project_flow(projeto, .td_fluxo_do(d$linhas))

  if (dir.exists(dir)) unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  escrever <- function(rel, conteudo) {
    p <- file.path(dir, rel)
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    if (is.raw(conteudo)) writeBin(conteudo, p) else writeLines(enc2utf8(conteudo), p, useBytes = TRUE)
    rel
  }
  for (rel in .td_pacote_arquivos(documento, d$linhas)) {
    destino <- if (identical(rel, basename(documento))) "index.qmd" else rel
    dir.create(dirname(file.path(dir, destino)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(pasta_doc, rel), file.path(dir, destino))
  }
  # O `_quarto.yml` do documento manda a saída para `_saida/`; no pacote a
  # página sai ao lado do `index.qmd`, que é o que o Pages serve.
  qy <- if (file.exists(file.path(pasta_doc, "_quarto.yml"))) .td_yaml_ler(file.path(pasta_doc, "_quarto.yml")) else list()
  if (is.list(qy$project)) qy$project[["output-dir"]] <- NULL
  if (!length(qy$project)) qy$project <- list(type = "default")
  escrever("_quarto.yml", .td_yaml_texto(qy))
  escrever("_variables.yml", .td_yaml_texto(.td_variaveis(y, versao, repositorio, data)))

  m <- pub$metadados
  ambiente <- .td_ambiente(projeto, pasta_doc, pub$regioes)
  completas <- vapply(Filter(function(r) isTRUE(r$tabela_completa), pub$regioes), function(r) r$no, "")
  dados_entrada <- .td_dados_entrada(doc, projeto, completas)
  escrever("flow.json", as.character(trama::tr_doc_json(doc)))
  escrever("fluxo.svg", .td_fluxo_svg(doc, .td_rotulos(doc, projeto$registry)))
  escrever("dados-entrada.json", .td_json_txt(list(
    nota = paste("Dados que o fluxo l\u{EA} e que n\u{E3}o vieram inteiros no pacote: vai a descri\u{E7}\u{E3}o,",
                 "n\u{E3}o o dado. Para reproduzir, traga o arquivo com o mesmo sha256."),
    dados = dados_entrada)))
  escrever("ambiente.json", .td_json_txt(ambiente))
  escrever("README.md", .td_readme(m, versao, repositorio, data, pub$regioes, dados_entrada))
  escrever("CITATION.cff", .td_cff(m, versao, repositorio, data))
  escrever("LICENSE", .td_licenca(m, data))
  escrever("ro-crate-metadata.json", .td_json_txt(.td_rocrate(m, versao, dir, data, pasta_doc, pub$regioes,
                                                              web = .td_web(d$linhas, pasta_doc))))
  invisible(list(dir = dir, arquivos = .td_listar(dir), publicacao = pub))
}

#' Arquivos da pasta do documento que vão no pacote (relativos a ela): tudo
#' menos `_saida/`, `.quarto/`, ocultos e `_variables.yml`/`_quarto.yml`
#' (refeitos); de `_trama/`, as pastas das regiões e as que o texto cita.
#' @noRd
.td_pacote_arquivos <- function(documento, linhas) {
  pasta <- dirname(documento)
  todos <- list.files(pasta, recursive = TRUE, all.files = FALSE)
  todos <- todos[!grepl("^(_saida|\\.quarto)/", todos) & !grepl("(^|/)\\.", todos) &
                   !todos %in% c("_quarto.yml", "_variables.yml")]
  usadas <- vapply(.td_regioes(linhas), `[[`, "", "chave")
  texto <- paste(linhas, collapse = "\n")
  em_trama <- grepl(paste0("^", .TD_PASTA, "/"), todos)
  pasta_de <- sub(paste0("^", .TD_PASTA, "/([^/]+)/.*$"), "\\1", todos)
  usada <- pasta_de %in% usadas | vapply(pasta_de, function(p) grepl(paste0(.TD_PASTA, "/", p, "/"), texto, fixed = TRUE), TRUE)
  sort(todos[!em_trama | usada], method = "radix")
}

.td_json_txt <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA))
}

.td_listar <- function(dir) {
  # `radix`: ordem de bytes, igual em qualquer locale (o snapshot e o crate).
  sort(list.files(dir, recursive = TRUE, all.files = FALSE), method = "radix")
}

.td_sha256 <- function(path) digest::digest(file = path, algo = "sha256")

#' Versões de R, do trama, das coleções do projeto e dos pacotes que os
#' preditores usam (os do `web.json`).
#' @noRd
.td_ambiente <- function(projeto, pasta_doc, regioes) {
  ver <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  colecoes <- sort(unique(c(grep("^trama\\.", loadedNamespaces(), value = TRUE), "trama.documentos")))
  colecoes <- setdiff(colecoes, "trama.github")
  pacotes <- unique(unlist(lapply(regioes, function(r) {
    f <- file.path(.td_pasta_regiao(pasta_doc, r$chave), "web.json")
    if (file.exists(f)) names(jsonlite::fromJSON(f, simplifyVector = FALSE)$pacotes)
  })))
  pacotes <- sort(setdiff(pacotes, "stats"))
  list(r = paste(R.version$major, R.version$minor, sep = "."),
       trama = ver("trama"),
       colecoes = as.list(stats::setNames(vapply(colecoes, ver, ""), colecoes)),
       pacotes = as.list(stats::setNames(vapply(pacotes, ver, ""), pacotes)))
}

#' Os dados que o fluxo lê e que não vão inteiros no pacote: a descrição
#' (colunas, linhas) do resultado em cache e o sha256 do arquivo, quando o nó
#' lê um arquivo do projeto (`path`).
#' @noRd
.td_dados_entrada <- function(doc, projeto, ligados = character()) {
  registry <- projeto$registry
  raiz <- projeto$root
  plano <- tryCatch(.td_na_raiz(projeto, trama::tr_plan(doc, registry = registry, store = projeto$store,
                                                        settings = projeto$settings)),
                    error = function(e) NULL)
  out <- list()
  for (id in names(doc$nodes)) {
    no <- doc$nodes[[id]]
    spec <- registry$nodes[[no$type]]
    if (is.null(spec) || length(spec$inputs) || id %in% ligados) next
    portas <- names(spec$outputs)[vapply(spec$outputs, function(p) identical(p$type, "data/table"), TRUE)]
    if (!length(portas)) next
    h <- plano$units[[id]]$handles[[portas[[1]]]]
    caminho <- no$params$path %||% no$params$caminho %||% ""
    arq <- if (is.character(caminho) && nzchar(caminho) && !grepl("^[a-z]+://", caminho)) {
      p <- if (grepl("^(/|[A-Za-z]:)", caminho)) caminho else file.path(raiz, caminho)
      if (file.exists(p) && !dir.exists(p)) p
    }
    out[[length(out) + 1L]] <- Filter(Negate(is.null), list(
      no = id, rotulo = no$label %||% spec$label, bloco = no$type,
      arquivo = if (!is.null(arq)) basename(arq) else if (nzchar(caminho)) caminho else NULL,
      sha256 = if (!is.null(arq)) .td_sha256(arq) else NULL,
      bytes = if (!is.null(arq)) as.character(file.info(arq)$size) else NULL,
      linhas = h$summary$linhas,
      colunas = lapply(h$schema$colunas %||% list(), function(cl) list(nome = cl$nome, classe = cl$classe))))
  }
  out
}

#' Rótulo de cada nó para o diagrama: o do card, ou o do bloco.
#' @noRd
.td_rotulos <- function(doc, registry) {
  lapply(doc$nodes, function(n) n$label %||% registry$nodes[[n$type]]$label %||% n$type)
}

# ---- RO-Crate ----------------------------------------------------------------------

#' `encodingFormat` pela extensão (IANA).
#' @noRd
.td_formato_arquivo <- function(arq) {
  switch(tolower(tools::file_ext(arq)),
         csv = "text/csv", json = "application/json", png = "image/png", svg = "image/svg+xml",
         rds = "application/x-r-rds", md = "text/markdown", qmd = "text/markdown",
         cff = "application/x-yaml", yml = "application/x-yaml", yaml = "application/x-yaml",
         lua = "text/x-lua", js = "text/javascript", css = "text/css", ojs = "text/javascript",
         bib = "application/x-bibtex", csl = "application/xml", pdf = "application/pdf",
         jpg = "image/jpeg", jpeg = "image/jpeg",
         "application/octet-stream")
}

.td_pessoa <- function(a) {
  id <- if (!is.null(a$orcid)) paste0("https://orcid.org/", a$orcid) else paste0("#", gsub("\\s+", "-", tolower(a$nome)))
  list("@id" = id, "@type" = "Person", name = a$nome)
}

#' O `ro-crate-metadata.json` (RO-Crate 1.1): o descritor, o dataset raiz,
#' as pessoas, as licenças e cada arquivo da pasta. Arquivos de região com a
#' descrição que a parte deu (modelos com o vocabulário FAIR4ML,
#' `https://w3id.org/fair4ml#`).
#' @noRd
.td_rocrate <- function(m, versao, dir, data, pasta_doc, regioes, web = FALSE) {
  arquivos <- setdiff(.td_listar(dir), "ro-crate-metadata.json")
  # A extensão do Quarto é ferramenta, não dado: um Dataset só, com o nome.
  ext <- grepl("^_extensions/", arquivos)
  arquivos <- arquivos[!ext]
  pessoas <- lapply(m$autores, .td_pessoa)
  por_chave <- stats::setNames(regioes, vapply(regioes, `[[`, "", "chave"))
  entidade <- function(rel) {
    p <- file.path(dir, rel)
    e <- list("@id" = rel, "@type" = "File", name = basename(rel),
              encodingFormat = .td_formato_arquivo(rel),
              contentSize = as.character(file.info(p)$size),
              sha256 = .td_sha256(p))
    partes <- strsplit(rel, "/", fixed = TRUE)[[1]]
    if (length(partes) == 3L && partes[[1]] == .TD_PASTA) {
      chave <- partes[[2]]
      arq <- partes[[3]]
      man <- .td_manifesto(pasta_doc, chave)
      md <- man$metadados %||% list()
      e$name <- paste0(man$titulo %||% chave, " \u{2014} ", arq)
      if (identical(arq, "trama.json")) {
        e$description <- "Manifesto do trama: de que n\u{F3} veio a regi\u{E3}o e o hash do que foi materializado."
        return(e)
      }
      e$description <- md$descricao %||% NULL
      # Modelo: o arquivo do modelo reduzido é o `fair4ml:MLModel`; o motor e
      # o web.json são partes dele.
      if (identical(md$tipo_schema, "fair4ml:MLModel") && identical(arq, "modelo.rds")) {
        e[["@type"]] <- list("File", "fair4ml:MLModel")
        e <- c(e, Filter(Negate(is.null), md$propriedades %||% list()))
      } else if (!is.null(md$tipo_schema) && !md$tipo_schema %in% c("fair4ml:MLModel", "Dataset") &&
                 grepl("\\.(png|svg)$", arq)) {
        e[["@type"]] <- list("File", md$tipo_schema)
      }
      # A marca: o arquivo leva linhas do treino (só chega aqui com o aceite).
      if (!is.null(man$dados_treino) && arq %in% c("modelo.rds", "motor.rds")) {
        e[["trama:contemDadosTreino"]] <- TRUE
        e[["trama:dadosTreinoEmbutidos"]] <- man$dados_treino
      }
      if (identical(md$tipo_schema, "Dataset") && identical(arq, "dados.csv") &&
          !is.null(md$propriedades$variableMeasured)) {
        e$variableMeasured <- md$propriedades$variableMeasured
      }
    }
    e
  }
  especiais <- list(
    "index.qmd" = "O documento: texto dos autores e os resultados do trama (Quarto).",
    "flow.json" = "O fluxo do trama que gerou os resultados; abre no editor.",
    "fluxo.svg" = "Diagrama do fluxo.",
    "dados-entrada.json" = "Descri\u{E7}\u{E3}o e sha256 dos dados de entrada n\u{E3}o inclu\u{ED}dos.",
    "ambiente.json" = "Vers\u{F5}es de R, do trama, das cole\u{E7}\u{F5}es e dos pacotes.",
    "_variables.yml" = "Vers\u{E3}o, licen\u{E7}as e cita\u{E7}\u{E3}o que o documento mostra.",
    "README.md" = "Como citar e reproduzir.", "CITATION.cff" = "Cita\u{E7}\u{E3}o.", "LICENSE" = "Licen\u{E7}as.")
  arquivos_ent <- lapply(arquivos, function(rel) {
    e <- entidade(rel)
    if (!is.null(especiais[[rel]])) e$description <- especiais[[rel]]
    if (rel == "flow.json") e$conformsTo <- list("@id" = .TD_ESQUEMA_FLUXO)
    e
  })
  tem_ext <- any(ext)
  partes <- c(lapply(arquivos, function(a) list("@id" = a)), if (tem_ext) list(list("@id" = "_extensions/")))
  raiz <- Filter(Negate(is.null), list(
    "@id" = "./", "@type" = "Dataset", name = m$titulo,
    description = if (nzchar(m$descricao)) m$descricao else m$titulo,
    datePublished = format(data, "%Y-%m-%d"), version = versao,
    license = list("@id" = .td_licenca_url(m$licenca)),
    keywords = if (length(m$palavras_chave)) as.list(m$palavras_chave) else NULL,
    author = lapply(pessoas, function(p) list("@id" = p[["@id"]])),
    hasPart = partes))
  nome_ext <- paste0("Modelo do documento (extens\u{E3}o Quarto)",
                     if (web) paste0(", com o quarto-live ", .TD_QUARTO_LIVE, " (r-wasm/quarto-live, MIT)") else "")
  grafo <- c(
    list(list("@id" = "ro-crate-metadata.json", "@type" = "CreativeWork",
              conformsTo = list("@id" = "https://w3id.org/ro/crate/1.1"), about = list("@id" = "./"))),
    list(raiz),
    pessoas,
    list(list("@id" = .td_licenca_url(m$licenca), "@type" = "CreativeWork", name = m$licenca),
         list("@id" = .td_licenca_url(m$licenca_codigo), "@type" = "CreativeWork", name = m$licenca_codigo)),
    arquivos_ent,
    if ("flow.json" %in% arquivos) list(list("@id" = .TD_ESQUEMA_FLUXO, "@type" = "CreativeWork",
                                             name = "Documento do trama, formato 1 (JSON Schema)")),
    if (tem_ext) list(Filter(Negate(is.null), list(
      "@id" = "_extensions/", "@type" = "Dataset", name = nome_ext,
      url = if (web) "https://github.com/r-wasm/quarto-live"))))
  # Licenças iguais (conteúdo e código) não repetem a entidade.
  grafo <- grafo[!duplicated(vapply(grafo, function(g) g[["@id"]], ""))]
  list("@context" = list("https://w3id.org/ro/crate/1.1/context",
                         list(fair4ml = "https://w3id.org/fair4ml#", trama = .TD_VOCABULARIO)),
       "@graph" = grafo)
}

# ---- textos do pacote --------------------------------------------------------------

.td_autores_txt <- function(autores) {
  paste(vapply(autores, function(a) if (is.null(a$orcid)) a$nome else sprintf("%s (https://orcid.org/%s)", a$nome, a$orcid), ""),
        collapse = "; ")
}

.td_readme <- function(m, versao, repositorio, data, regioes, dados_entrada) {
  dados <- if (length(dados_entrada)) {
    paste(vapply(dados_entrada, function(d) {
      sprintf("- `%s` (%s): %s linhas, %s colunas; sha256 `%s`", d$arquivo %||% d$no, d$rotulo %||% "",
              d$linhas %||% "?", length(d$colunas %||% list()), d$sha256 %||% "\u{2014}")
    }, ""), collapse = "\n")
  } else "Nenhum: tudo o que o fluxo l\u{EA} est\u{E1} no pacote."
  itens <- paste(vapply(regioes, function(r) {
    sprintf("  - `_trama/%s/` \u{2014} %s (%s): %s%s", r$chave, r$titulo, r$tipo %||% "?",
            paste(setdiff(unlist(r$arquivos), "trama.json"), collapse = ", "),
            if (is.null(r$dados_treino)) "" else paste0(". **Cont\u{E9}m dados de treino:** ", r$dados_treino))
  }, ""), collapse = "\n")
  .td_preencher_texto(.td_texto_publicar("readme.md"), list(
    titulo = m$titulo, descricao = m$descricao, autores = .td_autores_txt(m$autores),
    versao = versao, licenca = m$licenca, licenca_codigo = m$licenca_codigo,
    citacao = .td_citacao(m, versao, repositorio, data), itens = if (nzchar(itens)) itens else "  - nenhum",
    dados = dados, fluxo = "`flow.json`"))
}

.td_cff <- function(m, versao, repositorio, data) {
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
    sprintf("version: %s", q(versao)),
    sprintf("date-released: %s", q(format(data, "%Y-%m-%d"))),
    sprintf("license: %s", q(m$licenca)),
    if (length(m$palavras_chave)) c("keywords:", sprintf("  - %s", q(m$palavras_chave))),
    if (grepl("^https?://", repositorio)) sprintf("repository: %s", q(repositorio))
  ), collapse = "\n")
}

.td_licenca <- function(m, data) {
  titulares <- paste(vapply(m$autores, function(a) a$nome, ""), collapse = ", ")
  .td_preencher_texto(.td_texto_publicar("licenca.txt"), list(
    licenca = m$licenca, licenca_url = .td_licenca_url(m$licenca),
    licenca_codigo = m$licenca_codigo, licenca_codigo_url = .td_licenca_url(m$licenca_codigo),
    ano = format(data, "%Y"), titulares = titulares,
    texto_codigo = if (m$licenca_codigo == "MIT") {
      .td_preencher_texto(.td_texto_publicar("mit.txt"), list(ano = format(data, "%Y"), titulares = titulares))
    } else
      sprintf("O texto integral est\u{E1} em %s.", .td_licenca_url(m$licenca_codigo))))
}

#' Texto de `inst/publicar/` (UTF-8): os textos longos em português ficam
#' fora do código R.
#' @noRd
.td_texto_publicar <- function(nome) {
  p <- system.file("publicar", nome, package = "trama.documentos")
  paste(readLines(p, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

#' `{{chave}}` -> valor.
#' @noRd
.td_preencher_texto <- function(modelo, valores) {
  for (k in names(valores)) modelo <- gsub(paste0("{{", k, "}}"), valores[[k]] %||% "", modelo, fixed = TRUE)
  modelo
}

# ---- diagrama do fluxo -------------------------------------------------------------

.td_xml <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE); gsub("\"", "&quot;", x, fixed = TRUE)
}

#' O grafo desenhado, estático: caixas nas posições do editor, fios entre
#' elas. SVG puro, sem fonte externa.
#' @noRd
.td_fluxo_svg <- function(doc, rotulos = list()) {
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
            x, y, larg, alt, x + 10, y + 19, .td_xml(substr(rot(id), 1, 28)),
            x + 10, y + 35, .td_xml(doc$nodes[[id]]$type))
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
