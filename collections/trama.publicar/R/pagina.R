# A página: o `.qmd` gerado do layout, no formato artigo (uma coluna).
#
# Quarto + quarto-live (WebR): o Quarto dá tema, matemática e o carregamento
# do WebR; a extensão vai FIXADA no pacote (`_extensions/`, versão
# `.TR_PUBLICAR_QUARTO_LIVE`), nunca a mais recente. Sem preditor, a página
# sai em `html` comum: nada de R no navegador quando ninguém vai prever.

#' Gera o `index.qmd` da página
#'
#' @param material O valor do nó `publicar`.
#' @param tem_fluxo O pacote tem `fluxo.svg` (o bloco `fluxo` do layout).
#' @param data Data da publicação.
#' @return Texto do `.qmd`.
#' @export
tr_publicar_pagina <- function(material, tem_fluxo = TRUE, data = Sys.Date()) {
  m <- material$metadados
  blocos <- material$layout
  preditores <- Filter(function(b) identical(b$tipo, "item") &&
                         identical(material$itens[[b$id]]$componente$tipo, "preditor"), blocos)
  live <- length(preditores) > 0L
  pacotes_web <- unique(unlist(lapply(preditores, function(b) {
    names(material$itens[[b$id]]$componente$web$pacotes)
  })))
  # `stats` e `MASS` não se instalam: vêm na imagem base do WebR.
  instalar <- setdiff(pacotes_web, c("stats", "base", "utils", "MASS"))
  corpo <- vapply(blocos, function(b) switch(b$tipo,
    cabecalho = .tr_publicar_cabecalho(material, data, live),
    texto = paste(b$texto %||% "", collapse = "\n"),
    fluxo = if (tem_fluxo) "## O fluxo\n\n![O fluxo do trama que gerou este material.](fluxo.svg){fig-alt=\"Diagrama do fluxo\"}" else "",
    item = .tr_publicar_componente(b, material$itens[[b$id]])), "")
  if (live) corpo <- c(corpo, .tr_publicar_versoes_bloco(material, preditores))
  paste(c(.tr_publicar_yaml(m, data, live, instalar),
          if (live) "{{< include ./_extensions/r-wasm/live/_knitr.qmd >}}",
          "", corpo[nzchar(corpo)]), collapse = "\n\n")
}

.tr_publicar_yaml <- function(m, data, live, instalar) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  autores <- vapply(m$autores, function(a) {
    paste(c(sprintf("  - name: %s", q(a$nome)), if (!is.null(a$orcid)) sprintf("    orcid: %s", q(a$orcid))),
          collapse = "\n")
  }, "")
  paste(c("---",
          sprintf("title: %s", q(m$titulo)),
          if (length(autores)) c("author:", autores),
          sprintf("date: %s", q(format(data, "%Y-%m-%d"))),
          if (nzchar(m$descricao)) sprintf("description: %s", q(m$descricao)),
          sprintf("lang: pt"),
          if (live) c("engine: knitr", "format:", "  live-html:", "    toc: false",
                      "    embed-resources: false") else
            c("format:", "  html:", "    toc: false"),
          if (live && length(instalar)) c("webr:", "  packages:", sprintf("    - %s", q(instalar))),
          "resources:", "  - itens/", "  - fluxo.svg", "  - ro-crate-metadata.json",
          "---"), collapse = "\n")
}

.tr_publicar_cabecalho <- function(material, data, live) {
  m <- material$metadados
  # A descrição já sai sob o título (o `description` do YAML).
  linhas <- c(
    sprintf("**Vers\u{E3}o** %s \u{B7} **Licen\u{E7}a** [%s](%s) (conte\u{FA}do), [%s](%s) (c\u{F3}digo) \u{B7} [pacote completo](ro-crate-metadata.json)",
            m$versao, m$licenca, .tr_publicar_licenca_url(m$licenca), m$licenca_codigo,
            .tr_publicar_licenca_url(m$licenca_codigo)),
    if (length(m$palavras_chave)) sprintf("**Palavras-chave:** %s", paste(m$palavras_chave, collapse = ", ")),
    if (live) paste0("::: {.callout-note}\n## Carregando o R no seu navegador\n\n",
                     "Os preditores desta p\u{E1}gina rodam em R dentro do navegador (WebR). A primeira ",
                     "visita baixa o R e os pacotes e leva cerca de um minuto; as seguintes usam o cache. ",
                     "Nenhum dado digitado sai do seu computador.\n:::"),
    paste0("::: {.callout-tip collapse=\"true\"}\n## Como citar\n\n", .tr_publicar_citacao(material, data), "\n:::"),
    paste0("::: {.callout-tip collapse=\"true\"}\n## Como reproduzir\n\n",
           "Baixe o pacote, instale o trama e as cole\u{E7}\u{F5}es de `ambiente.json`, traga os dados ",
           "de `dados-entrada.json` (mesmo sha256) e abra `flow.json` no editor. O `README.md` traz o passo a passo.\n:::"))
  paste(linhas, collapse = "\n\n")
}

#' Um item na página, pelo componente que o adaptador declarou.
#' @noRd
.tr_publicar_componente <- function(b, it) {
  comp <- it$componente
  pasta <- file.path("itens", .tr_publicar_pasta(b$id))
  titulo <- b$titulo %||% it$titulo
  legenda <- b$legenda %||% ""
  cab <- if (nzchar(titulo)) paste0("## ", titulo) else ""
  corpo <- switch(comp$tipo,
    markdown = comp$texto,
    figura = sprintf("![%s](%s){fig-alt=\"%s\"}\n\n[Baixar a figura](%s)", legenda,
                     file.path(pasta, comp$arquivo), .tr_publicar_xml(if (nzchar(legenda)) legenda else titulo),
                     file.path(pasta, comp$arquivo)),
    tabela = paste(c(
      .tr_publicar_tabela_md(comp$previa),
      if (nzchar(legenda)) paste0(": ", legenda),
      sprintf("%s linhas e %s colunas%s. [Baixar a tabela completa (CSV)](%s)", comp$linhas, comp$colunas,
              if (comp$linhas > nrow(comp$previa)) sprintf("; acima, as %d primeiras", nrow(comp$previa)) else "",
              file.path(pasta, comp$arquivo))), collapse = "\n\n"),
    preditor = paste(c(if (nzchar(legenda)) legenda, .tr_publicar_preditor(b$id, pasta, comp$web)),
                     collapse = "\n\n"))
  paste(c(cab, corpo), collapse = "\n\n")
}

#' Tabela markdown (pipe) das primeiras linhas; números com 4 significativos.
#' @noRd
.tr_publicar_tabela_md <- function(x) {
  x <- as.data.frame(x)
  if (!ncol(x)) return("")
  cel <- function(v) {
    if (is.numeric(v)) out <- ifelse(is.na(v), "", format(signif(v, 4), trim = TRUE, scientific = FALSE, drop0trailing = TRUE))
    else out <- ifelse(is.na(v), "", as.character(v))
    gsub("|", "\\|", out, fixed = TRUE)
  }
  m <- vapply(x, cel, character(nrow(x)))
  if (!is.matrix(m)) m <- matrix(m, nrow = nrow(x))
  alinha <- vapply(x, function(v) if (is.numeric(v)) "---:" else ":---", "")
  linhas <- c(paste0("| ", paste(gsub("|", "\\|", names(x), fixed = TRUE), collapse = " | "), " |"),
              paste0("|", paste(alinha, collapse = "|"), "|"),
              if (nrow(x)) apply(m, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
  paste(linhas, collapse = "\n")
}

#' Nome de variável OJS (e de R, que o quarto-live repassa): letra, dígito, `_`.
#' @noRd
.tr_publicar_ojs <- function(id, col) {
  paste0("v_", gsub("[^A-Za-z0-9]", "_", id), "_", gsub("[^A-Za-z0-9]", "_", col))
}

.tr_publicar_js <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA))

#' O preditor: um controle OJS por entrada e uma célula WebR que lê o motor
#' da mesma origem, chama `prever` e mostra o resultado.
#' @noRd
.tr_publicar_preditor <- function(id, pasta, web) {
  vars <- vapply(web$entradas, function(e) .tr_publicar_ojs(id, e$nome), "")
  controles <- vapply(web$entradas, function(e) {
    v <- .tr_publicar_ojs(id, e$nome)
    switch(e$tipo,
      numero = sprintf("viewof %s = Inputs.range([%s, %s], {value: %s, step: %s, label: %s})", v,
                       .tr_publicar_js(e$min), .tr_publicar_js(e$max), .tr_publicar_js(e$padrao),
                       .tr_publicar_js(e$passo), .tr_publicar_js(e$nome)),
      fator = sprintf("viewof %s = Inputs.select(%s, {value: %s, label: %s})", v,
                      .tr_publicar_js(as.list(e$niveis)), .tr_publicar_js(e$padrao), .tr_publicar_js(e$nome)),
      logico = sprintf("viewof %s = Inputs.toggle({value: false, label: %s})", v, .tr_publicar_js(e$nome)))
  }, "")
  local <- paste0(.tr_publicar_pasta(id), "-motor.rds")
  # Endereço ABSOLUTO, montado no navegador: o `download.file()` do WebR roda
  # num worker, onde um caminho relativo não tem base e dá "Invalid URL".
  var_url <- .tr_publicar_ojs(id, "motor_url")
  url_js <- sprintf("%s = new URL(%s, document.baseURI).href", var_url,
                    .tr_publicar_js(file.path(pasta, "motor.rds")))
  novos <- paste(sprintf("`%s` = %s", vapply(web$entradas, function(e) e$nome, ""), vars), collapse = ", ")
  resposta <- web$saida$resposta %||% "resposta"
  mostrar <- if (identical(web$saida$tarefa, "classificacao")) {
    c(sprintf("cat(sprintf(\"Classe prevista para %s: %%s\\n\\n\", r$previsto[[1]]))", resposta),
      "p <- unlist(r[1, -1, drop = TRUE])",
      "print(data.frame(classe = sub(\"^prob_\", \"\", names(p)), probabilidade = round(p, 3)), row.names = FALSE)")
  } else {
    sprintf("cat(sprintf(\"%s previsto: %%s\\n\", format(r$previsto[[1]], digits = 6)))", resposta)
  }
  celula <- c("```{webr}", "#| edit: false", "#| echo: false", "#| input:", sprintf("#|   - %s", c(vars, var_url)),
              sprintf("if (!file.exists(\"%s\")) download.file(%s, \"%s\")", local, var_url, local),
              sprintf("motor <- readRDS(\"%s\")", local),
              paste0("prever <- ", web$prever),
              sprintf("novos <- data.frame(%s, check.names = FALSE)", novos),
              "r <- prever(motor, novos)",
              mostrar, "```")
  paste(c("```{ojs}", "//| echo: false", controles, url_js, "```", "", celula, "",
          sprintf("*Pr\u{E9}-processamento:* %s", web$preprocessamento %||% "")), collapse = "\n")
}

#' Versões do treino × as que o navegador carregou.
#' @noRd
.tr_publicar_versoes_bloco <- function(material, preditores) {
  pac <- list()
  for (b in preditores) pac <- utils::modifyList(pac, material$itens[[b$id]]$componente$web$pacotes)
  pac <- pac[setdiff(names(pac), "stats")]
  treino <- c(R = material$ambiente$r, unlist(pac))
  nomes <- names(treino)
  paste(c("## Vers\u{F5}es", "",
          "O modelo foi ajustado com as vers\u{F5}es da coluna *treino*; o navegador carregou as da coluna *navegador*.",
          "", "```{webr}", "#| edit: false", "#| echo: false",
          sprintf("treino <- c(%s)", paste(sprintf("`%s` = \"%s\"", nomes, treino), collapse = ", ")),
          "nav <- vapply(names(treino), function(p) if (p == \"R\") paste(R.version$major, R.version$minor, sep = \".\") else tryCatch(as.character(packageVersion(p)), error = function(e) \"ausente\"), \"\")",
          "print(data.frame(pacote = names(treino), treino = unname(treino), navegador = unname(nav)), row.names = FALSE)",
          "```"), collapse = "\n")
}
