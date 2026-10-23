# O destino Página web: um modelo vira preditor que roda no navegador.
#
# Portado do `trama.publicar` (`R/pagina.R`). A página é um documento do
# modelo Página web, que traz a extensão quarto-live FIXADA (ver
# `inst/modelos/pagina/ORIGEM.md`); aqui só mora o que a região de um modelo
# põe nela: um controle OJS por entrada e uma célula WebR que lê o motor da
# mesma origem (`_trama/<chave>/motor.rds`, servido junto com a página),
# chama `prever` e mostra o resultado, e as versões do treino × as que o
# navegador carregou.

#' Nome de variável OJS (e de R, que o quarto-live repassa): letra, dígito, `_`.
#' @noRd
.td_ojs <- function(chave, col) {
  paste0("v_", gsub("[^A-Za-z0-9]", "_", chave), "_", gsub("[^A-Za-z0-9]", "_", col))
}

.td_js <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA))

#' Pacotes que o motor pede e que a imagem base do WebR não traz.
#' @noRd
.td_pacotes_web <- function(web) setdiff(names(web$pacotes), c("stats", "base", "utils", "MASS"))

#' O `preditor.qmd` da região de um modelo, incluído pelo corpo dela.
#'
#' @param chave Chave da região (nomes de variável e do arquivo no navegador).
#' @param motor Caminho do `motor.rds` relativo ao documento.
#' @param web O bloco de `trama.models::tr_models_web()` sem o motor.
#' @param r Versão do R do treino.
#' @noRd
.td_preditor <- function(chave, motor, web, r = paste(R.version$major, R.version$minor, sep = ".")) {
  vars <- vapply(web$entradas, function(e) .td_ojs(chave, e$nome), "")
  controles <- vapply(web$entradas, function(e) {
    v <- .td_ojs(chave, e$nome)
    switch(e$tipo,
      numero = sprintf("viewof %s = Inputs.range([%s, %s], {value: %s, step: %s, label: %s})", v,
                       .td_js(e$min), .td_js(e$max), .td_js(e$padrao), .td_js(e$passo), .td_js(e$nome)),
      fator = sprintf("viewof %s = Inputs.select(%s, {value: %s, label: %s})", v,
                      .td_js(as.list(e$niveis)), .td_js(e$padrao), .td_js(e$nome)),
      logico = sprintf("viewof %s = Inputs.toggle({value: false, label: %s})", v, .td_js(e$nome)))
  }, "")
  local <- paste0(gsub("[^A-Za-z0-9_.-]", "_", chave), "-motor.rds")
  # Endereço ABSOLUTO, montado no navegador: o `download.file()` do WebR roda
  # num worker, onde um caminho relativo não tem base e dá "Invalid URL".
  var_url <- .td_ojs(chave, "motor_url")
  url_js <- sprintf("%s = new URL(%s, document.baseURI).href", var_url, .td_js(motor))
  novos <- paste(sprintf("`%s` = %s", vapply(web$entradas, function(e) e$nome, ""), vars), collapse = ", ")
  resposta <- web$saida$resposta %||% "resposta"
  mostrar <- if (identical(web$saida$tarefa, "classificacao")) {
    c(sprintf("cat(sprintf(\"Classe prevista para %s: %%s\\n\\n\", r$previsto[[1]]))", resposta),
      "p <- unlist(r[1, -1, drop = TRUE])",
      "print(data.frame(classe = sub(\"^prob_\", \"\", names(p)), probabilidade = round(p, 3)), row.names = FALSE)")
  } else {
    sprintf("cat(sprintf(\"%s previsto: %%s\\n\", format(r$previsto[[1]], digits = 6)))", resposta)
  }
  instalar <- .td_pacotes_web(web)
  celula <- c("```{webr}", "#| edit: false", "#| echo: false", "#| input:", sprintf("#|   - %s", c(vars, var_url)),
              if (length(instalar)) sprintf("for (p in %s) if (!requireNamespace(p, quietly = TRUE)) webr::install(p)",
                                            .td_r_vetor(instalar)),
              sprintf("if (!file.exists(\"%s\")) download.file(%s, \"%s\")", local, var_url, local),
              sprintf("motor <- readRDS(\"%s\")", local),
              paste0("prever <- ", web$prever),
              sprintf("novos <- data.frame(%s, check.names = FALSE)", novos),
              "r <- prever(motor, novos)",
              mostrar, "```")
  c("```{ojs}", "//| echo: false", controles, url_js, "```", "", celula, "",
    sprintf("*Pr\u{E9}-processamento:* %s", web$preprocessamento %||% ""), "",
    .td_versoes_bloco(web, r))
}

#' Versões do treino × as que o navegador carregou, recolhidas num callout.
#' @noRd
.td_versoes_bloco <- function(web, r) {
  pac <- web$pacotes[setdiff(names(web$pacotes), "stats")]
  treino <- c(R = r, unlist(pac))
  c("::: {.callout-note collapse=\"true\"}", "## Vers\u{F5}es do treino e do navegador", "",
    "O modelo foi ajustado com as vers\u{F5}es da coluna *treino*; o navegador carregou as da coluna *navegador*.",
    "", "```{webr}", "#| edit: false", "#| echo: false",
    sprintf("treino <- c(%s)", paste(sprintf("`%s` = \"%s\"", names(treino), treino), collapse = ", ")),
    paste0("nav <- vapply(names(treino), function(p) if (p == \"R\") paste(R.version$major, R.version$minor, sep = \".\") ",
           "else tryCatch(as.character(packageVersion(p)), error = function(e) \"ausente\"), \"\")"),
    "print(data.frame(pacote = names(treino), treino = unname(treino), navegador = unname(nav)), row.names = FALSE)",
    "```", ":::")
}

.td_r_vetor <- function(x) paste0("c(", paste(sprintf("\"%s\"", x), collapse = ", "), ")")
