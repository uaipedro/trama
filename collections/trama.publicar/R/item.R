# O item de publicação (`publicar/item`) e os adaptadores que o produzem.
#
# Toda coisa que entra no pacote chega pela porta `itens` do `publicar` como
# um `publicar/item`, convertida por um adaptador registrado `<tipo> ->
# publicar/item` (o motor o põe na aresta). É assim que a publicação serve a
# coleções que ela não conhece: uma coleção nova registra o próprio
# adaptador, com `tr_publicar_item()`, e o item entra no pacote e na página.

#' Item de publicação: o contrato dos adaptadores `<tipo> -> publicar/item`
#'
#' O que um adaptador devolve. O `publicar` grava `arquivos` em
#' `itens/<id>/` (o `id` é o nó de origem, dado pela porta), descreve o item
#' no `ro-crate-metadata.json` com `metadados` e, se houver `componente`,
#' mostra-o na página.
#'
#' @param tipo Texto curto do que é o item (`"tabela"`, `"figura"`,
#'   `"modelo"`, ou o que a coleção quiser).
#' @param titulo Título do item (o rótulo na página e no pacote).
#' @param arquivos Lista nomeada: nome do arquivo (relativo à pasta do item,
#'   sem `/` nem `..`) -> conteúdo em `raw` ou texto.
#' @param metadados Lista com o que descreve o item no RO-Crate: `tipo_schema`
#'   (o `@type`, padrão `"Dataset"`), `descricao` e `propriedades` (lista de
#'   pares JSON-LD extras, como `"fair4ml:mlTask"`). Cada arquivo ganha também
#'   `encodingFormat`, tamanho e sha256 sozinho.
#' @param componente `NULL` (só vai para o pacote) ou lista com `tipo`, um de
#'   `"tabela"` (`arquivo` CSV e `previa`, um data.frame curto), `"figura"`
#'   (`arquivo` de imagem), `"preditor"` (`web`, o bloco de
#'   `trama.models::tr_models_web()`) ou `"markdown"` (`texto`).
#' @param avisos Textos que vão para a conferência (não bloqueiam).
#' @param erro Texto: o item não entra (e, na fase de envio, bloqueia o botão).
#' @return Objeto de classe `tr_publicar_item`.
#' @export
tr_publicar_item <- function(tipo, titulo = "", arquivos = list(), metadados = list(),
                             componente = NULL, avisos = character(), erro = NULL) {
  ruim <- function(msg) rlang::abort(msg, class = "tr_publicar_error_bad_item")
  if (!is.character(tipo) || length(tipo) != 1L || !nzchar(tipo)) ruim("Item sem 'tipo'.")
  if (length(arquivos) && (is.null(names(arquivos)) || !all(nzchar(names(arquivos))))) {
    ruim("'arquivos' do item tem de ser lista nomeada pelo nome do arquivo.")
  }
  if (any(grepl("(^|/)\\.\\.(/|$)|^/|\\\\", names(arquivos)))) ruim("Nome de arquivo do item com caminho fora da pasta dele.")
  arquivos <- lapply(arquivos, function(a) if (is.raw(a)) a else charToRaw(enc2utf8(paste(a, collapse = "\n"))))
  if (!is.null(componente)) {
    if (!is.list(componente) || !componente$tipo %in% c("tabela", "figura", "preditor", "markdown")) {
      ruim("'componente' do item tem 'tipo' fora de tabela, figura, preditor e markdown.")
    }
  }
  structure(list(tipo = tipo, titulo = as.character(titulo), arquivos = arquivos,
                 metadados = metadados, componente = componente,
                 avisos = as.character(avisos), erro = erro),
            class = "tr_publicar_item")
}

#' O item que não entrou: o erro vira valor, e a conferência mostra.
#' @noRd
.tr_publicar_item_erro <- function(tipo, titulo, e) {
  tr_publicar_item(tipo, titulo, erro = conditionMessage(e))
}

# ---- adaptadores -------------------------------------------------------------------

.TR_PUBLICAR_PREVIA_LINHAS <- 20L

#' Texto CSV (UTF-8, ponto decimal) de uma tabela.
#' @noRd
.tr_publicar_csv <- function(x) {
  con <- rawConnection(raw(0), "wb")
  on.exit(close(con))
  utils::write.csv(as.data.frame(x), con, row.names = FALSE, fileEncoding = "UTF-8")
  rawConnectionValue(con)
}

#' `data/table` -> item: o CSV inteiro e as primeiras linhas para a página.
#' @noRd
.tr_publicar_de_tabela <- function(x) {
  x <- as.data.frame(x)
  colunas <- lapply(names(x), function(nm) list(nome = nm, classe = class(x[[nm]])[[1]]))
  tr_publicar_item(
    "tabela", titulo = "Tabela",
    arquivos = list(dados.csv = .tr_publicar_csv(x)),
    metadados = list(tipo_schema = "Dataset",
                     descricao = sprintf("Tabela com %d linhas e %d colunas.", nrow(x), ncol(x)),
                     propriedades = list(variableMeasured = lapply(colunas, function(cl) {
                       list("@type" = "PropertyValue", name = cl$nome, description = cl$classe)
                     }))),
    componente = list(tipo = "tabela", arquivo = "dados.csv",
                      previa = utils::head(x, .TR_PUBLICAR_PREVIA_LINHAS),
                      linhas = nrow(x), colunas = ncol(x)))
}

#' `view/plot` -> item: o PNG em qualidade de publicação. O objeto ggplot NÃO
#' vai: ele carrega a tabela inteira (`$data`).
#' @noRd
.tr_publicar_de_grafico <- function(x) {
  dir <- tempfile("publicar-fig-"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  ctx <- list(file = function(ext) file.path(dir, paste0("figura.", ext)),
              qualidade = list(formato = "png", dpi = 200))
  art <- trama.view::tr_view_render(x, ctx)
  arq <- art$files[[1]]
  rotulo <- x$labels$title %||% "Figura"
  tr_publicar_item(
    "figura", titulo = if (is.character(rotulo) && nzchar(rotulo)) rotulo else "Figura",
    arquivos = list(figura.png = readBin(arq, "raw", file.info(arq)$size)),
    metadados = list(tipo_schema = "ImageObject", descricao = "Figura gerada pelo fluxo."),
    componente = list(tipo = "figura", arquivo = "figura.png"))
}

#' `models/fit` -> item: o modelo reduzido (`tr_models_strip()`), o motor e
#' o bloco web. Classe sem `tr_models_strip()` vira item com erro: o card
#' mostra, e o modelo não entra — nunca o objeto inteiro com os dados.
#' @noRd
.tr_publicar_de_modelo <- function(x) {
  info <- tryCatch(trama.models::tr_models_info(x), error = function(e) NULL)
  titulo <- info$rotulo %||% "Modelo"
  tryCatch({
    reduzido <- trama.models::tr_models_strip(x)
    web <- trama.models::tr_models_web(x)
    arquivos <- list(modelo.rds = .tr_publicar_rds(trama.models::tr_models_serialize(reduzido)))
    avisos <- .tr_publicar_avisos_modelo(x)
    componente <- NULL
    if (is.null(web)) {
      avisos <- c(avisos, "Sem bloco web: o modelo vai no pacote, mas a p\u{E1}gina n\u{E3}o tem preditor.")
    } else {
      arquivos$motor.rds <- .tr_publicar_rds(web$motor)
      arquivos$web.json <- .tr_publicar_json(web[setdiff(names(web), "motor")])
      if (length(arquivos$motor.rds) > 20 * 1024^2) {
        avisos <- c(avisos, sprintf("O motor tem %.0f MB: o navegador baixa isso para prever.",
                                    length(arquivos$motor.rds) / 1024^2))
      }
      componente <- list(tipo = "preditor", web = web[setdiff(names(web), "motor")])
    }
    tarefa <- info$tarefa %||% "regressao"
    tr_publicar_item(
      "modelo", titulo = titulo, arquivos = arquivos,
      metadados = list(
        tipo_schema = "fair4ml:MLModel",
        descricao = sprintf("%s ajustado a %s linhas; prev\u{EA} '%s' a partir de %s.", titulo,
                            info$n %||% "?", info$resposta %||% "?",
                            paste(info$preditores %||% character(), collapse = ", ")),
        propriedades = list(
          "fair4ml:mlTask" = if (tarefa == "classificacao") "Classification" else "Regression",
          "fair4ml:modelCategory" = titulo,
          "fair4ml:usageInstructions" = paste(
            "Em R: readRDS('modelo.rds') com trama.models carregado e trama.models::tr_models_predict(modelo, novos).",
            "Sem o trama: readRDS('motor.rds') e a fun\u{E7}\u{E3}o 'prever' de web.json."),
          "fair4ml:modelRisksBiasLimitations" = if (length(avisos)) paste(avisos, collapse = " ") else NULL)),
      componente = componente, avisos = avisos)
  }, error = function(e) .tr_publicar_item_erro("modelo", titulo, e))
}

#' O que o modelo reduzido ainda leva do treino, por motor (medido em
#' `test-reduzir.R` da `trama.ml`).
#' @noRd
.tr_publicar_avisos_modelo <- function(x) {
  motor <- x$modelo %||% ""
  if (identical(motor, "svm")) {
    n <- tryCatch(x$ajuste$tot.nSV, error = function(e) NA)
    return(sprintf(paste0("A SVM guarda os vetores de suporte, que s\u{E3}o %s linhas do treino ",
                          "(escalonadas): publicar o modelo publica essas linhas."), n))
  }
  if (identical(motor, "forest") && identical(x$tarefa, "regressao")) {
    return(paste0("Na floresta de regress\u{E3}o, folha com uma s\u{F3} linha sorteada guarda a resposta ",
                  "dessa linha do treino."))
  }
  character()
}

.tr_publicar_rds <- function(x) {
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f))
  # `version = 3` (o padrão do R >= 3.6), explícito: o WebR lê, e o arquivo
  # não muda de formato se o padrão mudar.
  saveRDS(x, f, version = 3L)
  readBin(f, "raw", file.info(f)$size)
}

.tr_publicar_json <- function(x) {
  charToRaw(enc2utf8(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE,
                                                   null = "null", na = "null", digits = NA))))
}

`%||%` <- function(x, y) if (is.null(x)) y else x
