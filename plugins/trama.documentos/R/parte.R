# O que uma região mostra: a parte materializada de um resultado.
#
# Portado dos adaptadores do `trama.publicar` (`R/item.R`), que deixam de ser
# adaptadores de porta e viram um genérico S3 por classe do valor: o
# documento não é um nó, e uma coleção nova estende o genérico sem o plugin
# conhecê-la. A sanitização dos modelos (`tr_models_strip()`, recusa de
# classe sem ela, marca de dados de treino) é a mesma do publicar e vale para
# todo destino.

#' Parte de documento: o que um método de `tr_documento_componente()` devolve
#'
#' O motor grava `arquivos` em `_trama/<chave>/` ao lado do documento e põe
#' `corpo` dentro da região. O corpo referencia os arquivos pelo caminho
#' relativo ao documento (`ctx$arquivo("tabela.md")`), para que o render não
#' precise recalcular nada nem ter o store do projeto.
#'
#' @param tipo Texto curto do que é a parte (`"tabela"`, `"figura"`,
#'   `"modelo"`, `"teste"`, ou o que a coleção quiser).
#' @param titulo Título da parte (legenda de figura e tabela).
#' @param arquivos Lista nomeada: nome do arquivo (sem `/` nem `..`) ->
#'   conteúdo em `raw` ou texto.
#' @param corpo Linhas de Markdown que vão dentro da região.
#' @param metadados Lista que descreve a parte no pacote RO-Crate:
#'   `tipo_schema`, `descricao`, `propriedades` (pares JSON-LD).
#' @param dados_treino `NULL`, ou texto dizendo que linhas do treino a parte
#'   carrega (a marca de `trama.models::tr_models_strip()`). Parte marcada só
#'   entra no documento com aceite explícito.
#' @param avisos Textos para quem insere (não bloqueiam).
#' @param erro Texto: a parte não entra.
#' @return Objeto de classe `tr_documento_parte`.
#' @export
tr_documento_parte <- function(tipo, titulo = "", arquivos = list(), corpo = character(), metadados = list(),
                               dados_treino = NULL, avisos = character(), erro = NULL) {
  ruim <- function(msg) .td_erro(msg, "tr_documento_erro_parte")
  if (!is.character(tipo) || length(tipo) != 1L || !nzchar(tipo)) ruim("Parte sem 'tipo'.")
  if (length(arquivos) && (is.null(names(arquivos)) || !all(nzchar(names(arquivos))))) {
    ruim("'arquivos' da parte tem de ser lista nomeada pelo nome do arquivo.")
  }
  if (any(grepl("/|\\\\|^\\.\\.?$", names(arquivos)))) ruim("Nome de arquivo da parte com caminho: s\u{F3} o nome.")
  if ("trama.json" %in% names(arquivos)) ruim("'trama.json' \u{E9} o manifesto do motor; escolha outro nome.")
  # Texto termina em quebra de linha, como todo arquivo de texto.
  arquivos <- lapply(arquivos, function(a) if (is.raw(a)) a else charToRaw(enc2utf8(paste0(paste(a, collapse = "\n"), "\n"))))
  structure(list(tipo = tipo, titulo = as.character(titulo), arquivos = arquivos,
                 corpo = as.character(corpo), metadados = metadados, dados_treino = dados_treino,
                 avisos = as.character(avisos), erro = erro),
            class = "tr_documento_parte")
}

#' Materializa um resultado para o documento
#'
#' Genérico S3 pela classe do valor. Métodos embutidos: tabela
#' (`data.frame`), gráfico (`ggplot`), modelo (`tr_models_fit`, reduzido por
#' `trama.models::tr_models_strip()`) e teste (`tr_test`). Para os demais, o
#' método padrão usa o `report` do tipo da porta (`trama::tr_type(report =)`),
#' o mesmo que o relatório exportado usa; sem ele, a parte sai com erro.
#'
#' Uma coleção estende registrando um método para a classe dos seus valores
#' (`S3method(trama.documentos::tr_documento_componente, minha_classe)`, ou
#' `registerS3method()` no `.onLoad`, se o plugin for só sugerido).
#'
#' @param x O valor do nó.
#' @param ctx Lista com `no`, `porta`, `chave` (pasta em `_trama/`), `titulo`
#'   (o rótulo do card, ou o do bloco), `tipo` (o `tr_type` da porta, ou
#'   `NULL`), `opcoes` (atributos `trama-*` da região: `linhas`, `formato`,
#'   `largura`, `altura`), `web` (`TRUE` quando o documento roda R no
#'   navegador, como o modelo Página web: um modelo vira preditor) e
#'   `arquivo(nome)`, que dá o caminho de um arquivo da parte relativo ao
#'   documento.
#' @return Um [tr_documento_parte()].
#' @export
tr_documento_componente <- function(x, ctx) UseMethod("tr_documento_componente")

#' @export
tr_documento_componente.default <- function(x, ctx) {
  report <- ctx$tipo$report
  if (!is.function(report)) {
    return(tr_documento_parte("desconhecido", ctx$titulo, erro = sprintf(paste0(
      "O trama n\u{E3}o sabe mostrar um valor da classe '%s' num documento: a cole\u{E7}\u{E3}o pode declarar ",
      "'report' no tipo ou um m\u{E9}todo de tr_documento_componente()."), class(x)[[1]])))
  }
  r <- report(x)
  if (inherits(r, "ggplot")) return(tr_documento_componente(r, ctx))
  if (!is.character(r)) {
    return(tr_documento_parte("desconhecido", ctx$titulo, erro = sprintf(
      "O 'report' do tipo devolveu '%s', que n\u{E3}o \u{E9} texto nem gr\u{E1}fico.", class(r)[[1]])))
  }
  .td_parte_markdown("resultado", ctx, .td_linhas(r))
}

# ---- tabela --------------------------------------------------------------------------

.TD_LINHAS_PADRAO <- 20L

#' @export
tr_documento_componente.data.frame <- function(x, ctx) {
  x <- as.data.frame(x)
  max_linhas <- .td_opcao_int(ctx$opcoes$linhas, .TD_LINHAS_PADRAO)
  report <- ctx$tipo$report
  md <- if (is.function(report) && "max_linhas" %in% names(formals(report))) {
    .td_linhas(report(x, max_linhas = max_linhas))
  } else {
    c(.td_md_tabela(utils::head(x, max_linhas)),
      if (nrow(x) > max_linhas) c("", sprintf("*Primeiras %d de %d linhas.*", max_linhas, nrow(x))))
  }
  # Legenda ANTES da tabela (o Pandoc aceita dos dois lados): o rodapé do
  # report vem depois dela, e a legenda tem de encostar na tabela.
  tabela <- c(sprintf(": %s {#tbl-%s}", .td_legenda(ctx$titulo), .td_id_ref(ctx$chave)), "", .td_aparar(md))
  colunas <- lapply(names(x), function(nm) list(nome = nm, classe = class(x[[nm]])[[1]]))
  # O CSV leva só as linhas mostradas: inserir um nó de dados brutos não pode
  # pôr a tabela inteira (de um cliente, às vezes) ao lado do documento, que
  # se publica. A tabela inteira é escolha explícita: `trama-tabela="completa"`.
  completa <- identical(ctx$opcoes$tabela, "completa")
  tr_documento_parte(
    "tabela", titulo = ctx$titulo,
    arquivos = list(dados.csv = .td_csv(if (completa) x else utils::head(x, max_linhas)), tabela.md = tabela),
    corpo = sprintf("{{< include %s >}}", ctx$arquivo("tabela.md")),
    metadados = list(tipo_schema = "Dataset",
                     descricao = if (completa || nrow(x) <= max_linhas) {
                       sprintf("Tabela com %d linhas e %d colunas.", nrow(x), ncol(x))
                     } else sprintf("Primeiras %d de %d linhas (%d colunas).", max_linhas, nrow(x), ncol(x)),
                     propriedades = list(variableMeasured = lapply(colunas, function(cl) {
                       list("@type" = "PropertyValue", name = cl$nome, description = cl$classe)
                     }))))
}

#' Texto CSV (UTF-8, ponto decimal) de uma tabela.
#' @noRd
.td_csv <- function(x) {
  con <- rawConnection(raw(0), "wb")
  on.exit(close(con))
  utils::write.csv(as.data.frame(x), con, row.names = FALSE, fileEncoding = "UTF-8")
  rawConnectionValue(con)
}

#' Tabela pipe para quando o tipo não tem `report`: os números como no
#' relatório da `trama.data` (`tr_data_fmt_num()`: 4 significativos, vírgula
#' decimal, sem arredondar a parte inteira; #32).
#' @noRd
.td_md_tabela <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  cel <- lapply(df, function(v) {
    out <- if (is.numeric(v)) trama.data::tr_data_fmt_num(v) else ifelse(is.na(v), "", as.character(v))
    gsub("|", "\\|", out, fixed = TRUE)
  })
  alinha <- vapply(df, function(v) if (is.numeric(v)) "---:" else ":---", "")
  corpo <- if (nrow(df)) do.call(paste, c(unname(cel), sep = " | ")) else character()
  c(paste0("| ", paste(gsub("|", "\\|", names(df), fixed = TRUE), collapse = " | "), " |"),
    paste0("|", paste(alinha, collapse = "|"), "|"),
    if (length(corpo)) paste0("| ", corpo, " |"))
}

# ---- gráfico -------------------------------------------------------------------------

#' @export
tr_documento_componente.ggplot <- function(x, ctx) {
  formato <- ctx$opcoes$formato %||% "png"
  if (!formato %in% c("png", "svg")) {
    return(tr_documento_parte("figura", ctx$titulo, erro = sprintf(
      "Formato de figura '%s' desconhecido: use png ou svg.", formato)))
  }
  # Tamanho: o que o bloco de gráfico pediu (`tr_view_dim`), ou o do card;
  # a região pode trocar com `trama-largura`/`trama-altura` (polegadas).
  d <- attr(x, "tr_view_dim") %||% c(8, 4.5)
  larg <- .td_opcao_num(ctx$opcoes$largura, d[[1]])
  alt <- .td_opcao_num(ctx$opcoes$altura, d[[2]])
  dir <- tempfile("documento-fig-"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  arq <- file.path(dir, paste0("figura.", formato))
  # O objeto ggplot NÃO vai para o documento: ele carrega a tabela inteira
  # (`$data`). Vai a imagem, em qualidade de publicação.
  r <- tryCatch({
    if (formato == "png") {
      ggplot2::ggsave(arq, plot = x, width = larg, height = alt, units = "in", dpi = 200, device = ragg::agg_png)
    } else {
      ggplot2::ggsave(arq, plot = x, width = larg, height = alt, units = "in",
                      device = if (requireNamespace("svglite", quietly = TRUE)) svglite::svglite else grDevices::svg)
    }
    NULL
  }, error = function(e) e)
  if (inherits(r, "error")) return(tr_documento_parte("figura", ctx$titulo, erro = conditionMessage(r)))
  nome <- basename(arq)
  tr_documento_parte(
    "figura", titulo = ctx$titulo,
    arquivos = stats::setNames(list(readBin(arq, "raw", file.info(arq)$size)), nome),
    corpo = sprintf("![%s](%s){#fig-%s}", .td_legenda(ctx$titulo), ctx$arquivo(nome), .td_id_ref(ctx$chave)),
    metadados = list(tipo_schema = "ImageObject", descricao = "Figura gerada pelo fluxo."))
}

# ---- modelo --------------------------------------------------------------------------

#' `models/fit`: o modelo reduzido (`tr_models_strip()`), o motor e o bloco
#' web. Classe sem `tr_models_strip()` vira parte com erro: o modelo não
#' entra, nunca o objeto inteiro com os dados.
#' @noRd
#' @export
tr_documento_componente.tr_models_fit <- function(x, ctx) {
  if (!requireNamespace("trama.models", quietly = TRUE)) {
    return(tr_documento_parte("modelo", ctx$titulo, erro = "O pacote trama.models n\u{E3}o est\u{E1} instalado."))
  }
  info <- tryCatch(trama.models::tr_models_info(x), error = function(e) NULL)
  rotulo <- info$rotulo %||% "Modelo"
  tryCatch({
    reduzido <- trama.models::tr_models_strip(x)
    web <- trama.models::tr_models_web(x)
    arquivos <- list(modelo.rds = .td_rds(trama.models::tr_models_serialize(reduzido)))
    avisos <- character()
    if (is.null(web)) {
      avisos <- "Sem bloco web: o modelo vai para o documento, mas uma p\u{E1}gina web n\u{E3}o ter\u{E1} preditor."
    } else {
      arquivos$motor.rds <- .td_rds(web$motor)
      arquivos$web.json <- .td_json(web[setdiff(names(web), "motor")])
      if (length(arquivos$motor.rds) > 20 * 1024^2) {
        avisos <- c(avisos, sprintf("O motor tem %.0f MB: o navegador baixa isso para prever.",
                                    length(arquivos$motor.rds) / 1024^2))
      }
    }
    descricao <- sprintf("%s ajustado a %s linhas; prev\u{EA} '%s' a partir de %s.", rotulo,
                         info$n %||% "?", info$resposta %||% "?",
                         paste(info$preditores %||% character(), collapse = ", "))
    arquivos$modelo.md <- c(sprintf("**%s.** %s", .td_legenda(ctx$titulo), descricao),
                            if (!is.null(reduzido$dados_treino)) c("", sprintf("*%s*", reduzido$dados_treino)))
    tarefa <- info$tarefa %||% "regressao"
    corpo <- sprintf("{{< include %s >}}", ctx$arquivo("modelo.md"))
    # Página web (documento com o quarto-live): o modelo vira preditor que
    # roda no navegador, lendo o motor da pasta da região.
    if (isTRUE(ctx$web) && !is.null(web)) {
      arquivos$preditor.qmd <- .td_preditor(ctx$chave, ctx$arquivo("motor.rds"), web[setdiff(names(web), "motor")])
      corpo <- c(corpo, "", sprintf("{{< include %s >}}", ctx$arquivo("preditor.qmd")))
    }
    tr_documento_parte(
      "modelo", titulo = ctx$titulo, arquivos = arquivos,
      corpo = corpo,
      metadados = list(
        tipo_schema = "fair4ml:MLModel", descricao = descricao,
        propriedades = list(
          "fair4ml:mlTask" = if (tarefa == "classificacao") "Classification" else "Regression",
          "fair4ml:modelCategory" = rotulo,
          "fair4ml:usageInstructions" = paste(
            "Em R: readRDS('modelo.rds') com trama.models carregado e trama.models::tr_models_predict(modelo, novos).",
            "Sem o trama: readRDS('motor.rds') e a fun\u{E7}\u{E3}o 'prever' de web.json."),
          "fair4ml:modelRisksBiasLimitations" = reduzido$dados_treino)),
      dados_treino = reduzido$dados_treino, avisos = avisos)
  }, error = function(e) tr_documento_parte("modelo", ctx$titulo, erro = conditionMessage(e)))
}

.td_rds <- function(x) {
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f))
  # `version = 3` (o padrão do R >= 3.6), explícito: o WebR lê, e o arquivo
  # não muda de formato se o padrão mudar.
  saveRDS(x, f, version = 3L)
  readBin(f, "raw", file.info(f)$size)
}

.td_json <- function(x) {
  charToRaw(enc2utf8(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE,
                                                   null = "null", na = "null", digits = NA))))
}

# ---- teste ---------------------------------------------------------------------------

#' @export
tr_documento_componente.tr_test <- function(x, ctx) {
  report <- ctx$tipo$report
  md <- if (is.function(report)) .td_linhas(report(x)) else {
    t <- trama::tr_test_table(x)
    c(.td_md_tabela(t[intersect(c("teste", "estatistica", "gl", "p_valor", "decisao_5"), names(t))]),
      "", sprintf("H\u{2080}: %s. **Conclus\u{E3}o:** %s.", x$h0, x$conclusao))
  }
  .td_parte_markdown("teste", ctx, md)
}

# ---- comuns --------------------------------------------------------------------------

#' Parte de puro Markdown: um arquivo `<tipo>.md` incluído pela região.
#' @noRd
.td_parte_markdown <- function(tipo, ctx, md) {
  nome <- paste0(tipo, ".md")
  tr_documento_parte(tipo, titulo = ctx$titulo,
                     arquivos = stats::setNames(list(c(sprintf("**%s**", .td_legenda(ctx$titulo)), "", .td_aparar(md))), nome),
                     corpo = sprintf("{{< include %s >}}", ctx$arquivo(nome)),
                     metadados = list(tipo_schema = "CreativeWork", descricao = ctx$titulo))
}

#' Texto de `report` (às vezes um bloco só, com `\n`) em linhas.
#' @noRd
.td_linhas <- function(x) unlist(strsplit(paste(unclass(x), collapse = "\n"), "\n", fixed = TRUE))

#' Sem linhas em branco nas pontas.
#' @noRd
.td_aparar <- function(l) {
  cheias <- which(nzchar(trimws(l)))
  if (length(cheias)) l[cheias[[1]]:cheias[[length(cheias)]]] else character()
}

#' Legenda numa linha, sem o que fecha a sintaxe de legenda e imagem.
#' @noRd
.td_legenda <- function(x) {
  x <- gsub("[\r\n]+", " ", as.character(x %||% ""))
  x <- gsub("([][{}])", "\\\\\\1", x)
  if (nzchar(trimws(x))) trimws(x) else "Resultado"
}

#' Id de referência cruzada (`@fig-<id>`, `@tbl-<id>`): o Quarto aceita letras,
#' dígitos, `-` e `_`.
#' @noRd
.td_id_ref <- function(chave) gsub("[^A-Za-z0-9_-]", "-", chave)

.td_opcao_int <- function(v, padrao) {
  n <- suppressWarnings(as.integer(v))
  if (length(n) == 1L && !is.na(n) && n > 0L) n else padrao
}

.td_opcao_num <- function(v, padrao) {
  n <- suppressWarnings(as.numeric(v))
  if (length(n) == 1L && !is.na(n) && n > 0) n else padrao
}
