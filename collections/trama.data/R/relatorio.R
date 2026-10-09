# O que um objeto mostra no relatório Quarto exportado.
#
# O exportador do núcleo chama `report(objeto)` do tipo em cada chunk cuja
# saída ninguém consome; sem `report`, cai no `print` cru — e o `print` de uma
# lista nomeada despeja `$tabela`, `$titulo`, `attr(,"class")`. Estas funções
# devolvem Markdown pronto: no Quarto sai como tabela de verdade (knitr trata a
# classe `knit_asis` como texto literal); no script `.R`, o `print` mostra o
# mesmo Markdown no console.

#' Texto em Markdown para o relatório exportado.
#'
#' Embrulha linhas de Markdown num objeto que o knitr insere como está (classe
#' `knit_asis`) e que o `print` mostra no console. É a base dos `report` dos
#' tipos das coleções.
#'
#' @param linhas vetor de linhas de Markdown.
#' @return objeto `tr_data_report`.
#' @export
tr_data_report <- function(linhas) {
  structure(paste(c("", linhas, ""), collapse = "\n"),
            class = c("tr_data_report", "knit_asis"), knit_cacheable = NA)
}

#' @export
print.tr_data_report <- function(x, ...) {
  cat(unclass(x), "\n", sep = "")
  invisible(x)
}

#' Números e p-valores como se escrevem no relatório.
#'
#' `tr_data_fmt_num`: 4 significativos, vírgula decimal. `tr_data_fmt_p`: três
#' casas, e "< 0,001" abaixo de 0,001. `NA` vira texto vazio.
#'
#' @param v,p vetor numérico.
#' @param digitos algarismos significativos.
#' @return vetor de texto.
#' @export
tr_data_fmt_num <- function(v, digitos = 4L) {
  inteiro <- !is.na(v) & is.finite(v) & v == round(v) & abs(v) < 1e15
  out <- formatC(signif(v, digitos), format = "fg", digits = digitos, big.mark = ".",
                 decimal.mark = ",")
  out[inteiro] <- formatC(v[inteiro], format = "d", big.mark = ".", decimal.mark = ",")
  ifelse(is.na(v), "", trimws(out))
}

#' @rdname tr_data_fmt_num
#' @export
tr_data_fmt_p <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "< 0,001", formatC(p, format = "f", digits = 3L, decimal.mark = ",")))
}

#' Tabela de Markdown (pipe) a partir de um data frame.
#'
#' Colunas `p_valor` saem como p-valor ("< 0,001" abaixo de 0,001); as demais
#' numéricas com 4 significativos e vírgula decimal; texto e fatores, como
#' estão. `|` dentro de célula é escapado.
#'
#' @param df um data frame.
#' @param rotulos vetor nomeado `coluna = cabeçalho`; as colunas fora dele
#'   ficam com o próprio nome.
#' @return vetor de linhas de Markdown.
#' @export
tr_data_md_table <- function(df, rotulos = NULL) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  cel <- lapply(names(df), function(n) {
    v <- df[[n]]
    out <- if (n == "p_valor" && is.numeric(v)) tr_data_fmt_p(v)
           else if (is.numeric(v)) tr_data_fmt_num(v)
           else ifelse(is.na(v), "", as.character(v))
    gsub("|", "\\|", out, fixed = TRUE)
  })
  cab <- names(df)
  if (!is.null(rotulos)) cab <- ifelse(cab %in% names(rotulos), rotulos[cab], cab)
  cab <- gsub("|", "\\|", cab, fixed = TRUE)
  alinha <- vapply(df, function(v) if (is.numeric(v)) "---:" else ":---", "")
  corpo <- if (nrow(df)) do.call(paste, c(cel, sep = " | ")) else character()
  c(paste0("| ", paste(cab, collapse = " | "), " |"),
    paste0("|", paste(alinha, collapse = "|"), "|"),
    if (length(corpo)) paste0("| ", corpo, " |"))
}

#' Uma tabela no relatório exportado.
#'
#' Mostra até `max_linhas` linhas, tira as colunas inteiramente vazias (ficam
#' citadas embaixo) e diz o tamanho da tabela. É o `report` do tipo
#' `data/table`.
#'
#' @param x um data frame.
#' @param max_linhas quantas linhas mostrar.
#' @return objeto `tr_data_report`.
#' @export
tr_data_report_table <- function(x, max_linhas = 20L) {
  df <- as.data.frame(x)
  vazias <- names(df)[vapply(df, function(v) all(is.na(v)), logical(1))]
  if (length(vazias) && length(vazias) < ncol(df)) df <- df[setdiff(names(df), vazias)]
  else vazias <- character()
  n <- nrow(df)
  # Uma linha só e muitas colunas (medidas de ajuste, um resumo): de pé, uma
  # medida por linha, em vez de uma faixa que estoura a largura da página.
  if (n == 1L && ncol(df) > 6L) {
    df <- data.frame(medida = names(df),
                     valor = vapply(df, function(v) if (is.numeric(v)) tr_data_fmt_num(v) else as.character(v), ""),
                     stringsAsFactors = FALSE)
  }
  rodape <- c(
    if (n > max_linhas) sprintf("Primeiras %d de %d linhas.", max_linhas, n)
    else sprintf("%d %s × %d %s.", n, if (n == 1L) "linha" else "linhas",
                 ncol(x), if (ncol(x) == 1L) "coluna" else "colunas"),
    if (length(vazias)) sprintf("Colunas sem valor omitidas: %s.", paste(vazias, collapse = ", ")))
  tr_data_report(c(tr_data_md_table(utils::head(df, max_linhas)), "", paste0("*", paste(rodape, collapse = " "), "*")))
}

#' Um teste de hipótese no relatório exportado.
#'
#' Uma linha de quadro (estatística, gl, p-valor, decisão a 5%) seguida da
#' conclusão em texto, do efeito (quando o teste dá um) e da nota. É o `report`
#' do tipo `data/test`.
#'
#' @param x um teste (`trama::tr_test`).
#' @return objeto `tr_data_report`.
#' @export
tr_data_report_test <- function(x) {
  gl <- if (is.null(x$gl) || identical(x$gl, "NA") || all(is.na(x$gl))) "" else as.character(x$gl)
  linha <- data.frame(teste = x$teste, est = tr_data_fmt_num(x$estatistica), gl = gl,
                      p = if (is.null(x$p_valor)) "" else tr_data_fmt_p(x$p_valor),
                      decisao = x$decisao_5 %||% "", stringsAsFactors = FALSE)
  if (!nzchar(gl)) linha$gl <- NULL
  if (!nzchar(linha$p)) linha$p <- NULL
  rot <- c(teste = "Teste", est = x$rotulo_estat %||% "Estatística", gl = "gl",
           p = "p-valor", decisao = "Decisão (5%)")
  ef <- x$efeito
  efeito <- if (length(ef) && !is.null(ef$valor)) {
    sprintf("%s: %s%s", ef$rotulo, tr_data_fmt_num(ef$valor),
            if (!is.null(ef$li) && !is.na(ef$li)) sprintf(" (IC: %s a %s)", tr_data_fmt_num(ef$li), tr_data_fmt_num(ef$ls)) else "")
  }
  tr_data_report(c(
    tr_data_md_table(linha, rot), "",
    sprintf("H₀: %s. **Conclusão:** %s.", x$h0, x$conclusao),
    if (length(efeito)) c("", efeito),
    if (nzchar(x$nota %||% "")) c("", sprintf("*%s.*", sub("\\.$", "", x$nota))),
    if (nzchar(x$fonte %||% "")) c("", sprintf("Fonte: %s.", x$fonte))))
}
