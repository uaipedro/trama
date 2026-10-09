# O que os tipos daqui mostram no relatório Quarto exportado (`report` do
# tipo): o resumo do card e a tabela que se lê — autovalores e cargas na PCA,
# cargas na fatorial, os membros de cada grupo no agrupamento. O Markdown é o da
# `trama.data`.

.tr_multi_md <- function(...) trama.data::tr_data_report(c(...))

#' O que os tipos da `trama.multi` mostram no relatório exportado.
#'
#' `report` de cada tipo: `multi/pca` (resumo, autovalores e cargas),
#' `multi/fa` (resumo e cargas de padrão), `multi/dist` (resumo e, até 12
#' indivíduos, a matriz) e `multi/cluster` (resumo e os membros de cada grupo).
#'
#' @param x o objeto do tipo.
#' @return objeto `trama.data::tr_data_report`.
#' @export
tr_multi_report_pca <- function(x) {
  .tr_multi_md(
    trama.data::tr_data_md_summary(.tr_multi_pca_resumo(x), "Componentes principais"), "",
    "**Autovalores**", "", trama.data::tr_data_md_table(tr_multi_pca_variance(x)), "",
    "**Cargas (correlações com os componentes)**", "", trama.data::tr_data_md_table(tr_multi_pca_loadings(x)))
}

#' @rdname tr_multi_report_pca
#' @export
tr_multi_report_fa <- function(x) {
  .tr_multi_md(
    trama.data::tr_data_md_summary(.tr_multi_fa_resumo(x), "Análise fatorial"), "",
    "**Cargas de padrão**", "", trama.data::tr_data_md_table(tr_multi_fa_loadings(x)))
}

#' @rdname tr_multi_report_pca
#' @export
tr_multi_report_dist <- function(x) {
  v <- as.vector(x$d)
  res <- list(metodo = x$metodo, individuos = length(x$rotulos), variaveis = length(x$variaveis),
              minima = min(v), maxima = max(v))
  # Até 12 indivíduos a matriz cabe na página; acima, é o mapa de calor do card
  # que se lê, não um quadro de números.
  matriz <- if (length(x$rotulos) <= 12L) {
    M <- round(as.matrix(x$d), 4)
    M[upper.tri(M, diag = TRUE)] <- NA
    df <- cbind(data.frame(` ` = x$rotulos, check.names = FALSE), as.data.frame(M, optional = TRUE))
    c("", "**Matriz de distância**", "", trama.data::tr_data_md_table(df[-1, -ncol(df)]))
  }
  .tr_multi_md(trama.data::tr_data_md_summary(res, "Distância"), matriz)
}

#' @rdname tr_multi_report_pca
#' @export
tr_multi_report_cluster <- function(x) {
  res <- list(metodo = x$metodo, distancia = x$distancia$metodo, grupos = x$k,
              cofenetica = x$cofenetica)
  rot <- x$distancia$rotulos
  membros <- data.frame(
    grupo = seq_len(x$k), n = tabulate(x$grupos, x$k),
    membros = vapply(seq_len(x$k), function(g) {
      m <- rot[x$grupos == g]
      if (length(m) > 15L) paste0(paste(m[1:15], collapse = ", "), sprintf(" … (+%d)", length(m) - 15L))
      else paste(m, collapse = ", ")
    }, ""))
  .tr_multi_md(trama.data::tr_data_md_summary(res, "Agrupamento"), "",
               trama.data::tr_data_md_table(membros, c(grupo = "Grupo", n = "n", membros = "Membros")))
}
