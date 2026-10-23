# Descritivas de colunas numéricas: o par de números do `data/summary`.
#
# O `data/summary` diz o que CHEGOU (tipo, faltantes, extremos, forma em
# texto) para toda coluna; este nó diz a DISTRIBUIÇÃO das numéricas, com os
# momentos que um relatório cita (média, desvio, assimetria, curtose). São
# dois nós porque as perguntas são outras, e o resumo não vira tabela de
# números para ser lido como laudo.

#' Os momentos centrais de `x` (sem faltantes), com divisor n.
#' @noRd
.tr_data_momentos <- function(x) {
  d <- x - mean(x)
  c(m2 = mean(d^2), m3 = mean(d^3), m4 = mean(d^4))
}

#' Descritivas por coluna numérica: uma linha por coluna.
#'
#' Duas convenções, e as duas declaradas porque mudam o número:
#'
#' * `dp` é o desvio-padrão amostral, divisor n - 1 (`stats::sd`);
#' * `assimetria` e `curtose_excesso` são os coeficientes de momento g1 e g2
#'   de Joanes & Gill (1998), com divisor n: g1 = m3 / m2^(3/2) e
#'   g2 = m4 / m2^2 - 3. É a curtose em EXCESSO (zero na normal), e são
#'   os momentos que o Jarque-Bera usa. `moments::skewness` dá o g1 e
#'   `moments::kurtosis` dá g2 + 3.
#'
#' Em branco, todas as colunas numéricas, menos as de índice que a tabela
#' declara no atributo `trama_indice` (o `tempo` de uma série vinda do
#' adaptador). Coluna citada que não é numérica é erro, não descarte.
#' Faltantes ficam de fora de cada conta e são contados em `faltantes`.
#' @export
tr_describe <- function(dados, cols = "") {
  nomes <- .as_cols(cols)
  if (!length(nomes)) {
    indice <- as.character(attr(dados, "trama_indice", exact = TRUE))
    nomes <- setdiff(names(dados)[vapply(dados, is.numeric, TRUE)], indice)
    if (!length(nomes)) {
      rlang::abort("'data/describe': a tabela não tem coluna numérica para descrever.",
                   class = "tr_data_error_not_numeric")
    }
  } else {
    .tr_data_cols(dados, nomes, "cols")
    texto <- nomes[!vapply(dados[nomes], is.numeric, TRUE)]
    if (length(texto)) {
      rlang::abort(sprintf(paste0("Param 'cols': coluna não numérica: %s. Tire-a da lista, ",
                                  "ou converta antes num 'data/convert'."), paste(texto, collapse = ", ")),
                   class = "tr_data_error_not_numeric")
    }
  }
  linhas <- lapply(nomes, function(nm) {
    v <- as.numeric(dados[[nm]])
    x <- v[!is.na(v)]
    n <- length(x)
    m <- if (n) .tr_data_momentos(x) else c(m2 = NA_real_, m3 = NA_real_, m4 = NA_real_)
    # Coluna constante (m2 = 0) não tem forma: assimetria e curtose seriam 0/0.
    forma <- n > 0L && m[["m2"]] > 0
    tibble::tibble(
      coluna = nm, n = n, faltantes = sum(is.na(v)),
      media = if (n) mean(x) else NA_real_,
      dp = if (n > 1L) stats::sd(x) else NA_real_,
      mediana = if (n) stats::median(x) else NA_real_,
      minimo = if (n) min(x) else NA_real_,
      maximo = if (n) max(x) else NA_real_,
      assimetria = if (forma) m[["m3"]] / m[["m2"]]^1.5 else NA_real_,
      curtose_excesso = if (forma) m[["m4"]] / m[["m2"]]^2 - 3 else NA_real_)
  })
  out <- do.call(rbind, linhas)
  out$n <- as.integer(out$n)
  out$faltantes <- as.integer(out$faltantes)
  out
}
