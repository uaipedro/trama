# O tempo de uma série, visto de fora dela.
#
# Um `ts` guarda o tempo como número decimal (`1949.083` é fevereiro de 1949),
# que é ótimo para aritmética e péssimo para quem vai filtrar a tabela na
# `data`. Toda saída tabular da coleção passa por aqui, então a regra mora num
# lugar só: mensal e trimestral viram `Date` (o primeiro dia do período), anual
# vira inteiro, e o resto — diária com ciclo semanal, horária — fica decimal,
# porque não há calendário honesto a reconstruir só a partir da frequência.

.TR_SERIES_MESES <- c("jan", "fev", "mar", "abr", "mai", "jun",
                      "jul", "ago", "set", "out", "nov", "dez")

#' O ano de um tempo decimal, dado o período do ciclo.
#'
#' `round(tt - (ciclo - 1) / f)`, e nunca `floor(tt)`: o tempo decimal de
#' janeiro de 1950 pode chegar como `1949.9999999`, e o `floor` o jogaria para
#' 1949 — a tabela sairia com dois dezembros e nenhum janeiro, sem erro nenhum.
#'
#' Mora aqui sozinho porque tem mais de um chamador — a coluna `tempo` e o
#' rótulo de um período —, e duas cópias da regra divergiriam no dia em que uma
#' delas fosse ajustada. Já esteve copiado, e a cópia não era visível de dentro
#' de nenhum dos dois lugares.
#' @noRd
.tr_series_ano <- function(tt, ciclo, f) as.integer(round(tt - (ciclo - 1) / f))

#' A coluna `tempo` de uma série.
#' @noRd
.tr_series_tempo <- function(x) {
  f <- stats::frequency(x)
  tt <- as.numeric(stats::time(x))
  if (f %in% c(4, 12)) {
    ciclo <- as.integer(stats::cycle(x))
    ano <- .tr_series_ano(tt, ciclo, f)
    mes <- (ciclo - 1L) * (12L %/% as.integer(f)) + 1L
    return(as.Date(sprintf("%04d-%02d-01", ano, mes)))
  }
  if (f == 1 && all(abs(tt - round(tt)) < 1e-6)) return(as.integer(round(tt)))
  tt
}

#' Série -> tabela `tempo`, `valor`. É o adaptador `series/ts -> data/table`.
#' @noRd
.tr_series_tabela <- function(x) {
  tibble::tibble(tempo = .tr_series_tempo(x), valor = as.numeric(x))
}

#' Rótulo legível de um período: "1949 jan", "1960 T3", "1871".
#' @noRd
.tr_series_rotulo <- function(periodo, f) {
  ano <- periodo[[1]]
  ciclo <- if (length(periodo) > 1L) periodo[[2]] else 1
  if (f == 12) return(sprintf("%d %s", as.integer(ano), .TR_SERIES_MESES[[as.integer(ciclo)]]))
  if (f == 4) return(sprintf("%d T%d", as.integer(ano), as.integer(ciclo)))
  if (f == 1) return(format(ano))
  sprintf("%g (período %g de %g)", ano, ciclo, f)
}

#' O rótulo do período na posição `i` da série.
#'
#' Índice inteiro -> "1953 jun". O ano vem de `.tr_series_ano()`, o mesmo que a
#' coluna `tempo` usa: é a mesma pergunta, e duas cópias dela divergiriam.
#' @noRd
.tr_series_rotulo_em <- function(x, i) {
  f <- stats::frequency(x)
  ciclo <- as.integer(stats::cycle(x))[[i]]
  ano <- .tr_series_ano(as.numeric(stats::time(x))[[i]], ciclo, f)
  .tr_series_rotulo(c(ano, ciclo), f)
}

#' Lê "1955, 3" ou "1955" como período.
#'
#' Só número, e de propósito: aceitar "mar 1955" pediria um parser de nome de
#' mês por idioma, e o erro de digitação viraria um período adivinhado. O
#' separador pode ser vírgula, ponto e vírgula ou espaço, que é o que se
#' escreve sem pensar.
#' @noRd
.tr_series_periodo <- function(txt, param, f = NULL) {
  txt <- trimws(txt)
  partes <- strsplit(txt, "[,;[:space:]]+")[[1]]
  v <- suppressWarnings(as.numeric(partes))
  if (!length(v) || length(v) > 2L || anyNA(v)) {
    .tr_series_abort("tr_series_error_bad_period",
                     paste0("Param '%s': período ilegível '%s'. Escreva o ano, ou 'ano, período' ",
                            "(ex.: '1955, 3' para março de 1955 numa série mensal)."), param, txt)
  }
  if (length(v) == 2L && !is.null(f) && (v[[2]] < 1 || v[[2]] > f || v[[2]] != round(v[[2]]))) {
    .tr_series_abort("tr_series_error_bad_period",
                     "Param '%s': o período '%g' não existe numa série de frequência %g (vai de 1 a %g).",
                     param, v[[2]], f, f)
  }
  v
}

#' Período -> posição no tempo decimal do `ts`.
#' @noRd
.tr_series_pos <- function(v, f) v[[1]] + if (length(v) > 1L) (v[[2]] - 1) / f else 0

#' Tira a dimensão de um `ts` de uma coluna só.
#'
#' `x[, "DAX"]` e alguns cálculos devolvem série com `dim` n x 1, que o guard
#' do tipo `series/ts` recusaria por parecer múltipla. A série é a mesma.
#' @noRd
.tr_series_uni <- function(x) {
  if (!is.null(dim(x)) && NCOL(x) == 1L) {
    x <- stats::ts(as.numeric(x), start = stats::start(x), frequency = stats::frequency(x))
  }
  x
}

#' Os nomes das estações: meses, trimestres, ou o número do período.
#'
#' Mora aqui, com `.TR_SERIES_MESES` e `.tr_series_rotulo()`, porque não é
#' coisa de gráfico: além dos eixos, estes nomes viram os `levels` do fator de
#' estação da regressão, e portanto os NOMES dos coeficientes que se lê na
#' tabela — "estacaodez" diz o que "estacao12" não diria.
#' @noRd
.tr_series_estacoes <- function(f) {
  if (f == 12) return(.TR_SERIES_MESES)
  if (f == 4) return(paste0("T", 1:4))
  as.character(seq_len(f))
}
