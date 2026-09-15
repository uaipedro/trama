# Operadores: série entra, série sai.
#
# Só o que depende do TEMPO mora aqui. Somar uma constante, filtrar por valor,
# preencher faltante com zero: tudo isso é `data`, e a série chega lá pelo
# adaptador. Diferença, defasagem, janela e agregação não existem sem a
# frequência — é por isso que são desta coleção.

#' Diferença simples ou sazonal.
#'
#' `tipo = "sazonal"` usa a frequência como defasagem, e é aqui que a série
#' anual precisa de erro: `diff(x, lag = 1)` com outro nome não é diferença
#' sazonal, e o card afirmaria uma coisa que a conta não fez.
#' @export
tr_series_diff <- function(serie, tipo = "simples", ordem = 1L) {
  tipo <- .tr_series_enum(tipo, c("simples", "sazonal"), "tipo")
  ordem <- .tr_series_int(ordem, "ordem", min = 1, max = 3)
  lag <- 1L
  if (tipo == "sazonal") {
    .tr_series_sazonal(serie, "series/diff", ciclos = 1L)
    lag <- as.integer(stats::frequency(serie))
  }
  .tr_series_minimo(serie, lag * ordem + 2L, "series/diff",
                    sprintf("uma diferença de defasagem %d e ordem %d", lag, ordem))
  diff(serie, lag = lag, differences = ordem)
}

#' Defasa a série k períodos: o valor de t passa a morar em t + k.
#'
#' Os valores não mudam, só o tempo — que é exatamente o que o `ts` guarda.
#' Isso importa para quem vai juntar a série defasada com a original numa
#' tabela (`data/join` por `tempo`): é o join que alinha `x_t` com `x_{t-k}`.
#' `k` negativo adianta.
#' @export
tr_series_lag <- function(serie, k = 1L) {
  k <- .tr_series_int(k, "k", min = -(length(serie) - 1), max = length(serie) - 1)
  stats::lag(serie, -k)
}

#' Log, raiz ou Box-Cox: estabiliza a variância que cresce com o nível.
#'
#' Log de zero é `-Inf`, e de negativo é `NaN` com um aviso que não chega ao
#' card: a série sairia com buracos que parecem faltantes. Recusar com a
#' contagem é o que manda a pessoa somar uma constante antes (na `data`), com
#' a escolha de quanto somar feita às claras.
#'
#' `lambda` em branco é Box-Cox AUTOMÁTICO (Guerrero), e o λ escolhido fica
#' pendurado na série como atributo — é o que o `forecast` usa, e o que o
#' resumo mostraria se alguém perguntar "qual λ ele escolheu?".
#' @export
tr_series_transform <- function(serie, metodo = "log", lambda = "") {
  metodo <- .tr_series_enum(metodo, c("log", "raiz", "boxcox"), "metodo")
  piso <- if (metodo == "raiz") serie < 0 else serie <= 0
  if (any(piso, na.rm = TRUE)) {
    .tr_series_abort("tr_series_error_nonpositive",
                     paste0("'%s' pede valores %s, e a série tem %d abaixo disso (o menor é %g). ",
                            "Some uma constante antes, num 'data/mutate'."),
                     metodo, if (metodo == "raiz") "não negativos" else "positivos",
                     sum(piso, na.rm = TRUE), min(serie, na.rm = TRUE))
  }
  if (metodo == "log") return(log(serie))
  if (metodo == "raiz") return(sqrt(serie))
  lambda <- trimws(as.character(lambda %||% "")[[1]])
  lam <- if (!nzchar(lambda)) forecast::BoxCox.lambda(serie) else
    suppressWarnings(as.numeric(sub(",", ".", lambda, fixed = TRUE)))
  if (length(lam) != 1L || is.na(lam)) .tr_series_option("lambda", lambda, "um número, ou vazio para automático")
  out <- forecast::BoxCox(serie, lam)
  attr(out, "lambda") <- lam
  out
}

#' Recorta a série entre dois períodos.
#'
#' `fim` só com o ano quer dizer o ÚLTIMO período daquele ano. Sem isto,
#' `fim = "1956"` numa série mensal pararia em janeiro de 1956 — que é o que o
#' `window()` do R entende, e não o que ninguém quis dizer.
#'
#' Período fora da série é erro, não recorte silencioso até a borda: um
#' `inicio = "1995"` digitado no lugar de `"1959"` devolveria a série inteira
#' e o card verde.
#' @export
tr_series_window <- function(serie, inicio = "", fim = "") {
  inicio <- trimws(as.character(inicio %||% "")[[1]])
  fim <- trimws(as.character(fim %||% "")[[1]])
  if (!nzchar(inicio) && !nzchar(fim)) return(serie)
  f <- stats::frequency(serie)
  tsp <- stats::tsp(serie)
  vai <- sprintf("a série vai de %s a %s",
                 .tr_series_rotulo(stats::start(serie), f), .tr_series_rotulo(stats::end(serie), f))
  s <- if (nzchar(inicio)) .tr_series_periodo(inicio, "inicio", f) else stats::start(serie)
  e <- if (nzchar(fim)) .tr_series_periodo(fim, "fim", f) else stats::end(serie)
  if (nzchar(fim) && length(e) == 1L && f > 1) e <- c(e, f)
  ps <- .tr_series_pos(s, f); pe <- .tr_series_pos(e, f)
  eps <- 1e-8
  if (ps < tsp[[1]] - eps || ps > tsp[[2]] + eps) {
    .tr_series_abort("tr_series_error_bad_period", "Param 'inicio': '%s' está fora da série (%s).", inicio, vai)
  }
  if (pe < tsp[[1]] - eps || pe > tsp[[2]] + eps) {
    .tr_series_abort("tr_series_error_bad_period", "Param 'fim': '%s' está fora da série (%s).", fim, vai)
  }
  if (pe < ps) {
    .tr_series_abort("tr_series_error_bad_period", "O fim ('%s') vem antes do início ('%s').", fim, inicio)
  }
  stats::window(serie, start = s, end = e)
}

#' Média móvel de ordem k — a tendência a olho, sem modelo.
#'
#' Centrada com ordem par é a média 2×k (a de ordem 12 numa série mensal é a
#' que remove a sazonalidade inteira), que é o que `forecast::ma()` faz e o
#' que a decomposição clássica usa por dentro. As bordas saem NA — metade da
#' janela para cada lado —, e não encurtadas: é o honesto, e o gráfico mostra.
#' @export
tr_series_moving_average <- function(serie, ordem = 12L, centrada = TRUE) {
  ordem <- .tr_series_int(ordem, "ordem", min = 2, max = max(2, length(serie) - 1))
  forecast::ma(serie, order = ordem, centre = isTRUE(centrada))
}

#' Muda a frequência para uma menor: mensal -> trimestral -> anual.
#'
#' Não usa `stats::aggregate()`, e a razão foi medida: ele agrupa em blocos
#' a partir da PRIMEIRA observação, e não do calendário. Numa série mensal
#' que começa em março, o "ano" 1 soma de março a fevereiro e é rotulado com o
#' ano de março — anos que não existem, verdes. Aqui o primeiro bloco
#' incompleto é descartado, e o último também: o ano de 1960 com seis meses
#' somaria a metade e pareceria uma queda.
#' @export
tr_series_aggregate <- function(serie, frequencia = 1L, funcao = "soma") {
  f <- as.integer(stats::frequency(serie))
  nova <- .tr_series_int(frequencia, "frequencia", min = 1)
  funcao <- .tr_series_enum(funcao, c("soma", "media", "ultimo"), "funcao")
  if (nova >= f || f %% nova != 0L) {
    .tr_series_abort("tr_series_error_bad_frequency",
                     paste0("Param 'frequencia': %d não divide a frequência da série (%d). Uma série de ",
                            "frequência %d agrega para %s."), nova, f, f,
                     paste(Filter(function(k) f %% k == 0L, seq_len(f - 1L)), collapse = ", "))
  }
  b <- f %/% nova
  ciclo <- as.integer(stats::cycle(serie))
  primeiro <- which((ciclo - 1L) %% b == 0L)[1]
  x <- as.numeric(serie)[primeiro:length(serie)]
  blocos <- length(x) %/% b
  if (is.na(primeiro) || blocos < 1L) {
    .tr_series_abort("tr_series_error_too_short",
                     "'series/aggregate': a série não tem nenhum bloco completo de %d períodos.", b)
  }
  m <- matrix(x[seq_len(blocos * b)], nrow = b)
  fn <- switch(funcao, soma = sum, media = mean, ultimo = function(v) v[[length(v)]])
  ano <- .tr_series_ano(as.numeric(stats::time(serie))[[primeiro]], ciclo[[primeiro]], f)
  stats::ts(apply(m, 2, fn), start = c(ano, (ciclo[[primeiro]] - 1L) %/% b + 1L), frequency = nova)
}

#' Preenche faltantes pela estrutura da série.
#'
#' Existe ao lado do `data/replace_na` porque faz outra coisa: não põe uma
#' constante, estima o valor pelo vizinho no tempo e, em série sazonal, pela
#' mesma estação dos outros anos (`forecast::na.interp`). Preencher o julho
#' faltante com zero derruba a sazonalidade inteira; com a interpolação, o
#' julho sai com cara de julho.
#' @export
tr_series_interpolate <- function(serie) {
  if (!anyNA(serie)) return(serie)
  if (sum(!is.na(serie)) < 2L) {
    .tr_series_abort("tr_series_error_too_short",
                     "'series/interpolate': a série tem menos de dois valores observados; não há o que interpolar.")
  }
  forecast::na.interp(serie)
}
