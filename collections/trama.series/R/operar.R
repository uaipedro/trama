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

#' Tira a tendência da série, e diz qual tendência tirou.
#'
#' É o "estimo num lm e depois subtraio" num nó só. A sazonalidade FICA —
#' quem quer tirá-la junto usa a decomposição —, e a tendência estimada vai
#' pendurada na série como atributo `tendencia` (um `ts` no mesmo tempo): é o
#' que permite conferir, no console, que `saída + tendência` devolve a série.
#'
#' O polinômio é ortogonal (`poly()`), e não cru como o da `series/regression`:
#' aqui só o AJUSTE importa, não a leitura dos coeficientes, e `t⁵` cru numa
#' série de 144 meses tem número de condição astronômico — o `lm` perderia
#' dígitos sem avisar.
#'
#' Faltante não impede o ajuste (`na.exclude`): a tendência é prevista em todo
#' t, e a saída tem NA só onde a série tinha.
#'
#' `diferenca` é o outro jeito de tirar tendência, e o único sem modelo: a
#' "tendência" é o valor do período anterior, e a série perde a 1ª observação
#' — começa um período depois, com o calendário certo.
#' @export
tr_series_detrend <- function(serie, metodo = "linear", grau = 2L, suavidade = 0.75) {
  metodo <- .tr_series_enum(metodo, c("linear", "polinomial", "loess", "diferenca"), "metodo")
  f <- stats::frequency(serie)
  como_ts <- function(v, inicio = stats::tsp(serie)[[1]]) stats::ts(v, start = inicio, frequency = f)
  if (metodo == "diferenca") {
    .tr_series_minimo(serie, 3L, "series/detrend", "a diferença")
    x <- as.numeric(serie)
    out <- diff(serie)
    attr(out, "tendencia") <- como_ts(x[-length(x)], stats::tsp(serie)[[1]] + 1 / f)
    return(out)
  }
  tt <- seq_along(serie)
  y <- as.numeric(serie)
  if (metodo == "loess") {
    ok <- length(suavidade) == 1L && is.numeric(suavidade) && !is.na(suavidade) &&
      suavidade > 0 && suavidade <= 1
    if (!ok) .tr_series_option("suavidade", suavidade, "um número maior que 0 e até 1")
    .tr_series_minimo(serie, 5L, "series/detrend", "a tendência loess",
                      validas = sum(!is.na(serie)))
    fit <- stats::loess(y ~ tt, span = suavidade, degree = 2L, na.action = stats::na.exclude)
  } else {
    g <- if (metodo == "linear") 1L else .tr_series_int(grau, "grau", min = 2, max = 5)
    .tr_series_minimo(serie, g + 3L, "series/detrend", sprintf("um polinômio de grau %d", g),
                      validas = sum(!is.na(serie)))
    fit <- stats::lm(y ~ stats::poly(tt, g), na.action = stats::na.exclude)
  }
  tend <- as.numeric(stats::predict(fit, newdata = data.frame(tt = tt)))
  out <- como_ts(y - tend)
  attr(out, "tendencia") <- como_ts(tend)
  out
}

#' Opera duas séries ponto a ponto: a − b, a + b, a / b, a × b.
#'
#' O alinhamento é pelo TEMPO, e não pela posição: `a` de 1949 a 1960 menos
#' `b` de 1955 a 1965 é a diferença de 1955 a 1960 — a interseção. Operar os
#' vetores crus somaria janeiro de 1949 com janeiro de 1955, verde.
#'
#' Frequências diferentes são erro, e não conversão implícita: somar uma
#' mensal com uma trimestral pede escolher COMO agregar (soma? média?), e é o
#' `series/aggregate` que faz essa pergunta.
#'
#' Divisão por zero sai NA, e não `Inf`/`NaN`: um `Inf` no meio da série
#' quebra a escala do gráfico e todo nó seguinte, e o aviso do R não chega ao
#' card. O NA chega — o resumo da série conta os faltantes —, e o
#' `series/interpolate` sabe o que fazer com ele.
#' @export
tr_series_combine <- function(a, b, operacao = "a - b") {
  operacao <- .tr_series_enum(operacao, c("a - b", "a + b", "a / b", "a * b"), "operacao")
  fa <- stats::frequency(a); fb <- stats::frequency(b)
  if (!isTRUE(all.equal(fa, fb))) {
    .tr_series_abort("tr_series_error_frequency_mismatch",
                     paste0("'series/combine': 'a' tem frequência %g e 'b' tem %g. Leve as duas à ",
                            "mesma frequência antes, com 'series/aggregate'."), fa, fb)
  }
  # Mesma frequência não basta: uma mensal que começa em 1949,0 e outra em
  # 1949,04 (meio mês de defasagem) não têm nenhum período em comum, e o
  # `window()` arredondaria uma para a grade da outra, calado.
  desvio <- (stats::tsp(a)[[1]] - stats::tsp(b)[[1]]) * fa
  if (abs(desvio - round(desvio)) > 1e-6) {
    .tr_series_abort("tr_series_error_misaligned",
                     paste0("'series/combine': 'a' e 'b' têm frequência %g, mas as grades de tempo ",
                            "estão defasadas em %.3g de período: nenhum instante coincide. Confira o ",
                            "início declarado de cada série."), fa, desvio - round(desvio))
  }
  ini <- max(stats::tsp(a)[[1]], stats::tsp(b)[[1]])
  fim <- min(stats::tsp(a)[[2]], stats::tsp(b)[[2]])
  if (fim < ini - 1e-8) {
    .tr_series_abort("tr_series_error_no_overlap",
                     "'series/combine': as séries não têm período em comum ('a' vai de %s a %s; 'b', de %s a %s).",
                     .tr_series_rotulo(stats::start(a), fa), .tr_series_rotulo(stats::end(a), fa),
                     .tr_series_rotulo(stats::start(b), fb), .tr_series_rotulo(stats::end(b), fb))
  }
  # `as.numeric()` antes da conta: descarta atributos pendurados (a
  # `tendencia` do `series/detrend`), que a aritmética de `ts` carregaria para
  # a saída sem que ela quisesse mais dizer nada.
  xa <- as.numeric(stats::window(a, start = ini, end = fim))
  xb <- as.numeric(stats::window(b, start = ini, end = fim))
  out <- switch(operacao,
    "a - b" = xa - xb, "a + b" = xa + xb, "a * b" = xa * xb,
    "a / b" = ifelse(xb == 0, NA_real_, xa / xb))
  stats::ts(out, start = ini, frequency = fa)
}
