# Os cinco tipos da coleção, e os adaptadores que os ligam à `data`.
#
# Por que tipos próprios, e não `data/table` com colunas combinadas: a série
# carrega a FREQUÊNCIA, e é ela que dá sentido à diferença sazonal, à
# decomposição, ao correlograma e ao ingênuo sazonal. Numa tabela, cada nó
# pediria de novo "qual coluna é o tempo, qual é o valor, qual a frequência" —
# três campos repetidos em trinta cards, e a frequência digitada trinta vezes é
# trinta chances de discordar de si mesma em silêncio.
#
# O preço de um tipo próprio seria perder a `data`, e são os ADAPTADORES que o
# pagam: o motor os insere na aresta, sem caixa na tela, e um fio de uma série
# para `data/filter` ou `view/histogram` simplesmente funciona. A volta
# (tabela -> série) não pode ser adaptador porque precisa de params: é o nó
# `series/from_table`.
#
# Todo `store` é FUNIL, na doutrina das irmãs: o guard mora nele, e não em
# cada `fn`, e vale para qualquer nó que declare o tipo, hoje e amanhã.

.TR_SERIES_COR <- "#34d399"
.TR_SERIES_COR_2 <- "#60a5fa"

#' `series/ts`: um `ts` univariado.
#'
#' Univariado de propósito. Série múltipla (`mts`) é várias séries num objeto
#' só, e cada nó daqui teria de perguntar "qual delas?" — o fio já responde
#' isso de graça, um por série. O guard recusa `mts` nomeando as colunas,
#' que é o diagnóstico que manda escolher uma.
#' @noRd
series_ts_type <- function() {
  trama::tr_type(
    "series/ts", version = 1L, label = "Série", color = .TR_SERIES_COR, ext = "rds",
    store = function(x, path) {
      .tr_series_guard_ts(x)
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) {
      f <- stats::frequency(x)
      list(inicio = .tr_series_rotulo(stats::start(x), f),
           fim = .tr_series_rotulo(stats::end(x), f),
           frequencia = f, observacoes = length(x), faltantes = sum(is.na(x)))
    },
    # O card de uma série é o GRÁFICO dela: 25 linhas de `tempo, valor` não
    # dizem nada que o traço não diga melhor, e a tabela continua a um fio de
    # distância pelo adaptador. `2:1` porque série é comprida por natureza.
    preview = function(x, ctx) {
      trama.view::tr_view_render(tr_series_plot(x, aspecto = "2:1"), ctx)
    }
  )
}

.tr_series_guard_ts <- function(x) {
  if (stats::is.ts(x) && !is.null(dim(x)) && NCOL(x) > 1L) {
    .tr_series_abort("tr_series_error_not_a_series",
                     paste0("O nó produziu uma série múltipla (%d colunas: %s), e series/ts guarda ",
                            "uma série só. Escolha a coluna antes."),
                     NCOL(x), paste(colnames(x), collapse = ", "))
  }
  if (!stats::is.ts(x) || !is.numeric(x) || !is.null(dim(x))) {
    .tr_series_abort("tr_series_error_not_a_series",
                     "O nó produziu um objeto '%s', não uma série temporal.", class(x)[[1]])
  }
  invisible(x)
}

#' A decomposição: os quatro componentes e como foram obtidos.
#'
#' Lista com classe própria, e não o objeto do `decompose()` ou do `stl()`:
#' os dois guardam os componentes em formas diferentes (`$trend` num,
#' `$time.series[, "trend"]` no outro), e o `series/component` teria de saber
#' de onde veio cada um. Normalizar na saída do nó é o que deixa o resto da
#' coleção cego ao método.
#'
#' `regressor` é o quarto componente, e só a `series/regression` com covariável
#' o tem (β·x): NULL nas outras. Ele fica À PARTE, e não dentro da tendência,
#' porque a tendência promete ser função do tempo — somar β·x a ela faria a
#' "tendência" oscilar com a covariável, e o `sem_tendencia` tiraria as duas.
#' @noRd
.tr_series_decomp <- function(observado, tendencia, sazonal, resto, tipo, metodo, regressor = NULL) {
  structure(list(observado = observado, tendencia = tendencia, sazonal = sazonal,
                 resto = resto, tipo = tipo, metodo = metodo, regressor = regressor),
            class = "tr_series_decomp")
}

series_decomposition_type <- function() {
  trama::tr_type(
    "series/decomposition", version = 1L, label = "Decomposição", color = "#a3e635",
    ext = "rds",
    store = function(x, path) {
      if (!inherits(x, "tr_series_decomp")) {
        .tr_series_abort("tr_series_error_not_a_decomposition",
                         "O nó produziu um objeto '%s', não uma decomposição.", class(x)[[1]])
      }
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) {
      forca <- .tr_series_forca(x)
      list(metodo = x$metodo, tipo = x$tipo,
           forca_tendencia = round(forca[["tendencia"]], 3),
           forca_sazonal = round(forca[["sazonal"]], 3))
    },
    preview = function(x, ctx) {
      trama.view::tr_view_render(tr_series_plot_decomposition(x, aspecto = "4:3"), ctx)
    }
  )
}

#' Força da tendência e da sazonalidade (Hyndman & Athanasopoulos, cap. 4).
#'
#' `1 - var(resto) / var(componente + resto)`, cortado em zero. É o número que
#' responde "tem sazonalidade mesmo?" sem olhar o gráfico, e por isso vai no
#' resumo do card. Na multiplicativa a conta é feita no log, onde os
#' componentes voltam a somar.
#' @noRd
.tr_series_forca <- function(x) {
  tr <- x$tendencia; s <- x$sazonal; r <- x$resto
  if (identical(x$tipo, "multiplicativa")) { tr <- log(tr); s <- log(s); r <- log(r) }
  f <- function(comp) {
    ok <- !is.na(comp) & !is.na(r)
    if (sum(ok) < 3L) return(NA_real_)
    max(0, 1 - stats::var(r[ok]) / stats::var(comp[ok] + r[ok]))
  }
  c(tendencia = f(tr), sazonal = f(s))
}

.tr_series_decomp_tabela <- function(x) {
  tab <- tibble::tibble(tempo = .tr_series_tempo(x$observado),
                        observado = as.numeric(x$observado), tendencia = as.numeric(x$tendencia),
                        sazonal = as.numeric(x$sazonal), resto = as.numeric(x$resto))
  if (!is.null(x$regressor)) tab$regressor <- as.numeric(x$regressor)
  tab
}

.TR_SERIES_MODELOS <- c("Arima", "ets", "HoltWinters")

#' `series/model`: o ajuste, que a previsão e os resíduos consomem.
#'
#' Guarda o OBJETO, e não a especificação: `forecast()` e `residuals()`
#' precisam dele inteiro, e reajustar a cada consumidor seria pagar o caro de
#' novo em cada fio. Os três modelos carregam a série junto (`$x`), então o
#' objeto restaurado noutro processo prevê sozinho.
#'
#' O preview é TEXTO — o `print` do modelo, que é o que um estatístico lê para
#' conferir ordem, coeficientes e AIC. Um gráfico de modelo não existe sem
#' escolher antes o que mostrar, e essa escolha é dos nós de gráfico.
#' @noRd
series_model_type <- function() {
  trama::tr_type(
    "series/model", version = 1L, label = "Modelo", color = "#f59e0b", ext = "rds",
    store = function(x, path) {
      if (!inherits(x, .TR_SERIES_MODELOS)) {
        .tr_series_abort("tr_series_error_not_a_model",
                         "O nó produziu um objeto '%s', não um modelo de série (%s).",
                         class(x)[[1]], paste(.TR_SERIES_MODELOS, collapse = ", "))
      }
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) .tr_series_modelo_resumo(x),
    preview = function(x, ctx) {
      txt <- paste(utils::capture.output(print(x)), collapse = "\n")
      trama::tr_preview("trama/text", data = list(text = txt))
    }
  )
}

#' Nome do modelo como a literatura escreve: "ARIMA(0,1,1)(0,1,1)[12]".
#' @noRd
.tr_series_metodo <- function(m) {
  if (inherits(m, "Arima")) {
    a <- m$arma  # p, q, P, Q, período, d, D
    s <- sprintf("ARIMA(%d,%d,%d)", a[[1]], a[[6]], a[[2]])
    if (a[[3]] + a[[7]] + a[[4]] > 0) s <- sprintf("%s(%d,%d,%d)[%d]", s, a[[3]], a[[7]], a[[4]], a[[5]])
    return(s)
  }
  if (inherits(m, "ets")) return(m$method)
  if (inherits(m, "HoltWinters")) {
    comp <- c(if (!isFALSE(m$beta)) "tendência", if (!isFALSE(m$gamma)) m$seasonal)
    return(sprintf("Holt-Winters (%s)", if (length(comp)) paste(comp, collapse = ", ") else "nível"))
  }
  class(m)[[1]]
}

.tr_series_modelo_resumo <- function(m) {
  aic <- tryCatch(stats::AIC(m), error = function(e) NA_real_)
  list(metodo = .tr_series_metodo(m),
       aic = if (is.finite(aic)) round(aic, 2) else NA_real_,
       observacoes = length(m$x),
       sigma = round(sqrt(mean(stats::residuals(m)^2, na.rm = TRUE)), 4))
}

#' `series/forecast`: o objeto `forecast`, com histórico e intervalos.
#' @noRd
series_forecast_type <- function() {
  trama::tr_type(
    "series/forecast", version = 1L, label = "Previsão", color = "#f472b6", ext = "rds",
    store = function(x, path) {
      if (!inherits(x, "forecast")) {
        .tr_series_abort("tr_series_error_not_a_forecast",
                         "O nó produziu um objeto '%s', não uma previsão.", class(x)[[1]])
      }
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) {
      f <- stats::frequency(x$mean)
      list(metodo = x$method, horizonte = length(x$mean),
           de = .tr_series_rotulo(stats::start(x$mean), f),
           ate = .tr_series_rotulo(stats::end(x$mean), f))
    },
    preview = function(x, ctx) {
      trama.view::tr_view_render(tr_series_plot_forecast(x, aspecto = "2:1"), ctx)
    }
  )
}

#' Previsão -> tabela. Os níveis são 80 e 95 sempre, porque é o que os nós
#' pedem; um objeto `forecast` de fora com outros níveis cai no erro de
#' coluna do próprio R, e não num NA silencioso.
#' @noRd
.tr_series_forecast_tabela <- function(x) {
  nivel <- function(m, l) as.numeric(m[, match(l, x$level)])
  tibble::tibble(tempo = .tr_series_tempo(x$mean), previsto = as.numeric(x$mean),
                 li_80 = nivel(x$lower, 80), ls_80 = nivel(x$upper, 80),
                 li_95 = nivel(x$lower, 95), ls_95 = nivel(x$upper, 95))
}

.tr_series_adapters <- function() {
  list(
    trama::tr_adapter("series/ts", "data/table", .tr_series_tabela),
    trama::tr_adapter("series/decomposition", "data/table", .tr_series_decomp_tabela),
    trama::tr_adapter("series/forecast", "data/table", .tr_series_forecast_tabela),
    # O ajuste de uma série carrega os componentes: é por este adaptador que o
    # `series/component` e o `series/plot_decomposition` os leem. Um
    # `models/fit` que não veio de série é recusado com classe.
    trama::tr_adapter("models/fit", "series/decomposition", .tr_series_fit_decomp)
  )
}
