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
#' @noRd
.tr_series_decomp <- function(observado, tendencia, sazonal, resto, tipo, metodo) {
  structure(list(observado = observado, tendencia = tendencia, sazonal = sazonal,
                 resto = resto, tipo = tipo, metodo = metodo),
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
  tibble::tibble(tempo = .tr_series_tempo(x$observado),
                 observado = as.numeric(x$observado), tendencia = as.numeric(x$tendencia),
                 sazonal = as.numeric(x$sazonal), resto = as.numeric(x$resto))
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

#' `series/regression`: o ajuste paramétrico dos componentes.
#'
#' Guarda o `lm` inteiro — os testes parciais reajustam a partir dele — mais a
#' série e os componentes já separados, que é o que os adaptadores servem sem
#' recalcular nada.
#'
#' O preview é TEXTO, pelo mesmo motivo de `series/model`: o que se lê num
#' ajuste é a tabela de coeficientes com as estrelas, o R² e o F. O gráfico dos
#' componentes está a um fio de distância, pelo adaptador para a decomposição.
#' @noRd
series_regression_type <- function() {
  trama::tr_type(
    "series/regression", version = 1L, label = "Regressão", color = "#c084fc", ext = "rds",
    store = function(x, path) {
      if (!inherits(x, "tr_series_reg")) {
        .tr_series_abort("tr_series_error_not_a_regression",
                         "O nó produziu um objeto '%s', não uma regressão de série.", class(x)[[1]])
      }
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) {
      s <- summary(x$ajuste)
      fs <- s$fstatistic
      list(grau = x$grau, sazonalidade = x$sazonalidade,
           r2_ajustado = round(s$adj.r.squared, 3),
           p_valor_f = signif(stats::pf(fs[[1]], fs[[2]], fs[[3]], lower.tail = FALSE), 3),
           observacoes = length(x$serie))
    },
    preview = function(x, ctx) {
      txt <- paste(utils::capture.output(summary(x$ajuste)), collapse = "\n")
      trama::tr_preview("trama/text", data = list(text = txt))
    }
  )
}

#' Regressão -> decomposição. É o adaptador que dá de graça o
#' `series/component`, o `series/plot_decomposition` e a força do card: os
#' componentes já vêm separados do ajuste, e `metodo` é campo que o tipo da
#' decomposição já tinha.
#' @noRd
.tr_series_reg_decomp <- function(x) {
  .tr_series_decomp(x$serie, x$tendencia, x$sazonal, x$resto, "aditiva", "regressão")
}

#' Regressão -> tabela de coeficientes.
#' @noRd
.tr_series_reg_tabela <- function(x) {
  s <- stats::coef(summary(x$ajuste))
  tibble::tibble(termo = rownames(s), estimativa = as.numeric(s[, 1]),
                 erro_padrao = as.numeric(s[, 2]), estatistica_t = as.numeric(s[, 3]),
                 p_valor = as.numeric(s[, 4]))
}

#' `series/test`: o registro de UM teste de hipótese.
#'
#' Tipo próprio, e não `data/table`, por uma razão de front: o renderer é
#' resolvido por id, e registrar um renderer sob `data/table` mudaria toda
#' tabela do app. O tipo próprio dá ao teste um card que é dele, e o adaptador
#' devolve a tabela a quem quer exportar.
#'
#' Sem `summary`: o runtime acrescenta uma aba `resumo` automática a todo tipo
#' que declare um, e ela repetiria a vista `detalhe`.
#' @noRd
series_test_type <- function() {
  trama::tr_type(
    "series/test", version = 1L, label = "Teste", color = "#ef4444", ext = "rds",
    store = function(x, path) {
      .tr_series_guard_test(x)
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    preview = function(x, ctx) {
      trama::tr_preview("series/test", data = list(
        teste = x$teste, h0 = x$h0,
        estatistica = x$estatistica, rotulo_estat = x$rotulo_estat,
        p_valor = if (is.na(x$p_valor)) NULL else x$p_valor,
        # Lista NOMEADA: vira objeto no JSON, e o front indexa por "5%".
        criticos = if (is.null(x$criticos)) NULL else as.list(x$criticos),
        sentido = x$sentido, decisao_5 = x$decisao_5, conclusao = x$conclusao,
        nota = x$nota, fonte = x$fonte,
        extra = if (is.null(x$extra)) NULL else as.list(x$extra)
      ))
    }
  )
}

.TR_SERIES_CAMPOS_TESTE <- c("teste", "h0", "estatistica", "rotulo_estat", "p_valor",
                             "sentido", "decisao_5", "conclusao", "nota", "fonte")

#' @noRd
.tr_series_guard_test <- function(x) {
  falta <- setdiff(.TR_SERIES_CAMPOS_TESTE, names(x))
  if (!inherits(x, "tr_series_test") || length(falta)) {
    .tr_series_abort("tr_series_error_not_a_test",
                     "O nó produziu um objeto '%s', não o resultado de um teste%s.",
                     class(x)[[1]],
                     if (length(falta)) sprintf(" (faltam: %s)", paste(falta, collapse = ", ")) else "")
  }
  invisible(x)
}

#' Teste -> tabela: UMA linha, nas colunas de sempre mais a fonte.
#'
#' `valor_critico_5` sai da tabela de críticos quando ela existe, para que a
#' coluna continue significando o que significava antes — é o que faz um
#' `data/bind_rows` de testes diferentes continuar sendo um relatório legível.
#' @noRd
.tr_series_teste_tabela <- function(x) {
  cv <- if (is.null(x$criticos)) NA_real_ else unname(x$criticos[["5%"]])
  base <- tibble::tibble(
    teste = x$teste, h0 = x$h0, estatistica = x$estatistica,
    p_valor = x$p_valor, valor_critico_5 = as.numeric(cv),
    decisao_5 = x$decisao_5, conclusao = x$conclusao,
    nota = x$nota, fonte = x$fonte)
  # `extra` vira coluna SÓ quando existe: uma coluna vazia em todo teste seria
  # ruído em todo relatório.
  # O `as_tibble` de fora não é enfeite: o `cbind` despacha pro
  # `cbind.data.frame` e devolveria um data.frame pelado, fazendo a classe de
  # saída do adaptador depender da ENTRADA. Um adaptador só, com duas caras,
  # vira catorze quando os outros blocos chegarem.
  if (length(x$extra)) base <- tibble::as_tibble(cbind(base, tibble::as_tibble(x$extra)))
  base
}

.tr_series_adapters <- function() {
  list(
    trama::tr_adapter("series/ts", "data/table", .tr_series_tabela),
    trama::tr_adapter("series/decomposition", "data/table", .tr_series_decomp_tabela),
    trama::tr_adapter("series/forecast", "data/table", .tr_series_forecast_tabela),
    trama::tr_adapter("series/regression", "data/table", .tr_series_reg_tabela),
    trama::tr_adapter("series/regression", "series/decomposition", .tr_series_reg_decomp),
    trama::tr_adapter("series/test", "data/table", .tr_series_teste_tabela)
  )
}
