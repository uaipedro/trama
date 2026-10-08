# Séries multivariadas: o tipo `series/mts`, o ajuste `series/var`, e os nós
# que montam e desmontam uma série múltipla.
#
# Por que um tipo novo, e não o `series/ts` aceitando `mts`: os trinta nós
# univariados contam com UMA série, e cada um teria de perguntar "qual
# coluna?". O `series/mts` é a fronteira explícita: só os blocos que pensam em
# várias séries ao mesmo tempo (VAR, cointegração, Granger) o recebem, e o
# `series/pick` devolve uma coluna ao mundo univariado.
#
# Os blocos de cada grupo (seleção, causalidade, cointegração, impulso-resposta,
# diagnóstico) moram em `mv_*.R`, cada um com a sua lista de nós e de docs;
# `trama_collection()` e `.tr_series_doc()` só os concatenam.

.TR_SERIES_COR_MV <- "#2dd4bf"

# ---- series/mts --------------------------------------------------------------

#' Confere uma série múltipla: `ts` numérico com 2+ colunas de nomes únicos.
#' @noRd
.tr_series_guard_mts <- function(x) {
  if (!stats::is.mts(x) || !is.numeric(x) || NCOL(x) < 2L) {
    .tr_series_abort("tr_series_error_not_multivariate",
                     paste0("O nó produziu um objeto '%s', não uma série múltipla (duas ou mais ",
                            "séries no mesmo calendário)."), class(x)[[1]])
  }
  nm <- colnames(x)
  # Nomes sintáticos do R: o `vars` e o `urca` os passam por `make.names`, e
  # um nome com espaço ou acento deixaria de casar nos blocos seguintes.
  if (is.null(nm) || anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm) ||
      !identical(nm, make.names(nm, unique = TRUE)) || any(grepl("[^A-Za-z0-9._]", nm))) {
    .tr_series_abort("tr_series_error_not_multivariate",
                     paste0("A série múltipla precisa de um nome único por coluna, só com letras sem ",
                            "acento, dígitos, '.' e '_' (começando por letra); chegou: %s."),
                     paste(nm %||% "(sem nomes)", collapse = ", "))
  }
  invisible(x)
}

#' Série múltipla -> tabela larga: `tempo` e uma coluna por série.
#' @noRd
.tr_series_mts_tabela <- function(x) {
  tab <- tibble::tibble(tempo = .tr_series_tempo(x[, 1]))
  for (nm in colnames(x)) tab[[nm]] <- as.numeric(x[, nm])
  tab
}

#' Uma faixa por série, eixo y livre: níveis diferentes não se espremem.
#' @export
tr_series_plot_mts <- function(serie, aspecto = "16:9", tema = "padrão", titulo = "",
                               rotulo_x = "", rotulo_y = "", legenda = "direita") {
  .tr_series_guard_mts(serie)
  larga <- .tr_series_mts_tabela(serie)
  longa <- do.call(rbind, lapply(colnames(serie), function(nm) {
    data.frame(tempo = larga$tempo, serie = nm, valor = larga[[nm]])
  }))
  longa$serie <- factor(longa$serie, levels = colnames(serie))
  p <- ggplot2::ggplot(longa, ggplot2::aes(x = .data[["tempo"]], y = .data[["valor"]])) +
    ggplot2::geom_line(linewidth = .6, colour = .TR_SERIES_COR, na.rm = TRUE) +
    ggplot2::facet_wrap(ggplot2::vars(.data[["serie"]]), ncol = 1L, scales = "free_y") +
    ggplot2::labs(x = "tempo", y = "valor")
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

series_mts_type <- function() {
  trama::tr_type(
    "series/mts", version = 1L, label = "Séries", color = .TR_SERIES_COR_MV, ext = "rds",
    tema = TRUE,
    store = function(x, path) {
      .tr_series_guard_mts(x)
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) {
      f <- stats::frequency(x)
      list(series = paste(colnames(x), collapse = ", "),
           inicio = .tr_series_rotulo(stats::start(x), f), fim = .tr_series_rotulo(stats::end(x), f),
           frequencia = f, observacoes = nrow(x), faltantes = sum(is.na(x)))
    },
    preview = function(x, ctx) {
      trama.view::tr_view_render(tr_series_plot_mts(x, aspecto = "4:3"), ctx)
    },
    report = tr_series_plot_mts
  )
}

# ---- series/var --------------------------------------------------------------

#' O ajuste multivariado: VAR em nível (`varest`) ou VECM (`vec2var`).
#'
#' Tipo próprio, e não `models/fit`: o contrato da `trama.models` supõe UMA
#' resposta (`tr_models_info()$resposta` é um texto), e o VAR tem uma equação
#' por série. Também não é `series/model`: os consumidores daquele tipo
#' (resíduos, previsão univariada, adaptador para `models/fit`) leem uma série
#' só. O VECM viaja na forma `vec2var` — é nela que previsão, impulso-resposta
#' e diagnósticos do `vars` funcionam — e leva junto o `cajorls` (os
#' coeficientes de ajuste e o vetor de cointegração) para o card.
#' @noRd
.tr_series_var <- function(ajuste, serie, tipo, vecm = NULL, nota = "") {
  structure(list(ajuste = ajuste, serie = serie, tipo = tipo, vecm = vecm, nota = nota),
            class = "tr_series_var")
}

.tr_series_guard_var <- function(x, no = NULL) {
  if (!inherits(x, "tr_series_var") || !inherits(x$ajuste, c("varest", "vec2var"))) {
    .tr_series_abort("tr_series_error_not_var",
                     "%s precisa de um ajuste VAR ou VECM (series/var, series/vecm); chegou '%s'.",
                     if (is.null(no)) "O tipo series/var" else sprintf("'%s'", no), class(x)[[1]])
  }
  invisible(x)
}

#' "VAR(2), constante" / "VECM(2), posto 1".
#' @noRd
.tr_series_var_rotulo <- function(x) {
  a <- x$ajuste
  if (identical(x$tipo, "VECM")) return(sprintf("VECM(%d), posto %d", a$p, x$vecm$posto))
  sprintf("VAR(%d), %s", a$p, c(const = "constante", trend = "tendência", both = "constante e tendência",
                                none = "sem determinístico")[[a$type]])
}

#' Resíduos como série múltipla, no calendário das observações que os têm.
#' @noRd
.tr_series_var_residuos <- function(x) {
  r <- stats::residuals(x$ajuste)
  f <- stats::frequency(x$serie)
  inicio <- stats::time(x$serie)[nrow(x$serie) - nrow(r) + 1L]
  stats::ts(r, start = inicio, frequency = f, names = colnames(x$serie))
}

#' Tabela dos coeficientes: uma linha por (equação, termo).
#' @noRd
.tr_series_var_coefs <- function(x) {
  a <- x$ajuste
  if (inherits(a, "varest")) {
    do.call(rbind, lapply(names(a$varresult), function(eq) {
      s <- stats::coef(summary(a$varresult[[eq]]))
      tibble::tibble(equacao = eq, termo = rownames(s), estimativa = s[, 1], erro_padrao = s[, 2],
                     t = s[, 3], p_valor = s[, 4])
    }))
  } else {
    s <- summary(x$vecm$rls$rlm)
    do.call(rbind, lapply(seq_along(s), function(i) {
      cf <- s[[i]]$coefficients
      tibble::tibble(equacao = colnames(x$serie)[[i]], termo = rownames(cf), estimativa = cf[, 1],
                     erro_padrao = cf[, 2], t = cf[, 3], p_valor = cf[, 4])
    }))
  }
}

series_var_type <- function() {
  trama::tr_type(
    "series/var", version = 1L, label = "Modelo multivariado", color = "#fb923c", ext = "rds",
    store = function(x, path) {
      .tr_series_guard_var(x)
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) {
      raizes <- if (inherits(x$ajuste, "varest")) max(vars::roots(x$ajuste)) else NA_real_
      list(metodo = .tr_series_var_rotulo(x), nota = x$nota, series = paste(colnames(x$serie), collapse = ", "),
           observacoes = nrow(x$serie),
           maior_raiz = if (is.finite(raizes)) round(raizes, 4) else NA_real_)
    },
    # O card é a tabela dos coeficientes (equação, termo, estimativa, t, p),
    # no mesmo formato do card de tabela da `data`; o rótulo do modelo e a
    # nota (critério, estabilidade) ficam no resumo.
    preview = function(x, ctx) {
      cf <- as.data.frame(.tr_series_var_coefs(x))
      for (col in c("estimativa", "erro_padrao")) cf[[col]] <- signif(cf[[col]], 4)
      cf$t <- round(cf$t, 2); cf$p_valor <- signif(cf$p_valor, 3)
      mostra <- utils::head(cf, 25L)
      trama::tr_preview("data/table", data = list(
        columns = as.list(names(mostra)),
        rows = lapply(seq_len(nrow(mostra)), function(i) as.list(mostra[i, , drop = TRUE])),
        nrow = nrow(cf), ncol = ncol(cf)))
    }
  )
}

# ---- Nós de base -------------------------------------------------------------

#' Junta séries univariadas numa múltipla.
#'
#' Os fios variádicos não trazem nome: o nome vem de `nomes` (separados por
#' vírgula, na ordem dos fios) ou, sem ele, do atributo `nome` que a série
#' carrega desde a origem (`series/example` com `EuStockMarkets$DAX`,
#' `series/from_table`) — e, faltando os dois, `serie_1`, `serie_2`...
#'
#' Frequências diferentes são recusadas: alinhar mensal com trimestral pede
#' agregação, e é o `series/aggregate` que a faz, explicitamente. Janelas
#' diferentes, não: o recorte é a interseção, e a nota diz quanto se perdeu.
#' @export
tr_series_join <- function(series, nomes = "") {
  if (!is.list(series) || stats::is.ts(series)) series <- list(series)
  if (length(series) < 2L) {
    .tr_series_abort("tr_series_error_too_short",
                     "'series/join' precisa de duas ou mais séries ligadas; chegaram %d.", length(series))
  }
  for (s in series) .tr_series_guard_ts(s)
  fs <- vapply(series, stats::frequency, numeric(1))
  if (any(abs(fs - fs[[1]]) > 1e-8)) {
    .tr_series_abort("tr_series_error_frequency_mismatch",
                     paste0("'series/join': as séries têm frequências diferentes (%s). Agregue a mais ",
                            "fina antes, com 'series/aggregate'."), paste(fs, collapse = ", "))
  }
  nm <- trimws(strsplit(as.character(nomes %||% "")[[1]], ",", fixed = TRUE)[[1]])
  nm <- nm[nzchar(nm)]
  if (length(nm) && length(nm) != length(series)) {
    .tr_series_abort("tr_series_error_bad_option",
                     "Param 'nomes': %d nome(s) para %d série(s) ligadas.", length(nm), length(series))
  }
  if (!length(nm)) {
    nm <- vapply(seq_along(series), function(i) {
      a <- attr(series[[i]], "nome")
      if (is.character(a) && length(a) == 1L && nzchar(a)) a else paste0("serie_", i)
    }, character(1))
  }
  # Nomes que o `vars`/`urca` aceitam: sem acento, espaço vira `_`.
  limpo <- iconv(nm, to = "ASCII//TRANSLIT", sub = "")
  limpo <- gsub("[^A-Za-z0-9._]+", "_", limpo)
  limpo <- make.names(limpo, unique = TRUE)
  renomeadas <- nm != limpo
  nm <- limpo
  x <- do.call(stats::ts.intersect, unname(series))
  if (is.null(x) || NROW(x) < 1L) {
    .tr_series_abort("tr_series_error_no_overlap", "'series/join': as séries não têm nenhum período em comum.")
  }
  x <- stats::ts(unclass(x), start = stats::start(x), frequency = stats::frequency(x), names = nm)
  perdidos <- max(vapply(series, length, integer(1))) - nrow(x)
  notas <- c(if (perdidos > 0L) sprintf("recortada no período comum: %d observação(ões) a menos que a série mais longa", perdidos),
             if (any(renomeadas)) sprintf("nomes ajustados: %s", paste(nm[renomeadas], collapse = ", ")))
  if (length(notas)) attr(x, "nota") <- paste(notas, collapse = "; ")
  x
}

#' Uma coluna da série múltipla, de volta ao mundo univariado.
#' @export
tr_series_pick <- function(series, variavel = "") {
  .tr_series_guard_mts(series)
  v <- .tr_series_obrigatorio(variavel, "variavel")
  if (!v %in% colnames(series)) .tr_series_option("variavel", v, colnames(series))
  x <- .tr_series_uni(series[, v])
  attr(x, "nome") <- v
  x
}

#' Tabela larga -> série múltipla: uma coluna por série, um tempo comum.
#'
#' Reaproveita o `series/from_table` coluna a coluna, para que as regras de
#' calendário (tempo repetido, buraco, início) sejam as mesmas, ditas com as
#' mesmas mensagens.
#' @export
tr_series_from_table_mts <- function(dados, valores = character(), tempo = "", frequencia = 12L, inicio = "") {
  valores <- as.character(unlist(valores %||% character()))
  valores <- valores[nzchar(valores)]
  if (length(valores) < 2L) {
    .tr_series_abort("tr_series_error_blank_param",
                     "Param 'valores': escolha duas ou mais colunas numéricas (uma por série).")
  }
  series <- lapply(valores, function(v) {
    tr_series_from_table(dados, valor = v, tempo = tempo, frequencia = frequencia, inicio = inicio)
  })
  tr_series_join(series, nomes = paste(valores, collapse = ","))
}

#' VAR(p) por mínimos quadrados equação a equação (Lütkepohl 2005, cap. 3).
#'
#' `defasagens = 0` escolhe p pelo critério em `criterio` (até `max_defasagens`),
#' a mesma conta do `series/var_select`; com p fixo, `criterio` é ignorado.
#' Sazonais (dummies centradas do `vars`) só em série com ciclo.
#' @export
tr_series_var <- function(series, defasagens = 0L, max_defasagens = 8L, criterio = "AIC",
                          deterministico = "constante", sazonal = FALSE) {
  .tr_series_guard_mts(series)
  .tr_series_sem_na(series, "series/var")
  criterio <- .tr_series_enum(criterio, c("AIC", "HQ", "SC", "FPE"), "criterio")
  det <- .tr_series_var_det(deterministico)
  p <- .tr_series_int(defasagens, "defasagens", min = 0, max = 50)
  pmax <- .tr_series_int(max_defasagens, "max_defasagens", min = 1, max = 50)
  saz <- if (isTRUE(sazonal)) {
    f <- stats::frequency(series)
    if (f <= 1) .tr_series_abort("tr_series_error_no_season", "'series/var': sazonais pedem série com ciclo (frequência > 1).")
    as.integer(f)
  }
  k <- ncol(series)
  .tr_series_minimo(series[, 1], k * max(p, if (p == 0L) pmax else p) + 10L, "series/var",
                    "estimar as equações com essas defasagens")
  # `do.call` com VALORES: o `vars` guarda a chamada e o bootstrap (irf) a
  # refaz com `update()`; com símbolos, `det` viraria `stats::det` lá dentro.
  args <- list(y = series, type = det, season = saz)
  args <- c(args, if (p == 0L) list(lag.max = pmax, ic = criterio) else list(p = p))
  aj <- .tr_series_ajustar(do.call(vars::VAR, args), "series/var")
  nota <- if (p == 0L) sprintf("p = %d escolhido por %s (até %d)", aj$p, criterio, pmax) else ""
  if (max(vars::roots(aj)) >= 1) nota <- paste(c(nota[nzchar(nota)], "VAR instável: há raiz com módulo >= 1 (diferencie ou use VECM)"), collapse = "; ")
  .tr_series_var(aj, series, "VAR", nota = nota)
}

.TR_SERIES_DET <- c(constante = "const", "tendência" = "trend", ambos = "both", nenhum = "none")

.tr_series_var_det <- function(deterministico) {
  d <- .tr_series_enum(deterministico, names(.TR_SERIES_DET), "deterministico")
  .TR_SERIES_DET[[d]]
}

# ---- Previsão multivariada ---------------------------------------------------

#' Previsão de VAR/VECM no formato `mforecast` do `forecast`.
#'
#' Pelo `predict()` do `vars` (que serve aos dois — o `forecast::forecast()`
#' não aceita `vec2var`), duas vezes, a 80 e 95%: o intervalo de cada série é o
#' de Lütkepohl (2005, sec. 3.5), com a variância do erro de previsão acumulada
#' pelos coeficientes MA. Cada série vira um objeto `forecast` comum, e o resto
#' da coleção (gráfico, tabela, acurácia) os lê um a um.
#' @noRd
.tr_series_var_prever <- function(x, h) {
  .tr_series_guard_var(x, "series/forecast")
  p80 <- stats::predict(x$ajuste, n.ahead = h, ci = 0.80)$fcst
  p95 <- stats::predict(x$ajuste, n.ahead = h, ci = 0.95)$fcst
  f <- stats::frequency(x$serie)
  ini <- stats::tsp(x$serie)[[2]] + 1 / f
  metodo <- .tr_series_var_rotulo(x)
  # Resíduos no calendário inteiro (NA nas p primeiras, que não têm passado):
  # é deles que a acurácia de treino sai.
  res <- stats::residuals(x$ajuste)
  pad <- nrow(x$serie) - nrow(res)
  fc <- lapply(stats::setNames(colnames(x$serie), colnames(x$serie)), function(nm) {
    mk <- function(v) stats::ts(v, start = ini, frequency = f)
    lo <- cbind(p80[[nm]][, "lower"], p95[[nm]][, "lower"]); up <- cbind(p80[[nm]][, "upper"], p95[[nm]][, "upper"])
    colnames(lo) <- colnames(up) <- c("80%", "95%")
    obs <- .tr_series_uni(x$serie[, nm])
    r <- stats::ts(c(rep(NA_real_, pad), res[, match(nm, colnames(x$serie))]),
                   start = stats::start(obs), frequency = f)
    structure(list(method = metodo, level = c(80, 95), mean = mk(p95[[nm]][, "fcst"]),
                   lower = mk(lo), upper = mk(up), x = obs, series = nm,
                   residuals = r, fitted = obs - r),
              class = "forecast")
  })
  structure(list(forecast = fc, method = rep(metodo, length(fc)), x = x$serie), class = "mforecast")
}
