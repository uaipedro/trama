# Intervenções: declaradas ANTES do modelo, estimadas DENTRO dele.
#
# Uma intervenção é um evento numa data (uma lei, uma greve, um dado errado)
# que o modelo precisa saber para não confundir com a dinâmica da série. Até a
# versão 1, o bloco `series/intervencao` montava o regressor E ajustava um
# ARIMA próprio, com as seis ordens copiadas do `series/arima`, e devolvia só
# uma tabela: não ia à previsão, aos resíduos nem aos blocos de modelo, e só
# cabia uma intervenção. Agora o bloco só DECLARA: carimba a série com o
# evento, e quantos blocos se encadearem, tantas intervenções. O modelo que
# chega depois lê o carimbo e estima os efeitos junto com o resto.
#
# Por que carimbo e não uma série-regressor: o tipo `series/ts` é univariado,
# e várias intervenções pediriam várias colunas. Por que não estimar no bloco:
# a intervenção INOVACIONAL (Fox 1972; Chen & Liu 1993) entra pelo filtro do
# próprio modelo, ψ(B) = θ(B)/φ(B), e não existe antes dele. A aditiva, o
# degrau e a rampa são regressores comuns, mas é o modelo que sabe montá-los
# no horizonte da previsão.
#
# O carimbo vale só para a série exata que o recebeu (valores e calendário),
# como o `card_ajuste` de `type.R`: `log(x)` herda atributos, e uma
# intervenção declarada antes de uma transformação mudaria de sentido calada.
# Carimbo vencido é ERRO no modelo, e não intervenção esquecida.

.TR_SERIES_INTERV_TIPOS <- c("pulso", "degrau", "rampa", "inovacional")

#' Declara uma intervenção na série, para o modelo seguinte estimar.
#'
#' Devolve a mesma série, com a intervenção somada às que já vinham
#' declaradas. `pulso` é o outlier aditivo (AO), `degrau` a mudança de nível
#' (LS), `rampa` a mudança de inclinação e `inovacional` o choque que passa
#' pela dinâmica do modelo (IO). Com `dinamica = "gradual"` (pulso e degrau),
#' o efeito entra por ω/(1 − δB), de Box & Tiao (1975).
#' @export
tr_series_intervencao <- function(serie, data, tipo = "degrau", dinamica = "imediata") {
  tipo <- .tr_series_enum(tipo, .TR_SERIES_INTERV_TIPOS, "tipo")
  dinamica <- .tr_series_enum(dinamica, c("imediata", "gradual"), "dinamica")
  if (dinamica == "gradual" && !tipo %in% c("pulso", "degrau")) {
    .tr_series_abort("tr_series_error_bad_option",
                     paste0("Param 'dinamica': a resposta gradual ω/(1 − δB) de Box & Tiao vale para ",
                            "pulso e degrau; '%s' só com dinâmica imediata."), tipo)
  }
  .tr_series_sem_na(serie, "series/intervencao")
  f <- stats::frequency(serie)
  v <- .tr_series_periodo(.tr_series_obrigatorio(data, "data"), "data", f)
  tt <- as.numeric(stats::time(serie))
  i0 <- which(abs(tt - .tr_series_pos(v, f)) < 1e-6 / f)
  if (length(i0) != 1L || i0 < 2L) {
    .tr_series_abort("tr_series_error_bad_period",
                     paste0("Param 'data': '%s' tem de ser um período DA série, depois da primeira ",
                            "observação (%s a %s) — sem observação antes, não há nível de referência."),
                     data, .tr_series_rotulo(stats::start(serie), f), .tr_series_rotulo(stats::end(serie), f))
  }
  ja <- .tr_series_intervencoes(serie, "series/intervencao")
  nova <- list(tipo = tipo, dinamica = dinamica, indice = i0, rotulo = .tr_series_rotulo_em(serie, i0))
  nova$termo <- .tr_series_interv_termo(nova)
  if (nova$termo %in% vapply(ja, `[[`, "", "termo")) {
    .tr_series_abort("tr_series_error_bad_option",
                     "'series/intervencao': a série já tem %s em %s declarado antes.", tipo, nova$rotulo)
  }
  todas <- c(ja, list(nova))
  if (sum(vapply(todas, function(s) s$dinamica == "gradual", NA)) > 1L) {
    .tr_series_abort("tr_series_error_bad_option",
                     paste0("'series/intervencao': só uma intervenção gradual por modelo — cada uma traz ",
                            "um δ, e mais de um δ não se separa bem numa série só."))
  }
  # O card do ajuste de cima (os coeficientes de um `series/deseasonalize`,
  # por exemplo) vale para a série de valores idênticos, e esta é idêntica:
  # sem tirá-lo, o card da intervenção mostraria coeficientes que não são dela.
  attr(serie, "card_ajuste") <- NULL
  attr(serie, "intervencoes") <- list(lista = todas, valores = as.numeric(serie), tsp = stats::tsp(serie))
  serie
}

#' Nome do coeficiente: tipo e data, como "degrau_1983_fev".
#' @noRd
.tr_series_interv_termo <- function(s) {
  paste0(s$tipo, if (s$dinamica == "gradual") "_gradual", "_",
         gsub("[^[:alnum:]]+", "_", s$rotulo))
}

#' As intervenções declaradas na série (lista vazia se nenhuma).
#'
#' Carimbo que não confere com a série é erro: alguém transformou, recortou
#' ou diferenciou a série DEPOIS de declarar, e a data ou o sentido do efeito
#' já não são os mesmos.
#' @noRd
.tr_series_intervencoes <- function(serie, no) {
  c <- attr(serie, "intervencoes")
  if (is.null(c)) return(list())
  if (!identical(c$tsp, stats::tsp(serie)) || !identical(c$valores, as.numeric(serie))) {
    .tr_series_abort("tr_series_error_bad_option",
                     paste0("'%s': a série tem intervenções declaradas antes de uma transformação, um ",
                            "recorte ou uma diferença — o efeito delas já não é o mesmo. Ponha os blocos ",
                            "'Intervenção' logo antes do modelo."), no)
  }
  c$lista
}

#' Pesos ψ do modelo inteiro, com as diferenças: o eco de um choque.
#'
#' É o polinômio que o `tsoutliers::coefs2poly()` monta — AR do ARMA
#' (`model$phi`, já com a parte sazonal expandida) vezes (1 − B)^d (1 − B^s)^D,
#' e o MA de `model$theta` — e os pesos de `ARMAtoMA()`. Com as diferenças
#' dentro, o regressor diferenciado pelo `Arima` volta a ser o ψ do ARMA.
#' @noRd
.tr_series_psi <- function(fit, n) {
  a <- fit$arma  # p, q, P, Q, s, d, D
  ar <- c(1, -fit$model$phi)
  mult <- function(p, q) {
    out <- numeric(length(p) + length(q) - 1L)
    for (i in seq_along(p)) out[i - 1L + seq_along(q)] <- out[i - 1L + seq_along(q)] + p[[i]] * q
    out
  }
  for (k in seq_len(a[[6]])) ar <- mult(ar, c(1, -1))
  for (k in seq_len(a[[7]])) ar <- mult(ar, c(1, rep(0, a[[5]] - 1L), -1))
  ma <- fit$model$theta
  if (n < 2L) return(1)
  c(1, stats::ARMAtoMA(ar = -ar[-1], ma = if (length(ma)) ma else numeric(), lag.max = n - 1L))
}

#' A matriz de regressores das intervenções, `n_total` linhas.
#'
#' `n_total` pode passar do fim da série: é assim que a previsão estende cada
#' efeito (o pulso volta a zero, o degrau fica em 1, a rampa segue subindo, o
#' gradual continua decaindo e o inovacional segue os pesos ψ).
#' @noRd
.tr_series_interv_matriz <- function(lista, n_total, fit = NULL, delta = NULL) {
  idx <- seq_len(n_total)
  cols <- lapply(lista, function(s) {
    i0 <- s$indice
    base <- switch(s$tipo,
      pulso = as.numeric(idx == i0),
      degrau = as.numeric(idx >= i0),
      rampa = pmax(0, idx - i0 + 1),
      inovacional = {
        if (is.null(fit)) {
          as.numeric(idx == i0)
        } else {
          psi <- .tr_series_psi(fit, n_total)
          ifelse(idx >= i0, psi[pmax(1L, idx - i0 + 1L)], 0)
        }
      })
    if (s$dinamica == "gradual") base <- as.numeric(stats::filter(base, delta, method = "recursive"))
    base
  })
  m <- do.call(cbind, cols)
  colnames(m) <- vapply(lista, `[[`, "", "termo")
  m
}

#' ARIMA com as intervenções declaradas na série.
#'
#' Imediatas (pulso, degrau, rampa): regressores fixos no `xreg`, estimados
#' por máxima verossimilhança junto com o ARMA — a forma de ordem zero de Box
#' & Tiao (1975). Gradual: δ perfilado (`optimize`), erro-padrão pela hessiana
#' da verossimilhança completa (o δ entra no AIC). Inovacional: o regressor é
#' ψ(B) aplicado ao pulso, com os ψ do próprio ajuste; itera-se até o ψ usado
#' no regressor ser o do modelo que ele produz (ponto fixo). É o regressor que
#' o `tsoutliers` monta (Chen & Liu 1993, eq. 2.2), aqui com os parâmetros
#' finais em vez dos da etapa anterior; o erro-padrão de ω trata ψ como
#' conhecido.
#' @noRd
.tr_series_arima_interv <- function(serie, lista, ordem, sazo, constante) {
  no <- "series/arima"
  n <- length(serie)
  ajusta <- function(X, fixed = NULL) {
    forecast::Arima(serie, order = ordem, seasonal = sazo, xreg = X,
                    include.constant = isTRUE(constante), fixed = fixed,
                    transform.pars = is.null(fixed))
  }
  grad <- vapply(lista, function(s) s$dinamica == "gradual", NA)
  inov <- vapply(lista, function(s) s$tipo == "inovacional", NA)
  if (any(grad) && any(inov)) {
    .tr_series_abort("tr_series_error_bad_option",
                     paste0("'%s': intervenção gradual e inovacional no mesmo modelo não são estimadas ",
                            "juntas aqui. Declare uma das duas, ou troque a gradual por um pulso ou degrau."), no)
  }
  extra <- NULL
  if (any(inov)) {
    fit <- .tr_series_ajustar(ajusta(.tr_series_interv_matriz(lista, n)), no)
    # Para onde der: |Δcoef| < 1e-8. Em modelos com muitos termos o `optim`
    # oscila no 3º ou 4º algarismo de um passo para o outro e o ponto fixo
    # exato não chega; aí aceita-se a variação relativa < 1e-3, que é o ruído
    # do próprio otimizador.
    ok <- FALSE
    for (it in seq_len(100L)) {
      antes <- stats::coef(fit)
      fit <- .tr_series_ajustar(ajusta(.tr_series_interv_matriz(lista, n, fit = fit)), no)
      dif <- abs(stats::coef(fit) - antes)
      if (max(dif) < 1e-8) { ok <- TRUE; break }
    }
    if (!ok && max(dif / pmax(abs(antes), 1e-2)) < 1e-3) ok <- TRUE
    if (!ok) {
      .tr_series_abort("tr_series_error_fit",
                       paste0("'%s': o ajuste com intervenção inovacional não se estabilizou em 100 passos; ",
                              "simplifique a ordem ou declare a intervenção como pulso."), no)
    }
  } else if (any(grad)) {
    mat <- function(dl) .tr_series_interv_matriz(lista, n, delta = dl)
    o <- .tr_series_ajustar(
      stats::optimize(function(dl) ajusta(mat(dl))$loglik, c(-0.999, 0.999), maximum = TRUE, tol = 1e-8), no)
    delta <- o$maximum
    if (abs(delta) > 0.99) {
      .tr_series_abort("tr_series_error_fit",
                       paste0("'%s': o δ da intervenção gradual foi para a borda (%.3f); a resposta não ",
                              "se estabiliza — no degrau, experimente a rampa; no pulso, o degrau."), no, delta)
    }
    fit <- .tr_series_ajustar(ajusta(mat(delta)), no)
    th <- c(stats::coef(fit), delta = delta)
    k <- length(th)
    nll <- function(t) -ajusta(mat(t[[k]]), fixed = unname(t[-k]))$loglik
    H <- tryCatch(stats::optimHess(th, nll), error = function(e) NULL)
    V <- if (is.null(H)) NULL else tryCatch(solve(H), error = function(e) NULL)
    if (is.null(V) || any(!is.finite(diag(V))) || any(diag(V) <= 0)) {
      .tr_series_abort("tr_series_error_fit",
                       "'%s': a matriz de covariância com o δ saiu singular; simplifique a ordem do ARIMA.", no)
    }
    dimnames(V) <- list(names(th), names(th))
    extra <- list(delta = delta, vcov = V, termo = lista[[which(grad)]]$termo)
    # δ é um parâmetro estimado: entra na contagem do AIC, do AICc e do BIC.
    np <- length(stats::coef(fit)[fit$mask]) + 2L
    fit$aic <- -2 * fit$loglik + 2 * np
    fit$aicc <- fit$aic + 2 * np * (np + 1) / (fit$nobs - np - 1)
    fit$bic <- fit$aic + np * (log(fit$nobs) - 2)
  } else {
    fit <- .tr_series_ajustar(ajusta(.tr_series_interv_matriz(lista, n)), no)
  }
  fit$tr_intervencoes <- list(lista = lista, gradual = extra)
  fit
}

#' Regressores das intervenções no horizonte da previsão (NULL se não há).
#' @noRd
.tr_series_interv_futuro <- function(modelo, h) {
  iv <- modelo$tr_intervencoes
  if (is.null(iv)) return(NULL)
  n <- length(modelo$x)
  m <- .tr_series_interv_matriz(iv$lista, n + h, fit = modelo, delta = iv$gradual$delta)
  m[n + seq_len(h), , drop = FALSE]
}

#' As linhas do δ e do efeito de longo prazo, para a tabela de coeficientes.
#'
#' Com resposta gradual, ω é o efeito do PRIMEIRO período; no degrau, o de
#' longo prazo é ω/(1 − δ), com erro-padrão pelo método delta sobre a
#' covariância completa. Os erros-padrão de todos os termos saem dessa
#' covariância, e não do `var.coef` condicional em δ.
#' @noRd
.tr_series_interv_gradual_coefs <- function(aj) {
  g <- aj$tr_intervencoes$gradual
  V <- g$vcov
  est <- c(stats::coef(aj)[aj$mask], delta = g$delta)
  ep <- sqrt(diag(V))[names(est)]
  lp <- NULL
  s <- Filter(function(s) s$termo == g$termo, aj$tr_intervencoes$lista)[[1]]
  if (s$tipo == "degrau") {
    w <- est[[g$termo]]
    v <- w / (1 - g$delta)
    gr <- c(1 / (1 - g$delta), w / (1 - g$delta)^2)
    ii <- c(g$termo, "delta")
    lp <- c(efeito_longo_prazo = v)
    ep <- c(ep, efeito_longo_prazo = sqrt(drop(t(gr) %*% V[ii, ii] %*% gr)))
  }
  list(est = c(est, lp), ep = ep)
}

#' Procura intervenções que ninguém declarou: o método de Chen & Liu (1993).
#'
#' Ajusta um ARIMA (o do `modelo` ligado ou um automático), calcula para cada
#' instante e cada tipo a estatística t do efeito, marca os que passam do
#' valor crítico, reajusta e descarta os que deixaram de ser significativos.
#' A conta é a do `tsoutliers::tso()`; a coleção traduz os tipos e o tempo.
#' @export
tr_series_detect_interventions <- function(serie, modelo = NULL, pulso = TRUE, degrau = TRUE,
                                           temporaria = TRUE, inovacional = FALSE, valor_critico = 0) {
  no <- "series/detect_interventions"
  tipos <- c(AO = isTRUE(pulso), LS = isTRUE(degrau), TC = isTRUE(temporaria), IO = isTRUE(inovacional))
  if (!any(tipos)) {
    .tr_series_abort("tr_series_error_bad_option", "'%s': ligue ao menos um tipo de intervenção.", no)
  }
  vc <- suppressWarnings(as.numeric(valor_critico))
  if (length(vc) != 1L || is.na(vc) || vc < 0) {
    .tr_series_abort("tr_series_error_bad_option",
                     "Param 'valor_critico': um número ≥ 0 (0 = a regra do tsoutliers pelo tamanho da série).")
  }
  .tr_series_sem_na(serie, no)
  .tr_series_minimo(serie, 12L, no, "a detecção de intervenções")
  args <- list(y = serie, types = names(tipos)[tipos])
  if (vc > 0) args$cval <- vc
  if (!is.null(modelo)) {
    if (!inherits(modelo, "Arima")) {
      .tr_series_abort("tr_series_error_not_arima",
                       "'%s': o modelo ligado tem de ser um ARIMA (chegou '%s').", no, class(modelo)[[1]])
    }
    a <- modelo$arma
    args$tsmethod <- "arima"
    args$args.tsmethod <- list(order = a[c(1, 6, 2)],
                               seasonal = list(order = a[c(3, 7, 4)], period = a[[5]]),
                               include.mean = "intercept" %in% names(stats::coef(modelo)))
  }
  r <- .tr_series_ajustar(suppressWarnings(do.call(tsoutliers::tso, args)), no)
  o <- r$outliers
  traduz <- c(AO = "pulso", LS = "degrau", TC = "temporaria", IO = "inovacional")
  f <- stats::frequency(serie)
  if (!NROW(o)) {
    return(tibble::tibble(data = character(), tipo = character(), sigla = character(),
                          indice = integer(), efeito = numeric(), t = numeric()))
  }
  o <- o[order(o$ind), , drop = FALSE]
  tibble::tibble(
    data = vapply(o$ind, function(i) .tr_series_rotulo_em(serie, i), ""),
    tipo = unname(traduz[as.character(o$type)]),
    sigla = as.character(o$type),
    indice = as.integer(o$ind),
    efeito = as.numeric(o$coefhat),
    t = as.numeric(o$tstat))
}

#' O texto do card do modelo: o `print` do ajuste e, com intervenções, o que
#' o `print` não mostra (o δ da gradual, com o erro-padrão da covariância
#' completa, e a leitura de cada termo).
#' @noRd
.tr_series_modelo_texto <- function(m) {
  txt <- utils::capture.output(print(m))
  iv <- m$tr_intervencoes
  if (!is.null(iv)) {
    desc <- c(pulso = "pulso (outlier aditivo)", degrau = "degrau (mudança de nível)",
              rampa = "rampa (mudança de inclinação)", inovacional = "inovacional (choque pela dinâmica)")
    txt <- c(txt, "", "Intervenções:",
             vapply(iv$lista, function(s) sprintf("  %s: %s%s em %s", s$termo, desc[[s$tipo]],
                                                  if (s$dinamica == "gradual") ", gradual" else "", s$rotulo), ""))
    if (!is.null(iv$gradual)) {
      g <- .tr_series_interv_gradual_coefs(m)
      txt <- c(txt, sprintf("  delta = %.4f (EP %.4f)", g$est[["delta"]], g$ep[["delta"]]))
      if ("efeito_longo_prazo" %in% names(g$est)) {
        txt <- c(txt, sprintf("  efeito de longo prazo ω/(1 − δ) = %.4f (EP %.4f)",
                              g$est[["efeito_longo_prazo"]], g$ep[["efeito_longo_prazo"]]))
      }
      txt <- c(txt, "  (os EP dos coeficientes acima são condicionais em δ; os completos estão em 'models/coefficients')")
    }
  }
  paste(txt, collapse = "\n")
}

#' Recusa intervenções declaradas num modelo que não as estima.
#'
#' Sem isso, ETS, Holt-Winters e a regressão as ignorariam caladas, e o
#' usuário leria um ajuste sem o efeito que achou que tinha declarado.
#' @noRd
.tr_series_sem_intervencao <- function(serie, no) {
  if (length(.tr_series_intervencoes(serie, no))) {
    .tr_series_abort("tr_series_error_bad_option",
                     paste0("'%s' não estima intervenções, e a série chega com intervenções declaradas. ",
                            "Ligue a série num 'series/arima', ou tire os blocos 'Intervenção' do caminho."), no)
  }
  invisible(serie)
}
