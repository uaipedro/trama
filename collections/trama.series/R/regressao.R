#' Regressão da série por FÓRMULA, e a previsão dela.
#'
#' O que a clássica e a STL não dão: coeficiente, erro-padrão e p-valor. A
#' fórmula é escrita sobre nomes que o bloco constrói a partir da série —
#' `valor` (a série), `t` (1, 2, ..., n), `periodo` (fator do ciclo: jan, ...,
#' dez), `ano` (o ano civil de cada observação) e `regressor` (a série ligada na
#' entrada de mesmo nome) —, e é por serem construídos AQUI que a previsão sabe
#' estendê-los: o futuro de `t`, `periodo` e `ano` é conhecido; o do
#' `regressor`, não, e vem pela entrada `futuro` do `series/forecast`.
#'
#' O ajuste sai como `models/fit` (contrato da `trama.models`): coeficientes,
#' quadro da ANOVA, estatísticas e diagnóstico são os blocos de lá. Os
#' componentes — tendência, sazonal, efeito do regressor e resto — viajam no
#' mesmo objeto, e o `series/component` os lê pelo adaptador
#' `models/fit -> series/decomposition`.
#'
#' O `contraste` muda como os coeficientes de `periodo` são LIDOS, e não a
#' decomposição. Com `soma_zero`, cada coeficiente sazonal é o desvio do período
#' em relação à média; com `categoria_base`, é a diferença para o primeiro
#' período (o `summary()` do R). Em ambos o componente sazonal devolvido é
#' centrado em zero e a tendência absorve a média.
#' @export
tr_series_regression <- function(serie, formula = "valor ~ t + periodo", contraste = "soma_zero",
                                 regressor = NULL, erro = "independente", ar = 1L, ma = 0L,
                                 excluir = "", remover_ns = FALSE, confianca = 0.95) {
  no <- "series/regression"
  contraste <- .tr_series_enum(contraste, c("soma_zero", "categoria_base"), "contraste")
  erro <- .tr_series_enum(erro, c("independente", "arma"), "erro")
  ar <- .tr_series_int(ar, "ar", min = 0, max = 3)
  ma <- .tr_series_int(ma, "ma", min = 0, max = 3)
  if (erro == "arma" && ar + ma == 0L) {
    .tr_series_abort("tr_series_error_bad_option",
                     paste0("Params 'ar' e 'ma': erro ARMA(0, 0) é erro independente. Suba 'ar' ",
                            "ou 'ma', ou escolha erro = 'independente'."))
  }
  xreg <- if (!is.null(regressor)) .tr_series_regressor(serie, regressor)
  f <- .tr_series_formula(formula, tem_regressor = !is.null(xreg))
  vars <- all.vars(f[[3]])
  sazonalidade <- "periodo" %in% vars
  .tr_series_sem_na(serie, no)
  if (sazonalidade) .tr_series_sazonal(serie, no)
  remover_ns <- isTRUE(remover_ns)
  confianca <- suppressWarnings(as.numeric(confianca))
  if (remover_ns && (length(confianca) != 1L || is.na(confianca) || confianca <= 0 || confianca >= 1)) {
    .tr_series_abort("tr_series_error_bad_option", "Param 'confianca': um número entre 0 e 1, como 0.95.")
  }
  alfa <- 1 - confianca
  tem_excluir <- nzchar(trimws(paste(excluir, collapse = "")))
  if (!sazonalidade && (tem_excluir || remover_ns)) {
    .tr_series_abort("tr_series_error_bad_option",
                     paste0("'%s': 'excluir' e 'remover não significativos' agem sobre os termos de ",
                            "'periodo', e a fórmula não tem 'periodo'."), no)
  }
  fora <- if (sazonalidade) .tr_series_estacoes_fora(serie, excluir) else character()

  base <- .tr_series_base(serie, xreg)
  ajustar <- function(fora, contraste) {
    dados <- base
    if (sazonalidade) dados$periodo <- .tr_series_fator_periodo(serie, fora, contraste)
    fit <- stats::lm(f, data = dados)
    X <- stats::model.matrix(fit)
    attr(X, "rotulos") <- attr(stats::terms(fit), "term.labels")
    # Graus de liberdade, e não só tamanho: com tantos parâmetros quanto
    # observações nenhum coeficiente falta, e o que sai é R² ajustado NaN e
    # p-valor NaN com aparência de ajuste perfeito. Dois graus residuais é o
    # mínimo para que erro-padrão e p-valor queiram dizer alguma coisa.
    if (nrow(X) < ncol(X) + 2L) {
      .tr_series_abort("tr_series_error_too_short",
                       paste0("'%s': a série tem %d observações e a fórmula pede %d parâmetros, o que ",
                              "deixa %d graus de liberdade residuais. Abaixo de 2 não há erro-padrão nem ",
                              "p-valor: simplifique a fórmula."),
                       no, nrow(X), ncol(X), nrow(X) - ncol(X))
    }
    b <- stats::coef(fit)
    reg_cols <- grepl("regressor", names(b), fixed = TRUE)
    if (any(reg_cols) && anyNA(b[reg_cols])) {
      .tr_series_abort("tr_series_error_fit",
                       paste0("O regressor é colinear com os outros termos da fórmula: ele é uma ",
                              "combinação exata deles, e seu coeficiente não se estima. Ligue outro ",
                              "regressor, ou simplifique a fórmula."))
    }
    aviso <- NULL
    if (erro == "arma" && !anyNA(b)) {
      # Mínimos quadrados generalizados com erro ARMA(p, q) (Morettin & Toloi
      # 2006; Pinheiro & Bates 2000), por máxima verossimilhança — e não REML —
      # para que o ajuste seja a MESMA função que o `stats::arima(xreg = )`
      # maximiza, que é o oráculo dos testes e o motor da previsão.
      rss <- sum(stats::residuals(fit)^2)
      if (rss <= 1e-12 * max(1, sum((dados$valor - mean(dados$valor))^2))) {
        .tr_series_abort("tr_series_error_singular_fit",
                         paste0("'%s': o modelo reproduz a série exatamente (resíduo nulo), e sem ",
                                "resíduo não há erro ARMA a estimar. Use erro = 'independente', ou ",
                                "simplifique a fórmula."), no)
      }
      cor <- nlme::corARMA(p = ar, q = ma, form = ~ 1)
      fit <- tryCatch(
        nlme::gls(f, data = dados, method = "ML", correlation = cor),
        error = function(e) {
          if (grepl("singular", conditionMessage(e), fixed = TRUE)) {
            .tr_series_abort("tr_series_error_singular_fit",
                             paste0("'%s': o ajuste com erro ARMA(%d, %d) ficou singular (%s) — ",
                                    "resíduo quase nulo ou termos colineares. Simplifique a fórmula ",
                                    "ou use erro = 'independente'."),
                             no, ar, ma, conditionMessage(e))
          }
          .tr_series_abort("tr_series_error_fit",
                           paste0("O ajuste com erro ARMA(%d, %d) não convergiu (%s). Baixe a ",
                                  "ordem do erro, ou use erro = 'independente'."),
                           ar, ma, conditionMessage(e))
        })
      # A chamada leva a tabela e a fórmula dentro dela: os leitores da models
      # que reajustam (o quadro marginal, com soma zero) reencontram tudo sem
      # procurar no ambiente de quem chamou, que não existe depois do RDS.
      fit$call <- as.call(list(quote(nlme::gls), model = f, data = dados, correlation = cor,
                               method = "ML"))
      aviso <- .tr_series_aviso_raiz(fit, ar)
    }
    if (anyNA(stats::coef(fit))) {
      .tr_series_abort("tr_series_error_fit",
                       paste0("O ajuste ficou indeterminado (%d coeficientes sem estimativa): a série é ",
                              "curta demais, ou há termos repetidos na fórmula. Simplifique a fórmula."),
                       sum(is.na(stats::coef(fit))))
    }
    list(fit = fit, X = X, dados = dados, aviso = aviso)
  }

  # A eliminação parte dos desvios em relação à média do ciclo (soma zero): com
  # categoria base, a primeira estação seria a referência e nunca sairia.
  a <- ajustar(fora, if (remover_ns) "soma_zero" else contraste)
  removidos <- character()
  if (remover_ns) {
    rot <- .tr_series_estacoes(as.integer(stats::frequency(serie)))
    repeat {
      p <- .tr_series_p_estacoes(a$fit, a$X, a$dados)
      if (!length(p) || max(p) <= alfa) break
      pior <- names(p)[which.max(p)]
      # Sem nenhuma estação sobrando, `periodo` ficaria constante: para antes.
      if (length(setdiff(rot, c(fora, pior))) < 1L) break
      fora <- c(fora, pior); removidos <- c(removidos, pior)
      a <- ajustar(fora, "soma_zero")
    }
    if (!length(fora)) a <- ajustar(fora, contraste)
  }
  if (length(fora)) contraste <- "categoria_base"

  comp <- .tr_series_componentes(serie, a$fit, a$X, sazonalidade)
  formula_txt <- paste(deparse(f, width.cutoff = 500L), collapse = " ")
  rotulo <- sprintf("Regressão da série · %s%s", formula_txt,
                    if (erro == "arma") sprintf(" · erro ARMA(%d, %d)", ar, ma) else "")
  info <- list(
    serie = serie, formula = formula_txt, sazonalidade = sazonalidade, contraste = contraste,
    estacoes_fora = fora, estacoes_removidas = removidos, alfa = if (remover_ns) alfa,
    erro = erro, ordem = if (erro == "arma") c(ar = ar, ma = ma) else NULL,
    regressor = if (!is.null(regressor)) regressor, aviso = a$aviso,
    tendencia = comp$tendencia, sazonal = comp$sazonal, resto = comp$resto,
    efeito_regressor = comp$regressor)
  trama.models::tr_models_as_fit(a$fit, if (erro == "arma") "gls" else "lm", rotulo, f, a$dados,
                                 "valor", nota = a$aviso %||% "", extra = list(serie_reg = info))
}

#' A fórmula digitada, conferida: resposta `valor`, e só os nomes que o bloco
#' constrói. Com regressor ligado e fora da fórmula, ele entra no fim — é o que
#' o bloco sempre fez, e a fórmula mostrada no card é a que foi ajustada.
#' @noRd
.tr_series_formula <- function(formula, tem_regressor) {
  txt <- .tr_series_obrigatorio(paste(formula, collapse = " "), "formula")
  f <- tryCatch(stats::as.formula(txt, env = .tr_series_env_formula()), error = function(e) NULL)
  if (is.null(f) || length(f) != 3L) {
    .tr_series_abort("tr_series_error_bad_option",
                     "Param 'formula': '%s' não é uma fórmula. Escreva como 'valor ~ t + periodo'.", txt)
  }
  if (!identical(all.vars(f[[2]]), "valor")) {
    .tr_series_abort("tr_series_error_bad_option",
                     "Param 'formula': a resposta é 'valor' (a própria série), como em 'valor ~ t + periodo'.")
  }
  permitidos <- c("t", "periodo", "ano", "regressor")
  estranhos <- setdiff(all.vars(f[[3]]), permitidos)
  if (length(estranhos)) {
    .tr_series_abort("tr_series_error_bad_option",
                     paste0("Param 'formula': '%s' não existe aqui. A fórmula enxerga t (1, 2, ...), ",
                            "periodo (o período do ciclo), ano e regressor (a série ligada na entrada)."),
                     paste(estranhos, collapse = "', '"))
  }
  usa_reg <- "regressor" %in% all.vars(f[[3]])
  if (usa_reg && !tem_regressor) {
    .tr_series_abort("tr_series_error_bad_option",
                     "Param 'formula': a fórmula usa 'regressor', e a entrada 'regressor' não está ligada.")
  }
  if (tem_regressor && !usa_reg) f <- stats::update(f, . ~ . + regressor)
  environment(f) <- .tr_series_env_formula()
  f
}

#' Ambiente das fórmulas: só `base` e `stats` (`poly`, `I`, `log`), e nada do
#' ambiente de quem chamou — a fórmula viaja no RDS do ajuste e é reavaliada na
#' previsão, onde nenhuma variável de fora existe.
#' @noRd
.tr_series_env_formula <- function() new.env(parent = asNamespace("stats"))

#' A tabela sobre a qual a fórmula é ajustada, sem `periodo` (que depende das
#' estações fora e do contraste, e é posto por quem ajusta).
#' @noRd
.tr_series_base <- function(serie, xreg = NULL) {
  d <- data.frame(valor = as.numeric(serie), t = seq_along(serie),
                  ano = floor(as.numeric(stats::time(serie)) + 1e-8))
  if (!is.null(xreg)) d$regressor <- as.numeric(xreg)
  d
}

#' O fator `periodo` de uma série (ou do futuro dela, por `ciclo`).
#'
#' Estações fora do modelo viram UM nível base, "demais": o efeito delas é o
#' mesmo, e cada coeficiente que fica é a diferença para esse grupo. Por isso o
#' contraste é o de categoria base quando há estação fora — com soma zero,
#' "tirar um mês" não quereria dizer "esse mês não tem efeito". O contraste é
#' posto NO FATOR, e não no `lm`: todo reajuste feito a partir dos dados herda a
#' mesma parametrização.
#' @noRd
.tr_series_fator_periodo <- function(serie, fora, contraste, ciclo = as.integer(stats::cycle(serie))) {
  f <- as.integer(stats::frequency(serie))
  rot <- .tr_series_estacoes(f)
  if (length(fora)) {
    lab <- rot[ciclo]
    lab[lab %in% fora] <- "demais"
    x <- factor(lab, levels = c("demais", setdiff(rot, fora)))
    stats::contrasts(x) <- stats::contr.treatment
  } else {
    x <- factor(ciclo, levels = seq_len(f), labels = rot)
    stats::contrasts(x) <- if (contraste == "soma_zero") stats::contr.sum else stats::contr.treatment
  }
  x
}

#' Tendência, sazonal, efeito do regressor e resto de um ajuste.
#'
#' O sazonal sai da PARTE do preditor linear que vem do termo `periodo`, e não
#' dos coeficientes pelo nome: com soma zero o último período não tem
#' coeficiente próprio. O efeito do regressor é a parte dos termos que o usam.
#' A tendência é o resto do ajustado — intercepto e o que for função do tempo
#' (e interações). Assim `série = tendência + sazonal + regressor + resto`.
#' @noRd
.tr_series_componentes <- function(serie, fit, X, sazonalidade) {
  b <- stats::coef(fit)
  rot <- attr(X, "rotulos")
  asg <- attr(X, "assign")
  parte <- function(termos) {
    cols <- asg %in% match(termos, rot)
    if (!any(cols)) return(rep(0, nrow(X)))
    as.numeric(X[, cols, drop = FALSE] %*% b[cols])
  }
  saz <- if (sazonalidade) parte("periodo") else rep(0, nrow(X))
  if (sazonalidade) {
    # Centra pela média do CICLO, e não da amostra: com anos incompletos a média
    # da amostra pesa mais os meses que aparecem mais vezes, e os efeitos
    # deixariam de somar zero, que é o que esta decomposição promete.
    saz <- saz - mean(tapply(saz, stats::cycle(serie), mean))
  }
  termos_reg <- rot[vapply(rot, function(r) "regressor" %in% all.vars(str2lang(r)), logical(1))]
  efeito <- if (length(termos_reg)) parte(termos_reg) else NULL
  como_ts <- function(v) stats::ts(v, start = stats::start(serie), frequency = stats::frequency(serie))
  ajustado <- as.numeric(stats::fitted(fit))
  list(tendencia = como_ts(ajustado - saz - (efeito %||% 0)), sazonal = como_ts(saz),
       resto = como_ts(as.numeric(serie) - ajustado),
       regressor = if (!is.null(efeito)) como_ts(efeito))
}

#' O ajuste que chegou é de uma série?
#' @noRd
.tr_series_exige_fit_serie <- function(ajuste, no) {
  if (!inherits(ajuste, "tr_models_fit") || is.null(ajuste$serie_reg)) {
    .tr_series_abort("tr_series_error_not_a_regression",
                     paste0("'%s' recebeu um modelo que não é de uma série. Ligue a saída de ",
                            "'series/regression', 'series/detrend' ou 'series/deseasonalize'."), no)
  }
  invisible(ajuste$serie_reg)
}

#' `models/fit` de série -> decomposição: o que dá de graça o
#' `series/component` e o `series/plot_decomposition`.
#' @noRd
.tr_series_fit_decomp <- function(x) {
  r <- .tr_series_exige_fit_serie(x, "series/component")
  .tr_series_decomp(r$serie, r$tendencia, r$sazonal, r$resto, "aditiva", "regressão",
                    regressor = r$efeito_regressor)
}

#' Previsão de um ajuste de série, `h` períodos à frente.
#'
#' `t`, `periodo` e `ano` do futuro saem da própria série; o regressor futuro,
#' de `futuro`, que tem de cobrir os `h` períodos seguintes ao fim da série.
#' Erro independente: intervalo de predição do `lm` (t de Student). Erro ARMA:
#' o mesmo modelo reajustado por `forecast::Arima(xreg = )` — a mesma
#' verossimilhança do GLS por ML —, cuja previsão propaga o erro ARMA.
#' @noRd
.tr_series_prever_fit <- function(ajuste, h, futuro = NULL) {
  r <- .tr_series_exige_fit_serie(ajuste, "series/forecast")
  serie <- r$serie
  fr <- stats::frequency(serie)
  f <- stats::as.formula(r$formula, env = .tr_series_env_formula())
  n <- length(serie)
  fim <- stats::tsp(serie)[[2]]
  tempos <- fim + seq_len(h) / fr
  novos <- data.frame(t = n + seq_len(h), ano = floor(tempos + 1e-8))
  if (r$sazonalidade) {
    ciclo_fim <- as.integer(stats::cycle(serie))[[n]]
    ciclo <- ((ciclo_fim + seq_len(h) - 1L) %% as.integer(fr)) + 1L
    novos$periodo <- .tr_series_fator_periodo(serie, r$estacoes_fora, r$contraste, ciclo = ciclo)
  }
  if ("regressor" %in% all.vars(f[[3]])) {
    if (is.null(futuro)) {
      .tr_series_abort("tr_series_error_bad_option",
                       paste0("'series/forecast': a regressão usa um regressor, e o futuro dele não é ",
                              "conhecido aqui. Ligue na entrada 'futuro' a série do regressor com os %d ",
                              "períodos depois de %s."), h, .tr_series_rotulo(stats::end(serie), fr))
    }
    if (!isTRUE(all.equal(stats::frequency(futuro), fr))) {
      .tr_series_abort("tr_series_error_frequency_mismatch",
                       "'series/forecast': o regressor futuro tem frequência %g e a série, %g.",
                       stats::frequency(futuro), fr)
    }
    tf <- as.numeric(stats::time(futuro))
    idx <- vapply(tempos, function(x) { i <- which(abs(tf - x) < 1e-6); if (length(i)) i[[1]] else NA_integer_ }, 1L)
    if (anyNA(idx) || anyNA(futuro[idx])) {
      k <- if (anyNA(idx)) which(is.na(idx))[[1]] - 1L else which(is.na(futuro[idx]))[[1]] - 1L
      .tr_series_abort("tr_series_error_bad_option",
                       paste0("'series/forecast': o regressor futuro cobre %d dos %d períodos depois do ",
                              "fim da série. Baixe o horizonte para %d, ou estenda o regressor."),
                       k, h, k)
    }
    novos$regressor <- as.numeric(futuro[idx])
  }
  fit <- ajuste$ajuste
  inicio <- fim + 1 / fr
  como_ts <- function(v) stats::ts(v, start = inicio, frequency = fr)
  if (identical(r$erro, "arma")) {
    # Os `terms` do `lm` equivalente, e não os da fórmula crua: eles levam o
    # `predvars`, e é ele que faz `poly(t, 2)`, `scale(t)` ou `ns(t)` no futuro
    # usarem a base do ajuste em vez de recalculá-la sobre os h pontos novos.
    tl <- stats::delete.response(stats::terms(stats::lm(f, data = ajuste$dados)))
    X <- stats::model.matrix(tl, data = ajuste$dados)
    Xn <- stats::model.matrix(tl, data = novos)
    tira <- colnames(X) == "(Intercept)"
    o <- r$ordem
    arima <- .tr_series_ajustar(
      forecast::Arima(serie, order = c(o[["ar"]], 0L, o[["ma"]]), xreg = X[, !tira, drop = FALSE],
                      include.mean = any(tira), method = "ML"), "series/forecast")
    fc <- forecast::forecast(arima, xreg = Xn[, !tira, drop = FALSE], level = c(80, 95))
    fc$method <- sprintf("Regressão da série com erro ARMA(%d, %d)", o[["ar"]], o[["ma"]])
    return(fc)
  }
  # O `predict.lm` usa o contraste guardado no ajuste; o do fator novo só
  # geraria o aviso de "contrastes descartados".
  if (!is.null(novos$periodo)) attr(novos$periodo, "contrasts") <- NULL
  p80 <- stats::predict(fit, newdata = novos, interval = "prediction", level = 0.80)
  p95 <- stats::predict(fit, newdata = novos, interval = "prediction", level = 0.95)
  lim <- function(a, b) {
    m <- cbind(a, b)
    colnames(m) <- c("80%", "95%")
    stats::ts(m, start = inicio, frequency = fr)
  }
  structure(list(
    method = "Regressão da série", model = NULL, level = c(80, 95),
    mean = como_ts(p80[, "fit"]),
    lower = lim(p80[, "lwr"], p95[, "lwr"]), upper = lim(p80[, "upr"], p95[, "upr"]),
    x = serie,
    # Pelo resto da série, e não por `fitted(fit)`: com faltante (detrend) o
    # ajuste tem menos linhas que a série, e o `ts` sairia deslocado no tempo.
    fitted = serie - r$resto,
    residuals = r$resto),
    class = "forecast")
}

# ---- Componentes com inferência --------------------------------------------

#' Ciclos incompletos: a série não começa no primeiro período nem termina no
#' último. Aí o ajuste de um componente sozinho deixa de coincidir com o
#' conjunto (tendência e sazonal deixam de ser ortogonais), e o bloco avisa.
#' @noRd
.tr_series_aviso_ciclos <- function(serie, no) {
  f <- as.integer(stats::frequency(serie))
  if (f <= 1L) return(NULL)
  ci <- as.integer(stats::cycle(serie))
  if (ci[[1]] == 1L && ci[[length(ci)]] == f) return(NULL)
  msg <- paste0("ciclos incompletos: a estimativa deste componente sozinho difere da do ajuste ",
                "conjunto; para os dois juntos, use 'series/regression'")
  rlang::warn(paste0("'", no, "': ", msg), class = "tr_series_warn_incomplete_cycles")
  msg
}

#' O ajuste da tendência de `series/detrend`, como `models/fit`.
#'
#' Linear e polinomial têm coeficientes: `valor ~ t` e
#' `valor ~ poly(t, g, raw = TRUE)` (potências cruas, para que cada
#' coeficiente seja o de t^k e a previsão seja a do mesmo polinômio). Loess e
#' diferença não têm: a saída é a reta de mínimos quadrados, dita como
#' referência na nota e no rótulo, para que o fio continue existindo sem
#' fingir que a tendência removida tem p-valor.
#' @noRd
.tr_series_detrend_fit <- function(serie, metodo, grau, tendencia) {
  g <- if (metodo == "polinomial") grau else 1L
  formula <- if (g == 1L) "valor ~ t" else sprintf("valor ~ poly(t, %d, raw = TRUE)", g)
  f <- .tr_series_formula(formula, FALSE)
  base <- .tr_series_base(serie)
  ok <- !is.na(base$valor)
  fit <- stats::lm(f, data = base[ok, , drop = FALSE])
  aviso <- .tr_series_aviso_ciclos(serie, "series/detrend")
  referencia <- metodo %in% c("loess", "diferenca")
  nota <- paste(c(if (referencia) sprintf(paste0("o método '%s' não tem coeficientes: este ajuste é a ",
                                                 "reta de mínimos quadrados, só como referência"), metodo),
                  aviso), collapse = "; ")
  ajustado <- rep(NA_real_, length(serie)); ajustado[ok] <- stats::fitted(fit)
  como_ts <- function(v) stats::ts(v, start = stats::start(serie), frequency = stats::frequency(serie))
  info <- list(serie = serie, formula = paste(deparse(f, width.cutoff = 500L), collapse = " "),
               sazonalidade = FALSE, contraste = "soma_zero", estacoes_fora = character(),
               estacoes_removidas = character(), erro = "independente", ordem = NULL, aviso = aviso,
               tendencia = como_ts(ajustado), sazonal = como_ts(rep(0, length(serie))),
               resto = como_ts(as.numeric(serie) - ajustado), efeito_regressor = NULL)
  rotulo <- if (referencia) "Tendência · reta de referência" else sprintf("Tendência · %s", formula)
  out <- trama.models::tr_models_as_fit(fit, "lm", rotulo, f, base[ok, , drop = FALSE], "valor",
                                        nota = nota, extra = list(serie_reg = info))
  out$descartadas <- sum(!ok)
  out
}

#' Tira a sazonalidade da série, com os efeitos estimados.
#'
#' Os efeitos de cada período saem de uma regressão em `periodo` (dummies,
#' contraste à escolha). Com **controlar_tendencia**, a tendência linear entra
#' no MESMO ajuste (`valor ~ t + periodo`) e só o sazonal sai da série: numa
#' série com tendência, a média de dezembro fica acima da de janeiro só porque
#' dezembro vem depois, e as dummies sozinhas chamariam isso de sazonalidade.
#' @export
tr_series_deseasonalize <- function(serie, controlar_tendencia = TRUE, contraste = "soma_zero",
                                    excluir = "", remover_ns = FALSE, confianca = 0.95) {
  no <- "series/deseasonalize"
  .tr_series_sem_na(serie, no)
  .tr_series_sazonal(serie, no)
  formula <- if (isTRUE(controlar_tendencia)) "valor ~ t + periodo" else "valor ~ periodo"
  aviso <- .tr_series_aviso_ciclos(serie, no)
  fit <- tr_series_regression(serie, formula = formula, contraste = contraste, excluir = excluir,
                              remover_ns = remover_ns, confianca = confianca)
  fit$rotulo <- sprintf("Sazonalidade · %s", fit$serie_reg$formula)
  if (!is.null(aviso)) fit$nota <- paste(c(if (nzchar(fit$nota)) fit$nota, aviso), collapse = "; ")
  sazonal <- fit$serie_reg$sazonal
  out <- serie - sazonal
  attr(out, "sazonal") <- sazonal
  list(out = out, ajuste = fit)
}
