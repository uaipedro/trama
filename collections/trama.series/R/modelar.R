# Modelos e previsão.
#
# Ajuste e previsão são nós separados, e o motivo é o custo: ajustar um ARIMA
# automático é o caro (centenas de modelos candidatos), prever com ele é
# aritmética. Com um nó só, trocar o horizonte de 12 para 24 refaria a busca
# inteira; separados, o recálculo incremental do trama recomputa só a cauda
# barata. Pelo mesmo motivo `series/residuals` e `series/accuracy` são irmãos
# pendurados no MESMO ajuste.
#
# Não há `lambda` (Box-Cox) nos modelos: a transformação é o nó
# `series/transform`, visível no fio, e não um param escondido dentro do
# ajuste — a mesma escolha do `adjust` fora do estimador na `experiments`.

#' Roda o ajuste reclassificando a falha, com a causa logo abaixo.
#'
#' Os ajustadores do `forecast` falham com mensagens boas ("non-stationary AR
#' part"), mas sem classe e sem dizer QUAL nó falhou; o `parent = e` mantém a
#' mensagem original inteira.
#' @noRd
.tr_series_ajustar <- function(code, no) {
  tryCatch(force(code), error = function(e) {
    .tr_series_abort("tr_series_error_fit", "'%s': o ajuste falhou.", no, parent = e)
  })
}

#' ARIMA, automático (Hyndman-Khandakar) ou com a ordem digitada.
#'
#' `automatico = TRUE` ignora as seis ordens, e a ajuda diz isso com todas as
#' letras: esconder os campos dependeria de um param condicional que o card
#' não tem. O resumo do card mostra a ordem que a busca escolheu, então o
#' caminho natural é rodar automático, ler a ordem, e só então fixar à mão.
#' @export
tr_series_arima <- function(serie, automatico = TRUE, p = 1L, d = 1L, q = 1L,
                            P = 0L, D = 0L, Q = 0L, sazonal = TRUE, constante = TRUE) {
  .tr_series_minimo(serie, 8L, "series/arima", "um ARIMA")
  f <- stats::frequency(serie)
  if (isTRUE(automatico)) {
    return(.tr_series_ajustar(
      forecast::auto.arima(serie, seasonal = isTRUE(sazonal) && f > 1,
                           allowdrift = isTRUE(constante), allowmean = isTRUE(constante)),
      "series/arima"))
  }
  ordem <- c(.tr_series_int(p, "p", 0, 5), .tr_series_int(d, "d", 0, 2), .tr_series_int(q, "q", 0, 5))
  sazo <- c(.tr_series_int(P, "P", 0, 2), .tr_series_int(D, "D", 0, 1), .tr_series_int(Q, "Q", 0, 2))
  if (sum(sazo) > 0L) .tr_series_sazonal(serie, "series/arima (parte sazonal P, D, Q)", ciclos = 1L)
  .tr_series_ajustar(
    forecast::Arima(serie, order = ordem, seasonal = sazo, include.constant = isTRUE(constante)),
    "series/arima")
}

#' Suavização exponencial em espaço de estados (ETS).
#'
#' O modelo é o código de três letras da literatura — erro, tendência,
#' sazonalidade —, e não três enums, porque é assim que ele aparece em todo
#' livro e em todo resumo ("ETS(M,Ad,M)"): quem leu `MAM` no resumo de um
#' ajuste automático copia o código para fixá-lo.
#' @export
tr_series_ets <- function(serie, modelo = "ZZZ", amortecida = "auto") {
  modelo <- toupper(.tr_series_obrigatorio(modelo, "modelo"))
  if (!grepl("^[AMZ][ANMZ][ANMZ]$", modelo)) {
    .tr_series_abort("tr_series_error_bad_ets",
                     paste0("Param 'modelo': '%s' não é um código ETS. São três letras: erro (A, M), ",
                            "tendência (N, A, M) e sazonalidade (N, A, M), com Z deixando o ajuste ",
                            "escolher. Ex.: ANN, AAN, AAA, MAM, ZZZ."), modelo)
  }
  amortecida <- .tr_series_enum(amortecida, c("auto", "sim", "não"), "amortecida")
  f <- stats::frequency(serie)
  if (substr(modelo, 3, 3) %in% c("A", "M")) {
    .tr_series_sazonal(serie, "series/ets (sazonalidade A ou M)")
    if (f > 24) {
      .tr_series_option("modelo", modelo,
                        sprintf("sazonalidade N ou Z: o ETS não modela ciclo maior que 24 (a série tem %g)", f))
    }
  }
  .tr_series_minimo(serie, 4L, "series/ets", "um ETS")
  .tr_series_ajustar(
    forecast::ets(serie, model = modelo,
                  damped = switch(amortecida, auto = NULL, sim = TRUE, "não" = FALSE)),
    "series/ets")
}

#' Holt-Winters clássico, pelos mínimos quadrados do `stats`.
#'
#' Fica ao lado do ETS, que o generaliza, porque é o método que se ensina e o
#' que se cita: tirar a tendência ou a sazonalidade é desligar uma chave, e o
#' efeito na previsão aparece no card seguinte.
#' @export
tr_series_holt_winters <- function(serie, tendencia = TRUE, sazonalidade = TRUE, tipo = "aditiva") {
  tipo <- .tr_series_enum(tipo, c("aditiva", "multiplicativa"), "tipo")
  if (isTRUE(sazonalidade)) .tr_series_sazonal(serie, "series/holt_winters")
  .tr_series_sem_na(serie, "series/holt_winters")
  .tr_series_minimo(serie, 4L, "series/holt_winters", "um Holt-Winters")
  .tr_series_ajustar(
    stats::HoltWinters(serie, beta = if (isTRUE(tendencia)) NULL else FALSE,
                       gamma = if (isTRUE(sazonalidade)) NULL else FALSE,
                       seasonal = if (tipo == "aditiva") "additive" else "multiplicative"),
    "series/holt_winters")
}

#' Previsão de um modelo ajustado, com intervalos de 80 e 95%.
#'
#' Os níveis são fixos: são os que todo gráfico de previsão mostra, e os que o
#' adaptador para tabela nomeia (`li_80`, `ls_95`). Um param de nível
#' mudaria o nome das colunas a jusante, e um `data/filter` escrito sobre
#' `ls_95` quebraria ao trocar o nível no card.
#' @export
tr_series_forecast <- function(modelo, horizonte = 12L, intervalo = "normal", .seed = NULL) {
  h <- .tr_series_int(horizonte, "horizonte", min = 1, max = 1000)
  intervalo <- .tr_series_enum(intervalo, c("normal", "bootstrap"), "intervalo")
  if (intervalo == "normal") {
    return(.tr_series_ajustar(forecast::forecast(modelo, h = h, level = c(80, 95)), "series/forecast"))
  }
  # Bootstrap dos resíduos (FPP3, sec. 5.5): 5000 trajetórias simuladas com
  # erros reamostrados; os limites são quantis empíricos. Só ARIMA e ETS têm
  # simulação no `forecast`; Holt-Winters do `stats` não.
  if (!inherits(modelo, c("Arima", "ets"))) {
    .tr_series_option("intervalo", intervalo,
                      "normal (o bootstrap pede um modelo series/arima ou series/ets)")
  }
  semente <- if (is.null(.seed) || !length(.seed) || is.na(.seed[[1]])) 1L else as.integer(.seed[[1]])
  .tr_series_com_semente(semente, .tr_series_ajustar(
    forecast::forecast(modelo, h = h, level = c(80, 95), bootstrap = TRUE, npaths = 5000),
    "series/forecast"))
}

#' Roda com a semente do nó e devolve o RNG como estava.
#' @noRd
.tr_series_com_semente <- function(seed, expr) {
  tem <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (tem) antigo <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  antigo_kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(antigo_kind))
    if (tem) assign(".Random.seed", antigo, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  force(expr)
}

#' As previsões de referência: o que qualquer modelo tem de bater.
#'
#' Um ARIMA que erra mais que "o mesmo mês do ano passado" não merece o
#' card, e sem o ingênuo sazonal ao lado ninguém fica sabendo. Por isso a
#' referência é um nó de primeira ordem, e não uma coluna escondida no
#' `accuracy`.
#' @export
tr_series_baseline <- function(serie, metodo = "ingênuo sazonal", horizonte = 12L) {
  metodo <- .tr_series_enum(metodo, c("média", "ingênuo", "ingênuo sazonal", "deriva"), "metodo")
  h <- .tr_series_int(horizonte, "horizonte", min = 1, max = 1000)
  if (metodo == "ingênuo sazonal") .tr_series_sazonal(serie, "series/baseline (ingênuo sazonal)", ciclos = 1L)
  .tr_series_minimo(serie, 2L, "series/baseline", "uma previsão de referência")
  lv <- c(80, 95)
  .tr_series_ajustar(switch(metodo,
    "média" = forecast::meanf(serie, h = h, level = lv),
    "ingênuo" = forecast::naive(serie, h = h, level = lv),
    "ingênuo sazonal" = forecast::snaive(serie, h = h, level = lv),
    deriva = forecast::rwf(serie, h = h, drift = TRUE, level = lv)), "series/baseline")
}

#' Os resíduos do ajuste, como série — para testar e para ver.
#'
#' Série, e não tabela, porque o destino natural é `series/ljung_box` e
#' `series/acf`: o diagnóstico de um modelo é perguntar se o que sobrou ainda
#' tem estrutura temporal.
#' @export
tr_series_residuals <- function(modelo) .tr_series_uni(stats::residuals(modelo))

#' Medidas de erro da previsão, no treino e — se houver — no teste.
#'
#' Sem a série real, só a linha do treino: erro de um passo à frente dentro
#' da amostra, que é otimista por construção. Com a série real (os períodos
#' que o modelo não viu, recortados por `series/window`), sai a linha do teste,
#' que é a que responde se a previsão presta.
#'
#' Série real que não cobre nenhum período da previsão é ERRO, e não linha
#' vazia: é quase sempre o fio ligado na série de treino por engano, e uma
#' tabela só com o treino pareceria resposta.
#' @export
tr_series_accuracy <- function(previsao, real = NULL) {
  m <- if (is.null(real)) {
    forecast::accuracy(previsao)
  } else {
    fp <- stats::frequency(previsao$mean)
    if (stats::frequency(real) != fp) {
      .tr_series_abort("tr_series_error_no_overlap",
                       "A série real tem frequência %g e a previsão, %g: os períodos não se comparam.",
                       stats::frequency(real), fp)
    }
    comum <- intersect(round(as.numeric(stats::time(previsao$mean)) * fp),
                       round(as.numeric(stats::time(real)) * fp))
    if (!length(comum)) {
      .tr_series_abort("tr_series_error_no_overlap",
                       paste0("A série real (%s a %s) não cobre nenhum período da previsão (%s a %s). ",
                              "Ligue a série que contém o período previsto."),
                       .tr_series_rotulo(stats::start(real), fp), .tr_series_rotulo(stats::end(real), fp),
                       .tr_series_rotulo(stats::start(previsao$mean), fp),
                       .tr_series_rotulo(stats::end(previsao$mean), fp))
    }
    forecast::accuracy(previsao, real)
  }
  conjunto <- c("Training set" = "treino", "Test set" = "teste")[rownames(m)]
  out <- as.data.frame(m, row.names = NULL)
  names(out) <- gsub("[^a-z0-9]+", "_", tolower(names(out)))
  tibble::as_tibble(cbind(metodo = previsao$method, conjunto = unname(conjunto), out))
}

#' Modelo de intervenção: ARIMA com regressor de degrau, pulso ou rampa.
#'
#' A forma de ordem zero de Box & Tiao (1975): y_t = ω I_t + N_t, com N_t
#' ARIMA e I_t = 1 a partir da data (degrau), só na data (pulso) ou t − T + 1
#' a partir dela (rampa). Estimação conjunta por máxima verossimilhança
#' (`forecast::Arima(xreg = )`), com a diferenciação aplicada também ao
#' regressor. Devolve a tabela dos coeficientes com erro-padrão, IC de 95%
#' (Wald, normal) e p-valor.
#' @export
tr_series_intervencao <- function(serie, data, tipo = "degrau", p = 0L, d = 1L, q = 1L,
                                  P = 0L, D = 0L, Q = 0L, constante = FALSE,
                                  resposta = "imediata") {
  tipo <- .tr_series_enum(tipo, c("degrau", "pulso", "rampa"), "tipo")
  resposta <- .tr_series_enum(resposta, c("imediata", "gradual"), "resposta")
  if (resposta == "gradual" && tipo == "rampa") {
    .tr_series_abort("tr_series_error_bad_option",
                     paste0("Param 'resposta': a resposta gradual vale para degrau e pulso ",
                            "(função de transferência ω/(1 − δB) de Box & Tiao); rampa só com ",
                            "resposta imediata."))
  }
  .tr_series_sem_na(serie, "series/intervencao")
  .tr_series_minimo(serie, 12L, "series/intervencao", "um modelo de intervenção")
  f <- stats::frequency(serie)
  v <- .tr_series_periodo(.tr_series_obrigatorio(data, "data"), "data", f)
  pos <- .tr_series_pos(v, f)
  tt <- as.numeric(stats::time(serie))
  i0 <- which(abs(tt - pos) < 1e-6 / f)
  n <- length(serie)
  if (length(i0) != 1L || i0 < 2L) {
    .tr_series_abort("tr_series_error_bad_period",
                     paste0("Param 'data': '%s' tem de ser um período DA série, depois da primeira ",
                            "observação (%s a %s) — sem observação antes, não há nível de referência."),
                     data, .tr_series_rotulo(stats::start(serie), f), .tr_series_rotulo(stats::end(serie), f))
  }
  idx <- seq_len(n)
  reg <- switch(tipo,
    degrau = as.numeric(idx >= i0),
    pulso = as.numeric(idx == i0),
    rampa = pmax(0, idx - i0 + 1))
  ordem <- c(.tr_series_int(p, "p", 0, 5), .tr_series_int(d, "d", 0, 2), .tr_series_int(q, "q", 0, 5))
  sazo <- c(.tr_series_int(P, "P", 0, 2), .tr_series_int(D, "D", 0, 1), .tr_series_int(Q, "Q", 0, 2))
  if (sum(sazo) > 0L) .tr_series_sazonal(serie, "series/intervencao (parte sazonal P, D, Q)", ciclos = 1L)
  ajusta <- function(xr, fixed = NULL) {
    forecast::Arima(serie, order = ordem, seasonal = sazo, xreg = cbind(intervencao = xr),
                    include.constant = isTRUE(constante), fixed = fixed,
                    transform.pars = is.null(fixed))
  }
  if (resposta == "imediata") {
    fit <- .tr_series_ajustar(ajusta(reg), "series/intervencao")
    b <- stats::coef(fit)
    se <- sqrt(diag(fit$var.coef))
  } else {
    # Box & Tiao (1975): y_t = ω/(1 − δB) I_t + N_t. Para δ fixo, o regressor
    # filtrado x_t = I_t + δ x_{t−1} (zero antes da data) entra linear com
    # coeficiente ω, e a verossimilhança perfilada em δ se maximiza numa
    # dimensão (`optimize`). O erro-padrão vem da hessiana numérica da
    # log-verossimilhança completa (ARMA, ω e δ juntos), não da condicional em
    # δ. Oráculo: `TSA::arimax(transfer = list(c(1, 0)))` (Cryer & Chan 2008).
    filtra <- function(dl) as.numeric(stats::filter(reg, dl, method = "recursive"))
    o <- .tr_series_ajustar(
      stats::optimize(function(dl) ajusta(filtra(dl))$loglik, c(-0.999, 0.999),
                      maximum = TRUE, tol = 1e-8),
      "series/intervencao")
    delta <- o$maximum
    fit <- .tr_series_ajustar(ajusta(filtra(delta)), "series/intervencao")
    th <- c(stats::coef(fit), delta = delta)
    k <- length(th)
    nll <- function(t) -ajusta(filtra(t[[k]]), fixed = unname(t[-k]))$loglik
    H <- tryCatch(stats::optimHess(th, nll), error = function(e) NULL)
    V <- if (is.null(H)) NULL else tryCatch(solve(H), error = function(e) NULL)
    se <- if (is.null(V)) rep(NA_real_, k) else { dv <- diag(V); ifelse(dv > 0, sqrt(abs(dv)), NA_real_) }
    b <- th
    names(se) <- names(b)
    if (abs(delta) > 0.99) {
      .tr_series_abort("tr_series_error_fit",
                       paste0("'series/intervencao': o δ da resposta gradual foi para a borda ",
                              "(%.3f); a resposta não se estabiliza — com degrau, experimente ",
                              "a rampa; com pulso, o degrau."), delta)
    }
  }
  if (anyNA(se) || any(!is.finite(se))) {
    .tr_series_abort("tr_series_error_fit",
                     "'series/intervencao': a matriz de covariância saiu singular; simplifique a ordem do ARIMA.")
  }
  z <- stats::qnorm(0.975)
  tb <- tibble::tibble(
    termo = names(b), estimativa = unname(b), erro_padrao = unname(se),
    li_95 = unname(b - z * se), ls_95 = unname(b + z * se),
    z = unname(b / se), p_valor = unname(2 * stats::pnorm(-abs(b / se))))
  # Série em log: o efeito em porcentagem é exp(ω) − 1 (só faz sentido no
  # degrau e no pulso, e só se a série foi logaritmizada — a coluna vem sempre
  # e a ajuda diz quando lê-la).
  tb$efeito_pct <- ifelse(tb$termo == "intervencao", 100 * (exp(tb$estimativa) - 1), NA_real_)
  if (resposta == "gradual") {
    # Com resposta gradual, ω é o efeito do PRIMEIRO período; o de longo prazo
    # do degrau é ω/(1 − δ) (o pulso volta a zero). A linha de longo prazo tem
    # erro-padrão pelo método delta, com a covariância completa.
    tb$efeito_pct[tb$termo == "intervencao"] <- NA_real_
    if (tipo == "degrau") {
      w <- b[["intervencao"]]
      lp <- w / (1 - delta)
      gr <- c(1 / (1 - delta), w / (1 - delta)^2)
      ii <- match(c("intervencao", "delta"), names(b))
      se_lp <- if (is.null(V)) NA_real_ else sqrt(drop(t(gr) %*% V[ii, ii] %*% gr))
      tb <- rbind(tb, tibble::tibble(
        termo = "efeito_longo_prazo", estimativa = lp, erro_padrao = se_lp,
        li_95 = lp - z * se_lp, ls_95 = lp + z * se_lp, z = lp / se_lp,
        p_valor = 2 * stats::pnorm(-abs(lp / se_lp)), efeito_pct = 100 * (exp(lp) - 1)))
    }
  }
  prim <- c("intervencao", "delta", "efeito_longo_prazo")
  tb[order(match(tb$termo, prim, nomatch = 99L)), ]
}
