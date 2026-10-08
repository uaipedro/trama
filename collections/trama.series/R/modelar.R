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
  interv <- .tr_series_intervencoes(serie, "series/arima")
  if (length(interv)) .tr_series_sem_na(serie, "series/arima (com intervenção)")
  if (isTRUE(automatico)) {
    # Com intervenções, a busca escolhe a ordem com os regressores na forma
    # imediata (o inovacional como pulso, o gradual sem δ); se houver
    # inovacional ou gradual, a ordem escolhida é reajustada com eles inteiros.
    X <- if (length(interv)) .tr_series_interv_matriz(lapply(interv, function(s) {
      s$dinamica <- "imediata"; s
    }), length(serie))
    # `xreg` só quando há: o `forecast` reavalia a chamada gravada, e um
    # `xreg = X` com X nulo o faria procurar um X que não existe mais.
    args <- list(quote(serie), seasonal = isTRUE(sazonal) && f > 1,
                 allowdrift = isTRUE(constante), allowmean = isTRUE(constante))
    if (!is.null(X)) args$xreg <- X
    fit <- .tr_series_ajustar(do.call(forecast::auto.arima, args), "series/arima")
    if (!length(interv)) return(fit)
    if (!any(vapply(interv, function(s) s$tipo == "inovacional" || s$dinamica == "gradual", NA))) {
      fit$tr_intervencoes <- list(lista = interv, gradual = NULL)
      return(fit)
    }
    a <- fit$arma
    cf <- names(stats::coef(fit))
    return(.tr_series_arima_interv(serie, interv, a[c(1, 6, 2)], a[c(3, 7, 4)],
                                   any(c("intercept", "drift") %in% cf)))
  }
  ordem <- c(.tr_series_int(p, "p", 0, 5), .tr_series_int(d, "d", 0, 2), .tr_series_int(q, "q", 0, 5))
  sazo <- c(.tr_series_int(P, "P", 0, 2), .tr_series_int(D, "D", 0, 1), .tr_series_int(Q, "Q", 0, 2))
  if (sum(sazo) > 0L) .tr_series_sazonal(serie, "series/arima (parte sazonal P, D, Q)", ciclos = 1L)
  if (length(interv)) return(.tr_series_arima_interv(serie, interv, ordem, sazo, constante))
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
  .tr_series_sem_intervencao(serie, "series/ets")
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
  .tr_series_sem_intervencao(serie, "series/holt_winters")
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
#'
#' Com `ajuste` (a saída de `series/regression`, `series/detrend` ou
#' `series/deseasonalize`) em vez de `modelo`, prevê a fórmula: o futuro de
#' `t`, `periodo` e `ano` sai da série; o do regressor, de `futuro`.
tr_series_forecast <- function(modelo = NULL, horizonte = 12L, intervalo = "normal", ajuste = NULL,
                               futuro = NULL, var = NULL, .seed = NULL) {
  h <- .tr_series_int(horizonte, "horizonte", min = 1, max = 1000)
  intervalo <- .tr_series_enum(intervalo, c("normal", "bootstrap"), "intervalo")
  if (sum(!is.null(modelo), !is.null(ajuste), !is.null(var)) != 1L) {
    .tr_series_abort("tr_series_error_bad_option",
                     paste0("'series/forecast': ligue um modelo (ARIMA, ETS, Holt-Winters), o ajuste de ",
                            "uma regressão da série OU um VAR/VECM — um dos três."))
  }
  if (!is.null(var)) {
    if (intervalo != "normal") .tr_series_option("intervalo", intervalo, "normal (VAR e VECM)")
    return(.tr_series_var_prever(var, h))
  }
  if (!is.null(ajuste)) {
    if (intervalo != "normal") {
      .tr_series_option("intervalo", intervalo, "normal (o bootstrap pede um modelo series/arima ou series/ets)")
    }
    return(.tr_series_prever_fit(ajuste, h, futuro))
  }
  # Modelo com intervenções: o futuro de cada regressor é conhecido (o pulso
  # volta a zero, o degrau fica, a rampa segue), e a coleção o monta.
  xf <- .tr_series_interv_futuro(modelo, h)
  if (intervalo == "normal") {
    return(.tr_series_ajustar(forecast::forecast(modelo, h = h, level = c(80, 95), xreg = xf),
                              "series/forecast"))
  }
  # Bootstrap dos resíduos (FPP3, sec. 5.5): 5000 trajetórias simuladas com
  # erros reamostrados; os limites são quantis empíricos. Só ARIMA e ETS têm
  # simulação no `forecast`; Holt-Winters do `stats` não.
  if (!is.null(xf)) {
    .tr_series_option("intervalo", intervalo,
                      "normal (o bootstrap do forecast não simula com os regressores das intervenções)")
  }
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
tr_series_accuracy <- function(previsao, real = NULL, reais = NULL) {
  if (!is.null(reais)) {
    if (!inherits(previsao, "mforecast")) {
      .tr_series_abort("tr_series_error_not_a_series",
                       "'reais' (séries múltiplas) é para a previsão de um VAR/VECM; aqui ligue 'real'.")
    }
    real <- reais
  }
  # VAR/VECM: as medidas de cada série, empilhadas com a coluna `serie`. A
  # série real, se vier, é múltipla (`series/mts`), e cada coluna confere a
  # previsão da série de mesmo nome.
  if (inherits(previsao, "mforecast")) {
    if (!is.null(real)) .tr_series_guard_mts(real)
    return(do.call(rbind, lapply(names(previsao$forecast), function(nm) {
      r <- if (!is.null(real)) {
        if (!nm %in% colnames(real)) {
          .tr_series_abort("tr_series_error_no_overlap",
                           "A série real não tem a coluna '%s', que a previsão tem.", nm)
        }
        .tr_series_uni(real[, nm])
      }
      tibble::add_column(tr_series_accuracy(previsao$forecast[[nm]], r), serie = nm, .before = 1L)
    })))
  }
  if (!is.null(real) && stats::is.mts(real)) {
    .tr_series_abort("tr_series_error_not_a_series",
                     "A série real é múltipla e a previsão é de uma série só: escolha a coluna em 'series/pick'.")
  }
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
