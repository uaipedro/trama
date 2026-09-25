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
