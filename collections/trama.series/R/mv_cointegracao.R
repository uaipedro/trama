# Grupo multivariado "cointegracao": nós e docs (ver mv_registro.R).
#
# Três blocos: Engle-Granger (um teste de resíduos), Johansen (quantas relações
# de cointegração, por traço ou autovalor) e VECM (o modelo com posto dado).
# A conta do Johansen e do VECM é do `urca::ca.jo`; o VECM tira os coeficientes
# do `urca::cajorls` e a previsão do `vars::vec2var`.

# O p-valor de Engle-Granger sai da superfície de MacKinnon para N variáveis.
# `urca::punitroot()` não serve: fixa `niv = 1` (raiz unitária de UMA série) e
# usa o `N` como tamanho de amostra. A função interna do urca aceita o número
# de variáveis (`niv`), e é ela que este bloco usa. Conferida a 1e-10 contra o
# `punitroot` no caso N = 1.
.tr_series_urcval <- function(stat, nobs, niv, trend) {
  f <- utils::getFromNamespace(".urcval", "urca")
  out <- NULL
  utils::capture.output(out <- f(arg = stat, nobs = nobs, niv = niv,
                                 itt = 1L, itv = if (trend == "ct") 3L else 2L, nc = 2L))
  out
}

#' P-valor de Engle-Granger para `k` variáveis (dependente + regressoras).
#' @noRd
.tr_series_eg_p <- function(stat, n, k, trend = "c") {
  if (k < 2L || k > 6L) {
    .tr_series_abort("tr_series_error_bad_option",
                     "'series/engle_granger': a superfície de MacKinnon vai de 2 a 6 variáveis; chegaram %d.", k)
  }
  .tr_series_urcval(stat, nobs = n, niv = k, trend = trend)
}

#' Engle-Granger: as séries são cointegradas?
#'
#' Regressão por MQO da resposta nas outras séries (com constante, ou constante e
#' tendência); ADF sem constante nos resíduos; p-valor pela superfície de
#' MacKinnon para N variáveis. Não se usam os críticos do ADF comum: os resíduos
#' de uma regressão estimada têm distribuição própria.
#' @export
tr_series_engle_granger <- function(series, resposta = "", deterministico = "constante",
                                    defasagens = 0L) {
  .tr_series_guard_mts(series)
  .tr_series_sem_na(series, "series/engle_granger")
  y_nome <- .tr_series_obrigatorio(resposta, "resposta")
  if (!y_nome %in% colnames(series)) .tr_series_option("resposta", y_nome, colnames(series))
  det <- .tr_series_enum(deterministico, c("constante", "tendência"), "deterministico")
  .tr_series_minimo(series[, 1], 20L, "series/engle_granger", "o teste de cointegração")
  k <- ncol(series)
  x <- as.matrix(series)
  n <- nrow(x)
  regs <- setdiff(colnames(x), y_nome)
  Z <- if (det == "tendência") cbind(constante = 1, tendencia = seq_len(n), x[, regs, drop = FALSE])
       else cbind(constante = 1, x[, regs, drop = FALSE])
  fit <- stats::lm.fit(Z, x[, y_nome])
  res <- unname(fit$residuals)
  m <- length(res)
  p_lags <- .tr_series_int(defasagens, "defasagens", min = 0L, max = 50L)
  if (p_lags == 0L) p_lags <- as.integer(trunc((m - 1)^(1 / 3)))
  a <- urca::ur.df(res, type = "none", lags = p_lags, selectlags = "AIC")
  stat <- a@teststat[[1]]
  p <- .tr_series_eg_p(stat, m, k, trend = if (det == "tendência") "ct" else "c")
  coefs <- tibble::tibble(termo = names(fit$coefficients), estimativa = unname(fit$coefficients))
  .tr_series_teste(
    "Engle-Granger", "as séries não são cointegradas", stat, "t (ADF nos resíduos)",
    p_valor = p,
    sentido = "menor",
    conclusao_sim = "cointegradas",
    conclusao_nao = "não há evidência de cointegração",
    nota = sprintf(paste0("resposta %s, regressoras %s; %s; defasagens do ADF = %d (AIC); ",
                          "p-valor pela superfície de MacKinnon para N = %d variáveis e %d observações"),
                   y_nome, paste(regs, collapse = ", "),
                   if (det == "tendência") "constante e tendência" else "constante",
                   p_lags, k, m),
    fonte = "Engle & Granger (1987); MacKinnon (1996)",
    extra = list(coeficientes = coefs, defasagens = p_lags, observacoes = m))
}

# Os três casos do `urca::ca.jo`, pelo que FAZEM (conferido no código do
# ca.jo): `const` = constante só dentro da relação de cointegração; `trend` =
# tendência dentro da relação e constante livre no VECM; `none` = constante
# livre no VECM (tendência linear nos dados) — não é "sem determinístico".
.TR_SERIES_ECDET <- c("constante restrita" = "const", "tendência restrita" = "trend",
                      "constante livre" = "none")

#' Mapeia o determinístico do usuário para o `ecdet` do `urca::ca.jo`.
#' @noRd
.tr_series_ecdet <- function(deterministico) {
  d <- .tr_series_enum(deterministico, names(.TR_SERIES_ECDET), "deterministico")
  .TR_SERIES_ECDET[[d]]
}

#' Johansen: quantas relações de cointegração? Traço ou autovalor máximo.
#'
#' Uma linha por hipótese de posto, de r = 0 até k - 1. Pelo traço, a hipótese
#' é `r <= r0` contra `r > r0`; pelo autovalor máximo, `r = r0` contra
#' `r = r0 + 1`. A decisão é a 5%, e o posto sugerido é o primeiro r0 em que
#' a hipótese não é rejeitada (a regra sequencial usual).
#' @export
tr_series_johansen <- function(series, metodo = "traco", defasagens = 2L,
                               deterministico = "constante restrita", sazonal = FALSE) {
  .tr_series_guard_mts(series)
  .tr_series_sem_na(series, "series/johansen")
  met <- .tr_series_enum(metodo, c("traco", "autovalor"), "metodo")
  K <- .tr_series_int(defasagens, "defasagens", min = 2L, max = 50L)
  ecdet <- .tr_series_ecdet(deterministico)
  k <- ncol(series)
  .tr_series_minimo(series[, 1], 5L * (K + k), "series/johansen", "o teste de Johansen")
  saz <- if (isTRUE(sazonal)) {
    f <- stats::frequency(series)
    if (f <= 1) .tr_series_abort("tr_series_error_no_season", "'series/johansen': sazonais pedem série com ciclo (frequência > 1).")
    as.integer(f)
  }
  ca <- .tr_series_ajustar(
    urca::ca.jo(series, type = if (met == "traco") "trace" else "eigen", ecdet = ecdet,
                K = K, spec = "longrun", season = saz),
    "series/johansen")
  # O `ca.jo` põe a estatística e a linha de críticos na MESMA ordem: a linha i
  # é a hipótese r <= k - i. Para r0 = 0..k-1, então, a linha é k - r0.
  r0 <- seq_len(k) - 1L
  i <- k - r0
  crit <- ca@cval[i, , drop = FALSE]
  est <- unname(ca@teststat[i])
  rejeita <- est > crit[, "5pct"]
  decisao <- ifelse(rejeita, "rejeita H0", "não rejeita H0")
  nao <- which(!rejeita)
  posto <- if (length(nao)) r0[[nao[[1]]]] else k
  nota <- rep("", k)
  nota[[if (length(nao)) nao[[1]] else k]] <- if (length(nao)) {
    sprintf("primeiro r em que não se rejeita H0 a 5%%: posto sugerido r = %d", posto)
  } else {
    "todas as hipóteses rejeitadas: posto k, série estacionária em nível"
  }
  hip <- if (met == "traco") ifelse(r0 == 0L, "r = 0", paste0("r <= ", r0)) else paste0("r = ", r0)
  tibble::tibble(
    hipotese = hip,
    estatistica = est,
    critico_10 = unname(crit[, "10pct"]),
    critico_5 = unname(crit[, "5pct"]),
    critico_1 = unname(crit[, "1pct"]),
    decisao_5 = decisao,
    nota = nota)
}

#' VECM com posto dado: coeficientes do `cajorls`, previsão do `vec2var`.
#'
#' O posto é informado (r >= 1), não escolhido aqui: o `series/johansen` diz o
#' posto sugerido. O ajuste viaja como `series/var` (tipo `VECM`), com o
#' `cajorls` e o `ca.jo` guardados para o card e a previsão.
#' @export
tr_series_vecm <- function(series, posto = 1L, defasagens = 2L, deterministico = "constante restrita",
                           sazonal = FALSE) {
  .tr_series_guard_mts(series)
  .tr_series_sem_na(series, "series/vecm")
  k <- ncol(series)
  r <- .tr_series_int(posto, "posto", min = 1L, max = k - 1L)
  K <- .tr_series_int(defasagens, "defasagens", min = 2L, max = 50L)
  ecdet <- .tr_series_ecdet(deterministico)
  .tr_series_minimo(series[, 1], 5L * (K + k), "series/vecm", "o VECM")
  saz <- if (isTRUE(sazonal)) {
    f <- stats::frequency(series)
    if (f <= 1) .tr_series_abort("tr_series_error_no_season", "'series/vecm': sazonais pedem série com ciclo (frequência > 1).")
    as.integer(f)
  }
  ca <- .tr_series_ajustar(
    urca::ca.jo(series, type = "trace", ecdet = ecdet, K = K, spec = "longrun", season = saz),
    "series/vecm")
  rls <- .tr_series_ajustar(urca::cajorls(ca, r = r), "series/vecm")
  vec <- .tr_series_ajustar(vars::vec2var(ca, r = r), "series/vecm")
  .tr_series_var(vec, series, "VECM",
                 vecm = list(rls = rls, posto = r, ca = ca),
                 nota = sprintf("posto r = %d informado; confira com series/johansen", r))
}

.tr_series_nos_mv_cointegracao <- function() {
  MV <- "series/mts"; VAR <- "series/var"; TE <- "data/test"; T <- "data/table"
  I <- trama::tr_param_int; E <- trama::tr_param_enum; B <- trama::tr_param_bool; P <- trama::tr_param
  icone <- function(n) trama::tr_icon(n)
  list(
    trama::tr_node("series/engle_granger", fn = tr_series_engle_granger, label = "Engle-Granger",
      pressupostos = .tr_series_doc("series/engle_granger")$pressupostos,
      referencias = .tr_series_doc("series/engle_granger")$referencias,
      category = "serie_mv_testes", icon = icone("test-tube"),
      description = "Cointegração de Engle-Granger: a regressão dos resíduos tem raiz unitária?",
      inputs = list(series = MV), outputs = list(out = TE),
      params = list(
        resposta = P("text", "", label = "Resposta", example = "consumo"),
        deterministico = E("constante", c("constante", "tendência"), label = "Determinístico"),
        defasagens = I(0L, min = 0L, max = 50L, label = "Defasagens do ADF", vazio = 0L,
                       example = "0 = pela regra automática")),
      help = .tr_series_ajuda(r"---[
Testa se a série RESPOSTA é cointegrada com as demais, isto é, se existe uma
combinação linear delas que é estacionária.

A regressão da resposta nas outras séries é estimada por mínimos quadrados. O
teste é o ADF SEM constante nos resíduos dessa regressão, com H0 = não há
cointegração. Rejeitar conclui cointegradas.

### Por que não o ADF comum

Os resíduos de uma regressão já estimada são MENORES do que um resíduo qualquer:
o ajuste os aproxima de zero. Os críticos do ADF comum são otimistas nesse caso
e rejeitam demais. O p-valor sai da superfície de MacKinnon para N variáveis
(N = resposta mais regressoras), que é a distribuição certa.

### Limites

Com mais de duas séries, o teste dá UMA relação, e o resultado depende de qual
série é a resposta. Para contar as relações de uma vez, use o `series/johansen`.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.
]---", r"---[
- **Resposta** — a série explicada; as outras da série múltipla são as
  regressoras.
- **Determinístico** — constante (padrão), ou constante com tendência na regressão.
- **Defasagens do ADF** — teto das defasagens do ADF nos resíduos; 0 usa a regra
  automática, e o AIC escolhe dentro dele.
]---", r"---[
Um teste (`data/test`). A estatística é o t do ADF nos resíduos; o p-valor é o
de MacKinnon para N variáveis, e a decisão a 5% sai dele.
]---", r"---[
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac")) |>
  tr_add("eg", "series/engle_granger", resposta = "dax", from = "j")
]---", r"---[
`series/johansen`, que conta as relações de cointegração de uma vez; `series/vecm`,
para modelar as séries cointegradas; `series/adf` e `series/kpss`, para checar que
cada série é I(1) antes.
]---", teste = TRUE)),

    trama::tr_node("series/johansen", fn = tr_series_johansen, label = "Johansen",
      pressupostos = .tr_series_doc("series/johansen")$pressupostos,
      referencias = .tr_series_doc("series/johansen")$referencias,
      category = "serie_mv_testes", icon = icone("test-tube"),
      description = "Johansen: quantas relações de cointegração há, pelo traço ou pelo autovalor máximo.",
      inputs = list(series = MV), outputs = list(out = T),
      params = list(
        metodo = E("traco", c("traco", "autovalor"), label = "Estatística"),
        defasagens = I(2L, min = 2L, max = 50L, label = "Defasagens (K, em nível)", example = "2"),
        deterministico = E("constante restrita", names(.TR_SERIES_ECDET), label = "Determinístico"),
        sazonal = B(FALSE, label = "Dummies sazonais")),
      help = .tr_series_ajuda(r"---[
Conta quantas relações de cointegração há entre as séries, pelo procedimento de
Johansen (1991), com o VAR em nível de K defasagens.

Para cada posto r0 = 0, 1, ... até k - 1, a tabela traz a estatística, os
críticos a 10, 5 e 1% e a decisão a 5%. O posto sugerido é o primeiro r0 em que
a hipótese não é rejeitada.

### Traço e autovalor

O **traço** testa `r <= r0` contra `r > r0`; o **autovalor máximo** testa
`r = r0` contra `r = r0 + 1`. Quando discordam, o traço é o mais citado e é o
padrão aqui.

### Determinísticos

Três casos, pelo que entra em cada parte do modelo:

- **constante restrita** (padrão) — a constante só dentro da relação de
  cointegração: as séries não têm tendência, e o equilíbrio tem nível.
- **constante livre** — constante solta no VECM: as séries têm tendência
  linear, a relação de cointegração não.
- **tendência restrita** — tendência dentro da relação e constante livre: a
  própria relação de equilíbrio tem tendência.

Os críticos mudam com o caso. Dummies sazonais entram centradas, e pedem série
com ciclo.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.
]---", r"---[
- **Estatística** — traço (padrão) ou autovalor máximo.
- **Defasagens (K)** — defasagens do VAR em nível, no mínimo 2 (o VECM tem K - 1
  em diferença).
- **Determinístico** — constante restrita, constante livre ou tendência
  restrita (ver acima).
- **Dummies sazonais** — centradas por período, para série com ciclo.
]---", r"---[
Uma tabela (`data/table`) com uma linha por posto: `hipotese`, `estatistica`,
`critico_10`, `critico_5`, `critico_1`, `decisao_5` e `nota`, que marca o posto
sugerido.
]---", r"---[
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac")) |>
  tr_add("jo", "series/johansen", metodo = "traco", defasagens = 2L, from = "j")
]---", r"---[
`series/engle_granger` para uma resposta só; `series/vecm`, com o posto
escolhido aqui.
]---")),

    trama::tr_node("series/vecm", fn = tr_series_vecm, label = "VECM",
      pressupostos = .tr_series_doc("series/vecm")$pressupostos,
      referencias = .tr_series_doc("series/vecm")$referencias,
      category = "serie_mv", icon = icone("network"),
      description = "Modelo de correção de erro (VECM) com o posto de cointegração informado.",
      inputs = list(series = MV), outputs = list(out = VAR),
      params = list(
        posto = I(1L, min = 1L, max = 10L, label = "Posto (r)", example = "1"),
        defasagens = I(2L, min = 2L, max = 50L, label = "Defasagens (K, em nível)", example = "2"),
        deterministico = E("constante restrita", names(.TR_SERIES_ECDET), label = "Determinístico"),
        sazonal = B(FALSE, label = "Dummies sazonais")),
      help = .tr_series_ajuda(r"---[
Ajusta um modelo de correção de erro (VECM) para séries cointegradas: as
diferenças de cada série são explicadas pelas diferenças passadas e pelo desvio
da relação de longo prazo (o vetor de cointegração).

O posto r é o número de relações de cointegração e é INFORMADO aqui. Quem o
escolhe é o `series/johansen`, pelo posto sugerido. Os coeficientes de ajuste
e o vetor de cointegração saem do `urca::cajorls`; a previsão e os impulsos
saem do `vars::vec2var`.

O resultado é um ajuste de `series/var`, com o tipo VECM: o card mostra os
coeficientes de cada equação e a previsão funciona como nos VARs.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.
]---", r"---[
- **Posto (r)** — número de relações de cointegração, de 1 a k - 1.
- **Defasagens (K)** — defasagens do VAR em nível, no mínimo 2.
- **Determinístico** — constante restrita (só na relação de cointegração),
  constante livre (no VECM: séries com tendência) ou tendência restrita
  (tendência na relação, constante livre).
- **Dummies sazonais** — centradas por período, para série com ciclo.
]---", r"---[
Um ajuste multivariado (`series/var`, tipo VECM).
]---", r"---[
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac")) |>
  tr_add("v", "series/vecm", posto = 1L, defasagens = 2L, from = "j")
]---", r"---[
`series/johansen`, para escolher o posto; `series/forecast`, para prever.
]---"))
  )
}

.tr_series_docs_mv_cointegracao <- function() {
  R <- trama::tr_ref
  P <- .tr_series_P
  I <- .tr_series_impl
  johansen <- R(autores = "Johansen, S.", ano = 1991,
                titulo = "Estimation and hypothesis testing of cointegration vectors in Gaussian vector autoregressive models",
                fonte = "Econometrica, 59(6), 1551-1580", doi = "10.2307/2938278", papel = "teoria")
  engle <- R(autores = c("Engle, R. F.", "Granger, C. W. J."), ano = 1987,
             titulo = "Co-integration and error correction: representation, estimation, and testing",
             fonte = "Econometrica, 55(2), 251-276", doi = "10.2307/1913236", papel = "teoria")
  jj <- R(autores = c("Johansen, S.", "Juselius, K."), ano = 1990,
          titulo = "Maximum likelihood estimation and inference on cointegration: with applications to the demand for money",
          fonte = "Oxford Bulletin of Economics and Statistics, 52(2), 169-210",
          doi = "10.1111/j.1468-0084.1990.mp52002003.x", papel = "teoria")
  lutkepohl <- R(autores = "Lütkepohl, H.", ano = 2005,
                 titulo = "New Introduction to Multiple Time Series Analysis",
                 fonte = "Berlin: Springer", doi = "10.1007/978-3-540-27752-1", papel = "livro-texto")
  list(
    "series/engle_granger" = list(
      pressupostos = list(
        P("As séries são todas I(1): se a resposta ou alguma regressora for estacionária, a regressão é espúria e o teste não vale.",
          verificar = c("series/adf", "series/kpss"),
          se_falhar = "teste cada série com series/adf e series/kpss; só faça Engle-Granger com séries I(1)"),
        P("Há no máximo uma relação de cointegração, e o resultado depende de qual série é a resposta.",
          verificar = "series/johansen",
          se_falhar = "use series/johansen, que conta as relações de uma vez")),
      referencias = list(engle, I("urca", "ur.df", nota = "ADF nos resíduos; p-valor pela superfície de MacKinnon (1996) via função interna do urca, com N = número de variáveis (punitroot fixa N = 1)."))),
    "series/johansen" = list(
      pressupostos = list(
        P("As séries são I(1): o teste conta relações de cointegração e não serve para séries estacionárias em nível.",
          verificar = c("series/adf", "series/kpss"),
          se_falhar = "teste cada série e diferencie as I(1) antes de chamar series/johansen"),
        P("Os resíduos do VAR são ruído branco: K pequeno demais deixa autocorrelação e infla a estatística.",
          verificar = "series/portmanteau_mv",
          se_falhar = "aumente K (defasagens em nível)")),
      referencias = list(johansen, jj, I("urca", "ca.jo"))),
    "series/vecm" = list(
      pressupostos = list(
        P("O posto informado é o número de relações de cointegração: um posto errado troca o longo prazo por ruído.",
          verificar = "series/johansen",
          se_falhar = "rode series/johansen e use o posto sugerido"),
        P("As séries são I(1), e o VAR em nível tem K defasagens que deixam os resíduos sem autocorrelação.",
          verificar = c("series/adf", "series/kpss", "series/portmanteau_mv"),
          se_falhar = "teste a ordem de integração e aumente K se os resíduos saírem correlacionados")),
      referencias = list(johansen, lutkepohl,
                         I("urca", "cajorls"), I("vars", "vec2var")))
  )
}
