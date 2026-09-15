# Decomposição: a série como soma (ou produto) de tendência, sazonal e resto.
#
# Dois métodos, um tipo de saída. A clássica é a do livro-texto, e é a que se
# ensina primeiro porque cada passo é uma conta que se faz à mão; a STL é a que
# se usa, porque deixa a sazonalidade mudar devagar e aguenta outlier. As duas
# saem normalizadas em `tr_series_decomp` (R/type.R), e o `series/component` e
# o gráfico não sabem de qual vieram.

#' Decomposição clássica por médias móveis.
#'
#' A multiplicativa pede série positiva: o sazonal é um fator, e um fator
#' calculado sobre zero ou negativo sai infinito ou com o sinal trocado, sem
#' erro. Recusar é o que manda a pessoa para a aditiva, ou para o log.
#' @export
tr_series_decompose <- function(serie, tipo = "aditiva") {
  tipo <- .tr_series_enum(tipo, c("aditiva", "multiplicativa"), "tipo")
  .tr_series_sazonal(serie, "series/decompose")
  .tr_series_sem_na(serie, "series/decompose")
  if (tipo == "multiplicativa" && any(serie <= 0)) {
    .tr_series_abort("tr_series_error_nonpositive",
                     paste0("A decomposição multiplicativa pede valores positivos, e a série tem ",
                            "%d <= 0. Use a aditiva."), sum(serie <= 0))
  }
  d <- stats::decompose(serie, type = if (tipo == "aditiva") "additive" else "multiplicative")
  .tr_series_decomp(serie, d$trend, d$seasonal, d$random, tipo, "clássica")
}

#' Decomposição STL (Cleveland et al., 1990).
#'
#' `janela_sazonal = 0` é "periódica": o mesmo padrão sazonal em todos os
#' anos, o que aproxima a clássica. Um número (ímpar, >= 7) é quantos anos
#' entram na suavização de cada estação — menor deixa a sazonalidade mudar
#' mais depressa. O zero como "periódica" evita um param de texto que
#' aceitasse "periodic" e mais nada.
#'
#' `robusta` troca a média pela suavização com pesos que ignoram outlier: um
#' mês de greve deixa de puxar a sazonalidade de todos os outros anos — e vai
#' parar no resto, que é onde ele deve aparecer.
#' @export
tr_series_stl <- function(serie, janela_sazonal = 0L, robusta = FALSE) {
  janela <- .tr_series_int(janela_sazonal, "janela_sazonal", min = 0)
  if (janela > 0L && janela < 7L) {
    .tr_series_option("janela_sazonal", janela, "0 (periódica) ou um inteiro >= 7")
  }
  .tr_series_sazonal(serie, "series/stl")
  .tr_series_sem_na(serie, "series/stl")
  s <- stats::stl(serie, s.window = if (janela == 0L) "periodic" else janela,
                  robust = isTRUE(robusta))
  comp <- function(nm) .tr_series_uni(s$time.series[, nm])
  .tr_series_decomp(serie, comp("trend"), comp("seasonal"), comp("remainder"), "aditiva", "STL")
}

#' Um componente da decomposição, de volta como série.
#'
#' `dessazonalizada` é a série sem o sazonal — o que se publica quando se diz
#' "o desemprego subiu em março, descontado o efeito do mês". Na
#' multiplicativa é a divisão, e não a subtração: subtrair um fator de 1,08
#' de um valor na casa das centenas não tira sazonalidade nenhuma.
#' @export
tr_series_component <- function(decomposicao, componente = "dessazonalizada") {
  componente <- .tr_series_enum(componente,
                                c("tendencia", "sazonal", "resto", "dessazonalizada"), "componente")
  d <- decomposicao
  out <- switch(componente,
    tendencia = d$tendencia, sazonal = d$sazonal, resto = d$resto,
    dessazonalizada = if (identical(d$tipo, "multiplicativa")) d$observado / d$sazonal
                      else d$observado - d$sazonal)
  .tr_series_uni(out)
}

#' Decomposição por REGRESSÃO: tendência polinomial e sazonalidade por dummies.
#'
#' O que a clássica e a STL não dão: coeficiente, erro-padrão e p-valor. Aqui
#' os componentes são ESTIMADOS, e por isso podem ser testados — é o caminho
#' que se ensina junto com a diferenciação, e o que responde "essa
#' sazonalidade existe mesmo?" com um número em vez de um gráfico.
#'
#' O `contraste` muda como os coeficientes são LIDOS, e não a decomposição.
#' Com `soma_zero`, cada coeficiente sazonal é o desvio do período em relação
#' à média; com `categoria_base`, é a diferença para o primeiro período (o
#' `summary()` do R). Em ambos o componente sazonal devolvido é centrado em
#' zero e a tendência absorve a média — senão a mesma série daria duas
#' decomposições diferentes conforme uma escolha de parametrização, que é
#' exatamente o tipo de resultado errado com cara de certo que a coleção
#' recusa.
#' @export
tr_series_regression <- function(serie, grau = 1L, sazonalidade = TRUE,
                                 contraste = "soma_zero", regressor = NULL) {
  grau <- .tr_series_int(grau, "grau", min = 0, max = 3)
  contraste <- .tr_series_enum(contraste, c("soma_zero", "categoria_base"), "contraste")
  if (!is.null(regressor)) {
    .tr_series_abort("tr_series_error_xreg_unsupported",
                     paste0("'series/regression' ainda não usa regressor externo. Desligue o fio da ",
                            "entrada 'regressor'."))
  }
  if (grau == 0L && !isTRUE(sazonalidade)) {
    .tr_series_abort("tr_series_error_empty_model",
                     paste0("'series/regression' com grau 0 e sem sazonalidade não tem nada a ",
                            "estimar. Suba o grau, ou ligue a sazonalidade."))
  }
  .tr_series_sem_na(serie, "series/regression")
  if (isTRUE(sazonalidade)) .tr_series_sazonal(serie, "series/regression")
  # Graus de liberdade, e não só tamanho: o guard tem de conhecer o número de
  # PARÂMETROS do modelo. Com `grau >= frequência` o ajuste chega a zero graus
  # residuais e nenhum coeficiente falta — o `anyNA(coef)` abaixo não pega, e o
  # que sai é R² ajustado NaN, p-valor NaN e uma decomposição de aparência
  # perfeita. Dois graus é o mínimo para que erro-padrão e p-valor queiram
  # dizer alguma coisa.
  n_saz <- if (isTRUE(sazonalidade)) as.integer(stats::frequency(serie)) - 1L else 0L
  n_par <- 1L + grau + n_saz
  if (length(serie) < n_par + 2L) {
    .tr_series_abort("tr_series_error_too_short",
                     paste0("'series/regression': a série tem %d observações e o modelo pede %d ",
                            "parâmetros (1 intercepto, %d de tendência, %d sazonais), o que deixa ",
                            "%d graus de liberdade residuais. Abaixo de 2 não há erro-padrão nem ",
                            "p-valor: baixe o grau, ou desligue a sazonalidade."),
                     length(serie), n_par, grau, n_saz, length(serie) - n_par)
  }

  dados <- data.frame(y = as.numeric(serie))
  tt <- seq_along(serie)
  for (g in seq_len(grau)) dados[[paste0("t", g)]] <- tt^g
  if (isTRUE(sazonalidade)) {
    f <- as.integer(stats::frequency(serie))
    dados$estacao <- factor(as.integer(stats::cycle(serie)), levels = seq_len(f),
                            labels = .tr_series_estacoes(f))
    # O contraste é posto NO FATOR, e não no `contrasts=` do `lm`: assim
    # qualquer reajuste feito a partir de `fit$model` (é o que o teste parcial
    # da significância faz) herda a mesma parametrização, sem ter de
    # recarregá-la.
    stats::contrasts(dados$estacao) <-
      if (contraste == "soma_zero") stats::contr.sum else stats::contr.treatment
  }

  fit <- stats::lm(y ~ ., data = dados)
  if (anyNA(stats::coef(fit))) {
    .tr_series_abort("tr_series_error_fit",
                     paste0("O ajuste ficou indeterminado (%d coeficientes sem estimativa): a série é ",
                            "curta demais para %d termos. Baixe o grau ou desligue a sazonalidade."),
                     sum(is.na(stats::coef(fit))), length(stats::coef(fit)))
  }

  # O sazonal sai da PARTE do preditor linear que vem das dummies, e não dos
  # coeficientes pelo nome: com `contr.sum` o último período não tem
  # coeficiente próprio (é menos a soma dos outros), e somar pelo nome o
  # perderia.
  saz <- rep(0, nrow(dados))
  if (isTRUE(sazonalidade)) {
    X <- stats::model.matrix(fit)
    cols <- grepl("^estacao", colnames(X))
    saz <- as.numeric(X[, cols, drop = FALSE] %*% stats::coef(fit)[cols])
    # Centra pela média do CICLO, e não da amostra: com anos completos as duas
    # coincidem, mas com anos incompletos a média da amostra pesa mais os meses
    # que aparecem mais vezes — e os doze efeitos deixariam de somar zero, que é
    # justamente o que esta decomposição promete.
    saz <- saz - mean(tapply(saz, stats::cycle(serie), mean))
  }
  como_ts <- function(v) {
    stats::ts(v, start = stats::start(serie), frequency = stats::frequency(serie))
  }
  structure(list(ajuste = fit, serie = serie, grau = grau,
                 sazonalidade = isTRUE(sazonalidade), contraste = contraste,
                 tendencia = como_ts(as.numeric(stats::fitted(fit)) - saz),
                 sazonal = como_ts(saz),
                 resto = como_ts(as.numeric(stats::residuals(fit)))),
            class = "tr_series_reg")
}
