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
#'
#' `sem_tendencia` é o espelho: a série sem a tendência, com a sazonalidade
#' dentro (sazonal + resto, ou sazonal × resto). É o "estimo a tendência e
#' subtraio" feito às claras, e a mesma regra da divisão vale — na
#' multiplicativa a tendência é o NÍVEL, e a série sem ela é a razão.
#' @export
tr_series_component <- function(decomposicao, componente = "dessazonalizada") {
  componente <- .tr_series_enum(componente,
                                c("tendencia", "sazonal", "resto", "dessazonalizada", "sem_tendencia",
                                  "regressor"),
                                "componente")
  d <- decomposicao
  if (componente == "regressor" && is.null(d$regressor)) {
    .tr_series_abort("tr_series_error_no_component",
                     paste0("'series/component': a decomposição (%s) não tem componente de regressor. ",
                            "Ele só existe numa 'series/regression' com a entrada 'regressor' ligada."),
                     d$metodo %||% "?")
  }
  mult <- identical(d$tipo, "multiplicativa")
  out <- switch(componente,
    tendencia = d$tendencia, sazonal = d$sazonal, resto = d$resto, regressor = d$regressor,
    dessazonalizada = if (mult) d$observado / d$sazonal else d$observado - d$sazonal,
    sem_tendencia = if (mult) d$observado / d$tendencia else d$observado - d$tendencia)
  out <- .tr_series_uni(out)
  # O nome legível do componente viaja com a série: é o que o `series/plot`
  # escreve na legenda quando ela entra como `sobreposta`. Atributo e não
  # classe: a série continua um `ts` comum para todo o resto.
  attr(out, "tr_series_nome") <- c(tendencia = "tendência", sazonal = "sazonal", resto = "resto",
                                   dessazonalizada = "dessazonalizada", sem_tendencia = "sem tendência",
                                   regressor = "regressor")[[componente]]
  out
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
#'
#' O `regressor` é uma covariável externa (outra série, no mesmo tempo) que
#' entra no `lm` ao lado da tendência e das dummies: o coeficiente dele sai na
#' tabela com erro-padrão e p-valor, e os componentes passam a ser os
#' "descontado o regressor" (o efeito β·x é o quarto componente, à parte da
#' tendência). É a pergunta "a tendência continua depois de
#' controlar pela renda?" respondida no mesmo ajuste.
#' @export
tr_series_regression <- function(serie, grau = 1L, sazonalidade = TRUE,
                                 contraste = "soma_zero", regressor = NULL,
                                 erro = "independente", ar = 1L, ma = 0L,
                                 excluir = "", remover_ns = FALSE, alfa = 0.05) {
  grau <- .tr_series_int(grau, "grau", min = 0, max = 3)
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
  if (grau == 0L && !isTRUE(sazonalidade) && is.null(xreg)) {
    .tr_series_abort("tr_series_error_empty_model",
                     paste0("'series/regression' com grau 0, sem sazonalidade e sem regressor não ",
                            "tem nada a estimar. Suba o grau, ou ligue a sazonalidade."))
  }
  .tr_series_sem_na(serie, "series/regression")
  if (isTRUE(sazonalidade)) .tr_series_sazonal(serie, "series/regression")
  remover_ns <- isTRUE(remover_ns)
  alfa <- suppressWarnings(as.numeric(alfa))
  if (remover_ns && (length(alfa) != 1L || is.na(alfa) || alfa <= 0 || alfa >= 1)) {
    .tr_series_abort("tr_series_error_bad_option", "Param 'alfa': um número entre 0 e 1, como 0.05.")
  }
  fora <- if (isTRUE(sazonalidade)) .tr_series_estacoes_fora(serie, excluir) else character()
  # Graus de liberdade, e não só tamanho: o guard tem de conhecer o número de
  # PARÂMETROS do modelo. Com `grau >= frequência` o ajuste chega a zero graus
  # residuais e nenhum coeficiente falta — o `anyNA(coef)` abaixo não pega, e o
  # que sai é R² ajustado NaN, p-valor NaN e uma decomposição de aparência
  # perfeita. Dois graus é o mínimo para que erro-padrão e p-valor queiram
  # dizer alguma coisa.
  n_saz <- if (isTRUE(sazonalidade)) as.integer(stats::frequency(serie)) - max(1L, length(fora)) else 0L
  n_x <- if (is.null(xreg)) 0L else 1L
  n_par <- 1L + grau + n_saz + n_x
  if (length(serie) < n_par + 2L) {
    .tr_series_abort("tr_series_error_too_short",
                     paste0("'series/regression': a série tem %d observações e o modelo pede %d ",
                            "parâmetros (1 intercepto, %d de tendência, %d sazonais, %d do ",
                            "regressor), o que deixa %d graus de liberdade residuais. Abaixo de 2 ",
                            "não há erro-padrão nem p-valor: baixe o grau, ou desligue a sazonalidade."),
                     length(serie), n_par, grau, n_saz, n_x, length(serie) - n_par)
  }

  # O ajuste é função das estações que ficam FORA: a eliminação dos meses não
  # significativos reajusta até todo mês que sobra ter p <= alfa.
  ajustar <- function(fora) {
    dados <- data.frame(y = as.numeric(serie))
    tt <- seq_along(serie)
    for (g in seq_len(grau)) dados[[paste0("t", g)]] <- tt^g
    if (!is.null(xreg)) dados$regressor <- xreg
    if (isTRUE(sazonalidade) && length(fora)) {
      # Estações fora do modelo viram UM nível base, "demais": o efeito delas é
      # o mesmo, e cada dummy que fica é a diferença para esse grupo. Por isso
      # o contraste é o de categoria base — com soma zero, "tirar um mês" não
      # quer dizer "esse mês não tem efeito".
      rot <- .tr_series_estacoes(as.integer(stats::frequency(serie)))
      lab <- rot[as.integer(stats::cycle(serie))]
      lab[lab %in% fora] <- "demais"
      dados$estacao <- factor(lab, levels = c("demais", setdiff(rot, fora)))
      stats::contrasts(dados$estacao) <- stats::contr.treatment
    } else if (isTRUE(sazonalidade)) {
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
    if (!is.null(xreg) && is.na(stats::coef(fit)[["regressor"]])) {
      .tr_series_abort("tr_series_error_fit",
                       paste0("O regressor é colinear com a tendência e/ou a sazonalidade: ele é uma ",
                              "combinação exata dos outros termos, e seu coeficiente não se estima. ",
                              "Ligue outro regressor, ou baixe o grau / desligue a sazonalidade."))
    }
    X <- stats::model.matrix(fit)
    # Os rótulos dos termos vão junto com a matriz: é por `assign` que o F de
    # Wald acha as colunas de um bloco quando o ajuste é GLS.
    attr(X, "rotulos") <- attr(stats::terms(fit), "term.labels")
    aviso <- NULL
    if (erro == "arma" && !anyNA(stats::coef(fit))) {
      # Mínimos quadrados generalizados com erro ARMA(p, q) (Morettin & Toloi
      # 2006; Pinheiro & Bates 2000), por máxima verossimilhança — e não REML —
      # para que o ajuste seja a MESMA função que o `stats::arima(xreg = )`
      # maximiza, que é o oráculo dos testes. O `lm` acima fica só como checagem
      # de posto; os F passam a ser de Wald sobre o `vcov` do GLS.
      # Sem resíduo não há erro a modelar: a série que o polinômio e as dummies
      # reproduzem exatamente deixa o GLS singular, e isso não é "não convergiu".
      rss <- sum(stats::residuals(fit)^2)
      if (rss <= 1e-12 * max(1, sum((dados$y - mean(dados$y))^2))) {
        .tr_series_abort("tr_series_error_singular_fit",
                         paste0("'series/regression': o modelo reproduz a série exatamente (resíduo ",
                                "nulo), e sem resíduo não há erro ARMA a estimar. Use erro = ",
                                "'independente', ou baixe o grau / desligue a sazonalidade."))
      }
      fit <- tryCatch(
        nlme::gls(y ~ ., data = dados, method = "ML",
                  correlation = nlme::corARMA(p = ar, q = ma, form = ~ 1)),
        error = function(e) {
          if (grepl("singular", conditionMessage(e), fixed = TRUE)) {
            .tr_series_abort("tr_series_error_singular_fit",
                             paste0("'series/regression': o ajuste com erro ARMA(%d, %d) ficou ",
                                    "singular (%s) — resíduo quase nulo ou termos colineares. ",
                                    "Baixe o grau, desligue a sazonalidade ou use erro = 'independente'."),
                             ar, ma, conditionMessage(e))
          }
          .tr_series_abort("tr_series_error_fit",
                           paste0("O ajuste com erro ARMA(%d, %d) não convergiu (%s). Baixe a ",
                                  "ordem do erro, ou use erro = 'independente'."),
                           ar, ma, conditionMessage(e))
        })
      aviso <- .tr_series_aviso_raiz(fit, ar)
    }
    if (anyNA(stats::coef(fit))) {
      .tr_series_abort("tr_series_error_fit",
                       paste0("O ajuste ficou indeterminado (%d coeficientes sem estimativa): a série é ",
                              "curta demais para %d termos. Baixe o grau ou desligue a sazonalidade."),
                       sum(is.na(stats::coef(fit))), length(stats::coef(fit)))
    }
    list(fit = fit, X = X, dados = dados, aviso = aviso)
  }
  # A eliminação parte dos desvios em relação à média do ano (soma zero): com
  # categoria base, a primeira estação seria a referência e nunca sairia.
  contraste_pedido <- contraste
  if (remover_ns) contraste <- "soma_zero"
  a <- ajustar(fora)
  removidos <- character()
  if (remover_ns && isTRUE(sazonalidade)) {
    rot <- .tr_series_estacoes(as.integer(stats::frequency(serie)))
    repeat {
      p <- .tr_series_p_estacoes(a$fit, a$X, a$dados)
      if (!length(p) || max(p) <= alfa) break
      # Sem nenhuma estação sobrando, o modelo fica sem sazonalidade — e sem
      # nada, se não houver tendência nem regressor: aí para antes.
      if (length(p) == 1L && grau == 0L && is.null(xreg)) break
      pior <- names(p)[which.max(p)]
      fora <- c(fora, pior); removidos <- c(removidos, pior)
      if (all(rot %in% fora)) { sazonalidade <- FALSE; fora <- character() }
      a <- ajustar(fora)
      if (!isTRUE(sazonalidade)) break
    }
  }
  if (contraste != contraste_pedido) {
    contraste <- contraste_pedido
    if (!length(fora)) a <- ajustar(fora)
  }
  if (length(fora)) contraste <- "categoria_base"
  fit <- a$fit; X <- a$X; dados <- a$dados; aviso <- a$aviso

  # O sazonal sai da PARTE do preditor linear que vem das dummies, e não dos
  # coeficientes pelo nome: com `contr.sum` o último período não tem
  # coeficiente próprio (é menos a soma dos outros), e somar pelo nome o
  # perderia.
  saz <- rep(0, nrow(dados))
  if (isTRUE(sazonalidade)) {
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
  # O efeito do regressor é componente PRÓPRIO, fora da tendência: a
  # tendência fica função só do tempo (intercepto + polinômio), e a
  # identidade passa a ser série = tendência + sazonal + regressor + resto.
  efeito <- if (is.null(xreg)) NULL else stats::coef(fit)[["regressor"]] * xreg
  structure(list(ajuste = fit, serie = serie, grau = grau,
                 sazonalidade = isTRUE(sazonalidade), contraste = contraste,
                 efeito_regressor = if (!is.null(efeito)) como_ts(efeito),
                 erro = erro, ordem = if (erro == "arma") c(ar = ar, ma = ma) else NULL,
                 aviso = aviso,
                 estacoes_fora = if (length(fora)) fora else removidos,
                 estacoes_removidas = removidos, alfa = if (remover_ns) alfa,
                 matriz = X,
                 tendencia = como_ts(as.numeric(stats::fitted(fit)) - saz - (efeito %||% 0)),
                 sazonal = como_ts(saz),
                 resto = como_ts(as.numeric(stats::residuals(fit)))),
            class = "tr_series_reg")
}

#' O regressor alinhado à série, ou erro dizendo por que não se alinha.
#'
#' Tem de COBRIR a série inteira, e não só cruzar com ela: recortar a série
#' para a interseção mudaria a decomposição sem que o card dissesse — o
#' `series/window` antes é o jeito de fazer isso às claras.
#' @noRd
.tr_series_regressor <- function(serie, regressor) {
  fs <- stats::frequency(serie); fr <- stats::frequency(regressor)
  if (!isTRUE(all.equal(fs, fr))) {
    .tr_series_abort("tr_series_error_frequency_mismatch",
                     paste0("'series/regression': a série tem frequência %g e o regressor, %g. Leve o ",
                            "regressor à frequência da série com 'series/aggregate'."), fs, fr)
  }
  ts_s <- stats::tsp(serie); ts_r <- stats::tsp(regressor)
  eps <- 1e-8
  if (ts_r[[1]] > ts_s[[1]] + eps || ts_r[[2]] < ts_s[[2]] - eps) {
    .tr_series_abort("tr_series_error_no_overlap",
                     paste0("'series/regression': o regressor (%s a %s) não cobre a série inteira ",
                            "(%s a %s). Recorte a série com 'series/window'."),
                     .tr_series_rotulo(stats::start(regressor), fr), .tr_series_rotulo(stats::end(regressor), fr),
                     .tr_series_rotulo(stats::start(serie), fs), .tr_series_rotulo(stats::end(serie), fs))
  }
  x <- stats::window(regressor, start = ts_s[[1]], end = ts_s[[2]])
  .tr_series_sem_na(x, "series/regression (regressor)")
  as.numeric(x)
}

#' Aviso de AR perto da raiz unitária no erro do GLS.
#'
#' Maior módulo das raízes inversas do polinômio AR >= 0,9: medido (fase 1),
#' com phi = 0,9 o F de tendência por GLS ainda rejeita 17% a 5% sob H0. O
#' aviso sai como condição com classe (console) e fica no ajuste, de onde os
#' três F o levam para a `nota`.
#' @noRd
.tr_series_aviso_raiz <- function(fit, ar) {
  if (ar < 1L) return(NULL)
  phi <- stats::coef(fit$modelStruct$corStruct, unconstrained = FALSE)[seq_len(ar)]
  m <- max(1 / Mod(polyroot(c(1, -phi))))
  if (m < 0.9) return(NULL)
  msg <- sprintf(paste0("erro AR perto da raiz unitária (maior raiz inversa %.3f): o GLS não ",
                        "segura o nível dos F; considere diferenciar a série"), m)
  rlang::warn(paste0("'series/regression': ", msg), class = "tr_series_warn_near_unit_root")
  msg
}

#' As estações que o usuário tirou à mão: rótulos ("fev") ou números (2).
#' @noRd
.tr_series_estacoes_fora <- function(serie, excluir) {
  excluir <- trimws(paste(excluir, collapse = ","))
  if (!nzchar(excluir)) return(character())
  rot <- .tr_series_estacoes(as.integer(stats::frequency(serie)))
  pedidos <- trimws(strsplit(excluir, "[,;[:space:]]+")[[1]])
  pedidos <- pedidos[nzchar(pedidos)]
  num <- suppressWarnings(as.integer(pedidos))
  out <- ifelse(!is.na(num) & num >= 1L & num <= length(rot), rot[pmax(1L, pmin(num, length(rot)))],
                rot[match(tolower(pedidos), tolower(rot))])
  if (anyNA(out)) {
    .tr_series_abort("tr_series_error_bad_option",
                     "Param 'excluir': '%s' não é termo sazonal desta série (período do ciclo). Use %s, ou os números de 1 a %d.",
                     paste(pedidos[is.na(out)], collapse = "', '"),
                     paste(utils::head(rot, 3), collapse = ", "), length(rot))
  }
  out <- unique(out)
  if (length(out) >= length(rot)) {
    .tr_series_abort("tr_series_error_bad_option",
                     "Param 'excluir': todos os termos sazonais ficariam fora. Desligue a sazonalidade.")
  }
  out
}

#' O p-valor de cada estação que está no modelo, bilateral por t.
#'
#' Pela linha do contraste do fator, e não pelo coeficiente: com soma zero a
#' última estação não tem coeficiente próprio (é menos a soma das outras), e
#' é pela linha dela na matriz de contraste que o p dela sai. Com o nível
#' "demais", o p é o da diferença para esse grupo.
#' @noRd
.tr_series_p_estacoes <- function(fit, X, dados) {
  C <- stats::contrasts(dados$estacao)
  cols <- which(attr(X, "assign") == match("estacao", attr(X, "rotulos")))
  b <- stats::coef(fit)[cols]
  V <- stats::vcov(fit)[cols, cols, drop = FALSE]
  gl <- nrow(X) - ncol(X)
  niveis <- setdiff(rownames(C), "demais")
  vapply(niveis, function(l) {
    a <- C[l, ]
    t <- sum(a * b) / sqrt(drop(t(a) %*% V %*% a))
    2 * stats::pt(-abs(t), gl)
  }, numeric(1))
}
