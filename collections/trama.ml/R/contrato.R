# Os modelos da ml no contrato de modelo da `trama.models`.
#
# Todo ajuste daqui (linear, CART, FIGS, floresta, SVM, XGBoost, e o vencedor
# do tuning) viaja como `models/fit`, com classe c("tr_ml_fit",
# "tr_models_fit"), e responde aos genéricos de lá — registrados no NAMESPACE
# com `S3method(trama.models::<genérico>, tr_ml_fit)`. É o que deixa
# `models/predict`, `models/evaluate`, `models/confusion`, `models/roc` e
# `models/importance` lerem um XGBoost como leem um GLM, e o que permitiu
# apagar os cinco leitores que a ml mantinha em paralelo. Os que ficaram aqui
# (regras, desenho da árvore) só fazem sentido nos motores daqui e recusam o
# resto com classe (`.tr_ml_exigir_fit`).

# Folds da validação cruzada do "só modelo" (confusion/roc/evaluate sem
# `dados`). Cinco, e não deixa-um-fora como na models e na multi: aqui cada
# fold é um REAJUSTE do motor (uma floresta de 200 árvores, um XGBoost de 100
# rodadas), e n reajustes num card seriam minutos. Cinco é o default do
# `ml/tune`, e a mesma conta de folds (`.tr_ml_make_folds`, estratificada por
# classe) com a semente do modelo — o número é reproduzível.
.TR_ML_CV_FOLDS <- 5L

# Os hiperparâmetros que cada motor de fato usa: o `tr_models_stats()` mostra
# só estes, e a validação cruzada reajusta com eles.
.TR_ML_HIPER <- list(linear = character(), cart = c("max_depth", "min_n"),
                     figs = c("max_splits", "min_n"),
                     forest = c("trees", "mtry", "min_n", "max_depth"),
                     svm = c("cost", "gamma", "kernel"),
                     xgboost = c("nrounds", "max_depth", "eta"))

#' O topo do card: o método e a tarefa, como o `rotulo` dos modelos da models.
#' @noRd
.tr_ml_rotulo <- function(modelo, tarefa) {
  nome <- switch(modelo, linear = if (tarefa == "regressao") "Linear" else "Log\u{ED}stica",
                 cart = "CART", figs = "FIGS", forest = "Random forest", svm = "SVM",
                 xgboost = "XGBoost")
  paste0(nome, " \u{B7} ", if (tarefa == "regressao") "regress\u{E3}o" else "classifica\u{E7}\u{E3}o")
}

# ---- info ----------------------------------------------------------------------

#' @export
tr_models_info.tr_ml_fit <- function(x) {
  list(tarefa = x$tarefa,
       # `alvo` era o nome do campo antes da Fase 4; um objeto desses só chega
       # aqui lido à mão de um RDS velho, mas ler os dois custa uma linha.
       resposta = x$resposta %||% x$alvo, preditores = x$preditores,
       niveis = x$niveis, n = x$n, rotulo = x$rotulo %||% .tr_ml_rotulo(x$modelo, x$tarefa),
       familia = if (x$modelo != "linear") NULL else if (x$tarefa == "regressao") "gaussian" else "binomial")
}

# ---- predict_raw / predict_cv --------------------------------------------------

#' @export
tr_models_predict_raw.tr_ml_fit <- function(x, novos, ...) {
  .tr_ml_checar_previsao(x, novos)
  p <- .tr_ml_prever(x, .tr_ml_novos_dados(x, novos))
  list(previsto = p$previsto, prob = p$prob, extra = NULL)
}

#' Resubstituição = o modelo prevê o próprio treino; cruzada = k-fold.
#'
#' Na cruzada cada fold é previsto por um modelo reajustado SEM ele, com os
#' mesmos hiperparâmetros e a mesma semente. Os folds são estratificados por
#' classe; com uma classe menor que 5, `k` cai para o tamanho dela (ao menos 2).
#' Modelo vindo do `ml/tune` (`extras$tunado`) recusa a cruzada: seria otimista.
#' @export
tr_models_predict_cv.tr_ml_fit <- function(x, validacao = "resubstitui\u{E7}\u{E3}o") {
  validacao <- .tr_ml_enum(validacao, c("resubstitui\u{E7}\u{E3}o", "cruzada"), "validacao")
  if (validacao != "cruzada") return(tr_models_predict_raw.tr_ml_fit(x, .tr_ml_sem_origem(x$dados)))
  # Modelo tunado: a busca já escolheu os hiperparâmetros pela nota nestas
  # linhas (e, com a mesma semente, nestas mesmas partições). Reavaliar por
  # cruzada devolveria a nota do vencedor, otimista por construção — viés de
  # seleção. Só dado que o tune não viu mede o modelo.
  if (isTRUE(x$extras$tunado)) {
    .tr_ml_abort("tr_models_error_not_applicable",
                 "Valida\u{E7}\u{E3}o 'cruzada' n\u{E3}o se aplica a %s: os hiperpar\u{E2}metros foram escolhidos nestas mesmas parti\u{E7}\u{F5}es, e a valida\u{E7}\u{E3}o cruzada seria otimista. Avalie em dados separados: `ml/split` antes do `ml/tune`, e ligue o teste na entrada 'dados'.",
                 x$rotulo)
  }
  resposta <- x$resposta %||% x$alvo
  dados <- as.data.frame(x$dados, check.names = FALSE)
  y <- dados[[resposta]]
  y <- if (x$tarefa == "classificacao") factor(y) else y
  menor <- if (is.factor(y)) min(table(y)) else length(y)
  k <- min(.TR_ML_CV_FOLDS, menor)
  if (k < 2L) {
    .tr_ml_abort("tr_models_error_not_applicable",
                 "Valida\u{E7}\u{E3}o 'cruzada' n\u{E3}o se aplica a %s: alguma classe tem uma s\u{F3} linha, e nenhum fold a deixaria no treino. Use 'resubstitui\u{E7}\u{E3}o' ou ligue 'dados' de teste.",
                 x$rotulo)
  }
  partes <- .tr_ml_with_seed(x$seed, .tr_ml_make_folds(y, k))
  hiper <- x$extras$parametros[.TR_ML_HIPER[[x$modelo]]]
  n <- nrow(dados)
  previsto <- if (x$tarefa == "classificacao") character(n) else numeric(n)
  prob <- if (x$tarefa == "classificacao") matrix(NA_real_, n, length(x$niveis), dimnames = list(NULL, x$niveis))
  for (fora in partes) {
    m <- do.call(tr_ml_fit, c(list(dados = dados[-fora, , drop = FALSE], resposta = resposta,
                                   preditores = x$preditores, modelo = x$modelo,
                                   tarefa = x$tarefa, seed = x$seed), hiper))
    p <- .tr_ml_prever(m, .tr_ml_novos_dados(m, dados[fora, , drop = FALSE]))
    if (is.null(prob)) { previsto[fora] <- p$previsto; next }
    previsto[fora] <- as.character(p$previsto)
    prob[fora, ] <- p$prob[, x$niveis, drop = FALSE]
  }
  list(previsto = if (is.null(prob)) previsto else factor(previsto, levels = x$niveis),
       prob = prob, extra = NULL)
}

# ---- stats, resid ------------------------------------------------------------------

#' Uma linha: o que foi ajustado, com quê, e os hiperparâmetros do motor.
#'
#' Sem métrica de treino: a ml nunca mediu o ajuste no treino (é otimista, e
#' os avaliadores medem por cruzada ou no teste).
#' @export
tr_models_stats.tr_ml_fit <- function(x) {
  base <- tibble::tibble(modelo = x$rotulo %||% .tr_ml_rotulo(x$modelo, x$tarefa), metodo = x$modelo,
                         tarefa = x$tarefa, n = x$n, preditores = length(x$preditores),
                         motor = paste(x$extras$engine, x$extras$engine_version))
  hiper <- x$extras$parametros[.TR_ML_HIPER[[x$modelo]]]
  if (length(hiper)) base <- tibble::as_tibble(c(as.list(base), hiper))
  base
}

#' A tabela do ajuste com `ajustado` e `residuo` (observado − ajustado).
#' @export
tr_models_resid.tr_ml_fit <- function(x) {
  if (x$tarefa != "regressao") {
    .tr_ml_abort("tr_models_error_not_applicable",
                 "Res\u{ED}duo n\u{E3}o se aplica a %s: numa classifica\u{E7}\u{E3}o n\u{E3}o h\u{E1} observado \u{2212} previsto. Veja 'models/confusion'.",
                 x$rotulo)
  }
  d <- x$dados
  aj <- tr_models_predict_raw.tr_ml_fit(x, d)$previsto
  nomes <- c("ajustado", "residuo")
  # Coluna que já existe ganha sufixo, como nos modelos da models.
  nomes <- ifelse(nomes %in% names(d), paste0(nomes, "_modelo"), nomes)
  d[[nomes[[1]]]] <- aj
  d[[nomes[[2]]]] <- d[[x$resposta %||% x$alvo]] - aj
  d
}

# ---- importance, coefs ---------------------------------------------------------------

#' Árvores: a importância interna do motor. Linear: |t| (|z|) dos coeficientes,
#' a régua da models para `lm`/`glm`. SVM não tem importância interna.
#' @export
tr_models_importance.tr_ml_fit <- function(x) {
  if (x$modelo == "svm") {
    .tr_ml_abort("tr_models_error_not_applicable",
                 "'models/importance' n\u{E3}o se aplica a %s: a SVM n\u{E3}o tem uma import\u{E2}ncia interna por preditora.",
                 x$rotulo)
  }
  if (x$modelo != "linear") return(.tr_ml_importancia_arvores(x))
  tab <- tr_models_coefs.tr_ml_fit(x)
  col <- tab$coluna_estat
  t <- tab$tabela[tab$tabela$termo != "(Intercept)", , drop = FALSE]
  out <- tibble::tibble(termo = t$termo, importancia = abs(t[[col]]), medida = paste0("|", col, "|"))
  out[order(-out$importancia), , drop = FALSE]
}

#' Os coeficientes da referência linear, na forma da `models/coefficients`.
#'
#' Delega ao `lm`/`glm` que a ml ajustou por dentro; o que muda é só trocar os
#' nomes internos (`x1`, `x2`) pelos das colunas. Os demais motores não têm
#' coeficiente: recusam.
#' @export
tr_models_coefs.tr_ml_fit <- function(x, exponenciar = FALSE, escala = "unidade", confianca = 0.95, ...) {
  if (x$modelo != "linear") {
    .tr_ml_abort("tr_models_error_not_applicable",
                 "'models/coefficients' n\u{E3}o se aplica a %s: s\u{F3} a refer\u{EA}ncia linear da ml tem coeficientes. Veja 'models/importance'.",
                 x$rotulo)
  }
  escala <- .tr_ml_enum(escala, c("unidade", "desvio padr\u{E3}o"), "escala")
  confianca <- .tr_ml_num(confianca, "confianca", 0.5)
  aj <- x$ajuste
  glm <- inherits(aj, "glm")
  if (isTRUE(exponenciar) && !glm) {
    .tr_ml_abort("tr_models_error_not_applicable",
                 "Exponenciar s\u{F3} faz sentido na log\u{ED}stica; %s \u{E9} uma regress\u{E3}o linear.", x$rotulo)
  }
  s <- stats::coef(summary(aj))
  ic <- if (glm) stats::confint.default(aj, level = confianca) else stats::confint(aj, level = confianca)
  coluna <- if (glm) "z" else "t"
  termo <- rownames(s)
  mapa <- match(termo, x$internos)
  termo[!is.na(mapa)] <- x$preditores[mapa[!is.na(mapa)]]
  tab <- tibble::tibble(termo = termo, estimativa = unname(s[, 1]), erro_padrao = unname(s[, 2]),
                        estat = unname(s[, 3]), p_valor = unname(s[, 4]),
                        li = unname(ic[rownames(s), 1]), ls = unname(ic[rownames(s), 2]))
  if (escala == "desvio padr\u{E3}o") {
    dp <- apply(stats::model.matrix(aj), 2L, stats::sd)[rownames(s)]
    dp[is.na(dp) | dp == 0] <- 1
    for (col in c("estimativa", "erro_padrao", "li", "ls")) tab[[col]] <- tab[[col]] * unname(dp)
  }
  nota <- if (glm) "intervalo de Wald, na escala da liga\u{E7}\u{E3}o" else ""
  if (isTRUE(exponenciar)) {
    tab$estimativa <- exp(tab$estimativa); tab$li <- exp(tab$li); tab$ls <- exp(tab$ls)
    tab$erro_padrao <- NULL
    nota <- "estimativa e intervalo exponenciados (raz\u{E3}o de chances)"
  }
  if (escala == "desvio padr\u{E3}o") nota <- paste0(nota, if (nzchar(nota)) "; ", "por desvio padr\u{E3}o do preditor")
  pct <- sub(".", "_", as.character(round(100 * confianca, 1)), fixed = TRUE)
  names(tab)[names(tab) == "estat"] <- coluna
  names(tab)[names(tab) == "li"] <- paste0("li_", pct)
  names(tab)[names(tab) == "ls"] <- paste0("ls_", pct)
  trama.models::tr_models_effects(tab, "Coeficientes", coluna_estat = coluna,
                                  rodape = list(n = as.character(x$n)), nota = nota)
}

# ---- card, tabela, serialização --------------------------------------------------------

#' O que o card mostrava como `ml/fit`: as regras (CART, FIGS) ou um resumo.
#' @noRd
.tr_ml_card_tabela <- function(x) {
  if (x$modelo %in% c("cart", "figs")) return(tr_ml_rules(x))
  tibble::tibble(modelo = x$modelo, tarefa = x$tarefa, resposta = x$resposta %||% x$alvo,
                 linhas_treino = x$n, preditores = paste(x$preditores, collapse = ", "),
                 leitura = "Use Prever e Avaliar com dados de teste; ajuste n\u{E3}o mede generaliza\u{E7}\u{E3}o.")
}

#' @export
tr_models_card.tr_ml_fit <- function(x, ctx) .tr_ml_table_preview(.tr_ml_card_tabela(x))

#' `models/fit` -> `data/table`: a mesma tabela do card.
#' @export
tr_models_as_table.tr_ml_fit <- function(x) .tr_ml_card_tabela(x)

#' Um booster do XGBoost é ponteiro nativo: não sobrevive ao RDS. Vai em UBJ
#' (o formato binário do próprio xgboost, independente da sessão); o resto do
#' objeto segue no RDS da models.
#' @export
tr_models_serialize.tr_ml_fit <- function(x) {
  if (identical(x$modelo, "xgboost") && !isTRUE(x$.booster_raw)) {
    x$ajuste <- xgboost::xgb.save.raw(x$ajuste, raw_format = "ubj")
    x$.booster_raw <- TRUE
  }
  x
}

#' @export
tr_models_unserialize.tr_ml_fit <- function(x) {
  if (isTRUE(x$.booster_raw)) {
    .tr_ml_require("xgboost", "xgboost")
    x$ajuste <- xgboost::xgb.load.raw(x$ajuste)
    x$.booster_raw <- NULL
  }
  x
}
