# A discriminante e a logística no contrato de modelo da `trama.models`.
#
# Os dois classificadores viajam como `models/fit` e respondem aos genéricos de
# lá (`trama.models::tr_models_info`, `..._predict_raw`, ...), registrados no
# NAMESPACE com `S3method(trama.models::<genérico>, <classe>)`. É isso que deixa
# `models/predict`, `models/confusion`, `models/roc` e `models/evaluate` lerem
# uma LDA como leem um GLM, sem que a models saiba o que é uma LDA — e é o que
# permitiu apagar os três nós de classificação que a multi mantinha.
#
# O que era de `.tr_multi_prever` (a história comum de classify, confusion e
# roc) mora agora em `tr_models_predict_cv`: resubstituição ou deixa-um-fora,
# com a MESMA conta de antes — o `CV = TRUE` exato da MASS na LDA/QDA, e n
# reajustes na logística. As taxas e AUCs dos fluxos antigos não mudam.

.TR_MULTI_VALIDACOES <- c("cruzada", "resubstituição")

# ---- info --------------------------------------------------------------------

#' Os níveis da resposta, na ordem do ajuste (sem os vazios, como no ajuste).
#' @noRd
.tr_multi_niveis <- function(x) {
  if (inherits(x, "tr_multi_logit")) return(x$niveis)
  levels(droplevels(as.factor(x$dados[[x$grupo]])))
}

.tr_multi_info <- function(x) {
  list(tarefa = "classificacao", resposta = x$grupo, preditores = x$preditores,
       niveis = .tr_multi_niveis(x), n = nrow(x$dados), rotulo = x$rotulo,
       familia = if (inherits(x, "tr_multi_logit")) "binomial" else NULL)
}

#' @export
tr_models_info.tr_multi_lda <- function(x) .tr_multi_info(x)
#' @export
tr_models_info.tr_multi_logit <- function(x) .tr_multi_info(x)

# ---- predict_raw / predict_cv --------------------------------------------------

#' A matriz dos preditores de uma tabela nova, com as recusas de classificar.
#'
#' A models já conferiu que as colunas existem (`.tr_models_novos`); aqui fica
#' o que é da técnica: preditor numérico e sem faltante. Não é
#' `.tr_multi_matriz`, que recusa variável constante — classificar UMA linha
#' nova (desvio padrão NA) ou duas iguais é legítimo.
#' @noRd
.tr_multi_X_novos <- function(x, novos) {
  preds <- x$preditores
  texto <- preds[!vapply(novos[preds], is.numeric, TRUE)]
  if (length(texto)) {
    .tr_multi_abort("tr_multi_error_not_numeric",
                    "Na tabela a prever, coluna não numérica entre os preditores de %s: %s.",
                    x$rotulo, paste(texto, collapse = ", "))
  }
  X <- as.matrix(as.data.frame(novos)[, preds, drop = FALSE])
  storage.mode(X) <- "double"
  incompletas <- !stats::complete.cases(X)
  if (any(incompletas)) {
    .tr_multi_abort("tr_multi_error_missing_values",
                    paste0("%s não classifica linha com faltante, e %d linha(s) da tabela a prever têm ",
                           "(colunas: %s). Ligue um 'data/drop_na' antes."),
                    x$rotulo, sum(incompletas), paste(preds[colSums(is.na(X)) > 0], collapse = ", "))
  }
  X
}

#' A lista do contrato: classe (fator nos níveis), probabilidades, escores.
#' @noRd
.tr_multi_prev <- function(prob, classe, niveis, extra = NULL) {
  prob <- as.matrix(prob)
  colnames(prob) <- niveis
  rownames(prob) <- NULL
  list(previsto = factor(as.character(classe), levels = niveis), prob = prob, extra = extra)
}

#' Os escores LD1.. da LDA linear, ou NULL (a QDA não tem funções).
#'
#' Saem mesmo na cruzada (são as coordenadas no plano do modelo completo): o
#' `view/points` colorido pelo previsto honesto precisa deles.
#' @noRd
.tr_multi_lda_escores <- function(x, X) {
  if (!identical(x$metodo, "linear")) return(NULL)
  tibble::as_tibble(as.data.frame(stats::predict(x$ajuste, newdata = X)$x))
}

#' @export
tr_models_predict_raw.tr_multi_lda <- function(x, novos, ...) {
  X <- .tr_multi_X_novos(x, novos)
  p <- stats::predict(x$ajuste, newdata = X)
  .tr_multi_prev(p$posterior, p$class, .tr_multi_niveis(x), .tr_multi_lda_escores(x, X))
}

#' @export
tr_models_predict_raw.tr_multi_logit <- function(x, novos, ...) {
  p <- .tr_multi_logit_prever(x, .tr_multi_X_novos(x, novos))
  .tr_multi_prev(p$prob, p$classe, x$niveis)
}

#' @export
tr_models_predict_cv.tr_multi_lda <- function(x, validacao = "resubstituição") {
  no <- "multi/discriminant"
  validacao <- .tr_multi_enum(validacao, .TR_MULTI_VALIDACOES, "validacao")
  tr <- .tr_multi_treino(x)
  niv <- levels(tr$g)
  if (identical(validacao, "resubstituição")) {
    p <- stats::predict(x$ajuste, newdata = tr$X)
    return(.tr_multi_prev(p$posterior, p$class, niv, .tr_multi_lda_escores(x, tr$X)))
  }
  if (identical(x$metodo, "quadrática")) {
    # Deixando uma fora, o grupo dela fica com n - 1; a covariância só é
    # inversível se ainda sobrarem p + 1.
    .tr_multi_grupo_minimo(tr$g, ncol(tr$X) + 2L, no,
                           "A validação cruzada da quadrática tira uma observação do grupo e ainda precisa inverter a covariância dele.")
  }
  # Cruzada exata e sem reajuste: o `CV = TRUE` da MASS.
  cv <- .tr_multi_lda_ajuste(tr$X, tr$g, x$metodo, .tr_multi_prior_vetor(tr$g, x$priors), no, CV = TRUE)
  .tr_multi_prev(cv$posterior, cv$class, niv, .tr_multi_lda_escores(x, tr$X))
}

#' @export
tr_models_predict_cv.tr_multi_logit <- function(x, validacao = "resubstituição") {
  no <- "multi/logistic"
  validacao <- .tr_multi_enum(validacao, .TR_MULTI_VALIDACOES, "validacao")
  g <- droplevels(as.factor(x$dados[[x$grupo]]))
  X <- .tr_multi_logit_X(x, x$dados)
  if (identical(validacao, "resubstituição")) {
    p <- .tr_multi_logit_prever(x, X)
    return(.tr_multi_prev(p$prob, p$classe, x$niveis))
  }
  # Na logística não há atalho: são n ajustes, cada um sem uma linha — rápido
  # no `glm`, alguns segundos no `multinom` com centenas de linhas.
  .tr_multi_grupo_minimo(g, 3L, no,
                         "Deixando uma observação de fora, o grupo dela ainda precisa de duas para entrar no ajuste.")
  n <- nrow(X)
  prob <- matrix(NA_real_, n, length(x$niveis), dimnames = list(NULL, x$niveis))
  classe <- character(n)
  for (i in seq_len(n)) {
    sem <- x
    # Sem hessiana: o reajuste só prevê a observação deixada de fora.
    sem$ajuste <- .tr_multi_logit_ajuste(X[-i, , drop = FALSE], g[-i], no, hess = FALSE)
    pr <- .tr_multi_logit_prever(sem, X[i, , drop = FALSE])
    prob[i, ] <- pr$prob[1, ]
    classe[i] <- as.character(pr$classe)
  }
  .tr_multi_prev(prob, classe, x$niveis)
}

# ---- coefs ---------------------------------------------------------------------

#' `li_95`/`ls_95`: o nome que a `models/coefficients` dá ao intervalo.
#' @noRd
.tr_multi_ic_nomes <- function(confianca) {
  pct <- sub(".", "_", as.character(round(100 * confianca, 1)), fixed = TRUE)
  paste0(c("li_", "ls_"), pct)
}

#' Os coeficientes da logística na forma da `models/coefficients`.
#'
#' Mesmas colunas de um GLM da models (`termo`, `estimativa`, `erro_padrao`,
#' `z`, `p_valor`, `li_<nível>`, `ls_<nível>`), mais `grupo`: o grupo cuja
#' chance a linha modela contra a referência (um só na binária, um por grupo na
#' multinomial). `exponenciar` dá a razão de chances e o intervalo de Wald
#' exponenciado — o que a antiga `multi/logistic_coefficients` mostrava.
#' @export
tr_models_coefs.tr_multi_logit <- function(x, exponenciar = FALSE, escala = "unidade", confianca = 0.95, ...) {
  no <- "models/coefficients"
  escala <- .tr_multi_enum(escala, .TR_MULTI_ESCALAS_OR, "escala")
  confianca <- .tr_multi_num(confianca, "confianca", min = 0.5, max = 0.999)
  .tr_multi_sem_separacao(x, no)
  d <- .tr_multi_logit_coefs(x, escala)
  q <- stats::qnorm((1 + confianca) / 2)
  z <- d$coeficiente / d$erro_padrao
  tab <- tibble::tibble(grupo = d$grupo, termo = d$termo, estimativa = d$coeficiente,
                        erro_padrao = d$erro_padrao, z = z, p_valor = 2 * stats::pnorm(-abs(z)),
                        li = d$coeficiente - q * d$erro_padrao, ls = d$coeficiente + q * d$erro_padrao)
  nota <- sprintf("Wald; referência: '%s'", x$niveis[[1]])
  if (isTRUE(exponenciar)) {
    tab$estimativa <- exp(tab$estimativa); tab$li <- exp(tab$li); tab$ls <- exp(tab$ls)
    tab$erro_padrao <- NULL
    nota <- paste0(nota, "; estimativa e intervalo exponenciados (razão de chances)")
  }
  if (identical(escala, "desvio padrão")) nota <- paste0(nota, "; por desvio padrão do preditor")
  names(tab)[names(tab) %in% c("li", "ls")] <- .tr_multi_ic_nomes(confianca)
  trama.models::tr_models_effects(tab, "Coeficientes da logística", coluna_estat = "z",
                                  rodape = list(n = as.character(nrow(x$dados))), nota = nota)
}

#' A LDA não tem coeficiente com p-valor: as funções têm os testes delas.
#' @export
tr_models_coefs.tr_multi_lda <- function(x, ...) {
  .tr_multi_abort("tr_models_error_not_applicable",
                  paste0("'models/coefficients' não se aplica a %s: as funções discriminantes não têm ",
                         "coeficiente com p-valor. Os coeficientes (brutos, padronizados, estrutura) e ",
                         "os testes de Wilks estão em 'multi/discriminant_functions'."), x$rotulo)
}

# ---- stats ---------------------------------------------------------------------

#' @export
tr_models_stats.tr_multi_lda <- function(x) {
  tr <- .tr_multi_treino(x)
  # Wilks de TODAS as funções: o teste global "os grupos diferem nas médias?",
  # que vem dos dados e vale igual para a quadrática.
  lambda <- tryCatch(.tr_multi_canonica(tr$X, tr$g), error = function(e) NA_real_)
  tibble::tibble(modelo = x$rotulo, n = nrow(tr$X), grupos = nlevels(tr$g),
                 preditores = length(x$preditores), priors = x$priors,
                 lambda_wilks = prod(1 / (1 + lambda)),
                 acerto_resubstituicao = mean(stats::predict(x$ajuste, newdata = tr$X)$class == tr$g))
}

#' @export
tr_models_stats.tr_multi_logit <- function(x) {
  g <- droplevels(as.factor(x$dados[[x$grupo]]))
  aj <- x$ajuste
  desvio <- aj$deviance  # `glm` e `multinom` guardam com o mesmo nome
  aic <- if (identical(x$tipo, "binária")) aj$aic else aj$AIC
  # O desvio do modelo só com intercepto tem forma fechada (as proporções dos
  # grupos): vale para o `glm` e para o `multinom`, sem reajustar nada.
  nk <- as.vector(table(g))
  nulo <- -2 * sum(nk * log(nk / sum(nk)))
  tibble::tibble(modelo = x$rotulo, n = length(g), grupos = nlevels(g),
                 preditores = length(x$preditores), aic = aic, desvio = desvio, desvio_nulo = nulo,
                 # McFadden: 1 − desvio/desvio nulo. Com separação vai a ~1, e
                 # diz pouco; o card avisa pela ROC.
                 pseudo_r2 = 1 - desvio / nulo,
                 acerto_resubstituicao = mean(.tr_multi_logit_prever(x, .tr_multi_logit_X(x, x$dados))$classe == g))
}

# ---- importance ----------------------------------------------------------------

#' LDA: |coeficiente padronizado|, somado pelas funções com o peso da separação.
#'
#' Com uma função é o |padronizado| dela. Com várias, cada uma pesa pela
#' proporção da separação que explica: uma medida que só pesa em LD2 (1% da
#' separação na `iris`) não é importante para discriminar.
#' @export
tr_models_importance.tr_multi_lda <- function(x) {
  if (!identical(x$metodo, "linear")) {
    .tr_multi_abort("tr_models_error_not_applicable",
                    paste0("'models/importance' não se aplica a %s: a quadrática não tem funções ",
                           "discriminantes, e não há coeficiente padronizado para medir."), x$rotulo)
  }
  pad <- tr_multi_discriminant_functions(x, "padronizados")
  prop <- tr_multi_discriminant_functions(x, "funções")$proporcao
  M <- abs(as.matrix(pad[, -1L]))
  out <- tibble::tibble(termo = pad$variavel, importancia = as.vector(M %*% prop),
                        medida = "|coef. padronizado| ponderado pela separação")
  out[order(-out$importancia), , drop = FALSE]
}

#' Logística: |z| de Wald; na multinomial, o maior entre os grupos.
#' @export
tr_models_importance.tr_multi_logit <- function(x) {
  .tr_multi_sem_separacao(x, "models/importance")
  d <- .tr_multi_logit_coefs(x)
  d <- d[d$termo != "(intercepto)", , drop = FALSE]
  z <- tapply(abs(d$coeficiente / d$erro_padrao), factor(d$termo, levels = x$preditores), max)
  out <- tibble::tibble(termo = names(z), importancia = as.vector(z),
                        medida = if (identical(x$tipo, "binária")) "|z|" else "|z| (maior entre os grupos)")
  out[order(-out$importancia), , drop = FALSE]
}

# ---- card e tabela ---------------------------------------------------------------

#' @export
tr_models_card.tr_multi_lda <- function(x, ctx) {
  trama.view::tr_view_render(tr_multi_plot_discriminant(x), ctx)
}

#' O card é o das RAZÕES DE CHANCES; com separação elas não existem (vão ao
#' infinito), e o card cai para a ROC por resubstituição, que continua honesta.
#' @export
tr_models_card.tr_multi_logit <- function(x, ctx) {
  p <- if (length(x$separacao)) trama.models::tr_models_roc(x, validacao = "resubstituição")
       else tr_multi_plot_odds(x)
  trama.view::tr_view_render(p, ctx)
}

#' `models/fit` -> `data/table`: o treino classificado (o que o adaptador de
#' `multi/lda` e `multi/logit` fazia), com `LD1..` na discriminante linear.
#' @export
tr_models_as_table.tr_multi_lda <- function(x) trama.models::tr_models_predict(x)
#' @export
tr_models_as_table.tr_multi_logit <- function(x) trama.models::tr_models_predict(x)
