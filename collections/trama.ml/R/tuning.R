# Busca de hiperparâmetros deliberadamente pequena na interface: espaços,
# folds, direção da métrica e reajuste final ficam dentro deste módulo.

.tr_ml_tuning_space <- function(modelo, p, amplitude) {
  amplo <- identical(amplitude, "ampla")
  switch(modelo,
    cart = list(max_depth = c(1L, if (amplo) 15L else 8L), min_n = c(1L, if (amplo) 30L else 15L)),
    figs = list(max_splits = c(1L, if (amplo) 30L else 12L), min_n = c(1L, if (amplo) 30L else 15L)),
    forest = list(trees = c(if (amplo) 100L else 150L, if (amplo) 800L else 400L),
      mtry = c(1L, p), min_n = c(1L, if (amplo) 30L else 15L),
      max_depth = c(2L, if (amplo) 20L else 10L)),
    svm = list(cost = c(if (amplo) 1e-3 else 1e-2, if (amplo) 1e3 else 1e2),
      gamma = c(if (amplo) 1e-5 else 1e-4, if (amplo) 10 else 1),
      kernel = c("linear", "radial", "polynomial")),
    xgboost = list(nrounds = c(20L, if (amplo) 500L else 200L),
      max_depth = c(1L, if (amplo) 12L else 7L), eta = c(if (amplo) .005 else .01, .3)))
}

.tr_ml_sample_config <- function(space, modelo) {
  inteiro <- c("max_depth", "min_n", "max_splits", "trees", "mtry", "nrounds")
  logar <- c("cost", "gamma", "eta")
  out <- lapply(names(space), function(nm) {
    z <- space[[nm]]
    if (is.character(z)) return(sample(z, 1L))
    if (nm %in% inteiro) return(sample(seq.int(z[[1]], z[[2]]), 1L))
    if (nm %in% logar) return(exp(stats::runif(1L, log(z[[1]]), log(z[[2]]))))
    stats::runif(1L, z[[1]], z[[2]])
  })
  stats::setNames(out, names(space))
}

.tr_ml_make_folds <- function(y, k) {
  grupos <- if (is.factor(y) || is.character(y) || is.logical(y)) split(seq_along(y), y) else
    list(todos = seq_along(y))
  if (any(lengths(grupos) < k))
    .tr_ml_abort("tr_ml_error_bad_folds", "Cada classe precisa ter pelo menos 'folds' observações.")
  ids <- integer(length(y))
  for (g in grupos) ids[g] <- sample(rep(seq_len(k), length.out = length(g)))
  lapply(seq_len(k), function(i) which(ids == i))
}

.tr_ml_tune_metric <- function(pred, alvo, tarefa, metrica) {
  z <- tr_ml_evaluate(pred, alvo, tarefa = tarefa)
  i <- match(metrica, z$metrica)
  if (is.na(i)) .tr_ml_abort("tr_ml_error_bad_param", sprintf("Métrica '%s' não serve para esta tarefa.", metrica))
  z$valor[[i]]
}

#' Ajustar hiperparâmetros por validação cruzada
#'
#' O conjunto recebido é usado para validação interna; após escolher a melhor
#' configuração, o modelo é reajustado em todas as linhas. Mantenha o teste
#' final fora deste nó.
#' @param dados Tabela de treino.
#' @param alvo,cols,modelo,tarefa Argumentos de [tr_ml_fit()].
#' @param metrica `auto`, uma métrica de regressão ou classificação.
#' @param tentativas Número de configurações avaliadas.
#' @param folds Número de folds da validação cruzada.
#' @param amplitude Espaço `conservadora` ou `ampla`.
#' @param seed Semente local e reprodutível.
#' @return `tr_ml_tuning`, com `modelo` e `historico`.
#' @export
tr_ml_tune <- function(dados, alvo = "", cols = "", modelo = "cart", tarefa = "auto",
                       metrica = "auto", tentativas = 20L, folds = 5L,
                       amplitude = "conservadora", seed = 42L) {
  modelo <- .tr_ml_enum(modelo, c("linear", "cart", "figs", "forest", "svm", "xgboost"), "modelo")
  if (identical(modelo, "linear"))
    .tr_ml_abort("tr_ml_error_not_tunable", "O modelo linear não possui hiperparâmetros nesta coleção.")
  tentativas <- .tr_ml_int(tentativas, "tentativas", 1L)
  folds <- .tr_ml_int(folds, "folds", 2L)
  seed <- .tr_ml_int(seed, "seed", 0L)
  amplitude <- .tr_ml_enum(amplitude, c("conservadora", "ampla"), "amplitude")
  d <- .tr_ml_dados(dados, alvo, cols, tarefa)
  if (folds > d$n) .tr_ml_abort("tr_ml_error_bad_folds", "'folds' não pode superar o número de linhas.")
  tarefa <- d$tarefa
  if (identical(metrica, "auto")) metrica <- if (tarefa == "regressao") "rmse" else "macro_f1"
  validas <- if (tarefa == "regressao") c("mae", "rmse", "r2") else
    c("accuracy", "balanced_accuracy", "macro_f1")
  metrica <- .tr_ml_enum(metrica, validas, "metrica")
  minimizar <- metrica %in% c("mae", "rmse")
  space <- .tr_ml_tuning_space(modelo, length(d$preditores), amplitude)

  resultado <- .tr_ml_with_seed(seed, {
    y_folds <- if (tarefa == "classificacao") factor(dados[[d$alvo]]) else dados[[d$alvo]]
    partes <- .tr_ml_make_folds(y_folds, folds)
    configs <- lapply(seq_len(tentativas), function(i) .tr_ml_sample_config(space, modelo))
    linhas <- vector("list", tentativas)
    for (i in seq_len(tentativas)) {
      cfg <- configs[[i]]; valores <- numeric()
      erro <- NULL
      for (validacao in partes) {
        treino <- dados[-validacao, , drop = FALSE]
        teste <- dados[validacao, , drop = FALSE]
        args <- c(list(dados = treino, alvo = alvo, cols = cols, modelo = modelo,
                       tarefa = tarefa, seed = as.integer((as.double(seed) + i) %% .Machine$integer.max)), cfg)
        valor <- tryCatch({
          fit <- do.call(tr_ml_fit, args)
          pred <- tr_ml_predict(fit, teste)
          .tr_ml_tune_metric(pred, alvo, tarefa, metrica)
        }, error = function(e) { erro <<- conditionMessage(e); NA_real_ })
        valores <- c(valores, valor)
        if (!is.null(erro)) break
      }
      linha <- c(list(tentativa = i), cfg,
                  list(media = if (all(is.finite(valores))) mean(valores) else NA_real_,
                       desvio = if (length(valores) > 1L && all(is.finite(valores))) stats::sd(valores) else NA_real_,
                       status = if (is.null(erro)) "ok" else "erro",
                       erro = erro %||% ""))
      linhas[[i]] <- linha
    }
    historico <- tibble::as_tibble(do.call(rbind.data.frame, c(linhas, stringsAsFactors = FALSE)))
    historico$tentativa <- as.integer(historico$tentativa)
    historico$media <- as.numeric(historico$media); historico$desvio <- as.numeric(historico$desvio)
    boas <- which(historico$status == "ok" & is.finite(historico$media))
    if (!length(boas)) .tr_ml_abort("tr_ml_error_tuning_failed", "Todas as tentativas de tuning falharam.")
    melhor <- boas[[if (minimizar) which.min(historico$media[boas]) else which.max(historico$media[boas])]]
    acumulado <- if (minimizar) cummin(replace(historico$media, !is.finite(historico$media), Inf)) else
      cummax(replace(historico$media, !is.finite(historico$media), -Inf))
    historico$melhor <- acumulado
    final_args <- c(list(dados = dados, alvo = alvo, cols = cols, modelo = modelo,
                         tarefa = tarefa, seed = seed), configs[[melhor]])
    list(modelo = do.call(tr_ml_fit, final_args), historico = historico,
         melhor_tentativa = melhor, metrica = metrica, minimizar = minimizar,
         folds = folds, seed = seed)
  })
  structure(resultado, class = "tr_ml_tuning")
}
