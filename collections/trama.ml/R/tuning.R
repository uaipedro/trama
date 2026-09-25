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
    .tr_ml_abort("tr_ml_error_bad_folds", "Cada classe precisa ter pelo menos 'folds' observa\u{E7}\u{F5}es.")
  ids <- integer(length(y))
  for (g in grupos) ids[g] <- sample(rep(seq_len(k), length.out = length(g)))
  lapply(seq_len(k), function(i) which(ids == i))
}

.tr_ml_tune_metric <- function(y, pred, tarefa, metrica) {
  z <- .tr_ml_metricas(y, pred, tarefa)
  i <- match(metrica, z$metrica)
  if (is.na(i)) .tr_ml_abort("tr_ml_error_bad_param", sprintf("M\u{E9}trica '%s' n\u{E3}o serve para esta tarefa.", metrica))
  z$valor[[i]]
}

#' Ajustar hiperparâmetros por validação cruzada
#'
#' O conjunto recebido é usado para validação interna; após escolher a melhor
#' configuração, o modelo é reajustado em todas as linhas. O conjunto de teste
#' final fica fora deste nó.
#' @param dados Tabela de treino com pelo menos duas linhas.
#' @param resposta Nome de uma coluna existente em `dados`.
#' @param preditores Preditores numéricos separados por vírgula. Vazio usa todos os
#'   numéricos, exceto `resposta`.
#' @param modelo Família com hiperparâmetros: `"cart"`, `"figs"`, `"forest"`,
#'   `"svm"` ou `"xgboost"`.
#' @param tarefa Uma de `"auto"`, `"regressao"` ou `"classificacao"`. `"auto"`
#'   interpreta resposta numérica como regressão.
#' @param metrica Métrica compatível com a tarefa. `"auto"` usa `"rmse"` em
#'   regressão e `"macro_f1"` em classificação.
#' @param tentativas Número inteiro positivo de configurações avaliadas.
#' @param folds Número inteiro de partições, a partir de dois. Não pode superar
#'   o número de linhas nem o tamanho da menor classe.
#' @param amplitude Limites `"conservadora"` ou `"ampla"` para a busca.
#' @param seed Inteiro entre zero e 2147483647. Controla folds, configurações e
#'   ajustes sem alterar o estado aleatório da sessão.
#' @return Objeto `tr_ml_tuning`: lista com o `modelo` vencedor reajustado (um
#'   `models/fit`),
#'   `historico` por tentativa e fold, índice `melhor_tentativa`, `metrica`,
#'   direção `minimizar`, número de `folds` e `seed`.
#' @export
tr_ml_tune <- function(dados, resposta = "", preditores = "", modelo = "cart", tarefa = "auto",
                       metrica = "auto", tentativas = 20L, folds = 5L,
                       amplitude = "conservadora", seed = 42L) {
  modelo <- .tr_ml_enum(modelo, c("linear", "cart", "figs", "forest", "svm", "xgboost"), "modelo")
  if (identical(modelo, "linear"))
    .tr_ml_abort("tr_ml_error_not_tunable", "O modelo linear n\u{E3}o possui hiperpar\u{E2}metros nesta cole\u{E7}\u{E3}o.")
  tentativas <- .tr_ml_int(tentativas, "tentativas", 1L)
  folds <- .tr_ml_int(folds, "folds", 2L)
  seed <- .tr_ml_int(seed, "seed", 0L)
  amplitude <- .tr_ml_enum(amplitude, c("conservadora", "ampla"), "amplitude")
  d <- .tr_ml_dados(dados, resposta, preditores, tarefa)
  if (folds > d$n) .tr_ml_abort("tr_ml_error_bad_folds", "'folds' n\u{E3}o pode superar o n\u{FA}mero de linhas.")
  tarefa <- d$tarefa
  if (identical(metrica, "auto")) metrica <- if (tarefa == "regressao") "rmse" else "macro_f1"
  validas <- if (tarefa == "regressao") c("mae", "rmse", "r2") else
    c("accuracy", "balanced_accuracy", "macro_f1")
  metrica <- .tr_ml_enum(metrica, validas, "metrica")
  minimizar <- metrica %in% c("mae", "rmse")
  space <- .tr_ml_tuning_space(modelo, length(d$preditores), amplitude)

  resultado <- .tr_ml_with_seed(seed, {
    y_folds <- if (tarefa == "classificacao") factor(dados[[d$resposta]]) else dados[[d$resposta]]
    partes <- .tr_ml_make_folds(y_folds, folds)
    configs <- lapply(seq_len(tentativas), function(i) .tr_ml_sample_config(space, modelo))
    linhas <- vector("list", tentativas)
    for (i in seq_len(tentativas)) {
      cfg <- configs[[i]]; valores <- numeric()
      erro <- NULL; avisos <- character(); inicio <- proc.time()[["elapsed"]]
      for (validacao in partes) {
        treino <- dados[-validacao, , drop = FALSE]
        teste <- dados[validacao, , drop = FALSE]
        args <- c(list(dados = treino, resposta = resposta, preditores = preditores, modelo = modelo,
                       tarefa = tarefa, seed = as.integer((as.double(seed) + i) %% .Machine$integer.max)), cfg)
        valor <- tryCatch(withCallingHandlers({
            fit <- do.call(tr_ml_fit, args)
            pred <- .tr_ml_prever(fit, .tr_ml_novos_dados(fit, teste))$previsto
            .tr_ml_tune_metric(teste[[d$resposta]], pred, tarefa, metrica)
          }, warning = function(w) {
            avisos <<- c(avisos, conditionMessage(w)); invokeRestart("muffleWarning")
          }), error = function(e) { erro <<- conditionMessage(e); NA_real_ })
        valores <- c(valores, valor)
        if (!is.null(erro)) break
      }
      por_fold <- stats::setNames(as.list(c(valores, rep(NA_real_, folds - length(valores)))),
                                  paste0("fold_", seq_len(folds)))
      linha <- c(list(tentativa = i), cfg, por_fold,
                  list(media = if (all(is.finite(valores))) mean(valores) else NA_real_,
                       desvio = if (length(valores) > 1L && all(is.finite(valores))) stats::sd(valores) else NA_real_,
                       segundos = proc.time()[["elapsed"]] - inicio,
                       status = if (is.null(erro)) "ok" else "erro",
                       avisos = paste(unique(avisos), collapse = " | "),
                       erro = erro %||% ""))
      linhas[[i]] <- linha
    }
    historico <- tibble::as_tibble(do.call(rbind.data.frame, c(linhas, stringsAsFactors = FALSE)))
    historico$tentativa <- as.integer(historico$tentativa)
    numericas <- c(names(space)[vapply(space, is.numeric, logical(1))],
      paste0("fold_", seq_len(folds)), "media", "desvio", "segundos")
    for (nm in numericas) historico[[nm]] <- as.numeric(historico[[nm]])
    boas <- which(historico$status == "ok" & is.finite(historico$media))
    if (!length(boas)) .tr_ml_abort("tr_ml_error_tuning_failed", "Todas as tentativas de tuning falharam.")
    melhor <- boas[[if (minimizar) which.min(historico$media[boas]) else which.max(historico$media[boas])]]
    acumulado <- if (minimizar) cummin(replace(historico$media, !is.finite(historico$media), Inf)) else
      cummax(replace(historico$media, !is.finite(historico$media), -Inf))
    historico$melhor <- acumulado
    final_args <- c(list(dados = dados, resposta = resposta, preditores = preditores, modelo = modelo,
                         tarefa = tarefa, seed = seed), configs[[melhor]])
    # Marca de origem: os hiperparâmetros vieram destas linhas, e a cruzada
    # do `predict_cv` as repartiria do mesmo jeito (mesma semente) — ver lá.
    final <- do.call(tr_ml_fit, final_args)
    final$extras$tunado <- TRUE
    list(modelo = final, historico = historico,
         melhor_tentativa = melhor, metrica = metrica, minimizar = minimizar,
         folds = folds, seed = seed)
  })
  structure(resultado, class = "tr_ml_tuning")
}
