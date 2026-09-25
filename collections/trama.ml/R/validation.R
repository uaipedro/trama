# Validações compartilhadas pelos modelos de aprendizado de máquina.

.tr_ml_abort <- function(classe, ..., call = NULL) {
  stop(structure(list(message = sprintf(...), call = call),
                 class = c(classe, "tr_ml_error", "error", "condition")))
}

.tr_ml_texto <- function(x, nome) {
  if (length(x) != 1L || is.na(x) || !is.character(x)) {
    .tr_ml_abort("tr_ml_error_bad_param", "Param '%s' deve ser um texto \u{FA}nico.", nome)
  }
  trimws(x)
}

.tr_ml_enum <- function(x, opcoes, nome) {
  x <- tolower(.tr_ml_texto(x, nome))
  if (!x %in% opcoes) {
    .tr_ml_abort("tr_ml_error_bad_option",
                 "Param '%s': escolha uma destas op\u{E7}\u{F5}es: %s.", nome,
                 paste(sprintf("'%s'", opcoes), collapse = ", "))
  }
  x
}

.tr_ml_int <- function(x, nome, min = 0L) {
  if (length(x) != 1L || is.na(x) || !is.numeric(x) || !is.finite(x) ||
      x != floor(x) || x < min || x > .Machine$integer.max) {
    .tr_ml_abort("tr_ml_error_bad_param",
                 "Param '%s' deve ser um inteiro maior ou igual a %d.", nome, min)
  }
  as.integer(x)
}

.tr_ml_num <- function(x, nome, min = -Inf, aberto = FALSE) {
  ruim <- length(x) != 1L || is.na(x) || !is.numeric(x) || !is.finite(x) ||
    if (aberto) x <= min else x < min
  if (ruim) {
    comp <- if (aberto) "maior que" else "maior ou igual a"
    .tr_ml_abort("tr_ml_error_bad_param", "Param '%s' deve ser num\u{E9}rico, finito e %s %s.",
                 nome, comp, format(min))
  }
  as.numeric(x)
}

.tr_ml_cols <- function(preditores) {
  if (length(preditores) > 1L) return(trimws(as.character(preditores)))
  preditores <- .tr_ml_texto(preditores, "preditores")
  if (!nzchar(preditores)) return(character())
  trimws(strsplit(preditores, ",", fixed = TRUE)[[1L]])
}

.tr_ml_dados <- function(dados, resposta, preditores, tarefa) {
  if (!is.data.frame(dados)) {
    .tr_ml_abort("tr_ml_error_not_table", "Param 'dados' deve ser uma tabela (data.frame ou tibble).")
  }
  if (!nrow(dados)) .tr_ml_abort("tr_ml_error_empty_data", "A tabela n\u{E3}o tem nenhuma linha.")
  resposta <- .tr_ml_texto(resposta, "resposta")
  if (!nzchar(resposta)) .tr_ml_abort("tr_ml_error_blank_param", "Param 'resposta' n\u{E3}o pode ficar em branco.")
  if (!resposta %in% names(dados)) {
    .tr_ml_abort("tr_ml_error_unknown_column", "A coluna resposta '%s' n\u{E3}o existe na tabela.", resposta)
  }
  pred <- .tr_ml_cols(preditores)
  if (!length(pred)) pred <- setdiff(names(dados)[vapply(dados, is.numeric, TRUE)], resposta)
  if (any(!nzchar(pred)) || anyDuplicated(pred)) {
    .tr_ml_abort("tr_ml_error_bad_columns", "Param 'preditores' cont\u{E9}m nome vazio ou repetido.")
  }
  desconhecidas <- setdiff(pred, names(dados))
  if (length(desconhecidas)) {
    .tr_ml_abort("tr_ml_error_unknown_column", "Estas colunas n\u{E3}o existem: %s.",
                 paste(sprintf("'%s'", desconhecidas), collapse = ", "))
  }
  if (resposta %in% pred) {
    .tr_ml_abort("tr_ml_error_target_leakage",
                 "A coluna resposta '%s' tamb\u{E9}m aparece nos preditores. Retire-a de 'preditores'.", resposta)
  }
  if (!length(pred)) {
    .tr_ml_abort("tr_ml_error_no_predictors",
                 "Nenhum preditor num\u{E9}rico foi encontrado. Informe-os em 'preditores'.")
  }
  nao_num <- pred[!vapply(dados[pred], is.numeric, TRUE)]
  if (length(nao_num)) {
    .tr_ml_abort("tr_ml_error_not_numeric",
                 "Nesta vers\u{E3}o, os preditores devem ser num\u{E9}ricos; ajuste: %s.",
                 paste(sprintf("'%s'", nao_num), collapse = ", "))
  }
  usados <- dados[c(resposta, pred)]
  if (anyNA(usados)) {
    .tr_ml_abort("tr_ml_error_missing",
                 "H\u{E1} valores ausentes na resposta ou nos preditores. Impute ou remova essas linhas antes do ajuste.")
  }
  finitos <- vapply(usados[pred], function(x) all(is.finite(x)), TRUE)
  if (is.numeric(usados[[resposta]])) finitos <- c(finitos, resposta = all(is.finite(usados[[resposta]])))
  if (!all(finitos)) {
    .tr_ml_abort("tr_ml_error_nonfinite", "H\u{E1} valores Inf ou -Inf na resposta ou nos preditores.")
  }
  tarefa <- .tr_ml_enum(tarefa, c("auto", "regressao", "classificacao"), "tarefa")
  if (tarefa == "auto") tarefa <- if (is.factor(usados[[resposta]]) || is.character(usados[[resposta]])) "classificacao" else "regressao"
  if (tarefa == "regressao") {
    if (!is.numeric(usados[[resposta]])) {
      .tr_ml_abort("tr_ml_error_bad_target", "Regress\u{E3}o exige uma coluna resposta num\u{E9}rica.")
    }
    y <- as.numeric(usados[[resposta]])
    niveis <- NULL
  } else {
    y <- factor(usados[[resposta]])
    if (nlevels(y) < 2L) .tr_ml_abort("tr_ml_error_bad_target", "Classifica\u{E7}\u{E3}o exige pelo menos duas classes observadas.")
    niveis <- levels(y)
  }
  x <- as.data.frame(usados[pred], check.names = FALSE)
  internos <- paste0("x", seq_along(pred))
  names(x) <- internos
  list(x = x, y = y, resposta = resposta, preditores = pred, internos = internos,
       tarefa = tarefa, niveis = niveis, n = nrow(dados))
}

#' Os preditores de uma tabela nova, com os nomes internos do motor.
#'
#' A models já conferiu que as colunas existem (`.tr_models_novos`); aqui fica
#' o que é da ml: número finito e sem faltante. Colunas `previsto`/`prob_*` já
#' existentes não são problema daqui — a `models/predict` as substitui.
#' @noRd
.tr_ml_novos_dados <- function(modelo, dados) {
  if (!is.data.frame(dados)) .tr_ml_abort("tr_ml_error_not_table", "Param 'dados' deve ser uma tabela.")
  .tr_ml_require(modelo$extras$engine, modelo$modelo)
  faltam <- setdiff(modelo$preditores, names(dados))
  if (length(faltam)) .tr_ml_abort("tr_ml_error_unknown_column", "Faltam preditores na tabela: %s.", paste(sprintf("'%s'", faltam), collapse = ", "))
  x <- dados[modelo$preditores]
  if (anyNA(x) || !all(vapply(x, function(z) is.numeric(z) && all(is.finite(z)), TRUE))) {
    .tr_ml_abort("tr_ml_error_bad_newdata", "Os novos preditores devem ser num\u{E9}ricos, finitos e sem valores ausentes.")
  }
  names(x) <- modelo$internos
  as.data.frame(x)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

.tr_ml_require <- function(pkg, modelo) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    .tr_ml_abort("tr_ml_error_missing_engine",
                 "O modelo '%s' precisa do pacote opcional '%s'. Instale-o com install.packages(\"%s\").",
                 modelo, pkg, pkg)
  }
}

.tr_ml_with_seed <- function(seed, code) {
  tinha <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (tinha) anterior <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(kind))
    if (tinha) assign(".Random.seed", anterior, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  force(code)
}
