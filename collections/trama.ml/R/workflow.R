#' Divide dados em treino e teste
#'
#' Faz uma divisão reprodutível. Para alvos categóricos, amostra cada classe
#' nos dois subconjuntos e recusa classes singleton, que não podem ser
#' representadas em treino e teste.
#'
#' @param dados Data frame ou tibble com as observações.
#' @param resposta Nome da coluna resposta.
#' @param proporcao Fração entre 0 e 1 destinada ao treino.
#' @param estratificar Se TRUE, preserva cada classe categórica nos dois lados.
#' @param seed Semente inteira; o estado RNG do chamador é restaurado.
#' @return Lista com tibbles treino e teste.
#' @export
tr_ml_split <- function(dados, resposta = "", proporcao = 0.75,
                        estratificar = TRUE, seed = 42L) {
  .tr_ml_validate_data(dados, resposta)
  if (!is.numeric(proporcao) || length(proporcao) != 1L ||
      !is.finite(proporcao) || proporcao <= 0 || proporcao >= 1) {
    stop("proporcao deve ser um n\u{FA}mero finito estritamente entre 0 e 1.",
         call. = FALSE)
  }
  if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed) ||
      seed != floor(seed) || seed < 0 || seed > .Machine$integer.max) {
    stop("seed deve ser um inteiro entre 0 e 2147483647.", call. = FALSE)
  }
  if (length(estratificar) != 1L || !is.logical(estratificar) ||
      is.na(estratificar)) {
    stop("estratificar deve ser TRUE ou FALSE.", call. = FALSE)
  }
  n <- nrow(dados)
  if (n < 2L) stop("dados precisa ter pelo menos duas linhas.", call. = FALSE)
  y <- dados[[resposta]]
  if (anyNA(y)) {
    stop(sprintf("A coluna resposta %s cont\u{E9}m valores ausentes; trate-os antes da divis\u{E3}o.", resposta),
         call. = FALSE)
  }
  .tr_ml_with_rng(seed, {
    if (isTRUE(estratificar) &&
        (is.factor(y) || is.character(y) || is.logical(y))) {
      classes <- unique(y)
      tamanhos <- tabulate(match(y, classes))
      if (any(tamanhos < 2L)) {
        ruins <- paste(classes[tamanhos < 2L], collapse = ", ")
        stop(sprintf("Estratifica\u{E7}\u{E3}o imposs\u{ED}vel: classe(s) singleton (%s) precisam de pelo menos duas linhas.",
                     ruins), call. = FALSE)
      }
      idx <- unlist(lapply(seq_along(classes), function(i) {
        dentro <- which(y == classes[i])
        nt <- min(length(dentro) - 1L,
                  max(1L, floor(length(dentro) * proporcao)))
        sort(sample(dentro, nt, replace = FALSE))
      }), use.names = FALSE)
      idx <- sort(idx)
    } else {
      nt <- min(n - 1L, max(1L, floor(n * proporcao)))
      idx <- sort(sample.int(n, nt, replace = FALSE))
    }
    list(
      treino = tibble::as_tibble(dados[idx, , drop = FALSE]),
      teste = tibble::as_tibble(dados[-idx, , drop = FALSE])
    )
  })
}

#' Avalia previsões de regressão ou classificação
#'
#' Não remove silenciosamente linhas: resposta e previsão ausentes produzem erro.
#' Em classificação, balanced accuracy e F1 são médias macro sobre as classes
#' observadas na resposta; uma classe ausente nas previsões recebe seu recall/F1
#' correspondente (zero quando aplicável).
#'
#' @param dados Data frame com resposta e previsão.
#' @param resposta Nome da coluna observada.
#' @param predito Nome da coluna prevista.
#' @param tarefa auto, regressao ou classificacao.
#' @return Tibble com colunas metrica, valor e n.
#' @export
tr_ml_evaluate <- function(dados, resposta = "", predito = ".pred",
                           tarefa = "auto") {
  .tr_ml_validate_pair(dados, resposta, predito)
  if (length(tarefa) != 1L || is.na(tarefa) ||
      !tarefa %in% c("auto", "regressao", "classificacao")) {
    stop("tarefa deve ser auto, regressao ou classificacao.", call. = FALSE)
  }
  y <- dados[[resposta]]
  p <- dados[[predito]]
  if (anyNA(y) || anyNA(p)) {
    stop("Alvo e previs\u{E3}o n\u{E3}o podem conter valores ausentes; nenhuma linha foi descartada.",
         call. = FALSE)
  }
  if (!length(y)) {
    stop("Alvo e previs\u{E3}o precisam conter pelo menos uma observa\u{E7}\u{E3}o.",
         call. = FALSE)
  }
  classe <- if (tarefa == "auto") {
    if (is.factor(y) || is.character(y) || is.logical(y) ||
        is.factor(p) || is.character(p) || is.logical(p)) "classificacao" else "regressao"
  } else tarefa
  if (classe == "regressao") {
    if (!is.numeric(y) || !is.numeric(p)) {
      stop("Regress\u{E3}o exige resposta e previs\u{E3}o num\u{E9}ricos.", call. = FALSE)
    }
    if (any(!is.finite(y)) || any(!is.finite(p))) {
      stop("Alvo e previs\u{E3}o de regress\u{E3}o devem conter apenas valores finitos.",
           call. = FALSE)
    }
    erro <- p - y
    sst <- sum((y - mean(y))^2)
    tibble::tibble(
      metrica = c("mae", "rmse", "r2"),
      valor = c(mean(abs(erro)), sqrt(mean(erro^2)),
                if (sst == 0) NA_real_ else 1 - sum(erro^2) / sst),
      n = length(y)
    )
  } else {
    if (is.numeric(y) && any(!is.finite(y))) {
      stop("A resposta de classifica\u{E7}\u{E3}o num\u{E9}rico deve conter apenas valores finitos.", call. = FALSE)
    }
    if (is.numeric(p) && any(!is.finite(p))) {
      stop("A previs\u{E3}o de classifica\u{E7}\u{E3}o num\u{E9}rica deve conter apenas valores finitos.", call. = FALSE)
    }
    .tr_ml_classification_metrics(y, p)
  }
}

#' Produz uma matriz de confusão em formato longo
#'
#' @param dados Data frame com as colunas observada e prevista.
#' @param resposta Nome da coluna observada.
#' @param predito Nome da coluna prevista.
#' @return Tibble com observado, previsto e n.
#' @export
tr_ml_confusion <- function(dados, resposta = "", predito = ".pred") {
  .tr_ml_validate_pair(dados, resposta, predito)
  y <- dados[[resposta]]
  p <- dados[[predito]]
  if (anyNA(y) || anyNA(p)) {
    stop("Alvo e previs\u{E3}o n\u{E3}o podem conter valores ausentes; nenhuma linha foi descartada.",
         call. = FALSE)
  }
  if ((is.numeric(y) && any(!is.finite(y))) ||
      (is.numeric(p) && any(!is.finite(p)))) {
    stop("Alvo e previs\u{E3}o categ\u{F3}ricos num\u{E9}ricos devem conter apenas valores finitos.",
         call. = FALSE)
  }
  categ <- function(x) is.factor(x) || is.character(x) || is.logical(x) ||
    is.numeric(x)
  if (!categ(y) || !categ(p)) {
    stop("A matriz de confus\u{E3}o exige resposta e previs\u{E3}o categ\u{F3}ricos.", call. = FALSE)
  }
  ys <- as.character(y); ps <- as.character(p)
  niveis <- unique(c(ys, ps))
  tab <- table(factor(ys, levels = niveis), factor(ps, levels = niveis))
  out <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(out) <- c("observado", "previsto", "n")
  tibble::as_tibble(out)
}

#' Carrega um conjunto pequeno para exemplos da coleção ML
#'
#' @param nome iris, iris_binaria ou mtcars.
#' @return Um tibble.
#' @export
tr_ml_example <- function(nome = "iris") {
  if (length(nome) != 1L || is.na(nome) ||
      !nome %in% c("iris", "iris_binaria", "mtcars")) {
    stop("nome deve ser iris, iris_binaria ou mtcars.", call. = FALSE)
  }
  if (nome == "iris") return(tibble::as_tibble(datasets::iris))
  if (nome == "mtcars") return(tibble::as_tibble(datasets::mtcars, rownames = "modelo"))
  out <- datasets::iris[datasets::iris$Species != "setosa", , drop = FALSE]
  out$Species <- droplevels(out$Species)
  tibble::as_tibble(out)
}

.tr_ml_validate_data <- function(dados, resposta) {
  if (!is.data.frame(dados)) stop("dados deve ser um data frame ou tibble.", call. = FALSE)
  if (length(resposta) != 1L || is.na(resposta) || !nzchar(resposta) || !resposta %in% names(dados)) {
    stop("resposta deve ser o nome de uma coluna existente em dados.", call. = FALSE)
  }
  invisible(TRUE)
}

.tr_ml_validate_pair <- function(dados, resposta, predito) {
  .tr_ml_validate_data(dados, resposta)
  if (length(predito) != 1L || is.na(predito) || !nzchar(predito) ||
      !predito %in% names(dados)) {
    stop("predito deve ser o nome de uma coluna existente em dados.", call. = FALSE)
  }
  if (identical(resposta, predito)) stop("resposta e predito devem ser colunas diferentes.", call. = FALSE)
  invisible(TRUE)
}

.tr_ml_with_rng <- function(seed, expr) {
  .tr_ml_with_seed(seed, expr)
}

.tr_ml_classification_metrics <- function(y, p) {
  ys <- as.character(y); ps <- as.character(p)
  classes <- unique(ys)
  acc <- mean(ys == ps)
  recalls <- vapply(classes, function(k) {
    den <- sum(ys == k)
    sum(ys == k & ps == k) / den
  }, numeric(1))
  f1 <- vapply(classes, function(k) {
    tp <- sum(ys == k & ps == k)
    fp <- sum(ys != k & ps == k)
    fn <- sum(ys == k & ps != k)
    if (tp == 0 || (2 * tp + fp + fn) == 0) 0 else 2 * tp / (2 * tp + fp + fn)
  }, numeric(1))
  tibble::tibble(
    metrica = c("accuracy", "balanced_accuracy", "macro_f1"),
    valor = c(acc, mean(recalls), mean(f1)),
    n = length(ys)
  )
}
