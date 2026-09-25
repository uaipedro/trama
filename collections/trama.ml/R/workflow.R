#' Divide dados em treino e teste
#'
#' Faz uma divisão reprodutível. Para alvos categóricos, amostra cada classe
#' nos dois subconjuntos e recusa classes singleton, que não podem ser
#' representadas em treino e teste.
#'
#' @param dados Data frame ou tibble com as observações.
#' @param alvo Nome da coluna resposta.
#' @param proporcao Fração entre 0 e 1 destinada ao treino.
#' @param estratificar Se TRUE, preserva cada classe categórica nos dois lados.
#' @param estrategia `"aleatoria"` (padrão) sorteia linhas; `"temporal"` põe
#'   no treino as linhas mais antigas segundo `ordem` e no teste as posteriores,
#'   sem partir instantes empatados; `"grupo"` sorteia grupos inteiros de
#'   `grupo` (nenhum grupo nos dois lados). Nas duas últimas `estratificar` é
#'   ignorado.
#' @param ordem Coluna de tempo (número, data ou data-hora, sem ausentes) usada
#'   por `estrategia = "temporal"`.
#' @param grupo Coluna que identifica indivíduo, lote ou área, usada por
#'   `estrategia = "grupo"`; `proporcao` passa a ser a fração dos grupos.
#' @param seed Semente inteira; o estado RNG do chamador é restaurado.
#' @return Lista com tibbles treino e teste.
#' @export
tr_ml_split <- function(dados, alvo = "", proporcao = 0.75,
                        estratificar = TRUE, estrategia = "aleatoria",
                        ordem = "", grupo = "", seed = 42L) {
  .tr_ml_validate_data(dados, alvo)
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
  y <- dados[[alvo]]
  if (anyNA(y)) {
    stop(sprintf("A coluna alvo %s cont\u{E9}m valores ausentes; trate-os antes da divis\u{E3}o.", alvo),
         call. = FALSE)
  }
  estrategia <- .tr_ml_enum(estrategia, c("aleatoria", "temporal", "grupo"), "estrategia")
  if (estrategia != "aleatoria") {
    idx <- .tr_ml_with_rng(seed, .tr_ml_split_dependente(dados, estrategia, proporcao, ordem, grupo))
    return(list(treino = tibble::as_tibble(dados[idx, , drop = FALSE]),
                teste = tibble::as_tibble(dados[-idx, , drop = FALSE])))
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
#' Não remove silenciosamente linhas: alvo e previsão ausentes produzem erro.
#' Em classificação, balanced accuracy (média das revocações), F1, precisão e
#' revocação macro são médias sobre as classes observadas no alvo; as
#' ponderadas usam o suporte de cada classe; o kappa de Cohen (1960) desconta a
#' concordância esperada pelas marginais. Precisão de classe nunca prevista e
#' F1 sem acerto valem zero.
#'
#' @param dados Data frame com alvo e previsão.
#' @param alvo Nome da coluna observada.
#' @param predito Nome da coluna prevista.
#' @param tarefa auto, regressao ou classificacao.
#' @return Tibble com colunas metrica, classe, valor e n. Na classificação,
#'   as métricas globais têm `classe` ausente e as por classe (`precision`,
#'   `recall`, `f1`) trazem a classe e seu suporte em `n`; na regressão não há
#'   coluna `classe`.
#' @export
tr_ml_evaluate <- function(dados, alvo = "", predito = ".pred",
                           tarefa = "auto") {
  .tr_ml_validate_pair(dados, alvo, predito)
  if (length(tarefa) != 1L || is.na(tarefa) ||
      !tarefa %in% c("auto", "regressao", "classificacao")) {
    stop("tarefa deve ser auto, regressao ou classificacao.", call. = FALSE)
  }
  y <- dados[[alvo]]
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
      stop("Regress\u{E3}o exige alvo e previs\u{E3}o num\u{E9}ricos.", call. = FALSE)
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
      stop("O alvo de classifica\u{E7}\u{E3}o num\u{E9}rico deve conter apenas valores finitos.", call. = FALSE)
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
#' @param alvo Nome da coluna observada.
#' @param predito Nome da coluna prevista.
#' @return Tibble com observado, previsto e n.
#' @export
tr_ml_confusion <- function(dados, alvo = "", predito = ".pred") {
  .tr_ml_validate_pair(dados, alvo, predito)
  y <- dados[[alvo]]
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
    stop("A matriz de confus\u{E3}o exige alvo e previs\u{E3}o categ\u{F3}ricos.", call. = FALSE)
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

.tr_ml_validate_data <- function(dados, alvo) {
  if (!is.data.frame(dados)) stop("dados deve ser um data frame ou tibble.", call. = FALSE)
  if (length(alvo) != 1L || is.na(alvo) || !nzchar(alvo) || !alvo %in% names(dados)) {
    stop("alvo deve ser o nome de uma coluna existente em dados.", call. = FALSE)
  }
  invisible(TRUE)
}

.tr_ml_validate_pair <- function(dados, alvo, predito) {
  .tr_ml_validate_data(dados, alvo)
  if (length(predito) != 1L || is.na(predito) || !nzchar(predito) ||
      !predito %in% names(dados)) {
    stop("predito deve ser o nome de uma coluna existente em dados.", call. = FALSE)
  }
  if (identical(alvo, predito)) stop("alvo e predito devem ser colunas diferentes.", call. = FALSE)
  invisible(TRUE)
}

.tr_ml_with_rng <- function(seed, expr) {
  .tr_ml_with_seed(seed, expr)
}

.tr_ml_classification_metrics <- function(y, p) {
  ys <- as.character(y); ps <- as.character(p)
  classes <- unique(ys)
  n <- length(ys)
  acc <- mean(ys == ps)
  # Por classe (um contra todos). Precisão de uma classe nunca prevista e F1
  # sem acerto valem 0 (convenção zero_division = 0 do scikit-learn).
  por <- vapply(classes, function(k) {
    tp <- sum(ys == k & ps == k); fp <- sum(ys != k & ps == k); fn <- sum(ys == k & ps != k)
    c(precision = if (tp + fp == 0) 0 else tp / (tp + fp),
      recall = tp / (tp + fn),
      f1 = if (tp == 0) 0 else 2 * tp / (2 * tp + fp + fn),
      suporte = tp + fn)
  }, numeric(4))
  w <- por["suporte", ] / n
  # Kappa de Cohen (1960): concordância observada contra a esperada pelas
  # marginais da tabela observado x previsto (rótulos de ambos os lados).
  rotulos <- union(ys, ps)
  pe <- sum(vapply(rotulos, function(k) mean(ys == k) * mean(ps == k), numeric(1)))
  kappa <- if (pe == 1) NA_real_ else (acc - pe) / (1 - pe)
  globais <- tibble::tibble(
    metrica = c("accuracy", "balanced_accuracy", "macro_f1", "kappa",
                "macro_precision", "macro_recall", "weighted_precision",
                "weighted_recall", "weighted_f1"),
    classe = NA_character_,
    valor = c(acc, mean(por["recall", ]), mean(por["f1", ]), kappa,
              mean(por["precision", ]), mean(por["recall", ]),
              sum(w * por["precision", ]), sum(w * por["recall", ]), sum(w * por["f1", ])),
    n = n)
  k <- length(classes)
  por_classe <- tibble::tibble(
    metrica = rep(c("precision", "recall", "f1"), times = k),
    classe = rep(classes, each = 3L),
    valor = as.numeric(por[c("precision", "recall", "f1"), ]),
    n = rep(as.integer(por["suporte", ]), each = 3L))
  rbind(globais, por_classe)
}

# Coluna auxiliar de ordem ou grupo: existe, não tem ausentes.
.tr_ml_coluna_aux <- function(dados, coluna, nome) {
  coluna <- .tr_ml_texto(coluna, nome)
  if (!nzchar(coluna))
    .tr_ml_abort("tr_ml_error_blank_param", "Param '%s' n\u{E3}o pode ficar em branco com esta estrat\u{E9}gia.", nome)
  if (!coluna %in% names(dados))
    .tr_ml_abort("tr_ml_error_unknown_column", "A coluna '%s' n\u{E3}o existe na tabela.", coluna)
  x <- dados[[coluna]]
  if (anyNA(x))
    .tr_ml_abort("tr_ml_error_missing", "A coluna '%s' tem valores ausentes; trate-os antes.", coluna)
  x
}

# Índices do treino. Temporal: o corte é o instante da linha floor(n * p) na
# ordem do tempo; entram no treino todas as linhas até ele (empates juntos),
# de modo que todo teste é estritamente posterior. Grupo: sorteia
# floor(G * p) grupos inteiros (entre 1 e G - 1).
.tr_ml_split_dependente <- function(dados, estrategia, proporcao, ordem, grupo) {
  n <- nrow(dados)
  if (estrategia == "temporal") {
    t <- .tr_ml_coluna_aux(dados, ordem, "ordem")
    if (is.character(t) || is.factor(t) || is.logical(t))
      .tr_ml_abort("tr_ml_error_bad_order", "A coluna 'ordem' deve ser num\u{E9}rica, data ou data-hora.")
    r <- xtfrm(t)
    u <- sort(unique(r))
    if (length(u) < 2L)
      .tr_ml_abort("tr_ml_error_bad_order", "A coluna 'ordem' precisa de pelo menos dois instantes distintos.")
    nt <- min(n - 1L, max(1L, floor(n * proporcao)))
    corte <- sort(r)[[nt]]
    if (corte == u[[length(u)]]) corte <- u[[length(u) - 1L]]
    return(which(r <= corte))
  }
  g <- as.character(.tr_ml_coluna_aux(dados, grupo, "grupo"))
  grupos <- unique(g)
  if (length(grupos) < 2L)
    .tr_ml_abort("tr_ml_error_bad_groups", "A divis\u{E3}o por grupo precisa de pelo menos dois grupos.")
  ng <- min(length(grupos) - 1L, max(1L, floor(length(grupos) * proporcao)))
  which(g %in% sample(grupos, ng))
}
