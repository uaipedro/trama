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

.tr_ml_exigir_estrat_classes <- function(estrategia, tarefa) {
  if (estrategia == "grupo_estratificado" && tarefa != "classificacao")
    .tr_ml_abort("tr_ml_error_bad_option",
                 "A estrat\u{E9}gia 'grupo_estratificado' equilibra classes e s\u{F3} serve para classifica\u{E7}\u{E3}o; use 'grupo'.")
}

# Partições de validação: lista de pares treino/validação (índices de linha).
# aleatoria: k folds sorteados, estratificados pela classe (versão anterior).
# grupo: grupos inteiros distribuídos em k folds (nenhum grupo dos dois lados).
# temporal: origem móvel com janela crescente (Tashman 2000; FPP3, sec. 5.10):
#   os instantes distintos, em ordem, formam k + 1 blocos contíguos; a origem i
#   treina nos blocos 1..i e valida no bloco i + 1.
.tr_ml_folds <- function(dados, estrategia, k, y = NULL, ordem = "", grupo = "") {
  n <- nrow(dados)
  todos <- seq_len(n)
  par <- function(v) list(treino = setdiff(todos, v), validacao = v)
  if (estrategia == "aleatoria") return(lapply(.tr_ml_make_folds(y, k), par))
  if (estrategia == "grupo_estratificado") return(lapply(.tr_ml_folds_grupo_estrat(dados, k, y, grupo), par))
  if (estrategia == "grupo") {
    g <- as.character(.tr_ml_coluna_aux(dados, grupo, "grupo"))
    grupos <- unique(g)
    if (k > length(grupos))
      .tr_ml_abort("tr_ml_error_bad_folds", "'folds' n\u{E3}o pode superar o n\u{FA}mero de grupos (%d).", length(grupos))
    id <- stats::setNames(sample(rep(seq_len(k), length.out = length(grupos))), grupos)
    return(lapply(seq_len(k), function(i) par(unname(which(id[g] == i)))))
  }
  t <- .tr_ml_coluna_aux(dados, ordem, "ordem")
  if (is.character(t) || is.factor(t) || is.logical(t))
    .tr_ml_abort("tr_ml_error_bad_order", "A coluna 'ordem' deve ser num\u{E9}rica, data ou data-hora.")
  r <- xtfrm(t); u <- sort(unique(r))
  if (length(u) < k + 1L)
    .tr_ml_abort("tr_ml_error_bad_folds", "A valida\u{E7}\u{E3}o temporal com %d folds precisa de pelo menos %d instantes distintos.", k, k + 1L)
  bloco <- ceiling(seq_along(u) * (k + 1L) / length(u))[match(r, u)]
  lapply(seq_len(k), function(i) list(treino = which(bloco <= i), validacao = which(bloco == i + 1L)))
}

# Grupos inteiros em k folds equilibrando as classes, como o StratifiedGroupKFold
# do scikit-learn: grupos embaralhados (pela semente) e ordenados pelo desvio
# da sua distribuição de classes, do mais concentrado ao mais misto; cada um
# vai ao fold que deixa menor a média, nas classes, do desvio-padrão entre
# folds da fração da classe; empate, o fold com menos linhas e depois o de
# menor índice. Não garante as duas classes em todo fold (grupos grandes
# demais podem impedir), mas evita os folds puros que o sorteio produz.
.tr_ml_folds_grupo_estrat <- function(dados, k, y, grupo) {
  g <- as.character(.tr_ml_coluna_aux(dados, grupo, "grupo"))
  grupos <- unique(g)
  if (k > length(grupos))
    .tr_ml_abort("tr_ml_error_bad_folds", "'folds' n\u{E3}o pode superar o n\u{FA}mero de grupos (%d).", length(grupos))
  y <- factor(y)
  cont <- unclass(table(factor(g, levels = grupos), y))       # grupos x classes
  total <- colSums(cont)
  grupos_ord <- sample(seq_along(grupos))
  desvio <- apply(cont[grupos_ord, , drop = FALSE], 1L, stats::sd)
  grupos_ord <- grupos_ord[order(-desvio, method = "radix")]
  por_fold <- matrix(0, k, ncol(cont)); fold_de <- integer(length(grupos))
  for (gi in grupos_ord) {
    custo <- vapply(seq_len(k), function(f) {
      m <- por_fold; m[f, ] <- m[f, ] + cont[gi, ]
      mean(apply(sweep(m, 2L, total, "/"), 2L, stats::sd))
    }, numeric(1))
    cand <- which(abs(custo - min(custo)) < 1e-12)
    cand <- cand[order(rowSums(por_fold)[cand], cand)][[1L]]
    por_fold[cand, ] <- por_fold[cand, ] + cont[gi, ]; fold_de[[gi]] <- cand
  }
  id <- stats::setNames(fold_de, grupos)
  lapply(seq_len(k), function(i) unname(which(id[g] == i)))
}

# Folds de validação com uma só classe: macro F1, kappa e acurácia balanceada
# degeneram neles. Devolve a nota (ou NULL) e avisa.
.tr_ml_nota_folds_puros <- function(partes, y, tarefa, onde) {
  if (tarefa != "classificacao") return(NULL)
  puros <- which(vapply(partes, function(p) length(unique(y[p$validacao])) < 2L, logical(1)))
  if (!length(puros)) return(NULL)
  nota <- sprintf(paste(
    "%s: %d de %d folds de valida\u{E7}\u{E3}o t\u{EA}m uma classe s\u{F3} (folds %s);",
    "neles macro F1, kappa e acur\u{E1}cia balanceada degeneram. Use estrategia",
    "'grupo_estratificado' ou menos folds."), onde, length(puros), length(partes),
    paste(puros, collapse = ", "))
  warning(nota, call. = FALSE)
  nota
}

# Sem `cols`, os preditores são os numéricos exceto alvo, ordem e grupo.
.tr_ml_cols_sem_aux <- function(dados, alvo, cols, ordem, grupo) {
  if (length(.tr_ml_cols(cols))) return(cols)
  num <- names(dados)[vapply(dados, is.numeric, TRUE)]
  paste(setdiff(num, c(alvo, trimws(ordem), trimws(grupo))), collapse = ", ")
}

.tr_ml_tune_metric <- function(pred, alvo, tarefa, metrica) {
  # A validação de um fold é parte do treino marcado pelo `ml/split`, mas não
  # ajustou o modelo do fold: a medida é fora da amostra, sem nota de otimismo.
  attr(pred, .tr_ml_origem_attr) <- NULL
  z <- tr_ml_evaluate(pred, alvo, tarefa = tarefa)
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
#' @param alvo Nome de uma coluna existente em `dados`.
#' @param cols Preditores numéricos separados por vírgula. Vazio usa todos os
#'   numéricos, exceto `alvo`.
#' @param modelo Família com hiperparâmetros: `"cart"`, `"figs"`, `"forest"`,
#'   `"svm"` ou `"xgboost"`.
#'   Com `"cart"`, cada ajuste inclui a poda 1-EP de [tr_ml_fit()], com sua
#'   própria validação cruzada interna (até 10 ajustes extras); `cp` não entra
#'   na busca porque a poda já o escolhe.
#' @param tarefa Uma de `"auto"`, `"regressao"` ou `"classificacao"`. `"auto"`
#'   interpreta resposta numérica como regressão.
#' @param metrica Métrica compatível com a tarefa. `"auto"` usa `"rmse"` em
#'   regressão e `"macro_f1"` em classificação.
#' @param tentativas Número inteiro positivo de configurações avaliadas.
#' @param folds Número inteiro de partições, a partir de dois. Não pode superar
#'   o número de linhas nem o tamanho da menor classe.
#' @param amplitude Limites `"conservadora"` ou `"ampla"` para a busca.
#' @param estrategia Como formar os folds: `"aleatoria"` (padrão; estratificada
#'   pela classe), `"grupo"` (grupos inteiros de `grupo` por fold),
#'   `"grupo_estratificado"` (grupos inteiros, distribuídos para equilibrar as
#'   classes entre os folds, como o StratifiedGroupKFold do scikit-learn; só
#'   classificação) ou
#'   `"temporal"` (origem móvel com janela crescente: os instantes de `ordem`
#'   formam `folds + 1` blocos contíguos e cada fold valida o bloco seguinte ao
#'   treino).
#' @param ordem Coluna de tempo para `estrategia = "temporal"`.
#' @param grupo Coluna de grupo para `estrategia = "grupo"`. Com `cols` vazio,
#'   `ordem` e `grupo` nunca entram como preditores.
#' @param seed Inteiro entre zero e 2147483647. Controla folds, configurações e
#'   ajustes sem alterar o estado aleatório da sessão.
#' @return Objeto `tr_ml_tuning`: lista com o `modelo` vencedor reajustado,
#'   `historico` por tentativa e fold, índice `melhor_tentativa`, `metrica`,
#'   direção `minimizar`, número de `folds`, `seed` e `nota` (NULL, ou o aviso
#'   de folds de validação com uma classe só).
#' @export
tr_ml_tune <- function(dados, alvo = "", cols = "", modelo = "cart", tarefa = "auto",
                       metrica = "auto", tentativas = 20L, folds = 5L,
                       amplitude = "conservadora", estrategia = "aleatoria",
                       ordem = "", grupo = "", seed = 42L) {
  modelo <- .tr_ml_enum(modelo, c("linear", "cart", "figs", "forest", "svm", "xgboost"), "modelo")
  if (identical(modelo, "linear"))
    .tr_ml_abort("tr_ml_error_not_tunable", "O modelo linear n\u{E3}o possui hiperpar\u{E2}metros nesta cole\u{E7}\u{E3}o.")
  tentativas <- .tr_ml_int(tentativas, "tentativas", 1L)
  folds <- .tr_ml_int(folds, "folds", 2L)
  seed <- .tr_ml_int(seed, "seed", 0L)
  amplitude <- .tr_ml_enum(amplitude, c("conservadora", "ampla"), "amplitude")
  estrategia <- .tr_ml_enum(estrategia, c("aleatoria", "temporal", "grupo", "grupo_estratificado"), "estrategia")
  cols <- .tr_ml_cols_sem_aux(dados, alvo, cols, ordem, grupo)
  d <- .tr_ml_dados(dados, alvo, cols, tarefa)
  .tr_ml_exigir_estrat_classes(estrategia, d$tarefa)
  if (folds > d$n) .tr_ml_abort("tr_ml_error_bad_folds", "'folds' n\u{E3}o pode superar o n\u{FA}mero de linhas.")
  tarefa <- d$tarefa
  if (identical(metrica, "auto")) metrica <- if (tarefa == "regressao") "rmse" else "macro_f1"
  validas <- if (tarefa == "regressao") c("mae", "rmse", "r2") else
    c("accuracy", "balanced_accuracy", "macro_f1", "kappa", "weighted_f1")
  metrica <- .tr_ml_enum(metrica, validas, "metrica")
  minimizar <- metrica %in% c("mae", "rmse")
  space <- .tr_ml_tuning_space(modelo, length(d$preditores), amplitude)

  # A entrada já foi checada por `.tr_ml_dados`; os folds usam uma cópia sem
  # marca e ajustam sem impressões (só o reajuste final as calcula).
  base <- dados; attr(base, .tr_ml_origem_attr) <- NULL
  resultado <- .tr_ml_with_seed(seed, {
    y_folds <- if (tarefa == "classificacao") factor(dados[[d$alvo]]) else dados[[d$alvo]]
    partes <- .tr_ml_folds(dados, estrategia, folds, y_folds, ordem, grupo)
    nota <- .tr_ml_nota_folds_puros(partes, y_folds, tarefa, "ml/tune")
    configs <- lapply(seq_len(tentativas), function(i) .tr_ml_sample_config(space, modelo))
    linhas <- vector("list", tentativas)
    for (i in seq_len(tentativas)) {
      cfg <- configs[[i]]; valores <- numeric()
      erro <- NULL; avisos <- character(); inicio <- proc.time()[["elapsed"]]
      for (parte in partes) {
        treino <- base[parte$treino, , drop = FALSE]
        teste <- base[parte$validacao, , drop = FALSE]
        args <- c(list(dados = treino, alvo = alvo, cols = cols, modelo = modelo,
                       tarefa = tarefa, seed = as.integer((as.double(seed) + i) %% .Machine$integer.max)), cfg)
        valor <- tryCatch(withCallingHandlers({
            fit <- .tr_ml_sem_impressao(do.call(tr_ml_fit, args))
            pred <- tr_ml_predict(fit, teste)
            .tr_ml_tune_metric(pred, alvo, tarefa, metrica)
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
    final_args <- c(list(dados = dados, alvo = alvo, cols = cols, modelo = modelo,
                         tarefa = tarefa, seed = seed), configs[[melhor]])
    list(modelo = do.call(tr_ml_fit, final_args), historico = historico,
         melhor_tentativa = melhor, metrica = metrica, minimizar = minimizar,
         folds = folds, estrategia = estrategia, seed = seed, nota = nota)
  })
  structure(resultado, class = "tr_ml_tuning")
}

#' Validação cruzada aninhada
#'
#' Estima o desempenho de todo o procedimento de ajuste — busca de
#' hiperparâmetros incluída — sem reaproveitar as linhas que escolheram o
#' vencedor (Varma & Simon 2006). Cada fold externo roda um [tr_ml_tune()]
#' completo só no seu treino e mede o vencedor na sua validação, que a busca
#' nunca viu.
#' @inheritParams tr_ml_tune
#' @param folds_externos Partições externas, a partir de dois.
#' @param folds Partições internas de cada busca.
#' @return Tibble com uma linha por fold externo (`fold`, `n_treino`,
#'   `n_validacao`, `tentativa`, `interna` = média dos folds internos do
#'   vencedor, otimista; `externa` = métrica na validação externa) e uma linha
#'   final `fold = "media"` com as médias; a estimativa honesta é `externa`.
#'   Com folds externos de validação de uma classe só, avisa e guarda a nota
#'   no atributo `nota`.
#' @examples
#' d <- tr_ml_example("iris_binaria")
#' tr_ml_nested_cv(d, alvo = "Species", tentativas = 3, folds_externos = 3, folds = 3)
#' @export
tr_ml_nested_cv <- function(dados, alvo = "", cols = "", modelo = "cart", tarefa = "auto",
                            metrica = "auto", tentativas = 10L, folds_externos = 5L,
                            folds = 3L, amplitude = "conservadora", estrategia = "aleatoria",
                            ordem = "", grupo = "", seed = 42L) {
  modelo <- .tr_ml_enum(modelo, c("cart", "figs", "forest", "svm", "xgboost"), "modelo")
  folds_externos <- .tr_ml_int(folds_externos, "folds_externos", 2L)
  seed <- .tr_ml_int(seed, "seed", 0L)
  estrategia <- .tr_ml_enum(estrategia, c("aleatoria", "temporal", "grupo", "grupo_estratificado"), "estrategia")
  cols <- .tr_ml_cols_sem_aux(dados, alvo, cols, ordem, grupo)
  d <- .tr_ml_dados(dados, alvo, cols, tarefa)
  tarefa <- d$tarefa
  .tr_ml_exigir_estrat_classes(estrategia, tarefa)
  if (folds_externos > d$n)
    .tr_ml_abort("tr_ml_error_bad_folds", "'folds_externos' n\u{E3}o pode superar o n\u{FA}mero de linhas.")
  partes <- .tr_ml_with_seed(seed, {
    y <- if (tarefa == "classificacao") factor(dados[[d$alvo]]) else dados[[d$alvo]]
    .tr_ml_folds(dados, estrategia, folds_externos, y, ordem, grupo)
  })
  y_ext <- dados[[d$alvo]]
  nota <- .tr_ml_nota_folds_puros(partes, y_ext, tarefa, "ml/nested_cv (folds externos)")
  base <- dados; attr(base, .tr_ml_origem_attr) <- NULL
  linhas <- .tr_ml_sem_impressao(lapply(seq_along(partes), function(i) {
    parte <- partes[[i]]
    treino <- base[parte$treino, , drop = FALSE]
    validacao <- base[parte$validacao, , drop = FALSE]
    z <- tr_ml_tune(treino, alvo = alvo, cols = cols, modelo = modelo, tarefa = tarefa,
                    metrica = metrica, tentativas = tentativas, folds = folds,
                    amplitude = amplitude, estrategia = estrategia, ordem = ordem, grupo = grupo,
                    seed = as.integer((as.double(seed) + i) %% .Machine$integer.max))
    pred <- tr_ml_predict(z$modelo, validacao)
    tibble::tibble(fold = as.character(i), n_treino = nrow(treino), n_validacao = nrow(validacao),
                   tentativa = z$melhor_tentativa, metrica = z$metrica,
                   interna = z$historico$media[[z$melhor_tentativa]],
                   externa = .tr_ml_tune_metric(pred, alvo, tarefa, z$metrica))
  }))
  out <- do.call(rbind, linhas)
  out <- rbind(out, tibble::tibble(fold = "media", n_treino = NA_integer_, n_validacao = NA_integer_,
                                   tentativa = NA_integer_, metrica = out$metrica[[1]],
                                   interna = mean(out$interna), externa = mean(out$externa)))
  if (!is.null(nota)) attr(out, "nota") <- nota
  out
}
