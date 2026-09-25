# Ajuste, previsão e inspeção de modelos clássicos de aprendizado de máquina.

#' Ajustar um modelo clássico de aprendizado de máquina
#'
#' Ajusta regressão linear/logística, CART, FIGS, floresta aleatória, SVM ou
#' XGBoost sobre preditores numéricos. Os engines além de `stats` são opcionais
#' e a mensagem de erro informa exatamente qual pacote instalar.
#'
#' @param dados Tabela de treino.
#' @param alvo Nome da coluna a prever.
#' @param cols Preditores separados por vírgula; em branco, usa todas as
#'   colunas numéricas, exceto o alvo.
#' @param modelo Um de `"linear"`, `"cart"`, `"figs"`, `"forest"`, `"svm"`
#'   ou `"xgboost"`.
#' @param tarefa `"auto"`, `"regressao"` ou `"classificacao"`.
#' @param seed Semente local; o estado aleatório da sessão é preservado.
#' @param max_depth Profundidade máxima das árvores (CART, forest e XGBoost).
#' @param min_n Mínimo por folha no CART; tamanho mínimo do nó em FIGS e ranger.
#' @param max_splits Orçamento total de divisões do FIGS.
#' @param trees Número de árvores da floresta.
#' @param mtry Preditores candidatos por divisão; zero usa raiz de p.
#' @param cost Penalidade dos erros da SVM, estritamente positiva.
#' @param gamma Escala do kernel da SVM.
#' @param kernel Kernel linear, radial, polynomial ou sigmoid.
#' @param nrounds Rodadas do XGBoost.
#' @param eta Taxa de aprendizado do XGBoost.
#' @param cp Parâmetro de complexidade do CART: a árvore cresce só com divisões
#'   que melhoram o ajuste relativo em pelo menos `cp`; zero cresce a árvore
#'   máxima permitida por `max_depth` e `min_n`, depois podada.
#' @param poda Poda do CART por custo-complexidade, escolhida pela validação
#'   cruzada do `rpart` com min(10, n) folds (Breiman et al. 1984): `"1ep"` fica com a
#'   menor árvore cujo erro de validação não passa do mínimo mais um
#'   erro-padrão; `"minimo"`, com a de menor erro; `"nenhuma"` não poda.
#' @param corte Na logística binária, a classe prevista é a segunda quando sua
#'   probabilidade é maior ou igual a `corte` (entre 0 e 1, exclusivos).
#'   Ignorado pelos demais modelos.
#' @param importancia Medida de importância da floresta (`ranger`):
#'   `"impureza"` (padrão, redução de Gini/variância, enviesada para
#'   preditores com muitos valores; Strobl et al. 2007), `"permutacao"`
#'   (aumento do erro fora da bolsa ao permutar o preditor, Breiman 2001: erro
#'   quadrático médio na regressão, erro de Brier na classificação) ou
#'   `"impureza_corrigida"` (AIR, Nembrini, König & Wright 2018). Ignorado
#'   pelos demais modelos.
#' @return Objeto `tr_ml_fit`. Guarda as impressões digitais das linhas de
#'   treino (`treino_impressoes`), com que `ml/predict` recusa prever linhas
#'   do teste que o modelo viu.
#' @export
tr_ml_fit <- function(dados, alvo = "", cols = "", modelo = "cart", tarefa = "auto",
                      seed = 42L, max_depth = 3L, min_n = 5L, max_splits = 6L,
                      trees = 200L, mtry = 0L, cost = 1, gamma = 0.1,
                      kernel = "radial", nrounds = 100L, eta = 0.1,
                      cp = 0, poda = "1ep", corte = 0.5, importancia = "impureza") {
  modelo <- .tr_ml_enum(modelo, c("linear", "cart", "figs", "forest", "svm", "xgboost"), "modelo")
  seed <- .tr_ml_int(seed, "seed", 0L); max_depth <- .tr_ml_int(max_depth, "max_depth", 1L)
  min_n <- .tr_ml_int(min_n, "min_n", 1L); max_splits <- .tr_ml_int(max_splits, "max_splits", 1L)
  trees <- .tr_ml_int(trees, "trees", 1L); mtry <- .tr_ml_int(mtry, "mtry", 0L)
  cost <- .tr_ml_num(cost, "cost", 0, TRUE); gamma <- .tr_ml_num(gamma, "gamma", 0)
  nrounds <- .tr_ml_int(nrounds, "nrounds", 1L); eta <- .tr_ml_num(eta, "eta", 0, TRUE)
  kernel <- .tr_ml_enum(kernel, c("linear", "polynomial", "radial", "sigmoid"), "kernel")
  corte <- .tr_ml_num(corte, "corte", 0, TRUE)
  if (corte >= 1) .tr_ml_abort("tr_ml_error_bad_param", "Param 'corte' deve estar entre 0 e 1, exclusivos.")
  cp <- .tr_ml_num(cp, "cp", 0); poda <- .tr_ml_enum(poda, c("1ep", "minimo", "nenhuma"), "poda")
  importancia <- .tr_ml_enum(importancia, c("impureza", "permutacao", "impureza_corrigida"), "importancia")
  d <- .tr_ml_dados(dados, alvo, cols, tarefa)
  if (d$tarefa == "classificacao" && length(d$niveis) > 2L && modelo %in% c("linear", "figs")) {
    .tr_ml_abort("tr_ml_error_binary_only", "O modelo '%s' aceita classifica\u{E7}\u{E3}o com exatamente duas classes.", modelo)
  }
  treino <- d$x; treino$.y <- d$y
  f <- stats::reformulate(d$internos, response = ".y")
  # Todos os nomes da fórmula vêm da tabela; não capture o frame do ajuste
  # (e suas cópias de dados) ao persistir o modelo.
  environment(f) <- baseenv()
  extras <- list()
  ajuste <- .tr_ml_with_seed(seed, switch(modelo,
    linear = if (d$tarefa == "regressao") stats::lm(f, treino) else stats::glm(f, treino, family = stats::binomial()),
    cart = {
      .tr_ml_require("rpart", modelo)
      # `min_n` is documented as the minimum number of observations in a
      # leaf.  `minsplit` only decides whether a node is considered for a
      # split; cap its derived value before rpart coerces it to integer.
      minsplit <- min(as.double(.Machine$integer.max), 2 * as.double(min_n))
      arvore <- rpart::rpart(f, treino, method = if (d$tarefa == "regressao") "anova" else "class",
                   control = rpart::rpart.control(maxdepth = max_depth,
                                                  minbucket = min_n,
                                                  minsplit = minsplit, cp = cp,
                                                  xval = if (poda == "nenhuma") 0L else min(10L, nrow(treino))))
      extras$poda <- .tr_ml_poda_cart(arvore, poda)
      extras$poda$folds <- if (poda == "nenhuma") 0L else min(10L, nrow(treino))
      if (!is.null(extras$poda$cp)) arvore <- rpart::prune(arvore, cp = extras$poda$cp)
      arvore
    },
    figs = {
      .tr_ml_require("figsr", modelo)
      figsr::figs(f, treino, max_splits = max_splits, min_n = min_n,
                  mode = if (d$tarefa == "regressao") "regression" else "classification")
    },
    forest = {
      .tr_ml_require("ranger", modelo)
      mm <- if (mtry == 0L) max(1L, floor(sqrt(length(d$preditores)))) else mtry
      if (mm > length(d$preditores)) .tr_ml_abort("tr_ml_error_bad_param", "Param 'mtry' n\u{E3}o pode superar o n\u{FA}mero de preditores.")
      extras$mtry <- mm
      ranger::ranger(f, treino, num.trees = trees, mtry = mm,
                     min.node.size = min_n, max.depth = max_depth,
                     probability = d$tarefa == "classificacao", seed = seed,
                     importance = c(impureza = "impurity", permutacao = "permutation",
                                    impureza_corrigida = "impurity_corrected")[[importancia]])
    },
    svm = {
      .tr_ml_require("e1071", modelo)
      e1071::svm(x = d$x, y = d$y, type = if (d$tarefa == "regressao") "eps-regression" else "C-classification",
                 kernel = kernel, cost = cost, gamma = gamma, probability = d$tarefa == "classificacao")
    },
    xgboost = {
      .tr_ml_require("xgboost", modelo)
      X <- data.matrix(d$x)
      if (d$tarefa == "regressao") {
        objective <- "reg:squarederror"; label <- d$y
      } else if (length(d$niveis) == 2L) {
        objective <- "binary:logistic"; label <- as.integer(d$y) - 1L
      } else {
        objective <- "multi:softprob"; label <- as.integer(d$y) - 1L
      }
      params <- list(objective = objective, max_depth = max_depth, eta = eta, nthread = 1L)
      if (length(d$niveis) > 2L) params$num_class <- length(d$niveis)
      xgboost::xgb.train(params = params, data = xgboost::xgb.DMatrix(X, label = label), nrounds = nrounds, verbose = 0)
    }
  ))
  engine <- switch(modelo, linear = "stats", cart = "rpart", figs = "figsr",
                   forest = "ranger", svm = "e1071", xgboost = "xgboost")
  extras$engine <- engine
  extras$engine_version <- as.character(utils::packageVersion(engine))
  extras$parametros <- list(max_depth = max_depth, min_n = min_n,
                            max_splits = max_splits, trees = trees,
                            mtry = extras$mtry %||% mtry, cost = cost,
                            gamma = gamma, kernel = kernel,
                            nrounds = nrounds, eta = eta, cp = cp,
                            poda = poda, corte = corte, importancia = importancia)
  structure(list(ajuste = ajuste, modelo = modelo, tarefa = d$tarefa, alvo = d$alvo,
                 preditores = d$preditores, internos = d$internos, niveis = d$niveis,
                 n = d$n, seed = seed, extras = extras,
                 origem = .tr_ml_origem_curta(dados),
                 treino_impressoes = .tr_ml_impressoes_treino(dados, d$impressoes)), class = "tr_ml_fit")
}

# Custo-complexidade (Breiman et al. 1984, sec. 3.4.3): na sequência aninhada
# de subárvores do `cptable`, a regra 1-EP fica com a menor cujo `xerror` não
# passa de min(xerror) + xstd do mínimo. Podar com o CP da linha escolhida
# devolve exatamente essa subárvore (`prune.rpart` corta nós com
# complexidade <= cp).
.tr_ml_poda_cart <- function(arvore, poda) {
  tab <- arvore$cptable
  if (poda == "nenhuma" || nrow(tab) < 2L || !"xerror" %in% colnames(tab) ||
      all(is.na(tab[, "xerror"]))) {
    return(list(metodo = poda, cp = NULL, divisoes = unname(tab[nrow(tab), "nsplit"]), cptable = tab))
  }
  i_min <- which.min(tab[, "xerror"])
  limite <- tab[i_min, "xerror"] + if (poda == "1ep") tab[i_min, "xstd"] else 0
  i <- which(tab[, "xerror"] <= limite)[[1L]]
  list(metodo = poda, cp = unname(tab[i, "CP"]), divisoes = unname(tab[i, "nsplit"]), cptable = tab)
}

#' Prever com um modelo de aprendizado de máquina
#' @param modelo Objeto criado por [tr_ml_fit()].
#' @param dados Nova tabela, preservada integralmente na saída.
#' @return Tibble com os dados originais, `.pred` e, quando disponíveis,
#'   probabilidades `.prob_<classe>`. A marca de treino/teste do `ml/split`
#'   (atributo `tr_ml_origem`) passa adiante; prever o teste de outra divisão
#'   com um modelo ajustado no treino de uma divisão marcada é recusado
#'   (`tr_ml_error_split_mismatch`), e prever linhas do teste que o modelo viu
#'   no ajuste (pelas impressões digitais das linhas) também
#'   (`tr_ml_error_test_leak`).
#' @export
tr_ml_predict <- function(modelo, dados) {
  x <- .tr_ml_novos_dados(modelo, dados)
  o_dados <- .tr_ml_origem(dados); o_modelo <- modelo$origem
  if (identical(o_dados$papel, "teste") && !is.null(o_modelo$divisao) &&
      !identical(o_dados$divisao, o_modelo$divisao)) {
    .tr_ml_abort("tr_ml_error_split_mismatch", paste(
      "O modelo foi ajustado no treino de uma divis\u{E3}o e estas linhas s\u{E3}o o teste de outra:",
      "parte delas pode ter estado no treino. Use o teste do mesmo `ml/split` do modelo."))
  }
  vistas <- .tr_ml_teste_no_treino(modelo, dados)
  if (vistas > 0L) {
    .tr_ml_abort("tr_ml_error_test_leak", paste(
      "O modelo foi ajustado com %d linha(s) deste teste (ajuste antes de dividir, ou numa",
      "tabela que cont\u{E9}m o teste): a previs\u{E3}o n\u{E3}o mede generaliza\u{E7}\u{E3}o. Ajuste s\u{F3}",
      "na sa\u{ED}da treino do `ml/split`."), vistas)
  }
  cls <- modelo$tarefa == "classificacao"; prob <- NULL
  if (modelo$modelo == "linear") {
    if (cls) { p <- as.numeric(stats::predict(modelo$ajuste, x, type = "response")); prob <- cbind(1-p, p)
      pred <- modelo$niveis[1L + (p >= (modelo$extras$parametros$corte %||% .5))] }
    else pred <- as.numeric(stats::predict(modelo$ajuste, x))
  } else if (modelo$modelo == "cart") {
    if (cls) { prob <- stats::predict(modelo$ajuste, x, type = "prob"); pred <- colnames(prob)[max.col(prob, ties.method = "first")] }
    else pred <- as.numeric(stats::predict(modelo$ajuste, x))
  } else if (modelo$modelo == "figs") {
    if (cls) {
      z <- stats::predict(modelo$ajuste, new_data = x, type = "prob")
      prob <- as.matrix(z)
      colnames(prob) <- sub("^\\.pred_", "", colnames(prob))
      pred <- modelo$niveis[max.col(prob[, modelo$niveis, drop = FALSE], ties.method = "first")]
    }
    else pred <- as.numeric(stats::predict(modelo$ajuste, new_data = x)$.pred)
  } else if (modelo$modelo == "forest") {
    z <- stats::predict(modelo$ajuste, data = x)$predictions
    if (cls) { prob <- as.matrix(z); pred <- colnames(prob)[max.col(prob, ties.method = "first")] } else pred <- as.numeric(z)
  } else if (modelo$modelo == "svm") {
    z <- stats::predict(modelo$ajuste, x, probability = cls)
    if (cls) {
      # Com `probability = TRUE` o LIBSVM já rotula pela maior probabilidade de
      # Platt (svm_predict_probability), não pela margem; fixamos a regra aqui
      # para que `.pred` e `.prob_*` nunca discordem, com empates resolvidos
      # na ordem dos níveis como nos outros modelos.
      prob <- attr(z, "probabilities")[, modelo$niveis, drop = FALSE]
      pred <- modelo$niveis[max.col(prob, ties.method = "first")]
    } else pred <- as.numeric(z)
  } else {
    z <- stats::predict(modelo$ajuste, data.matrix(x))
    if (!cls) pred <- as.numeric(z) else if (length(modelo$niveis) == 2L) {
      z <- as.numeric(z); prob <- cbind(1-z, z); pred <- modelo$niveis[1L + (z >= .5)]
    } else {
      k <- length(modelo$niveis)
      prob <- if (is.matrix(z)) z else
        matrix(as.numeric(z), nrow = nrow(x), ncol = k, byrow = TRUE)
      if (!identical(dim(prob), c(nrow(x), k))) {
        .tr_ml_abort("tr_ml_error_bad_prediction",
                     "O engine XGBoost devolveu probabilidades com dimens\u{F5}es inesperadas.")
      }
      pred <- modelo$niveis[max.col(prob, ties.method = "first")]
    }
  }
  out <- tibble::as_tibble(dados); out$.pred <- if (cls) factor(pred, levels = modelo$niveis) else as.numeric(pred)
  if (!is.null(prob)) {
    prob <- as.matrix(prob)
    if (is.null(colnames(prob)) || !all(modelo$niveis %in% colnames(prob))) colnames(prob) <- modelo$niveis
    prob <- prob[, modelo$niveis, drop = FALSE]
    for (j in seq_along(modelo$niveis)) out[[paste0(".prob_", modelo$niveis[[j]])]] <- as.numeric(prob[, j])
  }
  attr(out, .tr_ml_origem_attr) <- o_dados
  out
}

#' Regras legíveis de CART ou FIGS
#' @param modelo Objeto criado por [tr_ml_fit()].
#' @return Tibble com as regras exibidas pelo engine.
#' @export
tr_ml_rules <- function(modelo) {
  if (!inherits(modelo, "tr_ml_fit")) .tr_ml_abort("tr_ml_error_not_fit", "Param 'modelo' n\u{E3}o \u{E9} um ajuste de machine learning.")
  if (modelo$modelo == "cart") {
    fr <- modelo$ajuste$frame
    nos <- as.integer(row.names(fr))
    folhas <- nos[fr$var == "<leaf>"]
    # `path.rpart()` formata cortes segundo `options("digits")`. Lemos a
    # matriz de splits para não arredondar a regra que o modelo executa.
    primarios <- list(); pos <- 1L
    for (i in which(fr$var != "<leaf>")) {
      s <- modelo$ajuste$splits[pos, , drop = FALSE]
      primarios[[as.character(nos[[i]])]] <- list(
        var = modelo$preditores[match(row.names(modelo$ajuste$splits)[[pos]], modelo$internos)],
        corte = unname(s[1L, "index"]),
        esquerda_menor = unname(s[1L, "ncat"]) < 0)
      pos <- pos + 1L + fr$ncompete[[i]] + fr$nsurrogate[[i]]
    }
    regra_folha <- function(no) {
      partes <- character()
      atual <- no
      while (atual > 1L) {
        pai <- atual %/% 2L; sp <- primarios[[as.character(pai)]]
        esquerda <- atual %% 2L == 0L
        menor <- identical(esquerda, sp$esquerda_menor)
        partes <- c(sprintf("%s %s %s", sp$var, if (menor) "<" else ">=",
                            format(sp$corte, digits = 17L, scientific = FALSE, trim = TRUE)), partes)
        atual <- pai
      }
      paste(partes, collapse = " e ")
    }
    regras <- vapply(folhas, regra_folha, "")
    valor <- fr$yval[match(folhas, nos)]
    if (modelo$tarefa == "classificacao") valor <- factor(modelo$niveis[valor], levels = modelo$niveis)
    out <- tibble::tibble(arvore = 1L, regra = regras, valor = valor, no = folhas)
    attr(out, "nota") <- "Cada linha \u{E9} uma folha; 'valor' \u{E9} a previs\u{E3}o do CART nessa folha."
    return(out)
  }
  if (modelo$modelo == "figs") {
    linhas <- lapply(seq_along(modelo$ajuste$trees), function(k) {
      tr <- modelo$ajuste$trees[[k]]
      folhas <- tr[vapply(tr, `[[`, logical(1), "is_leaf")]
      regra <- vapply(folhas, function(folha) {
        partes <- character(); atual <- folha$id
        repeat {
          ip <- which(vapply(tr, function(no) identical(no$left_child, atual) || identical(no$right_child, atual), TRUE))
          if (!length(ip)) break
          pai <- tr[[ip[[1L]]]]
          esquerda <- identical(pai$left_child, atual)
          op <- if (esquerda) "<=" else ">"
          variavel <- modelo$preditores[match(pai$feature, modelo$internos)]
          partes <- c(sprintf("%s %s %s", variavel, op,
                              format(pai$split_val, digits = 17L, scientific = FALSE, trim = TRUE)), partes)
          atual <- pai$id
        }
        paste(partes, collapse = " e ")
      }, "")
      tibble::tibble(arvore = k, regra = regra,
                     valor = vapply(folhas, `[[`, numeric(1), "value"))
    })
    out <- if (length(linhas)) do.call(rbind, linhas) else
      tibble::tibble(arvore = 0L, regra = "", valor = as.numeric(modelo$ajuste$intercept %||% 0))
    attr(out, "nota") <- if (modelo$tarefa == "classificacao") {
      sprintf("Some um valor por \u{E1}rvore para obter P(%s); a previs\u{E3}o limita a soma ao intervalo [0, 1].", modelo$niveis[[2L]])
    } else {
      "A previs\u{E3}o \u{E9} a soma de um valor por \u{E1}rvore."
    }
    return(out)
  }
  .tr_ml_abort("tr_ml_error_not_applicable", "Regras expl\u{ED}citas est\u{E3}o dispon\u{ED}veis apenas para CART e FIGS.")
}

#' Importância das variáveis nos modelos baseados em árvores
#' @param modelo Objeto criado por [tr_ml_fit()].
#' @return Tibble `variavel`, `importancia` e `medida` (o que o número mede):
#'   redução de impureza no CART (com divisões substitutas) e no FIGS (em % do
#'   total); no XGBoost, o Gain relativo (fração do ganho total, soma 1); na
#'   floresta, a escolhida em `importancia` no ajuste — com `"permutacao"`, o
#'   aumento do erro fora da bolsa ao permutar o preditor: erro quadrático
#'   médio na regressão e, na classificação (floresta de probabilidade), o erro
#'   de Brier do `ranger`, média de (1 - p da classe observada)^2, e não queda
#'   de acurácia. O código curto da medida fica no atributo `medida`.
#' @export
tr_ml_importance <- function(modelo) {
  if (!inherits(modelo, "tr_ml_fit")) .tr_ml_abort("tr_ml_error_not_fit", "Param 'modelo' n\u{E3}o \u{E9} um ajuste de machine learning.")
  imp <- switch(modelo$modelo,
    cart = modelo$ajuste$variable.importance,
    figs = { z <- figsr::figsr_importance(modelo$ajuste); stats::setNames(z[[ncol(z)]], z[[1L]]) },
    forest = modelo$ajuste$variable.importance,
    xgboost = { z <- xgboost::xgb.importance(model = modelo$ajuste); stats::setNames(z$Gain, z$Feature) },
    .tr_ml_abort("tr_ml_error_not_applicable", "Import\u{E2}ncia est\u{E1} dispon\u{ED}vel para CART, FIGS, forest e XGBoost."))
  if (is.null(imp)) imp <- numeric()
  nomes <- names(imp) %||% character()
  mapa <- match(nomes, modelo$internos)
  nomes[!is.na(mapa)] <- modelo$preditores[mapa[!is.na(mapa)]]
  codigo <- if (modelo$modelo == "forest") modelo$extras$parametros$importancia %||% "impureza" else "impureza"
  out <- tibble::tibble(variavel = nomes, importancia = as.numeric(imp),
                        medida = rep(.tr_ml_medida_importancia(modelo, codigo), length(nomes)))
  out <- out[order(-out$importancia), , drop = FALSE]
  attr(out, "medida") <- codigo
  out
}

# O que o número de cada engine mede, em palavras. Na permutação do `ranger`,
# o erro fora da bolsa é o EQM na regressão e, na floresta de probabilidade
# (a nossa classificação), a média de (1 - p da classe observada)^2, uma forma
# do erro de Brier; a importância é o aumento desse erro, não queda de acurácia.
.tr_ml_medida_importancia <- function(modelo, codigo) {
  cls <- modelo$tarefa == "classificacao"
  switch(modelo$modelo,
    cart = "redu\u{E7}\u{E3}o de impureza somada nos cortes, inclusive divis\u{F5}es substitutas (rpart)",
    figs = "ganho de impureza somado nos cortes do FIGS, em % do total (figsr)",
    xgboost = "Gain: fra\u{E7}\u{E3}o do ganho total das divis\u{F5}es que usam o preditor (xgboost)",
    forest = switch(codigo,
      impureza = if (cls) "redu\u{E7}\u{E3}o de impureza de Gini somada nas \u{E1}rvores (ranger)" else
        "redu\u{E7}\u{E3}o de vari\u{E2}ncia somada nas \u{E1}rvores (ranger)",
      impureza_corrigida = "impureza corrigida AIR (Nembrini et al. 2018; ranger)",
      permutacao = if (cls) paste("permuta\u{E7}\u{E3}o: aumento do erro de Brier fora da bolsa,",
                                  "m\u{E9}dia de (1 - p da classe observada)^2 (ranger)") else
        "permuta\u{E7}\u{E3}o: aumento do erro quadr\u{E1}tico m\u{E9}dio fora da bolsa (ranger)"))
}
