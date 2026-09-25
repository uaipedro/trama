test_that("linear ajusta regressão e preserva os dados na previsão", {
  m <- tr_ml_fit(mtcars, "mpg", "wt, hp", modelo = "linear")
  expect_s3_class(m, "tr_ml_fit")
  expect_equal(m$preditores, c("wt", "hp"))
  p <- tr_ml_predict(m, mtcars[1:4, ])
  expect_s3_class(p, "tbl_df")
  expect_equal(p$.pred, unname(predict(lm(mpg ~ wt + hp, mtcars), mtcars[1:4, ])))
  expect_equal(names(p)[seq_along(names(mtcars))], names(mtcars))
})

test_that("nomes arbitrários são isolados dos engines", {
  d <- data.frame(check.names = FALSE,
                  "produção total" = c(2, 3, 5, 7, 8, 9),
                  "chuva (mm)" = c(1, 2, 3, 4, 5, 6))
  m <- tr_ml_fit(d, "produção total", "chuva (mm)", modelo = "cart", min_n = 2)
  expect_equal(m$internos, "x1")
  expect_equal(nrow(tr_ml_predict(m, d)), nrow(d))
})

test_that("classificação devolve classe e probabilidades", {
  d <- data.frame(x = seq_len(20), z = rep(c(0, 1), 10),
                  classe = factor(rep(c("nao", "sim", "sim", "nao"), 5)))
  d$z[c(3, 14)] <- 1 - d$z[c(3, 14)]
  m <- tr_ml_fit(d, "classe", "x, z", modelo = "linear")
  p <- tr_ml_predict(m, d[1:5, ])
  expect_true(is.factor(p$.pred))
  expect_equal(levels(p$.pred), levels(d$classe))
  expect_true(all(c(".prob_nao", ".prob_sim") %in% names(p)))
  expect_equal(p$.prob_nao + p$.prob_sim, rep(1, 5), tolerance = 1e-10)
})

test_that("tarefa explícita permite classe numérica", {
  d <- mtcars
  m <- tr_ml_fit(d, "am", "wt, hp", modelo = "cart", tarefa = "classificacao")
  expect_identical(m$tarefa, "classificacao")
  expect_equal(levels(tr_ml_predict(m, d)$.pred), c("0", "1"))
})

test_that("tarefa explícita também permite regressão de alvo numérico discreto", {
  d <- data.frame(x = 1:12, y = rep(0:2, each = 4))
  m <- tr_ml_fit(d, "y", "x", modelo = "linear", tarefa = "regressao")
  expect_identical(m$tarefa, "regressao")
  expect_type(tr_ml_predict(m, d[1, , drop = FALSE])$.pred, "double")
})

test_that("CART expõe regras e importância com nomes originais", {
  m <- tr_ml_fit(iris, "Species", "Sepal.Length, Sepal.Width", modelo = "cart", min_n = 2)
  r <- tr_ml_rules(m)
  expect_s3_class(r, "tbl_df")
  expect_equal(nrow(r), sum(m$ajuste$frame$var == "<leaf>"))
  expect_true(any(grepl("Sepal", r$regra, fixed = TRUE)))
  expect_true("valor" %in% names(r))
  i <- tr_ml_importance(m)
  expect_named(i, c("variavel", "importancia"))
  expect_true(all(i$variavel %in% m$preditores))
  expect_true(all(diff(i$importancia) <= 0))
})

test_that("FIGS expõe folhas exatas e explica a soma quando disponível", {
  skip_if_not_installed("figsr")
  d <- droplevels(subset(iris, Species != "virginica"))
  m <- tr_ml_fit(d, "Species", "Sepal.Length, Sepal.Width", modelo = "figs", max_splits = 3)
  r <- tr_ml_rules(m)
  expect_named(r, c("arvore", "regra", "valor"))
  expect_equal(nrow(r), sum(vapply(m$ajuste$trees[[1]], `[[`, logical(1), "is_leaf")))
  expect_match(attr(r, "nota"), "Some um valor")
  expect_true(all(c("engine", "engine_version", "parametros") %in% names(m$extras)))
})

test_that("validação recusa vazamento, colunas ruins e dados inválidos", {
  expect_error(tr_ml_fit(mtcars, "mpg", "mpg, wt"), class = "tr_ml_error_target_leakage")
  expect_error(tr_ml_fit(mtcars, "mpg", "peso"), class = "tr_ml_error_unknown_column")
  expect_error(tr_ml_fit(iris, "Species", "Species"), class = "tr_ml_error_target_leakage")
  d <- mtcars; d$wt[1] <- NA
  expect_error(tr_ml_fit(d, "mpg", "wt"), class = "tr_ml_error_missing")
  d <- mtcars; d$wt[1] <- Inf
  expect_error(tr_ml_fit(d, "mpg", "wt"), class = "tr_ml_error_nonfinite")
})

test_that("previsão não sobrescreve colunas reservadas", {
  m <- tr_ml_fit(mtcars, "mpg", "wt", modelo = "linear")
  d <- mtcars; d$.pred <- 0
  expect_error(tr_ml_predict(m, d), class = "tr_ml_error_output_collision")
})

test_that("ajuste preserva exatamente o RNG da sessão", {
  set.seed(831)
  antes <- .Random.seed
  invisible(tr_ml_fit(mtcars, "mpg", "wt", modelo = "cart"))
  expect_identical(.Random.seed, antes)
})

test_that("SVM funciona quando o engine está disponível", {
  skip_if_not_installed("e1071")
  d <- subset(iris, Species != "virginica")
  m <- tr_ml_fit(d, "Species", "Sepal.Length, Sepal.Width", modelo = "svm")
  p <- tr_ml_predict(m, d[1:6, ])
  expect_true(all(c(".pred", ".prob_setosa", ".prob_versicolor") %in% names(p)))
})

test_that("engines opcionais dão instrução acionável quando ausentes", {
  expect_error(.tr_ml_require("tramaMLengineAbsent", "exemplo"),
               "install.packages", class = "tr_ml_error_missing_engine")
})

engine_disponivel <- function(engine) {
  pkg <- c(linear = "stats", cart = "rpart", figs = "figsr", forest = "ranger",
           svm = "e1071", xgboost = "xgboost")[[engine]]
  requireNamespace(pkg, quietly = TRUE)
}

test_that("todos os engines disponíveis ajustam regressão e preveem uma linha", {
  d <- data.frame(check.names = FALSE,
                  "resposta contínua" = mtcars$mpg,
                  "peso (mil lb)" = mtcars$wt,
                  "potência bruta" = mtcars$hp)
  for (engine in c("linear", "cart", "figs", "forest", "svm", "xgboost")) {
    if (!engine_disponivel(engine)) next
    m <- tr_ml_fit(d, "resposta contínua", "peso (mil lb), potência bruta",
                   modelo = engine, trees = 20, nrounds = 5, max_splits = 3)
    p <- tr_ml_predict(m, d[1, , drop = FALSE])
    expect_equal(nrow(p), 1L, info = engine)
    expect_true(is.numeric(p$.pred) && is.finite(p$.pred), info = engine)
    expect_equal(m$preditores, c("peso (mil lb)", "potência bruta"), info = engine)
  }
})

test_that("todos os engines disponíveis ajustam classificação binária e uma linha", {
  d <- data.frame(check.names = FALSE,
                  "grupo final" = factor(ifelse(mtcars$am == 1, "manual", "automático")),
                  "peso (mil lb)" = mtcars$wt,
                  "potência bruta" = mtcars$hp)
  for (engine in c("linear", "cart", "figs", "forest", "svm", "xgboost")) {
    if (!engine_disponivel(engine)) next
    m <- suppressWarnings(tr_ml_fit(d, "grupo final", "peso (mil lb), potência bruta",
                                    modelo = engine, trees = 20, nrounds = 5,
                                    max_splits = 3))
    p <- tr_ml_predict(m, d[1, , drop = FALSE])
    expect_equal(nrow(p), 1L, info = engine)
    expect_true(is.factor(p$.pred), info = engine)
    probs <- p[paste0(".prob_", m$niveis)]
    expect_equal(sum(unlist(probs)), 1, tolerance = 1e-6, info = engine)
  }
})

test_that("engines compatíveis cobrem classificação multiclasse", {
  for (engine in c("cart", "forest", "svm", "xgboost")) {
    if (!engine_disponivel(engine)) next
    m <- tr_ml_fit(iris, "Species", "Sepal.Length, Sepal.Width",
                   modelo = engine, trees = 20, nrounds = 5)
    p <- tr_ml_predict(m, iris[1, , drop = FALSE])
    expect_equal(levels(p$.pred), levels(iris$Species), info = engine)
    probs <- unlist(p[paste0(".prob_", levels(iris$Species))], use.names = FALSE)
    expect_equal(sum(probs), 1, tolerance = 1e-6, info = engine)
  }
  expect_error(tr_ml_fit(iris, "Species", "Sepal.Length", modelo = "linear"),
               class = "tr_ml_error_binary_only")
  if (engine_disponivel("figs")) {
    expect_error(tr_ml_fit(iris, "Species", "Sepal.Length", modelo = "figs"),
                 class = "tr_ml_error_binary_only")
  }
})

test_that("CART e FIGS descrevem corretamente os dois lados do corte", {
  d <- data.frame(x = 1:12, y = c(rep(0, 6), rep(10, 6)))
  cart <- tr_ml_fit(d, "y", "x", modelo = "cart", min_n = 2, max_depth = 1)
  corte_cart <- unname(cart$ajuste$splits[1, "index"])
  pc <- tr_ml_predict(cart, data.frame(x = c(corte_cart - 1e-8, corte_cart, corte_cart + 1e-8)))$.pred
  rc <- tr_ml_rules(cart)
  expect_match(rc$regra[[1]], "x <")
  expect_match(rc$regra[[2]], "x >=")
  expect_equal(pc, c(rc$valor[[1]], rc$valor[[2]], rc$valor[[2]]))

  if (engine_disponivel("figs")) {
    figs <- tr_ml_fit(d, "y", "x", modelo = "figs", min_n = 2, max_splits = 1)
    raiz <- figs$ajuste$trees[[1]][[1]]
    corte_figs <- raiz$split_val
    pf <- tr_ml_predict(figs, data.frame(x = c(corte_figs, corte_figs + 1e-8)))$.pred
    rf <- tr_ml_rules(figs)
    expect_true(any(grepl("x <=", rf$regra, fixed = TRUE)))
    expect_true(any(grepl("x >", rf$regra, fixed = TRUE)))
    esquerda <- rf$valor[grepl("x <=", rf$regra, fixed = TRUE)]
    direita <- rf$valor[grepl("x >", rf$regra, fixed = TRUE)]
    expect_equal(pf, c(esquerda, direita))
  }
})

test_that("regras de CART e FIGS recompõem as previsões", {
  d <- data.frame(x = 1:24, z = rep(c(0, 1), 12))
  d$y <- ifelse(d$x <= 8, 2, ifelse(d$x <= 16, 7, 12)) + 3 * d$z

  satisfaz <- function(regra, linha) {
    if (!nzchar(regra)) return(TRUE)
    expr <- gsub(" e ", " & ", regra, fixed = TRUE)
    isTRUE(eval(parse(text = expr), envir = list2env(as.list(linha))))
  }
  valor_regras <- function(regras, linha, soma = FALSE) {
    escolhidas <- vapply(regras$regra, satisfaz, logical(1), linha = linha)
    if (soma) sum(regras$valor[escolhidas]) else regras$valor[escolhidas][[1L]]
  }

  cart <- tr_ml_fit(d, "y", "x, z", modelo = "cart", min_n = 2, max_depth = 3)
  rc <- tr_ml_rules(cart)
  pelas_regras <- vapply(seq_len(nrow(d)), function(i)
    valor_regras(rc, d[i, , drop = FALSE]), numeric(1))
  expect_equal(pelas_regras, tr_ml_predict(cart, d)$.pred)

  if (engine_disponivel("figs")) {
    figs <- tr_ml_fit(d, "y", "x, z", modelo = "figs", min_n = 2, max_splits = 5)
    rf <- tr_ml_rules(figs)
    pelas_regras <- vapply(seq_len(nrow(d)), function(i)
      valor_regras(rf, d[i, , drop = FALSE], soma = TRUE), numeric(1))
    expect_equal(pelas_regras, tr_ml_predict(figs, d)$.pred)
  }
})

test_that("CART sem divisão ainda produz regra e previsão da raiz", {
  d <- data.frame(x = 1:8, y = rep(3, 8))
  m <- tr_ml_fit(d, "y", "x", modelo = "cart", min_n = 20)
  r <- tr_ml_rules(m)
  expect_equal(nrow(r), 1L)
  expect_identical(r$regra, "")
  expect_equal(r$valor, 3)
  expect_equal(tr_ml_predict(m, d[1, , drop = FALSE])$.pred, 3)
})

test_that("FIGS sem divisão expõe sua constante como regra", {
  skip_if_not_installed("figsr")
  d <- data.frame(x = 1:8, y = rep(3, 8))
  m <- tr_ml_fit(d, "y", "x", modelo = "figs", min_n = 8, max_splits = 2)
  r <- tr_ml_rules(m)
  expect_equal(nrow(r), 1L)
  expect_identical(r$arvore, 0L)
  expect_identical(r$regra, "")
  expect_equal(r$valor, 3)
  expect_equal(tr_ml_predict(m, d[1, , drop = FALSE])$.pred, r$valor)
})

test_that("regras usam nomes originais por correspondência exata", {
  d <- data.frame(check.names = FALSE,
                  resposta = c(rep(0, 6), rep(10, 6)),
                  x2 = 1:12,
                  x1 = rep(c(0, 1), 6))
  m <- tr_ml_fit(d, "resposta", "x2, x1", modelo = "cart", min_n = 2)
  r <- tr_ml_rules(m)
  expect_true(any(grepl("x2", r$regra, fixed = TRUE)))
  expect_false(any(grepl("x1[0-9]", r$regra)))
  expect_true(all(tr_ml_importance(m)$variavel %in% c("x2", "x1")))
})

test_that("min_n do CART limita folhas e aceita o maior inteiro sem overflow", {
  d <- data.frame(x = 1:12, y = c(rep(0, 6), rep(10, 6)))
  m <- tr_ml_fit(d, "y", "x", modelo = "cart", min_n = 4)
  folhas <- m$ajuste$frame[m$ajuste$frame$var == "<leaf>", , drop = FALSE]
  expect_true(all(folhas$n >= 4))
  expect_no_warning(m2 <- tr_ml_fit(d, "y", "x", modelo = "cart",
                                    min_n = .Machine$integer.max))
  expect_equal(nrow(tr_ml_rules(m2)), 1L)
})

test_that("forest encaminha min_n ao tamanho mínimo dos nós", {
  skip_if_not_installed("ranger")
  m <- tr_ml_fit(mtcars, "mpg", "wt, hp", modelo = "forest",
                 min_n = 7, trees = 5)
  expect_equal(m$ajuste$min.node.size, 7L)
  expect_equal(m$extras$parametros$min_n, 7L)
})
test_that("XGBoost preserva a correspondencia linha-classe em multiclasse", {
  skip_if_not_installed("xgboost")
  m <- tr_ml_fit(iris, "Species", modelo = "xgboost", nrounds = 10L)
  novos <- iris[c(1L, 51L, 101L, 2L, 52L, 102L), ]
  x <- as.data.frame(novos[m$preditores])
  names(x) <- m$internos
  esperado <- stats::predict(m$ajuste, data.matrix(x))
  obtido <- tr_ml_predict(m, novos)
  expect_equal(unname(as.matrix(obtido[paste0(".prob_", m$niveis)])),
               unname(esperado), tolerance = 1e-7)
  expect_equal(as.character(obtido$.pred), m$niveis[max.col(esperado, ties.method = "first")])
})

test_that("CART poda por custo-complexidade com a regra 1-EP (oráculo rpart)", {
  skip_if_not_installed("rpart")
  # Breiman et al. (1984, sec. 3.4.3): a menor árvore cujo erro de validação
  # cruzada não passa do mínimo + 1 erro-padrão. Reproduz printcp/prune do
  # rpart com a mesma semente e os mesmos 10 folds.
  d <- datasets::airquality[stats::complete.cases(datasets::airquality), ]
  cols <- "Solar.R, Wind, Temp, Month, Day"
  m <- tr_ml_cart(d, "Ozone", cols, max_depth = 30, min_n = 3, seed = 42)
  oraculo <- trama.ml:::.tr_ml_with_seed(42L, rpart::rpart(
    Ozone ~ Solar.R + Wind + Temp + Month + Day, d, method = "anova",
    control = rpart::rpart.control(maxdepth = 30, minbucket = 3, minsplit = 6, cp = 0, xval = 10)))
  tab <- oraculo$cptable
  expect_equal(unname(m$extras$poda$cptable), unname(tab), tolerance = 1e-12)
  i_min <- which.min(tab[, "xerror"])
  i_1ep <- which(tab[, "xerror"] <= tab[i_min, "xerror"] + tab[i_min, "xstd"])[[1]]
  # Valores da semente 42 (printcp): árvore cheia com 30 divisões, mínimo do
  # xerror em outra linha, 1-EP com 3 divisões.
  expect_equal(unname(tab[nrow(tab), "nsplit"]), 30)
  expect_equal(unname(tab[i_1ep, "nsplit"]), 3)
  expect_lt(i_1ep, i_min)
  podada <- rpart::prune(oraculo, cp = tab[i_1ep, "CP"])
  expect_equal(m$extras$poda$cp, unname(tab[i_1ep, "CP"]))
  expect_equal(m$extras$poda$divisoes, 3)
  expect_equal(sum(m$ajuste$frame$var != "<leaf>"), 3L)
  expect_equal(tr_ml_predict(m, d)$.pred, unname(stats::predict(podada, d)))
  # A regra do mínimo escolhe a árvore de menor xerror; sem poda, a árvore cheia.
  mm <- tr_ml_cart(d, "Ozone", cols, max_depth = 30, min_n = 3, seed = 42, poda = "minimo")
  expect_equal(mm$extras$poda$divisoes, unname(tab[i_min, "nsplit"]))
  mn <- tr_ml_cart(d, "Ozone", cols, max_depth = 30, min_n = 3, seed = 42, poda = "nenhuma")
  expect_equal(sum(mn$ajuste$frame$var != "<leaf>"), 30L)
  # Regras continuam recompondo a árvore podada.
  expect_equal(nrow(tr_ml_rules(m)), 4L)
})

test_that("CART: cp cresce a árvore mínima e parâmetros inválidos são recusados", {
  skip_if_not_installed("rpart")
  m <- tr_ml_cart(mtcars, "mpg", "wt, hp", cp = 0.5, poda = "nenhuma")
  expect_equal(sum(m$ajuste$frame$var != "<leaf>"), 1L)
  expect_error(tr_ml_cart(mtcars, "mpg", "wt", cp = -1), class = "tr_ml_error_bad_param")
  expect_error(tr_ml_cart(mtcars, "mpg", "wt", poda = "tudo"), class = "tr_ml_error_bad_option")
})

test_that("logística classifica pelo corte informado sobre P(segunda classe)", {
  d <- tr_ml_example("iris_binaria")
  ref <- stats::glm(Species ~ Sepal.Length + Sepal.Width, d, family = stats::binomial())
  p <- unname(stats::fitted(ref))
  for (corte in c(.5, .3, .8)) {
    m <- tr_ml_linear(d, "Species", "Sepal.Length, Sepal.Width", corte = corte)
    prev <- tr_ml_predict(m, d)
    expect_equal(prev$.prob_virginica, p, tolerance = 1e-10)
    esperado <- factor(levels(d$Species)[1L + (p >= corte)], levels = levels(d$Species))
    expect_identical(prev$.pred, esperado)
  }
  expect_gt(sum(tr_ml_predict(tr_ml_linear(d, "Species", "Sepal.Length, Sepal.Width", corte = .3), d)$.pred == "virginica"),
            sum(tr_ml_predict(tr_ml_linear(d, "Species", "Sepal.Length, Sepal.Width"), d)$.pred == "virginica"))
  expect_error(tr_ml_linear(d, "Species", corte = 0), class = "tr_ml_error_bad_param")
  expect_error(tr_ml_linear(d, "Species", corte = 1), class = "tr_ml_error_bad_param")
})

test_that("SVM: .pred é a classe de maior probabilidade (coerência com .prob_*)", {
  skip_if_not_installed("e1071")
  argmax <- function(prev, niveis) {
    pr <- as.matrix(prev[paste0(".prob_", niveis)])
    factor(niveis[max.col(pr, ties.method = "first")], levels = niveis)
  }
  d <- droplevels(subset(iris, Species != "setosa"))
  m <- tr_ml_svm(d, "Species", "Sepal.Length, Sepal.Width")
  prev <- tr_ml_predict(m, d)
  # Neste exemplo a margem e a calibração de Platt discordam em algumas linhas.
  margem <- stats::predict(m$ajuste, d[c("Sepal.Length", "Sepal.Width")])
  expect_true(any(as.character(margem) != as.character(argmax(prev, m$niveis))))
  expect_identical(prev$.pred, argmax(prev, m$niveis))
  m3 <- tr_ml_svm(iris, "Species", "Sepal.Length, Sepal.Width")
  prev3 <- tr_ml_predict(m3, iris)
  expect_identical(prev3$.pred, argmax(prev3, m3$niveis))
})

test_that("poda do CART com n < 10 usa min(10, n) folds: deixa-um-fora exato", {
  skip_if_not_installed("rpart")
  # Com n <= 10 os folds viram deixa-um-fora, que não depende de sorteio: o
  # cptable tem de ser igual ao do rpart com xval = 1:n (grupos explícitos).
  for (n in c(3L, 5L, 8L)) {
    d <- mtcars[seq_len(n), c("mpg", "wt", "hp")]
    m <- tr_ml_fit(d, "mpg", "wt, hp", modelo = "cart", min_n = 1)
    expect_equal(m$extras$poda$folds, n)
    ref <- rpart::rpart(mpg ~ wt + hp, d, method = "anova",
                        control = rpart::rpart.control(minbucket = 1, minsplit = 2, cp = 0,
                                                       maxdepth = 3, xval = seq_len(n)))
    expect_equal(unname(m$extras$poda$cptable), unname(ref$cptable), tolerance = 1e-12, info = n)
    expect_true(all(is.finite(tr_ml_predict(m, d)$.pred)))
  }
  expect_equal(tr_ml_fit(mtcars, "mpg", "wt", modelo = "cart")$extras$poda$folds, 10L)
})
