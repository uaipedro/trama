# Isolamento do teste imposto pela proveniência que o `ml/split` grava
# (Kaufman et al. 2012, doi:10.1145/2382577.2382579: vazamento treino-teste).

binaria <- function() tr_ml_example("iris_binaria")

test_that("split marca treino e teste com o mesmo id, reprodutível", {
  s <- tr_ml_split(binaria(), "Species", seed = 3)
  ot <- attr(s$treino, "tr_ml_origem"); oe <- attr(s$teste, "tr_ml_origem")
  expect_equal(ot$papel, "treino"); expect_equal(oe$papel, "teste")
  expect_identical(ot$divisao, oe$divisao)
  expect_identical(attr(tr_ml_split(binaria(), "Species", seed = 3)$teste, "tr_ml_origem"), oe)
  expect_false(identical(attr(tr_ml_split(binaria(), "Species", seed = 4)$teste, "tr_ml_origem")$divisao,
                         oe$divisao))
  for (est in c("temporal", "grupo")) {
    d <- tibble::tibble(t = 1:20, g = rep(letters[1:5], each = 4), y = rnorm(20), x = 1:20)
    z <- tr_ml_split(d, "y", estrategia = est, ordem = "t", grupo = "g")
    expect_equal(attr(z$teste, "tr_ml_origem")$papel, "teste", info = est)
  }
  # os dados continuam os mesmos: só o atributo foi acrescentado
  expect_equal(nrow(s$treino) + nrow(s$teste), 100L)
  expect_equal(names(s$teste), names(binaria()))
})

test_that("ajustar no teste é recusado em todos os blocos de ajuste", {
  s <- tr_ml_split(binaria(), "Species")
  expect_error(tr_ml_cart(s$teste, "Species"), class = "tr_ml_error_test_leak")
  expect_error(tr_ml_linear(s$teste, "Species"), class = "tr_ml_error_test_leak")
  expect_error(tr_ml_fit(s$teste, "Species", modelo = "linear"), class = "tr_ml_error_test_leak")
  expect_error(tr_ml_tune(s$teste, "Species", tentativas = 2, folds = 2), class = "tr_ml_error_test_leak")
  expect_error(tr_ml_nested_cv(s$teste, "Species", tentativas = 2, folds_externos = 2, folds = 2),
               class = "tr_ml_error_test_leak")
  expect_error(tr_ml_split(s$teste, "Species"), class = "tr_ml_error_test_leak")
  # a marca sobrevive a filtro, subconjunto e coluna nova
  f <- s$teste[s$teste$Sepal.Length > 5, ]; f$novo <- 1
  expect_error(tr_ml_cart(f, "Species"), class = "tr_ml_error_test_leak")
  # treino e dividir o treino de novo (validação) continuam permitidos
  expect_s3_class(tr_ml_cart(s$treino, "Species"), "tr_ml_fit")
  v <- tr_ml_split(s$treino, "Species", seed = 9)
  expect_equal(attr(v$treino, "tr_ml_origem")$papel, "treino")
})

test_that("tune com treino marcado mede os folds sem recusa e o modelo herda a origem", {
  s <- tr_ml_split(binaria(), "Species")
  z <- tr_ml_tune(s$treino, "Species", tentativas = 2, folds = 2)
  expect_true(all(z$historico$status == "ok"))
  expect_identical(z$modelo$origem, attr(s$treino, "tr_ml_origem")[c("papel", "divisao")])
  n <- tr_ml_nested_cv(s$treino, "Species", tentativas = 2, folds_externos = 2, folds = 2)
  expect_true(all(is.finite(n$externa)))
})

test_that("avaliar o treino: erro por padrão, nota e aviso com permitir_treino", {
  s <- tr_ml_split(binaria(), "Species")
  m <- tr_ml_cart(s$treino, "Species")
  pt <- tr_ml_predict(m, s$treino)
  expect_equal(attr(pt, "tr_ml_origem")$papel, "treino")
  expect_error(tr_ml_evaluate(pt, "Species"), class = "tr_ml_error_train_eval")
  expect_error(tr_ml_confusion(pt, "Species"), class = "tr_ml_error_train_eval")
  expect_error(tr_ml_roc(pt, "Species", ".prob_virginica"), class = "tr_ml_error_train_eval")
  expect_error(tr_ml_pr_curve(pt, "Species", ".prob_virginica"), class = "tr_ml_error_train_eval")
  expect_error(tr_ml_evaluate(pt, "Species", permitir_treino = NA), class = "tr_ml_error_bad_param")

  expect_warning(e <- tr_ml_evaluate(pt, "Species", permitir_treino = TRUE), "otimista")
  sem <- pt; attr(sem, "tr_ml_origem") <- NULL
  ref <- tr_ml_evaluate(sem, "Species")
  expect_equal(e$valor, ref$valor)                     # mesmo número, só anotado
  expect_true(all(grepl("otimista", e$nota)))
  expect_false("nota" %in% names(ref))
  expect_warning(cf <- tr_ml_confusion(pt, "Species", permitir_treino = TRUE), "otimista")
  expect_equal(cf$n, tr_ml_confusion(sem, "Species")$n)
  expect_warning(r <- tr_ml_roc(pt, "Species", ".prob_virginica", permitir_treino = TRUE), "otimista")
  expect_match(r$labels$caption, "otimista")
  expect_equal(r$data$auc, tr_ml_roc(sem, "Species", ".prob_virginica")$data$auc)
  expect_warning(pr <- tr_ml_pr_curve(pt, "Species", ".prob_virginica", permitir_treino = TRUE), "otimista")
  expect_true(all(grepl("otimista", pr$data$nota)))
})

test_that("avaliar o teste e tabelas sem marca seguem como antes", {
  s <- tr_ml_split(binaria(), "Species")
  m <- tr_ml_cart(s$treino, "Species")
  p <- tr_ml_predict(m, s$teste)
  expect_equal(attr(p, "tr_ml_origem")$papel, "teste")
  expect_no_warning(e <- tr_ml_evaluate(p, "Species"))
  expect_false("nota" %in% names(e))
  expect_no_warning(tr_ml_roc(p, "Species", ".prob_virginica"))
  # divisão feita pelo usuário (sem marca): nada muda, nem no treino
  d <- binaria(); idx <- seq(1, 100, 2)
  m2 <- tr_ml_cart(d[idx, ], "Species")
  expect_null(m2$origem)
  expect_no_warning(tr_ml_evaluate(tr_ml_predict(m2, d[idx, ]), "Species"))
  expect_s3_class(tr_ml_cart(d[-idx, ], "Species"), "tr_ml_fit")
})

test_that("prever o teste de outra divisão com o modelo desta é recusado", {
  s1 <- tr_ml_split(binaria(), "Species", seed = 1)
  s2 <- tr_ml_split(binaria(), "Species", seed = 2)
  m <- tr_ml_cart(s1$treino, "Species")
  expect_error(tr_ml_predict(m, s2$teste), class = "tr_ml_error_split_mismatch")
  expect_s3_class(tr_ml_predict(m, s1$teste), "tbl_df")
  # sem a marca, as impressões ainda veem linhas do teste 2 no treino 1
  sem <- s1$treino; attr(sem, "tr_ml_origem") <- NULL
  expect_error(tr_ml_predict(tr_ml_cart(sem, "Species"), s2$teste), class = "tr_ml_error_test_leak")
  # modelo de dados disjuntos do teste prevê normalmente
  expect_s3_class(tr_ml_predict(tr_ml_cart(sem, "Species"), s1$teste), "tbl_df")
})

test_that("marca sobrevive ao store e à releitura do cache no fluxo real", {
  reg <- trama::tr_registry()
  trama::tr_use("trama.data", registry = reg)
  trama::tr_use("trama.view", registry = reg)
  trama::tr_use(trama_collection(), registry = reg)
  f <- trama::tr_flow(reg) |>
    trama::tr_add("dados", "ml/example", nome = "iris_binaria") |>
    trama::tr_add("divisao", "ml/split", alvo = "Species", from = "dados") |>
    trama::tr_add("errado", "ml/cart", alvo = "Species", from = "divisao:teste") |>
    trama::tr_add("modelo", "ml/cart", alvo = "Species", from = "divisao:treino") |>
    trama::tr_add("no_treino", "ml/predict", from = c("modelo", "divisao:treino")) |>
    trama::tr_add("aval_treino", "ml/evaluate", alvo = "Species", from = "no_treino") |>
    trama::tr_add("aval_treino_ok", "ml/evaluate", alvo = "Species", permitir_treino = TRUE, from = "no_treino") |>
    trama::tr_add("no_teste", "ml/predict", from = c("modelo", "divisao:teste")) |>
    trama::tr_add("aval_teste", "ml/evaluate", alvo = "Species", from = "no_teste")
  store <- trama::tr_store(tempfile())
  doc <- trama::tr_flow_doc(f)
  val <- function(id, port = NULL) trama::tr_value(doc, id, port = port, registry = reg, store = store)
  for (rodada in 1:2) {   # a segunda lê tudo do cache (restore do RDS)
    expect_equal(attr(val("divisao", "teste"), "tr_ml_origem")$papel, "teste", info = rodada)
    expect_error(val("errado"), "treina no teste", info = rodada)
    expect_error(val("aval_treino"), "otimista", info = rodada)
    ok <- suppressWarnings(val("aval_treino_ok"))
    expect_true(all(grepl("otimista", ok$nota)), info = rodada)
    expect_false("nota" %in% names(val("aval_teste")), info = rodada)
    expect_equal(val("modelo")$origem$papel, "treino", info = rodada)
  }
  typ <- trama.data::trama_collection()$types
  tabela <- Filter(function(t) identical(t$id, "data/table"), typ)[[1L]]
  path <- tempfile(fileext = ".rds")
  tabela$store(val("divisao", "teste"), path)
  expect_error(tr_ml_cart(tabela$restore(path), "Species"), class = "tr_ml_error_test_leak")
})

# --- Proveniência por linha (impressões digitais): os desvios do revisor ----
# Cada caso reproduz um contorno de scratchpad/leak.R e leak2.R.

test_that("B1: modelo que viu linhas do teste não prevê esse teste", {
  ex <- binaria(); s <- tr_ml_split(ex, "Species")
  # ajustado na tabela inteira, antes de dividir
  expect_error(tr_ml_predict(tr_ml_cart(ex, "Species"), s$teste), class = "tr_ml_error_test_leak")
  # ajustado numa cópia do teste sem a marca
  u <- s$teste; attr(u, "tr_ml_origem") <- NULL
  expect_error(tr_ml_predict(tr_ml_cart(u, "Species"), s$teste), class = "tr_ml_error_test_leak")
  # ajustado no treino mais algumas linhas do teste sem marca
  mais <- rbind(as.data.frame(s$treino), as.data.frame(u)[1:3, ])
  attr(mais, "tr_ml_origem") <- NULL
  expect_error(tr_ml_predict(tr_ml_linear(mais, "Species"), s$teste), class = "tr_ml_error_test_leak")
  # o caminho legítimo continua aberto
  expect_s3_class(tr_ml_predict(tr_ml_cart(s$treino, "Species"), s$teste), "tbl_df")
})

test_that("B2: tabela de treino que contém o teste é recusada no ajuste e na divisão", {
  s <- tr_ml_split(binaria(), "Species"); tr <- s$treino; te <- s$teste
  juntos <- list(rbind = rbind(tr, te),
                 bind_rows = trama.data::tr_bind_rows(list(tr, te)),
                 parte = rbind(tr, te[1, ]))
  if (requireNamespace("dplyr", quietly = TRUE)) juntos$dplyr <- dplyr::bind_rows(tr, te)
  for (nm in names(juntos)) {
    x <- juntos[[nm]]
    expect_error(tr_ml_cart(x, "Species"), class = "tr_ml_error_test_leak", info = nm)
    expect_error(tr_ml_split(x, "Species", seed = 7), class = "tr_ml_error_test_leak", info = nm)
  }
  expect_error(tr_ml_tune(juntos$rbind, "Species", tentativas = 2, folds = 2),
               class = "tr_ml_error_test_leak")
  expect_error(tr_ml_nested_cv(juntos$rbind, "Species", tentativas = 2, folds_externos = 2, folds = 2),
               class = "tr_ml_error_test_leak")
  # colunas novas e reordenadas não apagam a impressão (ela usa as colunas da divisão)
  x <- juntos$rbind; x$z <- 1; x <- x[rev(names(x))]
  expect_error(tr_ml_cart(x, "Species"), class = "tr_ml_error_test_leak")
})

test_that("B4: avaliar previsões de treino misturadas ao teste exige permitir_treino", {
  s <- tr_ml_split(binaria(), "Species")
  m <- tr_ml_cart(s$treino, "Species")
  pe <- tr_ml_predict(m, s$teste); pt <- tr_ml_predict(m, s$treino)
  mistos <- list(rbind = rbind(pe, pt), bind_rows = trama.data::tr_bind_rows(list(pe, pt)))
  if (requireNamespace("dplyr", quietly = TRUE)) mistos$dplyr <- dplyr::bind_rows(pe, pt)
  for (nm in names(mistos)) {
    x <- mistos[[nm]]
    expect_error(tr_ml_evaluate(x, "Species"), class = "tr_ml_error_train_eval", info = nm)
    expect_error(tr_ml_confusion(x, "Species"), class = "tr_ml_error_train_eval", info = nm)
    expect_error(tr_ml_roc(x, "Species", ".prob_virginica"), class = "tr_ml_error_train_eval", info = nm)
    expect_error(tr_ml_pr_curve(x, "Species", ".prob_virginica"), class = "tr_ml_error_train_eval", info = nm)
    expect_warning(e <- tr_ml_evaluate(x, "Species", permitir_treino = TRUE), "otimista")
    expect_true(all(grepl("otimista", e$nota)))
  }
  # teste repetido também não é o teste
  expect_error(tr_ml_evaluate(rbind(pe, pe), "Species"), class = "tr_ml_error_train_eval")
})

test_that("sem falsos positivos: filtros, colunas novas e linhas duplicadas entre os lados", {
  # iris_binaria tem linhas idênticas (102 e 143 do iris); com duplicatas
  # forçadas nos dois lados, o multiconjunto separa cópia legítima de vazamento.
  d <- binaria(); d <- rbind(d, d[1:20, ])
  for (sd in 1:5) {
    s <- tr_ml_split(d, "Species", seed = sd)
    m <- tr_ml_cart(s$treino, "Species")
    p <- tr_ml_predict(m, s$teste)
    expect_no_error(tr_ml_evaluate(p, "Species"))
    f <- p[p$Sepal.Length > 5.5, ]; f$extra <- 1
    expect_no_error(tr_ml_evaluate(f, "Species"))
    expect_no_error(tr_ml_tune(s$treino, "Species", tentativas = 1, folds = 2))
    v <- tr_ml_split(s$treino, "Species", seed = 1)
    expect_no_error(tr_ml_predict(tr_ml_cart(v$treino, "Species"), v$teste))
  }
  # o modelo guarda as impressões do treino, qualquer que seja a marca
  s <- tr_ml_split(binaria(), "Species")
  m <- tr_ml_cart(s$treino, "Species")
  expect_equal(sum(m$treino_impressoes$n), nrow(s$treino))
  expect_equal(sum(tr_ml_cart(binaria(), "Species")$treino_impressoes$n), 100L)
})

test_that("B3: limites documentados da marca continuam como descritos", {
  # Fixa o que a documentação diz que a marca NÃO cobre; se um destes passar a
  # ser coberto, atualize proveniencia.R, a ajuda e a página do site.
  s <- tr_ml_split(binaria(), "Species"); te <- s$teste
  chave <- data.frame(Species = levels(te$Species), k = 1)
  direita <- trama.data::tr_join(chave, te, by = "Species", type = "left")
  expect_null(attr(direita, "tr_ml_origem"))
  mao <- data.frame(as.list(as.data.frame(te)))
  expect_null(attr(mao, "tr_ml_origem"))
  expect_s3_class(tr_ml_cart(mao, "Species"), "tr_ml_fit")
  # coluna da divisão reescrita: só a checagem de papel continua
  x <- rbind(s$treino, te); x$Sepal.Length <- round(x$Sepal.Length + 0.01, 2)
  expect_s3_class(tr_ml_cart(x, "Species"), "tr_ml_fit")
})
