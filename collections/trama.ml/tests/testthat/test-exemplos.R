# Os fluxos de exemplos/machine-learning foram gravados com os blocos antigos
# (ml/predict, ml/evaluate, `alvo`, `.pred`). Abertos agora, a migração da
# models os leva aos blocos de lá, e as métricas têm de sair IGUAIS às dos
# blocos antigos — os números abaixo foram capturados com a ml de antes da
# Fase 4, rodando os mesmos três fluxos.
test_that("fluxos de exemplo migram, rodam e repetem as métricas dos blocos antigos", {
  pasta <- test_path("..", "..", "..", "..", "exemplos", "machine-learning")
  skip_if_not(dir.exists(pasta), "exemplos fora da árvore (pacote instalado)")
  for (pkg in c("rpart", "figsr", "ranger", "e1071", "xgboost")) skip_if_not_installed(pkg)
  esperado <- list(
    main = list(cart_avaliar = c(accuracy = 0.7913043478, balanced_accuracy = 0.7931034483, macro_f1 = 0.7826086957),
                figs_avaliar = c(accuracy = 0.7869565217, balanced_accuracy = 0.7823529412, macro_f1 = 0.7759398795)),
    `cart-vs-figs` = list(cart_avaliar = c(accuracy = 0.9615384615, balanced_accuracy = 0.9615384615, macro_f1 = 0.9614814815),
                          figs_avaliar = c(accuracy = 0.9615384615, balanced_accuracy = 0.9615384615, macro_f1 = 0.9614814815)),
    regressao = list(linear_avaliar = c(mae = 2.8958402911, rmse = 3.5266947296, r2 = 0.7353487338),
                     cart_avaliar = c(mae = 4.0875000000, rmse = 5.0015174260, r2 = 0.4677179619),
                     figs_avaliar = c(mae = 3.9015416166, rmse = 4.7733065975, r2 = 0.5151840492),
                     forest_avaliar = c(mae = 2.3884789908, rmse = 3.0404245090, r2 = 0.8032989456),
                     svm_avaliar = c(mae = 3.6642966890, rmse = 4.7307408993, r2 = 0.5237921353),
                     xgboost_avaliar = c(mae = 2.7387759686, rmse = 3.5056633269, r2 = 0.7384958114)))
  n_teste <- c(main = 230L, `cart-vs-figs` = 26L, regressao = 8L)
  # `main` lê heart.csv por caminho relativo à pasta do exemplo.
  antiga <- setwd(pasta)
  on.exit(setwd(antiga), add = TRUE)
  reg <- ml_registry()
  for (arq in names(esperado)) {
    doc <- trama::tr_doc_migrate(trama::tr_doc_read(file.path("flows", paste0(arq, ".json"))), reg)
    expect_length(trama::tr_doc_validate(doc, reg), 0L)
    tipos <- vapply(doc$nodes, `[[`, "", "type")
    expect_false(any(tipos %in% c("ml/predict", "ml/evaluate")), info = arq)
    store <- trama::tr_store(tempfile())
    res <- trama::tr_run(doc, registry = reg, store = store)
    expect_length(res$skipped, 0L)
    for (no in names(esperado[[arq]])) {
      expect_equal(tipos[[no]], "models/evaluate")
      tab <- trama::tr_value(doc, no, registry = reg, store = store)
      obtido <- stats::setNames(tab$valor, tab$metrica)[names(esperado[[arq]][[no]])]
      expect_equal(obtido, esperado[[arq]][[no]], tolerance = 1e-8, info = paste(arq, no))
      expect_equal(unique(tab$n), n_teste[[arq]], info = paste(arq, no))
    }
  }
})
