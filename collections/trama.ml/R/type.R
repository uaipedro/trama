#' Persistência e resumo de modelos supervisionados.
#' @noRd
.tr_ml_model_type <- function() {
  trama::tr_type("ml/fit", label = "Modelo preditivo", color = "#65a30d",
    store = function(x, path) {
      if (!inherits(x, "tr_ml_fit") ||
          !all(c("ajuste", "modelo", "tarefa", "alvo", "preditores", "n") %in% names(x))) {
        rlang::abort("A porta espera um modelo ajustado por trama.ml.", class = "tr_ml_error_model")
      }
      # Um booster contém ponteiros nativos. O formato UBJ guarda o modelo
      # independentemente da sessão; o restante do contrato continua em RDS.
      if (identical(x$modelo, "xgboost")) {
        x$ajuste <- xgboost::xgb.save.raw(x$ajuste, raw_format = "ubj")
        x$.booster_raw <- TRUE
      }
      saveRDS(x, path)
    },
    restore = function(path) {
      x <- readRDS(path)
      if (isTRUE(x$.booster_raw)) {
        if (!requireNamespace("xgboost", quietly = TRUE)) {
          rlang::abort("Instale 'xgboost' para restaurar este modelo.", class = "tr_ml_error_dependency")
        }
        x$ajuste <- xgboost::xgb.load.raw(x$ajuste)
        x$.booster_raw <- NULL
      }
      x
    },
    summary = function(x) list(modelo = x$modelo, tarefa = x$tarefa,
      alvo = x$alvo, treino = x$n, preditores = paste(x$preditores, collapse = ", "),
      motor = paste(x$extras$engine, x$extras$engine_version),
      leitura = if (x$modelo %in% c("cart", "figs")) attr(tr_ml_rules(x), "nota") else
        "Avalie generaliza\u{E7}\u{E3}o com dados de teste."),
    preview = function(x, ctx) {
      tab <- if (x$modelo %in% c("cart", "figs")) tr_ml_rules(x) else
        tibble::tibble(modelo = x$modelo, tarefa = x$tarefa, alvo = x$alvo,
          linhas_treino = x$n, preditores = paste(x$preditores, collapse = ", "),
          leitura = "Use Prever e Avaliar com dados de teste; ajuste n\u{E3}o mede generaliza\u{E7}\u{E3}o.")
      .tr_ml_table_preview(tab)
    })
}

.tr_ml_table_preview <- function(x) {
  tipos <- trama.data::trama_collection()$types
  tabela <- Filter(function(tipo) identical(tipo$id, "data/table"), tipos)[[1L]]
  tabela$preview(x, NULL)
}
