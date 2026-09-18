#' @importFrom rlang .data
#' @noRd
NULL

# Gráficos diagnósticos e de inspeção. Todos atravessam o mesmo funil visual
# da coleção view, de modo que tema, proporção e preview permaneçam uniformes.

.tr_ml_finish_plot <- function(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda) {
  if (!requireNamespace("trama.view", quietly = TRUE)) {
    .tr_ml_abort("tr_ml_error_missing_engine",
                 "Instale 'trama.view' para usar os visualizadores de machine learning.")
  }
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

.tr_ml_plot_args <- function(aspecto = "16:9", tema = "padrão", titulo = "",
                             rotulo_x = "", rotulo_y = "", legenda = "direita") {
  list(aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

.tr_ml_tree_layout <- function(nodes) {
  ids <- nodes$id
  filhos <- split(nodes$id[!is.na(nodes$pai)], nodes$pai[!is.na(nodes$pai)])
  profundidade <- function(id) {
    p <- nodes$pai[match(id, ids)]
    if (is.na(p)) 0L else profundidade(p) + 1L
  }
  y <- -vapply(ids, profundidade, integer(1))
  folhas <- ids[nodes$folha]
  x_folha <- stats::setNames(seq_along(folhas), folhas)
  pos_x <- function(id) {
    ch <- filhos[[as.character(id)]]
    if (!length(ch)) return(unname(x_folha[[as.character(id)]]))
    mean(vapply(ch, pos_x, numeric(1)))
  }
  nodes$x <- vapply(ids, pos_x, numeric(1))
  nodes$y <- y
  nodes$x_pai <- nodes$x[match(nodes$pai, ids)]
  nodes$y_pai <- nodes$y[match(nodes$pai, ids)]
  nodes
}

.tr_ml_cart_plot_data <- function(modelo) {
  fr <- modelo$ajuste$frame
  ids <- as.integer(row.names(fr))
  labels <- character(nrow(fr)); pos <- 1L
  for (i in seq_len(nrow(fr))) {
    if (fr$var[[i]] == "<leaf>") {
      valor <- if (modelo$tarefa == "classificacao") modelo$niveis[[fr$yval[[i]]]] else
        format(fr$yval[[i]], digits = 4L)
      labels[[i]] <- paste0("previsão: ", valor, "\nn = ", fr$n[[i]])
    } else {
      sp <- modelo$ajuste$splits[pos, , drop = FALSE]
      variavel <- modelo$preditores[match(row.names(modelo$ajuste$splits)[[pos]], modelo$internos)]
      labels[[i]] <- paste0(variavel, " < ", format(unname(sp[1L, "index"]), digits = 5L),
                            "\nn = ", fr$n[[i]])
      pos <- pos + 1L + fr$ncompete[[i]] + fr$nsurrogate[[i]]
    }
  }
  tibble::tibble(id = ids, pai = ifelse(ids == 1L, NA_integer_, ids %/% 2L),
                 folha = fr$var == "<leaf>", label = labels)
}

.tr_ml_figs_plot_data <- function(modelo, arvore) {
  arvore <- .tr_ml_int(arvore, "arvore", 1L)
  if (arvore > length(modelo$ajuste$trees)) {
    .tr_ml_abort("tr_ml_error_bad_param", "Param 'arvore' excede o número de árvores do FIGS.")
  }
  tr <- modelo$ajuste$trees[[arvore]]
  ids <- vapply(tr, `[[`, numeric(1), "id")
  pai <- vapply(ids, function(id) {
    p <- which(vapply(tr, function(no) identical(no$left_child, id) || identical(no$right_child, id), logical(1)))
    if (length(p)) ids[[p[[1L]]]] else NA_real_
  }, numeric(1))
  labels <- vapply(tr, function(no) {
    if (no$is_leaf) return(paste0("contribuição: ", format(no$value, digits = 4L),
                                  "\nn = ", length(no$sample_indices)))
    variavel <- modelo$preditores[match(no$feature, modelo$internos)]
    paste0(variavel, " <= ", format(no$split_val, digits = 5L),
           "\nn = ", length(no$sample_indices))
  }, character(1))
  tibble::tibble(id = ids, pai = pai,
                 folha = vapply(tr, `[[`, logical(1), "is_leaf"), label = labels)
}

#' Visualizar uma árvore CART ou uma das árvores do FIGS
#' @param modelo Modelo `tr_ml_fit` CART ou FIGS.
#' @param arvore Árvore do FIGS a mostrar.
#' @param aspecto,tema,titulo,rotulo_x,rotulo_y,legenda Aparência do gráfico.
#' @return Um `ggplot` editável.
#' @export
tr_ml_tree_plot <- function(modelo, arvore = 1L, aspecto = "16:9", tema = "padrão",
                            titulo = "", rotulo_x = "", rotulo_y = "",
                            legenda = "direita") {
  if (!inherits(modelo, "tr_ml_fit"))
    .tr_ml_abort("tr_ml_error_not_fit", "Param 'modelo' não é um ajuste de machine learning.")
  if (!modelo$modelo %in% c("cart", "figs"))
    .tr_ml_abort("tr_ml_error_not_applicable", "A visualização de árvore aceita apenas CART e FIGS.")
  nodes <- if (modelo$modelo == "cart") .tr_ml_cart_plot_data(modelo) else
    .tr_ml_figs_plot_data(modelo, arvore)
  nodes <- .tr_ml_tree_layout(nodes)
  edges <- nodes[!is.na(nodes$pai), ]
  p <- ggplot2::ggplot(nodes, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_segment(data = edges,
      ggplot2::aes(x = .data$x_pai, y = .data$y_pai, xend = .data$x, yend = .data$y),
      inherit.aes = FALSE, colour = "#94a3b8", linewidth = .7) +
    ggplot2::geom_label(ggplot2::aes(label = .data$label, fill = .data$folha),
                        label.size = .25, size = 3.2, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c(`FALSE` = "#dbeafe", `TRUE` = "#dcfce7")) +
    ggplot2::labs(x = NULL, y = NULL) + ggplot2::theme(axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
  .tr_ml_finish_plot(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Gráfico de resíduos de regressão
#' @param dados Tabela com valores observados e previstos.
#' @param alvo Nome da coluna observada.
#' @param predito Nome da coluna prevista.
#' @param aspecto,tema,titulo,rotulo_x,rotulo_y,legenda Aparência do gráfico.
#' @return Um `ggplot` editável.
#' @export
tr_ml_residuals <- function(dados, alvo = "", predito = ".pred", aspecto = "16:9",
                            tema = "padrão", titulo = "", rotulo_x = "",
                            rotulo_y = "", legenda = "direita") {
  .tr_ml_validate_pair(dados, alvo, predito)
  if (!is.numeric(dados[[alvo]]) || !is.numeric(dados[[predito]]))
    .tr_ml_abort("tr_ml_error_not_applicable", "Resíduos exigem alvo e previsão numéricos.")
  if (anyNA(dados[[alvo]]) || anyNA(dados[[predito]]) ||
      any(!is.finite(dados[[alvo]])) || any(!is.finite(dados[[predito]])))
    .tr_ml_abort("tr_ml_error_bad_prediction", "Resíduos exigem valores presentes e finitos.")
  d <- tibble::tibble(.previsto = dados[[predito]], .residuo = dados[[alvo]] - dados[[predito]])
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$.previsto, y = .data$.residuo)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#94a3b8", linewidth = .6) +
    ggplot2::geom_point(size = 2.4, alpha = .85) +
    ggplot2::labs(x = "Previsão", y = "Resíduo (observado − previsto)")
  .tr_ml_finish_plot(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Curva ROC para classificação binária
#' @param dados Tabela com a classe observada e sua probabilidade prevista.
#' @param alvo Nome da coluna observada.
#' @param probabilidade Coluna com a probabilidade da classe positiva.
#' @param positiva Classe tratada como positiva; vazio usa a segunda observada.
#' @param aspecto,tema,titulo,rotulo_x,rotulo_y,legenda Aparência do gráfico.
#' @return Um `ggplot` editável com a AUC em `data$auc`.
#' @export
tr_ml_roc <- function(dados, alvo = "", probabilidade = "", positiva = "",
                      aspecto = "16:9", tema = "padrão", titulo = "",
                      rotulo_x = "", rotulo_y = "", legenda = "direita") {
  .tr_ml_validate_pair(dados, alvo, probabilidade)
  y <- as.character(dados[[alvo]]); prob <- dados[[probabilidade]]
  classes <- unique(y)
  if (anyNA(y) || length(classes) != 2L || !is.numeric(prob) || anyNA(prob) ||
      any(!is.finite(prob)) || any(prob < 0 | prob > 1))
    .tr_ml_abort("tr_ml_error_not_applicable",
                 "ROC exige duas classes e uma probabilidade finita entre 0 e 1.")
  if (!nzchar(positiva)) positiva <- classes[[2L]]
  if (!positiva %in% classes)
    .tr_ml_abort("tr_ml_error_bad_param", "Param 'positiva' deve ser uma classe observada.")
  ord <- order(prob, decreasing = TRUE)
  positivo <- y[ord] == positiva
  grupos <- cumsum(c(TRUE, diff(prob[ord]) != 0))
  tp <- c(0, cumsum(as.numeric(rowsum(as.integer(positivo), grupos))))
  fp <- c(0, cumsum(as.numeric(rowsum(as.integer(!positivo), grupos))))
  d <- tibble::tibble(fpr = fp / sum(!positivo), tpr = tp / sum(positivo))
  n_curva <- nrow(d)
  d$auc <- sum(diff(d$fpr) * (d$tpr[-n_curva] + d$tpr[-1L]) / 2)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$fpr, y = .data$tpr)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "#94a3b8") +
    ggplot2::geom_step(linewidth = 1) + ggplot2::coord_equal() +
    ggplot2::annotate("text", x = .62, y = .08, label = sprintf("AUC = %.3f", d$auc[[1]])) +
    ggplot2::labs(x = "Taxa de falsos positivos", y = "Taxa de verdadeiros positivos")
  .tr_ml_finish_plot(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Visualizar o histórico de uma busca de hiperparâmetros
#' @param dados Histórico devolvido por [tr_ml_tune()].
#' @param aspecto,tema,titulo,rotulo_x,rotulo_y,legenda Aparência do gráfico.
#' @return Um `ggplot` editável.
#' @export
tr_ml_tuning_plot <- function(dados, aspecto = "16:9", tema = "padrão", titulo = "",
                              rotulo_x = "", rotulo_y = "", legenda = "direita") {
  obrigatorias <- c("tentativa", "media", "melhor", "status")
  if (!is.data.frame(dados) || !all(obrigatorias %in% names(dados)))
    .tr_ml_abort("tr_ml_error_bad_tuning", "O histórico precisa das colunas tentativa, media, melhor e status.")
  d <- dados[dados$status == "ok", , drop = FALSE]
  if (!nrow(d)) .tr_ml_abort("tr_ml_error_bad_tuning", "O histórico não contém tentativas válidas.")
  longo <- rbind(
    data.frame(tentativa = d$tentativa, valor = d$media, serie = "Tentativa"),
    data.frame(tentativa = d$tentativa, valor = d$melhor, serie = "Melhor até aqui"))
  p <- ggplot2::ggplot(longo, ggplot2::aes(x = .data$tentativa, y = .data$valor,
                                           colour = .data$serie)) +
    ggplot2::geom_line(linewidth = .8) + ggplot2::geom_point(size = 2.2) +
    ggplot2::labs(x = "Tentativa", y = "Métrica média", colour = NULL)
  .tr_ml_finish_plot(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}
