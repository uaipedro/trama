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

.tr_ml_plot_args <- function(aspecto = "16:9", tema = "padr\u{E3}o", titulo = "",
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
  # A ordem das folhas sai de um percurso em profundidade com o ramo "sim"
  # primeiro, não da ordem das linhas: no FIGS a lista de nós não segue a
  # árvore, e o ramo "não" chegava a ser desenhado à esquerda do "sim".
  filhos <- lapply(filhos, function(ch) ch[order(nodes$ramo[match(ch, ids)] != "sim")])
  folhas <- character()
  percorre <- function(id) {
    ch <- filhos[[as.character(id)]]
    if (!length(ch)) folhas <<- c(folhas, as.character(id)) else for (c in ch) percorre(c)
  }
  for (r in ids[is.na(nodes$pai)]) percorre(r)
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

# Número de rótulo: vírgula decimal, no máximo `casas` casas e sem zeros à
# direita — "2,45", nunca "2,450", que se lê como dois mil quatrocentos e
# cinquenta.
.tr_ml_rotulo_num <- function(x, casas) {
  s <- formatC(round(x, casas), format = "f", digits = casas, big.mark = ".", decimal.mark = ",")
  if (casas > 0L) s <- sub(",$", "", sub("0+$", "", s))
  s
}

.tr_ml_cart_plot_data <- function(modelo, casas = 3L) {
  fr <- modelo$ajuste$frame
  ids <- as.integer(row.names(fr))
  decisao <- valor <- rep(NA_character_, nrow(fr)); valor_num <- rep(NA_real_, nrow(fr)); pos <- 1L
  for (i in seq_len(nrow(fr))) {
    if (fr$var[[i]] == "<leaf>") {
      z <- if (modelo$tarefa == "classificacao") modelo$niveis[[fr$yval[[i]]]] else
        format(fr$yval[[i]], digits = 4L)
      valor[[i]] <- z
      if (modelo$tarefa == "regressao") valor_num[[i]] <- fr$yval[[i]]
    } else {
      sp <- modelo$ajuste$splits[pos, , drop = FALSE]
      variavel <- modelo$preditores[match(row.names(modelo$ajuste$splits)[[pos]], modelo$internos)]
      operador <- if (unname(sp[1L, "ncat"]) < 0) " < " else " >= "
      decisao[[i]] <- paste0(variavel, operador,
                             .tr_ml_rotulo_num(unname(sp[1L, "index"]), casas))
      pos <- pos + 1L + fr$ncompete[[i]] + fr$nsurrogate[[i]]
    }
  }
  tibble::tibble(id = ids, pai = ifelse(ids == 1L, NA_integer_, ids %/% 2L),
    ramo = ifelse(ids == 1L, NA_character_, ifelse(ids %% 2L == 0L, "sim", "n\u{E3}o")),
    folha = fr$var == "<leaf>", decisao = decisao, valor = valor,
    valor_num = valor_num, n = fr$n, qualidade = fr$dev / pmax(fr$n, 1),
    nome_qualidade = "impureza")
}

.tr_ml_figs_plot_data <- function(modelo, arvore, casas = 3L) {
  arvore <- .tr_ml_int(arvore, "arvore", 1L)
  if (arvore > length(modelo$ajuste$trees)) {
    .tr_ml_abort("tr_ml_error_bad_param", "Param 'arvore' excede o n\u{FA}mero de \u{E1}rvores do FIGS.")
  }
  tr <- modelo$ajuste$trees[[arvore]]
  ids <- vapply(tr, `[[`, numeric(1), "id")
  pai <- vapply(ids, function(id) {
    p <- which(vapply(tr, function(no) identical(no$left_child, id) || identical(no$right_child, id), logical(1)))
    if (length(p)) ids[[p[[1L]]]] else NA_real_
  }, numeric(1))
  decisao <- vapply(tr, function(no) {
    if (no$is_leaf) return(NA_character_)
    variavel <- modelo$preditores[match(no$feature, modelo$internos)]
    paste0(variavel, " <= ", .tr_ml_rotulo_num(no$split_val, casas))
  }, character(1))
  ramo <- vapply(ids, function(id) {
    p <- which(vapply(tr, function(no) identical(no$left_child, id) || identical(no$right_child, id), logical(1)))
    if (!length(p)) return(NA_character_)
    if (identical(tr[[p[[1L]]]]$left_child, id)) "sim" else "n\u{E3}o"
  }, character(1))
  tibble::tibble(id = ids, pai = pai,
    ramo = ramo, folha = vapply(tr, `[[`, logical(1), "is_leaf"), decisao = decisao,
    valor = vapply(tr, function(no) if (no$is_leaf) format(no$value, digits = 17L) else NA_character_, character(1)),
    valor_num = vapply(tr, function(no) if (no$is_leaf) no$value else NA_real_, numeric(1)),
    n = vapply(tr, function(no) length(no$sample_indices), integer(1)),
    qualidade = vapply(tr, `[[`, numeric(1), "gain"), nome_qualidade = "ganho")
}

.tr_ml_forest_plot_data <- function(modelo, arvore, casas = 3L) {
  arvore <- .tr_ml_int(arvore, "arvore", 1L)
  if (!requireNamespace("ranger", quietly = TRUE))
    .tr_ml_abort("tr_ml_error_missing_engine", "Instale 'ranger' para visualizar esta floresta.")
  if (arvore > modelo$ajuste$num.trees)
    .tr_ml_abort("tr_ml_error_bad_param", "Param 'arvore' excede o número de árvores da floresta.")
  tr <- ranger::treeInfo(modelo$ajuste, tree = arvore)
  ids <- tr$nodeID
  pai <- vapply(ids, function(id) {
    i <- which(tr$leftChild == id | tr$rightChild == id)
    if (length(i)) tr$nodeID[[i[[1L]]]] else NA_integer_
  }, integer(1))
  ramo <- vapply(ids, function(id) {
    i <- which(tr$leftChild == id | tr$rightChild == id)
    if (!length(i)) return(NA_character_)
    if (tr$leftChild[[i[[1L]]]] == id) "sim" else "não"
  }, character(1))
  folha <- tr$terminal
  valor <- rep(NA_character_, length(ids))
  valor_num <- rep(NA_real_, length(ids))
  if ("prediction" %in% names(tr)) {
    valor[folha] <- as.character(tr$prediction[folha])
    if (is.numeric(tr$prediction)) valor_num[folha] <- tr$prediction[folha]
  } else {
    probs <- grep("^pred\\.", names(tr), value = TRUE)
    if (length(probs)) {
      classe <- sub("^pred\\.", "", probs)
      valor[folha] <- vapply(which(folha), function(i) classe[[which.max(unlist(tr[i, probs, drop = FALSE]))]], character(1))
    }
  }
  decisao <- rep(NA_character_, length(ids))
  variaveis <- modelo$preditores[match(tr$splitvarName, modelo$internos)]
  variaveis[is.na(variaveis)] <- tr$splitvarName[is.na(variaveis)]
  decisao[!folha] <- paste0(variaveis[!folha], " <= ",
                            .tr_ml_rotulo_num(tr$splitval[!folha], casas))
  tibble::tibble(id = ids + 1L, pai = ifelse(is.na(pai), NA_integer_, pai + 1L), ramo = ramo,
    folha = folha, decisao = decisao, valor = valor, valor_num = valor_num,
    n = as.integer(tr$numSamples %||% rep(NA_integer_, length(ids))),
    qualidade = tr$splitStat %||% rep(NA_real_, length(ids)), nome_qualidade = "ganho")
}

#' Visualizar uma árvore CART, FIGS ou de uma floresta aleatória
#' @param modelo Modelo CART, FIGS ou floresta devolvido por [tr_ml_fit()]; outro modelo
#'   é recusado com erro `tr_ml_error_not_fit`.
#' @param arvore Índice positivo da árvore no FIGS ou na floresta. Ignorado pelo CART.
#' @param mostrar_n Inclui em cada nó o número de observações que o alcançam.
#' @param mostrar_impureza Inclui a impureza do CART ou o ganho do FIGS.
#' @param casas Número inteiro, de zero a seis, de casas decimais nos rótulos.
#' @param aspecto Proporção do gráfico: `"16:9"`, `"4:3"`, `"1:1"`, `"3:4"`
#'   ou `"2:1"`.
#' @param tema Nome de um tema registrado no projeto.
#' @param titulo Título do gráfico. Vazio omite o título.
#' @param rotulo_x Rótulo do eixo X. Vazio mantém o rótulo gerado.
#' @param rotulo_y Rótulo do eixo Y. Vazio mantém o rótulo gerado.
#' @param legenda Posição `"direita"` ou `"abaixo"`. `"nenhuma"` a omite.
#' @return Objeto `ggplot` com os nós, ramos e folhas da árvore selecionada.
#' @export
tr_ml_tree_plot <- function(modelo, arvore = 1L, mostrar_n = TRUE,
                            mostrar_impureza = FALSE, casas = 3L,
                            aspecto = "16:9", tema = "padr\u{E3}o",
                            titulo = "", rotulo_x = "", rotulo_y = "",
                            legenda = "direita") {
  .tr_ml_exigir_fit(modelo, "ml/tree_plot")
  if (!modelo$modelo %in% c("cart", "figs", "forest"))
    .tr_ml_abort("tr_ml_error_not_applicable", "A visualiza\u{E7}\u{E3}o de \u{E1}rvore aceita apenas CART, FIGS e floresta aleat\u{F3}ria.")
  if (length(mostrar_n) != 1L || !is.logical(mostrar_n) || is.na(mostrar_n) ||
      length(mostrar_impureza) != 1L || !is.logical(mostrar_impureza) || is.na(mostrar_impureza))
    .tr_ml_abort("tr_ml_error_bad_param", "'mostrar_n' e 'mostrar_impureza' devem ser TRUE ou FALSE.")
  casas <- .tr_ml_int(casas, "casas", 0L)
  if (casas > 6L) .tr_ml_abort("tr_ml_error_bad_param", "Param 'casas' n\u{E3}o pode superar 6.")
  nodes <- switch(modelo$modelo, cart = .tr_ml_cart_plot_data(modelo, casas),
    figs = .tr_ml_figs_plot_data(modelo, arvore, casas),
    forest = .tr_ml_forest_plot_data(modelo, arvore, casas))
  numero <- function(x) .tr_ml_rotulo_num(x, casas)
  valor_rotulo <- ifelse(is.finite(nodes$valor_num), numero(nodes$valor_num), nodes$valor)
  nodes$label <- ifelse(nodes$folha,
    paste0(if (modelo$modelo == "figs") "contribui\u{E7}\u{E3}o: " else "previs\u{E3}o: ", valor_rotulo),
    nodes$decisao)
  if (mostrar_n && any(!is.na(nodes$n))) nodes$label <- ifelse(!is.na(nodes$n),
    paste0(nodes$label, "\nn = ", nodes$n), nodes$label)
  if (mostrar_impureza && any(is.finite(nodes$qualidade))) nodes$label <- ifelse(is.finite(nodes$qualidade),
    paste0(nodes$label, "\n", nodes$nome_qualidade, " = ", numero(nodes$qualidade)), nodes$label)
  nodes <- .tr_ml_tree_layout(nodes)
  edges <- nodes[!is.na(nodes$pai), ]
  p <- ggplot2::ggplot(nodes, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_segment(data = edges,
      ggplot2::aes(x = .data$x_pai, y = .data$y_pai, xend = .data$x, yend = .data$y),
      inherit.aes = FALSE, colour = "#94a3b8", linewidth = .7) +
    ggplot2::geom_label(data = edges,
      ggplot2::aes(x = (.data$x_pai + .data$x) / 2, y = (.data$y_pai + .data$y) / 2,
                   label = .data$ramo), inherit.aes = FALSE, size = 2.7,
      label.size = 0, fill = "white", colour = "#172033") +
    ggplot2::geom_label(ggplot2::aes(label = .data$label, fill = .data$folha),
                        label.size = .25, size = 3, label.padding = grid::unit(0.16, "lines"),
                        colour = "#172033", show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c(`FALSE` = "#dbeafe", `TRUE` = "#dcfce7")) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 0.65)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.75)) +
    ggplot2::coord_cartesian(clip = "off") + ggplot2::labs(x = NULL, y = NULL)
  p <- .tr_ml_finish_plot(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
  p + ggplot2::theme(axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(), plot.margin = ggplot2::margin(14, 24, 14, 24))
}

#' Gráfico de resíduos de regressão
#' @param dados Tabela com valores observados e previstos.
#' @param resposta Nome da coluna observada.
#' @param predito Nome da coluna prevista; o padrão `previsto` é a coluna que
#'   a `models/predict` escreve.
#' @param aspecto Proporção do gráfico: `"16:9"`, `"4:3"`, `"1:1"`, `"3:4"`
#'   ou `"2:1"`.
#' @param tema Nome de um tema registrado no projeto.
#' @param titulo Título do gráfico. Vazio omite o título.
#' @param rotulo_x Rótulo do eixo X. Vazio mantém `"Previsão"`.
#' @param rotulo_y Rótulo do eixo Y. Vazio mantém o rótulo de resíduo.
#' @param legenda Posição `"direita"` ou `"abaixo"`. `"nenhuma"` a omite.
#' @return Objeto `ggplot` dos resíduos contra as previsões recebidas.
#' @export
tr_ml_residuals <- function(dados, resposta = "", predito = "previsto", aspecto = "16:9",
                            tema = "padr\u{E3}o", titulo = "", rotulo_x = "",
                            rotulo_y = "", legenda = "direita") {
  .tr_ml_validate_pair(dados, resposta, predito)
  if (!is.numeric(dados[[resposta]]) || !is.numeric(dados[[predito]]))
    .tr_ml_abort("tr_ml_error_not_applicable", "Res\u{ED}duos exigem resposta e previs\u{E3}o num\u{E9}ricos.")
  if (anyNA(dados[[resposta]]) || anyNA(dados[[predito]]) ||
      any(!is.finite(dados[[resposta]])) || any(!is.finite(dados[[predito]])))
    .tr_ml_abort("tr_ml_error_bad_prediction", "Res\u{ED}duos exigem valores presentes e finitos.")
  d <- tibble::tibble(.previsto = dados[[predito]], .residuo = dados[[resposta]] - dados[[predito]])
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$.previsto, y = .data$.residuo)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#94a3b8", linewidth = .6) +
    ggplot2::geom_point(size = 2.4, alpha = .85) +
    ggplot2::labs(x = "Previs\u{E3}o", y = "Res\u{ED}duo (observado \u{2212} previsto)")
  .tr_ml_finish_plot(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Visualizar o histórico de uma busca de hiperparâmetros
#' @param dados Histórico devolvido por [tr_ml_tune()].
#' @param hiperparametro Nome de uma coluna numérica do histórico. Vazio mostra
#'   a evolução das tentativas e do melhor valor acumulado.
#' @param aspecto Proporção do gráfico: `"16:9"`, `"4:3"`, `"1:1"`, `"3:4"`
#'   ou `"2:1"`.
#' @param tema Nome de um tema registrado no projeto.
#' @param titulo Título do gráfico. Vazio omite o título.
#' @param rotulo_x Rótulo do eixo X. Vazio mantém o rótulo gerado.
#' @param rotulo_y Rótulo do eixo Y. Vazio mantém `"Métrica média"`.
#' @param legenda Posição `"direita"` ou `"abaixo"`. `"nenhuma"` a omite.
#' @return Objeto `ggplot` da evolução do tuning ou da relação entre um
#'   hiperparâmetro e a métrica média.
#' @export
tr_ml_tuning_plot <- function(dados, hiperparametro = "", aspecto = "16:9", tema = "padr\u{E3}o", titulo = "",
                              rotulo_x = "", rotulo_y = "", legenda = "direita") {
  obrigatorias <- c("tentativa", "media", "melhor", "status")
  if (!is.data.frame(dados) || !all(obrigatorias %in% names(dados)))
    .tr_ml_abort("tr_ml_error_bad_tuning", "O hist\u{F3}rico precisa das colunas tentativa, media, melhor e status.")
  d <- dados[dados$status == "ok", , drop = FALSE]
  if (!nrow(d)) .tr_ml_abort("tr_ml_error_bad_tuning", "O hist\u{F3}rico n\u{E3}o cont\u{E9}m tentativas v\u{E1}lidas.")
  if (nzchar(hiperparametro)) {
    if (!hiperparametro %in% names(d) || !is.numeric(d[[hiperparametro]]))
      .tr_ml_abort("tr_ml_error_bad_tuning", "'hiperparametro' deve nomear uma coluna num\u{E9}rica do hist\u{F3}rico.")
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[hiperparametro]], y = .data$media)) +
      ggplot2::geom_point(size = 2.5, alpha = .85) +
      ggplot2::labs(x = hiperparametro, y = "M\u{E9}trica m\u{E9}dia")
    return(.tr_ml_finish_plot(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda))
  }
  longo <- rbind(
    data.frame(tentativa = d$tentativa, valor = d$media, serie = "Tentativa"),
    data.frame(tentativa = d$tentativa, valor = d$melhor, serie = "Melhor at\u{E9} aqui"))
  p <- ggplot2::ggplot(longo, ggplot2::aes(x = .data$tentativa, y = .data$valor,
                                           colour = .data$serie)) +
    ggplot2::geom_line(linewidth = .8) + ggplot2::geom_point(size = 2.2) +
    ggplot2::labs(x = "Tentativa", y = "M\u{E9}trica m\u{E9}dia", colour = NULL)
  .tr_ml_finish_plot(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}
