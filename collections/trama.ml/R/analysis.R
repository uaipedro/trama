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
                             format(unname(sp[1L, "index"]), digits = 17L))
      pos <- pos + 1L + fr$ncompete[[i]] + fr$nsurrogate[[i]]
    }
  }
  tibble::tibble(id = ids, pai = ifelse(ids == 1L, NA_integer_, ids %/% 2L),
    ramo = ifelse(ids == 1L, NA_character_, ifelse(ids %% 2L == 0L, "sim", "n\u{E3}o")),
    folha = fr$var == "<leaf>", decisao = decisao, valor = valor,
    valor_num = valor_num, n = fr$n, qualidade = fr$dev / pmax(fr$n, 1),
    nome_qualidade = "impureza")
}

.tr_ml_figs_plot_data <- function(modelo, arvore) {
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
    paste0(variavel, " <= ", format(no$split_val, digits = 17L))
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

#' Visualizar uma árvore CART ou uma das árvores do FIGS
#' @param modelo Modelo CART ou FIGS devolvido por [tr_ml_fit()].
#' @param arvore Índice positivo da árvore do FIGS. Ignorado pelo CART.
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
  if (!inherits(modelo, "tr_ml_fit"))
    .tr_ml_abort("tr_ml_error_not_fit", "Param 'modelo' n\u{E3}o \u{E9} um ajuste de machine learning.")
  if (!modelo$modelo %in% c("cart", "figs"))
    .tr_ml_abort("tr_ml_error_not_applicable", "A visualiza\u{E7}\u{E3}o de \u{E1}rvore aceita apenas CART e FIGS.")
  nodes <- if (modelo$modelo == "cart") .tr_ml_cart_plot_data(modelo) else
    .tr_ml_figs_plot_data(modelo, arvore)
  if (length(mostrar_n) != 1L || !is.logical(mostrar_n) || is.na(mostrar_n) ||
      length(mostrar_impureza) != 1L || !is.logical(mostrar_impureza) || is.na(mostrar_impureza))
    .tr_ml_abort("tr_ml_error_bad_param", "'mostrar_n' e 'mostrar_impureza' devem ser TRUE ou FALSE.")
  casas <- .tr_ml_int(casas, "casas", 0L)
  if (casas > 6L) .tr_ml_abort("tr_ml_error_bad_param", "Param 'casas' n\u{E3}o pode superar 6.")
  numero <- function(x) formatC(x, format = "f", digits = casas)
  valor_rotulo <- ifelse(is.finite(nodes$valor_num), numero(nodes$valor_num), nodes$valor)
  nodes$label <- ifelse(nodes$folha,
    paste0(if (modelo$modelo == "figs") "contribui\u{E7}\u{E3}o: " else "previs\u{E3}o: ", valor_rotulo),
    nodes$decisao)
  if (mostrar_n) nodes$label <- paste0(nodes$label, "\nn = ", nodes$n)
  if (mostrar_impureza) nodes$label <- paste0(nodes$label, "\n", nodes$nome_qualidade,
                                               " = ", numero(nodes$qualidade))
  nodes <- .tr_ml_tree_layout(nodes)
  edges <- nodes[!is.na(nodes$pai), ]
  p <- ggplot2::ggplot(nodes, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_segment(data = edges,
      ggplot2::aes(x = .data$x_pai, y = .data$y_pai, xend = .data$x, yend = .data$y),
      inherit.aes = FALSE, colour = "#94a3b8", linewidth = .7) +
    ggplot2::geom_label(data = edges,
      ggplot2::aes(x = (.data$x_pai + .data$x) / 2, y = (.data$y_pai + .data$y) / 2,
                   label = .data$ramo), inherit.aes = FALSE, size = 2.7,
      label.size = 0, fill = "white") +
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
#' @param aspecto Proporção do gráfico: `"16:9"`, `"4:3"`, `"1:1"`, `"3:4"`
#'   ou `"2:1"`.
#' @param tema Nome de um tema registrado no projeto.
#' @param titulo Título do gráfico. Vazio omite o título.
#' @param rotulo_x Rótulo do eixo X. Vazio mantém `"Previsão"`.
#' @param rotulo_y Rótulo do eixo Y. Vazio mantém o rótulo de resíduo.
#' @param legenda Posição `"direita"` ou `"abaixo"`. `"nenhuma"` a omite.
#' @return Objeto `ggplot` dos resíduos contra as previsões recebidas.
#' @export
tr_ml_residuals <- function(dados, alvo = "", predito = ".pred", aspecto = "16:9",
                            tema = "padr\u{E3}o", titulo = "", rotulo_x = "",
                            rotulo_y = "", legenda = "direita") {
  .tr_ml_validate_pair(dados, alvo, predito)
  if (!is.numeric(dados[[alvo]]) || !is.numeric(dados[[predito]]))
    .tr_ml_abort("tr_ml_error_not_applicable", "Res\u{ED}duos exigem alvo e previs\u{E3}o num\u{E9}ricos.")
  if (anyNA(dados[[alvo]]) || anyNA(dados[[predito]]) ||
      any(!is.finite(dados[[alvo]])) || any(!is.finite(dados[[predito]])))
    .tr_ml_abort("tr_ml_error_bad_prediction", "Res\u{ED}duos exigem valores presentes e finitos.")
  d <- tibble::tibble(.previsto = dados[[predito]], .residuo = dados[[alvo]] - dados[[predito]])
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$.previsto, y = .data$.residuo)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#94a3b8", linewidth = .6) +
    ggplot2::geom_point(size = 2.4, alpha = .85) +
    ggplot2::labs(x = "Previs\u{E3}o", y = "Res\u{ED}duo (observado \u{2212} previsto)")
  .tr_ml_finish_plot(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Curva ROC para classificação binária
#' @param dados Tabela com a classe observada e sua probabilidade prevista.
#' @param alvo Nome da coluna observada.
#' @param probabilidade Coluna com a probabilidade da classe positiva.
#' @param positiva Classe tratada como positiva. Vazio deduz a classe do nome da
#'   coluna de probabilidade (`.prob_<classe>`, como sai de [tr_ml_predict()]);
#'   se o nome não indicar uma classe observada, o bloco recusa
#'   (`tr_ml_error_positive_required`) em vez de adivinhar.
#' @param aspecto Proporção do gráfico: `"16:9"`, `"4:3"`, `"1:1"`, `"3:4"`
#'   ou `"2:1"`.
#' @param tema Nome de um tema registrado no projeto.
#' @param titulo Título do gráfico. Vazio omite o título.
#' @param rotulo_x Rótulo do eixo X. Vazio mantém o rótulo gerado.
#' @param rotulo_y Rótulo do eixo Y. Vazio mantém o rótulo gerado.
#' @param legenda Posição `"direita"` ou `"abaixo"`. `"nenhuma"` a omite.
#' @param confianca Nível do intervalo de confiança da AUC (DeLong et al.
#'   1988), entre 0 e 1.
#' @param permitir_treino Se FALSE (padrão), recusa linhas marcadas como
#'   treino pelo `ml/split` (`tr_ml_error_train_eval`); se TRUE, desenha,
#'   avisa e põe a nota de otimismo na legenda e na coluna `nota`.
#' @return Objeto `ggplot` da curva ROC. Os dados do gráfico trazem, por corte,
#'   `limiar` (prevê positivo com P >= limiar), `fpr` e `tpr`, e repetidos a
#'   `auc`, seu erro-padrão (`auc_ep`) e intervalo (`auc_inf`, `auc_sup`) de
#'   DeLong, `auc_nota` (NA, ou o motivo quando o IC é indisponível: AUC = 0
#'   ou 1 dá variância de DeLong zero e o IC sai NA), o corte de Youden (`youden_limiar`, `youden_j`) e a marca
#'   `youden` na linha escolhida.
#' @export
tr_ml_roc <- function(dados, alvo = "", probabilidade = "", positiva = "",
                      confianca = 0.95, permitir_treino = FALSE, aspecto = "16:9", tema = "padr\u{E3}o", titulo = "",
                      rotulo_x = "", rotulo_y = "", legenda = "direita") {
  .tr_ml_validate_pair(dados, alvo, probabilidade)
  nota <- .tr_ml_checar_avaliacao(dados, permitir_treino)
  if (!is.numeric(confianca) || length(confianca) != 1L || !is.finite(confianca) ||
      confianca <= 0 || confianca >= 1)
    .tr_ml_abort("tr_ml_error_bad_param", "Param 'confianca' deve estar entre 0 e 1.")
  y <- as.character(dados[[alvo]]); prob <- dados[[probabilidade]]
  classes <- unique(y)
  if (anyNA(y) || length(classes) != 2L || !is.numeric(prob) || anyNA(prob) ||
      any(!is.finite(prob)) || any(prob < 0 | prob > 1))
    .tr_ml_abort("tr_ml_error_not_applicable",
                 "ROC exige duas classes e uma probabilidade finita entre 0 e 1.")
  if (!nzchar(positiva)) positiva <- .tr_ml_roc_positiva(dados[[alvo]], probabilidade)
  if (!positiva %in% classes)
    .tr_ml_abort("tr_ml_error_bad_param", "Param 'positiva' deve ser uma classe observada.")
  ord <- order(prob, decreasing = TRUE)
  positivo <- y[ord] == positiva
  grupos <- cumsum(c(TRUE, diff(prob[ord]) != 0))
  tp <- c(0, cumsum(as.numeric(rowsum(as.integer(positivo), grupos))))
  fp <- c(0, cumsum(as.numeric(rowsum(as.integer(!positivo), grupos))))
  d <- tibble::tibble(limiar = c(Inf, prob[ord][!duplicated(grupos)]),
                      fpr = fp / sum(!positivo), tpr = tp / sum(positivo))
  n_curva <- nrow(d)
  d$auc <- sum(diff(d$fpr) * (d$tpr[-n_curva] + d$tpr[-1L]) / 2)
  ic <- .tr_ml_auc_delong(prob[y == positiva], prob[y != positiva], confianca)
  d$auc_ep <- ic$ep; d$auc_inf <- ic$inf; d$auc_sup <- ic$sup; d$auc_nota <- ic$nota
  # Corte de Youden (1950): maximiza J = sensibilidade + especificidade - 1;
  # em empate, o de maior limiar (menos positivos previstos).
  j <- d$tpr - d$fpr
  i <- which.max(j)
  d$youden_limiar <- d$limiar[[i]]; d$youden_j <- j[[i]]
  d$youden <- seq_len(n_curva) == i
  d <- .tr_ml_com_nota(d, nota)
  ic_txt <- if (is.na(ic$inf)) "IC indispon\u{ED}vel" else
    sprintf("IC %s%%: %.3f a %.3f", format(100 * confianca), ic$inf, ic$sup)
  rotulo <- sprintf("AUC = %.3f (%s)\nYouden: J = %.3f com P \u{2265} %s",
                    d$auc[[1]], ic_txt, j[[i]], format(signif(d$limiar[[i]], 3)))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$fpr, y = .data$tpr)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "#94a3b8") +
    ggplot2::geom_step(linewidth = 1) + ggplot2::coord_equal() +
    ggplot2::geom_point(data = d[i, ], size = 3, colour = "#dc2626") +
    ggplot2::annotate("text", x = .6, y = .1, label = rotulo, size = 3.3) +
    ggplot2::labs(x = "Taxa de falsos positivos", y = "Taxa de verdadeiros positivos",
                  caption = if (nzchar(nota)) nota else NULL)
  .tr_ml_finish_plot(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Curva precisão-revocação para classificação binária
#'
#' Ordena as linhas pela probabilidade da classe positiva e traça a precisão
#' contra a revocação em cada corte (empates num só degrau). A precisão média
#' (AP) é a soma, nos cortes, do ganho de revocação vezes a precisão
#' (sem interpolação; Su, Yuan & Zhu 2015). A linha de referência é a
#' prevalência da classe positiva, a precisão de um classificador ao acaso
#' (Saito & Rehmsmeier 2015). A `area` é a área sob a curva com a interpolação
#' não linear de Davis & Goadrich (2006), integrada de forma contínua; é menor
#' que a AP quando há degraus, e a interpolação linear da ROC não vale aqui.
#' @inheritParams tr_ml_roc
#' @return Objeto `ggplot`. Os dados do gráfico trazem `limiar`, `recall`,
#'   `precision`, a `ap`, a `area` (Davis & Goadrich) e a `prevalencia`.
#' @examples
#' d <- data.frame(y = c("sim", "nao", "sim", "nao", "sim"),
#'                 .prob_sim = c(.9, .8, .7, .6, .2))
#' tr_ml_pr_curve(d, "y", ".prob_sim")
#' @export
tr_ml_pr_curve <- function(dados, alvo = "", probabilidade = "", positiva = "",
                           permitir_treino = FALSE, aspecto = "16:9", tema = "padr\u{E3}o", titulo = "",
                           rotulo_x = "", rotulo_y = "", legenda = "direita") {
  .tr_ml_validate_pair(dados, alvo, probabilidade)
  y <- as.character(dados[[alvo]]); prob <- dados[[probabilidade]]
  classes <- unique(y)
  if (anyNA(y) || length(classes) != 2L || !is.numeric(prob) || anyNA(prob) ||
      any(!is.finite(prob)) || any(prob < 0 | prob > 1))
    .tr_ml_abort("tr_ml_error_not_applicable",
                 "A curva precis\u{E3}o-revoca\u{E7}\u{E3}o exige duas classes e uma probabilidade finita entre 0 e 1.")
  nota <- .tr_ml_checar_avaliacao(dados, permitir_treino)
  if (!nzchar(positiva)) positiva <- .tr_ml_roc_positiva(dados[[alvo]], probabilidade)
  if (!positiva %in% classes)
    .tr_ml_abort("tr_ml_error_bad_param", "Param 'positiva' deve ser uma classe observada.")
  d <- .tr_ml_com_nota(.tr_ml_pr_pontos(y == positiva, prob), nota)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$recall, y = .data$precision)) +
    ggplot2::geom_hline(yintercept = d$prevalencia[[1]], linetype = 2, colour = "#94a3b8") +
    ggplot2::geom_step(direction = "vh", linewidth = 1) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::annotate("text", x = .3, y = .08,
                      label = sprintf("AP = %.3f  (acaso = %.3f)", d$ap[[1]], d$prevalencia[[1]])) +
    ggplot2::labs(x = "Revoca\u{E7}\u{E3}o", y = "Precis\u{E3}o",
                  caption = if (nzchar(nota)) nota else NULL)
  .tr_ml_finish_plot(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

# Um ponto por limiar distinto (decrescente): prevê positivo quando prob >= limiar.
.tr_ml_pr_pontos <- function(positivo, prob) {
  ord <- order(prob, decreasing = TRUE)
  positivo <- positivo[ord]; prob <- prob[ord]
  grupos <- cumsum(c(TRUE, diff(prob) != 0))
  tp <- cumsum(as.numeric(rowsum(as.integer(positivo), grupos)))
  fp <- cumsum(as.numeric(rowsum(as.integer(!positivo), grupos)))
  recall <- tp / sum(positivo); precision <- tp / (tp + fp)
  ap <- sum(diff(c(0, recall)) * precision)
  # Área com a interpolação de Davis & Goadrich (2006), contínua (Keilwagen,
  # Grosse & Grau 2014): entre dois cortes, cada positivo a mais traz
  # s = dFP/dTP falsos positivos, e a precisão (a + x)/(c + k x), com
  # a = TP, c = TP + FP e k = 1 + s, é integrada em forma fechada.
  a <- c(0, tp[-length(tp)]); b <- c(0, fp[-length(fp)])
  dtp <- tp - a; dfp <- fp - b
  area <- 0
  for (i in which(dtp > 0)) {
    k <- 1 + dfp[[i]] / dtp[[i]]; c0 <- a[[i]] + b[[i]]
    area <- area + if (c0 == 0) dtp[[i]] / k else
      dtp[[i]] / k + (a[[i]] - c0 / k) / k * log((c0 + k * dtp[[i]]) / c0)
  }
  tibble::tibble(limiar = prob[!duplicated(grupos)], recall = recall, precision = precision,
                 ap = ap, area = area / sum(positivo), prevalencia = mean(positivo))
}

# IC da AUC por DeLong, DeLong & Clarke-Pearson (1988): componentes
# estruturais (placements) de cada positivo e de cada negativo, variância
# S10/m + S01/n e intervalo normal truncado em [0, 1] (como `pROC`).
.tr_ml_auc_delong <- function(pos, neg, confianca) {
  m <- length(pos); n <- length(neg)
  psi <- outer(pos, neg, function(a, b) (a > b) + 0.5 * (a == b))
  v10 <- rowMeans(psi); v01 <- colMeans(psi); auc <- mean(psi)
  # A variância de DeLong usa a variância amostral dos componentes de cada
  # classe: com menos de duas linhas numa classe ela não existe. A curva e a
  # AUC continuam válidas, então o bloco não recusa (o `multi/roc`, que só
  # reporta o IC, recusa com `tr_multi_error_small_group`): o IC sai NA com a nota.
  if (m < 2L || n < 2L) return(list(ep = NA_real_, inf = NA_real_, sup = NA_real_, nota = sprintf(paste(
    "IC de DeLong indispon\u{ED}vel: h\u{E1} menos de duas linhas numa classe (%d positivas e %d",
    "negativas), e a vari\u{E2}ncia precisa de ao menos duas de cada."), m, n)))
  ep <- sqrt(stats::var(v10) / m + stats::var(v01) / n)
  # AUC = 0 ou 1 (separação perfeita): todos os componentes estruturais são
  # iguais, a variância de DeLong é zero e o intervalo teria largura zero. Isso
  # não é certeza, é o estimador sem informação: o IC sai NA com a nota.
  if (ep == 0) {
    return(list(ep = ep, inf = NA_real_, sup = NA_real_, nota = paste(
      "IC de DeLong degenerado: com AUC =", format(auc), "a vari\u{E2}ncia estimada \u{E9} zero",
      "e o intervalo n\u{E3}o informa a incerteza; use mais linhas ou reamostragem.")))
  }
  z <- stats::qnorm(1 - (1 - confianca) / 2)
  list(ep = ep, inf = max(0, auc - z * ep), sup = min(1, auc + z * ep), nota = NA_character_)
}

# Classe positiva padrão: a que a coluna `.prob_<classe>` nomeia. Sem esse
# nome, não há como saber de que classe é a probabilidade, e adivinhar pode
# espelhar a curva (AUC vira 1 - AUC): exige `positiva`.
.tr_ml_roc_positiva <- function(y, probabilidade) {
  niveis <- unique(as.character(y))
  da_coluna <- sub("^\\.prob_", "", probabilidade)
  if (startsWith(probabilidade, ".prob_") && da_coluna %in% niveis) return(da_coluna)
  .tr_ml_abort("tr_ml_error_positive_required",
    "A coluna '%s' n\u{E3}o indica a classe (esperado '.prob_<classe>'); informe 'positiva' com a classe cuja probabilidade ela cont\u{E9}m.",
    probabilidade)
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
