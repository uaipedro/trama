# Curva ROC dos classificadores: a troca entre sensibilidade e especificidade
# ao longo de todos os cortes.
#
# A matriz de confusão responde "quanto acerta NESTE corte"; a ROC mostra todos
# de uma vez, e a área sob ela (AUC) é a probabilidade de um caso positivo ao
# acaso ter escore maior que um negativo ao acaso — independente do corte. Por
# padrão sai da validação CRUZADA, pelo mesmo motivo da confusão.

#' Pontos da curva e AUC para um escore e um vetor lógico de positivos.
#'
#' Os cortes são os escores distintos, do maior ao menor; empates andam na
#' diagonal (sobem e avançam juntos). A AUC é a de Mann-Whitney com meio ponto
#' por empate, que é exatamente a área trapezoidal dessa curva.
#' @noRd
.tr_multi_roc_curva <- function(score, positivo) {
  n1 <- sum(positivo); n0 <- sum(!positivo)
  cortes <- sort(unique(score), decreasing = TRUE)
  tpr <- vapply(cortes, function(k) sum(score >= k & positivo) / n1, 0)
  fpr <- vapply(cortes, function(k) sum(score >= k & !positivo) / n0, 0)
  auc <- (sum(rank(score)[positivo]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  list(pontos = data.frame(fpr = c(0, fpr), tpr = c(0, tpr)), auc = auc)
}

.tr_multi_virgula <- function(x, d = 3L) formatC(x, format = "f", digits = d, decimal.mark = ",")

#' Curva ROC de um classificador.
#' @param modelo objeto `tr_multi_lda` ou `tr_multi_logit`.
#' @param validacao `"cruzada"` ou `"resubstituição"`.
#' @inheritParams trama.view::tr_view_finish
#' @return ggplot.
#' @export
tr_multi_roc <- function(modelo, validacao = "cruzada", aspecto = "1:1", tema = "padrão",
                         titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  no <- "multi/roc"
  validacao <- .tr_multi_enum(validacao, .TR_MULTI_VALIDACOES, "validacao")
  pr <- .tr_multi_prever(modelo, validacao, no)
  niv <- levels(pr$g)
  diag_df <- data.frame(x = c(0, 1), y = c(0, 1))
  base <- ggplot2::ggplot() +
    ggplot2::geom_line(data = diag_df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
                       colour = .TR_MULTI_CINZA, linetype = "dashed")
  # geom_path, e não geom_step: os pontos já saem um por corte, na ordem, e um
  # empate entre positivo e negativo precisa andar na DIAGONAL até o próximo
  # ponto — é essa a curva cuja área trapezoidal é a AUC do subtítulo.
  if (length(niv) == 2L) {
    # Positivo é o SEGUNDO nível, como na logística binária.
    cur <- .tr_multi_roc_curva(pr$prob[, niv[[2]]], pr$g == niv[[2]])
    corte <- if (inherits(modelo, "tr_multi_logit")) modelo$corte else 0.5
    s <- pr$prob[, niv[[2]]] >= corte
    ponto <- data.frame(fpr = mean(s[pr$g == niv[[1]]]), tpr = mean(s[pr$g == niv[[2]]]))
    p <- base +
      ggplot2::geom_path(data = cur$pontos, ggplot2::aes(x = .data[["fpr"]], y = .data[["tpr"]]),
                         colour = .TR_MULTI_COR, linewidth = .9) +
      ggplot2::geom_point(data = ponto, ggplot2::aes(x = .data[["fpr"]], y = .data[["tpr"]]),
                          colour = .TR_MULTI_COR_2, size = 3) +
      ggplot2::labs(subtitle = sprintf("AUC %s · validação %s · positivo: %s · ponto: corte %s",
                                       .tr_multi_virgula(cur$auc), validacao, niv[[2]],
                                       .tr_multi_virgula(corte, 2L)))
  } else {
    # Uma curva por grupo, ele contra todos os outros.
    # O rótulo leva a AUC, e rótulo de texto sairia em ordem ALFABÉTICA na
    # legenda; o fator com os níveis na ordem dos grupos a mantém.
    curvas <- lapply(niv, function(l) {
      cur <- .tr_multi_roc_curva(pr$prob[, l], pr$g == l)
      cbind(cur$pontos, grupo = sprintf("%s (AUC %s)", l, .tr_multi_virgula(cur$auc)))
    })
    rotulos <- vapply(curvas, function(d) as.character(d$grupo[[1]]), "")
    curvas <- do.call(rbind, curvas)
    curvas$grupo <- factor(curvas$grupo, levels = rotulos)
    p <- base +
      ggplot2::geom_path(data = curvas, ggplot2::aes(x = .data[["fpr"]], y = .data[["tpr"]],
                                                     colour = .data[["grupo"]], group = .data[["grupo"]]),
                         linewidth = .8) +
      ggplot2::labs(colour = modelo$grupo,
                    subtitle = sprintf("Cada grupo contra os outros · validação %s", validacao))
  }
  p <- p + ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(x = "1 − especificidade (falsos positivos)", y = "sensibilidade (verdadeiros positivos)")
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

.tr_multi_nos_roc <- function() {
  list(
    trama::tr_node("multi/roc",
      pressupostos = .tr_multi_doc("multi/roc")$pressupostos,
      referencias = .tr_multi_doc("multi/roc")$referencias,
      role = "avaliacao", fn = tr_multi_roc, label = "Curva ROC",
      category = "multi_discriminante", icon = trama::tr_icon("chart-line"),
      description = "Sensibilidade × especificidade em todos os cortes, com a AUC, por validação cruzada.",
      inputs = list(modelo = "multi/classifier"), outputs = list(out = "view/plot"),
      params = .tr_multi_props(
        validacao = trama::tr_param_enum("cruzada", .TR_MULTI_VALIDACOES, label = "Validação"),
        .aspecto = "1:1"),
      help = .tr_multi_ajuda(r"---[
Para cada corte de probabilidade possível, a fração dos positivos que a regra
pega (**sensibilidade**) contra a fração dos negativos que ela alarma à toa
(**1 − especificidade**). Um classificador inútil anda na diagonal tracejada;
um perfeito sobe reto até o canto superior esquerdo.

A **AUC** (área sob a curva) é a probabilidade de um caso positivo sorteado ter
probabilidade prevista maior que a de um negativo sorteado: 0,5 é moeda, 1 é
perfeito. Não depende do corte, e por isso compara modelos melhor que a taxa
de acerto quando os grupos são desbalanceados (no `pima`, um terço tem
diabetes: prever "não" para todas já acerta 67%).

- **Dois grupos** — uma curva; o positivo é o SEGUNDO nível. O ponto rosa é o
  corte do modelo (o `corte` da `multi/logistic`; 0,5 na discriminante).
- **Três ou mais** — uma curva por grupo, ele contra todos os outros, com a AUC
  de cada na legenda.

Por padrão as probabilidades vêm da validação **cruzada** (deixa-um-fora): a
curva por resubstituição é otimista pelo mesmo motivo da `multi/confusion`.
Serve à discriminante e à logística.
]---", r"---[
- **Validação** — `cruzada` (padrão) ou `resubstituição`.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("pima", "multi/example", dataset = "pima") |>
  tr_add("lg", "multi/logistic", grupo = "diabetes", from = "pima") |>
  tr_add("roc", "multi/roc", validacao = "cruzada", from = "lg")
]---", r"---[
`multi/confusion` para o acerto num corte; `multi/logistic` para mudar o corte;
`multi/classify` com validação cruzada para ver os casos.
]---", grafico = TRUE))
  )
}
