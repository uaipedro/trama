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

#' AUC com o intervalo de DeLong, DeLong & Clarke-Pearson (1988).
#'
#' A AUC de Mann-Whitney é a média das componentes estruturais: V10 de cada
#' positivo (a fração dos negativos abaixo dele, meio por empate) e V01 de cada
#' negativo. A variância é var(V10)/n1 + var(V01)/n0, e o intervalo é o normal
#' em torno da AUC, cortado em [0, 1] como no `pROC::ci.auc` (método
#' "delong"), conferido nos testes.
#'
#' Duas bordas dão IC NA com `nota`, como a `ml/roc` (a curva e a AUC
#' continuam válidas, então o gráfico não é recusado): menos de dois casos numa
#' classe (a variância amostral dos componentes não existe) e variância zero
#' (AUC 0 ou 1, separação perfeita: o intervalo teria largura zero, o que é o
#' estimador sem informação, não certeza).
#' @noRd
.tr_multi_auc_delong <- function(score, positivo, confianca) {
  n1 <- sum(positivo); n0 <- sum(!positivo)
  x <- score[positivo]; y <- score[!positivo]
  psi <- outer(x, y, function(a, b) (a > b) + 0.5 * (a == b))
  auc <- mean(psi)
  sem_ic <- function(ep, nota) list(auc = auc, ep = ep, ic_inf = NA_real_, ic_sup = NA_real_, nota = nota)
  if (n1 < 2L || n0 < 2L) {
    return(sem_ic(NA_real_, sprintf(paste0(
      "IC de DeLong indisponível: há menos de duas linhas numa classe (%d positivas e %d negativas), ",
      "e a variância precisa de ao menos duas de cada."), n1, n0)))
  }
  v10 <- rowMeans(psi); v01 <- colMeans(psi)
  ep <- sqrt(stats::var(v10) / n1 + stats::var(v01) / n0)
  if (ep == 0) {
    return(sem_ic(ep, sprintf(paste0(
      "IC de DeLong degenerado: com AUC = %s a variância estimada é zero e o intervalo não informa ",
      "a incerteza; use mais linhas ou reamostragem."), format(auc))))
  }
  z <- stats::qnorm(1 - (1 - confianca) / 2)
  list(auc = auc, ep = ep, ic_inf = max(0, auc - z * ep), ic_sup = min(1, auc + z * ep), nota = NA_character_)
}

#' AUC multiclasse M de Hand & Till (2001).
#'
#' Para cada par de grupos (i, j), só com os casos dos dois: A(i|j) é a AUC do
#' escore de i com i positivo, A(j|i) a do escore de j com j positivo, e
#' Â(i, j) a média delas. M é a média de Â sobre os c(c − 1)/2 pares. Não
#' depende das prevalências (cada par usa só os seus casos), ao contrário da
#' média das AUCs um-contra-os-outros.
#' @noRd
.tr_multi_auc_hand_till <- function(prob, g) {
  niv <- levels(g)
  pares <- utils::combn(niv, 2L)
  a <- function(i, j) {
    k <- g %in% c(i, j)
    .tr_multi_roc_curva(prob[k, i], g[k] == i)$auc
  }
  mean(apply(pares, 2L, function(p) (a(p[[1]], p[[2]]) + a(p[[2]], p[[1]])) / 2))
}

.tr_multi_virgula <- function(x, d = 3L) formatC(x, format = "f", digits = d, decimal.mark = ",")

#' Curva ROC de um classificador.
#' @param modelo objeto `tr_multi_lda` ou `tr_multi_logit`.
#' @param validacao `"cruzada"` ou `"resubstituição"`.
#' @param confianca nível do intervalo de DeLong da AUC.
#' @inheritParams trama.view::tr_view_finish
#' @return ggplot.
#' @export
tr_multi_roc <- function(modelo, validacao = "cruzada", confianca = 0.95, aspecto = "1:1", tema = "padrão",
                         titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  no <- "multi/roc"
  validacao <- .tr_multi_enum(validacao, .TR_MULTI_VALIDACOES, "validacao")
  confianca <- .tr_multi_num(confianca, "confianca", min = 0.5, max = 0.999)
  pr <- .tr_multi_prever(modelo, validacao, no)
  pct <- format(100 * confianca, decimal.mark = ",")
  # As notas de IC indisponível (uma por curva) vão juntas para a legenda.
  notas <- character()
  auc_ic <- function(s, pos, quem = NULL) {
    d <- .tr_multi_auc_delong(s, pos, confianca)
    if (!is.na(d$nota)) {
      notas <<- c(notas, if (is.null(quem)) d$nota else paste0(quem, ": ", d$nota))
      return(sprintf("AUC %s (IC indisponível)", .tr_multi_virgula(d$auc)))
    }
    sprintf("AUC %s (IC %s%% DeLong %s–%s)", .tr_multi_virgula(d$auc), pct,
            .tr_multi_virgula(d$ic_inf), .tr_multi_virgula(d$ic_sup))
  }
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
      ggplot2::labs(subtitle = sprintf("%s · validação %s · positivo: %s · ponto: corte %s",
                                       auc_ic(pr$prob[, niv[[2]]], pr$g == niv[[2]]), validacao, niv[[2]],
                                       .tr_multi_virgula(corte, 2L)))
  } else {
    # Uma curva por grupo, ele contra todos os outros.
    # O rótulo leva a AUC, e rótulo de texto sairia em ordem ALFABÉTICA na
    # legenda; o fator com os níveis na ordem dos grupos a mantém.
    curvas <- lapply(niv, function(l) {
      cur <- .tr_multi_roc_curva(pr$prob[, l], pr$g == l)
      cbind(cur$pontos, grupo = sprintf("%s (%s)", l, auc_ic(pr$prob[, l], pr$g == l, l)))
    })
    rotulos <- vapply(curvas, function(d) as.character(d$grupo[[1]]), "")
    curvas <- do.call(rbind, curvas)
    curvas$grupo <- factor(curvas$grupo, levels = rotulos)
    p <- base +
      ggplot2::geom_path(data = curvas, ggplot2::aes(x = .data[["fpr"]], y = .data[["tpr"]],
                                                     colour = .data[["grupo"]], group = .data[["grupo"]]),
                         linewidth = .8) +
      ggplot2::labs(colour = modelo$grupo,
                    subtitle = sprintf("Cada grupo contra os outros · AUC multiclasse (Hand & Till) %s · validação %s",
                                       .tr_multi_virgula(.tr_multi_auc_hand_till(pr$prob, pr$g)), validacao))
  }
  if (length(notas)) p <- p + ggplot2::labs(caption = paste(notas, collapse = "\n"))
  p <- p + ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(x = "1 − especificidade (falsos positivos)", y = "sensibilidade (verdadeiros positivos)")
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

.tr_multi_nos_roc <- function() {
  list(
    trama::tr_node("multi/roc", version = 4L,
      pressupostos = .tr_multi_doc("multi/roc")$pressupostos,
      referencias = .tr_multi_doc("multi/roc")$referencias,
      role = "avaliacao", fn = tr_multi_roc, label = "Curva ROC",
      category = "multi_discriminante", icon = trama::tr_icon("chart-line"),
      description = "Sensibilidade × especificidade em todos os cortes, com a AUC, por validação cruzada.",
      inputs = list(modelo = "multi/classifier"), outputs = list(out = "view/plot"),
      params = .tr_multi_props(
        validacao = trama::tr_param_enum("cruzada", .TR_MULTI_VALIDACOES, label = "Validação"),
        confianca = trama::tr_param_num(0.95, min = 0.5, max = 0.999, label = "Confiança da AUC"),
        .aspecto = "1:1"),
      help = .tr_multi_ajuda(r"---[
Para cada corte de probabilidade possível, a fração dos positivos que a regra
pega (**sensibilidade**) contra a fração dos negativos que ela alarma à toa
(**1 − especificidade**). Um classificador inútil anda na diagonal tracejada;
um perfeito sobe reto até o canto superior esquerdo.

A **AUC** (área sob a curva) é a probabilidade de um caso positivo sorteado ter
probabilidade prevista maior que a de um negativo sorteado: 0,5 é moeda, 1 é
perfeito. O intervalo é o de DeLong, DeLong & Clarke-Pearson (1988), não
paramétrico (variância pelas componentes estruturais da estatística de
Mann-Whitney), cortado em [0, 1]; com poucos positivos ele é largo, e é
justamente o que ele deve mostrar. Com validação cruzada as probabilidades
vêm de n ajustes diferentes, e o intervalo as trata como um escore só (a
prática usual; a variância do próprio ajuste não entra).
Com AUC 0 ou 1 (separação perfeita) a variância de DeLong é zero, e com
menos de dois casos numa classe ela não existe: nos dois casos a AUC sai
com "IC indisponível" e a legenda do gráfico diz por quê, sem recusar a
curva (como na ROC da coleção de aprendizado de máquina). Não depende do corte, e por isso compara modelos melhor que a taxa
de acerto quando os grupos são desbalanceados (no `pima`, um terço tem
diabetes: prever "não" para todas já acerta 67%).

- **Dois grupos** — uma curva; o positivo é o SEGUNDO nível. O ponto rosa é o
  corte do modelo (o `corte` da `multi/logistic`; 0,5 na discriminante).
- **Três ou mais** — uma curva por grupo, ele contra todos os outros, com a AUC
  de cada na legenda. O subtítulo traz a **AUC multiclasse** M de Hand & Till
  (2001): para cada par de grupos, só com os casos dos dois, a média da AUC
  do escore de um e da do escore do outro; M é a média sobre os pares. Resume
  o classificador inteiro sem depender das proporções dos grupos (a média das
  AUCs um-contra-os-outros depende). Não tem intervalo aqui.

Por padrão as probabilidades vêm da validação **cruzada** (deixa-um-fora): a
curva por resubstituição é otimista pelo mesmo motivo da `multi/confusion`.
Serve à discriminante e à logística.
]---", r"---[
- **Validação** — `cruzada` (padrão) ou `resubstituição`.
- **Confiança da AUC** — nível do intervalo de DeLong (padrão 0,95).
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
