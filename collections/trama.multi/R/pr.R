# Curva precisão-revocação dos classificadores: a irmã da ROC para quando o
# grupo que interessa é raro.
#
# A ROC divide os falsos positivos pelo total de NEGATIVOS; com um grupo raro
# eles se diluem no grupo grande e a curva parece boa enquanto metade dos
# alarmes é falsa. A precisão divide pelos casos que a regra CHAMA de
# positivos, e é ela que mostra isso (Saito & Rehmsmeier 2015). A conta é a
# mesma da `ml/pr_curve` (a coleção não depende da `trama.ml`, então mora aqui
# também), sobre as probabilidades por deixa-um-fora da `multi/roc`.

#' Pontos da curva, AP e área de Davis & Goadrich.
#'
#' Um ponto por escore distinto, do maior ao menor (empates num degrau só).
#' AP = Σ ΔR·P, sem interpolação (a do `yardstick` e do scikit-learn). A área
#' interpolada é a de Davis & Goadrich (2006), integrada em forma fechada
#' (Keilwagen, Grosse & Grau 2014; a do `PRROC`): entre dois cortes, cada
#' positivo a mais traz s = ΔFP/ΔTP falsos positivos, e a precisão (a + x)/(c
#' + kx), com a = TP, c = TP + FP e k = 1 + s, integra-se em fechado.
#' @noRd
.tr_multi_pr_pontos <- function(positivo, prob) {
  ord <- order(prob, decreasing = TRUE)
  positivo <- positivo[ord]; prob <- prob[ord]
  grupos <- cumsum(c(TRUE, diff(prob) != 0))
  tp <- cumsum(as.numeric(rowsum(as.integer(positivo), grupos)))
  fp <- cumsum(as.numeric(rowsum(as.integer(!positivo), grupos)))
  recall <- tp / sum(positivo); precision <- tp / (tp + fp)
  ap <- sum(diff(c(0, recall)) * precision)
  a <- c(0, tp[-length(tp)]); b <- c(0, fp[-length(fp)])
  dtp <- tp - a; dfp <- fp - b
  area <- 0
  for (i in which(dtp > 0)) {
    k <- 1 + dfp[[i]] / dtp[[i]]; c0 <- a[[i]] + b[[i]]
    area <- area + if (c0 == 0) dtp[[i]] / k else
      dtp[[i]] / k + (a[[i]] - c0 / k) / k * log((c0 + k * dtp[[i]]) / c0)
  }
  data.frame(limiar = prob[!duplicated(grupos)], recall = recall, precision = precision,
             ap = ap, area = area / sum(positivo), prevalencia = mean(positivo))
}

#' Curva precisão-revocação de um classificador.
#' @param modelo objeto `tr_multi_lda` ou `tr_multi_logit`.
#' @param validacao `"cruzada"` ou `"resubstituição"`.
#' @inheritParams trama.view::tr_view_finish
#' @return ggplot.
#' @export
tr_multi_pr_curve <- function(modelo, validacao = "cruzada", aspecto = "1:1", tema = "padrão",
                              titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  no <- "multi/pr_curve"
  validacao <- .tr_multi_enum(validacao, .TR_MULTI_VALIDACOES, "validacao")
  pr <- .tr_multi_prever(modelo, validacao, no)
  niv <- levels(pr$g)
  v <- .tr_multi_virgula
  if (length(niv) == 2L) {
    # Positivo é o SEGUNDO nível, como na `multi/roc` e na logística binária.
    d <- .tr_multi_pr_pontos(pr$g == niv[[2]], pr$prob[, niv[[2]]])
    d$grupo <- niv[[2]]
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["recall"]], y = .data[["precision"]])) +
      ggplot2::geom_hline(yintercept = d$prevalencia[[1]], colour = .TR_MULTI_CINZA, linetype = "dashed") +
      ggplot2::geom_step(direction = "vh", colour = .TR_MULTI_COR, linewidth = .9) +
      ggplot2::geom_point(colour = .TR_MULTI_COR, size = 1.4) +
      ggplot2::labs(subtitle = sprintf("AP %s · acaso %s · área (Davis & Goadrich) %s · validação %s · positivo: %s",
                                       v(d$ap[[1]]), v(d$prevalencia[[1]]), v(d$area[[1]]), validacao,
                                       niv[[2]]))
  } else {
    curvas <- lapply(niv, function(l) {
      d <- .tr_multi_pr_pontos(pr$g == l, pr$prob[, l])
      d$grupo <- sprintf("%s (AP %s; acaso %s)", l, v(d$ap[[1]]), v(d$prevalencia[[1]]))
      d
    })
    rotulos <- vapply(curvas, function(d) d$grupo[[1]], "")
    d <- do.call(rbind, curvas)
    d$grupo <- factor(d$grupo, levels = rotulos)
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["recall"]], y = .data[["precision"]],
                                         colour = .data[["grupo"]], group = .data[["grupo"]])) +
      ggplot2::geom_step(direction = "vh", linewidth = .8) +
      ggplot2::labs(colour = modelo$grupo,
                    subtitle = sprintf("Cada grupo contra os outros · validação %s", validacao))
  }
  p <- p + ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(x = "revocação (sensibilidade)", y = "precisão (valor preditivo positivo)")
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

.tr_multi_nos_pr <- function() {
  list(
    trama::tr_node("multi/pr_curve",
      pressupostos = .tr_multi_doc("multi/pr_curve")$pressupostos,
      referencias = .tr_multi_doc("multi/pr_curve")$referencias,
      role = "avaliacao", fn = tr_multi_pr_curve, label = "Curva precisão-revocação",
      category = "multi_discriminante", icon = trama::tr_icon("chart-line"),
      description = "Precisão × revocação em todos os cortes, com a precisão média (AP), por validação cruzada.",
      inputs = list(modelo = "multi/classifier"), outputs = list(out = "view/plot"),
      params = .tr_multi_props(
        validacao = trama::tr_param_enum("cruzada", .TR_MULTI_VALIDACOES, label = "Validação"),
        .aspecto = "1:1"),
      help = .tr_multi_ajuda(r"---[
Para cada corte de probabilidade, a fração dos positivos que a regra pega
(**revocação**, a sensibilidade da ROC) contra a fração dos que ela chama de
positivos que são mesmo positivos (**precisão**). Com um grupo raro, é a
curva que mostra os alarmes falsos: na ROC eles se diluem no grupo grande
(Saito & Rehmsmeier 2015).

- **AP** (precisão média) — Σ (ganho de revocação × precisão) nos cortes, sem
  interpolação (a do `yardstick` e do scikit-learn).
- **acaso** — a linha tracejada: a prevalência do positivo, a precisão de uma
  regra ao acaso. A AP só diz algo comparada a ela, e não se compara entre
  tabelas com prevalências diferentes.
- **área (Davis & Goadrich)** — a área sob a curva com a interpolação não
  linear de Davis & Goadrich (2006), a do `PRROC`. Ligar os pontos por reta,
  como na ROC, superestima a área.

**Dois grupos** — uma curva; o positivo é o SEGUNDO nível, como na
`multi/roc`. **Três ou mais** — uma curva por grupo contra os outros, com AP e
prevalência de cada na legenda.

Por padrão as probabilidades vêm da validação **cruzada** (deixa-um-fora),
pelo mesmo motivo da `multi/confusion`. Serve à discriminante e à logística.
É a mesma conta da curva precisão-revocação da coleção de aprendizado de máquina (ml/pr_curve).
]---", r"---[
- **Validação** — `cruzada` (padrão) ou `resubstituição`.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("pima", "multi/example", dataset = "pima") |>
  tr_add("lg", "multi/logistic", grupo = "diabetes", from = "pima") |>
  tr_add("pr", "multi/pr_curve", from = "lg")
]---", r"---[
`multi/roc` para a troca sensibilidade × especificidade; `multi/confusion`
(`tabela = "métricas"`) para precisão e revocação num corte.
]---", grafico = TRUE))
  )
}
