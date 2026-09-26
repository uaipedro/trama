# As declarações dos nós de análise: contrastes, Box-Cox e superfície de
# resposta. A coleção os inclui na categoria `exp_analisar`.
#
# Entram e saem pelos tipos do `trama.models` (`models/fit`, `models/effects`):
# a análise de um experimento continua sendo um modelo da coleção models, e
# tudo o que já lê um `models/fit` lê o que sai daqui.

#' Os cosméticos da `view`, com a proporção padrão própria do gráfico.
#' @noRd
.tr_exp_an_props <- function(..., .aspecto = "4:3") {
  ps <- trama.view::tr_view_props(...)
  ps$aspecto$default <- .aspecto
  ps
}

.tr_experiments_nos_analisar <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; N <- trama::tr_param_num
  Fm <- "models/fit"; EF <- "models/effects"; T <- "data/table"
  list(
    trama::tr_node("experiments/contrasts", version = 3L, fn = tr_experiments_contrasts, label = "Contrastes",
      category = "exp_analisar", icon = trama::tr_icon("divide"),
      description = "Abre o SQ do tratamento: uma linha por contraste (polinomiais, Helmert, controle, 2^k ou digitados), com a conferência da soma e da ortogonalidade.",
      inputs = list(modelo = Fm), outputs = list(out = EF, ortogonalidade = T),
      params = list(
        fator = P("cols", "", label = "Fator", example = "nitrogenio"),
        conjunto = E("polinomiais", .TR_EXP_AN_CONJUNTOS, label = "Conjunto"),
        contrastes = P("expr", "", label = "Contrastes (digitados)",
                       example = "ctrl vs trat: 2 -1 -1; trt1 vs trt2: trt1 - trt2"),
        controle = P("text", "", label = "Controle", example = "ctrl"),
        doses = P("text", "", label = "Doses (valores dos níveis)", example = "0 30 60 120"),
        dentro = P("cols", "", label = "Dentro de (desdobrar)", example = "variedade")),
      help = .tr_exp_an_ajuda_contrasts()),

    trama::tr_node("experiments/boxcox", version = 2L, fn = tr_experiments_boxcox, label = "Box-Cox",
      category = "exp_analisar", icon = trama::tr_icon("chart-line"),
      description = "Perfil de verossimilhança em λ: a potência da resposta que normaliza o erro, com IC e a transformação sugerida.",
      inputs = list(modelo = Fm), outputs = list(out = "view/plot", resumo = T, perfil = T),
      params = c(list(
        lambda_min = N(-2, min = -10, max = 10, step = 0.5, label = "λ mínimo"),
        lambda_max = N(2, min = -10, max = 10, step = 0.5, label = "λ máximo"),
        passo = N(0.1, min = 0.001, max = 1, step = 0.05, label = "Passo"),
        confianca = N(0.95, min = 0.5, max = 0.999, step = 0.01, label = "Confiança")),
        .tr_exp_an_props(.aspecto = "4:3")),
      help = .tr_exp_an_ajuda_boxcox()),

    trama::tr_node("experiments/response_surface", version = 2L, fn = tr_experiments_response_surface,
      label = "Superfície de resposta", category = "exp_analisar",
      # Ajusta o modelo (a porta `modelo` é um models/fit): papel de ajuste, e não
      # a cor clara de leitura dos outros dois, que leem um ajuste pronto.
      role = "ajuste", icon = trama::tr_icon("chart-area"),
      description = "Modelo de 1ª ou 2ª ordem em fatores codificados, análise canônica, falta de ajuste e contorno.",
      inputs = list(dados = T),
      outputs = list(modelo = Fm, quadro = EF, canonica = T, grafico = "view/plot"),
      params = c(list(
        resposta = P("cols", "", label = "Resposta", example = "rendimento"),
        fatores = P("cols", "", label = "Fatores codificados", example = "x1, x2"),
        ordem = E("2", .TR_EXP_AN_ORDENS, label = "Ordem"),
        bloco = P("cols", "", label = "Bloco (opcional)", example = "bloco")),
        .tr_exp_an_props(.aspecto = "1:1")),
      help = .tr_exp_an_ajuda_superficie())
  )
}
