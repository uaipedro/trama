# Regressão logística como CLASSIFICADOR: a irmã da discriminante.
#
# A `models/glm` binomial já ajusta logística — lá ela é modelo de regressão,
# com desvio, contrastes e médias marginais. Aqui ela é regra de classificação
# sobre uma matriz de preditores, com as mesmas recusas da discriminante e os
# mesmos nós de classificar, de matriz de confusão e de ROC. É o que deixa
# comparar LDA, QDA e logística pelo acerto em validação cruzada ligando três
# fios no mesmo tipo de nó.
#
# A diferença de fundo: a LDA supõe preditores normais com covariância comum e
# modela como as MEDIDAS se distribuem em cada grupo; a logística não supõe
# nada sobre as medidas e modela direto a PROBABILIDADE do grupo. Com a
# hipótese da LDA valendo, a LDA é mais eficiente; fora dela, a logística é
# mais robusta.
#
# Dois grupos: `glm` binomial, a referência dos erros padrão de Wald. Três ou
# mais: `nnet::multinom`, que vem com o R como o `MASS`.

#' Preditores renomeados `v1..vp`, prontos para a fórmula.
#' @noRd
.tr_multi_logit_df <- function(X) {
  d <- as.data.frame(unname(X))
  names(d) <- paste0("v", seq_len(ncol(X)))
  d
}

#' A matriz dos preditores de uma tabela, na ordem do modelo.
#' @noRd
.tr_multi_logit_X <- function(modelo, tab) {
  X <- as.matrix(as.data.frame(tab)[, modelo$preditores, drop = FALSE])
  storage.mode(X) <- "double"
  X
}

#' Os avisos de separação do `glm.fit`, no idioma da sessão.
#'
#' O R traduz as mensagens do `stats` (no pt_BR, "glm.fit: algoritmo não
#' convergiu"); comparar com o texto em inglês deixava o aviso vazar. O
#' `gettext` com o domínio "R-stats" devolve a mesma tradução que o `warning`
#' usou, e é recalculado a cada ajuste porque o idioma pode mudar na sessão.
#' @noRd
.tr_multi_avisos_separacao <- function() {
  c(gettext("glm.fit: fitted probabilities numerically 0 or 1 occurred", domain = "R-stats"),
    gettext("glm.fit: algorithm did not converge", domain = "R-stats"))
}

#' O ajuste cru: `glm` binomial ou `multinom`.
#'
#' Os avisos do `glm` de separação ("fitted probabilities numerically 0 or 1",
#' "algorithm did not converge") são CALADOS aqui: a separação é detectada pela
#' regra de `.tr_multi_separados`, que diz QUAL grupo, e o aviso solto no
#' console não chega ao card. O `maxit = 1000` do `multinom` é porque o padrão
#' (100) para antes em dado real com sete preditores. `hess = FALSE` poupa a
#' hessiana nos n reajustes da validação cruzada, que só leem as probabilidades
#' (a hessiana só serve aos erros padrão do `summary`).
#' @noRd
.tr_multi_logit_ajuste <- function(X, g, no, hess = TRUE) {
  d <- .tr_multi_logit_df(X)
  d$.y <- g
  f <- stats::as.formula(paste(".y ~", paste(names(d)[names(d) != ".y"], collapse = " + ")))
  .tr_multi_ajustar(withCallingHandlers(
    if (nlevels(g) == 2L) {
      stats::glm(f, family = stats::binomial(), data = d)
    } else {
      nnet::multinom(f, data = d, trace = FALSE, maxit = 1000L, Hess = hess)
    },
    warning = function(w) {
      if (conditionMessage(w) %in% .tr_multi_avisos_separacao()) invokeRestart("muffleWarning")
    }), no)
}

#' Probabilidades de cada grupo e classe prevista, para a matriz X.
#' @return `list(prob = matriz n × g com colunas nos níveis, classe = fator)`.
#' @noRd
.tr_multi_logit_prever <- function(modelo, X) {
  d <- .tr_multi_logit_df(X)
  niv <- modelo$niveis
  if (identical(modelo$tipo, "binária")) {
    p2 <- unname(stats::predict(modelo$ajuste, newdata = d, type = "response"))
    prob <- cbind(1 - p2, p2)
    classe <- ifelse(p2 >= modelo$corte, niv[[2]], niv[[1]])
  } else {
    prob <- stats::predict(modelo$ajuste, newdata = d, type = "probs")
    # Uma linha só volta como VETOR do `predict.multinom`.
    if (is.null(dim(prob))) prob <- matrix(prob, nrow = 1L)
    classe <- niv[max.col(prob, ties.method = "first")]
  }
  colnames(prob) <- niv
  list(prob = prob, classe = factor(classe, levels = niv))
}

#' Os grupos separados sem sobreposição.
#'
#' Um grupo está separado quando a MENOR probabilidade ajustada de ser dele,
#' entre os seus casos, passa da MAIOR entre os casos de fora: existe um corte
#' que o isola sem erro, e a verossimilhança cresce sem limite empurrando os
#' coeficientes para o infinito. O `glm` só avisa; o `multinom` nem isso — na
#' `iris` ele devolve coeficientes de dezenas com erro padrão de centenas.
#' @noRd
.tr_multi_separados <- function(prob, g) {
  niv <- levels(g)
  sep <- vapply(niv, function(l) min(prob[g == l, l]) > max(prob[g != l, l]), TRUE)
  niv[sep]
}

#' Recusa o que depende dos coeficientes quando há separação.
#' @noRd
.tr_multi_sem_separacao <- function(modelo, no) {
  if (length(modelo$separacao)) {
    # Na binária os dois grupos saem separados juntos (isolar um é isolar o
    # outro): a frase vai para o plural em vez de "o grupo 'a', 'b' é".
    sep <- sprintf("'%s'", modelo$separacao)
    quem <- if (length(sep) == 1L) {
      sprintf("o grupo %s é separado dos outros", sep)
    } else {
      sprintf("os grupos %s e %s são separados", paste(utils::head(sep, -1L), collapse = ", "),
              utils::tail(sep, 1L))
    }
    .tr_multi_abort("tr_multi_error_separation",
                    paste0("'%s': separação completa — %s sem ",
                           "nenhuma sobreposição, e os coeficientes vão ao infinito ",
                           "(o que o otimizador devolve é arbitrário). A CLASSIFICAÇÃO continua ",
                           "valendo (`models/confusion`, `models/roc`); para descrever o que separa, ",
                           "use a `multi/discriminant`, ou tire o preditor que isola os grupos."),
                    no, quem)
  }
  invisible(modelo)
}

#' Regressão logística binária ou multinomial.
#' @param dados tabela.
#' @param resposta coluna com o grupo conhecido (a resposta a classificar).
#' @param preditores colunas preditoras; em branco, todas as numéricas menos a resposta.
#' @param corte na binária, a probabilidade do segundo grupo a partir da qual
#'   se prevê ele.
#' @return modelo `models/fit` de classe `tr_multi_logit`.
#' @export
tr_multi_logistic <- function(dados, resposta = "", preditores = "", corte = 0.5) {
  no <- "multi/logistic"
  corte <- .tr_multi_num(corte, "corte", min = 0.01, max = 0.99)
  gr <- .tr_multi_grupos(dados, resposta, preditores, no, param = "resposta")
  .tr_multi_grupo_minimo(gr$g, 2L, no,
                         "Com uma observação só, o grupo não tem como ter probabilidade estimada.")
  # A matriz dos preditores singular deixa coeficientes NA no `glm` (e em
  # silêncio no `multinom`): a mesma recusa da covariância na discriminante.
  if (.tr_multi_cov_singular(stats::cov(gr$X))) {
    .tr_multi_abort("tr_multi_error_singular_matrix",
                    paste0("'%s': os preditores são colineares — algum é combinação exata dos ",
                           "outros, e o coeficiente dele não se identifica. Tire o redundante."), no)
  }
  ajuste <- .tr_multi_logit_ajuste(gr$X, gr$g, no)
  tipo <- if (nlevels(gr$g) == 2L) "binária" else "multinomial"
  m <- .tr_multi_logit_obj(ajuste, tipo, gr$grupo, gr$preditores, levels(gr$g),
                           tibble::as_tibble(dados), if (tipo == "binária") corte else NA_real_,
                           character())
  m$separacao <- .tr_multi_separados(.tr_multi_logit_prever(m, gr$X)$prob, gr$g)
  m
}

.TR_MULTI_ESCALAS_OR <- c("unidade", "desvio padrão")

#' Coeficientes em formato longo, na escala pedida, sem IC.
#'
#' Base comum da tabela, do gráfico e do jackknife. "desvio padrão" multiplica
#' coeficiente e erro padrão pelo DP do preditor na tabela de treino: a razão
#' de chances vira "por um desvio padrão a mais", comparável entre medidas de
#' unidades diferentes (glicose em mg/dL, pedigree em fração). z e p não mudam.
#' @noRd
.tr_multi_logit_coefs <- function(modelo, escala = "unidade") {
  termos <- c("(intercepto)", modelo$preditores)
  if (identical(modelo$tipo, "binária")) {
    cs <- stats::coef(summary(modelo$ajuste))
    b <- matrix(cs[, 1], nrow = 1L, dimnames = list(modelo$niveis[[2]], termos))
    se <- matrix(cs[, 2], nrow = 1L, dimnames = dimnames(b))
  } else {
    s <- summary(modelo$ajuste)
    # Com três ou mais grupos o `multinom` sempre devolve matrizes (a binária
    # não passa por aqui).
    b <- s$coefficients; se <- s$standard.errors
    dimnames(b) <- dimnames(se) <- list(modelo$niveis[-1L], termos)
  }
  if (identical(escala, "desvio padrão")) {
    X <- .tr_multi_logit_X(modelo, modelo$dados)
    dp <- c(1, apply(X, 2L, stats::sd))
    b <- sweep(b, 2L, dp, "*"); se <- sweep(se, 2L, dp, "*")
  }
  # Linhas agrupadas por grupo (todos os termos de B, depois os de C), com os
  # termos na ordem dos preditores: `t()` vira a leitura por linha da matriz.
  data.frame(grupo = rep(rownames(b), each = ncol(b)),
             referencia = modelo$niveis[[1]],
             termo = rep(termos, times = nrow(b)),
             coeficiente = as.vector(t(b)), erro_padrao = as.vector(t(se)),
             stringsAsFactors = FALSE)
}

#' As razões de chances com o intervalo de Wald, para o gráfico.
#'
#' A tabela que o leitor vê sai da `models/coefficients` (o método
#' `tr_models_coefs` em `R/contrato.R`, na forma de toda a coleção de modelos);
#' esta é a do GRÁFICO, que precisa do intervalo já exponenciado e do grupo.
#' @noRd
.tr_multi_logit_or <- function(modelo, escala = "unidade", confianca = 0.95) {
  d <- .tr_multi_logit_coefs(modelo, escala)
  q <- stats::qnorm((1 + confianca) / 2)
  tibble::tibble(grupo = d$grupo, termo = d$termo, razao_chances = exp(d$coeficiente),
                 ic_inf = exp(d$coeficiente - q * d$erro_padrao),
                 ic_sup = exp(d$coeficiente + q * d$erro_padrao))
}

#' As razões de chances com intervalo, em escala log.
#' @param modelo uma regressão logística (`multi/logistic`).
#' @param escala `"desvio padrão"` (o padrão aqui: compara medidas) ou `"unidade"`.
#' @inheritParams trama.view::tr_view_finish
#' @return ggplot.
#' @export
tr_multi_plot_odds <- function(modelo, escala = "desvio padrão", aspecto = "16:9", tema = "padrão",
                               titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  no <- "multi/plot_odds"
  .tr_multi_exigir(modelo, "logit", no)
  escala <- .tr_multi_enum(escala, .TR_MULTI_ESCALAS_OR, "escala")
  .tr_multi_sem_separacao(modelo, no)
  tab <- .tr_multi_logit_or(modelo, escala = escala)
  tab <- tab[tab$termo != "(intercepto)", ]
  tab$termo <- factor(tab$termo, levels = rev(modelo$preditores))
  tab$sinal <- ifelse(tab$ic_inf > 1, "aumenta", ifelse(tab$ic_sup < 1, "diminui", "inclui 1"))
  # `geom_errorbarh` está deprecado no ggplot2 4: a barra horizontal é a
  # `geom_errorbar` com `orientation = "y"`.
  p <- ggplot2::ggplot(tab, ggplot2::aes(x = .data[["razao_chances"]], y = .data[["termo"]],
                                         colour = .data[["sinal"]])) +
    ggplot2::geom_vline(xintercept = 1, colour = .TR_MULTI_CINZA, linetype = "dashed") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data[["ic_inf"]], xmax = .data[["ic_sup"]]),
                           width = .2, linewidth = .6, orientation = "y") +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_colour_manual(values = c(aumenta = .TR_MULTI_COR_2, diminui = .TR_MULTI_COR,
                                            `inclui 1` = .TR_MULTI_CINZA), name = "IC 95%") +
    ggplot2::labs(x = sprintf("razão de chances (por %s, escala log)",
                              if (escala == "unidade") "unidade" else "desvio padrão"),
                  y = NULL,
                  subtitle = sprintf("Chance de %s contra %s",
                                     if (identical(modelo$tipo, "binária")) sprintf("'%s'", modelo$niveis[[2]])
                                     else "cada grupo", sprintf("'%s'", modelo$niveis[[1]])))
  if (identical(modelo$tipo, "multinomial")) p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[["grupo"]]))
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

# ---------------------------------------------------------------------------
# Declarações

.tr_multi_nos_logistica <- function() {
  P <- trama::tr_param
  TB <- "data/table"
  LG <- "models/fit"
  list(
    trama::tr_node("multi/logistic", fn = tr_multi_logistic, label = "Regressão logística",
      category = "multi_logistica", icon = trama::tr_icon("chart-spline"),
      description = "Ajusta uma regressão logística binária (2 grupos) ou multinomial (3+) para classificar grupos conhecidos.",
      inputs = list(dados = TB), outputs = list(out = LG),
      params = list(
        resposta = P("cols", "", label = "Resposta (grupo)", example = "diabetes"),
        preditores = P("cols", "", label = "Preditores", example = "glicose, imc, idade"),
        corte = trama::tr_param_num(0.5, min = 0.01, max = 0.99, label = "Corte (binária)")),
      help = .tr_multi_ajuda(r"---[
Modela a PROBABILIDADE de cada observação pertencer a cada grupo a partir das
medidas, e classifica pelo grupo mais provável. É a irmã da
`multi/discriminant`: recebe a mesma tabela, com as mesmas recusas, e o modelo
entra nos mesmos blocos de previsão da coleção de modelos (`models/predict`,
`models/confusion`, `models/roc`).

### Binária ou multinomial

- **Dois grupos** — `glm` binomial: log(p / (1 − p)) = b₀ + b₁x₁ + ... + bₚxₚ,
  com p a probabilidade do SEGUNDO grupo (na ordem dos níveis; o primeiro é a
  referência). A classe prevista é o segundo grupo quando p ≥ **corte**.
- **Três ou mais** — multinomial (`nnet::multinom`): um conjunto de
  coeficientes por grupo, cada um comparado à referência; prevê o grupo de
  maior probabilidade, e o corte não se aplica.

### Logística ou discriminante

A LDA supõe medidas normais com a mesma covariância em todo grupo; quando isso
vale, ela aproveita melhor os dados. A logística não supõe nada sobre as
medidas (serve com preditor assimétrico, contagem ou 0/1) e costuma ganhar
fora dessa hipótese. A decisão prática é a mesma de sempre: compare as taxas
de acerto CRUZADAS em `models/confusion`. No `pima` as duas empatam perto de 78%.

### O corte

Com 0,5 a regra minimiza o erro total. Quando errar um diabético custa mais que
alarmar um saudável, baixe o corte: a sensibilidade sobe, a especificidade
cai. A `models/roc` mostra essa troca inteira e marca o corte escolhido.

### Separação

Se um grupo é separável dos outros SEM nenhuma sobreposição (setosa na `iris`,
o cultivar C nos `vinhos` com as seis medidas), a verossimilhança cresce sem
limite e os coeficientes vão ao infinito. A classificação continua certa, e o
modelo sai; o card avisa em `separacao`, e os nós que leem coeficientes
(`models/coefficients`, `multi/plot_odds`, `multi/jackknife_logistic`)
recusam. É sinal de que a pergunta "o que separa" é mais bem respondida pela
discriminante.

### Recusas

Grupo com faltante, grupo único, grupo com uma observação, menos de dois
preditores numéricos, preditor não numérico, constante ou colinear.
]---", r"---[
- **Resposta** (`resposta`) — a coluna com o grupo conhecido. Os níveis sem linha são
  descartados; o primeiro nível é a referência.
- **Preditores** (`preditores`) — as medidas, separadas por vírgula. Em branco, todas as
  numéricas menos o grupo.
- **Corte (binária)** — probabilidade do segundo grupo a partir da qual se
  prevê ele. Ignorado com três ou mais grupos.
]---", r"---[
Um modelo (`models/fit`). O card mostra as razões de chances (ou a ROC por
resubstituição, se há separação). Ligado a um nó de tabela, vira o treino
classificado (como o `models/predict`).
]---", r"---[
tr_flow(reg) |>
  tr_add("pima", "multi/example", dataset = "pima") |>
  tr_add("lg", "multi/logistic", resposta = "diabetes", from = "pima") |>
  tr_add("cv", "models/confusion", validacao = "cruzada", from = "lg")
]---", r"---[
`models/coefficients` (com `exponenciar`) para as razões de chances;
`models/roc` para escolher o corte; `multi/discriminant` para a comparação; models/glm para a
logística como modelo de regressão, com desvio e contrastes.
]---")),

    trama::tr_node("multi/plot_odds", role = "leitura", fn = tr_multi_plot_odds, label = "Gráfico das razões de chances",
      category = "multi_logistica", icon = trama::tr_icon("chart-bar"),
      description = "Razões de chances de cada preditor com intervalo de 95%, em escala log.",
      inputs = list(modelo = LG), outputs = list(out = "view/plot"),
      params = .tr_multi_props(
        escala = trama::tr_param_enum("desvio padrão", .TR_MULTI_ESCALAS_OR, label = "Escala"),
        .aspecto = "16:9"),
      help = .tr_multi_ajuda(r"---[
Um ponto por preditor na razão de chances, com o intervalo de 95%, em eixo LOG
(dobrar e reduzir à metade ficam à mesma distância do 1). A linha tracejada é
o 1: intervalo que a cruza é efeito não distinguível de zero, e fica cinza;
acima de 1, aumenta a chance; abaixo, diminui.

O padrão é a escala por **desvio padrão**, que é a que deixa comparar as
barras entre si. Na multinomial, um painel por grupo contra a referência. Com
separação, o gráfico é recusado (veja `multi/logistic`).
]---", r"---[
- **Escala** — `desvio padrão` (padrão) ou `unidade`.
]---", r"---[
Um gráfico (`view/plot`). É também o card de toda logística sem separação.
]---", r"---[
tr_flow(reg) |>
  tr_add("v", "multi/example", dataset = "vinhos") |>
  tr_add("lg", "multi/logistic", resposta = "cultivar", preditores = "alcool, acidez_malica, magnesio, fenois_totais", from = "v") |>
  tr_add("g", "multi/plot_odds", from = "lg")
]---", r"---[
`models/coefficients` para os números; `models/roc` para o desempenho.
]---", grafico = TRUE))
  )
}
