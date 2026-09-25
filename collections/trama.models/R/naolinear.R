# Não linear: as curvas prontas de crescimento, absorção e resposta a dose.
#
# Cinco modelos, e não uma fórmula livre: o que trava um `nls` na prática é o
# CHUTE inicial, e os quatro primeiros têm self-starter no `stats` (a curva se
# chuta sozinha a partir dos dados). O linear-platô não tem, e o chute sai de
# uma busca pelo ponto de quebra (`.tr_models_chute_plato`). Quem precisa de
# outra forma escreve a fórmula num `models/lm` linearizado, ou fora daqui.
#
# O `grupo` (uma curva por nível) ficou de fora: vira N ajustes com N
# convergências possíveis de falhar em separado, e comparar curvas pede os
# testes de parâmetros comuns — um bloco de outro tamanho.

.TR_MODELS_NLS <- c("logístico", "Michaelis-Menten", "exponencial assintótico", "Gompertz", "linear-platô")

#' O chute do linear-platô: a quebra que deixa a menor soma de quadrados.
#'
#' Para cada x candidato a ponto de quebra (deixando ao menos três pontos na
#' rampa e um no platô), a reta `y ~ pmin(x, x0)` é linear nos parâmetros e se
#' ajusta direto; a melhor dá a, b e x0 para o `nls` refinar.
#' @noRd
.tr_models_chute_plato <- function(x, y) {
  u <- sort(unique(x))
  cands <- u[u >= u[min(3L, length(u))] & u < max(u)]
  if (!length(cands)) cands <- stats::median(u)
  sqr <- vapply(cands, function(c) sum(stats::lm.fit(cbind(1, pmin(x, c)), y)$residuals^2), 0)
  x0 <- cands[[which.min(sqr)]]
  b <- stats::lm.fit(cbind(1, pmin(x, x0)), y)$coefficients
  list(a = unname(b[[1]]), b = unname(b[[2]]), x0 = x0)
}

#' Chute pela forma da curva, para quando o self-starter falha.
#'
#' Grosso de propósito: o `nls` só precisa começar no vale certo. Assíntota no
#' maior y, meio da curva no x cujo y está mais perto da metade dela, e a
#' escala uma fração da amplitude de x.
#' @noRd
.tr_models_chute_forma <- function(modelo, x, y) {
  topo <- max(y)
  meio <- x[[which.min(abs(y - topo / 2))]]
  amp <- diff(range(x))
  y0 <- mean(y[x == min(x)])
  switch(modelo,
    `logístico` = list(Asym = topo, xmid = meio, scal = amp / 10),
    `Michaelis-Menten` = list(Vm = topo, K = max(meio, amp / 20)),
    `exponencial assintótico` = list(Asym = mean(y[x == max(x)]), R0 = y0, lrc = log(3 / amp)),
    Gompertz = list(Asym = topo, b2 = if (y0 > 0) log(topo / y0) else 5, b3 = exp(-5 / amp)),
    `linear-platô` = .tr_models_chute_plato(x, y))
}

#' A fórmula do `nls` de cada modelo.
#' @noRd
.tr_models_nls_formula <- function(modelo, Y, X) {
  rhs <- switch(modelo,
    `logístico` = sprintf("SSlogis(%s, Asym, xmid, scal)", X),
    `Michaelis-Menten` = sprintf("SSmicmen(%s, Vm, K)", X),
    `exponencial assintótico` = sprintf("SSasymp(%s, Asym, R0, lrc)", X),
    Gompertz = sprintf("SSgompertz(%s, Asym, b2, b3)", X),
    `linear-platô` = sprintf("a + b * pmin(%s, x0)", X))
  f <- stats::as.formula(paste(Y, "~", rhs))
  environment(f) <- globalenv()
  f
}

#' Regressão não linear com modelos prontos.
#'
#' @param dados tabela.
#' @param resposta coluna da resposta.
#' @param preditor a coluna numérica da preditora (uma só).
#' @param modelo `"logístico"`, `"Michaelis-Menten"`, `"exponencial
#'   assintótico"`, `"Gompertz"` ou `"linear-platô"`.
#' @return objeto `tr_models_fit` de classe `tr_models_nls`.
#' @export
tr_models_nls <- function(dados, resposta = "", preditor = "", modelo = "logístico") {
  no <- "models/nls"
  modelo <- .tr_models_enum(modelo, .TR_MODELS_NLS, "modelo")
  resp <- .tr_models_col(dados, resposta, "resposta")
  x <- .tr_models_col(dados, preditor, "preditor")
  .tr_models_numerica(dados, resp, "resposta")
  .tr_models_numerica(dados, x, "preditor")
  npar <- if (modelo == "Michaelis-Menten") 2L else 3L
  p <- .tr_models_preparar(dados, c(resp, x), no, min_linhas = npar + 2L)
  d <- as.data.frame(p$dados)
  if (length(unique(d[[x]])) < npar + 1L) {
    .tr_models_abort("tr_models_error_too_few_rows",
                     "'%s': o modelo %s tem %d parâmetros e pede pelo menos %d valores distintos de '%s'; há %d.",
                     no, modelo, npar, npar + 1L, x, length(unique(d[[x]])))
  }
  f <- .tr_models_nls_formula(modelo, .tr_models_bt(resp), .tr_models_bt(x))
  inicio <- if (modelo == "linear-platô") .tr_models_chute_plato(d[[x]], d[[resp]]) else NULL
  ajustar <- function(ini) {
    # `start` só quando há chute: passado como NULL, o `nls` não o trata como
    # ausente e não chama o self-starter.
    suppressWarnings(do.call(stats::nls, c(list(f, data = d, control = stats::nls.control(maxiter = 200L)),
                                           if (!is.null(ini)) list(start = ini))))
  }
  ajuste <- tryCatch(ajustar(inicio), error = function(e) {
    # O self-starter falha em casos comuns de campo — o do Michaelis-Menten
    # lineariza por 1/x e quebra com a dose zero —, e aí o chute sai da forma
    # da curva (`.tr_models_chute_forma`). Só se esse também falha, o card é
    # vermelho.
    tryCatch(ajustar(.tr_models_chute_forma(modelo, d[[x]], d[[resp]])), error = function(e2) {
      .tr_models_abort("tr_models_error_no_convergence",
                       paste0("'%s': o ajuste %s não convergiu (%s). Os dados precisam ter a forma da curva ",
                              "— %s — com pontos dos dois lados da mudança; confira o gráfico de ",
                              "dispersão, ou tente outro modelo."),
                       no, modelo, trimws(conditionMessage(e)),
                       switch(modelo, `logístico` = "um S que sobe e se estabiliza",
                              `Michaelis-Menten` = "uma subida que satura a partir de zero",
                              `exponencial assintótico` = "uma curva que se aproxima de uma assíntota",
                              Gompertz = "um S assimétrico",
                              `linear-platô` = "uma reta que vira platô"),
                       parent = e)
    })
  })
  fit <- .tr_models_fit_obj(ajuste, "nls", sprintf("Não linear · %s", modelo), f, p$dados, resp,
                            descartadas = p$descartadas)
  fit$preditor <- x
  fit$modelo <- modelo
  fit
}

# ---- Contrato da classe tr_models_nls ------------------------------------------------

#' @export
tr_models_info.tr_models_nls <- function(x) {
  list(tarefa = "regressao", resposta = x$resposta, preditores = x$preditor, niveis = NULL,
       n = nrow(x$dados), rotulo = x$rotulo, familia = "gaussian")
}
#' @export
tr_models_predict_raw.tr_models_nls <- function(x, novos, ...) {
  .tr_models_prev(as.numeric(stats::predict(x$ajuste, newdata = as.data.frame(novos))))
}
#' A cruzada reajusta sem cada linha, partindo dos parâmetros do ajuste
#' completo (que já estão perto); linha cujo reajuste não converge sai NA.
#' @export
tr_models_predict_cv.tr_models_nls <- function(x, validacao = "resubstituição") {
  if (.tr_models_validacao(validacao) == "resubstituição") return(.tr_models_prev(as.numeric(stats::fitted(x$ajuste))))
  d <- as.data.frame(x$dados)
  f <- stats::formula(x$ajuste)
  ini <- as.list(stats::coef(x$ajuste))
  .tr_models_prev(vapply(seq_len(nrow(d)), function(i) {
    tryCatch({
      m <- suppressWarnings(stats::nls(f, data = d[-i, , drop = FALSE], start = ini))
      as.numeric(stats::predict(m, newdata = d[i, , drop = FALSE]))
    }, error = function(e) NA_real_)
  }, numeric(1)))
}
#' Coeficientes com IC de Wald (t com os gl do resíduo): o perfil de
#' verossimilhança seria mais honesto na assimetria, mas falha justo nos
#' ajustes difíceis, e um card que ora tem IC ora não confunde mais.
#' @export
tr_models_coefs.tr_models_nls <- function(x, exponenciar = FALSE, escala = "unidade", confianca = 0.95, ...) {
  if (isTRUE(exponenciar)) {
    .tr_models_exigir(x, "glm", "models/coefficients", "Exponenciar só faz sentido num GLM com ligação log ou logit.")
  }
  if (!identical(escala, "unidade")) {
    .tr_models_abort("tr_models_error_not_applicable",
                     "'models/coefficients': num modelo não linear os parâmetros não são inclinações, e a escala por desvio padrão não se aplica.")
  }
  confianca <- .tr_models_num(confianca, "confianca", min = 0.5, max = 0.999)
  s <- stats::coef(summary(x$ajuste))
  tq <- stats::qt(1 - (1 - confianca) / 2, stats::df.residual(x$ajuste))
  tab <- data.frame(termo = rownames(s), estimativa = s[, 1], erro_padrao = s[, 2], t = s[, 3], p_valor = s[, 4],
                    li = s[, 1] - tq * s[, 2], ls = s[, 1] + tq * s[, 2])
  .tr_models_coefs_efeitos(x, .tr_models_coefs_ic_nomes(tab, confianca), "t",
                           sprintf("intervalo de Wald; parâmetros do modelo %s", x$modelo))
}
#' R² como 1 − SQres/SQtotal (pseudo: fora do linear ele não é a fração
#' explicada nem fica entre 0 e 1 garantido) e a raiz do erro quadrático médio.
#' @export
tr_models_stats.tr_models_nls <- function(x) {
  aj <- x$ajuste
  y <- x$dados[[x$resposta]]
  sqr <- sum(stats::residuals(aj)^2)
  linha <- .tr_models_stats_linha(x, aj, r2 = 1 - sqr / sum((y - mean(y))^2), sig = stats::sigma(aj))
  linha$rmse <- sqrt(sqr / length(y))
  linha
}
#' @export
tr_models_resid.tr_models_nls <- function(x) {
  .tr_models_resid_tabela(x, x$ajuste, stats::residuals(x$ajuste) / stats::sigma(x$ajuste))
}
#' @export
tr_models_importance.tr_models_nls <- function(x) {
  .tr_models_abort("tr_models_error_not_applicable",
                   "'models/importance' não se aplica a %s: há uma preditora só. Leia os parâmetros.", x$rotulo)
}
#' @export
tr_models_card.tr_models_nls <- function(x, ctx) trama.view::tr_view_render(tr_models_plot_regression(x), ctx)
#' @export
tr_models_as_table.tr_models_nls <- function(x) tr_models_coefficients(x)$tabela
