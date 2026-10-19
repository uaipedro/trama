# O modelo reduzido para publicação: `tr_models_strip()` (contrato.R).
#
# Publicar o `.rds` de um ajuste como ele sai do `lm()` publica os dados de
# treino junto: `model` é a tabela, `residuals`, `fitted.values` e `effects`
# têm uma posição por linha e `qr$qr` é a própria matriz de delineamento. O
# `terms` carrega o ambiente onde a fórmula nasceu: nos ajustes do trama ele
# já é o global, mas um `lm` feito dentro de uma função leva o quadro dela,
# tabela inclusive — o reduzido troca pelo global sempre. Guarda só o que o
# `predict()` do stats lê com `newdata`: coeficientes, `terms` (com as
# `predvars` de `poly()`/`scale()` e as classes das colunas), `xlevels`,
# `contrasts`, o posto e a pivotagem do QR e, no GLM, a família.

#' Reduz um `lm`/`glm` do stats ao que prevê com `newdata`
#'
#' Fica o que `predict.lm()`/`predict.glm()` leem para o ponto previsto
#' (`type = "response"` ou `"link"`): `coefficients`, `rank`, `qr` só com
#' `pivot` e `rank`, `terms` com ambiente global, `xlevels`, `contrasts`,
#' `family` e, na binomial negativa do MASS, `theta`. Sai todo campo com uma
#' posição por linha do treino (`model`, `residuals`, `fitted.values`,
#' `effects`, `weights`, `prior.weights`, `y`, `linear.predictors`, `offset`,
#' `na.action`), a matriz `qr$qr`, o `R` do GLM, a tabela `data` e a chamada
#' (que, num `do.call`, levaria a própria tabela). Intervalo de confiança ou de
#' predição e `se.fit` deixam de funcionar: pedem os resíduos.
#'
#' Exportado para as coleções irmãs que ajustam `lm`/`glm` por dentro (a
#' referência linear da `trama.ml`) reduzirem igual.
#' @param aj Um objeto de classe `lm` (inclui `glm` e `negbin`).
#' @return O mesmo objeto, só com os campos acima.
#' @export
tr_models_strip_stats <- function(aj) {
  if (!inherits(aj, "lm")) {
    .tr_models_abort("tr_models_error_no_method",
                     "tr_models_strip_stats() reduz um lm ou glm do stats; recebeu um de classe '%s'.",
                     class(aj)[[1]])
  }
  # O posto deficiente faz o `predict.lm()` recente procurar a base do espaço
  # nulo em `qr.R()`, que pede a matriz inteira: melhor recusar aqui que
  # publicar um modelo que só falha no navegador.
  if (aj$rank < length(stats::coef(aj))) {
    .tr_models_abort("tr_models_error_not_applicable",
                     "O ajuste tem posto deficiente (coeficientes NA): reduza a fórmula antes de publicar.")
  }
  manter <- c("coefficients", "rank", "terms", "xlevels", "contrasts", "family",
              "df.residual", "theta")
  out <- unclass(aj)[intersect(manter, names(aj))]
  out$qr <- structure(list(pivot = aj$qr$pivot, rank = aj$qr$rank), class = "qr")
  tt <- out$terms
  # Global, e não `baseenv()`: `poly()`, `ns()` e companhia são procurados no
  # ambiente da fórmula na hora de prever, e o global enxerga os pacotes
  # anexados. Serializado, o global é só uma referência — não leva nada.
  attr(tt, ".Environment") <- globalenv()
  out$terms <- tt
  # `offset = ` passado como argumento (e não na fórmula) é relido da chamada
  # pelo `predict.lm()`: fica só ele, como expressão sobre `newdata`.
  if (!is.null(aj$call$offset)) out$call <- as.call(list(as.name("lm"), offset = aj$call$offset))
  class(out) <- class(aj)
  out
}

#' @export
tr_models_strip.tr_models_lm <- function(x) .tr_models_strip_proprio(x)
#' @export
tr_models_strip.tr_models_glm <- function(x) .tr_models_strip_proprio(x)

#' O `tr_models_fit` reduzido: sem `dados` nem os auxiliares, com o que
#' `tr_models_info()` lia da tabela guardado antes (`n` e, no GLM binomial de
#' resposta binária, os níveis).
#' @noRd
.tr_models_strip_proprio <- function(x) {
  if (isTRUE(x$reduzido)) return(x)
  # `x["campo"] <- list(v)` e não `x$campo <- v`: NULL apagaria o campo, e o
  # NULL dos níveis é resposta ("é regressão"), não ausência.
  x["niveis_resposta"] <- list(.tr_models_glm_niveis(x))
  x$n <- nrow(x$dados)
  x$ajuste <- tr_models_strip_stats(x$ajuste)
  x["dados"] <- list(NULL)
  x["aux_lm"] <- list(NULL)
  x["aux_misto"] <- list(NULL)
  x$reduzido <- TRUE
  x
}

# ---- bloco web: o que a página precisa para prever no navegador ----------------

#' O modelo descrito para prever no navegador (WebR)
#'
#' Opcional no contrato: o default é `NULL`, "não ganha preditor na página" — o
#' modelo vai no pacote, mas a página não o usa. Quem implementa devolve, a
#' partir do modelo COMPLETO (as faixas das entradas saem do treino):
#'
#' - `motor`: o objeto que o WebR lê com `readRDS()` e entrega a `prever` —
#'   o ajuste do motor já reduzido por [tr_models_strip()], sem classe do trama
#'   (o navegador não tem o trama).
#' - `pacotes`: lista nomeada `pacote = versão do treino` do que `prever` usa.
#' - `entradas`: lista, uma por coluna de `novos`, com `nome`, `tipo`
#'   (`"numero"`, `"fator"` ou `"logico"`) e `niveis`, ou `min`, `max`,
#'   `passo` e `padrao` (faixa arredondada por `pretty()`, sem valor do treino).
#' - `saida`: `tarefa`, `resposta`, `niveis`, `colunas` (as que `prever` devolve).
#' - `preprocessamento`: texto, o que acontece entre `novos` e o motor.
#' - `prever`: código R de `function(motor, novos)` que devolve um data.frame
#'   com `previsto` e, na classificação, `prob_<nivel>` — só R base e `pacotes`.
#' @param x Um `models/fit`.
#' @return Lista acima, ou `NULL`.
#' @export
tr_models_web <- function(x) UseMethod("tr_models_web")
#' @export
tr_models_web.default <- function(x) NULL

#' As entradas do formulário de previsão, pelas colunas do treino.
#'
#' Faixa por `pretty()` do intervalo observado e padrão no meio dela, num
#' múltiplo do passo: o formulário cobre o treino sem expor o mínimo, o máximo
#' ou a mediana de ninguém. Fator e texto viram a lista de níveis.
#' @param dados A tabela do treino.
#' @param preditores Nomes das colunas.
#' @param niveis Lista nomeada de níveis já conhecidos (o `xlevels` do ajuste).
#' @return Lista de entradas, no formato de [tr_models_web()].
#' @export
tr_models_web_entradas <- function(dados, preditores, niveis = list()) {
  lapply(preditores, function(nm) {
    v <- dados[[nm]]
    if (!is.null(niveis[[nm]]) || is.factor(v) || is.character(v)) {
      niv <- niveis[[nm]] %||% (if (is.factor(v)) levels(v) else sort(unique(stats::na.omit(v))))
      return(list(nome = nm, tipo = "fator", niveis = as.character(niv), padrao = as.character(niv[[1]])))
    }
    if (is.logical(v)) return(list(nome = nm, tipo = "logico", padrao = FALSE))
    p <- pretty(range(v, na.rm = TRUE))
    passo <- diff(p)[[1]] / 10
    meio <- round(mean(range(p)) / passo) * passo
    list(nome = nm, tipo = "numero", min = min(p), max = max(p), passo = passo, padrao = meio)
  })
}

#' Versão instalada de cada pacote, como lista nomeada.
#' @noRd
.tr_models_versoes <- function(pacotes) {
  stats::setNames(lapply(pacotes, function(p) as.character(utils::packageVersion(p))), pacotes)
}

#' A saída de classificação binária em código R literal para `prever`.
#' @noRd
.tr_models_web_binaria <- function(niveis, corte, expr_p) {
  cols <- .tr_models_colunas_prob(niveis)
  paste0("function(motor, novos) {\n",
         "  p <- as.numeric(", expr_p, ")\n",
         "  out <- data.frame(previsto = ifelse(p >= ", format(corte, digits = 17), ", ",
         deparse(niveis[[2]]), ", ", deparse(niveis[[1]]), "))\n",
         "  out[[", deparse(cols[[1]]), "]] <- 1 - p\n",
         "  out[[", deparse(cols[[2]]), "]] <- p\n",
         "  out\n}")
}

#' @export
tr_models_web.tr_models_lm <- function(x) .tr_models_web_proprio(x)
#' @export
tr_models_web.tr_models_glm <- function(x) .tr_models_web_proprio(x)

.tr_models_web_proprio <- function(x) {
  if (isTRUE(x$reduzido)) {
    .tr_models_abort("tr_models_error_not_applicable",
                     "tr_models_web() pede o modelo completo: as faixas das entradas saem do treino.")
  }
  info <- tr_models_info(x)
  r <- tr_models_strip(x)
  glm <- inherits(x, "tr_models_glm")
  prever <- if (!is.null(info$niveis)) {
    .tr_models_web_binaria(info$niveis, x$corte %||% 0.5, "predict(motor, newdata = novos, type = \"response\")")
  } else if (glm) {
    "function(motor, novos) data.frame(previsto = as.numeric(predict(motor, newdata = novos, type = \"response\")))"
  } else {
    "function(motor, novos) data.frame(previsto = as.numeric(predict(motor, newdata = novos)))"
  }
  list(motor = r$ajuste,
       pacotes = .tr_models_versoes(if (inherits(r$ajuste, "negbin")) c("stats", "MASS") else "stats"),
       entradas = tr_models_web_entradas(x$dados, info$preditores, r$ajuste$xlevels %||% list()),
       saida = list(tarefa = info$tarefa, resposta = info$resposta, niveis = info$niveis,
                    colunas = c("previsto", if (!is.null(info$niveis)) .tr_models_colunas_prob(info$niveis))),
       preprocessamento = paste0("Nenhum fora do modelo: a fórmula (", x$formula,
                                 ") é aplicada pelo predict() do stats",
                                 if (glm) ", na escala da resposta." else "."),
       prever = prever)
}
