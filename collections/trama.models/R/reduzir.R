# O modelo reduzido para publicação: `tr_models_strip()` (contrato.R).
#
# Publicar o `.rds` de um ajuste como ele sai do `lm()` publica os dados de
# treino junto: `model` é a tabela, `residuals`, `fitted.values` e `effects`
# têm uma posição por linha, `qr$qr` é a própria matriz de delineamento e o
# `terms` carrega o ambiente onde a fórmula nasceu — no trama, o quadro da
# função que ajustou, com `dados` dentro. O reduzido guarda só o que o
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
