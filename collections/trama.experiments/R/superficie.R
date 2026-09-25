# Superfície de resposta: o modelo de 1ª ou 2ª ordem em fatores codificados,
# a análise canônica e a falta de ajuste.
#
# O modelo sai como `models/fit` comum, ajustado pelo próprio `tr_models_lm`
# com a fórmula escrita aqui: não há tipo de modelo novo, e tudo o que lê um
# `models/fit` (resíduos, pressupostos, previsão, coeficientes) funciona nele.
# O que é próprio da superfície — o quadro com primeira ordem, interações,
# quadráticos, falta de ajuste e erro puro, e a análise canônica — sai em
# portas ao lado, calculado a partir desse mesmo ajuste.
#
# Os fatores entram JÁ CODIFICADOS (−1, 0, +1, ±α): a análise canônica e a
# distância do ponto estacionário ao centro só têm leitura na escala codificada.

.TR_EXP_AN_ORDENS <- c("1", "2")

#' Termos da fórmula: primeira ordem, interações duplas e quadráticos puros.
#' @noRd
.tr_exp_an_termos_sup <- function(x, ordem) {
  fo <- x
  twi <- if (ordem == "2" && length(x) > 1L) {
    apply(utils::combn(x, 2), 2, paste, collapse = ":")
  } else character()
  pq <- if (ordem == "2") sprintf("I(%s^2)", x) else character()
  list(fo = fo, twi = twi, pq = pq)
}

#' b e B do modelo ŷ = b0 + x'b + x'Bx, lidos dos coeficientes.
#' @noRd
.tr_exp_an_bB <- function(coefs, x, ordem) {
  k <- length(x)
  b <- stats::setNames(unname(coefs[x]), x)
  B <- matrix(0, k, k, dimnames = list(x, x))
  if (ordem == "2") {
    for (i in seq_len(k)) B[i, i] <- coefs[[sprintf("I(%s^2)", x[[i]])]]
    if (k > 1L) for (i in 1:(k - 1L)) for (j in (i + 1L):k) {
      nm <- paste(x[[i]], x[[j]], sep = ":")
      B[i, j] <- B[j, i] <- coefs[[nm]] / 2
    }
  }
  list(b = b, B = B)
}

#' Falta de ajuste contra o erro puro das repetições.
#'
#' O erro puro vem das observações repetidas no MESMO ponto do delineamento:
#' tipicamente os pontos centrais. Com bloco, é o resíduo de `y ~ bloco + ponto`
#' — o bloco entra aditivo, como no modelo, e os pontos repetidos em blocos
#' diferentes também contam —, que é a conta do `rsm` e de Myers, Montgomery &
#' Anderson-Cook. A falta de ajuste é o resto do resíduo. Sem repetição não há
#' erro puro, e o teste não existe.
#' @noRd
.tr_exp_an_falta_ajuste <- function(aj, d, x, bloco) {
  ponto <- interaction(d[, x, drop = FALSE], drop = TRUE)
  ep <- if (length(bloco)) stats::lm(d[[".y"]] ~ d[[bloco]] + ponto) else stats::lm(d[[".y"]] ~ ponto)
  sq_ep <- sum(stats::resid(ep)^2)
  gl_ep <- stats::df.residual(ep)
  sq_res <- sum(stats::resid(aj)^2); gl_res <- stats::df.residual(aj)
  list(sq_ep = sq_ep, gl_ep = gl_ep, sq_fa = sq_res - sq_ep, gl_fa = gl_res - gl_ep)
}

#' Superfície de resposta de 1ª ou 2ª ordem.
#'
#' @param dados tabela com a resposta e os fatores codificados.
#' @param resposta coluna numérica da resposta.
#' @param fatores de 1 a 6 colunas numéricas, codificadas, separadas por vírgula.
#' @param ordem `"1"` (plano) ou `"2"` (quadrática completa).
#' @param bloco coluna do bloco (opcional): entra como efeito fixo aditivo.
#' @inheritParams trama.view::tr_view_finish
#' @return lista com `modelo` (`tr_models_fit`), `quadro` (`tr_models_effects`,
#'   a ANOVA da superfície), `canonica` (tibble) e `grafico` (contorno, `view/plot`).
#' @export
tr_experiments_response_surface <- function(dados, resposta = "", fatores = "", ordem = "2", bloco = "",
                                            aspecto = "1:1", tema = "padrão", titulo = "", rotulo_x = "",
                                            rotulo_y = "", legenda = "direita") {
  no <- "experiments/response_surface"
  d <- as.data.frame(dados)
  ordem <- as.character(ordem)
  if (!ordem %in% .TR_EXP_AN_ORDENS) {
    .tr_experiments_abort("tr_experiments_error_bad_option", "'%s': 'ordem' é 1 ou 2.", no)
  }
  if (!.tr_exp_preenchido(resposta)) {
    .tr_experiments_abort("tr_experiments_error_bad_option", "'%s': diga a coluna da 'resposta'.", no)
  }
  resp <- .tr_exp_an_cols(d, resposta, "resposta", no)
  x <- .tr_exp_an_cols(d, fatores, "fatores", no)
  blc <- if (.tr_exp_preenchido(bloco)) .tr_exp_an_cols(d, bloco, "bloco", no) else character()
  if (length(x) < 1L || length(x) > 6L) {
    .tr_experiments_abort("tr_experiments_error_bad_option", "'%s': de 1 a 6 fatores codificados em 'fatores'.", no)
  }
  for (v in c(resp, x)) {
    if (!is.numeric(d[[v]])) {
      .tr_experiments_abort("tr_experiments_error_not_numeric",
                       "'%s': '%s' tem de ser numérica (fatores codificados: −1, 0, +1, ±α).", no, v)
    }
  }
  if (any(make.names(c(resp, x)) != c(resp, x))) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                     "'%s': use nomes simples (letras, números, ponto, _) na resposta e nos fatores.", no)
  }
  if (length(blc)) d[[blc]] <- factor(d[[blc]])
  d <- d[stats::complete.cases(d[, c(resp, x, blc), drop = FALSE]), , drop = FALSE]
  if (max(abs(unlist(d[, x]))) > 10) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                     "'%s': os fatores parecem estar na unidade original (|x| > 10). Codifique: x = (valor − centro) / meia-amplitude.", no)
  }

  t <- .tr_exp_an_termos_sup(x, ordem)
  rhs <- c(if (length(blc)) paste0("`", blc, "`"), t$fo, t$twi, t$pq)
  np <- 1L + length(rhs) - length(blc) + if (length(blc)) nlevels(d[[blc]]) - 1L else 0L
  if (nrow(d) <= np) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                     "'%s': o modelo de %sª ordem tem %d parâmetros e há %d observações completas.", no, ordem, np, nrow(d))
  }
  fit <- trama.models::tr_models_lm(d, formula = paste(resp, "~", paste(rhs, collapse = " + ")))
  fit$rotulo <- sprintf("Superfície de resposta · %sª ordem", ordem)
  aj <- fit$ajuste
  cf <- stats::coef(aj)
  if (anyNA(cf)) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                     "'%s': o delineamento não estima todos os termos (%s); faltam pontos (axiais ou centrais).",
                     no, paste(names(cf)[is.na(cf)], collapse = ", "))
  }

  # Quadro: SQ sequenciais agrupados na ordem bloco → 1ª ordem → interações →
  # quadráticos, como no quadro dos livros (e do `rsm`).
  a <- as.data.frame(stats::anova(aj))
  nomes <- trimws(rownames(a))
  grupo <- function(rotulo, termos) {
    i <- nomes %in% gsub("`", "", termos)
    if (!any(i)) return(NULL)
    data.frame(termo = rotulo, gl = sum(a$Df[i]), sq = sum(a$`Sum Sq`[i]))
  }
  linhas <- rbind(grupo("Bloco", blc), grupo("Primeira ordem", t$fo), grupo("Interações", t$twi),
                  grupo("Quadráticos", t$pq))
  qm_res <- a$`Mean Sq`[nomes == "Residuals"]; gl_res <- a$Df[nomes == "Residuals"]
  linhas$qm <- linhas$sq / linhas$gl
  linhas$F <- linhas$qm / qm_res
  linhas$p_valor <- stats::pf(linhas$F, linhas$gl, gl_res, lower.tail = FALSE)
  d$.y <- d[[resp]]
  fa <- .tr_exp_an_falta_ajuste(aj, d, x, blc)
  res <- data.frame(termo = "Resíduo", gl = gl_res, sq = a$`Sum Sq`[nomes == "Residuals"], qm = qm_res,
                    F = NA_real_, p_valor = NA_real_)
  extra <- if (fa$gl_ep > 0 && fa$gl_fa > 0) {
    f_fa <- (fa$sq_fa / fa$gl_fa) / (fa$sq_ep / fa$gl_ep)
    rbind(data.frame(termo = "Falta de ajuste", gl = fa$gl_fa, sq = fa$sq_fa, qm = fa$sq_fa / fa$gl_fa,
                     F = f_fa, p_valor = stats::pf(f_fa, fa$gl_fa, fa$gl_ep, lower.tail = FALSE)),
          data.frame(termo = "Erro puro", gl = fa$gl_ep, sq = fa$sq_ep, qm = fa$sq_ep / fa$gl_ep,
                     F = NA_real_, p_valor = NA_real_))
  } else NULL
  tab <- rbind(linhas, res, extra)
  nota_fa <- if (is.null(extra)) "sem pontos repetidos: não há erro puro, e a falta de ajuste não se testa" else ""
  quadro <- trama.models::tr_models_effects(
    tab, sprintf("Superfície de resposta · %sª ordem", ordem), coluna_estat = "F",
    rodape = list(R2 = format(signif(summary(aj)$r.squared, 4))), nota = nota_fa,
    fonte = "Myers, Montgomery & Anderson-Cook (2009); Box & Wilson (1951); Lenth (2009)")

  # Análise canônica: com blocos, o intercepto é a média dos blocos, para que o
  # ŷ no ponto estacionário não seja o de um bloco arbitrário.
  bb <- .tr_exp_an_bB(cf, x, ordem)
  b0 <- cf[["(Intercept)"]]
  if (length(blc)) b0 <- b0 + sum(cf[grepl(paste0("^`?", blc), names(cf))]) / nlevels(d[[blc]])
  superficie <- function(X) b0 + as.vector(X %*% bb$b) + rowSums((X %*% bb$B) * X)
  raio <- max(abs(unlist(d[, x])))
  if (ordem == "2") {
    e <- eigen(bb$B, symmetric = TRUE)
    xs <- tryCatch(as.vector(-0.5 * solve(bb$B, bb$b)), error = function(err) rep(NA_real_, length(x)))
    ys <- if (anyNA(xs)) NA_real_ else b0 + 0.5 * sum(xs * bb$b)
    rel <- abs(e$values) / max(abs(e$values))
    natureza <- if (all(e$values < 0)) "máximo" else if (all(e$values > 0)) "mínimo" else "ponto de sela"
    if (any(rel < 0.05)) natureza <- paste(natureza, "(autovalor perto de zero: cumeeira)")
    canonica <- tibble::tibble(
      item = c(paste0("x_s · ", x), "ŷ no ponto estacionário", paste0("autovalor ", seq_along(e$values)), "natureza",
               "distância ao centro"),
      valor = c(format(signif(xs, 6)), format(signif(ys, 6)), format(signif(e$values, 6)), natureza,
                format(signif(sqrt(sum(xs^2)), 4))),
      nota = c(rep("", length(x) + 1L),
               vapply(seq_along(e$values), function(i) paste(sprintf("%s %s", format(signif(e$vectors[, i], 4)), x), collapse = "; "), ""),
               "", if (!anyNA(xs) && max(abs(xs)) > raio) "fora da região experimentada: extrapolação" else ""))
  } else {
    xs <- rep(NA_real_, length(x))
    dir <- bb$b / sqrt(sum(bb$b^2))
    canonica <- tibble::tibble(item = paste0("maior subida · ", x), valor = format(signif(dir, 6)),
                               nota = "direção (vetor unitário) do caminho de maior subida, a partir do centro")
  }

  # Contorno nos dois primeiros fatores; os demais no centro (0).
  g <- seq(-raio, raio, length.out = 61)
  if (length(x) >= 2L) {
    grid <- expand.grid(a = g, b = g)
    X <- matrix(0, nrow(grid), length(x)); X[, 1] <- grid$a; X[, 2] <- grid$b
    grid$yhat <- superficie(X)
    p <- ggplot2::ggplot(grid, ggplot2::aes(x = .data[["a"]], y = .data[["b"]], z = .data[["yhat"]])) +
      ggplot2::geom_contour_filled(bins = 10) +
      ggplot2::geom_point(data = d, ggplot2::aes(x = .data[[x[[1]]]], y = .data[[x[[2]]]]), inherit.aes = FALSE,
                          shape = 21, fill = "white") +
      ggplot2::labs(x = x[[1]], y = x[[2]], fill = resp,
                    subtitle = if (length(x) > 2L) sprintf("%s no centro (0)", paste(x[-(1:2)], collapse = ", ")) else NULL)
    if (ordem == "2" && !anyNA(xs)) {
      p <- p + ggplot2::annotate("point", x = xs[[1]], y = xs[[2]], shape = 4, size = 4, stroke = 1.5)
    }
  } else {
    X <- matrix(g, ncol = 1)
    p <- ggplot2::ggplot(data.frame(a = g, yhat = superficie(X)), ggplot2::aes(x = .data[["a"]], y = .data[["yhat"]])) +
      ggplot2::geom_line() +
      ggplot2::geom_point(data = d, ggplot2::aes(x = .data[[x[[1]]]], y = .data[[resp]]), inherit.aes = FALSE) +
      ggplot2::labs(x = x[[1]], y = resp)
  }
  grafico <- trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
  list(modelo = fit, quadro = quadro, canonica = canonica, grafico = grafico)
}
