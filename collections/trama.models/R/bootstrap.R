# Bootstrap não paramétrico de coeficientes de um modelo linear.

tr_models_bootstrap <- function(modelo, quantidade = "coeficientes", estratos = "",
                                repeticoes = 1999L, confianca = 0.95, semente = 1L,
                                aspecto = "16:9", tema = "padrão", titulo = "",
                                rotulo_x = "", rotulo_y = "", legenda = "direita") {
  if (!inherits(modelo, "tr_models_lm") && !(inherits(modelo, "tr_models_fit") && identical(modelo$classe, "lm")))
    .tr_models_abort("tr_models_error_not_applicable", "'models/bootstrap' aceita modelos da classe lm; recebeu '%s'.", modelo$classe %||% class(modelo)[[1]])
  if (quantidade != "coeficientes") .tr_models_abort("tr_models_error_bad_option", "Nesta versão, Quantidade deve ser 'coeficientes'.")
  B <- as.integer(repeticoes); if (length(B) != 1L || is.na(B) || B < 99L) .tr_models_abort("tr_models_error_bad_option", "Repetições deve ser pelo menos 99.")
  d <- modelo$dados; X <- stats::model.matrix(modelo$ajuste); y <- stats::model.response(stats::model.frame(modelo$ajuste))
  if (nzchar(estratos)) {
    if (!estratos %in% names(d)) .tr_models_abort("tr_models_error_bad_option", "Estratos '%s' não existe na tabela.", estratos)
    grupos <- split(seq_len(nrow(d)), d[[estratos]])
  } else grupos <- list(todas = seq_len(nrow(d)))
  observado <- stats::coef(modelo$ajuste); nomes <- names(observado)
  estrato <- if (nzchar(estratos)) as.integer(factor(d[[estratos]])) else rep(1L, nrow(d))
  statistic <- function(data, indices) {
    fit <- tryCatch(stats::lm.fit(X[indices, , drop = FALSE], y[indices]), error = function(e) NULL)
    if (is.null(fit) || fit$rank < ncol(X)) rep(NA_real_, length(nomes)) else unname(fit$coefficients)
  }
  bobj <- .tr_models_com_semente(as.integer(semente), boot::boot(data = seq_len(nrow(d)), statistic = statistic,
                        R = B, strata = estrato, sim = "ordinary", simple = FALSE))
  vals <- bobj$t; colnames(vals) <- nomes; validas <- complete.cases(vals)
  dist <- as.data.frame(as.table(vals[validas, , drop = FALSE])); names(dist) <- c("reamostra", "quantidade", "valor")
  # boot.ci implementa os intervalos percentil e BCa, incluindo aceleração jackknife.
  tab <- lapply(seq_along(nomes), function(j) {
    z <- vals[validas, j]; bj <- bobj; bj$t <- matrix(z, ncol = 1); bj$t0 <- observado[j]; bj$R <- length(z); bj$L <- NULL
    ci <- tryCatch(boot::boot.ci(bj, conf = confianca, type = c("perc", "bca")), error = function(e) NULL)
    perc <- if (is.null(ci)) c(NA, NA) else ci$percent[4:5]; bca <- if (is.null(ci) || is.null(ci$bca)) c(NA, NA) else ci$bca[4:5]
    data.frame(quantidade = nomes[j], estimativa = observado[j], vies = mean(z) - observado[j], erro_padrao = stats::sd(z), li_perc = perc[1], ls_perc = perc[2], li_bca = bca[1], ls_bca = bca[2], mesmo_sinal = mean(sign(z) == sign(observado[j])), B = length(z), descartadas = B - length(z))
  })
  tabela <- do.call(rbind, tab)
  graf <- ggplot2::ggplot(dist, ggplot2::aes(x = valor)) +
    ggplot2::geom_histogram(bins = 30, fill = "#4c78a8", color = "white") +
    ggplot2::facet_wrap(~quantidade, scales = "free") +
    ggplot2::geom_vline(data = data.frame(quantidade = nomes, observado = observado),
                        ggplot2::aes(xintercept = observado), linetype = 2, color = "#d62728") +
    ggplot2::labs(x = "Estimativa reamostrada", y = "Frequência") + ggplot2::theme_minimal()
  graf <- trama.view::tr_view_finish(graf, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
  list(tabela = tabela, distribuicao = dist, out = graf)
}
