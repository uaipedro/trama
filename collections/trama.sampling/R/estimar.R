# As estimativas: média, total, proporção e razão, com o erro do desenho.
#
# Nenhum destes blocos pergunta pelo desenho. Ele vem no cabo, dentro do
# `sampling/sample`, e é essa a razão de a amostra ter tipo próprio: o campo
# "estrato" que não existe não pode ser esquecido.

.TR_SAMPLING_CAMPOS_ESTIMATIVA <- c("tabela", "quantidade", "variavel", "por", "confianca", "desenho",
                                    "percentual", "nota")

#' Monta a estimativa.
#'
#' - `tabela`: tibble com as colunas de domínio (a do `por`, e `nivel` na
#'   proporção) seguidas de `estimativa`, `erro_padrao`, `li`, `ls`, `margem`,
#'   `cv_pct`, `deff`, `n` e `gl`.
#' - `quantidade`: "Média", "Total", "Proporção", "Razão".
#' - `percentual`: o card mostra em % (proporção).
#' @noRd
.tr_sampling_estimativa <- function(tabela, quantidade, variavel, por, confianca, desenho,
                                    percentual = FALSE, nota = "") {
  structure(list(tabela = tibble::as_tibble(tabela), quantidade = quantidade, variavel = variavel, por = por,
                 confianca = confianca, desenho = desenho, percentual = percentual, nota = nota),
            class = "tr_sampling_estimate")
}

#' A coluna `dominio` interna vira a coluna com o nome do `por`, ou some.
#' @noRd
.tr_sampling_nomear_dominio <- function(t, por) {
  if (is.null(por)) t$dominio <- NULL else names(t)[names(t) == "dominio"] <- por
  t
}

#' Nota de faltantes e a do desenho, juntas.
#' @noRd
.tr_sampling_nota_estimativa <- function(amostra, faltantes, variavel) {
  .tr_sampling_nota(
    if (faltantes > 0) sprintf("%d linha(s) com faltante em %s fora (tratadas como domínio)", faltantes, variavel) else "",
    amostra$nota)
}

#' A amostra, a variável numérica e o domínio: o que as quatro estimativas leem.
#' @noRd
.tr_sampling_ler <- function(amostra, variavel, por, param = "variavel", numerica = TRUE) {
  .tr_sampling_amostra_conferir(amostra)
  d <- amostra$dados
  v <- .tr_sampling_col(d, variavel, param)
  if (numerica) .tr_sampling_numerica(d, v, param)
  p <- .tr_sampling_col_opcional(d, por, "por")
  list(d = d, v = v, por = p, dominio = if (is.null(p)) NULL else d[[p]])
}

.tr_sampling_media_ou_total <- function(amostra, variavel, por, confianca, tipo) {
  conf <- .tr_sampling_conf(confianca)
  l <- .tr_sampling_ler(amostra, variavel, por)
  t <- .tr_sampling_estimar(amostra, l$d[[l$v]], tipo, conf, dominio = l$dominio)
  falt <- attr(t, "faltantes")
  .tr_sampling_estimativa(.tr_sampling_nomear_dominio(t, l$por), if (tipo == "total") "Total" else "Média",
                          l$v, l$por, conf, amostra$rotulo,
                          nota = .tr_sampling_nota_estimativa(amostra, falt, l$v))
}

#' Média estimada pelo desenho.
#' @param amostra uma amostra (`sampling/sample`).
#' @param variavel coluna numérica.
#' @param por coluna dos domínios (em branco: a população toda).
#' @param confianca nível de confiança, entre 0,5 e 0,999 (0,95 = 95%).
#' @return uma estimativa (`sampling/estimate`).
#' @export
tr_sampling_mean <- function(amostra, variavel = "", por = "", confianca = 0.95) {
  .tr_sampling_media_ou_total(amostra, variavel, por, confianca, "media")
}

#' Total estimado pelo desenho (Horvitz-Thompson).
#' @inheritParams tr_sampling_mean
#' @return uma estimativa (`sampling/estimate`).
#' @export
tr_sampling_total <- function(amostra, variavel = "", por = "", confianca = 0.95) {
  .tr_sampling_media_ou_total(amostra, variavel, por, confianca, "total")
}

#' Proporção estimada pelo desenho, para cada categoria ou uma só.
#' @inheritParams tr_sampling_mean
#' @param variavel coluna categórica (texto, fator ou lógica).
#' @param nivel a categoria (em branco: todas).
#' @return uma estimativa (`sampling/estimate`).
#' @export
tr_sampling_proportion <- function(amostra, variavel = "", nivel = "", por = "", confianca = 0.95) {
  conf <- .tr_sampling_conf(confianca)
  l <- .tr_sampling_ler(amostra, variavel, por, numerica = FALSE)
  x <- as.character(l$d[[l$v]])
  presentes <- sort(unique(x[!is.na(x)]))
  if (.tr_sampling_preenchido(nivel)) {
    nivel <- trimws(as.character(nivel))
    if (!nivel %in% presentes) {
      .tr_sampling_abort("tr_sampling_error_bad_option",
                         "Param 'nivel': '%s' não aparece em '%s'. Presentes: %s.", nivel, l$v, paste(presentes, collapse = ", "))
    }
    niveis <- nivel
  } else {
    niveis <- presentes
    if (length(niveis) > 30L) {
      .tr_sampling_abort("tr_sampling_error_bad_option",
                         "A coluna '%s' tem %d categorias; proporção de todas é para variável categórica. Escolha um 'nivel'.",
                         l$v, length(niveis))
    }
  }
  partes <- lapply(niveis, function(nv) {
    t <- .tr_sampling_estimar(amostra, ifelse(is.na(x), NA_real_, as.numeric(x == nv)), "media", conf,
                              dominio = l$dominio)
    t <- .tr_sampling_nomear_dominio(t, l$por)
    tibble::add_column(t, nivel = nv, .before = "estimativa")
  })
  t <- do.call(rbind, partes)
  falt <- sum(is.na(x))
  .tr_sampling_estimativa(t, "Proporção", if (length(niveis) == 1L) sprintf("%s = %s", l$v, niveis) else l$v,
                          l$por, conf, amostra$rotulo, percentual = TRUE,
                          nota = .tr_sampling_nota_estimativa(amostra, falt, l$v))
}

#' Razão de dois totais estimada pelo desenho.
#' @inheritParams tr_sampling_mean
#' @param numerador,denominador colunas numéricas.
#' @return uma estimativa (`sampling/estimate`).
#' @export
tr_sampling_ratio <- function(amostra, numerador = "", denominador = "", por = "", confianca = 0.95) {
  conf <- .tr_sampling_conf(confianca)
  l <- .tr_sampling_ler(amostra, numerador, por, param = "numerador")
  den <- .tr_sampling_col(l$d, denominador, "denominador")
  .tr_sampling_numerica(l$d, den, "denominador")
  t <- .tr_sampling_estimar(amostra, l$d[[l$v]], "razao", conf, x = l$d[[den]], dominio = l$dominio)
  falt <- attr(t, "faltantes")
  .tr_sampling_estimativa(.tr_sampling_nomear_dominio(t, l$por), "Razão", sprintf("%s / %s", l$v, den),
                          l$por, conf, amostra$rotulo,
                          nota = .tr_sampling_nota_estimativa(amostra, falt, paste(l$v, "ou", den)))
}

#' A faixa de precisão pelo CV. Segue as marcas da régua em `index.js`.
#' @noRd
.tr_sampling_faixa_cv <- function(cv) {
  cut(cv, c(-Inf, 5, 15, 30, Inf), labels = c("ótima", "boa", "regular", "imprecisa"), right = TRUE)
}

#' Gráfico das estimativas com intervalo, uma linha por domínio.
#' @param estimativa uma estimativa (`sampling/estimate`).
#' @param aspecto,tema,titulo,rotulo_x,rotulo_y,legenda cosméticos (ver `trama.view`).
#' @return um ggplot (`view/plot`).
#' @export
tr_sampling_plot_estimates <- function(estimativa, aspecto = "16:9", tema = "padrão", titulo = "",
                                       rotulo_x = "", rotulo_y = "", legenda = "direita") {
  .tr_sampling_estimativa_conferir(estimativa)
  t <- as.data.frame(estimativa$tabela)
  t$rotulo <- .tr_sampling_rotulos_linhas(estimativa)
  t$rotulo <- factor(t$rotulo, levels = rev(unique(t$rotulo)))
  t$precisao <- .tr_sampling_faixa_cv(t$cv_pct)
  escala <- if (isTRUE(estimativa$percentual)) 100 else 1
  for (k in c("estimativa", "li", "ls")) t[[k]] <- t[[k]] * escala
  cores <- c(`ótima` = "#0e7490", boa = "#0891b2", regular = "#f59e0b", imprecisa = "#dc2626")
  p <- ggplot2::ggplot(t, ggplot2::aes(y = .data[["rotulo"]], x = .data[["estimativa"]], colour = .data[["precisao"]])) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data[["li"]], xmax = .data[["ls"]]), width = .25,
                           linewidth = .7, orientation = "y") +
    ggplot2::geom_point(size = 2.8) +
    ggplot2::scale_colour_manual(values = cores, drop = FALSE) +
    ggplot2::labs(y = NULL, colour = "precisão (CV)",
                  x = sprintf("%s de %s%s (IC %s%%)", estimativa$quantidade, estimativa$variavel,
                              if (isTRUE(estimativa$percentual)) ", em %" else "",
                              round(100 * estimativa$confianca)))
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' O rótulo de cada linha: o domínio, a categoria, ou os dois.
#' @noRd
.tr_sampling_rotulos_linhas <- function(estimativa) {
  t <- estimativa$tabela
  partes <- list()
  if (!is.null(estimativa$por)) partes$por <- as.character(t[[estimativa$por]])
  if ("nivel" %in% names(t)) partes$nivel <- as.character(t$nivel)
  if (!length(partes)) return(rep(estimativa$variavel, nrow(t)))
  do.call(paste, c(partes, sep = " · "))
}
