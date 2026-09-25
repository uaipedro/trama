# Peças da regressão nos tratamentos quantitativos (`models/polinomial`, em
# polinomial.R) e o contrato da curva que ela devolve (classe `tr_models_dose`).
#
# Até a integração 9.2 havia dois blocos para isto: o `models/polinomial` da
# main e o `models/dose_response` da branch. Viraram um só, com o id da main;
# a história de cada escolha está no cabeçalho de polinomial.R.
#
# O clássico das teses de agrárias: doses de adubo, lâminas de irrigação,
# densidades. A ANOVA trata a dose como fator (o F do tratamento diz SE a dose
# importa); o desdobramento dos graus de liberdade do tratamento em componentes
# polinomiais diz COMO — reta, parábola, cúbica — e a curva escolhida é a que
# vai para a figura, com a equação, o R² e, na parábola, a dose de máxima
# eficiência técnica.
#
# A entrada é o MODELO da ANOVA, e não a tabela: o procedimento de livro testa
# cada componente contra o QM do resíduo DAQUELA ANOVA (o do DBC, com o bloco
# fora), e é esse modelo que a pessoa já tem no fluxo quando a pergunta surge.
# Pela tabela o bloco teria de refazer o delineamento com outro nome de param.
#
# Sai um `models/fit` de classe própria (`tr_models_dose`), porque a curva é
# ajustada às MÉDIAS com o erro da ANOVA — nem o `lm` nas parcelas (o resíduo
# dele engoliria a falta de ajuste) nem o `lm` nas médias (o resíduo dele é o
# desvio da regressão, com k − g − 1 gl) dão o erro padrão que o livro dá. E
# sai, na outra porta, o quadro do desdobramento.


#' Os níveis de um fator como número, ou erro dizendo qual nível não é.
#' @noRd
.tr_models_doses <- function(fator, trat, no) {
  niv <- levels(fator)
  x <- suppressWarnings(as.numeric(gsub(",", ".", niv, fixed = TRUE)))
  if (anyNA(x)) {
    .tr_models_abort("tr_models_error_not_numeric",
                     paste0("'%s': os níveis de '%s' têm de ser doses (números), e %s não é. A ",
                            "regressão precisa da distância entre os níveis."),
                     no, trat, paste(sprintf("'%s'", niv[is.na(x)]), collapse = ", "))
  }
  x
}

#' O desdobramento do tratamento em componentes polinomiais, pela ANOVA.
#'
#' Reajusta o MESMO modelo com a dose entrando antes como polinômios ortogonais
#' e depois como fator: o SQ sequencial de cada polinômio é o componente, o do
#' fator é o desvio da regressão, e o resíduo é o da ANOVA (o espaço do modelo
#' não muda — o fator já continha os polinômios). Vale para DIC, DBC e DQL, e
#' no desbalanceado dá a partição sequencial depois dos controles.
#' @noRd
.tr_models_desdobrar <- function(modelo, trat, x, gmax) {
  d <- as.data.frame(modelo$dados)
  dose <- x[as.integer(d[[trat]])]
  P <- stats::poly(dose, gmax)
  for (j in seq_len(gmax)) d[[paste0(".p", j)]] <- P[, j]
  termos <- attr(stats::terms(stats::as.formula(modelo$formula)), "term.labels")
  controles <- setdiff(termos, .tr_models_bt(trat))
  f <- stats::as.formula(paste(.tr_models_bt(modelo$resposta), "~",
                               paste(c(controles, paste0(".p", seq_len(gmax)), .tr_models_bt(trat)), collapse = " + ")))
  a <- as.data.frame(stats::anova(stats::lm(f, data = d)))
  nomes <- trimws(rownames(a))
  comp <- a[paste0(".p", seq_len(gmax)), , drop = FALSE]
  desvio <- a[nomes == trat, , drop = FALSE]
  res <- a[nomes == "Residuals", , drop = FALSE]
  list(sq = comp$`Sum Sq`, gl_desvio = if (nrow(desvio)) desvio$Df else 0,
       sq_desvio = if (nrow(desvio)) desvio$`Sum Sq` else 0,
       gl_res = res$Df, qm_res = res$`Mean Sq`)
}

#' Uma linha do quadro, com F e p contra o resíduo da ANOVA.
#' @noRd
.tr_models_linha_f <- function(termo, gl, sq, qm_res, gl_res) {
  qm <- sq / gl
  f <- qm / qm_res
  data.frame(termo = termo, gl = gl, sq = sq, qm = qm, F = f,
             p_valor = stats::pf(f, gl, gl_res, lower.tail = FALSE))
}

#' A dose de máxima (ou mínima) eficiência técnica da parábola: −b1 / (2 b2).
#' @noRd
.tr_models_met <- function(b, faixa) {
  if (length(b) != 3L || !is.finite(b[[3]]) || b[[3]] == 0) return(NULL)
  xm <- -b[[2]] / (2 * b[[3]])
  list(x = xm, y = b[[1]] + b[[2]] * xm + b[[3]] * xm^2, tipo = if (b[[3]] < 0) "máximo" else "mínimo",
       dentro = xm >= faixa[[1]] && xm <= faixa[[2]])
}

# ---- Contrato da classe tr_models_dose -----------------------------------------------

#' A dose de `novos` como número (a coluna pode vir como fator da ANOVA).
#' @noRd
.tr_models_dose_x <- function(x, novos) {
  v <- novos[[x$tratamentos]]
  if (is.factor(v) || is.character(v)) v <- suppressWarnings(as.numeric(gsub(",", ".", as.character(v), fixed = TRUE)))
  stats::setNames(data.frame(as.numeric(v)), x$tratamentos)
}

.tr_models_dose_prever <- function(x, novos) {
  as.numeric(stats::predict(x$ajuste, newdata = .tr_models_dose_x(x, novos)))
}

#' @export
tr_models_info.tr_models_dose <- function(x) {
  list(tarefa = "regressao", resposta = x$resposta, preditores = x$tratamentos, niveis = NULL,
       n = nrow(x$dados), rotulo = x$rotulo, familia = "gaussian")
}
#' @export
tr_models_predict_raw.tr_models_dose <- function(x, novos, ...) .tr_models_prev(.tr_models_dose_prever(x, novos))
#' @export
tr_models_predict_cv.tr_models_dose <- function(x, validacao = "resubstituição") {
  if (.tr_models_validacao(validacao) == "cruzada") {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("Validação 'cruzada' não se aplica a %s: a curva é ajustada às médias das ",
                            "doses, e tirar uma parcela não tira uma dose. Use 'resubstituição'."), x$rotulo)
  }
  .tr_models_prev(.tr_models_dose_prever(x, x$dados))
}

#' Coeficientes com o erro da ANOVA: V = QM_res · (X'WX)⁻¹, t com os gl do resíduo.
#'
#' É o que os programas de experimentação fazem (a variância de uma média é
#' QM/r), e não o `summary()` do `lm` nas médias, cujo resíduo é o desvio da
#' regressão.
#' @export
tr_models_coefs.tr_models_dose <- function(x, exponenciar = FALSE, escala = "unidade", confianca = 0.95, ...) {
  if (isTRUE(exponenciar)) {
    .tr_models_exigir(x, "glm", "models/coefficients", "Exponenciar só faz sentido num GLM com ligação log ou logit.")
  }
  confianca <- .tr_models_num(confianca, "confianca", min = 0.5, max = 0.999)
  X <- stats::model.matrix(x$ajuste)
  V <- x$qm_res * solve(crossprod(X, X * x$repeticoes))
  b <- stats::coef(x$ajuste)
  ep <- sqrt(diag(V))
  tq <- stats::qt(1 - (1 - confianca) / 2, x$gl_res)
  tab <- data.frame(termo = names(b), estimativa = unname(b), erro_padrao = unname(ep), t = unname(b / ep),
                    p_valor = 2 * stats::pt(abs(unname(b / ep)), x$gl_res, lower.tail = FALSE),
                    li = unname(b - tq * ep), ls = unname(b + tq * ep))
  tab <- .tr_models_coefs_escala(tab, X, escala)
  met <- x$met
  .tr_models_coefs_efeitos(x, .tr_models_coefs_ic_nomes(tab, confianca), "t",
    .tr_models_nota(sprintf("erro do resíduo da ANOVA (%s gl)", .tr_models_gl(x$gl_res)), x$avisos,
                    if (!is.null(met)) sprintf("dose de %s: %s (ŷ = %s)", met$tipo, .tr_models_fmt(met$x, 4L), .tr_models_fmt(met$y, 4L)) else "",
                    .tr_models_nota_escala(escala)))
}
#' @export
tr_models_stats.tr_models_dose <- function(x) {
  y <- x$dados[[x$resposta]]
  .tr_models_stats_linha(x, NULL, r2 = x$r2, sig = sqrt(x$qm_res), cv = 100 * sqrt(x$qm_res) / mean(y),
                         gl_res = as.numeric(x$gl_res))
}
#' A tabela da ANOVA com a curva ao lado: o resíduo é o desvio de cada parcela
#' à curva (bloco e falta de ajuste inclusos), padronizado pelo erro da ANOVA.
#' @export
tr_models_resid.tr_models_dose <- function(x) {
  d <- x$dados
  aj <- .tr_models_dose_prever(x, d)
  nomes <- c("ajustado", "residuo", "residuo_padronizado")
  nomes <- ifelse(nomes %in% names(d), paste0(nomes, "_modelo"), nomes)
  d[[nomes[[1]]]] <- aj
  d[[nomes[[2]]]] <- d[[x$resposta]] - aj
  d[[nomes[[3]]]] <- d[[nomes[[2]]]] / sqrt(x$qm_res)
  d
}
#' @export
tr_models_importance.tr_models_dose <- function(x) {
  .tr_models_abort("tr_models_error_not_applicable",
                   "'models/importance' não se aplica a %s: há uma preditora só, a dose. Leia o desdobramento.", x$rotulo)
}
#' @export
tr_models_card.tr_models_dose <- function(x, ctx) trama.view::tr_view_render(tr_models_plot_regression(x), ctx)
#' @export
tr_models_as_table.tr_models_dose <- function(x) tr_models_coefficients(x)$tabela
