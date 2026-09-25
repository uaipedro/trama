# Dose-resposta: a regressão do fator QUANTITATIVO depois da ANOVA.
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

.TR_MODELS_GRAUS <- c("automático", "1", "2", "3")
.TR_MODELS_COMPONENTES <- c("Linear", "Quadrático", "Cúbico")

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

#' Regressão da dose depois da ANOVA: desdobramento e curva.
#'
#' @param modelo objeto `tr_models_fit` de uma ANOVA em DIC, DBC ou DQL.
#' @param tratamento o fator de doses (os níveis têm de ser números).
#' @param grau `"automático"` (o maior componente significativo, até o cúbico)
#'   ou `"1"`, `"2"`, `"3"`.
#' @param confianca o nível dos testes dos componentes (e do intervalo dos
#'   coeficientes): o grau automático é o maior com p < 1 − confianca.
#' @return lista com `modelo` (objeto `tr_models_fit` de classe
#'   `tr_models_dose`) e `quadro` (`tr_models_effects`, o desdobramento).
#' @export
tr_models_dose_response <- function(modelo, tratamento = "", grau = "automático", confianca = 0.95) {
  .tr_models_fit_conferir(modelo)
  no <- "models/dose_response"
  grau <- .tr_models_enum(as.character(grau), .TR_MODELS_GRAUS, "grau")
  confianca <- .tr_models_num(confianca, "confianca", min = 0.5, max = 0.999)
  alfa <- 1 - confianca
  if (!identical(modelo$classe, "lm") || !isTRUE(modelo$delineamento %in% c("DIC", "DBC", "DQL"))) {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'%s' desdobra o tratamento de uma ANOVA em DIC, DBC ou DQL, e chegou %s. ",
                            "No fatorial, ajuste a regressão dentro de cada nível do outro fator em ",
                            "'models/lm' (ex.: 'y ~ dose + I(dose^2)')."), no, modelo$rotulo)
  }
  trat <- .tr_models_col(modelo$dados, tratamento, "tratamento")
  if (!trat %in% modelo$tratamentos) {
    .tr_models_abort("tr_models_error_not_applicable", "'%s': '%s' não é o tratamento do modelo (%s).",
                     no, trat, paste(modelo$tratamentos, collapse = ", "))
  }
  fator <- modelo$dados[[trat]]
  x <- .tr_models_doses(fator, trat, no)
  k <- length(x)
  if (k < 3L) {
    .tr_models_abort("tr_models_error_too_few_rows",
                     paste0("'%s': com %d doses só passa uma reta por elas, e não sobra grau de liberdade ",
                            "para testar se ela serve. A regressão pede pelo menos 3 doses."), no, k)
  }
  gmax <- min(3L, k - 1L)
  dd <- .tr_models_ajustar(.tr_models_desdobrar(modelo, trat, x, gmax), no)
  linhas <- .tr_models_linha_f(.TR_MODELS_COMPONENTES[seq_len(gmax)], rep(1, gmax), dd$sq, dd$qm_res, dd$gl_res)
  sq_trat <- sum(dd$sq) + dd$sq_desvio
  g <- if (grau == "automático") {
    sig <- which(linhas$p_valor < alfa)
    if (!length(sig)) {
      .tr_models_abort("tr_models_error_not_applicable",
                       paste0("'%s': nenhum componente (linear a %s) é significativo a %s%%: as doses não ",
                              "seguem uma curva que o teste sustente. Para descrever mesmo assim, escolha o ",
                              "grau à mão."), no, tolower(.TR_MODELS_COMPONENTES[[gmax]]),
                       formatC(100 * alfa, format = "fg", decimal.mark = ","))
    }
    max(sig)
  } else as.integer(grau)
  if (g > gmax) {
    .tr_models_abort("tr_models_error_bad_option",
                     "Param 'grau': com %d doses o polinômio vai até o grau %d, e veio %d.", k, gmax, g)
  }
  tab <- rbind(.tr_models_linha_f("Tratamentos", k - 1, sq_trat, dd$qm_res, dd$gl_res), linhas)
  if (dd$gl_desvio > 0) {
    tab <- rbind(tab, .tr_models_linha_f("Desvios da regressão", dd$gl_desvio, dd$sq_desvio, dd$qm_res, dd$gl_res))
  }
  # A falta de ajuste do grau escolhido: tudo o que o tratamento explica e a
  # curva não. Com o grau 3 ela é a própria linha dos desvios, e não repete.
  if (g < gmax) {
    tab <- rbind(tab, .tr_models_linha_f(sprintf("Falta de ajuste (grau %d)", g), k - 1 - g,
                                         sq_trat - sum(dd$sq[seq_len(g)]), dd$qm_res, dd$gl_res))
  }
  tab <- rbind(tab, data.frame(termo = "Resíduo", gl = dd$gl_res, sq = dd$qm_res * dd$gl_res, qm = dd$qm_res,
                               F = NA_real_, p_valor = NA_real_))

  # A curva nas MÉDIAS, com peso nas repetições: no balanceado são os mesmos
  # coeficientes do ajuste nas parcelas, e o R² é o do livro (SQ da regressão
  # sobre SQ de tratamentos).
  y <- modelo$dados[[modelo$resposta]]
  medias <- data.frame(x, as.vector(tapply(y, fator, mean)), as.vector(table(fator)))
  names(medias) <- c(trat, modelo$resposta, ".r")
  X <- .tr_models_bt(trat)
  rhs <- c(X, if (g >= 2) sprintf("I(%s^2)", X), if (g >= 3) sprintf("I(%s^3)", X))
  f <- stats::as.formula(paste(.tr_models_bt(modelo$resposta), "~", paste(rhs, collapse = " + ")))
  environment(f) <- globalenv()
  # `weights` é avaliado DENTRO de `data` (avaliação não padrão do `lm`).
  ajuste <- stats::lm(f, data = medias, weights = .r)
  r2 <- sum(dd$sq[seq_len(g)]) / sq_trat
  met <- if (g == 2L) .tr_models_met(unname(stats::coef(ajuste)), range(x)) else NULL
  balanceado <- length(unique(medias$.r)) == 1L
  rodape <- list(grau = as.character(g), `R²` = .tr_models_fmt(r2, 4L))
  if (!is.null(met)) rodape[[paste("dose de", met$tipo)]] <- .tr_models_fmt(met$x, 4L)
  quadro <- .tr_models_efeitos(
    tibble::as_tibble(tab), sprintf("Desdobramento de %s em regressão", trat), coluna_estat = "F",
    rodape = rodape,
    nota = .tr_models_nota(
      sprintf("F contra o resíduo da ANOVA (%s); grau %s", modelo$rotulo,
              if (grau == "automático") sprintf("escolhido: o maior componente com p < %s", .tr_models_fmt(alfa)) else "fixado"),
      if (!is.null(met) && !met$dentro) sprintf("o %s da parábola (x = %s) cai fora das doses testadas", met$tipo, .tr_models_fmt(met$x, 4L)) else "",
      if (!balanceado) "desbalanceado: curva nas médias da tabela; partição sequencial depois dos controles" else ""),
    fonte = "Pimentel-Gomes (2009); Banzatto & Kronka (2006)")
  # A tabela do ajuste com a dose NUMÉRICA: é a preditora da curva, e o
  # `models/predict` confere nível de fator — uma dose nova (133) seria
  # recusada como nível que o ajuste não viu.
  dados <- modelo$dados
  dados[[trat]] <- x[as.integer(fator)]
  fit <- .tr_models_fit_obj(ajuste, "dose", sprintf("Regressão · %s", c("linear", "quadrática", "cúbica")[[g]]),
                            f, dados, modelo$resposta, tratamentos = trat,
                            descartadas = modelo$descartadas)
  fit$medias <- tibble::as_tibble(medias[, 1:2])
  fit$repeticoes <- medias$.r
  fit$grau <- g
  fit$r2 <- r2
  fit$qm_res <- dd$qm_res
  fit$gl_res <- dd$gl_res
  fit$met <- met
  fit$desdobramento <- quadro
  fit$origem <- modelo$rotulo
  list(modelo = fit, quadro = quadro)
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
    .tr_models_nota(sprintf("erro do resíduo da ANOVA (%s gl)", .tr_models_gl(x$gl_res)),
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
