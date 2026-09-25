# Regressão polinomial nos níveis quantitativos de um tratamento da ANOVA.
#
# A SQ de tratamentos (k - 1 gl) se decompõe em graus sucessivos: linear,
# quadrático, cúbico..., cada um com 1 gl, e o que sobra até k - 1 são os
# desvios da regressão. Cada parcela é o acréscimo de SQ ao somar o grau j ao
# modelo com os controles do delineamento (bloco; linha e coluna) e os graus
# menores — os polinômios ortogonais do livro no caso balanceado e igualmente
# espaçado, e a mesma decomposição sequencial com espaçamento ou repetições
# desiguais. Todos os F usam o QM e os gl do resíduo da ANOVA.
#
# Um bloco só desde a integração 9.2. A main tinha o `models/polinomial` (só o
# quadro) e a branch o `models/dose_response` (quadro + curva); as contas do
# desdobramento eram as mesmas (poly() nas doses reais, controles antes, fator
# por último — iguais a 1e-10 no teste). Ficou o id da main, e de cada lado o
# que o outro não tinha:
#
# - da main: grau máximo testado até 5 (`grau_max`), a equação no rodapé, e o
#   quadro mesmo quando nenhum grau é significativo (a resposta "não há
#   tendência" também é resultado);
# - da branch: o DQL, os níveis com vírgula decimal, o `grau` fixado à mão, a
#   falta de ajuste DO GRAU ESCOLHIDO (e não só a do grau máximo), o aviso de
#   curva saturada (grau k − 1 passa por todas as médias), a MET da parábola,
#   e a curva como `models/fit` (classe `tr_models_dose`, em dose.R): pesada
#   pelas repetições, com o erro da ANOVA nos coeficientes, que alimenta
#   `models/coefficients`, `models/predict` e `models/plot_regression`.
#
# Nenhum grau significativo no automático: a curva é a de grau 0 (a média
# geral, R² = 0), e a nota diz isso — em vez de recusar (branch) ou sair sem
# curva (main, que não tinha a porta do modelo).

.TR_MODELS_GRAUS <- c("Linear", "Quadrático", "Cúbico", "Quártico", "Quíntico")
.TR_MODELS_POLI_GRAUS <- c("automático", "1", "2", "3", "4", "5")

#' Regressão polinomial nos tratamentos quantitativos da ANOVA.
#'
#' @param modelo objeto `tr_models_fit` de uma ANOVA em DIC, DBC ou DQL.
#' @param tratamento o fator de doses (os níveis têm de ser números).
#' @param grau `"automático"` (o maior componente significativo) ou `"1"` a
#'   `"5"`, o grau fixado da curva.
#' @param grau_max maior grau testado no desdobramento (1 a 5, e menor que o
#'   número de níveis); sobe sozinho até o `grau` fixado.
#' @param confianca o nível dos testes dos componentes (e do intervalo dos
#'   coeficientes): o grau automático é o maior com p < 1 − confianca.
#' @return lista com `modelo` (objeto `tr_models_fit` de classe
#'   `tr_models_dose`) e `quadro` (`tr_models_effects`, o desdobramento).
#' @export
tr_models_polinomial <- function(modelo, tratamento = "", grau = "automático", grau_max = 3L, confianca = 0.95) {
  .tr_models_fit_conferir(modelo)
  no <- "models/polinomial"
  grau <- .tr_models_enum(as.character(grau), .TR_MODELS_POLI_GRAUS, "grau")
  grau_max <- as.integer(.tr_models_num(grau_max, "grau_max", min = 1, max = 5))
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
  fixo <- if (grau == "automático") NA_integer_ else as.integer(grau)
  if (isTRUE(fixo > k - 1L)) {
    .tr_models_abort("tr_models_error_bad_option",
                     "'%s': com %d níveis o grau vai até %d (pediu %d).", no, k, k - 1L, fixo)
  }
  # O máximo testado: o padrão 3 com 3 doses não pode virar erro (o tratamento
  # só tem 2 gl), então ele desce até k − 1 e a nota avisa; e o grau fixado
  # tem de estar no quadro, então sobe até ele.
  cortado <- grau_max > k - 1L
  gmax <- max(min(grau_max, k - 1L), fixo, na.rm = TRUE)
  dd <- .tr_models_ajustar(.tr_models_desdobrar(modelo, trat, x, gmax), no)
  linhas <- .tr_models_linha_f(.TR_MODELS_GRAUS[seq_len(gmax)], rep(1, gmax), dd$sq, dd$qm_res, dd$gl_res)
  sq_trat <- sum(dd$sq) + dd$sq_desvio
  g <- if (is.na(fixo)) {
    sig <- which(linhas$p_valor < alfa)
    if (length(sig)) max(sig) else 0L
  } else fixo
  tab <- rbind(.tr_models_linha_f("Tratamentos", k - 1, sq_trat, dd$qm_res, dd$gl_res), linhas)
  if (dd$gl_desvio > 0) {
    tab <- rbind(tab, .tr_models_linha_f("Desvios da regressão", dd$gl_desvio, dd$sq_desvio, dd$qm_res, dd$gl_res))
  }
  # A falta de ajuste do grau escolhido: tudo o que o tratamento explica e a
  # curva não. Com g = gmax ela é a própria linha dos desvios, e não repete; com
  # g = 0 seria o próprio tratamento.
  if (g >= 1L && g < gmax) {
    tab <- rbind(tab, .tr_models_linha_f(sprintf("Falta de ajuste (grau %d)", g), k - 1 - g,
                                         sq_trat - sum(dd$sq[seq_len(g)]), dd$qm_res, dd$gl_res))
  }
  # O grau escolhido pelo maior componente pode ainda deixar diferença entre
  # doses sem explicar: a falta de ajuste dele (ou os desvios, no grau máximo)
  # diz isso, e o aviso vai para o quadro E para os coeficientes, que é onde a
  # curva é lida. Grau = k − 1 passa por todas as médias: R² = 1 por
  # construção, e a curva não resume nada.
  p_fa <- if (g >= 1L) tab$p_valor[tab$termo %in% c(sprintf("Falta de ajuste (grau %d)", g),
                                                    if (g == gmax) "Desvios da regressão")] else numeric()
  a_txt <- formatC(100 * alfa, format = "fg", decimal.mark = ",")
  avisos <- c(
    if (g == 0L) sprintf("nenhum componente significativo a %s%%: a curva é a média geral (grau 0)", a_txt),
    if (length(p_fa) && isTRUE(p_fa[[1]] < alfa))
      sprintf("a curva de grau %d não explica toda a variação entre doses; veja a falta de ajuste (p = %s)", g, .tr_models_fmt_p(p_fa[[1]])),
    if (g == k - 1L) sprintf("grau %d com %d doses: a curva passa por todas as médias (R² = 1 por construção)", g, k))
  tab <- rbind(tab, data.frame(termo = "Resíduo", gl = dd$gl_res, sq = dd$qm_res * dd$gl_res, qm = dd$qm_res,
                               F = NA_real_, p_valor = NA_real_))

  # A curva nas MÉDIAS, com peso nas repetições: no balanceado são os mesmos
  # coeficientes do ajuste nas parcelas, e o R² é o do livro (SQ da regressão
  # sobre SQ de tratamentos).
  y <- modelo$dados[[modelo$resposta]]
  medias <- data.frame(x, as.vector(tapply(y, fator, mean)), as.vector(table(fator)))
  names(medias) <- c(trat, modelo$resposta, ".r")
  X <- .tr_models_bt(trat)
  rhs <- c(if (g >= 1L) X, if (g >= 2L) sprintf("I(%s^%d)", X, seq(2L, g)))
  f <- stats::as.formula(paste(.tr_models_bt(modelo$resposta), "~", if (length(rhs)) paste(rhs, collapse = " + ") else "1"))
  environment(f) <- globalenv()
  # `weights` é avaliado DENTRO de `data` (avaliação não padrão do `lm`).
  ajuste <- stats::lm(f, data = medias, weights = .r)
  r2 <- if (g >= 1L) sum(dd$sq[seq_len(g)]) / sq_trat else 0
  met <- if (g == 2L) .tr_models_met(unname(stats::coef(ajuste)), range(x)) else NULL
  balanceado <- length(unique(medias$.r)) == 1L
  rodape <- list(grau = as.character(g))
  if (g >= 1L) {
    b <- unname(stats::coef(ajuste))
    pot <- c("x", "x²", "x³", "x⁴", "x⁵")
    rodape[["equação"]] <- paste0("ŷ = ", .tr_models_fmt_eq(b[[1]]),
                                  paste(vapply(seq_len(g), function(j) .tr_models_termo_eq(b[[j + 1L]], pot[[j]]), ""), collapse = ""))
  }
  rodape[["R²"]] <- .tr_models_fmt(r2, 4L)
  if (!is.null(met)) rodape[[paste("dose de", met$tipo)]] <- .tr_models_fmt(met$x, 4L)
  quadro <- .tr_models_efeitos(
    tibble::as_tibble(tab), sprintf("Regressão polinomial em %s", trat), coluna_estat = "F",
    rodape = rodape,
    nota = .tr_models_nota(
      sprintf("%d níveis de %s; F contra o resíduo da ANOVA (%s, %d gl); grau %s", k, trat, modelo$rotulo,
              as.integer(dd$gl_res),
              if (is.na(fixo)) sprintf("escolhido: o maior componente com p < %s", .tr_models_fmt(alfa)) else "fixado"),
      avisos,
      if (cortado) sprintf("maior grau testado reduzido a %d (%d níveis)", k - 1L, k) else "",
      if (!is.null(met) && !met$dentro) sprintf("o %s da parábola (x = %s) cai fora das doses testadas", met$tipo, .tr_models_fmt(met$x, 4L)) else "",
      if (!balanceado) "repetições desiguais: curva nas médias da tabela; partição sequencial depois dos controles" else ""),
    fonte = "Pimentel-Gomes (2009); Banzatto & Kronka (2006)")
  # A tabela do ajuste com a dose NUMÉRICA: é a preditora da curva, e o
  # `models/predict` confere nível de fator — uma dose nova (133) seria
  # recusada como nível que o ajuste não viu.
  dados <- modelo$dados
  dados[[trat]] <- x[as.integer(fator)]
  rot <- c("média geral", "linear", "quadrática", "cúbica", "quártica", "quíntica")[[g + 1L]]
  fit <- .tr_models_fit_obj(ajuste, "dose", sprintf("Regressão · %s", rot),
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
  fit$avisos <- avisos
  list(modelo = fit, quadro = quadro)
}
