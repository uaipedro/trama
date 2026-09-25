# Resumir: modelo entra, o que se lê dele sai — quadro, coeficientes, medidas
# de ajuste, componentes de variância, resíduos.

.TR_MODELS_TIPOS_SQ <- c("I", "II", "III")

#' Exige uma das classes, dizendo o que usar no lugar.
#' @noRd
.tr_models_exigir <- function(fit, classes, no, dica) {
  if (!fit$classe %in% classes) {
    .tr_models_abort("tr_models_error_not_applicable", "'%s' não se aplica a %s. %s",
                     no, fit$rotulo, dica)
  }
  invisible(fit)
}

#' O ajuste com os fatores em contraste de soma zero, para o SQ tipo III.
#'
#' Tipo III com o contraste de tratamento (o padrão do R) testa o efeito de A
#' no NÍVEL DE REFERÊNCIA de B, e não na média — um quadro que parece o do SAS e
#' responde outra pergunta. Reajustar com `contr.sum` é o que o `car` pede, e é
#' feito aqui para ninguém ter de lembrar.
#' @noRd
.tr_models_soma_zero <- function(fit) {
  aj <- fit$ajuste
  fatores <- names(Filter(is.factor, fit$dados))
  usados <- intersect(fatores, all.vars(stats::formula(aj)))
  ctr <- stats::setNames(rep(list("contr.sum"), length(usados)), usados)
  if (fit$classe == "glm") {
    stats::glm(stats::formula(aj), family = stats::family(aj), data = fit$dados,
               contrasts = if (length(ctr)) ctr else NULL)
  } else {
    stats::lm(stats::formula(aj), data = fit$dados, contrasts = if (length(ctr)) ctr else NULL)
  }
}

#' O quadro do `stats`/`car`, com nomes em português e o QM calculado.
#' @noRd
.tr_models_quadro_lm <- function(fit, tipo) {
  aj <- fit$ajuste
  if (tipo == "I") {
    a <- as.data.frame(stats::anova(aj))
    tab <- data.frame(termo = rownames(a), gl = a$Df, sq = a$`Sum Sq`, qm = a$`Mean Sq`,
                      F = a$`F value`, p_valor = a$`Pr(>F)`)
  } else {
    m <- if (tipo == "III") .tr_models_soma_zero(fit) else aj
    a <- as.data.frame(car::Anova(m, type = if (tipo == "II") 2 else 3))
    a <- a[rownames(a) != "(Intercept)", , drop = FALSE]
    tab <- data.frame(termo = rownames(a), gl = a$Df, sq = a$`Sum Sq`, qm = a$`Sum Sq` / a$Df,
                      F = a$`F value`, p_valor = a$`Pr(>F)`)
  }
  tab$termo <- trimws(tab$termo)
  tab$termo[tab$termo == "Residuals"] <- "Resíduo"
  # O Total só no sequencial: é o único tipo em que as SQ somam a SQ total, e
  # uma linha de Total embaixo de um tipo III seria uma conta que não fecha.
  if (tipo == "I") tab <- .tr_models_com_total(tab, fit)
  tab
}

#' Acrescenta a linha do Total (gl n − 1, SQ total corrigida pela média).
#' @noRd
.tr_models_com_total <- function(tab, fit) {
  y <- fit$dados[[fit$resposta]]
  rbind(tab, data.frame(termo = "Total", gl = length(y) - 1, sq = sum((y - mean(y))^2),
                        qm = NA_real_, F = NA_real_, p_valor = NA_real_))
}

#' O quadro de um GLM: desvio e razão de verossimilhança.
#'
#' Nas famílias com dispersão estimada (gaussiana, gama, quasipoisson) o teste
#' é F; nas de dispersão fixa (binomial, poisson), qui-quadrado. É a mesma
#' escolha que o `anova.glm` recomenda, e o card diz qual foi.
#' @noRd
.tr_models_quadro_glm <- function(fit, tipo) {
  aj <- fit$ajuste
  usa_f <- stats::family(aj)$family %in% c("gaussian", "Gamma", "quasipoisson")
  if (tipo == "I") {
    a <- as.data.frame(stats::anova(aj, test = if (usa_f) "F" else "Chisq"))
    a <- a[rownames(a) != "NULL", , drop = FALSE]
    tab <- data.frame(termo = rownames(a), gl = a$Df, desvio = a$Deviance,
                      estat = if (usa_f) a$F else a$Deviance,
                      p_valor = if (usa_f) a$`Pr(>F)` else a$`Pr(>Chi)`)
  } else {
    m <- if (tipo == "III") .tr_models_soma_zero(fit) else aj
    a <- as.data.frame(car::Anova(m, type = if (tipo == "II") 2 else 3,
                                  test.statistic = if (usa_f) "F" else "LR"))
    a <- a[!rownames(a) %in% c("(Intercept)", "Residuals"), , drop = FALSE]
    tab <- data.frame(termo = rownames(a), gl = a$Df,
                      desvio = if (usa_f) NA_real_ else a$`LR Chisq`,
                      estat = if (usa_f) a$`F value` else a$`LR Chisq`,
                      p_valor = if (usa_f) a$`Pr(>F)` else a$`Pr(>Chisq)`)
  }
  names(tab)[names(tab) == "estat"] <- if (usa_f) "F" else "qui2"
  tab$termo <- trimws(tab$termo)
  list(tabela = tab, coluna = if (usa_f) "F" else "qui2")
}

#' O quadro da parcela subdividida: os estratos numa tabela só.
#'
#' O `summary.aovlist` não testa o bloco (fica sozinho no estrato dele); o
#' quadro dos livros o testa contra o erro a, e é o que se faz aqui.
#' @noRd
.tr_models_quadro_split <- function(fit) {
  s <- summary(fit$ajuste)
  pega <- function(estrato) {
    nm <- grep(paste0("^Error: ", estrato, "$"), names(s), value = TRUE)
    if (!length(nm)) return(NULL)
    a <- as.data.frame(s[[nm]][[1]])
    data.frame(termo = trimws(rownames(a)), gl = a$Df, sq = a$`Sum Sq`, qm = a$`Mean Sq`,
               F = if ("F value" %in% names(a)) a$`F value` else NA_real_,
               p_valor = if ("Pr(>F)" %in% names(a)) a$`Pr(>F)` else NA_real_)
  }
  blc <- fit$bloco; a <- fit$tratamentos[[1]]
  e1 <- pega(blc)
  e2 <- pega(paste0(blc, ":", a))
  e3 <- pega("Within")
  e2$termo[e2$termo == "Residuals"] <- "Resíduo (a)"
  e3$termo[e3$termo == "Residuals"] <- "Resíduo (b)"
  qm_a <- e2$qm[e2$termo == "Resíduo (a)"]; gl_a <- e2$gl[e2$termo == "Resíduo (a)"]
  e1$F <- e1$qm / qm_a
  e1$p_valor <- stats::pf(e1$F, e1$gl, gl_a, lower.tail = FALSE)
  .tr_models_com_total(rbind(e1, e2, e3), fit)
}

#' CV% e média geral, nos modelos em que fazem sentido (resposta numérica,
#' erro gaussiano, delineamento ou regressão).
#' @noRd
.tr_models_cv <- function(fit) {
  media <- mean(fit$dados[[fit$resposta]])
  if (fit$classe == "lm") {
    qm <- sum(stats::residuals(fit$ajuste)^2) / stats::df.residual(fit$ajuste)
    return(list(media = media, cv = 100 * sqrt(qm) / media))
  }
  if (fit$classe == "split") {
    q <- .tr_models_quadro_split(fit)
    return(list(media = media,
                cv_a = 100 * sqrt(q$qm[q$termo == "Resíduo (a)"]) / media,
                cv = 100 * sqrt(q$qm[q$termo == "Resíduo (b)"]) / media))
  }
  list(media = media)
}

.tr_models_pct <- function(x) paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%")

#' Quadro da ANOVA.
#' @param modelo objeto `tr_models_fit`.
#' @param tipo_sq `"I"` (sequencial), `"II"` ou `"III"`.
#' @return objeto `tr_models_effects`.
#' @export
tr_models_anova_table <- function(modelo, tipo_sq = "I") {
  .tr_models_fit_conferir(modelo)
  tipo <- .tr_models_enum(tipo_sq, .TR_MODELS_TIPOS_SQ, "tipo_sq")
  no <- "models/anova_table"
  # A curva da dose-resposta: o quadro dela é o desdobramento, feito no ajuste.
  if (identical(modelo$classe, "dose")) return(modelo$desdobramento)
  .tr_models_exigir(modelo, c("lm", "glm", "lmer", "split", "glmer"), no,
                    "Um modelo não linear não tem quadro de somas de quadrados; leia os parâmetros em 'models/coefficients'.")
  rodape <- list()
  coluna <- "F"
  nota <- ""
  tab <- switch(modelo$classe,
    lm = .tr_models_ajustar(.tr_models_quadro_lm(modelo, tipo), no),
    glm = { q <- .tr_models_ajustar(.tr_models_quadro_glm(modelo, tipo), no); coluna <- q$coluna; q$tabela },
    lmer = {
      a <- .tr_models_ajustar(as.data.frame(stats::anova(modelo$ajuste, type = tipo)), no)
      nota <- "gl do denominador por Satterthwaite"
      data.frame(termo = rownames(a), gl = a$NumDF, gl_den = a$DenDF, sq = a$`Sum Sq`,
                 qm = a$`Mean Sq`, F = a$`F value`, p_valor = a$`Pr(>F)`)
    },
    split = {
      if (tipo != "I") {
        .tr_models_abort("tr_models_error_not_applicable",
                         paste0("'%s': a parcela subdividida sai com SQ tipo I (no balanceado os ",
                                "três coincidem). Para dados desbalanceados, ajuste o misto ",
                                "equivalente em 'models/lmer' com '(1 | bloco:parcela)'."), no)
      }
      .tr_models_quadro_split(modelo)
    },
    glmer = {
      # No GLMM não há soma de quadrados: o quadro é de Wald (qui-quadrado) por
      # termo, pelo `car`, e só no tipo II — o III pediria reajustar com
      # contraste de soma zero, e o I (sequencial) o `lme4` não testa.
      if (tipo != "II") {
        .tr_models_abort("tr_models_error_not_applicable",
                         "'%s': no misto generalizado o quadro sai por Wald com SQ tipo II. Escolha tipo_sq = \"II\".", no)
      }
      a <- .tr_models_ajustar(as.data.frame(car::Anova(modelo$ajuste, type = 2)), no)
      coluna <- "qui2"
      nota <- "qui-quadrado de Wald"
      data.frame(termo = rownames(a), gl = a$Df, qui2 = a$Chisq, p_valor = a$`Pr(>Chisq)`)
    })
  cv <- .tr_models_cv(modelo)
  if (!is.null(cv$cv)) rodape[[if (modelo$classe == "split") "CV (b)" else "CV"]] <- .tr_models_pct(cv$cv)
  if (!is.null(cv$cv_a)) rodape[["CV (a)"]] <- .tr_models_pct(cv$cv_a)
  rodape[["média"]] <- .tr_models_fmt(cv$media, 4L)
  rodape[["n"]] <- as.character(nrow(modelo$dados))
  .tr_models_efeitos(
    tibble::as_tibble(tab), sprintf("Quadro da ANOVA · SQ tipo %s", tipo), coluna_estat = coluna,
    rodape = rodape,
    nota = .tr_models_nota(nota, .tr_models_nota_descarte(modelo$descartadas)),
    fonte = switch(tipo, I = "Fisher (1925)", II = "Langsrud (2003); Fox & Weisberg (2019)",
                   III = "Yates (1934); Fox & Weisberg (2019)"))
}

#' Os efeitos que o card do modelo mostra: o quadro tipo I nos delineamentos, os
#' coeficientes no resto.
#' @noRd
.tr_models_efeitos_do_fit <- function(fit) {
  if (!is.null(fit$delineamento) || fit$classe == "split") tr_models_anova_table(fit, "I")
  else tr_models_coefficients(fit)
}

#' O teste que resume o modelo inteiro, quando existe um.
#'
#' F global na regressão (todos os coeficientes zero), razão de verossimilhança
#' contra o modelo nulo no GLM, e o F do tratamento nos delineamentos de um fator.
#' No fatorial e no misto não há UM teste que resuma, e o card não inventa.
#' @noRd
.tr_models_teste_global <- function(fit) {
  out <- function(rotulo, p) list(rotulo = rotulo, p = p, estrelas = .tr_models_estrelas(p))
  if (fit$classe == "lm" && !is.null(fit$delineamento) && length(fit$tratamentos) == 1L) {
    q <- .tr_models_quadro_lm(fit, "I")
    return(out(sprintf("F de %s", fit$tratamentos), q$p_valor[q$termo == fit$tratamentos]))
  }
  if (fit$classe == "lm" && is.null(fit$delineamento)) {
    f <- stats::summary.lm(fit$ajuste)$fstatistic
    if (is.null(f)) return(NULL)
    return(out("F global", stats::pf(f[[1]], f[[2]], f[[3]], lower.tail = FALSE)))
  }
  if (fit$classe == "glm") {
    aj <- fit$ajuste
    gl <- aj$df.null - aj$df.residual
    if (gl < 1) return(NULL)
    return(out("RV vs. nulo", stats::pchisq(aj$null.deviance - aj$deviance, gl, lower.tail = FALSE)))
  }
  NULL
}

#' Coeficientes.
#' @param modelo objeto `tr_models_fit`.
#' @param exponenciar no GLM, devolver `exp()` da estimativa e do intervalo
#'   (razão de chances na binomial, razão de taxas na poisson).
#' @param escala `"unidade"` (por unidade da preditora) ou `"desvio padrão"`
#'   (por desvio padrão da coluna da matriz de design; t e p não mudam).
#' @param confianca nível do intervalo; as colunas saem `li_<nível>`,
#'   `ls_<nível>` (`li_95` no padrão).
#' @return objeto `tr_models_effects`; modelo de outra coleção pode trazer a
#'   coluna `grupo` (a classe de cada linha numa logística multinomial).
#' @export
tr_models_coefficients <- function(modelo, exponenciar = FALSE, escala = "unidade", confianca = 0.95) {
  .tr_models_modelo_conferir(modelo)
  tr_models_coefs(modelo, exponenciar = exponenciar, escala = escala, confianca = confianca)
}

#' Gráfico de floresta dos coeficientes.
#'
#' Lê o `tr_models_coefs()` do contrato, e não o ajuste: por isso vale para todo
#' modelo com coeficiente — `lm`, `glm`, misto, a logística da `multi` e a
#' referência linear da `ml`. Substitui o `multi/plot_odds`, que era este mesmo
#' gráfico só para a logística; um fluxo antigo abre aqui com `exponenciar` e a
#' escala por desvio padrão que eram o padrão de lá.
#'
#' A cor diz se o intervalo cruza a referência (0, ou 1 exponenciado): é a
#' leitura que o gráfico existe para facilitar. Exponenciado, o eixo é log, em
#' que dobrar e reduzir à metade ficam à mesma distância do 1.
#' @inheritParams tr_models_coefficients
#' @param ordenar `"modelo"` (a ordem dos termos no modelo, de cima para baixo)
#'   ou `"estimativa"` (do maior para o menor).
#' @inheritParams trama.view::tr_view_finish
#' @return ggplot.
#' @export
tr_models_plot_coefficients <- function(modelo, exponenciar = FALSE, escala = "unidade", confianca = 0.95,
                                        ordenar = "modelo", aspecto = "16:9", tema = "padrão", titulo = "",
                                        rotulo_x = "", rotulo_y = "", legenda = "direita") {
  .tr_models_modelo_conferir(modelo)
  ordenar <- .tr_models_enum(ordenar, c("modelo", "estimativa"), "ordenar")
  ef <- tr_models_coefs(modelo, exponenciar = exponenciar, escala = escala, confianca = confianca)
  d <- as.data.frame(ef$tabela)
  d <- d[!d$termo %in% c("(Intercept)", "(intercepto)"), , drop = FALSE]
  if (!nrow(d)) {
    .tr_models_abort("tr_models_error_not_applicable",
                     "'models/plot_coefficients': o modelo só tem o intercepto; não há coeficiente para desenhar.")
  }
  # As colunas do intervalo levam o nível no nome (`li_95`, `li_90`).
  d$li <- d[[grep("^li_", names(d), value = TRUE)[[1]]]]
  d$ls <- d[[grep("^ls_", names(d), value = TRUE)[[1]]]]
  ref <- if (isTRUE(exponenciar)) 1 else 0
  # Na multinomial o mesmo termo aparece uma vez por classe: a ordem é a da
  # primeira aparição, e cada classe ganha um painel.
  niveis <- unique(d$termo)
  if (ordenar == "estimativa") niveis <- unique(d$termo[order(-d$estimativa)])
  d$termo <- factor(d$termo, levels = rev(niveis))
  d$sinal <- ifelse(d$li > ref, "acima", ifelse(d$ls < ref, "abaixo", "cruza"))
  d$sinal <- factor(d$sinal, levels = c("acima", "abaixo", "cruza"))
  pct <- formatC(100 * confianca, format = "fg", decimal.mark = ",")
  rotulos <- if (isTRUE(exponenciar)) c(acima = "aumenta (> 1)", abaixo = "diminui (< 1)", cruza = "inclui 1")
             else c(acima = "positivo", abaixo = "negativo", cruza = "inclui 0")
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["estimativa"]], y = .data[["termo"]], colour = .data[["sinal"]])) +
    ggplot2::geom_vline(xintercept = ref, colour = .TR_MODELS_CINZA, linetype = "dashed") +
    # `geom_errorbarh` está deprecado no ggplot2 4: barra horizontal é a
    # `geom_errorbar` com `orientation = "y"`.
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data[["li"]], xmax = .data[["ls"]]),
                           width = .2, linewidth = .6, orientation = "y") +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_colour_manual(values = c(acima = .TR_MODELS_COR_2, abaixo = .TR_MODELS_COR, cruza = .TR_MODELS_CINZA),
                                 labels = rotulos, name = sprintf("IC %s%%", pct)) +
    ggplot2::labs(x = sprintf("%s (IC %s%%%s)", if (isTRUE(exponenciar)) "estimativa exponenciada, escala log" else "estimativa",
                              pct, if (identical(escala, "desvio padrão")) ", por desvio padrão" else ""),
                  y = NULL)
  if (isTRUE(exponenciar)) p <- p + ggplot2::scale_x_log10()
  if ("grupo" %in% names(d) && length(unique(d$grupo)) > 1L) p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[["grupo"]]))
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' R² marginal e condicional de um misto gaussiano (Nakagawa & Schielzeth 2013,
#' com a extensão de Johnson 2014 para inclinação aleatória).
#'
#' A variância de cada termo aleatório é a média, sobre as observações, de
#' `z' Σ z` — com só intercepto isso é a própria variância do grupo, e com
#' inclinação leva em conta a escala da covariável. Quarenta linhas a menos que
#' uma dependência (`performance`) para dois números.
#' @noRd
.tr_models_r2_misto <- function(aj) {
  x <- lme4::getME(aj, "X")
  var_f <- stats::var(as.vector(x %*% lme4::fixef(aj)))
  vc <- lme4::VarCorr(aj)
  mm <- lme4::getME(aj, "mmList")
  var_a <- sum(vapply(seq_along(mm), function(k) {
    z <- as.matrix(mm[[k]])
    s <- as.matrix(vc[[k]])
    mean(rowSums((z %*% s) * z))
  }, 1))
  var_e <- stats::sigma(aj)^2
  tot <- var_f + var_a + var_e
  c(marginal = var_f / tot, condicional = (var_f + var_a) / tot)
}

#' Medidas de ajuste, em colunas FIXAS.
#'
#' As mesmas colunas para todo modelo, com NA onde a medida não existe: é o que
#' deixa um `data/bind_rows` de três modelos virar a tabela de comparação.
#' @param modelo objeto `tr_models_fit`.
#' @return tibble de uma linha.
#' @export
tr_models_fit_stats <- function(modelo) {
  .tr_models_modelo_conferir(modelo)
  tr_models_stats(modelo)
}

#' O misto do modelo: o próprio, ou o equivalente da parcela subdividida.
#' @noRd
.tr_models_misto <- function(fit, no) {
  if (fit$classe %in% c("lmer", "glmer")) return(fit$ajuste)
  if (fit$classe == "split") return(fit$aux_misto)
  .tr_models_abort("tr_models_error_not_applicable",
                   "'%s' pede um modelo misto, e chegou %s. Ajuste um 'models/lmer'.", no, fit$rotulo)
}

#' Componentes de variância.
#' @param modelo objeto `tr_models_fit` de um misto (ou de parcela subdividida).
#' @return tibble `grupo`, `componente`, `variancia`, `desvio_padrao`,
#'   `correlacao`, `proporcao`.
#' @export
tr_models_random_effects <- function(modelo) {
  .tr_models_fit_conferir(modelo)
  aj <- .tr_models_misto(modelo, "models/random_effects")
  v <- as.data.frame(lme4::VarCorr(aj))
  e_var <- is.na(v$var2)
  comp <- ifelse(e_var, ifelse(v$grp == "Residual", "resíduo", v$var1),
                 sprintf("corr(%s, %s)", v$var1, v$var2))
  # Proporção só para interceptos e resíduo: a variância de uma inclinação está
  # em outra escala (por unidade da covariável²), e somá-la com as outras dá
  # um número sem significado.
  # No GLMM não há variância residual na escala da ligação para somar, e a
  # proporção entre só os grupos diria outra coisa: fica NA.
  soma_ok <- e_var & (v$grp == "Residual" | v$var1 == "(Intercept)") & modelo$classe != "glmer"
  total <- sum(v$vcov[soma_ok])
  tibble::tibble(
    grupo = ifelse(v$grp == "Residual", "resíduo", v$grp), componente = comp,
    variancia = ifelse(e_var, v$vcov, NA_real_), desvio_padrao = ifelse(e_var, v$sdcor, NA_real_),
    correlacao = ifelse(e_var, NA_real_, v$sdcor),
    proporcao = ifelse(soma_ok, v$vcov / total, NA_real_))
}

#' Os resíduos que os pressupostos testam.
#'
#' Na parcela subdividida são os do erro b, pelo `lm` auxiliar. No GLM não há
#' resíduo que deva ser normal ou de variância constante — é o ponto de usar um
#' GLM —, e o bloco que pedir isso vira card vermelho explicando.
#' @noRd
.tr_models_residuos <- function(fit, no, permitir_misto = TRUE) {
  if (fit$classe == "dose") {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'%s' não se aplica a %s: os pressupostos são os da ANOVA de origem (%s). ",
                            "Ligue o teste ao modelo da ANOVA."), no, fit$rotulo, fit$origem)
  }
  if (fit$classe == "nls") {
    aj <- fit$ajuste
    return(list(ajustado = as.numeric(stats::fitted(aj)), residuo = as.numeric(stats::residuals(aj)),
                padronizado = as.numeric(stats::residuals(aj)) / stats::sigma(aj), ajuste = NULL))
  }
  if (fit$classe %in% c("glm", "glmer")) {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'%s' não se aplica a %s: num GLM a variância acompanha a média e os ",
                            "resíduos não precisam ser normais — é para isso que ele existe. Olhe o ",
                            "gráfico de 'models/plot_diagnostics'."), no, fit$rotulo)
  }
  if (fit$classe == "lmer" && !permitir_misto) {
    .tr_models_abort("tr_models_error_not_applicable",
                     "'%s' não se aplica a %s. Olhe os resíduos em 'models/plot_diagnostics'.", no, fit$rotulo)
  }
  aj <- if (fit$classe == "split") fit$aux_lm else fit$ajuste
  list(ajustado = as.numeric(stats::fitted(aj)), residuo = as.numeric(stats::residuals(aj)),
       padronizado = if (fit$classe == "lmer") as.numeric(stats::residuals(aj, type = "pearson", scaled = TRUE))
                     else as.numeric(stats::rstandard(aj)),
       ajuste = aj)
}

#' Resíduos ao lado da tabela.
#' @param modelo objeto `tr_models_fit`.
#' @return a tabela usada no ajuste com `ajustado`, `residuo` e
#'   `residuo_padronizado` à direita.
#' @export
tr_models_residuals <- function(modelo) {
  .tr_models_modelo_conferir(modelo)
  tr_models_resid(modelo)
}

#' Diagnóstico gráfico dos resíduos: quatro painéis.
#'
#' Resíduos × ajustados (linearidade, variância), Q-Q normal, escala-locação
#' (variância) e histograma. Numa figura só, e não quatro nós, porque se leem
#' juntos: o funil do primeiro painel e a cauda do Q-Q costumam ser a mesma
#' coisa vista de dois lados.
#' @param modelo objeto `tr_models_fit`.
#' @export
tr_models_plot_diagnostics <- function(modelo, aspecto = "1:1", tema = "padrão", titulo = "",
                                       rotulo_x = "", rotulo_y = "", legenda = "direita") {
  r <- tr_models_residuals(modelo)
  aj_n <- ncol(r) - 2L
  aj <- r[[aj_n]]; res <- r[[aj_n + 1L]]; pad <- r[[aj_n + 2L]]
  n <- length(res)
  ord <- order(pad)
  paineis <- c("resíduos × ajustados", "Q-Q normal", "escala-locação", "histograma dos resíduos")
  d1 <- data.frame(painel = paineis[[1]], x = aj, y = res)
  d2 <- data.frame(painel = paineis[[2]], x = stats::qnorm(stats::ppoints(n)), y = pad[ord])
  d3 <- data.frame(painel = paineis[[3]], x = aj, y = sqrt(abs(pad)))
  dh <- data.frame(painel = paineis[[4]], x = res)
  fp <- function(d) { d$painel <- factor(d$painel, levels = paineis); d }
  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(data = fp(data.frame(painel = paineis[[1]], y = 0)),
                        ggplot2::aes(yintercept = .data[["y"]]), colour = .TR_MODELS_CINZA, linetype = "dashed") +
    # Intercepto e inclinação como ESTÉTICA vinda do dado: passados como
    # constante, o `geom_abline` ignora o `data` e desenha a diagonal nos quatro
    # painéis.
    ggplot2::geom_abline(data = fp(data.frame(painel = paineis[[2]], a = 0, b = 1)),
                         ggplot2::aes(intercept = .data[["a"]], slope = .data[["b"]]),
                         colour = .TR_MODELS_CINZA, linetype = "dashed") +
    ggplot2::geom_point(data = fp(rbind(d1, d2, d3)), ggplot2::aes(.data[["x"]], .data[["y"]]),
                        colour = .TR_MODELS_COR, alpha = .75, size = 1.6) +
    ggplot2::geom_histogram(data = fp(dh), ggplot2::aes(.data[["x"]]), bins = max(5L, min(30L, ceiling(sqrt(n)))),
                            fill = .TR_MODELS_COR, colour = NA, alpha = .8) +
    ggplot2::facet_wrap(~painel, scales = "free") +
    ggplot2::labs(x = NULL, y = NULL)
  if (n >= 10L) {
    p <- p + ggplot2::geom_smooth(data = fp(rbind(d1, d3)), ggplot2::aes(.data[["x"]], .data[["y"]]),
                                  method = "loess", formula = y ~ x, se = FALSE,
                                  colour = .TR_MODELS_COR_2, linewidth = .6)
  }
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

# ---- Comparar modelos -----------------------------------------------------------

#' Os termos de um modelo, em texto, para conferir o aninhamento.
#'
#' No misto, o termo aleatório entra pelo FATOR de agrupamento, e não pelo texto:
#' `(1 | sujeito)` está aninhado em `(dias | sujeito)`, e comparar os textos
#' diria que não. O aninhamento exato da estrutura aleatória não se confere pela
#' fórmula; os graus de liberdade, que o `compare` também exige diferentes,
#' fecham o resto.
#' @noRd
.tr_models_termos <- function(fit) {
  f <- stats::as.formula(fit$formula)
  fixos <- attr(stats::terms(reformulas::nobars(f)), "term.labels")
  grupos <- unlist(lapply(reformulas::findbars(f), function(b) paste0("| ", deparse(b[[3]]))))
  unique(c(fixos, grupos))
}

#' Dois modelos aninhados: o termo a mais melhora o ajuste?
#' @param modelo,outro objetos `tr_models_fit` da mesma família, nas mesmas linhas.
#' @return objeto `tr_models_test`.
#' @export
tr_models_compare <- function(modelo, outro) {
  .tr_models_fit_conferir(modelo); .tr_models_fit_conferir(outro)
  no <- "models/compare"
  if (modelo$classe != outro$classe || !modelo$classe %in% c("lm", "glm", "lmer", "glmer")) {
    .tr_models_abort("tr_models_error_not_nested",
                     paste0("'%s': compara dois modelos da mesma família (dois lm, dois glm, dois ",
                            "lmer ou dois glmer), e chegaram %s e %s."), no, modelo$rotulo, outro$rotulo)
  }
  if (nrow(modelo$dados) != nrow(outro$dados)) {
    .tr_models_abort("tr_models_error_not_nested",
                     paste0("'%s': os modelos usaram %d e %d linhas. Com linhas diferentes a ",
                            "comparação mede os dados, não o termo — tire os faltantes antes, num ",
                            "'data/drop_na', e ajuste os dois na mesma tabela."),
                     no, nrow(modelo$dados), nrow(outro$dados))
  }
  if (modelo$classe %in% c("glm", "glmer") && stats::family(modelo$ajuste)$family != stats::family(outro$ajuste)$family) {
    .tr_models_abort("tr_models_error_not_nested", "'%s': os dois GLM têm famílias diferentes.", no)
  }
  ta <- .tr_models_termos(modelo); tb <- .tr_models_termos(outro)
  gl_de <- function(f) if (f$classe %in% c("lmer", "glmer")) attr(stats::logLik(f$ajuste), "df") else length(stats::coef(f$ajuste))
  if (gl_de(modelo) <= gl_de(outro)) { menor <- modelo; maior <- outro; tm <- ta; tM <- tb }
  else { menor <- outro; maior <- modelo; tm <- tb; tM <- ta }
  if (!all(tm %in% tM) || gl_de(menor) == gl_de(maior)) {
    .tr_models_abort("tr_models_error_not_nested",
                     paste0("'%s': os modelos não são aninhados — os termos do menor (%s) têm de ",
                            "estar todos no maior (%s), e o maior ter algum a mais."),
                     no, paste(tm, collapse = " + "), paste(tM, collapse = " + "))
  }
  novos <- paste(setdiff(tM, tm), collapse = " + ")
  if (!nzchar(novos)) novos <- "estrutura aleatória"
  h0 <- sprintf("os termos a mais (%s) não melhoram o ajuste", novos)
  if (menor$classe %in% c("lmer", "glmer")) {
    a <- .tr_models_ajustar(.tr_models_capturar(stats::anova(menor$ajuste, maior$ajuste, refit = TRUE))$valor, no)
    return(.tr_models_teste("Razão de verossimilhança", h0, a$Chisq[[2]], "qui2", a$`Pr(>Chisq)`[[2]],
                            gl = as.character(a$Df[[2]]),
                            conclusao_sim = "o modelo maior ajusta melhor",
                            conclusao_nao = "não há evidência de que o modelo maior ajuste melhor",
                            nota = sprintf("%sAIC %s × %s", if (menor$classe == "lmer") "reajustados por máxima verossimilhança; " else "",
                                           .tr_models_fmt(a$AIC[[1]], 5L), .tr_models_fmt(a$AIC[[2]], 5L)),
                            fonte = "Wilks (1938)"))
  }
  usa_f <- menor$classe == "lm" || stats::family(menor$ajuste)$family %in% c("gaussian", "Gamma", "quasipoisson")
  a <- .tr_models_ajustar(as.data.frame(stats::anova(menor$ajuste, maior$ajuste,
                                                     test = if (usa_f) "F" else "Chisq")), no)
  if (usa_f) {
    .tr_models_teste("F de modelos aninhados", h0, a$F[[2]], "F", a$`Pr(>F)`[[2]],
                     gl = sprintf("%d; %d", as.integer(a$Df[[2]]), as.integer(a[[1]][[2]])),
                     conclusao_sim = "o modelo maior ajusta melhor",
                     conclusao_nao = "não há evidência de que o modelo maior ajuste melhor",
                     fonte = "Fisher (1925)")
  } else {
    .tr_models_teste("Razão de verossimilhança", h0, a$Deviance[[2]], "qui2", a$`Pr(>Chi)`[[2]],
                     gl = as.character(a$Df[[2]]),
                     conclusao_sim = "o modelo maior ajusta melhor",
                     conclusao_nao = "não há evidência de que o modelo maior ajuste melhor",
                     fonte = "Wilks (1938)")
  }
}

#' Teste dos efeitos aleatórios: cada um sai, e o ajuste piora?
#' @param modelo objeto `tr_models_fit` de um misto.
#' @return objeto `tr_models_effects`.
#' @export
tr_models_random_test <- function(modelo) {
  .tr_models_fit_conferir(modelo)
  no <- "models/random_test"
  aj <- .tr_models_misto(modelo, no)
  if (modelo$classe == "glmer") {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'%s' não se aplica ao misto generalizado (o ranova é do lmerTest, só gaussiano). ",
                            "Ajuste o modelo sem o termo e compare os dois em 'models/compare'."), no)
  }
  r <- .tr_models_ajustar(as.data.frame(.tr_models_capturar(lmerTest::ranova(aj))$valor), no)
  termo <- rownames(r)
  termo[termo == "<none>"] <- "modelo completo"
  tab <- tibble::tibble(termo = termo, parametros = r$npar, log_verossimilhanca = r$logLik,
                        aic = r$AIC, qui2 = r$LRT, gl = r$Df, p_valor = r[["Pr(>Chisq)"]])
  .tr_models_efeitos(tab, "Teste dos efeitos aleatórios", coluna_estat = "qui2",
                     nota = "cada linha tira um termo aleatório; na fronteira (variância zero) o p-valor é conservador",
                     fonte = "Kuznetsova, Brockhoff & Christensen (2017)")
}
