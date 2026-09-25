# Ajustar: tabela entra, MODELO sai.
#
# Três modelos de fórmula livre (`lm`, `glm`, `lmer`) e cinco delineamentos. A
# fórmula livre aceita as colunas OU a fórmula digitada, e a fórmula vence: quem
# a escreveu sabe o que quer, e os campos de coluna são o atalho de quem não
# quer escrever.

#' A fórmula do modelo: a digitada, ou montada das colunas.
#' @noRd
.tr_models_formula_ou_cols <- function(dados, formula, resposta, preditores, no) {
  if (.tr_models_preenchido(formula)) return(.tr_models_ler_formula(formula, dados))
  resp <- .tr_models_col(dados, resposta, "resposta")
  preds <- .tr_models_cols(dados, preditores, "preditores", minimo = 0L)
  .tr_models_montar_formula(resp, .tr_models_bt(preds))
}

#' Confere que sobrou grau de liberdade para o resíduo.
#'
#' Sem resíduo o `lm` ajusta perfeito, com p-valor NaN em tudo, e o card ficaria
#' verde com o quadro vazio. Acontece no DBC com uma repetição por tratamento
#' em cada bloco E a interação bloco × tratamento na fórmula.
#' @noRd
.tr_models_gl_residuo <- function(ajuste, no) {
  gl <- stats::df.residual(ajuste)
  if (is.null(gl) || is.na(gl) || gl < 1) {
    .tr_models_abort("tr_models_error_no_residual_df",
                     paste0("'%s': o modelo não deixa grau de liberdade para o resíduo (%d ",
                            "observações para %d parâmetros). Tire termos, ou confira se não ",
                            "há uma repetição só por combinação."),
                     no, stats::nobs(ajuste), length(stats::coef(ajuste)))
  }
  invisible(gl)
}

#' A tabela DENTRO da chamada do misto.
#'
#' `ranova` e `anova(refit = TRUE)` reajustam avaliando a chamada guardada —
#' que, ajustada aqui dentro, cita variáveis locais (`p$dados`, `reml`) que não
#' existem mais depois do RDS ("objeto 'p' não encontrado"). A chamada é
#' reescrita com os VALORES, e o modelo volta do disco sabendo se reajustar.
#' @noRd
.tr_models_embutir_dados <- function(ajuste, dados) {
  ajuste@call <- as.call(list(quote(lmerTest::lmer), formula = stats::formula(ajuste),
                              data = as.data.frame(dados), REML = lme4::isREML(ajuste)))
  ajuste
}

#' Regressão linear.
#'
#' Texto e lógico viram fator; número fica número. A decisão de tratar uma dose
#' como fator é da pessoa, na fórmula (`factor(dose)`), e não do bloco — nos
#' blocos de ANOVA é o contrário, e a ajuda dos dois diz isso.
#' @param dados tabela.
#' @param resposta coluna da resposta (ignorada quando há fórmula).
#' @param preditores colunas dos preditores, separadas por vírgula.
#' @param formula fórmula digitada, `"y ~ x1 + x2"`; quando preenchida, vence.
#' @return objeto `tr_models_fit`.
#' @export
tr_models_lm <- function(dados, resposta = "", preditores = "", formula = "") {
  f <- .tr_models_formula_ou_cols(dados, formula, resposta, preditores, "models/lm")
  vars <- all.vars(f)
  .tr_models_numerica(dados, all.vars(f[[2]])[[1]], "resposta")
  p <- .tr_models_preparar(dados, vars, "models/lm")
  cats <- .tr_models_categoricas(p$dados, all.vars(f[[3]]))
  p <- .tr_models_preparar(dados, vars, "models/lm", fatores = cats)
  ajuste <- .tr_models_ajustar(stats::lm(f, data = p$dados), "models/lm")
  .tr_models_gl_residuo(ajuste, "models/lm")
  .tr_models_fit_obj(ajuste, "lm", "Regressão linear", f, p$dados, all.vars(f[[2]])[[1]],
                     descartadas = p$descartadas)
}

.TR_MODELS_FAMILIAS <- c("gaussiana", "binomial", "poisson", "gama", "quasipoisson")

#' Modelo linear generalizado.
#' @param familia `"gaussiana"`, `"binomial"`, `"poisson"`, `"gama"` ou
#'   `"quasipoisson"`, cada uma com a ligação canônica (log na gama).
#' @inheritParams tr_models_lm
#' @return objeto `tr_models_fit`.
#' @export
tr_models_glm <- function(dados, resposta = "", preditores = "", formula = "", familia = "poisson") {
  familia <- .tr_models_enum(familia, .TR_MODELS_FAMILIAS, "familia")
  f <- .tr_models_formula_ou_cols(dados, formula, resposta, preditores, "models/glm")
  vars <- all.vars(f)
  resp <- all.vars(f[[2]])[[1]]
  # A gama na ligação log, e não na inversa canônica: a inversa quase nunca é o
  # que se quer interpretar, e diverge com facilidade.
  fam <- switch(familia, gaussiana = stats::gaussian(), binomial = stats::binomial(),
                poisson = stats::poisson(), gama = stats::Gamma(link = "log"),
                quasipoisson = stats::quasipoisson())
  p0 <- .tr_models_preparar(dados, vars, "models/glm")
  cats <- .tr_models_categoricas(p0$dados, all.vars(f[[3]]))
  p <- .tr_models_preparar(dados, vars, "models/glm", fatores = cats)
  if (familia != "binomial") .tr_models_numerica(p$dados, resp, "resposta")
  if (familia %in% c("poisson", "quasipoisson") && any(p$dados[[resp]] < 0)) {
    .tr_models_abort("tr_models_error_bad_option",
                     "'models/glm': a família %s é de contagem, e a resposta '%s' tem valor negativo.",
                     familia, resp)
  }
  ajuste <- .tr_models_ajustar(stats::glm(f, family = fam, data = p$dados), "models/glm")
  .tr_models_gl_residuo(ajuste, "models/glm")
  .tr_models_fit_obj(ajuste, "glm", sprintf("GLM · %s", familia), f, p$dados, resp,
                     descartadas = p$descartadas)
}

#' A fórmula de um misto: a digitada (com termo aleatório) ou a do atalho.
#' @param alternativa o bloco sugerido quando a fórmula não tem termo aleatório.
#' @noRd
.tr_models_formula_misto <- function(dados, formula, resposta, fixos, grupo, alternativa) {
  if (.tr_models_preenchido(formula)) {
    f <- .tr_models_ler_formula(formula, dados)
    if (is.null(reformulas::findbars(f))) {
      .tr_models_abort("tr_models_error_bad_formula",
                       paste0("Param 'formula': '%s' não tem termo aleatório. Escreva-o como ",
                              "'(1 | grupo)', ou use '%s' para um modelo só de efeitos fixos."),
                       formula, alternativa)
    }
  } else {
    resp <- .tr_models_col(dados, resposta, "resposta")
    fix <- .tr_models_cols(dados, fixos, "fixos", minimo = 0L)
    g <- .tr_models_col(dados, grupo, "grupo")
    rhs <- c(if (length(fix)) .tr_models_bt(fix) else "1", sprintf("(1 | %s)", .tr_models_bt(g)))
    f <- stats::as.formula(paste(.tr_models_bt(resp), "~", paste(rhs, collapse = " + ")))
    environment(f) <- globalenv()
  }
  f
}

#' Modelo linear misto (`lme4`, com os p-valores do `lmerTest`).
#'
#' Pela fórmula, com os termos aleatórios na sintaxe do `lme4` —
#' `(1 | bloco)`, `(dias | sujeito)` —, ou pelo atalho de colunas, que monta um
#' intercepto aleatório por `grupo`. O `lmerTest::lmer` e não o `lme4::lmer`:
#' é o mesmo ajuste, com a classe que sabe dar gl de Satterthwaite ao quadro e
#' aos coeficientes.
#' @param fixos colunas dos efeitos fixos (atalho sem fórmula).
#' @param grupo coluna do intercepto aleatório (atalho sem fórmula).
#' @param reml REML (padrão) ou máxima verossimilhança.
#' @inheritParams tr_models_lm
#' @return objeto `tr_models_fit`.
#' @export
tr_models_lmer <- function(dados, formula = "", resposta = "", fixos = "", grupo = "", reml = TRUE) {
  f <- .tr_models_formula_misto(dados, formula, resposta, fixos, grupo, "models/lm")
  resp <- all.vars(f[[2]])[[1]]
  .tr_models_numerica(dados, resp, "resposta")
  vars <- all.vars(f)
  grupos <- unique(unlist(lapply(reformulas::findbars(f), function(b) all.vars(b[[3]]))))
  p0 <- .tr_models_preparar(dados, vars, "models/lmer")
  cats <- union(.tr_models_categoricas(p0$dados, setdiff(vars, resp)), grupos)
  p <- .tr_models_preparar(dados, vars, "models/lmer", fatores = cats)
  r <- .tr_models_ajustar(.tr_models_capturar(
    lmerTest::lmer(f, data = p$dados, REML = isTRUE(reml))), "models/lmer")
  .tr_models_fit_obj(.tr_models_embutir_dados(r$valor, p$dados), "lmer", "Modelo misto", f, p$dados, resp, descartadas = p$descartadas)
}

.TR_MODELS_FAMILIAS_MISTO <- c("binomial", "poisson")

#' Modelo misto generalizado (`lme4::glmer`).
#'
#' O `models/lmer` para resposta que não é contínua: proporção de plantas
#' doentes por parcela, contagem de insetos por armadilha, com bloco, animal ou
#' local aleatórios. A fórmula e o atalho de colunas são os do `models/lmer`;
#' na binomial com contagem de sucessos, a resposta vai na fórmula como
#' `cbind(sucessos, fracassos)`.
#'
#' Sem REML (não existe para o GLMM) e sem Satterthwaite: os testes dos fixos
#' são de Wald (z), como o `summary()` do `lme4` dá.
#' @param familia `"binomial"` ou `"poisson"`, com a ligação canônica.
#' @inheritParams tr_models_lmer
#' @return objeto `tr_models_fit`.
#' @export
tr_models_glmer <- function(dados, formula = "", resposta = "", fixos = "", grupo = "", familia = "binomial") {
  no <- "models/glmer"
  familia <- .tr_models_enum(familia, .TR_MODELS_FAMILIAS_MISTO, "familia")
  f <- .tr_models_formula_misto(dados, formula, resposta, fixos, grupo, "models/glm")
  resp <- all.vars(f[[2]])[[1]]
  vars <- all.vars(f)
  grupos <- unique(unlist(lapply(reformulas::findbars(f), function(b) all.vars(b[[3]]))))
  p0 <- .tr_models_preparar(dados, vars, no)
  # Resposta de texto ("doente"/"sadia") vira fator, que o `glmer` binomial lê.
  cats <- union(.tr_models_categoricas(p0$dados, vars), grupos)
  p <- .tr_models_preparar(dados, vars, no, fatores = cats)
  if (familia == "poisson") {
    .tr_models_numerica(p$dados, resp, "resposta")
    if (any(p$dados[[resp]] < 0)) {
      .tr_models_abort("tr_models_error_bad_option",
                       "'%s': a família poisson é de contagem, e a resposta '%s' tem valor negativo.", no, resp)
    }
  }
  fam <- if (familia == "binomial") stats::binomial() else stats::poisson()
  r <- .tr_models_ajustar(.tr_models_capturar(lme4::glmer(f, data = p$dados, family = fam)), no)
  aj <- r$valor
  # A chamada com os VALORES, como no `models/lmer`: o `anova()` do
  # `models/compare` a avalia de novo, e depois do RDS `p` e `fam` não existem.
  aj@call <- as.call(list(quote(lme4::glmer), formula = stats::formula(aj), data = as.data.frame(p$dados), family = fam))
  fit <- .tr_models_fit_obj(aj, "glmer", sprintf("Misto generalizado · %s", familia), f, p$dados, resp,
                            descartadas = p$descartadas)
  # Os avisos de convergência e de ajuste singular do `lme4` não se perdem no
  # card: vão para a nota dos coeficientes.
  fit$avisos <- r$avisos
  fit
}

# ---- Delineamentos ------------------------------------------------------------

#' O miolo de todo delineamento: conferir, fatorar e ajustar o `aov`.
#'
#' `aov`, e não `lm`: é o mesmo ajuste, com a classe que o `emmeans`, o `car` e
#' o `summary` tratam como experimento. Os fatores do desenho viram fator AQUI —
#' é a diferença entre estes blocos e o `models/lm`.
#' @noRd
.tr_models_delineamento <- function(dados, resposta, tratamentos, controles, rhs, rotulo, sigla, no) {
  resp <- .tr_models_col(dados, resposta, "resposta")
  .tr_models_numerica(dados, resp, "resposta")
  fatores <- c(tratamentos, controles)
  if (anyDuplicated(c(resp, fatores))) {
    .tr_models_abort("tr_models_error_bad_option",
                     "'%s': a mesma coluna aparece em dois papéis (%s).", no,
                     paste(c(resp, fatores)[duplicated(c(resp, fatores))], collapse = ", "))
  }
  p <- .tr_models_preparar(dados, c(resp, fatores), no, fatores = fatores)
  f <- stats::as.formula(paste(.tr_models_bt(resp), "~", rhs))
  environment(f) <- globalenv()
  ajuste <- .tr_models_ajustar(stats::aov(f, data = p$dados), no)
  .tr_models_gl_residuo(ajuste, no)
  .tr_models_fit_obj(ajuste, "lm", paste("ANOVA ·", rotulo), f, p$dados, resp,
                     delineamento = sigla, tratamentos = tratamentos,
                     bloco = if (length(controles)) controles[[1]] else NULL,
                     descartadas = p$descartadas)
}

#' ANOVA de um delineamento inteiramente casualizado (DIC).
#' @param dados tabela.
#' @param resposta coluna da resposta.
#' @param tratamento coluna do tratamento (vira fator).
#' @return objeto `tr_models_fit`.
#' @export
tr_models_anova_dic <- function(dados, resposta = "", tratamento = "") {
  trat <- .tr_models_col(dados, tratamento, "tratamento")
  .tr_models_delineamento(dados, resposta, trat, character(), .tr_models_bt(trat),
                          "DIC", "DIC", "models/anova_dic")
}

#' ANOVA de um delineamento em blocos casualizados (DBC).
#' @inheritParams tr_models_anova_dic
#' @param bloco coluna do bloco (vira fator).
#' @return objeto `tr_models_fit`.
#' @export
tr_models_anova_dbc <- function(dados, resposta = "", tratamento = "", bloco = "") {
  trat <- .tr_models_col(dados, tratamento, "tratamento")
  blc <- .tr_models_col(dados, bloco, "bloco")
  .tr_models_delineamento(dados, resposta, trat, blc,
                          paste(.tr_models_bt(blc), "+", .tr_models_bt(trat)),
                          "DBC", "DBC", "models/anova_dbc")
}

#' ANOVA de um delineamento em quadrado latino (DQL).
#' @inheritParams tr_models_anova_dic
#' @param linha,coluna colunas das duas restrições (viram fator).
#' @return objeto `tr_models_fit`.
#' @export
tr_models_anova_dql <- function(dados, resposta = "", tratamento = "", linha = "", coluna = "") {
  trat <- .tr_models_col(dados, tratamento, "tratamento")
  lin <- .tr_models_col(dados, linha, "linha")
  col <- .tr_models_col(dados, coluna, "coluna")
  fit <- .tr_models_delineamento(dados, resposta, trat, c(lin, col),
                                 paste(.tr_models_bt(lin), "+", .tr_models_bt(col), "+", .tr_models_bt(trat)),
                                 "DQL", "DQL", "models/anova_dql")
  # O quadrado latino tem o MESMO número de linhas, colunas e tratamentos. Fora
  # disso o `aov` ajusta sem reclamar, e o quadro é de outro delineamento.
  k <- vapply(c(lin, col, trat), function(v) nlevels(fit$dados[[v]]), 1L)
  if (length(unique(k)) != 1L) {
    .tr_models_abort("tr_models_error_bad_option",
                     paste0("'models/anova_dql': num quadrado latino linhas, colunas e tratamentos ",
                            "têm o mesmo número de níveis, e vieram %s. Confira as colunas, ou use ",
                            "'models/anova_factorial'."),
                     paste(sprintf("%s = %d", names(k), k), collapse = ", "))
  }
  fit
}

#' ANOVA de um fatorial (2 ou 3 fatores), em DIC ou em blocos.
#' @inheritParams tr_models_anova_dic
#' @param fatores de 2 a 3 colunas, separadas por vírgula (viram fator).
#' @param bloco coluna do bloco; em branco, o fatorial é em DIC.
#' @return objeto `tr_models_fit`.
#' @export
tr_models_anova_factorial <- function(dados, resposta = "", fatores = "", bloco = "") {
  fts <- .tr_models_cols(dados, fatores, "fatores", minimo = 2L, maximo = 3L)
  blc <- if (.tr_models_preenchido(bloco)) .tr_models_col(dados, bloco, "bloco") else character()
  rhs <- paste(c(if (length(blc)) .tr_models_bt(blc),
                 paste(.tr_models_bt(fts), collapse = " * ")), collapse = " + ")
  .tr_models_delineamento(dados, resposta, fts, blc, rhs,
                          if (length(blc)) "fatorial em DBC" else "fatorial em DIC",
                          if (length(blc)) "fatorial_dbc" else "fatorial_dic",
                          "models/anova_factorial")
}

#' ANOVA de parcelas subdivididas em blocos.
#'
#' O quadro tem DOIS erros: o da parcela (erro a, bloco × parcela), que testa o
#' fator da parcela, e o de dentro (erro b), que testa a subparcela e a
#' interação. Um `lm` com um erro só testaria o fator da parcela contra o erro
#' errado — com gl a mais e p-valor pequeno demais, que é o engano clássico.
#'
#' Guarda três ajustes: o `aov` com `Error()` (o quadro), um `lm` com
#' `bloco:parcela` fixo (os resíduos do erro b, para os pressupostos) e o
#' `lmer` com `(1 | bloco:parcela)`, que dá os mesmos F no balanceado e é o
#' que o `emmeans` sabe usar — sobre o `aovlist` ele devolve erro padrão NaN.
#' @inheritParams tr_models_anova_dic
#' @param parcela coluna do fator da parcela.
#' @param subparcela coluna do fator da subparcela.
#' @param bloco coluna do bloco.
#' @return objeto `tr_models_fit`.
#' @export
tr_models_anova_split_plot <- function(dados, resposta = "", parcela = "", subparcela = "", bloco = "") {
  no <- "models/anova_split_plot"
  resp <- .tr_models_col(dados, resposta, "resposta")
  .tr_models_numerica(dados, resp, "resposta")
  a <- .tr_models_col(dados, parcela, "parcela")
  b <- .tr_models_col(dados, subparcela, "subparcela")
  blc <- .tr_models_col(dados, bloco, "bloco")
  if (anyDuplicated(c(resp, a, b, blc))) {
    .tr_models_abort("tr_models_error_bad_option", "'%s': a mesma coluna aparece em dois papéis.", no)
  }
  p <- .tr_models_preparar(dados, c(resp, a, b, blc), no, fatores = c(a, b, blc))
  R <- .tr_models_bt(resp); A <- .tr_models_bt(a); B <- .tr_models_bt(b); K <- .tr_models_bt(blc)
  fm <- function(txt) { f <- stats::as.formula(txt); environment(f) <- globalenv(); f }
  f <- fm(sprintf("%s ~ %s + %s * %s + Error(%s/%s)", R, K, A, B, K, A))
  ajuste <- .tr_models_ajustar(stats::aov(f, data = p$dados), no)
  aux_lm <- .tr_models_ajustar(stats::lm(fm(sprintf("%s ~ %s + %s + %s:%s + %s + %s:%s", R, K, A, K, A, B, A, B)),
                                         data = p$dados), no)
  .tr_models_gl_residuo(aux_lm, no)
  aux <- .tr_models_ajustar(.tr_models_capturar(
    lmerTest::lmer(fm(sprintf("%s ~ %s + %s * %s + (1 | %s:%s)", R, K, A, B, K, A)), data = p$dados)), no)
  .tr_models_fit_obj(ajuste, "split", "ANOVA · parcela subdividida", f, p$dados, resp,
                     delineamento = "split_plot", tratamentos = c(a, b), bloco = blc,
                     aux_lm = aux_lm, aux_misto = .tr_models_embutir_dados(aux$valor, p$dados), descartadas = p$descartadas)
}
