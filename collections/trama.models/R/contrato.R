# O contrato de modelo: o que TODO `models/fit` sabe responder.
#
# Até aqui os leitores decidiam o que fazer por `$classe == "lm"|"glm"|...`, e
# isso fechava o tipo às quatro classes da coleção: um LDA da `multi` ou um
# xgboost da `ml` não tinham como entrar num `models/predict` sem que a models
# conhecesse cada um. O contrato inverte a dependência: a models declara os
# GENÉRICOS, e quem produz um modelo implementa os métodos da sua classe
# (`S3method(trama.models::tr_models_info, tr_ml_fit)` no NAMESPACE da irmã).
# Os leitores da models passam a chamar os genéricos, e um modelo novo cabe em
# todos eles sem que se toque uma linha daqui.
#
# A classe S3 é `c("<específica>", "tr_models_fit")`. As da coleção são
# `tr_models_lm`, `tr_models_glm`, `tr_models_lmer`, `tr_models_split`,
# `tr_models_glmer`, `tr_models_nls` e `tr_models_dose` (a curva da
# dose-resposta, em `dose.R`; a não linear em `naolinear.R`); o
# campo `$classe` continua, porque os leitores que NÃO são do contrato (quadro
# da ANOVA, médias, testes de pressuposto) ainda o usam, e porque é por ele que
# um RDS antigo — gravado só com `"tr_models_fit"` — recupera a subclasse.
#
# O método em `tr_models_fit` de cada genérico é o PORTEIRO, não uma
# implementação: promove o objeto antigo e redespacha, ou diz com classe qual
# método falta. Um default que "funcionasse" para qualquer modelo teria de
# adivinhar campos, e o erro sairia longe do lugar que o causou.

.TR_MODELS_CLASSES <- c("lm", "glm", "lmer", "split", "glmer", "nls", "dose")
.TR_MODELS_VALIDACOES <- c("resubstituição", "cruzada")
.TR_MODELS_TAREFAS <- c("regressao", "classificacao")

#' Nome de coluna saneado: o que vem depois de `prob_` numa probabilidade.
#'
#' Nível de fator é texto livre ("Iris setosa", "não-germinou", "2"), e coluna
#' com espaço ou hífen obriga crase em todo `data/mutate` a jusante. Troca cada
#' sequência de caracteres que não são letra ou dígito (Unicode: acento fica)
#' por `_`, apara as pontas, e desempata com `make.unique` — dois níveis que
#' saneiam igual ("a b" e "a-b") não podem virar a mesma coluna.
#' @param x vetor de textos (os níveis).
#' @return vetor de mesmo tamanho, sem repetição.
#' @export
tr_models_clean_name <- function(x) {
  x <- gsub("(*UCP)[^\\p{L}\\p{N}]+", "_", as.character(x), perl = TRUE)
  x <- gsub("^_+|_+$", "", x)
  x[!nzchar(x)] <- "grupo"
  make.unique(x, sep = "_")
}

#' Os nomes das colunas de probabilidade, na ordem dos níveis.
#' @noRd
.tr_models_colunas_prob <- function(niveis) paste0("prob_", tr_models_clean_name(niveis))

# ---- Genéricos -----------------------------------------------------------------

#' Contrato de modelo da `trama.models`
#'
#' Todo objeto que viaja no tipo `models/fit` tem classe
#' `c("<específica>", "tr_models_fit")` e implementa estes genéricos. O store
#' aceita qualquer classe assim desde que [tr_models_info()] devolva uma
#' descrição válida; os demais métodos são pedidos só por quem os usa, e o que
#' faltar vira erro `tr_models_error_no_method` nomeando a classe.
#'
#' - `tr_models_info(x)`: **obrigatório**. Lista com `tarefa` (`"regressao"` ou
#'   `"classificacao"`), `resposta` (texto), `preditores` (texto; as colunas que
#'   `novos` precisa ter), `niveis` (texto, os níveis da resposta na ordem do
#'   modelo — obrigatório na classificação, `NULL` na regressão), `n` (linhas
#'   do ajuste), `rotulo` (o topo do card) e `familia` (texto ou `NULL`).
#' - `tr_models_predict_raw(x, novos, ...)`: prevê em `novos` (data.frame já
#'   com as colunas de `info$preditores`). Devolve
#'   `list(previsto, prob, extra)`: `previsto` numérico na regressão, FATOR com
#'   os níveis de `info$niveis` na classificação; `prob` matriz `n × k` com
#'   `colnames = info$niveis` (ou `NULL` na regressão / sem probabilidade);
#'   `extra` data.frame de `n` linhas para colunas a mais (`li`, `ls`) ou
#'   `NULL`. As probabilidades saem para a tabela como `prob_<nivel>`, com o
#'   nível saneado por [tr_models_clean_name()].
#' - `tr_models_predict_cv(x, validacao)`: o mesmo formato, no TREINO:
#'   `"resubstituição"` (prevê as linhas que ajustaram) ou `"cruzada"` (cada
#'   linha prevista por um modelo que não a viu).
#' - `tr_models_coefs(x, ...)`: um `tr_models_effects` (colunas `termo`,
#'   `p_valor`; `grupo` opcional).
#' - `tr_models_stats(x)`: tibble de UMA linha com as medidas de ajuste.
#' - `tr_models_resid(x)`: a tabela do ajuste com os resíduos (só regressão).
#' - `tr_models_importance(x)`: tibble `termo`, `importancia`, `medida`.
#' - `tr_models_card(x, ctx)`: o `trama::tr_preview()` do card.
#' - `tr_models_serialize(x)` / `tr_models_unserialize(x)`: o que o store grava
#'   e o que devolve do que leu. Default: identidade (o objeto vai inteiro no
#'   RDS); existe para quem guarda ponteiro externo (xgboost).
#' - `tr_models_as_table(x)`: o adaptador `models/fit` → `data/table`.
#'
#' @param x um modelo (`inherits(x, "tr_models_fit")`).
#' @name tr_models_contract
NULL

#' @rdname tr_models_contract
#' @export
tr_models_info <- function(x) UseMethod("tr_models_info")

#' @rdname tr_models_contract
#' @param novos data.frame com as colunas de `info$preditores`.
#' @param ... argumentos da classe (na models: `intervalo`, `confianca`).
#' @export
tr_models_predict_raw <- function(x, novos, ...) UseMethod("tr_models_predict_raw")

#' @rdname tr_models_contract
#' @param validacao `"resubstituição"` ou `"cruzada"`.
#' @export
tr_models_predict_cv <- function(x, validacao = "resubstituição") UseMethod("tr_models_predict_cv")

#' @rdname tr_models_contract
#' @export
tr_models_coefs <- function(x, ...) UseMethod("tr_models_coefs")

#' @rdname tr_models_contract
#' @export
tr_models_stats <- function(x) UseMethod("tr_models_stats")

#' @rdname tr_models_contract
#' @export
tr_models_resid <- function(x) UseMethod("tr_models_resid")

#' @rdname tr_models_contract
#' @export
tr_models_importance <- function(x) UseMethod("tr_models_importance")

#' @rdname tr_models_contract
#' @param ctx o contexto do preview (`ctx$file(ext)`).
#' @export
tr_models_card <- function(x, ctx) UseMethod("tr_models_card")

#' @rdname tr_models_contract
#' @export
tr_models_serialize <- function(x) UseMethod("tr_models_serialize")

#' @rdname tr_models_contract
#' @export
tr_models_unserialize <- function(x) UseMethod("tr_models_unserialize")

#' @rdname tr_models_contract
#' @export
tr_models_as_table <- function(x) UseMethod("tr_models_as_table")

# ---- Porteiro (método em tr_models_fit) -----------------------------------------

#' RDS antigo: classe só `"tr_models_fit"`, subclasse derivada de `$classe`.
#' @noRd
.tr_models_promover <- function(x) {
  if (identical(class(x), "tr_models_fit") && is.list(x) &&
      length(x$classe) == 1L && x$classe %in% .TR_MODELS_CLASSES) {
    class(x) <- c(paste0("tr_models_", x$classe), "tr_models_fit")
  }
  x
}

#' A coleção que provavelmente define a classe: `tr_ml_fit` → `trama.ml`.
#'
#' Convenção de nome das coleções (`tr_<coleção>_...`), e não registro: quem
#' restaura um modelo sem a coleção carregada não tem de onde ler outra coisa.
#' @noRd
.tr_models_colecao_da_classe <- function(cls) {
  if (grepl("^tr_[a-z0-9]+_", cls)) sub("^tr_([a-z0-9]+)_.*$", "trama.\\1", cls) else "que define essa classe"
}

.tr_models_sem_metodo <- function(generico, x) {
  cls <- class(x)[[1]]
  .tr_models_abort("tr_models_error_no_method",
                   paste0("O modelo de classe '%s' não implementa '%s'. Se ele vem de outra coleção, ",
                          "carregue '%s' (ela registra os métodos do contrato)."),
                   cls, generico, .tr_models_colecao_da_classe(cls))
}

#' O porteiro: promove e redespacha, ou recusa com classe.
#' @noRd
.tr_models_porteiro <- function(generico, x, ...) {
  y <- .tr_models_promover(x)
  if (!identical(class(y), class(x))) return(get(generico, mode = "function")(y, ...))
  .tr_models_sem_metodo(generico, x)
}

#' @export
tr_models_info.tr_models_fit <- function(x) .tr_models_porteiro("tr_models_info", x)
#' @export
tr_models_predict_raw.tr_models_fit <- function(x, novos, ...) .tr_models_porteiro("tr_models_predict_raw", x, novos, ...)
#' @export
tr_models_predict_cv.tr_models_fit <- function(x, validacao = "resubstituição") .tr_models_porteiro("tr_models_predict_cv", x, validacao)
#' @export
tr_models_coefs.tr_models_fit <- function(x, ...) .tr_models_porteiro("tr_models_coefs", x, ...)
#' @export
tr_models_stats.tr_models_fit <- function(x) .tr_models_porteiro("tr_models_stats", x)
#' @export
tr_models_resid.tr_models_fit <- function(x) .tr_models_porteiro("tr_models_resid", x)
#' @export
tr_models_importance.tr_models_fit <- function(x) .tr_models_porteiro("tr_models_importance", x)
#' @export
tr_models_card.tr_models_fit <- function(x, ctx) .tr_models_porteiro("tr_models_card", x, ctx)
#' @export
tr_models_as_table.tr_models_fit <- function(x) .tr_models_porteiro("tr_models_as_table", x)

# Serializar é o único com default de verdade: o objeto R inteiro no RDS é o
# que sempre se fez, e só quem guarda ponteiro externo precisa de outra coisa.
#' @export
tr_models_serialize.default <- function(x) x
#' @export
tr_models_unserialize.default <- function(x) x

# ---- Conferência (store e leitores) ---------------------------------------------

#' É uma das classes da models (ou um RDS antigo que vira uma)?
#' @noRd
.tr_models_e_proprio <- function(x) {
  class(x)[[1]] %in% c(paste0("tr_models_", .TR_MODELS_CLASSES), "tr_models_fit")
}

#' A descrição do modelo tem a forma que o contrato promete?
#' @noRd
.tr_models_info_conferir <- function(info, x) {
  ruim <- function(motivo) {
    .tr_models_abort("tr_models_error_bad_info",
                     "O modelo de classe '%s' devolveu um tr_models_info() inválido: %s.",
                     class(x)[[1]], motivo)
  }
  if (!is.list(info)) ruim("não é uma lista")
  falta <- setdiff(c("tarefa", "resposta", "preditores", "n", "rotulo"), names(info))
  if (length(falta)) ruim(sprintf("faltam %s", paste(falta, collapse = ", ")))
  if (length(info$tarefa) != 1L || !info$tarefa %in% .TR_MODELS_TAREFAS) {
    ruim("'tarefa' tem de ser \"regressao\" ou \"classificacao\"")
  }
  if (!is.character(info$resposta) || length(info$resposta) != 1L) ruim("'resposta' não é um texto")
  if (!is.character(info$preditores)) ruim("'preditores' não é texto")
  if (info$tarefa == "classificacao" && (!is.character(info$niveis) || length(info$niveis) < 2L)) {
    ruim("classificação sem 'niveis' (texto, dois ou mais)")
  }
  invisible(info)
}

#' O guard do contrato: classe, os campos das classes da models, e o info.
#'
#' Os campos fixos (`.TR_MODELS_CAMPOS_FIT`) só se conferem nas classes daqui:
#' um modelo de outra coleção guarda o que quiser, e responde pelo contrato.
#' @noRd
.tr_models_modelo_conferir <- function(x) {
  if (!inherits(x, "tr_models_fit")) .tr_models_fit_conferir(x)
  if (.tr_models_e_proprio(x)) .tr_models_fit_conferir(x)
  .tr_models_info_conferir(tr_models_info(x), x)
  invisible(x)
}

#' Restaurado sem a coleção dona carregada? Diz qual carregar.
#'
#' Sem isto o objeto voltaria do RDS e o erro sairia no primeiro leitor, como
#' "não implementa tr_models_info" — certo, mas longe da causa.
#' @noRd
.tr_models_exigir_metodos <- function(x) {
  if (.tr_models_e_proprio(x)) return(invisible(x))
  achou <- any(vapply(setdiff(class(x), "tr_models_fit"), function(cl) {
    !is.null(utils::getS3method("tr_models_info", cl, optional = TRUE,
                                envir = asNamespace("trama.models")))
  }, logical(1)))
  if (!achou) .tr_models_sem_metodo("tr_models_info", x)
  invisible(x)
}

# ---- Métodos comuns às classes da models ------------------------------------------

.tr_models_info_proprio <- function(x) {
  f <- stats::as.formula(x$formula)
  list(tarefa = "regressao", resposta = x$resposta,
       # As preditoras pelo NOME da variável — `all.vars()` do lado direito pega
       # `x` dentro de `poly(x, 2)` sem confundir a função com a coluna (e, no
       # misto, o fator de agrupamento de `(1 | bloco)`, que `novos` precisa ter).
       preditores = all.vars(f[[3]]), niveis = NULL, n = nrow(x$dados), rotulo = x$rotulo,
       familia = if (x$classe %in% c("glm", "glmer")) stats::family(x$ajuste)$family else "gaussian")
}

#' Os dois níveis de um GLM binomial de resposta binária, ou NULL.
#'
#' Só é classificação quando a resposta É a classe: fator de dois níveis,
#' lógica, ou número só com 0 e 1. Proporção com pesos ou `cbind(sucessos,
#' fracassos)` continuam regressão — o que se prevê ali é uma taxa, e não há
#' classe observada linha a linha contra a qual conferir. A ordem é a do
#' `glm()`: o primeiro nível é o "fracasso", e a probabilidade prevista é a do
#' SEGUNDO — a mesma convenção da logística da `multi`.
#' @noRd
.tr_models_glm_niveis <- function(x) {
  if (!x$classe %in% c("glm", "glmer") || stats::family(x$ajuste)$family != "binomial") return(NULL)
  lhs <- stats::as.formula(x$formula)[[2]]
  if (!is.name(lhs)) return(NULL)
  y <- x$dados[[as.character(lhs)]]
  if (is.factor(y)) return(if (nlevels(y) == 2L) levels(y) else NULL)
  if (is.logical(y)) return(c("FALSE", "TRUE"))
  if (is.numeric(y) && all(y[!is.na(y)] %in% c(0, 1))) return(c("0", "1"))
  NULL
}

#' @export
tr_models_info.tr_models_glm <- tr_models_info.tr_models_glmer <- function(x) {
  i <- .tr_models_info_proprio(x)
  niv <- .tr_models_glm_niveis(x)
  if (!is.null(niv)) { i$tarefa <- "classificacao"; i$niveis <- niv }
  i
}

#' Probabilidade do segundo nível -> a lista do contrato de classificação.
#'
#' O corte é `x$corte` quando o modelo traz um (a logística da `multi` traz), e
#' 0,5 nos daqui; `>=`, como na `multi`, para que as duas contem igual o empate.
#' @noRd
.tr_models_prev_binaria <- function(p2, niveis, corte = 0.5) {
  classe <- ifelse(p2 >= corte, niveis[[2]], niveis[[1]])
  prob <- cbind(1 - p2, p2)
  colnames(prob) <- niveis
  list(previsto = factor(classe, levels = niveis), prob = prob, extra = NULL)
}

.tr_models_card_proprio <- function(x, ctx) {
  trama::tr_preview("models/fit", data = .tr_models_fit_preview(x))
}

# Registro dos quatro de uma vez: os métodos comuns são os mesmos, e escrever
# 4 × 3 funções idênticas só convidaria uma a divergir. O NAMESPACE declara os
# `S3method()` apontando para estes nomes.
tr_models_info.tr_models_lm <-
  tr_models_info.tr_models_lmer <- tr_models_info.tr_models_split <- .tr_models_info_proprio
tr_models_card.tr_models_lm <- tr_models_card.tr_models_glm <- tr_models_card.tr_models_glmer <-
  tr_models_card.tr_models_lmer <- tr_models_card.tr_models_split <- .tr_models_card_proprio
tr_models_as_table.tr_models_lm <- tr_models_as_table.tr_models_glm <- tr_models_as_table.tr_models_glmer <-
  tr_models_as_table.tr_models_lmer <- function(x) tr_models_coefficients(x)$tabela
# A parcela subdividida não tem coeficientes que se leiam: sai o quadro.
tr_models_as_table.tr_models_split <- function(x) tr_models_anova_table(x)$tabela

# ---- stats ------------------------------------------------------------------------

#' A linha de medidas, em colunas FIXAS, com NA onde a medida não existe.
#' @noRd
.tr_models_stats_linha <- function(modelo, aj, r2 = NA_real_, r2a = NA_real_, r2m = NA_real_,
                                   r2c = NA_real_, dexp = NA_real_, sig = NA_real_, cv = NA_real_,
                                   gl_res = as.numeric(stats::df.residual(aj))) {
  na <- NA_real_
  ll <- tryCatch(stats::logLik(aj), error = function(e) NULL)
  # Os valores saem do objeto ANTES do `tibble()`: lá dentro, `modelo` já é a
  # coluna recém-criada, e `modelo$formula` falharia sobre uma string.
  rotulo <- modelo$rotulo; fml <- modelo$formula; n <- nrow(modelo$dados)
  tibble::tibble(
    modelo = rotulo, formula = fml, n = n,
    gl_residuo = gl_res,
    r2 = r2, r2_ajustado = r2a, r2_marginal = r2m, r2_condicional = r2c,
    desvio_explicado = dexp, sigma = sig, cv_pct = cv,
    aic = if (is.null(ll)) na else stats::AIC(aj), bic = if (is.null(ll)) na else stats::BIC(aj),
    log_verossimilhanca = if (is.null(ll)) na else as.numeric(ll))
}

.tr_models_stats_mq <- function(x, aj) {
  s <- stats::summary.lm(aj)
  .tr_models_stats_linha(x, aj, r2 = s$r.squared, r2a = s$adj.r.squared, sig = s$sigma,
                         cv = .tr_models_cv(x)$cv)
}

#' @export
tr_models_stats.tr_models_lm <- function(x) .tr_models_stats_mq(x, x$ajuste)
# Na parcela subdividida, pelo `lm` auxiliar (os resíduos do erro b).
#' @export
tr_models_stats.tr_models_split <- function(x) .tr_models_stats_mq(x, x$aux_lm)
#' @export
tr_models_stats.tr_models_glm <- function(x) {
  aj <- x$ajuste
  .tr_models_stats_linha(x, aj, dexp = 1 - aj$deviance / aj$null.deviance,
                         sig = if (stats::family(aj)$family == "gaussian") stats::sigma(aj) else NA_real_)
}
#' @export
tr_models_stats.tr_models_lmer <- function(x) {
  aj <- x$ajuste
  r <- .tr_models_r2_misto(aj)
  .tr_models_stats_linha(x, aj, r2m = r[["marginal"]], r2c = r[["condicional"]],
                         sig = stats::sigma(aj), gl_res = NA_real_)
}

#' No GLMM não há R² de uso geral (o de Nakagawa pede a variância da
#' distribuição na escala da ligação, que muda com a família e a ligação): as
#' medidas são as de verossimilhança.
#' @export
tr_models_stats.tr_models_glmer <- function(x) {
  linha <- .tr_models_stats_linha(x, x$ajuste, gl_res = NA_real_)
  # A razão de Pearson (Poisson; NA na binomial), que a nota dos coeficientes
  # usa para avisar sobredispersão acima de 1,5.
  linha$razao_dispersao <- x$dispersao %||% NA_real_
  linha
}

# ---- coefs ------------------------------------------------------------------------

.TR_MODELS_ESCALAS <- c("unidade", "desvio padrão")

#' @export
tr_models_coefs.tr_models_lm <- function(x, exponenciar = FALSE, escala = "unidade", confianca = 0.95, ...) {
  .tr_models_coefs_mq(x, exponenciar, escala, confianca)
}
#' @export
tr_models_coefs.tr_models_glm <- function(x, exponenciar = FALSE, escala = "unidade", confianca = 0.95, ...) {
  .tr_models_coefs_mq(x, exponenciar, escala, confianca)
}
#' @export
tr_models_coefs.tr_models_lmer <- function(x, exponenciar = FALSE, escala = "unidade", confianca = 0.95, ...) {
  if (isTRUE(exponenciar)) {
    .tr_models_exigir(x, "glm", "models/coefficients",
                      "Exponenciar só faz sentido num GLM com ligação log ou logit.")
  }
  s <- stats::coef(summary(x$ajuste))
  ic <- suppressMessages(stats::confint(x$ajuste, method = "Wald", parm = "beta_", level = confianca))
  tab <- data.frame(termo = rownames(s), estimativa = s[, "Estimate"], erro_padrao = s[, "Std. Error"],
                    gl = s[, "df"], t = s[, "t value"], p_valor = s[, "Pr(>|t|)"],
                    li = ic[rownames(s), 1], ls = ic[rownames(s), 2])
  tab <- .tr_models_coefs_escala(tab, lme4::getME(x$ajuste, "X"), escala)
  .tr_models_coefs_efeitos(x, .tr_models_coefs_ic_nomes(tab, confianca), "t",
                           .tr_models_nota("gl de Satterthwaite; intervalo de Wald", .tr_models_nota_escala(escala)))
}
#' Coeficientes do GLMM: z de Wald e intervalo de Wald, como no `summary()` do
#' `lme4`; o perfil de verossimilhança reajusta o modelo dezenas de vezes.
#' @export
tr_models_coefs.tr_models_glmer <- function(x, exponenciar = FALSE, escala = "unidade", confianca = 0.95, ...) {
  confianca <- .tr_models_num(confianca, "confianca", min = 0.5, max = 0.999)
  s <- stats::coef(summary(x$ajuste))
  z <- stats::qnorm(1 - (1 - confianca) / 2)
  tab <- data.frame(termo = rownames(s), estimativa = s[, 1], erro_padrao = s[, 2], z = s[, 3], p_valor = s[, 4],
                    li = s[, 1] - z * s[, 2], ls = s[, 1] + z * s[, 2])
  tab <- .tr_models_coefs_escala(tab, lme4::getME(x$ajuste, "X"), escala)
  nota <- "intervalo de Wald, na escala da ligação"
  if (isTRUE(exponenciar)) {
    tab$estimativa <- exp(tab$estimativa); tab$li <- exp(tab$li); tab$ls <- exp(tab$ls)
    tab$erro_padrao <- NULL
    nota <- "estimativa e intervalo exponenciados (razão de chances ou de taxas)"
  }
  .tr_models_coefs_efeitos(x, .tr_models_coefs_ic_nomes(tab, confianca), "z",
                           .tr_models_nota(nota, .tr_models_nota_escala(escala),
                                           if (length(x$avisos)) paste(x$avisos, collapse = " | ") else ""))
}
#' @export
tr_models_coefs.tr_models_split <- function(x, ...) {
  .tr_models_exigir(x, c("lm", "glm", "lmer"), "models/coefficients",
                    "Na parcela subdividida os coeficientes misturam os dois erros; compare as médias em 'models/emmeans'.")
}

#' Coeficiente "por desvio padrão" da coluna da matriz de design.
#'
#' Porte do `escala = "desvio padrão"` da logística da `multi`, na forma
#' genérica: estimativa, erro padrão e limites multiplicados pelo DP da coluna
#' do `model.matrix` — "quanto muda a resposta (ou o log-odds) quando a
#' preditora sobe um desvio padrão", que deixa comparáveis preditoras em
#' unidades diferentes. A estatística e o p-valor não mudam (numerador e
#' denominador escalam juntos). O intercepto e a coluna constante ficam como
#' estão. Numa dummy de fator o DP é o da 0/1, que é o que o livro faz, mas lê
#' pior — a nota do card avisa.
#' @noRd
.tr_models_coefs_escala <- function(tab, X, escala) {
  if (.tr_models_enum(escala, .TR_MODELS_ESCALAS, "escala") == "unidade") return(tab)
  dp <- apply(X, 2L, stats::sd)[tab$termo]
  dp[is.na(dp) | dp == 0] <- 1
  for (col in intersect(c("estimativa", "erro_padrao", "li", "ls"), names(tab))) tab[[col]] <- tab[[col]] * dp
  tab
}

.tr_models_nota_escala <- function(escala) {
  if (identical(escala, "desvio padrão")) "coeficientes por desvio padrão da preditora (dummy de fator: DP da 0/1)" else ""
}

#' `li`/`ls` com o nível no nome: `li_95` no padrão (o nome de sempre, que o
#' card e os fluxos a jusante já leem), `li_90` a 90%.
#' @noRd
.tr_models_coefs_ic_nomes <- function(tab, confianca) {
  pct <- sub(".", "_", as.character(round(100 * confianca, 1)), fixed = TRUE)
  names(tab)[names(tab) == "li"] <- paste0("li_", pct)
  names(tab)[names(tab) == "ls"] <- paste0("ls_", pct)
  tab
}

.tr_models_coefs_mq <- function(x, exponenciar, escala = "unidade", confianca = 0.95) {
  confianca <- .tr_models_num(confianca, "confianca", min = 0.5, max = 0.999)
  aj <- x$ajuste
  glm <- x$classe == "glm"
  # `summary.lm` explícito: nos delineamentos o ajuste é um `aov`, e o
  # `summary()` dele é o quadro, sem coeficientes.
  s <- if (glm) stats::coef(summary(aj)) else stats::coef(stats::summary.lm(aj))
  ic <- if (glm) stats::confint.default(aj, level = confianca) else stats::confint(aj, level = confianca)
  coluna <- if (grepl("^z", colnames(s)[3])) "z" else "t"
  tab <- data.frame(termo = rownames(s), estimativa = s[, 1], erro_padrao = s[, 2],
                    estat = s[, 3], p_valor = s[, 4], li = ic[rownames(s), 1], ls = ic[rownames(s), 2])
  names(tab)[names(tab) == "estat"] <- coluna
  tab <- .tr_models_coefs_escala(tab, stats::model.matrix(aj), escala)
  nota <- if (glm) "intervalo de Wald, na escala da ligação" else ""
  if (isTRUE(exponenciar)) {
    .tr_models_exigir(x, "glm", "models/coefficients",
                      "Exponenciar só faz sentido num GLM com ligação log ou logit.")
    tab$estimativa <- exp(tab$estimativa); tab$li <- exp(tab$li); tab$ls <- exp(tab$ls)
    tab$erro_padrao <- NULL
    nota <- "estimativa e intervalo exponenciados (razão de chances ou de taxas)"
  }
  .tr_models_coefs_efeitos(x, .tr_models_coefs_ic_nomes(tab, confianca), coluna,
                           .tr_models_nota(nota, .tr_models_nota_escala(escala)))
}

.tr_models_coefs_efeitos <- function(x, tab, coluna, nota) {
  rownames(tab) <- NULL
  .tr_models_efeitos(tibble::as_tibble(tab), "Coeficientes", coluna_estat = coluna,
                     rodape = list(n = as.character(nrow(x$dados))),
                     nota = .tr_models_nota(nota, .tr_models_nota_descarte(x$descartadas)),
                     fonte = "")
}

# ---- resid ------------------------------------------------------------------------

#' A tabela do ajuste com os resíduos à direita.
#' @noRd
.tr_models_resid_tabela <- function(x, aj, padronizado, tipo = "response") {
  d <- x$dados
  nomes <- c("ajustado", "residuo", "residuo_padronizado")
  # Coluna que já existe na tabela ganha sufixo, em vez de ser sobrescrita: um
  # `residuo` de outra análise perdido em silêncio é o tipo de coisa que só se
  # descobre no artigo.
  nomes <- ifelse(nomes %in% names(d), paste0(nomes, "_modelo"), nomes)
  d[[nomes[[1]]]] <- as.numeric(stats::fitted(aj))
  d[[nomes[[2]]]] <- as.numeric(stats::residuals(aj, type = tipo))
  d[[nomes[[3]]]] <- as.numeric(padronizado)
  d
}

#' @export
tr_models_resid.tr_models_lm <- function(x) .tr_models_resid_tabela(x, x$ajuste, stats::rstandard(x$ajuste))
#' @export
tr_models_resid.tr_models_split <- function(x) .tr_models_resid_tabela(x, x$aux_lm, stats::rstandard(x$aux_lm))
#' @export
tr_models_resid.tr_models_glm <- function(x) {
  .tr_models_resid_tabela(x, x$ajuste, stats::rstandard(x$ajuste, type = "deviance"), tipo = "deviance")
}
#' @export
tr_models_resid.tr_models_glmer <- function(x) {
  .tr_models_resid_tabela(x, x$ajuste, stats::residuals(x$ajuste, type = "pearson"), tipo = "deviance")
}
#' @export
tr_models_resid.tr_models_lmer <- function(x) {
  .tr_models_resid_tabela(x, x$ajuste, stats::residuals(x$ajuste, type = "pearson", scaled = TRUE))
}

# ---- predict_raw / predict_cv -------------------------------------------------------

.tr_models_prev <- function(previsto, extra = NULL) list(previsto = previsto, prob = NULL, extra = extra)

#' `predict()` do ajuste em `novos`, com o intervalo quando pedido.
#' @noRd
.tr_models_predict_ajuste <- function(x, novos, intervalo = "nenhum", confianca = 0.95, ...) {
  args <- list(object = x$ajuste, newdata = novos)
  # GLM: sem isto, o default de `predict.glm()` é a escala da LIGAÇÃO (log,
  # logit), e o card mostraria log-odds ou log-contagem como "previsto" sem
  # avisar — plausível e ilegível.
  if (x$classe %in% c("glm", "glmer")) args$type <- "response"
  if (intervalo != "nenhum") {
    args$interval <- if (intervalo == "confianca") "confidence" else "prediction"
    args$level <- confianca
  }
  pred <- .tr_models_ajustar(do.call(stats::predict, args), "models/predict")
  if (is.matrix(pred)) {
    .tr_models_prev(unname(pred[, "fit"]),
                    tibble::tibble(li = unname(pred[, "lwr"]), ls = unname(pred[, "upr"])))
  } else {
    .tr_models_prev(as.numeric(pred))
  }
}

#' @export
tr_models_predict_raw.tr_models_lm <- function(x, novos, ...) .tr_models_predict_ajuste(x, novos, ...)
#' @export
tr_models_predict_raw.tr_models_glm <- tr_models_predict_raw.tr_models_glmer <- function(x, novos, ...) {
  p <- .tr_models_predict_ajuste(x, novos, ...)
  niv <- .tr_models_glm_niveis(x)
  if (is.null(niv)) p else .tr_models_prev_binaria(p$previsto, niv, x$corte %||% 0.5)
}
#' @export
tr_models_predict_raw.tr_models_lmer <- function(x, novos, ...) .tr_models_predict_ajuste(x, novos, ...)
#' @export
tr_models_predict_raw.tr_models_split <- function(x, novos, ...) .tr_models_split_sem_predict()

.tr_models_split_sem_predict <- function() {
  .tr_models_abort("tr_models_error_not_applicable",
                   paste0("'models/predict' não se aplica à parcela subdividida: o ajuste é uma ",
                          "lista de modelos (aovlist, um por estrato de erro), sem um predict() só. ",
                          "Ajuste o misto equivalente em 'models/lmer' para prever."))
}

#' A cruzada do lm, fechada pela alavanca: sem reajustar nada.
#'
#' Tirar a linha i e reajustar dá o resíduo `e_i / (1 − h_ii)` (a identidade
#' do PRESS), então a previsão deixa-um-fora é `y_i − e_i / (1 − h_ii)` —
#' exata, e não uma aproximação, para mínimos quadrados. Um `update()` por
#' linha daria o mesmo número n vezes mais devagar. Linha de alavanca 1 (a
#' única de um nível) não tem previsão sem ela: sai NA.
#' @noRd
.tr_models_loo_lm <- function(aj) {
  h <- stats::hatvalues(aj)
  e <- stats::residuals(aj)
  y <- stats::fitted(aj) + e
  p <- as.numeric(y - e / (1 - h))
  p[h > 1 - 1e-10] <- NA_real_
  p
}

#' A cruzada do GLM por reajuste: n ajustes, cada um sem uma linha.
#'
#' Reajuste com fórmula e família do objeto, e não `update()`: a chamada
#' guardada no `glm` cita variáveis da função que ajustou (`f`, `d`), que não
#' existem mais aqui — nem depois do RDS.
#'
#' Sem fórmula fechada exata (a do hat é aproximação de um passo do IRLS). A
#' linha cujo nível sumiu do treino não tem previsão: sai NA, em vez de
#' derrubar as outras.
#' @noRd
.tr_models_loo_glm <- function(x) {
  d <- x$dados
  vapply(seq_len(nrow(d)), function(i) {
    tryCatch({
      m <- suppressWarnings(stats::glm(stats::formula(x$ajuste), family = stats::family(x$ajuste),
                                        data = d[-i, , drop = FALSE]))
      as.numeric(stats::predict(m, newdata = d[i, , drop = FALSE], type = "response"))
    }, error = function(e) NA_real_)
  }, numeric(1))
}

.tr_models_validacao <- function(validacao) .tr_models_enum(validacao, .TR_MODELS_VALIDACOES, "validacao")

.tr_models_sem_cruzada <- function(x) {
  .tr_models_abort("tr_models_error_not_applicable",
                   paste0("Validação 'cruzada' não se aplica a %s: deixar uma linha de fora quebra a ",
                          "estrutura de erro do misto/parcela subdividida. Use 'resubstituição'."),
                   x$rotulo)
}

#' @export
tr_models_predict_cv.tr_models_lm <- function(x, validacao = "resubstituição") {
  if (.tr_models_validacao(validacao) == "cruzada") return(.tr_models_prev(.tr_models_loo_lm(x$ajuste)))
  .tr_models_prev(as.numeric(stats::fitted(x$ajuste)))
}
#' @export
tr_models_predict_cv.tr_models_glm <- function(x, validacao = "resubstituição") {
  p <- if (.tr_models_validacao(validacao) == "cruzada") .tr_models_loo_glm(x) else
    as.numeric(stats::fitted(x$ajuste))  # `fitted.glm` já é a escala da resposta
  niv <- .tr_models_glm_niveis(x)
  if (is.null(niv)) .tr_models_prev(p) else .tr_models_prev_binaria(p, niv, x$corte %||% 0.5)
}
#' @export
tr_models_predict_cv.tr_models_lmer <- function(x, validacao = "resubstituição") {
  if (.tr_models_validacao(validacao) == "cruzada") .tr_models_sem_cruzada(x)
  .tr_models_prev(as.numeric(stats::fitted(x$ajuste)))
}
#' @export
tr_models_predict_cv.tr_models_glmer <- function(x, validacao = "resubstituição") {
  if (.tr_models_validacao(validacao) == "cruzada") .tr_models_sem_cruzada(x)
  p <- as.numeric(stats::fitted(x$ajuste))  # no `glmer`, já na escala da resposta
  niv <- .tr_models_glm_niveis(x)
  if (is.null(niv)) .tr_models_prev(p) else .tr_models_prev_binaria(p, niv, 0.5)
}
#' @export
tr_models_predict_cv.tr_models_split <- function(x, validacao = "resubstituição") {
  if (.tr_models_validacao(validacao) == "cruzada") .tr_models_sem_cruzada(x)
  .tr_models_prev(as.numeric(stats::fitted(x$aux_lm)))
}

# ---- importance -------------------------------------------------------------------

#' Importância do lm/glm: |t| (|z| no GLM de dispersão fixa) de cada coeficiente.
#'
#' É a ESTATÍSTICA t em valor absoluto, não o coeficiente: não depende da
#' escala da preditora (multiplicar x por 1000 divide o coeficiente e o erro
#' padrão juntos), que é o que "padronizado" pede aqui. Não é importância por
#' permutação, e a coluna `medida` diz qual foi, para ninguém comparar com a da
#' `ml` como se fosse a mesma régua. Sem o intercepto.
#' @noRd
.tr_models_importancia_t <- function(x) {
  tab <- tr_models_coefs(x)
  col <- tab$coluna_estat
  t <- tab$tabela[tab$tabela$termo != "(Intercept)", , drop = FALSE]
  out <- tibble::tibble(termo = t$termo, importancia = abs(t[[col]]), medida = paste0("|", col, "|"))
  out[order(-out$importancia), , drop = FALSE]
}

#' @export
tr_models_importance.tr_models_lm <- function(x) .tr_models_importancia_t(x)
#' @export
tr_models_importance.tr_models_glm <- function(x) .tr_models_importancia_t(x)
#' @export
tr_models_importance.tr_models_lmer <- function(x) .tr_models_sem_importancia(x)
#' @export
tr_models_importance.tr_models_glmer <- function(x) .tr_models_sem_importancia(x)
#' @export
tr_models_importance.tr_models_split <- function(x) .tr_models_sem_importancia(x)

.tr_models_sem_importancia <- function(x) {
  .tr_models_abort("tr_models_error_not_applicable",
                   paste0("'models/importance' não se aplica a %s: o |t| de um efeito fixo depende da ",
                          "estrutura aleatória. Leia os coeficientes ou o quadro da ANOVA."), x$rotulo)
}
