# Ponte para o ecossistema tidy: `tidy()`, `glance()` e `augment()` sobre
# qualquer `tr_models_fit`. Não calcula nada: chama os genéricos do contrato
# (`tr_models_coefs`, `tr_models_stats`, `tr_models_resid`,
# `tr_models_predict_raw`) e só traduz os nomes de coluna para os do `broom`
# na fronteira — o glossário em português (`termo`, `p_valor`) continua sendo o
# da trama. Os genéricos vêm do pacote `generics`, o mesmo que o `broom` usa,
# então `modelsummary`, `gtsummary` e afins leem o modelo sem `broom` instalado.

#' Tabelas tidy de um modelo da trama
#'
#' Métodos de [generics::tidy()], [generics::glance()] e [generics::augment()]
#' para todo objeto do tipo `models/fit` (inclusive os de outras coleções que
#' cumprem [tr_models_contract]). Os números são os de [tr_models_coefs()],
#' [tr_models_stats()] e [tr_models_resid()]; muda só o nome das colunas.
#'
#' - `tidy()`: `term`, `estimate`, `std.error`, `statistic`, `p.value` e, com
#'   `conf.int = TRUE`, `conf.low` e `conf.high`. Colunas extras do modelo
#'   (`gl`, `grupo`) seguem no fim, com o nome original.
#' - `glance()`: uma linha; `nobs`, `df.residual`, `r.squared`,
#'   `adj.r.squared`, `sigma`, `logLik`, `AIC`, `BIC` no nome do `broom`, e as
#'   demais medidas (`cv_pct`, `r2_marginal`, ...) com o nome da trama.
#' - `augment()`: sem `newdata`, a tabela do ajuste com `.fitted`, `.resid` e
#'   `.std.resid` (só regressão, como [tr_models_resid()]). Com `newdata`,
#'   `.fitted` na regressão, ou `.pred_class` e `.prob_<nível>` na
#'   classificação.
#'
#' @param x um modelo (`inherits(x, "tr_models_fit")`).
#' @param conf.int inclui `conf.low` e `conf.high`?
#' @param conf.level nível do intervalo (0,5 a 0,999).
#' @param exponentiate exponencia estimativa e intervalo (razão de chances ou
#'   de taxas; só GLM com ligação logit ou log).
#' @param newdata data.frame com as colunas de `tr_models_info(x)$preditores`.
#' @param ... em `tidy()`, repassados a [tr_models_coefs()] (`escala`, `intervalo`);
#'   nos outros, ignorados. Com `conf.int = FALSE` o intervalo ainda é calculado
#'   pelo leitor, só não é devolvido.
#' @return um tibble.
#' @name tr_models_tidy
NULL

.tr_models_renomear <- function(tab, de, para) {
  i <- match(de, names(tab))
  names(tab)[i[!is.na(i)]] <- para[!is.na(i)]
  tab
}

#' @rdname tr_models_tidy
#' @export
tidy.tr_models_fit <- function(x, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE, ...) {
  if (!is.numeric(conf.level) || length(conf.level) != 1L || is.na(conf.level) || conf.level < 0.5 || conf.level > 0.999) {
    .tr_models_abort("tr_models_error_bad_option", "'conf.level' tem de ser um n\u00FAmero entre 0,5 e 0,999.")
  }
  ef <- tr_models_coefs(x, ..., exponenciar = isTRUE(exponentiate), confianca = conf.level)
  tab <- as.data.frame(ef$tabela)
  li <- grep("^li_", names(tab), value = TRUE)
  ls <- grep("^ls_", names(tab), value = TRUE)
  tab <- .tr_models_renomear(tab, c("termo", "estimativa", "erro_padrao", ef$coluna_estat, "p_valor", li, ls),
                             c("term", "estimate", "std.error", "statistic", "p.value",
                               rep("conf.low", length(li)), rep("conf.high", length(ls))))
  if (!isTRUE(conf.int)) tab <- tab[setdiff(names(tab), c("conf.low", "conf.high"))]
  first <- intersect(c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high"), names(tab))
  tibble::as_tibble(tab[c(first, setdiff(names(tab), first))])
}

#' @rdname tr_models_tidy
#' @export
glance.tr_models_fit <- function(x, ...) {
  tibble::as_tibble(.tr_models_renomear(tr_models_stats(x),
                      c("n", "gl_residuo", "r2", "r2_ajustado", "log_verossimilhanca", "aic", "bic"),
                      c("nobs", "df.residual", "r.squared", "adj.r.squared", "logLik", "AIC", "BIC")))
}

#' @rdname tr_models_tidy
#' @export
augment.tr_models_fit <- function(x, newdata = NULL, ...) {
  if (is.null(newdata)) {
    tab <- tr_models_resid(x)
    # Pelo NOME, não pela posição: a `ml` devolve só `ajustado` e `residuo`, e o
    # leitor põe sufixo `_modelo` quando a tabela já tinha coluna de mesmo nome.
    base <- c("ajustado", "residuo", "residuo_padronizado")
    de <- ifelse(paste0(base, "_modelo") %in% names(tab), paste0(base, "_modelo"), base)
    tab <- .tr_models_renomear(tab, de, c(".fitted", ".resid", ".std.resid"))
    return(tibble::as_tibble(tab))
  }
  info <- tr_models_info(x)
  falta <- setdiff(info$preditores, names(newdata))
  if (length(falta)) {
    .tr_models_abort("tr_models_error_unknown_column",
                     "'newdata' n\u00E3o tem a(s) coluna(s) %s, que o modelo usa.", paste(falta, collapse = ", "))
  }
  p <- tr_models_predict_raw(x, as.data.frame(newdata))
  novas <- if (info$tarefa == "classificacao") {
    c(list(.pred_class = p$previsto),
      if (!is.null(p$prob)) {
        pr <- as.data.frame(p$prob)
        stats::setNames(as.list(pr), paste0(".prob_", tr_models_clean_name(colnames(p$prob))))
      })
  } else {
    c(list(.fitted = p$previsto),
      if (!is.null(p$extra)) stats::setNames(as.list(p$extra), paste0(".", names(p$extra))))
  }
  colide <- intersect(names(novas), names(newdata))
  if (length(colide)) {
    .tr_models_abort("tr_models_error_bad_option",
                     "'newdata' j\u00E1 tem a(s) coluna(s) %s, que o augment() cria; renomeie-as.",
                     paste(colide, collapse = ", "))
  }
  out <- tibble::as_tibble(newdata)
  for (k in names(novas)) out[[k]] <- novas[[k]]
  out
}
