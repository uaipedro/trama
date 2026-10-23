# Grupo multivariado "causalidade": nós e docs (ver mv_registro.R).
#
# Granger é precedência PREDITIVA: a série X ajuda a prever Y além do passado
# de Y. Não é causalidade estrutural, e o card diz isso na ajuda.

#' Causalidade de Granger sobre um VAR em nível.
#'
#' `metodo = "wald"`: o F de Wald do próprio VAR, como `vars::causality()`.
#' Vale com séries estacionárias.
#'
#' `metodo = "toda_yamamoto"` (Toda & Yamamoto, 1995): reajusta o VAR com
#' `p + d_max` defasagens, em nível, e testa por qui-quadrado só as `p`
#' primeiras defasagens da causa nas equações das outras séries. As defasagens
#' extras existem para que o teste tenha distribuição qui-quadrado mesmo com
#' raiz unitária ou cointegração; `d_max` é a maior ordem de integração entre
#' as séries (ADF a 5%, `forecast::ndiffs`).
#'
#' `metodo = "instantanea"` (Lütkepohl, 2005, seção 3.6.3): Wald qui-quadrado
#' sobre as covariâncias dos resíduos entre a causa e o resto, como o
#' `$Instant` de `vars::causality()`. É simétrico: não tem direção.
#' @export
tr_series_granger <- function(ajuste, causa = "", metodo = "wald") {
  .tr_series_guard_var(ajuste, "series/granger")
  metodo <- .tr_series_enum(metodo, c("wald", "toda_yamamoto", "instantanea"), "metodo")
  if (identical(ajuste$tipo, "VECM") || inherits(ajuste$ajuste, "vec2var")) {
    .tr_series_abort("tr_series_error_vecm_granger",
                     paste0("'series/granger' recebe um VAR em nível (series/var). Para um VECM, ",
                            "o teste de Granger não tem a forma usual aqui: use series/var ",
                            "sobre as séries diferenciadas, ou o método toda_yamamoto com um VAR em nível. ",
                            "A causalidade instantânea também pede o VAR em nível."))
  }
  serie <- ajuste$serie
  .tr_series_obrigatorio(causa, "causa")
  causas <- .tr_series_causa(causa, serie)
  resto <- setdiff(colnames(serie), causas)
  txt_causa <- paste(causas, collapse = " e ")
  txt_resto <- paste(resto, collapse = " e ")
  h0 <- sprintf("%s não Granger-causa %s", txt_causa, txt_resto)
  if (metodo == "instantanea") return(.tr_series_granger_instantanea(ajuste, causas, txt_causa, txt_resto))
  if (metodo == "wald") {
    r <- .tr_series_granger_wald(ajuste, causas)
    estat <- r$estatistica
    rotulo <- "F"
    p <- r$p_valor
    nota <- sprintf("F de Wald do VAR, com %g e %g graus de liberdade", r$gl[[1]], r$gl[[2]])
    fonte <- "Granger (1969); Pfaff (2008), vars::causality"
  } else {
    dmax <- .tr_series_dmax(serie)
    r <- .tr_series_granger_ty(ajuste, causas, dmax)
    estat <- r$estatistica
    rotulo <- "qui-quadrado"
    p <- r$p_valor
    nota <- sprintf(paste0("VAR(%d) reajustado com p + d_max = %d + %d; qui-quadrado com %d gl; ",
                           "d_max = maior ordem de integração entre as séries (ADF a 5%%)"),
                    r$p_total, r$p_lags, dmax, r$gl)
    fonte <- "Toda e Yamamoto (1995)"
  }
  res <- .tr_series_teste("Granger", h0, estat, rotulo, p_valor = p, gl = r$gl,
    sentido = "maior",
    conclusao_sim = sprintf("%s Granger-causa %s: ajuda a prever, além do próprio passado", txt_causa, txt_resto),
    conclusao_nao = sprintf("não há precedência preditiva de %s sobre %s", txt_causa, txt_resto),
    nota = nota, fonte = fonte)
  .tr_series_ferramentas(res, if (metodo == "wald") "vars::causality" else c("vars::VAR", "forecast::ndiffs"))
}

#' Os nomes da causa: separados por vírgula, todos colunas da série múltipla.
#' @noRd
.tr_series_causa <- function(causa, serie) {
  nomes <- trimws(strsplit(as.character(causa)[[1]], ",", fixed = TRUE)[[1]])
  nomes <- unique(nomes[nzchar(nomes)])
  for (nm in nomes) if (!nm %in% colnames(serie)) .tr_series_option("causa", nm, colnames(serie))
  if (length(setdiff(colnames(serie), nomes)) == 0L) {
    .tr_series_abort("tr_series_error_granger_sem_resto",
                     paste0("'series/granger': a causa cobre todas as séries do VAR, e não sobra ",
                            "nenhuma para ela prever. Deixe pelo menos uma série fora da causa."))
  }
  nomes
}

#' Wald do `vars`: F com os graus de liberdade do próprio pacote.
#' @noRd
.tr_series_granger_wald <- function(ajuste, causas) {
  g <- vars::causality(ajuste$ajuste, cause = causas)$Granger
  list(estatistica = unname(g$statistic[[1]]), gl = unname(g$parameter),
       p_valor = unname(g$p.value[[1]]))
}

#' Causalidade instantânea: o `$Instant` de `vars::causality()`.
#'
#' Wald sobre os elementos de Sigma_u (estimada com T - Kp - 1 no divisor, a
#' do `vars`) que ligam a causa ao resto; qui-quadrado com um grau de
#' liberdade por covariância testada. A variância assintótica usada é a do
#' caso gaussiano, 2 D+ (Sigma ⊗ Sigma) D+'.
#' @noRd
.tr_series_granger_instantanea <- function(ajuste, causas, txt_causa, txt_resto) {
  g <- vars::causality(ajuste$ajuste, cause = causas)$Instant
  gl <- unname(g$parameter[[1]])
  res <- .tr_series_teste("Causalidade instantânea",
    sprintf("não há causalidade instantânea entre %s e %s", txt_causa, txt_resto),
    unname(g$statistic[[1]]), "qui-quadrado", p_valor = unname(g$p.value[[1]]), gl = gl,
    sentido = "maior",
    conclusao_sim = sprintf("há causalidade instantânea entre %s e %s: os choques se correlacionam no mesmo período",
                            txt_causa, txt_resto),
    conclusao_nao = sprintf("não há evidência de correlação contemporânea entre os choques de %s e de %s",
                            txt_causa, txt_resto),
    nota = sprintf(paste0("Wald sobre as covariâncias dos resíduos entre %s e %s; qui-quadrado com %g gl; ",
                          "simétrico, sem direção"), txt_causa, txt_resto, gl),
    fonte = "Lütkepohl (2005, seção 3.6.3); Pfaff (2008), vars::causality")
  .tr_series_ferramentas(res, "vars::causality")
}

#' Toda-Yamamoto: VAR(p + dmax) em nível; Wald qui-quadrado nas p primeiras
#' defasagens da causa, em todas as equações das outras séries.
#'
#' A matriz de covariância dos coeficientes é Sigma ⊗ (X'X)^-1, com Sigma =
#' R'R / (n - k) — a de um SUR com regressores iguais, que é o caso do VAR. Sem
#' o termo cruzado entre equações, o teste ignoraria a correlação dos erros.
#' @noRd
.tr_series_granger_ty <- function(ajuste, causas, dmax) {
  serie <- ajuste$serie
  p <- ajuste$ajuste$p
  aj <- .tr_series_ajustar(
    vars::VAR(serie, p = p + dmax, type = ajuste$ajuste$type,
              season = if (.tr_series_tem_sazonal(ajuste$ajuste)) stats::frequency(serie)),
    "series/granger")
  eqs <- names(aj$varresult)
  nomes_coef <- names(stats::coef(aj$varresult[[1]]))
  k <- length(nomes_coef)
  outras <- setdiff(eqs, causas)
  # Posições (equação-major) dos coeficientes a zerar: lags 1..p da causa,
  # nas equações das outras séries.
  lags_causa <- as.vector(outer(paste0(causas, ".l"), seq_len(p), paste0))
  idx <- unlist(lapply(outras, function(eq) {
    (match(eq, eqs) - 1L) * k + match(lags_causa, nomes_coef)
  }))
  if (anyNA(idx)) {
    .tr_series_abort("tr_series_error_bad_option",
                     "'series/granger': defasagem da causa não encontrada no VAR reajustado.")
  }
  b <- unlist(lapply(aj$varresult, stats::coef), use.names = FALSE)
  # O model.matrix do vars traz um "(Intercept)" extra (fórmula com -1 + (...)): fica só o da equação.
  X <- stats::model.matrix(aj$varresult[[1]])[, nomes_coef, drop = FALSE]
  res <- do.call(cbind, lapply(aj$varresult, stats::residuals))
  n <- nrow(res)
  sigma <- crossprod(res) / (n - k)
  V <- kronecker(sigma, chol2inv(qr.R(qr(X))))  # QR, como o lm (crossprod + solve perde precisão aqui)
  rb <- b[idx]
  w <- drop(t(rb) %*% solve(V[idx, idx, drop = FALSE]) %*% rb)
  q <- length(idx)
  list(estatistica = w, gl = q, p_valor = stats::pchisq(w, df = q, lower.tail = FALSE),
       p_total = p + dmax, p_lags = p)
}

#' O `vars` põe dummies sazonais com nomes `sd1`, `sd2`...
#' @noRd
.tr_series_tem_sazonal <- function(aj) {
  any(grepl("^sd[0-9]+$", names(stats::coef(aj$varresult[[1]]))))
}

#' Maior ordem de integração entre as séries (ADF a 5%, sequencial).
#' @noRd
.tr_series_dmax <- function(serie) {
  max(vapply(colnames(serie), function(nm) {
    as.integer(forecast::ndiffs(serie[, nm], test = "adf"))
  }, integer(1)))
}

.tr_series_nos_mv_causalidade <- function() {
  VAR <- "series/var"; TE <- "data/test"
  E <- trama::tr_param_enum; P <- trama::tr_param
  icone <- function(n) trama::tr_icon(n)
  list(
    trama::tr_node("series/granger", fn = tr_series_granger, label = "Causalidade de Granger", version = 2L,
      pressupostos = .tr_series_doc("series/granger")$pressupostos,
      referencias = .tr_series_doc("series/granger")$referencias,
      category = "serie_mv_testes", icon = icone("arrow-right-left"),
      description = "Uma série ajuda a prever as outras, além do passado delas? (precedência preditiva)",
      inputs = list(ajuste = VAR), outputs = list(out = TE),
      params = list(
        causa = P("text", "", label = "Causa", example = "renda"),
        metodo = E("wald", c("wald", "toda_yamamoto", "instantanea"), label = "Método")),
      help = .tr_series_ajuda(r"---[
Testa se a série (ou as séries) da CAUSA ajuda a prever as demais do VAR,
além do que as próprias demais já dizem sobre si. A hipótese nula é que a
causa NÃO Granger-causa o resto.

Isto é PRECEDÊNCIA PREDITIVA: se o passado de X melhora a previsão de Y, X vem
antes de Y e carrega informação útil. Não é causalidade estrutural. Uma
precedência pode vir de uma terceira variável, de expectativas ou de um
atraso de medição; rejeitar não prova que X produz Y.

Escolha o método pelo tipo de série:
- **Wald** (padrão): o F do próprio VAR. Pede séries ESTACIONÁRIAS. Com raiz
  unitária, o F não tem a distribuição nominal e o p-valor engana.
- **Toda-Yamamoto**: serve a séries em NÍVEL, integradas ou cointegradas. Reajusta
  o VAR com defasagens extras (a maior ordem de integração) e testa só as
  primeiras p. É o método seguro quando não se sabe se as séries são
  estacionárias.
- **Instantânea**: outra pergunta. Não olha o passado: testa se os CHOQUES da
  causa e os das demais séries se correlacionam no mesmo período, pelas
  covariâncias dos resíduos do VAR. É simétrica (X com Y é o mesmo teste que Y
  com X) e não tem direção; rejeitar diz que um choque numa série vem junto com
  um choque na outra, o que também faz a IRF ortogonal depender da ordem.

O VECM não entra aqui: o bloco recebe o VAR em nível (series/var).
]---", r"---[
- **Causa** — uma ou mais séries, separadas por vírgula. A hipótese testada é
  que elas não Granger-causam as DEMAIS séries do VAR, em conjunto.
- **Método** — Wald (séries estacionárias), Toda-Yamamoto (nível, integradas
  ou cointegradas) ou instantânea (correlação contemporânea dos choques).
]---", r"---[
Um teste (`data/test`) com H0 "causa não Granger-causa resto", a estatística
(F para Wald, qui-quadrado para Toda-Yamamoto) e o p-valor. Na instantânea, a
H0 é "não há causalidade instantânea entre causa e resto", com qui-quadrado.
]---", r"---[
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac")) |>
  tr_add("v", "series/var", defasagens = 2L, from = "j") |>
  tr_add("g", "series/granger", causa = "dax", metodo = "toda_yamamoto", from = c(ajuste = "v"))
]---", r"---[
`series/var`, que ajusta o modelo; `series/var_select`, para escolher p antes;
`series/irf`, o efeito dinâmico de um choque.
]---", teste = TRUE))
  )
}

.tr_series_docs_mv_causalidade <- function() {
  R <- trama::tr_ref
  P <- .tr_series_P
  granger <- R(autores = "Granger, C. W. J.", ano = 1969,
               titulo = "Investigating causal relations by econometric models and cross-spectral methods",
               fonte = "Econometrica, 37(3), 424-438", doi = "10.2307/1912791", papel = "teoria")
  toda <- R(autores = c("Toda, H. Y.", "Yamamoto, T."), ano = 1995,
            titulo = "Statistical inference in vector autoregressions with possibly integrated processes",
            fonte = "Journal of Econometrics, 66(1-2), 225-250", doi = "10.1016/0304-4076(94)01616-8",
            papel = "teoria")
  lutkepohl <- R(autores = "Lütkepohl, H.", ano = 2005,
                 titulo = "New Introduction to Multiple Time Series Analysis",
                 fonte = "Berlin: Springer", doi = "10.1007/978-3-540-27752-1", papel = "livro-texto")
  list(
    "series/granger" = list(
      pressupostos = list(
        P("Pelo método Wald, as séries são estacionárias: com raiz unitária o F não segue a distribuição nominal. Pelo Toda-Yamamoto, basta que a ordem de integração esteja bem estimada.",
          verificar = c("series/adf", "series/kpss"),
          se_falhar = "use metodo = toda_yamamoto, que serve a séries em nível e integradas"),
        P("Os resíduos do VAR são ruído branco: a precedência só se lê se a dinâmica está bem capturada.",
          verificar = "series/portmanteau_mv",
          se_falhar = "aumente as defasagens (series/var_select)"),
        P("Pelo método instantânea, os resíduos são aproximadamente normais: a variância assintótica das covariâncias usada no Wald é a do caso gaussiano.",
          verificar = "series/normality_mv",
          se_falhar = "leia o p-valor com cautela; caudas pesadas tornam o teste liberal")),
      referencias = list(granger, toda, lutkepohl, .tr_series_impl("vars", "causality"))
    )
  )
}
