# Grupo multivariado "diagnostico": testes sobre os resíduos de VAR e VECM.
#
# Os blocos leem o ajuste `series/var` (VAR em nível ou VECM, na forma
# `vec2var`) e fazem a conta com as funções do `vars`, que é o oráculo dos
# testes. O único cálculo próprio é a regra automática de defasagens do
# portmanteau; o resto é repassado e lido pelo registro de testes.

.tr_series_nos_mv_diagnostico <- function() {
  S <- "series/ts"; MV <- "series/mts"; VAR <- "series/var"; TE <- "data/test"
  E <- trama::tr_param_enum; I <- trama::tr_param_int
  icone <- function(n) trama::tr_icon(n)
  list(
    trama::tr_node("series/residuals_mv", fn = tr_series_residuals_mv, label = "Resíduos (múltiplos)",
      role = "leitura", category = "serie_mv", icon = icone("scan-line"),
      description = "Os resíduos do VAR ou VECM, como série múltipla, para olhar com os blocos univariados.",
      inputs = list(modelo = trama::tr_port(VAR)), outputs = list(out = MV),
      params = list(),
      help = .tr_series_ajuda(r"---[
Devolve os resíduos de cada equação do VAR ou do VECM como uma série
múltipla, uma coluna por série, no calendário das observações que têm resíduo
(as `p` primeiras não têm, porque não há passado para elas).

Para olhar um resíduo com os blocos univariados (ACF, ADF, normalidade da
série) passe a saída por `series/pick`.
]---", r"---[
Este bloco não tem parâmetros.
]---", r"---[
Uma série múltipla (`series/mts`) com os resíduos.
]---", r"---[
tr_flow(reg) |>
  tr_add("a", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("b", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", from = c("a", "b")) |>
  tr_add("v", "series/var", defasagens = 2L, from = "j") |>
  tr_add("res", "series/residuals_mv", from = c(modelo = "v"))
]---", r"---[
`series/pick` para tirar uma coluna; `series/portmanteau_mv` para testar a
autocorrelação destes mesmos resíduos.
]---")),

    trama::tr_node("series/portmanteau_mv", fn = tr_series_portmanteau_mv,
      pressupostos = .tr_series_doc("series/portmanteau_mv")$pressupostos,
      referencias = .tr_series_doc("series/portmanteau_mv")$referencias,
      label = "Portmanteau multivariado", category = "serie_mv_testes", icon = icone("test-tube"),
      description = "Box-Pierce e Ljung-Box multivariados: os resíduos do VAR ou VECM são autocorrelacionados?",
      inputs = list(modelo = trama::tr_port(VAR)), outputs = list(out = TE),
      params = list(
        metodo = E("ljung_box", c("ljung_box", "box_pierce"), label = "Estatística"),
        defasagens = I(0L, min = 0L, max = 200L, label = "Defasagens (h)", vazio = 0L,
                       example = "0 = automático")),
      help = .tr_series_ajuda(r"---[
Testa se os resíduos do modelo ainda têm autocorrelação, somando as
autocorrelações cruzadas até a defasagem `h`. H0 é a AUSÊNCIA de
autocorrelação até `h`: rejeitar diz que o VAR (ou VECM) deixou dinâmica de
fora, e o caminho é aumentar `p` ou revisar o modelo.

As duas versões são as de Hosking (1980). O Box-Pierce (`box_pierce`) usa a
aproximação assintótica. O Ljung-Box (`ljung_box`, o padrão) corrige para
amostra finita, multiplicando cada termo por `T/(T - j)`; é o que o `vars`
chama de `PT.adjusted`, e o que se deve ler quando `T` não é grande.

Graus de liberdade: `K² (h − p)`, com `K` o número de séries e `p` a ordem do
VAR. Isso exige `h` bem maior que `p`: com `h` perto de `p` os graus de
liberdade somem, e a aproximação qui-quadrado fica frouxa. Para VECM, o `vars`
acrescenta `K` aos graus de liberdade; o bloco segue o pacote, e o número vai
no card.

Defasagens automáticas (`defasagens = 0`): `h = min(16, ⌊T/5⌋)`, com `T` o número
de observações dos resíduos. O 16 é o padrão do `vars` (Pfaff, 2008). O teto
`T/5` mantém o termo `T/(T − j)` perto de 1 até a última defasagem, e a
aproximação qui-quadrado sem depender de amostra grande. Se `h` der `p` ou
menos, o bloco recusa: não há graus de liberdade.
]---", r"---[
- **Estatística** — `ljung_box` (padrão, versão ajustada) ou `box_pierce`
  (assintótica).
- **Defasagens (h)** — número de defasagens somadas. 0 aplica a regra acima.
]---", r"---[
Um teste (`data/test`): estatística qui-quadrado, p-valor, e nota com `h` e os
graus de liberdade.
]---", r"---[
tr_flow(reg) |>
  tr_add("a", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("b", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", from = c("a", "b")) |>
  tr_add("v", "series/var", defasagens = 2L, from = "j") |>
  tr_add("lb", "series/portmanteau_mv", defasagens = 12L, from = c(modelo = "v"))
]---", r"---[
`series/normality_mv` e `series/arch_mv` para os outros dois diagnósticos dos
resíduos; `series/residuals_mv` para ver os resíduos.
]---", teste = TRUE)),

    trama::tr_node("series/normality_mv", fn = tr_series_normality_mv,
      pressupostos = .tr_series_doc("series/normality_mv")$pressupostos,
      referencias = .tr_series_doc("series/normality_mv")$referencias,
      label = "Normalidade multivariada", category = "serie_mv_testes", icon = icone("test-tube"),
      description = "Jarque-Bera multivariado: os resíduos são normais multivariados?",
      inputs = list(modelo = trama::tr_port(VAR)), outputs = list(out = TE),
      params = list(),
      help = .tr_series_ajuda(r"---[
Testa a normalidade conjunta dos resíduos do VAR ou VECM, pela versão
multivariada do Jarque-Bera. H0 é a normalidade multivariada. A estatística
soma uma parte de ASSIMETRIA e uma de CURTOSE, cada uma com `K` graus de
liberdade; o teste conjunto tem `2K`.

As duas partes saem separadas no campo `extra` (`assimetria` e `curtose`),
porque a rejeição costuma vir de uma só: resíduos com caudas pesadas rejeitam
pela curtose, e a assimetria pode estar em ordem.

Rejeitar não invalida a estimação por mínimos quadrados, mas invalida os
intervalos e testes baseados na normalidade (em amostra pequena, sobretudo).
]---", r"---[
Este bloco não tem parâmetros.
]---", r"---[
Um teste (`data/test`). Assimetria e curtose separadas no `extra`.
]---", r"---[
tr_flow(reg) |>
  tr_add("a", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("b", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", from = c("a", "b")) |>
  tr_add("v", "series/var", defasagens = 2L, from = "j") |>
  tr_add("jb", "series/normality_mv", from = c(modelo = "v"))
]---", r"---[
`series/portmanteau_mv` e `series/arch_mv` para os outros diagnósticos dos
resíduos.
]---", teste = TRUE)),

    trama::tr_node("series/arch_mv", fn = tr_series_arch_mv,
      pressupostos = .tr_series_doc("series/arch_mv")$pressupostos,
      referencias = .tr_series_doc("series/arch_mv")$referencias,
      label = "ARCH multivariado", category = "serie_mv_testes", icon = icone("test-tube"),
      description = "ARCH multivariado: a variância dos resíduos varia no tempo?",
      inputs = list(modelo = trama::tr_port(VAR)), outputs = list(out = TE),
      params = list(
        defasagens = I(5L, min = 1L, max = 50L, label = "Defasagens (ARCH)")),
      help = .tr_series_ajuda(r"---[
Testa efeito ARCH multivariado nos resíduos: a variância e as covariâncias
dos resíduos dependem do passado? H0 é a AUSÊNCIA de efeito ARCH. Rejeitar
indica heterocedasticidade condicional, e o erro-padrão dos coeficientes do
VAR fica otimista.

O teste regride os produtos cruzados padronizados dos resíduos nas suas
`defasagens` defasagens (é o ARCH-LM multivariado de Lütkepohl, na forma
do `vars`). Com `defasagens` maior, o teste olha lags mais longos e perde
graus de liberdade.
]---", r"---[
- **Defasagens (ARCH)** — quantas defasagens dos produtos cruzados entram na
  regressão. O padrão do `vars` é 5.
]---", r"---[
Um teste (`data/test`).
]---", r"---[
tr_flow(reg) |>
  tr_add("a", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("b", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", from = c("a", "b")) |>
  tr_add("v", "series/var", defasagens = 2L, from = "j") |>
  tr_add("arch", "series/arch_mv", defasagens = 5L, from = c(modelo = "v"))
]---", r"---[
`series/normality_mv` e `series/portmanteau_mv`; `series/residuals_mv` para ver
os resíduos.
]---", teste = TRUE))
  )
}

.tr_series_docs_mv_diagnostico <- function() {
  R <- trama::tr_ref
  P <- .tr_series_P
  hosking <- R(autores = "Hosking, J. R. M.", ano = 1980, titulo = "The Multivariate Portmanteau Statistic",
               fonte = "Journal of the American Statistical Association, 75(371), 602-608",
               doi = "10.1080/01621459.1980.10477520", papel = "teoria")
  lutkepohl <- R(autores = "Lütkepohl, H.", ano = 2005, titulo = "New Introduction to Multiple Time Series Analysis",
                 fonte = "Berlin: Springer", doi = "10.1007/978-3-540-27752-1", papel = "livro-texto")
  jb <- R(autores = c("Jarque, C. M.", "Bera, A. K."), ano = 1987,
          titulo = "A Test for Normality of Observations and Regression Residuals",
          fonte = "International Statistical Review, 55(2), 163-172",
          doi = "10.2307/1403192", papel = "teoria")
  pfaff <- R(autores = "Pfaff, B.", ano = 2008, titulo = "VAR, SVAR and SVEC Models: Implementation Within R Package vars",
             fonte = "Journal of Statistical Software, 27(4)", doi = "10.18637/jss.v027.i04", papel = "teoria")
  list(
    "series/portmanteau_mv" = list(
      pressupostos = list(
        P("Os resíduos são de um modelo bem especificado em `p`: autocorrelação restante indica dinâmica que o VAR não captura.",
          se_falhar = "aumente as defasagens do VAR e refaça o teste"),
        P("A aproximação qui-quadrado pede `h` bem maior que `p`, e amostra não pequena em relação a `h`.",
          se_falhar = "reduza `h` ou use a versão ajustada (ljung_box)")),
      referencias = list(hosking, lutkepohl, pfaff, .tr_series_impl("vars", "serial.test"))),
    "series/normality_mv" = list(
      pressupostos = list(
        P("Os resíduos são independentes e identicamente distribuídos; a normalidade é conjunta, não equação a equação.",
          verificar = "series/portmanteau_mv",
          se_falhar = "autocorrelação invalida o teste: aumente as defasagens do VAR")),
      referencias = list(jb, lutkepohl, .tr_series_impl("vars", "normality.test"))),
    "series/arch_mv" = list(
      pressupostos = list(
        P("A média dos resíduos é nula e o teste é lido em conjunto com a autocorrelação deles.",
          verificar = "series/portmanteau_mv",
          se_falhar = "corrija a dinâmica média antes de olhar a variância")),
      referencias = list(lutkepohl, .tr_series_impl("vars", "arch.test")))
  )
}

#' Regra automática de defasagens do portmanteau: min(16, T/5).
#' @noRd
.tr_series_portmanteau_h_auto <- function(T) as.integer(min(16L, floor(T / 5)))

#' Ljung-Box / Box-Pierce multivariado (Hosking 1980), via `vars::serial.test`.
#' @export
tr_series_portmanteau_mv <- function(modelo, metodo = "ljung_box", defasagens = 0L) {
  .tr_series_guard_var(modelo, "series/portmanteau_mv")
  metodo <- .tr_series_enum(metodo, c("ljung_box", "box_pierce"), "metodo")
  aj <- modelo$ajuste
  T <- nrow(stats::residuals(aj))
  h <- .tr_series_int(defasagens, "defasagens", min = 0L, max = 200L)
  if (h == 0L) h <- .tr_series_portmanteau_h_auto(T)
  p <- aj$p
  if (h <= p) {
    .tr_series_abort("tr_series_error_bad_option",
                     "'series/portmanteau_mv': defasagens h = %d não supera a ordem p = %d do modelo; sem graus de liberdade.",
                     h, p)
  }
  tipo <- if (metodo == "ljung_box") "PT.adjusted" else "PT.asymptotic"
  s <- vars::serial.test(aj, lags.pt = h, type = tipo)$serial
  df <- as.numeric(s$parameter)
  rotulo <- if (metodo == "ljung_box") "Ljung-Box multivariado" else "Box-Pierce multivariado"
  .tr_series_teste(
    rotulo, sprintf("não há autocorrelação nos resíduos até a defasagem %d", h),
    as.numeric(s$statistic), "qui-quadrado", p_valor = as.numeric(s$p.value),
    conclusao_sim = "resíduos autocorrelacionados: o modelo deixa dinâmica de fora",
    conclusao_nao = "não há evidência de autocorrelação nos resíduos",
    nota = sprintf("h = %d; gl = %d (K² (h − p), com K = %d e p = %d)%s",
                   h, df, aj$K, p,
                   if (identical(modelo$tipo, "VECM")) "; VECM: o vars soma K aos gl" else ""),
    fonte = "Hosking (1980)",
    extra = list(defasagens = h, graus_liberdade = df, metodo = metodo))
}

#' Jarque-Bera multivariado dos resíduos, com assimetria e curtose separadas.
#' @export
tr_series_normality_mv <- function(modelo) {
  .tr_series_guard_var(modelo, "series/normality_mv")
  r <- vars::normality.test(modelo$ajuste, multivariate.only = TRUE)$jb.mul
  jb <- r$JB; sk <- r$Skewness; ku <- r$Kurtosis
  parte <- function(x) list(estatistica = as.numeric(x$statistic), gl = as.numeric(x$parameter),
                            p_valor = as.numeric(x$p.value))
  .tr_series_teste(
    "Jarque-Bera multivariado", "os resíduos são normais multivariados",
    as.numeric(jb$statistic), "qui-quadrado", p_valor = as.numeric(jb$p.value),
    conclusao_sim = "resíduos não normais: confira assimetria e curtose no extra",
    conclusao_nao = "não há evidência contra a normalidade multivariada",
    nota = sprintf("assimetria p = %.4g; curtose p = %.4g", as.numeric(sk$p.value), as.numeric(ku$p.value)),
    fonte = "Jarque & Bera (1987)",
    extra = list(assimetria = parte(sk), curtose = parte(ku)))
}

#' ARCH multivariado dos resíduos, via `vars::arch.test`.
#' @export
tr_series_arch_mv <- function(modelo, defasagens = 5L) {
  .tr_series_guard_var(modelo, "series/arch_mv")
  k <- .tr_series_int(defasagens, "defasagens", min = 1L, max = 50L)
  a <- vars::arch.test(modelo$ajuste, lags.multi = k, multivariate.only = TRUE)$arch.mul
  .tr_series_teste(
    "ARCH multivariado", "não há efeito ARCH nos resíduos",
    as.numeric(a$statistic), "qui-quadrado", p_valor = as.numeric(a$p.value),
    conclusao_sim = "há heterocedasticidade condicional: erros-padrão podem estar otimistas",
    conclusao_nao = "não há evidência de efeito ARCH",
    nota = sprintf("%d defasagens; gl = %d", k, as.numeric(a$parameter)),
    fonte = "Lütkepohl (2005)",
    extra = list(defasagens = k, graus_liberdade = as.numeric(a$parameter)))
}

#' Os resíduos do VAR ou VECM como série múltipla.
#' @export
tr_series_residuals_mv <- function(modelo) {
  .tr_series_guard_var(modelo, "series/residuals_mv")
  .tr_series_var_residuos(modelo)
}
