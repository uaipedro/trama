# Grupo multivariado "selecao": a escolha da ordem do VAR (`series/var_select`)
# e a correlação cruzada entre duas séries (`series/ccf`).
#
# Por que `series/ccf` recebe DUAS portas `series/ts`, e não uma `series/mts`
# com o nome das colunas digitado: o gráfico precisa de exatamente duas séries,
# e cada porta mostra no canvas de qual nó vem cada uma (o fio aponta para a
# série, sem texto para errar). Um param de coluna novo ainda pediria um nome
# fora do glossário, e o glossário está travado. A série múltipla continua
# sendo a entrada do `series/var_select`, que olha todas de uma vez.

.tr_series_nos_mv_selecao <- function() {
  S <- "series/ts"; MV <- "series/mts"; TB <- "data/table"; G <- "view/plot"
  I <- trama::tr_param_int; E <- trama::tr_param_enum; B <- trama::tr_param_bool
  icone <- function(n) trama::tr_icon(n)
  list(
    trama::tr_node("series/ccf", fn = tr_series_ccf, label = "Correlação cruzada (CCF)",
      pressupostos = .tr_series_doc("series/ccf")$pressupostos,
      referencias = .tr_series_doc("series/ccf")$referencias,
      category = "serie_ver", icon = icone("chart-column"),
      description = "A correlação entre duas séries em cada defasagem, negativa e positiva, com a banda do ruído branco.",
      inputs = list(x = S, y = S), outputs = list(out = G),
      params = .tr_series_props(defasagens = I(0L, min = 0L, max = 500L, label = "Defasagens", vazio = 0L, example = "0 = automático")),
      help = .tr_series_ajuda(r"---[
A correlação cruzada: para cada defasagem k, a correlação entre x no tempo
t + k e y no tempo t. Responde a uma pergunta de defasagem: se uma série mexe
antes da outra, em quantos períodos isso aparece?

### Como ler

- barra em **k > 0**: y no tempo t se relaciona com x no tempo t + k, ou seja,
  **y antecede x** por k períodos.
- barra em **k < 0**: o espelho — **x antecede y** por |k| períodos.
- barra em **k = 0**: as duas se movem juntas no mesmo período.
- a linha tracejada é a banda de ±1,96/√n, a do ruído branco. Barra FORA dela
  é candidata a relação, não conclusão: com 40 defasagens, duas passam por
  acaso.

### Cuidado: tendência e autocorrelação enganam

Duas séries que só sobem com o tempo (o DAX e o CAC, por exemplo) têm
correlação cruzada alta em muitas defasagens, sem relação nenhuma entre elas:
a tendência de uma "explica" a da outra. O mesmo vale para qualquer série
muito autocorrelacionada, que tem memória e por isso parece acompanhar a
outra. A banda ±1,96/√n também supõe ruído branco; com autocorrelação ela fica
estreita demais.

As duas séries não aceitam faltantes: o período comum é recortado, e com buraco
dentro dele o `series/interpolate` vem antes.

Antes de ler a defasagem, **diferencie** cada série (`series/diff`) ou
**pré-branqueie**: ajuste um ARIMA em cada uma (`series/arima`) e leia a
correlação cruzada dos resíduos (`series/residuals`). Só então o pico numa
defasagem diz alguma coisa sobre quem antecede quem.
]---", r"---[
- **Defasagens** — até onde ir, dos dois lados. 0 é automático: 10·log10(n),
  mas nunca menos de três ciclos numa série sazonal.
]---", r"---[
Um gráfico (`view/plot`) com uma barra por defasagem, das negativas às
positivas. As barras FORA da banda saem coloridas.
]---", r"---[
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("dif_dax", "series/diff", from = "dax") |>
  tr_add("dif_cac", "series/diff", from = "cac") |>
  tr_add("cc", "series/ccf", from = c(x = "dif_dax", y = "dif_cac"))
]---", r"---[
`series/acf`, a mesma leitura para uma série só; `series/diff` para tirar a
tendência antes; `series/arima` e `series/residuals` para o pré-branqueamento;
`series/join` para montar a série múltipla que o `series/var_select` lê.
]---", grafico = TRUE)),

    trama::tr_node("series/var_select", fn = tr_series_var_select, label = "Defasagens do VAR",
      pressupostos = .tr_series_doc("series/var_select")$pressupostos,
      referencias = .tr_series_doc("series/var_select")$referencias,
      category = "serie_mv", icon = icone("table"), role = "avaliacao",
      description = "AIC, HQ, SC e FPE para cada defasagem do VAR, com a que cada critério escolhe.",
      inputs = list(series = MV), outputs = list(out = TB),
      params = list(
        max_defasagens = I(8L, min = 1L, max = 50L, label = "Máximo de defasagens"),
        deterministico = E("constante", names(.TR_SERIES_DET), label = "Determinístico"),
        sazonal = B(FALSE, label = "Dummies sazonais")),
      help = .tr_series_ajuda(r"---[
Ajusta o VAR de cada ordem de 1 até **Máximo de defasagens** e mostra, por
ordem, os quatro critérios de informação do VAR: AIC, HQ, SC (BIC) e FPE.
Quanto menor, melhor. A coluna **escolhida** diz quais critérios apontam para
aquela ordem.

Os critérios penalizam o número de parâmetros de jeitos diferentes. AIC e FPE
penalizam pouco e tendem a escolher ordens maiores; SC (BIC) penaliza mais e
tende às menores, e é consistente quando a verdadeira ordem existe. Quando
discordam, é comum preferir a ordem em que os resíduos do `series/var` ficam
sem autocorrelação (`series/ljung_box` nos resíduos) e que não deixa o VAR
instável.

A série não aceita faltantes: com buraco, o `series/interpolate` vem antes, ou
recorte a série com `series/window`.

A tabela usa as mesmas regras do `series/var`: o mesmo determinístico e as
mesmas dummies sazonais. A série precisa ser estacionária (ou ter sido
diferenciada) para a escolha fazer sentido.
]---", r"---[
- **Máximo de defasagens** — a última ordem testada.
- **Determinístico** — constante, tendência, ambos ou nenhum, em cada equação.
- **Dummies sazonais** — dummies centradas por período (série com ciclo).
]---", r"---[
Uma tabela (`data/table`) com uma linha por defasagem: `defasagem`, `AIC`,
`HQ`, `SC`, `FPE` e `escolhida` (os critérios que escolhem aquela ordem).
]---", r"---[
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac")) |>
  tr_add("sel", "series/var_select", max_defasagens = 6L, from = "j")
]---", r"---[
`series/var`, que ajusta o modelo na ordem escolhida (com `defasagens = 0`, a
mesma conta); `series/join` para montar a série múltipla; `series/ljung_box`
para os resíduos.
]---"))
  )
}

.tr_series_docs_mv_selecao <- function() {
  P <- .tr_series_P
  lutkepohl <- trama::tr_ref(autores = "Lütkepohl, H.", ano = 2005,
                             titulo = "New Introduction to Multiple Time Series Analysis",
                             fonte = "Berlin: Springer", doi = "10.1007/978-3-540-27752-1",
                             papel = "livro-texto")
  list(
    "series/var_select" = list(
      pressupostos = list(
        P("As séries são estacionárias: os critérios comparam ajustes de ordens diferentes, e com raiz unitária a escolha da defasagem não tem significado.",
          verificar = c("series/adf", "series/kpss"),
          se_falhar = "diferencie as séries antes de escolher a ordem; se cointegradas, a escolha é do VECM")),
      referencias = list(lutkepohl, .tr_series_impl("vars", "VARselect"))),
    "series/ccf" = list(
      pressupostos = list(
        P("As duas séries são estacionárias: com tendência ou raiz unitária, séries sem relação alguma mostram correlação cruzada grande (espúria).",
          verificar = c("series/adf", "series/kpss"),
          se_falhar = "diferencie as duas (series/diff), ou pré-branqueie com um ARIMA em cada uma e leia o ccf dos resíduos"),
        P("Os resíduos são ruído branco: a banda de ±1,96/√n só vale sem autocorrelação em nenhuma das duas.",
          verificar = c("series/acf", "series/ljung_box"),
          se_falhar = "pré-branqueie cada série com series/arima antes do ccf")),
      referencias = list(.tr_series_livros()$box_jenkins, .tr_series_impl("stats", "ccf")))
  )
}

# ---- series/ccf --------------------------------------------------------------

#' Confere o par da correlação cruzada: duas séries de mesma frequência, com
#' período comum e sem faltantes. Devolve as duas no calendário comum, como
#' `mts` de duas colunas.
#' @noRd
.tr_series_par <- function(x, y, no) {
  .tr_series_guard_ts(x)
  .tr_series_guard_ts(y)
  fx <- stats::frequency(x)
  fy <- stats::frequency(y)
  if (abs(fx - fy) > 1e-8) {
    .tr_series_abort("tr_series_error_frequency_mismatch",
                     paste0("'%s': as duas séries têm frequências diferentes (%s e %s). Agregue a mais ",
                            "fina antes, com 'series/aggregate'."), no, fx, fy)
  }
  # Sem período comum, o `ts.intersect` avisa e devolve vazio: a checagem vem
  # antes, para o erro sair com a classe e o nome do nó.
  if (max(stats::tsp(x)[[1]], stats::tsp(y)[[1]]) > min(stats::tsp(x)[[2]], stats::tsp(y)[[2]]) + 1e-8) {
    .tr_series_abort("tr_series_error_no_overlap", "'%s': as séries não têm nenhum período em comum.", no)
  }
  X <- stats::ts.intersect(stats::as.ts(x), stats::as.ts(y))
  if (is.null(X) || NROW(X) < 1L) {
    .tr_series_abort("tr_series_error_no_overlap", "'%s': as séries não têm nenhum período em comum.", no)
  }
  colnames(X) <- c("x", "y")
  .tr_series_sem_na(X, no)
  X
}

#' A tabela da correlação cruzada: uma linha por defasagem, de -k a k.
#'
#' Os valores são os do `stats::ccf` com o mesmo `lag.max`; a única coisa que
#' se acrescenta é a defasagem em observações (o `ccf` a dá em unidade de
#' tempo, como o `acf` faz com a sazonal) e a marca de fora da banda.
#' @noRd
.tr_series_ccf_tabela <- function(x, y, defasagens = 0L) {
  X <- .tr_series_par(x, y, "series/ccf")
  n <- nrow(X)
  .tr_series_minimo(X[, 1], 4L, "series/ccf", "uma correlação cruzada")
  f <- stats::frequency(X)
  k <- .tr_series_int(defasagens, "defasagens", min = 0)
  if (k == 0L) k <- max(10 * log10(n), if (f > 1) 3 * f else 0)
  k <- as.integer(min(floor(k), n - 2L))
  a <- stats::ccf(X[, 1], X[, 2], lag.max = k, plot = FALSE, na.action = stats::na.fail)
  d <- tibble::tibble(defasagem = round(as.numeric(a$lag) * f), r = as.numeric(a$acf))
  banda <- stats::qnorm(.975) / sqrt(n)
  d$fora <- abs(d$r) > banda
  attr(d, "banda") <- banda
  attr(d, "frequencia") <- f
  d
}

#' O gráfico: barras por defasagem, negativas à esquerda, com a banda.
#' @noRd
.tr_series_ccf_grafico <- function(x, y, defasagens) {
  d <- .tr_series_ccf_tabela(x, y, defasagens)
  banda <- attr(d, "banda")
  f <- attr(d, "frequencia")
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["defasagem"]], y = .data[["r"]])) +
    ggplot2::geom_hline(yintercept = 0, colour = .TR_SERIES_CINZA) +
    ggplot2::geom_hline(yintercept = c(-banda, banda), linetype = "dashed", colour = .TR_SERIES_COR_2) +
    ggplot2::geom_segment(ggplot2::aes(xend = .data[["defasagem"]], yend = 0, colour = .data[["fora"]]),
                          linewidth = 1.2) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = .TR_SERIES_CINZA, `TRUE` = .TR_SERIES_COR),
                                 labels = c(`FALSE` = "dentro da banda", `TRUE` = "fora da banda"),
                                 name = NULL) +
    ggplot2::labs(x = "defasagem (k > 0: y antecede x)", y = "correlação cruzada")
  if (f > 1) {
    m <- floor(max(abs(d$defasagem)) / f) * f
    p <- p + ggplot2::scale_x_continuous(breaks = seq(-m, m, by = f))
  }
  p
}

#' Correlação cruzada entre duas séries, por defasagem.
#' @export
tr_series_ccf <- function(x, y, defasagens = 0L, aspecto = "16:9", tema = "padrão",
                          titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  trama.view::tr_view_finish(.tr_series_ccf_grafico(x, y, defasagens),
                             aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

# ---- series/var_select -------------------------------------------------------

#' Tabela dos quatro critérios por defasagem, com a escolha de cada um.
#'
#' `escolhida` lista os critérios que apontam para a ordem da linha; uma ordem
#' que nenhum escolhe fica em branco.
#' @noRd
.tr_series_var_selecao_tabela <- function(sel) {
  crit <- sel$criteria
  nomes <- c("AIC", "HQ", "SC", "FPE")
  ordens <- as.integer(colnames(crit))
  escolhida <- vapply(ordens, function(p) {
    hit <- nomes[vapply(nomes, function(nm) identical(as.integer(sel$selection[[paste0(nm, "(n)")]]), p),
                        logical(1))]
    paste(hit, collapse = ", ")
  }, character(1))
  tibble::tibble(defasagem = ordens,
                 AIC = as.numeric(crit["AIC(n)", ]), HQ = as.numeric(crit["HQ(n)", ]),
                 SC = as.numeric(crit["SC(n)", ]), FPE = as.numeric(crit["FPE(n)", ]),
                 escolhida = escolhida)
}

#' Critérios de informação do VAR por defasagem, com a escolha de cada um.
#'
#' O cálculo é o do `vars::VARselect`, com o mesmo determinístico e as mesmas
#' dummies sazonais que o `series/var` usa.
#' @export
tr_series_var_select <- function(series, max_defasagens = 8L, deterministico = "constante",
                                 sazonal = FALSE) {
  .tr_series_guard_mts(series)
  .tr_series_sem_na(series, "series/var_select")
  det <- .tr_series_var_det(deterministico)
  pmax <- .tr_series_int(max_defasagens, "max_defasagens", min = 1, max = 50)
  saz <- if (isTRUE(sazonal)) {
    f <- stats::frequency(series)
    if (f <= 1) .tr_series_abort("tr_series_error_no_season", "'series/var_select': sazonais pedem série com ciclo (frequência > 1).")
    as.integer(f)
  }
  .tr_series_minimo(series[, 1], ncol(series) * pmax + 10L, "series/var_select",
                    sprintf("estimar as equações com até %d defasagens", pmax))
  sel <- .tr_series_ajustar(vars::VARselect(series, lag.max = pmax, type = det, season = saz),
                            "series/var_select")
  .tr_series_var_selecao_tabela(sel)
}
