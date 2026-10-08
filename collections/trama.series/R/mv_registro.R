# Registro dos nós multivariados.
#
# Cada grupo mora no seu `mv_<grupo>.R` e expõe duas funções:
# `.tr_series_nos_mv_<grupo>()` (lista de `tr_node`) e
# `.tr_series_docs_mv_<grupo>()` (pressupostos e referências por id). Aqui só
# se concatena: um grupo novo não toca no arquivo de outro.

.tr_series_nos_mv <- function() {
  c(.tr_series_nos_mv_base(), .tr_series_nos_mv_selecao(), .tr_series_nos_mv_causalidade(),
    .tr_series_nos_mv_cointegracao(), .tr_series_nos_mv_impulso(), .tr_series_nos_mv_diagnostico())
}

.tr_series_docs_mv <- function() {
  c(.tr_series_docs_mv_base(), .tr_series_docs_mv_selecao(), .tr_series_docs_mv_causalidade(),
    .tr_series_docs_mv_cointegracao(), .tr_series_docs_mv_impulso(), .tr_series_docs_mv_diagnostico())
}

.tr_series_docs_mv_base <- function() {
  R <- trama::tr_ref
  P <- .tr_series_P
  lutkepohl <- R(autores = "Lütkepohl, H.", ano = 2005, titulo = "New Introduction to Multiple Time Series Analysis",
                 fonte = "Berlin: Springer", doi = "10.1007/978-3-540-27752-1", papel = "livro-texto")
  pfaff <- R(autores = "Pfaff, B.", ano = 2008, titulo = "VAR, SVAR and SVEC Models: Implementation Within R Package vars",
             fonte = "Journal of Statistical Software, 27(4)", doi = "10.18637/jss.v027.i04", papel = "teoria")
  list(
    "series/var" = list(
      pressupostos = list(
        P("Todas as séries são estacionárias (ou o VAR em nível é usado só para Granger de Toda-Yamamoto); a maior raiz no card fica abaixo de 1.",
          verificar = c("series/adf", "series/kpss"),
          se_falhar = "diferencie as séries, ou, se cointegradas, use series/vecm"),
        P("Os resíduos são ruído branco: as defasagens bastam para a dinâmica.",
          verificar = "series/portmanteau_mv",
          se_falhar = "aumente as defasagens (series/var_select)")),
      referencias = list(lutkepohl, pfaff, .tr_series_impl("vars", "VAR")))
  )
}

.tr_series_nos_mv_base <- function() {
  S <- "series/ts"; MV <- "series/mts"; VAR <- "series/var"; T <- "data/table"
  I <- trama::tr_param_int; E <- trama::tr_param_enum; B <- trama::tr_param_bool; P <- trama::tr_param
  icone <- function(n) trama::tr_icon(n)
  list(
    trama::tr_node("series/from_table_mts", fn = tr_series_from_table_mts, label = "Tabela para séries",
      category = "serie_fonte", icon = icone("table-columns-split"),
      description = "Monta uma série múltipla a partir de várias colunas de valores e, opcionalmente, uma de tempo.",
      inputs = list(dados = T), outputs = list(out = MV),
      params = list(
        valores = trama::tr_param_col(character(), label = "Valores", role = "numerica", multi = TRUE,
                                      example = "consumo, renda"),
        tempo = trama::tr_param_col("", label = "Tempo", role = "qualquer", example = "mes"),
        frequencia = I(12L, min = 1L, max = 366L, label = "Frequência"),
        inicio = P("text", "", label = "Início", example = "2019, 7")),
      help = .tr_series_ajuda(r"---[
Transforma várias colunas de uma tabela numa série múltipla, uma série por
coluna, todas no mesmo calendário. As regras de tempo são as do
`series/from_table`: tempo repetido ou com buraco é recusado.
]---", r"---[
- **Valores** — as colunas numéricas, uma por série (duas ou mais).
- **Tempo**, **Frequência**, **Início** — como no `series/from_table`.
]---", r"---[
Uma série múltipla (`series/mts`).
]---", r"---[
tr_series_from_table_mts(dados, valores = c("consumo", "renda"), tempo = "mes")
]---", r"---[
`series/join` para juntar séries que já existem; `series/pick` para tirar uma.
]---")),

    trama::tr_node("series/join", fn = tr_series_join, label = "Juntar séries",
      category = "serie_mv", icon = icone("combine"),
      description = "Junta duas ou mais séries de mesma frequência numa série múltipla, no período comum.",
      inputs = list(series = trama::tr_port(S, multiple = TRUE)), outputs = list(out = MV),
      params = list(nomes = P("text", "", label = "Nomes", example = "consumo, renda")),
      help = .tr_series_ajuda(r"---[
Junta as séries ligadas numa série múltipla — a entrada dos blocos que olham
várias séries ao mesmo tempo (VAR, cointegração, Granger). As séries precisam
ter a mesma frequência; o período é o COMUM a todas, e o card avisa quantas
observações ficaram de fora.
]---", r"---[
- **Nomes** — nomes das séries, separados por vírgula, na ordem dos fios.
  Vazio usa `serie_1`, `serie_2`...
]---", r"---[
Uma série múltipla (`series/mts`).
]---", r"---[
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac"))
]---", r"---[
`series/pick` faz o caminho de volta; `series/var` ajusta o modelo.
]---")),

    trama::tr_node("series/pick", fn = tr_series_pick, label = "Escolher série",
      category = "serie_mv", icon = icone("list-filter"),
      description = "Tira uma série de uma série múltipla, para os blocos univariados.",
      inputs = list(series = MV), outputs = list(out = S),
      params = list(variavel = P("text", "", label = "Série", example = "renda")),
      help = .tr_series_ajuda(r"---[
Devolve uma das séries de uma série múltipla, para testar raiz unitária,
decompor ou ajustar um ARIMA nela.
]---", r"---[
- **Série** — o nome da coluna.
]---", r"---[
Uma série (`series/ts`).
]---", r"---[
tr_series_pick(series, variavel = "renda")
]---", r"---[
`series/join`.
]---")),

    trama::tr_node("series/var", fn = tr_series_var, label = "VAR",
      pressupostos = .tr_series_doc("series/var")$pressupostos,
      referencias = .tr_series_doc("series/var")$referencias,
      category = "serie_mv", icon = icone("network"),
      description = "Ajusta um vetor autorregressivo VAR(p): cada série explicada pelo passado de todas.",
      inputs = list(series = MV), outputs = list(out = VAR),
      params = list(
        defasagens = I(0L, min = 0L, max = 50L, label = "Defasagens (p)", vazio = 0L, example = "0 = pelo critério"),
        max_defasagens = trama::tr_when(I(8L, min = 1L, max = 50L, label = "Máximo de defasagens"), defasagens = 0L),
        criterio = trama::tr_when(E("AIC", c("AIC", "HQ", "SC", "FPE"), label = "Critério"), defasagens = 0L),
        deterministico = E("constante", names(.TR_SERIES_DET), label = "Determinístico"),
        sazonal = B(FALSE, label = "Dummies sazonais")),
      help = .tr_series_ajuda(r"---[
Ajusta um VAR(p) por mínimos quadrados, equação a equação: cada série é
regredida nas `p` defasagens de TODAS as séries. É o modelo de partida para
séries que se influenciam — previsão conjunta, causalidade de Granger,
impulso-resposta.

As séries devem ser estacionárias. Em nível e cointegradas, use o
`series/vecm`; em nível e não cointegradas, diferencie antes. O card mostra os
coeficientes de cada equação e o resumo, a maior raiz do polinômio: acima de 1,
o VAR é instável.
]---", r"---[
- **Defasagens (p)** — 0 escolhe pelo critério, até o máximo.
- **Critério** — AIC (padrão), HQ, SC (BIC) ou FPE.
- **Determinístico** — constante, tendência, ambos ou nenhum, em cada equação.
- **Dummies sazonais** — dummies centradas por período (série com ciclo).
]---", r"---[
Um ajuste multivariado (`series/var`).
]---", r"---[
tr_flow(reg) |>
  tr_add("j", "series/join", from = c("a", "b")) |>
  tr_add("v", "series/var", defasagens = 2L, from = "j") |>
  tr_add("prev", "series/forecast", horizonte = 8L, from = c(var = "v"))
]---", r"---[
`series/var_select` para comparar os critérios; `series/granger`;
`series/irf`; `series/portmanteau_mv` para os resíduos.
]---"))
  )
}
