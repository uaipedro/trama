#' O tipo `data/table` — e por que ele carrega a própria persistência.
#'
#' `store`/`restore` moram no TIPO, não no store do trama: é assim que o núcleo
#' permanece cego ao que guarda, e é o que permite uma coleção nova trazer
#' formato próprio sem tocar na biblioteca. Aqui é RDS por simplicidade e
#' fidelidade (preserva factor, POSIXct, atributos); parquet entraria trocando
#' três linhas, sem nada mudar do lado do motor.
#'
#' O `preview` devolve dado JÁ REDUZIDO — cabeçalho de 25 linhas — em vez de
#' um PNG. É a regra do desenho: arquivo só quando o dado não cabe. Tabela
#' cabe, e mandada como dado ela ganha ordenação, hover e seleção no front,
#' que uma imagem nunca teria.
#' O `store` é o FUNIL: é por ele que todo valor entra no artefato, venha do
#' nó que vier. Por isso o guard de tipo mora aqui, e não em cada `fn` — vale
#' pra qualquer nó que declare `data/table`, presente ou futuro, sem que
#' nenhum deles precise lembrar.
#'
#' Sem ele, o tipo aceitava qualquer objeto e o `preview` desenhava tabela em
#' cima: `1:10` virava uma coluna `x`, `list(a=1,b="x")` virava `a,b`, e um
#' `lm` virava uma tabela de ZERO colunas — card verde, tabela plausível, nada
#' errado à vista. E propagava: o `data/filter` ligado adiante estourava erro
#' cru de dplyr no card SEGUINTE, longe da causa.
#'
#' `restore` não repete a checagem: se nada inválido entra, nada inválido sai.
#' A única brecha é artefato gravado por versão ANTIGA, que nunca passou pelo
#' guard — e ela se fecha pela chave: `.tr_type_fingerprint()` (`R/hash.R:113`)
#' hasheia `version` junto com o corpo do `store`, então nenhum artefato velho
#' é reaproveitado. O bump de `version` para 2L não é, portanto, o que muda a
#' chave (o corpo novo já mudaria); é a declaração explícita de que a geração
#' anterior de artefatos deste tipo é incompatível, que é o que se lê no
#' handle sem precisar comparar hashes.
data_table_type <- function() {
  trama::tr_type(
    "data/table", version = 2L, label = "Tabela", color = "#38bdf8",
    ext = "rds",
    store   = function(x, path) {
      if (!is.data.frame(x)) {
        rlang::abort(
          sprintf("O nó produziu um objeto '%s', não uma tabela.", class(x)[[1]]),
          class = "tr_data_error_not_a_table")
      }
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) list(linhas = nrow(x), colunas = ncol(x),
                               nomes = paste(names(x), collapse = ", ")),
    preview = function(x, ctx) .tr_data_table_preview(x)
  )
}

# Teto do que o card recebe: colunas além de `max_col` não viajam (o card mostra
# "+N"), e o perfil é medido numa amostra de até `max_amostra` linhas — o
# preview roda a cada execução, então custo tem de ser O(amostra), não O(n).
.TR_DATA_PREVIEW_MAX_COL <- 30L
.TR_DATA_PREVIEW_AMOSTRA <- 5000L

.tr_data_table_preview <- function(x, max_col = .TR_DATA_PREVIEW_MAX_COL,
                                   max_amostra = .TR_DATA_PREVIEW_AMOSTRA) {
  df <- as.data.frame(x)
  nc <- min(ncol(df), max_col)
  df <- df[, seq_len(nc), drop = FALSE]
  head_df <- utils::head(df, 25L)
  amostra <- if (nrow(df) > max_amostra) {
    # Linhas espaçadas por igual, determinístico: o mesmo dado dá o mesmo
    # perfil (sem semente global mexida, sem card piscando entre execuções).
    df[unique(round(seq(1, nrow(df), length.out = max_amostra))), , drop = FALSE]
  } else df
  trama::tr_preview("data/table", data = list(
    columns = as.list(names(head_df)),
    rows = lapply(seq_len(nrow(head_df)), function(i) {
      as.list(lapply(head_df[i, , drop = FALSE], function(v) {
        v <- v[[1]]
        if (is.factor(v)) as.character(v) else if (inherits(v, "Date") ||
          inherits(v, "POSIXt")) format(v) else v
      }))
    }),
    perfil = lapply(amostra, .tr_data_perfil_coluna),
    amostrado = nrow(amostra) < nrow(x),
    nrow = nrow(x), ncol = ncol(x)
  ))
}

# Perfil de UMA coluna: tipo curto, proporção de NA e a forma — histograma de
# 10 classes (numérica) ou até 3 níveis mais frequentes (o resto). Datas e
# tempos ficam só com tipo e NA: histograma de data pede eixo próprio.
.tr_data_perfil_coluna <- function(v) {
  n <- length(v)
  na <- if (n) mean(is.na(v)) else 0
  tipo <- if (is.logical(v)) "lgl" else if (is.factor(v)) "fct" else
    if (inherits(v, "Date") || inherits(v, "POSIXt")) "data" else
    if (is.numeric(v)) "num" else if (is.character(v)) "chr" else class(v)[[1]]
  out <- list(tipo = tipo, na = na)
  ok <- v[!is.na(v)]
  if (tipo == "num") {
    ok <- ok[is.finite(ok)]
    if (length(ok)) {
      r <- range(ok)
      out$hist <- if (r[[1]] == r[[2]]) c(length(ok), integer(9)) else
        as.integer(tabulate(pmin(10L, 1L + floor(10 * (ok - r[[1]]) / (r[[2]] - r[[1]]))), 10L))
      out$min <- r[[1]]
      out$max <- r[[2]]
    }
  } else if (tipo %in% c("fct", "chr", "lgl")) {
    if (length(ok)) {
      tb <- sort(table(as.character(ok)), decreasing = TRUE)
      k <- min(3L, length(tb))
      out$top <- lapply(seq_len(k), function(i) list(nivel = names(tb)[[i]], prop = tb[[i]] / n))
      out$niveis <- length(tb)
    }
  }
  out
}
