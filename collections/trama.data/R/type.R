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
    preview = function(x, ctx) {
      head_df <- utils::head(as.data.frame(x), 25L)
      trama::tr_preview("data/table", data = list(
        columns = as.list(names(head_df)),
        rows = lapply(seq_len(nrow(head_df)), function(i) {
          as.list(lapply(head_df[i, , drop = FALSE], function(v) {
            v <- v[[1]]
            if (is.factor(v)) as.character(v) else if (inherits(v, "Date") ||
              inherits(v, "POSIXt")) format(v) else v
          }))
        }),
        nrow = nrow(x), ncol = ncol(x)
      ))
    }
  )
}
