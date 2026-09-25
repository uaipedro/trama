# O que toda técnica da coleção faz antes de começar: escolher as colunas,
# conferir que são medidas numéricas completas e montar a matriz.
#
# Mora num lugar só porque as recusas precisam ser AS MESMAS em todo nó. Se a
# PCA aceitasse um faltante que a análise fatorial recusa, dois cards ligados à
# mesma tabela contariam histórias de tamanhos diferentes — e ninguém repara
# que um usou 150 linhas e o outro 143.

.TR_MULTI_COR <- "#818cf8"
.TR_MULTI_COR_2 <- "#f472b6"
.TR_MULTI_CINZA <- "#8b949e"

#' Separa "a, b, c" em nomes, aparando espaços. Vazio vira `character()`.
#' @noRd
.tr_multi_split <- function(cols) {
  if (!length(cols) || all(is.na(cols))) return(character())
  x <- trimws(unlist(strsplit(paste(as.character(cols), collapse = ","), ",", fixed = TRUE)))
  unique(x[nzchar(x)])
}

#' Confere que UMA coluna citada existe, e devolve o nome.
#'
#' Mesma recusa das irmãs a `any_of()`: descartar nome inexistente em silêncio
#' faria `Especies` virar agrupamento de coisa nenhuma.
#' @noRd
.tr_multi_col <- function(dados, col, param) {
  col <- .tr_multi_obrigatorio(col, param)
  if (!col %in% names(dados)) {
    .tr_multi_abort("tr_multi_error_unknown_column",
                    "Param '%s': coluna inexistente: %s. Disponíveis: %s.",
                    param, col, paste(names(dados), collapse = ", "))
  }
  col
}

#' As variáveis de uma técnica: as citadas, ou todas as numéricas.
#'
#' Em branco usa TODA coluna numérica, e é o default de propósito: é o que se
#' quer em `USArrests` ou nos testes de Harman. A coluna citada que não é
#' numérica é ERRO, e não descarte — quem digitou `Species` entre as medidas
#' quer saber que ela não entrou. `excluir` tira colunas que o nó já usa para
#' outra coisa (o grupo da discriminante), para o branco não as engolir.
#' @noRd
.tr_multi_variaveis <- function(dados, cols, param = "cols", minimo = 2L, excluir = character()) {
  nomes <- .tr_multi_split(cols)
  if (!length(nomes)) {
    nomes <- setdiff(names(dados)[vapply(dados, is.numeric, TRUE)], excluir)
  } else {
    faltam <- setdiff(nomes, names(dados))
    if (length(faltam)) {
      .tr_multi_abort("tr_multi_error_unknown_column",
                      "Param '%s': coluna inexistente: %s. Disponíveis: %s.",
                      param, paste(faltam, collapse = ", "), paste(names(dados), collapse = ", "))
    }
    texto <- nomes[!vapply(dados[nomes], is.numeric, TRUE)]
    if (length(texto)) {
      .tr_multi_abort("tr_multi_error_not_numeric",
                      paste0("Param '%s': coluna não numérica entre as variáveis: %s. ",
                             "Tire-a da lista, ou converta antes num 'data/convert'."),
                      param, paste(texto, collapse = ", "))
    }
  }
  if (length(nomes) < minimo) {
    .tr_multi_abort("tr_multi_error_too_few_variables",
                    "Param '%s': a técnica precisa de pelo menos %d variáveis numéricas, e há %d%s.",
                    param, as.integer(minimo), length(nomes),
                    if (length(nomes)) sprintf(" (%s)", paste(nomes, collapse = ", ")) else "")
  }
  nomes
}

#' A matriz numérica das variáveis, conferida.
#'
#' Faltante é recusado, e não descartado por `na.omit` escondido: o card diria
#' "150 observações" tendo usado 143. A mensagem conta as linhas e aponta o nó
#' que descarta à vista, `data/drop_na`. Variável constante também é recusada
#' com o nome: a correlação dela é 0/0, e o `cor()` devolveria NA com um aviso
#' que o card não mostra.
#' @noRd
.tr_multi_matriz <- function(dados, variaveis, no, min_linhas = 3L) {
  m <- as.matrix(as.data.frame(dados)[, variaveis, drop = FALSE])
  storage.mode(m) <- "double"
  incompletas <- !stats::complete.cases(m)
  if (any(incompletas)) {
    cols <- variaveis[colSums(is.na(m)) > 0]
    .tr_multi_abort("tr_multi_error_missing_values",
                    paste0("'%s' não aceita faltantes, e %d linha(s) têm (colunas: %s). Ligue um ",
                           "'data/drop_na' antes, ou um 'data/replace_na' se souber o que pôr lá."),
                    no, sum(incompletas), paste(cols, collapse = ", "))
  }
  if (nrow(m) < min_linhas) {
    .tr_multi_abort("tr_multi_error_too_few_rows",
                    "'%s' precisa de pelo menos %d observações, e a tabela tem %d.",
                    no, as.integer(min_linhas), nrow(m))
  }
  dp <- apply(m, 2L, stats::sd)
  if (any(dp == 0)) {
    .tr_multi_abort("tr_multi_error_constant_variable",
                    "'%s': variável constante (variância zero): %s. Tire-a da lista de colunas.",
                    no, paste(variaveis[dp == 0], collapse = ", "))
  }
  m
}

#' A matriz de correlação, recusando a singular com a pista de onde olhar.
#'
#' `tol` é relativo: o menor autovalor comparado ao maior. Uma variável que é a
#' soma de outras duas dá um autovalor de ordem 1e-16, e a inversa que o KMO, a
#' PAF e o ML pedem sai com números enormes e verde.
#' @noRd
.tr_multi_correlacao <- function(m, no, tol = 1e-10) {
  r <- stats::cor(m)
  ev <- eigen(r, symmetric = TRUE, only.values = TRUE)$values
  if (min(ev) < tol * max(ev)) {
    .tr_multi_abort("tr_multi_error_singular_matrix",
                    paste0("'%s': a matriz de correlação é singular (menor autovalor %.2g). Alguma ",
                           "variável é combinação exata das outras, ou há mais variáveis que ",
                           "observações. Tire a redundante (um total, uma porcentagem que fecha 100)."),
                    no, min(ev))
  }
  r
}

#' As páginas de ajuda, com as seções na ordem de `?funcao`.
#'
#' Montadas por função, e não à mão, pelo mesmo motivo da `series`: com vinte
#' nós, a seção esquecida em um deles é certa. Os textos vêm em raw string
#' (`r"---[ ]---"`) porque citam código com aspas e barras.
#' @noRd
.tr_multi_ajuda <- function(descricao, parametros, valor, exemplos, veja, grafico = FALSE,
                            teste = FALSE) {
  paste0("## Descrição\n\n", trimws(descricao),
         "\n\n## Parâmetros\n\n", trimws(parametros),
         "\n\n## Valor\n\n", trimws(valor),
         "\n\n## Exemplos\n\n```r\n", trimws(exemplos), "\n```",
         "\n\n## Veja também\n\n", trimws(veja),
         if (grafico) paste0("\n", trama.view::tr_view_help_appearance()) else "",
         # O bloco que sai em `data/test` explica o card do núcleo com o texto
         # do núcleo, escrito uma vez só.
         if (teste) paste0("\n", trama::tr_help_test_card()) else "")
}

#' Os cosméticos da `view`, com a proporção padrão própria do gráfico.
#'
#' O círculo de correlações e o mapa de calor das cargas são quadrados por
#' natureza; em 16:9 o círculo vira elipse de espaço sobrando. Mudar só o
#' DEFAULT do spec mantém o param igual ao da `view` em tudo o mais, e o `fn`
#' declara o mesmo default, para que o card e o console desenhem igual.
#' @noRd
.tr_multi_props <- function(..., .aspecto = "16:9") {
  ps <- trama.view::tr_view_props(...)
  ps$aspecto$default <- .aspecto
  ps
}
