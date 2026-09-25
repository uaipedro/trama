#' Classes de erro da coleção `series`.
#'
#' Mesmo princípio de `trama::tr_errors()` e das coleções irmãs, e mesmo teste
#' de duas direções: uma tabela que um teste confere contra o código, pra não
#' envelhecer em silêncio.
#'
#' Série temporal tem um modo de falha próprio, e é por ele que metade destas
#' classes existe: o resultado ERRADO que sai com cara de certo. Uma série com
#' um mês faltando vira uma série com o calendário deslocado dali em diante; uma
#' agregação anual de série que começa em março soma anos que não existem; uma
#' diferença sazonal numa série anual é uma diferença de lag 1 com outro nome.
#' Nenhum desses erra no R — todos saem verdes. Aqui viram card vermelho.
#' @return data.frame com `class` e `when`.
#' @export
tr_series_errors <- function() {
  e <- c(
    tr_series_error_unknown_column = "param nomeia coluna que não existe na tabela de entrada",
    tr_series_error_blank_param = "param obrigatório deixado em branco no card",
    tr_series_error_bad_option = "param de escolha ou número fora do conjunto aceito",
    tr_series_error_not_numeric = "a coluna do valor da série não é numérica",
    tr_series_error_duplicate_time =
      "a coluna do tempo repete um instante: a série teria dois valores no mesmo período",
    tr_series_error_gap =
      "a coluna do tempo pula períodos: a série sairia com o calendário deslocado",
    tr_series_error_bad_period = "texto de período (início/fim) ilegível ou fora da série",
    tr_series_error_no_season =
      "o nó precisa de sazonalidade e a série chegou com frequência 1",
    tr_series_error_too_short = "série curta demais para o que o nó calcula",
    tr_series_error_missing_values =
      "a série tem faltantes e o método não os aceita (interpole antes)",
    tr_series_error_nonpositive = "transformação que exige valores positivos recebeu zero ou negativo",
    tr_series_error_bad_frequency = "a nova frequência não divide a frequência da série",
    tr_series_error_bad_ets = "código de modelo ETS inválido",
    tr_series_error_fit = "o ajuste do modelo falhou",
    tr_series_error_no_overlap =
      "duas séries que o nó alinha pelo tempo não têm o período em comum que a conta pede",
    tr_series_error_no_component =
      "o componente pedido não existe nesta decomposição (regressor fora de uma regressão com covariável)",
    tr_series_error_misaligned =
      "duas séries da mesma frequência com as grades de tempo defasadas: nenhum período coincide exatamente",
    tr_series_error_frequency_mismatch =
      "duas séries que o nó opera juntas chegaram com frequências diferentes",
    tr_series_error_not_a_series =
      "o nó produziu um objeto que não é série univariada, e o tipo series/ts recusa guardá-lo",
    tr_series_error_not_a_decomposition =
      "o nó produziu um objeto que não é decomposição, e o tipo series/decomposition o recusa",
    tr_series_error_not_a_model =
      "o nó produziu um objeto que não é modelo de série, e o tipo series/model o recusa",
    tr_series_error_not_a_forecast =
      "o nó produziu um objeto que não é previsão, e o tipo series/forecast o recusa",
    tr_series_error_not_a_regression =
      "o nó produziu um objeto que não é regressão de série, e o tipo series/regression o recusa",
    tr_series_error_empty_model =
      "a regressão ficou sem nenhum termo a estimar (grau 0 e sem sazonalidade)",
    tr_series_error_no_block =
      "o bloco de F testa um bloco de termos que a regressão ligada não tem",
    tr_series_error_bad_criticos =
      paste0("o bloco não trouxe como decidir: nem p-valor nem tabela de críticos, ",
             "ou tabela sem o nível da decisão nomeado")
  )
  data.frame(class = names(e), when = unname(e), stringsAsFactors = FALSE)
}

#' O único lugar que levanta erro da coleção.
#'
#' Existe pra que toda mensagem saia com a mesma forma e a classe fique ENTRE
#' ASPAS no ponto de uso — é pelas aspas que a varredura de `test-errors.R`
#' encontra as classes usadas no código.
#' @noRd
.tr_series_abort <- function(class, fmt, ..., parent = NULL) {
  rlang::abort(sprintf(fmt, ...), class = class, parent = parent)
}

#' Confere que a coluna citada existe, e devolve o nome.
#'
#' Mesma recusa das irmãs a `any_of()`/`intersect()`: descartar nome
#' inexistente em silêncio faria `vendsa` virar série de coisa nenhuma.
#' @noRd
.tr_series_col <- function(data, col, param) {
  if (!col %in% names(data)) {
    .tr_series_abort("tr_series_error_unknown_column",
                     "Param '%s': coluna inexistente: %s. Disponíveis: %s.",
                     param, col, paste(names(data), collapse = ", "))
  }
  col
}

#' Recusa param obrigatório em branco, culpando o campo.
#' @noRd
.tr_series_obrigatorio <- function(valor, param) {
  if (!length(valor) || is.na(valor[[1]]) || !nzchar(trimws(as.character(valor)[[1]]))) {
    .tr_series_abort("tr_series_error_blank_param",
                     "Param '%s': campo obrigatório em branco. Preencha-o no card.", param)
  }
  trimws(as.character(valor)[[1]])
}

#' Valor fora do conjunto aceito de um param de escolha.
#'
#' O enum protege pela UI, mas estes `fn` são nível 1 e chamáveis no console;
#' sem isto um `switch` sem default devolveria NULL e o nó seguiria sem a
#' transformação pedida.
#' @noRd
.tr_series_option <- function(param, valor, aceitos) {
  .tr_series_abort("tr_series_error_bad_option",
                   "Param '%s': valor inválido '%s'. Aceitos: %s.",
                   param, paste(as.character(valor), collapse = ", "),
                   paste(aceitos, collapse = ", "))
}

#' Confere um enum e devolve o valor.
#' @noRd
.tr_series_enum <- function(valor, aceitos, param) {
  if (length(valor) != 1L || !valor %in% aceitos) .tr_series_option(param, valor, aceitos)
  valor
}

#' Inteiro dentro de uma faixa, com o campo nomeado no erro.
#'
#' O `tr_param_int(min = )` protege pelo card; aqui é o console. Sem isto
#' `tr_series_diff(x, ordem = 0)` devolveria a série intacta, verde.
#' @noRd
.tr_series_int <- function(valor, param, min = -Inf, max = Inf) {
  ok <- length(valor) == 1L && is.numeric(valor) && !is.na(valor) &&
    valor == round(valor) && valor >= min && valor <= max
  if (!ok) {
    faixa <- if (is.finite(max)) sprintf("entre %g e %g", min, max) else sprintf(">= %g", min)
    .tr_series_abort("tr_series_error_bad_option",
                     "Param '%s': tem que ser um inteiro %s (veio '%s').",
                     param, faixa, paste(as.character(valor), collapse = ", "))
  }
  as.integer(valor)
}

#' A série tem sazonalidade? Senão, erro nomeando o nó e a frequência.
#'
#' Frequência 1 é o caso que mais engana: `diff(x, lag = frequency(x))` numa
#' série anual é uma diferença de lag 1, e a "decomposição sazonal" de uma
#' série sem ciclo não tem o que decompor. Os dois rodam; nenhum quer dizer o
#' que o card promete.
#' @noRd
.tr_series_sazonal <- function(x, no, ciclos = 2L) {
  f <- stats::frequency(x)
  if (f <= 1) {
    .tr_series_abort("tr_series_error_no_season",
                     paste0("'%s' precisa de uma série sazonal, e esta chegou com frequência %g. ",
                            "Declare a frequência no nó que cria a série (12 para mensal, 4 para ",
                            "trimestral)."), no, f)
  }
  if (length(x) < ciclos * f) {
    .tr_series_abort("tr_series_error_too_short",
                     "'%s' precisa de pelo menos %d ciclos completos (%d observações); a série tem %d.",
                     no, as.integer(ciclos), as.integer(ciclos * f), length(x))
  }
  invisible(x)
}

#' Recusa faltante onde o método não o aceita, apontando o nó que resolve.
#' @noRd
.tr_series_sem_na <- function(x, no) {
  if (anyNA(x)) {
    .tr_series_abort("tr_series_error_missing_values",
                     paste0("'%s' não aceita faltantes, e a série tem %d. Ligue um ",
                            "'series/interpolate' antes, ou recorte a série com 'series/window'."),
                     no, sum(is.na(x)))
  }
  invisible(x)
}

#' A tabela de valores críticos serve pra decidir no nível pedido?
#'
#' Os críticos do `urca` não têm contrato nenhum entre um teste e outro: os do
#' `ur.za` chegam SEM NOMES e em ordem invertida (1%, 5%, 10%), os do
#' `ur.kpss` têm quatro colunas. Um bloco que monte a tabela errado faria
#' `criticos[["5%"]]` levantar um erro cru de índice, sem classe e sem dizer
#' qual teste — e devolver NA em silêncio seria PIOR, porque o `isTRUE()` do
#' construtor transformaria o NA num "não rejeita H0" confiante.
#' @noRd
.tr_series_criticos <- function(criticos, teste, nivel = "5%") {
  nomes <- names(criticos)
  if (is.null(nomes) || !nivel %in% nomes) {
    .tr_series_abort("tr_series_error_bad_criticos",
                     "'%s': a tabela de valores críticos precisa do nível %s nomeado, e veio %s.",
                     teste, nivel,
                     if (is.null(nomes)) "sem nome nenhum"
                     else sprintf("só com: %s", paste(nomes, collapse = ", ")))
  }
  invisible(criticos)
}

#' Tamanho mínimo, com a conta na mensagem.
#'
#' `validas` existe porque `length(x)` CONTA O FALTANTE, e para os blocos que
#' toleram NA esse é o tamanho errado: uma série de quarenta pontos com seis
#' valores passava por aqui e saía com veredito confiante. Quem recusa faltante
#' chama isto DEPOIS do `.tr_series_sem_na`, e aí os dois números são o mesmo —
#' por isso um default, e não uma troca de semântica para todo mundo.
#' @noRd
.tr_series_minimo <- function(x, n, no, motivo, validas = length(x)) {
  if (validas < n) {
    uma <- validas == 1L
    quantas <- sprintf("%d observaç%s", validas, if (uma) "ão" else "ões")
    # O total só é nomeado quando difere: num nó que recusou faltante lá atrás,
    # "6 de 6" seria ruído. Quando difere, é o número que explica a recusa.
    if (validas < length(x)) {
      quantas <- sprintf("%s válida%s (de %d)", quantas, if (uma) "" else "s", length(x))
    }
    .tr_series_abort("tr_series_error_too_short",
                     "'%s': a série tem %s, e %s pede pelo menos %d.",
                     no, quantas, motivo, as.integer(n))
  }
  invisible(x)
}
