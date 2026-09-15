#' Classes de erro da coleção `sampling`.
#'
#' Mesmo princípio de `trama::tr_errors()` e das coleções irmãs, e mesmo teste
#' de duas direções: uma tabela que um teste confere contra o código, pra não
#' envelhecer em silêncio.
#'
#' Amostragem tem o seu modo de falha que sai verde: o estrato com uma unidade
#' só, cuja variância o R devolve como `NA` e o relatório imprime como zero; o
#' plano de 40 conglomerados ligado numa seleção por unidades, que sorteia 40
#' fazendas; o total estimado sem peso, que é o total da AMOSTRA. Aqui vira card
#' vermelho, dizendo o que fazer.
#' @return data.frame com `class` e `when`.
#' @export
tr_sampling_errors <- function() {
  e <- c(
    tr_sampling_error_unknown_column = "param nomeia coluna que não existe na tabela de entrada",
    tr_sampling_error_blank_param = "param obrigatório deixado em branco no card",
    tr_sampling_error_bad_option = "param de escolha ou número fora do conjunto aceito",
    tr_sampling_error_not_numeric = "coluna usada como variável, peso ou tamanho não é numérica",
    tr_sampling_error_too_large = "a amostra pedida é maior que a população (ou que um estrato)",
    tr_sampling_error_too_few = "unidades de menos para sortear ou estimar a variância",
    tr_sampling_error_lonely_psu = "estrato com uma unidade primária só: a variância do desenho não existe",
    tr_sampling_error_plan_mismatch = "o plano ligado é de outro tipo de desenho, ou cita estratos que o cadastro não tem",
    tr_sampling_error_bad_size = "medida de tamanho, peso ou população com valor faltante, zero ou negativo",
    tr_sampling_error_no_weights = "amostra declarada sem peso e sem população para calculá-lo",
    tr_sampling_error_missing_total = "pós-estrato da amostra sem total na tabela de totais, ou vice-versa",
    tr_sampling_error_no_convergence = "o raking não chegou aos totais no número de rodadas",
    tr_sampling_error_no_population = "a amostra não guarda a população, e simular pede re-sortear",
    tr_sampling_error_not_a_plan = "o nó produziu um objeto que não é plano, e o tipo sampling/plan o recusa",
    tr_sampling_error_not_a_sample = "o nó produziu um objeto que não é amostra, e o tipo sampling/sample o recusa",
    tr_sampling_error_not_an_estimate =
      "o nó produziu um objeto que não é estimativa, e o tipo sampling/estimate o recusa",
    tr_sampling_error_not_a_simulation =
      "o nó produziu um objeto que não é simulação, e o tipo sampling/simulation o recusa"
  )
  data.frame(class = names(e), when = unname(e), stringsAsFactors = FALSE)
}

#' O único lugar que levanta erro da coleção.
#'
#' Existe pra que toda mensagem saia com a mesma forma e a classe fique ENTRE
#' ASPAS no ponto de uso — é pelas aspas que a varredura de `test-errors.R`
#' encontra as classes usadas no código.
#' @noRd
.tr_sampling_abort <- function(class, fmt, ...) {
  rlang::abort(sprintf(fmt, ...), class = class)
}

#' O campo veio preenchido? Um lugar só para o teste de "em branco".
#' @noRd
.tr_sampling_preenchido <- function(valor) {
  length(valor) > 0L && !is.na(valor[[1]]) && nzchar(trimws(as.character(valor)[[1]]))
}

#' Confere um enum e devolve o valor.
#'
#' O enum protege pela UI, mas os `fn` são nível 1 e chamáveis no console; sem
#' isto um `switch` sem default devolveria NULL e o nó seguiria sem a opção.
#' @noRd
.tr_sampling_enum <- function(valor, aceitos, param) {
  if (length(valor) != 1L || is.na(valor) || !valor %in% aceitos) {
    .tr_sampling_abort("tr_sampling_error_bad_option",
                       "Param '%s': valor inválido '%s'. Aceitos: %s.",
                       param, paste(as.character(valor), collapse = ", "),
                       paste(aceitos, collapse = ", "))
  }
  valor
}

#' Número dentro de uma faixa. `aberto_min` exclui o mínimo (erro > 0).
#' @noRd
.tr_sampling_num <- function(valor, param, min = -Inf, max = Inf, aberto_min = FALSE) {
  ok <- length(valor) == 1L && is.numeric(valor) && !is.na(valor) &&
    (if (aberto_min) valor > min else valor >= min) && valor <= max
  if (!ok) {
    .tr_sampling_abort("tr_sampling_error_bad_option",
                       "Param '%s': tem que ser um número %s %g e <= %g (veio '%s').",
                       param, if (aberto_min) ">" else ">=", min, max,
                       paste(as.character(valor), collapse = ", "))
  }
  as.numeric(valor)
}
