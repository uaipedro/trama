#' Classes de erro da coleção `experiments`.
#'
#' Mesmo princípio de `trama::tr_errors()` e das coleções irmãs: uma tabela
#' que um teste confere contra o código nas duas direções, pra não envelhecer
#' em silêncio.
#'
#' Experimento tem o seu modo de falha que sai verde: o quadrado latino de dois
#' tratamentos, que não deixa grau de liberdade para o resíduo; o gerador de um
#' fracionado que repete um fator e deixa um efeito principal confundido com a
#' média; o bloco incompleto que não é balanceado e faz o λ variar entre pares.
#' Aqui vira card vermelho dizendo o que fazer.
#'
#' Os nós de análise (`R/nos_analisar.R`, de outro autor) podem acrescentar as
#' suas classes em `.tr_experiments_errors_analisar()`; a tabela junta as duas
#' para o teste de duas direções continuar valendo para a coleção inteira.
#' @return data.frame com `class` e `when`.
#' @export
tr_experiments_errors <- function() {
  e <- c(
    tr_experiments_error_blank_param = "param obrigatório deixado em branco no card",
    tr_experiments_error_bad_option = "param de escolha ou número fora do conjunto aceito",
    tr_experiments_error_bad_factors =
      "a lista de fatores não segue 'nome: nível, nível; ...' ou não serve à estrutura escolhida",
    tr_experiments_error_reserved_name =
      "fator com nome de coluna estrutural (bloco, parcela, ordem...) ou repetido",
    tr_experiments_error_bad_generator =
      "gerador do fracionado, ou efeito a confundir, malformado ou dependente dos outros",
    tr_experiments_error_no_design = "não há construção para os parâmetros pedidos (ex.: BIB grande demais)",
    tr_experiments_error_no_residual = "o delineamento pedido não deixa grau de liberdade para o resíduo",
    tr_experiments_error_not_a_plan =
      "o nó produziu um objeto que não é plano, e o tipo experiments/plan o recusa",
    tr_experiments_error_bad_term =
      "o termo de experiments/effect não cabe no plano (fator ausente, célula sem efeito, contraste inválido)",
    tr_experiments_error_bad_response =
      "o erro de experiments/error pedido não serve (sd negativo, correlação sem indivíduo, perturbação fora da normal)",
    tr_experiments_error_response_closed =
      "termo ou erro ligado depois de a resposta já ter sido fechada por experiments/error",
    tr_experiments_error_no_terms = "experiments/error recebeu um plano sem nenhum termo"
  )
  base <- data.frame(class = names(e), when = unname(e), stringsAsFactors = FALSE)
  extra <- if (exists(".tr_experiments_errors_analisar", mode = "function")) {
    get(".tr_experiments_errors_analisar", mode = "function")()
  }
  if (is.data.frame(extra)) rbind(base, extra[, c("class", "when")]) else base
}

#' O único lugar que levanta erro da coleção.
#'
#' A classe fica ENTRE ASPAS no ponto de uso: é pelas aspas que a varredura de
#' `test-errors.R` encontra as classes usadas no código.
#' @noRd
.tr_experiments_abort <- function(class, fmt, ...) {
  rlang::abort(sprintf(fmt, ...), class = class)
}
