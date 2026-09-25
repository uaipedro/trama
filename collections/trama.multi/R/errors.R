#' Classes de erro da coleção `multi`.
#'
#' Mesmo princípio de `trama::tr_errors()` e das coleções irmãs, e mesmo teste
#' de duas direções: uma tabela que um teste confere contra o código, pra não
#' envelhecer em silêncio.
#'
#' Análise multivariada tem o seu modo de falha que sai verde: o `prcomp` roda
#' com uma coluna de CEP entre as medidas, o `factanal` devolve cargas com a
#' matriz quase singular, a LDA classifica com um grupo de dois indivíduos. Nada
#' disso erra no R. Aqui vira card vermelho, nomeando a coluna ou o grupo.
#' @return data.frame com `class` e `when`.
#' @export
tr_multi_errors <- function() {
  e <- c(
    tr_multi_error_unknown_column = "param nomeia coluna que não existe na tabela de entrada",
    tr_multi_error_blank_param = "param obrigatório deixado em branco no card",
    tr_multi_error_bad_option = "param de escolha ou número fora do conjunto aceito",
    tr_multi_error_not_numeric = "coluna pedida como variável não é numérica",
    tr_multi_error_too_few_variables = "menos variáveis numéricas do que a técnica precisa",
    tr_multi_error_too_few_rows = "menos observações do que a técnica precisa",
    tr_multi_error_missing_values = "faltantes nas variáveis, e nenhum método da coleção os aceita",
    tr_multi_error_constant_variable = "variável com variância zero: a correlação dela não existe",
    tr_multi_error_singular_matrix =
      "matriz de correlação ou covariância singular (variável que é combinação das outras)",
    tr_multi_error_too_many_factors = "mais fatores do que o modelo consegue identificar",
    tr_multi_error_fit = "o ajuste falhou",
    tr_multi_error_heywood =
      "caso Heywood no eixo principal: comunalidade acima de 1, solução impossível",
    tr_multi_error_one_group = "a coluna do grupo tem menos de dois grupos",
    tr_multi_error_small_group = "algum grupo tem observações de menos para o método",
    tr_multi_error_not_a_pca =
      "o nó produziu um objeto que não é PCA, e o tipo multi/pca o recusa",
    tr_multi_error_not_a_fa =
      "o nó produziu um objeto que não é análise fatorial, e o tipo multi/fa o recusa",
    tr_multi_error_not_a_lda =
      "o bloco lê uma discriminante e chegou outro modelo (ou um objeto sem os campos dela)",
    tr_multi_error_not_a_logit =
      "o bloco lê uma regressão logística e chegou outro modelo (ou um objeto sem os campos dela)",
    tr_multi_error_separation =
      "separação completa: algum grupo é separado sem sobreposição e os coeficientes vão ao infinito",
    tr_multi_error_jackknife_replicate = "a réplica do jackknife sem uma das linhas falhou",
    tr_multi_error_too_many_rows = "linhas demais para reajustar a técnica uma vez por linha",
    tr_multi_error_bad_input = "entradas opcionais ligadas de um jeito que o nó não aceita (as duas, ou nenhuma)",
    tr_multi_error_not_a_dist =
      "o nó produziu (ou recebeu) um objeto que não é matriz de distância, e o tipo multi/dist o recusa",
    tr_multi_error_not_a_cluster =
      "o nó produziu (ou recebeu) um objeto que não é agrupamento, e o tipo multi/cluster o recusa"
  )
  data.frame(class = names(e), when = unname(e), stringsAsFactors = FALSE)
}

#' O único lugar que levanta erro da coleção.
#'
#' Existe pra que toda mensagem saia com a mesma forma e a classe fique ENTRE
#' ASPAS no ponto de uso — é pelas aspas que a varredura de `test-errors.R`
#' encontra as classes usadas no código.
#' @noRd
.tr_multi_abort <- function(class, fmt, ..., parent = NULL) {
  rlang::abort(sprintf(fmt, ...), class = class, parent = parent)
}

#' Recusa param obrigatório em branco, culpando o campo.
#' @noRd
.tr_multi_obrigatorio <- function(valor, param) {
  if (!length(valor) || is.na(valor[[1]]) || !nzchar(trimws(as.character(valor)[[1]]))) {
    .tr_multi_abort("tr_multi_error_blank_param",
                    "Param '%s': campo obrigatório em branco. Preencha-o no card.", param)
  }
  trimws(as.character(valor)[[1]])
}

#' Confere um enum e devolve o valor.
#'
#' O enum protege pela UI, mas os `fn` são nível 1 e chamáveis no console; sem
#' isto um `switch` sem default devolveria NULL e o nó seguiria sem a opção.
#' @noRd
.tr_multi_enum <- function(valor, aceitos, param) {
  if (length(valor) != 1L || is.na(valor) || !valor %in% aceitos) {
    .tr_multi_abort("tr_multi_error_bad_option",
                    "Param '%s': valor inválido '%s'. Aceitos: %s.",
                    param, paste(as.character(valor), collapse = ", "),
                    paste(aceitos, collapse = ", "))
  }
  valor
}

#' Inteiro dentro de uma faixa, com o campo nomeado no erro.
#' @noRd
.tr_multi_int <- function(valor, param, min = -Inf, max = Inf) {
  ok <- length(valor) == 1L && is.numeric(valor) && !is.na(valor) &&
    valor == round(valor) && valor >= min && valor <= max
  if (!ok) {
    faixa <- if (is.finite(max)) sprintf("entre %g e %g", min, max) else sprintf(">= %g", min)
    .tr_multi_abort("tr_multi_error_bad_option",
                    "Param '%s': tem que ser um inteiro %s (veio '%s').",
                    param, faixa, paste(as.character(valor), collapse = ", "))
  }
  as.integer(valor)
}

#' Número dentro de uma faixa fechada.
#' @noRd
.tr_multi_num <- function(valor, param, min = -Inf, max = Inf) {
  ok <- length(valor) == 1L && is.numeric(valor) && !is.na(valor) &&
    valor >= min && valor <= max
  if (!ok) {
    .tr_multi_abort("tr_multi_error_bad_option",
                    "Param '%s': tem que ser um número entre %g e %g (veio '%s').",
                    param, min, max, paste(as.character(valor), collapse = ", "))
  }
  as.numeric(valor)
}

#' Roda um ajuste reclassificando a falha, com a causa logo abaixo.
#'
#' `factanal`, `lda` e `solve` falham com mensagens em inglês e sem classe; o
#' `parent = e` mantém a mensagem original inteira, e a de cima diz QUAL nó.
#' @noRd
.tr_multi_ajustar <- function(code, no) {
  tryCatch(force(code), error = function(e) {
    if (inherits(e, "rlang_error") && any(startsWith(class(e), "tr_multi_error_"))) stop(e)
    .tr_multi_abort("tr_multi_error_fit", "'%s': o ajuste falhou.", no, parent = e)
  })
}
