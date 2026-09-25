#' Classes de erro da coleção `models`.
#'
#' Mesmo princípio de `trama::tr_errors()` e das coleções irmãs, e mesmo teste
#' de duas direções: uma tabela que um teste confere contra o código, pra não
#' envelhecer em silêncio.
#'
#' Modelo tem o seu modo de falha que sai verde: o `lm` ajusta dose 50/100/150
#' como reta quando se queria tratamento, o Shapiro roda nos resíduos de um GLM
#' de contagem onde normalidade nunca foi pressuposto, o `anova` compara dois
#' modelos ajustados em linhas diferentes. Nada disso erra no R. Aqui vira card
#' vermelho, dizendo o que fazer.
#' @return data.frame com `class` e `when`.
#' @export
tr_models_errors <- function() {
  e <- c(
    tr_models_error_unknown_column =
      "param nomeia coluna que não existe na tabela de entrada, ou o modelo precisa de uma coluna que a tabela não tem",
    tr_models_error_blank_param = "param obrigatório deixado em branco no card",
    tr_models_error_bad_option = "param de escolha ou número fora do conjunto aceito",
    tr_models_error_bad_formula = "a fórmula digitada não parseia, não tem resposta ou cita coluna inexistente",
    tr_models_error_not_numeric = "coluna usada como resposta ou medida não é numérica",
    tr_models_error_one_level = "fator do delineamento ou grupo com um nível só",
    tr_models_error_too_few_rows = "observações de menos para o modelo ou o teste",
    tr_models_error_no_residual_df = "o modelo não deixa grau de liberdade para o resíduo",
    tr_models_error_fit = "o ajuste ou o teste falhou dentro do R",
    tr_models_error_not_applicable = "o bloco não se aplica a esse tipo de modelo",
    tr_models_error_block_design =
      "o teste não tem correção publicada para o delineamento: Bartlett com bloco (DBC, DQL; use o models/levene) ou Levene/Bartlett na parcela subdividida",
    tr_models_error_two_groups = "o teste compara dois grupos, e a coluna do grupo não tem dois",
    tr_models_error_not_nested = "os dois modelos comparados não são da mesma família ou não usam as mesmas linhas",
    tr_models_error_unknown_level =
      "nível citado (o controle do Dunnett, ou um nível novo em 'newdata') não existe no fator do ajuste",
    tr_models_error_not_a_fit = "o nó produziu um objeto que não é modelo, e o tipo models/fit o recusa",
    tr_models_error_not_effects =
      "o nó produziu um objeto que não é quadro de efeitos, e o tipo models/effects o recusa",
    tr_models_error_not_a_test = "o nó produziu um objeto que não é teste, e o tipo models/test o recusa",
    tr_models_error_not_emm =
      "o nó produziu um objeto que não é grade de médias, e o tipo models/emm o recusa"
  )
  data.frame(class = names(e), when = unname(e), stringsAsFactors = FALSE)
}

#' O único lugar que levanta erro da coleção.
#'
#' Existe pra que toda mensagem saia com a mesma forma e a classe fique ENTRE
#' ASPAS no ponto de uso — é pelas aspas que a varredura de `test-errors.R`
#' encontra as classes usadas no código.
#' @noRd
.tr_models_abort <- function(class, fmt, ..., parent = NULL) {
  rlang::abort(sprintf(fmt, ...), class = class, parent = parent)
}

#' Recusa param obrigatório em branco, culpando o campo.
#' @noRd
.tr_models_obrigatorio <- function(valor, param) {
  if (!.tr_models_preenchido(valor)) {
    .tr_models_abort("tr_models_error_blank_param",
                     "Param '%s': campo obrigatório em branco. Preencha-o no card.", param)
  }
  trimws(as.character(valor)[[1]])
}

#' O campo veio preenchido? Um lugar só para o teste de "em branco".
#' @noRd
.tr_models_preenchido <- function(valor) {
  length(valor) > 0L && !is.na(valor[[1]]) && nzchar(trimws(as.character(valor)[[1]]))
}

#' Confere um enum e devolve o valor.
#'
#' O enum protege pela UI, mas os `fn` são nível 1 e chamáveis no console; sem
#' isto um `switch` sem default devolveria NULL e o nó seguiria sem a opção.
#' @noRd
.tr_models_enum <- function(valor, aceitos, param) {
  if (length(valor) != 1L || is.na(valor) || !valor %in% aceitos) {
    .tr_models_abort("tr_models_error_bad_option",
                     "Param '%s': valor inválido '%s'. Aceitos: %s.",
                     param, paste(as.character(valor), collapse = ", "),
                     paste(aceitos, collapse = ", "))
  }
  valor
}

#' Número dentro de uma faixa fechada.
#' @noRd
.tr_models_num <- function(valor, param, min = -Inf, max = Inf) {
  ok <- length(valor) == 1L && is.numeric(valor) && !is.na(valor) &&
    valor >= min && valor <= max
  if (!ok) {
    .tr_models_abort("tr_models_error_bad_option",
                     "Param '%s': tem que ser um número entre %g e %g (veio '%s').",
                     param, min, max, paste(as.character(valor), collapse = ", "))
  }
  as.numeric(valor)
}

#' Roda um ajuste reclassificando a falha, com a causa logo abaixo.
#'
#' `lm`, `lmer`, `emmeans` e `car` falham com mensagens em inglês e sem classe;
#' o `parent = e` mantém a mensagem original inteira, e a de cima diz QUAL nó.
#' Erro que já é da coleção passa direto, para não virar "o ajuste falhou"
#' escondendo a coluna que faltou.
#' @noRd
.tr_models_ajustar <- function(code, no) {
  tryCatch(force(code), error = function(e) {
    if (any(startsWith(class(e), "tr_models_error_"))) stop(e)
    .tr_models_abort("tr_models_error_fit", "'%s': o ajuste falhou.", no, parent = e)
  })
}
