# Proveniência das linhas: o isolamento do teste imposto por construção.
#
# O `ml/split` marca cada saída com o atributo `tr_ml_origem` = list(papel =
# "treino" | "teste", divisao = <hash>). Escolhemos atributo, e não coluna
# oculta, porque é o menos invasivo que sobrevive a tudo que o fluxo faz com
# a tabela: o tipo `data/table` guarda em RDS (atributos preservados na ida e
# volta do store e na releitura do cache), e subconjunto, `dplyr::filter` e
# `mutate` reconstroem a tabela copiando os atributos. Uma coluna apareceria no
# card, na exportação e em `cols` vazio; o atributo não muda os dados.
#
# O que a marca não cobre: juntar treino e teste (`rbind`/`bind_rows` ficam com
# o atributo da primeira tabela) e tabelas cuja divisão o usuário fez por fora;
# sem marca, os blocos se comportam como antes e os pressupostos explicam.
#
# Regras:
# - ajustar (`ml/<modelo>`, `ml/tune`, `ml/nested_cv`) com linhas "teste" é
#   erro `tr_ml_error_test_leak`: treinar no teste invalida toda avaliação;
# - `ml/predict` propaga a marca e recusa prever o teste de OUTRA divisão com um
#   modelo ajustado no treino de uma divisão marcada (linhas do teste podem ter
#   estado no treino): `tr_ml_error_split_mismatch`;
# - avaliar (`ml/evaluate`, `ml/confusion`, `ml/roc`, `ml/pr_curve`) linhas
#   "treino" é erro `tr_ml_error_train_eval`, salvo `permitir_treino = TRUE`,
#   que devolve o resultado com a nota de otimismo e um aviso.

.tr_ml_origem_attr <- "tr_ml_origem"

.tr_ml_marcar <- function(x, papel, divisao) {
  attr(x, .tr_ml_origem_attr) <- list(papel = papel, divisao = divisao)
  x
}

.tr_ml_origem <- function(x) {
  o <- attr(x, .tr_ml_origem_attr, exact = TRUE)
  if (is.list(o) && length(o$papel) == 1L && o$papel %in% c("treino", "teste")) o else NULL
}

.tr_ml_papel <- function(x) .tr_ml_origem(x)$papel %||% NA_character_

# Identificador da divisão: depende só dos dados e das linhas sorteadas, então
# a mesma divisão refeita (ou relida do cache) tem o mesmo id.
.tr_ml_divisao_id <- function(dados, idx) {
  substr(rlang::hash(list(rlang::hash(as.data.frame(dados)), as.integer(idx))), 1L, 16L)
}

.tr_ml_exigir_nao_teste <- function(dados) {
  if (identical(.tr_ml_papel(dados), "teste")) {
    .tr_ml_abort("tr_ml_error_test_leak", paste(
      "Estas linhas s\u{E3}o a sa\u{ED}da teste do `ml/split`: ajustar nelas treina no teste",
      "e invalida toda avalia\u{E7}\u{E3}o. Ligue a sa\u{ED}da treino; o teste vai s\u{F3} ao `ml/predict`."))
  }
  invisible(TRUE)
}

.tr_ml_nota_treino <- paste(
  "Avalia\u{E7}\u{E3}o no treino \u{E9} otimista: as linhas ajustaram o modelo e n\u{E3}o",
  "medem generaliza\u{E7}\u{E3}o. Reporte o desempenho no teste.")

# Devolve a nota (ou "") e recusa linhas de treino sem opt-in.
.tr_ml_checar_avaliacao <- function(dados, permitir_treino) {
  if (length(permitir_treino) != 1L || !is.logical(permitir_treino) || is.na(permitir_treino)) {
    .tr_ml_abort("tr_ml_error_bad_param", "Param 'permitir_treino' deve ser TRUE ou FALSE.")
  }
  if (!identical(.tr_ml_papel(dados), "treino")) return("")
  if (!permitir_treino) {
    .tr_ml_abort("tr_ml_error_train_eval", paste(
      "Estas previs\u{F5}es s\u{E3}o das linhas de treino do `ml/split`: a avalia\u{E7}\u{E3}o no treino",
      "\u{E9} otimista. Avalie a sa\u{ED}da teste passada pelo `ml/predict`, ou marque",
      "`permitir_treino` para medir o ajuste no treino de prop\u{F3}sito."))
  }
  warning(.tr_ml_nota_treino, call. = FALSE)
  .tr_ml_nota_treino
}
