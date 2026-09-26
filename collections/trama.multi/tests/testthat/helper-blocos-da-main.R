# Atalhos para os testes que vieram da main (IC perfilado, Firth, métricas da
# confusão, ROC com DeLong e Hand & Till, curva PR). Lá esses blocos eram da
# multi (`multi/logistic_coefficients`, `multi/confusion`, `multi/roc`,
# `multi/pr_curve`); aqui são os da `trama.models`, lidos pelo contrato. O
# atalho devolve a tabela no formato da main (razões de chances exponenciadas,
# `intervalo`), para que os oráculos fiquem como foram escritos.
tr_multi_logistic_coefficients <- function(modelo, escala = "unidade", confianca = 0.95, intervalo = "padrão") {
  e <- trama.models::tr_models_coefficients(modelo, escala = escala, confianca = confianca, intervalo = intervalo)
  t <- e$tabela
  li <- grep("^li_", names(t), value = TRUE); ls <- grep("^ls_", names(t), value = TRUE)
  tibble::tibble(grupo = t$grupo, termo = t$termo, coeficiente = t$estimativa, erro_padrao = t$erro_padrao,
                 z = t$z, p_valor = t$p_valor, razao_chances = exp(t$estimativa),
                 ic_inf = exp(t[[li]]), ic_sup = exp(t[[ls]]),
                 intervalo = if (grepl("^IC perfilado", e$nota)) "perfilado" else "Wald")
}

# As probabilidades por validação (antes `.tr_multi_prever`): `prob`, `classe`
# e o grupo real `g`, do contrato.
.tr_multi_prever <- function(modelo, validacao, no = NULL) {
  p <- trama.models::tr_models_predict_cv(modelo, validacao)
  g <- droplevels(as.factor(modelo$dados[[modelo$grupo]]))
  list(prob = p$prob, classe = factor(as.character(p$previsto), levels = levels(g)), g = g)
}

# As contas internas, agora na `trama.models`.
.tr_multi_pr_pontos <- function(positivo, prob) utils::getFromNamespace(".tr_models_pr_pontos", "trama.models")(positivo, prob)
.tr_multi_roc_curva <- function(score, positivo) utils::getFromNamespace(".tr_models_roc_curva", "trama.models")(score, positivo)
.tr_multi_auc_delong <- function(score, positivo, confianca) {
  utils::getFromNamespace(".tr_models_auc_delong", "trama.models")(score, positivo, confianca)
}
.tr_multi_auc_hand_till <- function(prob, g) {
  utils::getFromNamespace(".tr_models_auc_hand_till", "trama.models")(prob, as.character(g), levels(g))
}
.tr_multi_metricas <- function(m) utils::getFromNamespace(".tr_models_metricas_matriz", "trama.models")(m)
tr_multi_roc <- function(modelo, validacao = "cruzada", confianca = 0.95) {
  trama.models::tr_models_roc(modelo, validacao = validacao, confianca = confianca)
}
tr_multi_pr_curve <- function(modelo, validacao = "cruzada") trama.models::tr_models_pr_curve(modelo, validacao = validacao)
