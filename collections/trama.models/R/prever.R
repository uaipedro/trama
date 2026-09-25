# A predição: um nó COMUM, de dois inputs, sem nada sobre fluxo.
#
# É o achado da validação do desenho (Fase 7): o caso que motivou a região de
# fluxo inteira — um modelo ajustado fora, aplicado ponto a ponto — não
# precisava de uma peça NOVA de fluxo. Precisava desta, que já é útil fora de
# qualquer região: aplicar um `models/fit` a uma tabela nova. Dentro de uma
# região ela funciona sem mudar uma linha, porque é pura e não toma `.ctx` — a
# elevação automática (Fase 2) faz o resto.

.TR_MODELS_PREVER_INTERVALOS <- c("nenhum", "confianca", "predicao")

#' As colunas de `dados` conferidas contra o modelo, prontas para `predict`.
#'
#' Confere as colunas ANTES de chamar `predict()`, e é essa ordem que muda a
#' mensagem: `predict.lm()` já recusa coluna faltando ou nível de fator novo,
#' mas com o erro cru do R — "'newdata' had 1 rows but variables found have 32
#' rows" numa coluna esquecida, ou uma mensagem sobre contrastes/matriz de
#' design num nível novo. Nenhum dos dois nomeia a coluna. Aqui sim: o que o
#' modelo espera está em `modelo$dados` (as linhas USADAS no ajuste, com os
#' fatores já convertidos — o ambiente da fórmula não sobrevive ao RDS), então
#' conferir as colunas não exige reajustar nada. Comum a `predict`,
#' `confusion`, `roc` e `evaluate`, que preveem a mesma tabela do mesmo jeito.
#' @noRd
.tr_models_novos <- function(modelo, dados, no) {
  d <- as.data.frame(dados, stringsAsFactors = FALSE)
  # As preditoras vêm do contrato (`tr_models_info()$preditores`); nas classes
  # daqui são as variáveis do lado direito da fórmula, pelo NOME — `x` dentro
  # de `poly(x, 2)` ou `log(x)`, sem confundir a função com a coluna.
  preditoras <- tr_models_info(modelo)$preditores
  faltam <- setdiff(preditoras, names(d))
  if (length(faltam)) {
    .tr_models_abort("tr_models_error_unknown_column",
                     "'%s': 'dados' não tem a coluna '%s', que o modelo usa como preditora. Colunas de 'dados': %s.",
                     no, paste(faltam, collapse = "', '"), paste(names(d), collapse = ", "))
  }
  # Nível de fator que o ajuste nunca viu: `predict.lm()` recusaria dentro da
  # montagem da matriz de design, e só pela PRIMEIRA coluna que encontra. Aqui
  # a checagem roda para todas antes de chamar predict() uma vez sequer.
  for (v in preditoras) {
    # Os níveis do ajuste estão em `modelo$dados`; modelo de outra coleção sem
    # esse campo alinha os níveis no próprio `tr_models_predict_raw()`.
    niveis_ajuste <- if (is.data.frame(modelo$dados)) levels(modelo$dados[[v]]) else NULL
    if (is.null(niveis_ajuste)) next  # não é fator no ajuste: número é número
    vistos <- as.character(d[[v]])
    novos <- setdiff(unique(vistos[!is.na(vistos)]), niveis_ajuste)
    if (length(novos)) {
      .tr_models_abort("tr_models_error_unknown_level",
                       "'%s': a coluna '%s' tem o nível '%s', que não apareceu no ajuste. Níveis do ajuste: %s.",
                       no, v, paste(novos, collapse = "', '"), paste(niveis_ajuste, collapse = ", "))
    }
    # Fatorada com os MESMOS níveis do ajuste, na mesma ordem: sem isto,
    # `predict()` monta a matriz de design pelos níveis que aparecem em
    # `dados`, e um subconjunto deles desloca o nível de referência.
    d[[v]] <- factor(vistos, levels = niveis_ajuste)
  }
  d
}

#' Colunas lado a lado, sem `dplyr` (que a coleção não importa).
#' @noRd
.tr_models_juntar <- function(a, b) tibble::as_tibble(c(as.list(a), as.list(b)))

#' A previsão do contrato como colunas: `previsto`, `prob_<nivel>`, `extra`.
#' @noRd
.tr_models_prev_colunas <- function(p) {
  saida <- tibble::tibble(previsto = p$previsto)
  if (!is.null(p$prob)) {
    pr <- as.data.frame(unclass(p$prob))
    names(pr) <- .tr_models_colunas_prob(colnames(p$prob))
    saida <- .tr_models_juntar(saida, tibble::as_tibble(pr))
  }
  if (!is.null(p$extra)) saida <- .tr_models_juntar(saida, tibble::as_tibble(p$extra))
  saida
}

#' Prevê a resposta de um modelo: em dados novos, ou no próprio treino.
#'
#' @param modelo Um `models/fit`. Não a parcela subdividida — o ajuste dela é
#'   uma LISTA de modelos (`aovlist`, um por estrato de erro), sem um único
#'   `predict()` que valha; o card diz para usar o misto equivalente.
#' @param dados A tabela nova, ou `NULL`: sem ela, prevê as linhas do ajuste,
#'   pela `validacao`.
#' @param validacao Só sem `dados`: `"resubstituição"` (o modelo completo
#'   prevê as linhas que o ajustaram) ou `"cruzada"` (cada linha prevista por
#'   um modelo ajustado sem ela). Com `dados` é ignorada — uma tabela nova
#'   nunca esteve no ajuste, e já é uma previsão honesta.
#' @param intervalo `"nenhum"`, `"confianca"` (em torno da média prevista) ou
#'   `"predicao"` (em torno de uma observação nova, mais largo porque soma a
#'   variância do erro) — só em `models/lm`, e sem `dados` só por
#'   resubstituição. O GLM não tem `interval` em `predict.glm()` e o misto não
#'   tem erro padrão de predição fechado — os dois recusam.
#' @param confianca nível do intervalo (o `level` de `predict()`); só vale com
#'   intervalo diferente de `"nenhum"`.
#' @return `dados` (ou o treino) com `previsto`, `prob_<nivel>` na
#'   classificação, e `li`, `ls` com intervalo. Colunas com esses nomes que já
#'   existiam são SUBSTITUÍDAS: prever a saída de outra previsão não cria
#'   `previsto...2` ao lado da velha.
#' @export
tr_models_predict <- function(modelo, dados = NULL, validacao = "resubstituição", intervalo = "nenhum",
                              confianca = 0.95) {
  no <- "models/predict"
  .tr_models_modelo_conferir(modelo)
  if (identical(modelo$classe, "split")) .tr_models_split_sem_predict()
  validacao <- .tr_models_validacao(validacao)
  intervalo <- .tr_models_enum(intervalo, .TR_MODELS_PREVER_INTERVALOS, "intervalo")
  confianca <- .tr_models_num(confianca, "confianca", min = 0.5, max = 0.999)
  if (intervalo != "nenhum" && !identical(modelo$classe, "lm")) {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'models/predict': intervalo só está disponível em 'models/lm'. '%s' não ",
                            "tem um predict() com intervalo fechado — deixe 'intervalo' em 'nenhum'."),
                     modelo$classe %||% class(modelo)[[1]])
  }
  if (is.null(dados)) {
    base <- tibble::as_tibble(modelo$dados)
    if (intervalo != "nenhum" && validacao == "cruzada") {
      .tr_models_abort("tr_models_error_not_applicable",
                       paste0("'models/predict': intervalo não existe na validação cruzada (cada linha ",
                              "vem de um ajuste diferente). Use validacao = \"resubstituição\", ou ",
                              "deixe 'intervalo' em 'nenhum'."))
    }
    # Resubstituição com intervalo é o predict() nas linhas do ajuste: o mesmo
    # número de `fitted()`, com os limites que só `predict.lm()` dá.
    p <- if (intervalo != "nenhum") {
      tr_models_predict_raw(modelo, as.data.frame(modelo$dados), intervalo = intervalo, confianca = confianca)
    } else {
      tr_models_predict_cv(modelo, validacao)
    }
  } else {
    base <- tibble::as_tibble(dados)
    d <- .tr_models_novos(modelo, dados, no)
    p <- tr_models_predict_raw(modelo, d, intervalo = intervalo, confianca = confianca)
  }
  saida <- .tr_models_prev_colunas(p)
  base <- base[, setdiff(names(base), names(saida)), drop = FALSE]
  .tr_models_juntar(base, saida)
}
