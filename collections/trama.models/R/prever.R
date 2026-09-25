# A predição: um nó COMUM, de dois inputs, sem nada sobre fluxo.
#
# É o achado da validação do desenho (Fase 7): o caso que motivou a região de
# fluxo inteira — um modelo ajustado fora, aplicado ponto a ponto — não
# precisava de uma peça NOVA de fluxo. Precisava desta, que já é útil fora de
# qualquer região: aplicar um `models/fit` a uma tabela nova. Dentro de uma
# região ela funciona sem mudar uma linha, porque é pura e não toma `.ctx` — a
# elevação automática (Fase 2) faz o resto.

.TR_MODELS_PREVER_INTERVALOS <- c("nenhum", "confianca", "predicao")

#' Prevê a resposta de um modelo ajustado em dados novos.
#'
#' Confere as colunas ANTES de chamar `predict()`, e é essa ordem que muda a
#' mensagem: `predict.lm()` já recusa coluna faltando ou nível de fator novo,
#' mas com o erro cru do R — "'newdata' had 1 rows but variables found have 32
#' rows" numa coluna esquecida, ou uma mensagem sobre contrastes/matriz de
#' design num nível novo. Nenhum dos dois nomeia a coluna. Aqui sim: o que o
#' modelo espera está em `modelo$dados` (as linhas USADAS no ajuste, com os
#' fatores já convertidos — o mesmo campo que `models/coefficients` e o SQ
#' tipo III reencontram, porque o ambiente da fórmula não sobrevive ao RDS),
#' então conferir as colunas de `dados` contra ele não exige reajustar nada.
#'
#' @param modelo Um `models/fit`. Não a parcela subdividida — o ajuste dela é
#'   uma LISTA de modelos (`aovlist`, um por estrato de erro), sem um único
#'   `predict()` que valha; o card diz para usar o misto equivalente.
#' @param dados A tabela nova.
#' @param intervalo `"nenhum"`, `"confianca"` (em torno da média prevista) ou
#'   `"predicao"` (em torno de uma observação nova, mais largo porque soma a
#'   variância do erro) — só em `models/lm`. O GLM não tem `interval` em
#'   `predict.glm()` (dar o de `lm` exigiria linearizar a variância na escala
#'   da ligação, que o card não faz sozinho) e o misto não tem erro padrão de
#'   predição fechado — os dois recusam.
#' @param confianca nível do intervalo (o `level` de `predict()`); só vale com
#'   intervalo diferente de `"nenhum"`.
#' @return `dados` com `previsto` (e `li`, `ls` com intervalo) anexadas.
#' @export
tr_models_predict <- function(modelo, dados, intervalo = "nenhum", confianca = 0.95) {
  .tr_models_modelo_conferir(modelo)
  if (identical(modelo$classe, "split")) .tr_models_split_sem_predict()
  intervalo <- .tr_models_enum(intervalo, .TR_MODELS_PREVER_INTERVALOS, "intervalo")
  confianca <- .tr_models_num(confianca, "confianca", min = 0.5, max = 0.999)
  if (intervalo != "nenhum" && !identical(modelo$classe, "lm")) {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'models/predict': intervalo só está disponível em 'models/lm'. '%s' não ",
                            "tem um predict() com intervalo fechado — deixe 'intervalo' em 'nenhum'."),
                     modelo$classe %||% class(modelo)[[1]])
  }

  d <- as.data.frame(dados, stringsAsFactors = FALSE)
  # As preditoras vêm do contrato (`tr_models_info()$preditores`); nas classes
  # daqui são as variáveis do lado direito da fórmula, pelo NOME — `x` dentro
  # de `poly(x, 2)` ou `log(x)`, sem confundir a função com a coluna.
  preditoras <- tr_models_info(modelo)$preditores

  faltam <- setdiff(preditoras, names(d))
  if (length(faltam)) {
    .tr_models_abort("tr_models_error_unknown_column",
                     "'models/predict': 'dados' não tem a coluna '%s', que o modelo usa como preditora. Colunas de 'dados': %s.",
                     paste(faltam, collapse = "', '"), paste(names(d), collapse = ", "))
  }

  # Nível de fator que o ajuste nunca viu: `predict.lm()` recusaria dentro da
  # montagem da matriz de design, com uma mensagem sobre "factor ... has new
  # levels" que já nomeia a coluna — mas só a PRIMEIRA que encontra, e depois
  # de já ter tentado montar a matriz inteira. Aqui a checagem é explícita e
  # roda para todas as colunas antes de chamar predict() uma vez sequer.
  for (v in preditoras) {
    # Os níveis do ajuste estão em `modelo$dados`; modelo de outra coleção sem
    # esse campo alinha os níveis no próprio `tr_models_predict_raw()`.
    niveis_ajuste <- if (is.data.frame(modelo$dados)) levels(modelo$dados[[v]]) else NULL
    if (is.null(niveis_ajuste)) next  # não é fator no ajuste: número é número
    vistos <- as.character(d[[v]])
    novos <- setdiff(unique(vistos[!is.na(vistos)]), niveis_ajuste)
    if (length(novos)) {
      .tr_models_abort("tr_models_error_unknown_level",
                       "'models/predict': a coluna '%s' tem o nível '%s', que não apareceu no ajuste. Níveis do ajuste: %s.",
                       v, paste(novos, collapse = "', '"), paste(niveis_ajuste, collapse = ", "))
    }
    # Fatorada com os MESMOS níveis do ajuste, na mesma ordem: sem isto,
    # `predict()` monta a matriz de design pelos níveis que aparecem em
    # `dados`, e um subconjunto delas (só dois dos três tratamentos, por
    # exemplo) desloca qual coluna é o nível de referência.
    d[[v]] <- factor(vistos, levels = niveis_ajuste)
  }

  p <- tr_models_predict_raw(modelo, d, intervalo = intervalo, confianca = confianca)
  saida <- tibble::tibble(previsto = p$previsto)
  if (!is.null(p$extra)) saida <- dplyr::bind_cols(saida, tibble::as_tibble(p$extra))
  dplyr::bind_cols(tibble::as_tibble(dados), saida)
}
