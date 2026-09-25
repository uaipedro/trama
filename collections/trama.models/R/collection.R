# A coleção `models`: o registro dos tipos, das categorias e dos nós.
#
# As declarações dos nós moram em `R/nos_*.R`, por aba, e não num arquivo único
# como na `series`: são trinta e quatro páginas de ajuda, e ler a página com o
# grupo de funções ao lado é o que pega a ajuda que promete o que o código não
# faz.

#' A coleção `models`.
#'
#' Carrega DEPOIS de `trama.data` e `trama.view`: as portas usam `data/table` e
#' `view/plot`, e o registro recusa porta com tipo desconhecido.
#'
#' Os ids de categoria têm prefixo (`modelo_*`) porque o registro de categorias
#' é GLOBAL: uma categoria `testes` aqui sobrescreveria em silêncio a de outra
#' coleção que usasse o mesmo id.
#' @export
trama_collection <- function() {
  trama::tr_collection(
    id = "models", version = "0.1.0", label = "Modelos",
    js = "trama/index.js", css = "trama/models.css",
    types = list(models_fit_type(), models_effects_type(), models_test_type(), models_emm_type()),
    adapters = .tr_models_adapters(),
    # O corte das abas é pela PERGUNTA: ajustar, ler o ajuste, comparar médias,
    # conferir os pressupostos, testar sem modelo. Os delineamentos têm aba
    # própria porque quem planta um experimento procura "DBC", e não "lm".
    categories = list(
      trama::tr_category("modelo_fonte",        "Fonte", role = "origem"),
      trama::tr_category("modelo_ajustar",      "Ajustar", role = "ajuste"),
      trama::tr_category("modelo_anova",        "ANOVA", role = "ajuste"),
      trama::tr_category("modelo_resumir",      "Resumir", role = "leitura"),
      trama::tr_category("modelo_medias",       "Médias", role = "leitura"),
      trama::tr_category("modelo_pressupostos", "Pressupostos", role = "avaliacao"),
      # Prever e medir a previsão: a pergunta é "quanto acerta num caso novo",
      # que não é a do ajuste (Resumir) nem a dos pressupostos.
      trama::tr_category("modelo_avaliar",      "Prever e avaliar", role = "avaliacao"),
      trama::tr_category("modelo_testes",       "Testes", role = "avaliacao")
    ),
    nodes = c(.tr_models_nos_fonte(), .tr_models_nos_ajustar(), .tr_models_nos_anova(),
              .tr_models_nos_resumir(), .tr_models_nos_medias(),
              .tr_models_nos_pressupostos(), .tr_models_nos_testes(),
              .tr_models_nos_prever(), .tr_models_nos_avaliar(), .tr_models_nos_online()),
    # Glossário de params (docs/glossario-parametros.md): fluxos salvos com os
    # nomes antigos abrem já migrados. `alfa` vira `confianca` com o valor
    # complementar (alfa 0,05 -> confiança 0,95).
    #
    # Os leitores de classificador da `multi` vieram para cá na Fase 4 (o
    # contrato de modelo): a migração mora AQUI porque o destino é daqui, e as
    # chaves de params/ports são o id NOVO. Os params de `classify`, `confusion`
    # e `roc` têm o mesmo nome e o mesmo default dos de cá; a porta `novos` do
    # classify é a `dados` do predict. A `multi/logistic_coefficients` mostrava
    # razões de chances: o destino injeta `exponenciar = TRUE` só nos nós que
    # vieram de lá (o `models/coefficients` nativo, p.ex. de um `lm`, fica
    # intocado), e o `nivel` dela vira `confianca`.
    #
    # Os leitores da `ml` vieram na mesma fase, com a mesma regra: predict,
    # evaluate, confusion, roc e importance da ml têm as portas com os nomes
    # daqui (`modelo`, `dados`). Os renomes de params que a ml já declarava
    # para esses ids (`alvo` -> `resposta`) passam para cá sob o id novo; as
    # colunas de previsão da ml eram `.pred` e `.prob_<classe>`, e as daqui
    # são `previsto` e `prob_<classe>` — o param que as nomeava é convertido
    # no lugar, com `when` reconhecendo só o formato velho. Como a chave é o
    # id NOVO, a conversão atinge também um nó nativo daqui que tivesse
    # `predito = ".pred"`: improvável (nenhuma saída daqui tem esse nome), e
    # nesse caso a coluna também não existiria. O `tarefa` do antigo
    # `ml/evaluate` não tem par (a tarefa sai do modelo ou dos tipos das
    # colunas) e é descartado.
    migrations = list(
      nodes = c(list("multi/classify" = "models/predict", "multi/confusion" = "models/confusion",
                     "multi/roc" = "models/roc", "multi/logistic_coefficients" = list(to = "models/coefficients",
                                                         params = list(exponenciar = TRUE))),
                .tr_models_migracoes_ml()$nodes),
      ports = list("models/predict" = list(novos = "dados")),
      params = c(.tr_models_migracoes_ml()$params, list(
      "models/coefficients" = list(nivel = list(to = "confianca")),
      "models/emmeans" = list(alfa = list(to = "confianca", value = function(v) 1 - v)),
      "models/duncan" = list(alfa = list(to = "confianca", value = function(v) 1 - v)),
      "models/one_sample_t" = list(coluna = list(to = "variavel")),
      "models/shapiro" = list(coluna = list(to = "variavel")),
      # O enum da lagarta tinha o nível no nome ("IC 90%"); agora é "IC" + a
      # confiança à parte. `when` só pega o formato velho: "IC" puro e os EP
      # ficam como estão, e reabrir um fluxo já migrado não mexe nele.
      "models/plot_caterpillar" = list(intervalo = list(
        to = "intervalo", when = function(v) is.character(v) && grepl("^IC [0-9]+%$", v),
        value = function(v) list(intervalo = "IC", confianca = as.numeric(sub("^IC ([0-9]+)%$", "\\1", v)) / 100)))
    )))
  )
}

#' As migrações dos leitores que vieram da `ml` (Fase 4).
#'
#' Nós: `ml/<x>` -> `models/<x>`. Params, pelo id NOVO: `alvo` -> `resposta`
#' (o glossário que a ml aplicava), `.pred` -> `previsto` e `.prob_<classe>`
#' -> `prob_<classe>` (a classe saneada como nas colunas daqui) convertidos no lugar, e `tarefa` descartado — um `value`
#' que devolve lista nomeada com o próprio nome em NULL não deixa nada no nó.
#' @noRd
.tr_models_migracoes_ml <- function() {
  ids <- c("predict", "evaluate", "confusion", "roc", "importance")
  resposta <- list(alvo = list(to = "resposta"))
  predito <- list(predito = list(to = "predito", when = function(v) identical(v, ".pred"),
                                 value = function(v) "previsto"))
  prob <- list(probabilidade = list(to = "probabilidade",
                                    when = function(v) is.character(v) && length(v) == 1L && grepl("^\\.prob_", v),
                                    value = function(v) paste0("prob_", tr_models_clean_name(sub("^\\.prob_", "", v)))))
  tarefa <- list(tarefa = list(to = "tarefa", when = function(v) TRUE, value = function(v) list(tarefa = NULL)))
  list(nodes = stats::setNames(as.list(paste0("models/", ids)), paste0("ml/", ids)),
       params = list("models/evaluate" = c(resposta, predito, tarefa),
                     "models/confusion" = c(resposta, predito),
                     "models/roc" = c(resposta, prob)))
}
