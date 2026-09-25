# A previsão saiu da ml (Fase 4): os testes preveem pela `models/predict`, que
# é o que os fluxos usam. Colunas `previsto` e `prob_<classe>`.
prever <- function(m, d) trama.models::tr_models_predict(m, d)

# A ml exige a models carregada: os ajustes saem em `models/fit`.
ml_registry <- function() {
  r <- trama::tr_registry()
  trama::tr_use("trama.data", registry = r)
  trama::tr_use("trama.view", registry = r)
  trama::tr_use("trama.models", registry = r)
  trama::tr_use(trama_collection(), registry = r)
  r
}
