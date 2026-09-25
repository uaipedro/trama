# A coleção `experiments`: o registro do tipo, das categorias e dos nós.

#' A coleção `experiments`.
#'
#' Carrega DEPOIS de `trama.data`, `trama.view` e `trama.models`: as portas
#' usam `data/table`, `view/plot` e `models/fit`, e o registro recusa porta com
#' tipo desconhecido.
#'
#' Os ids de categoria têm prefixo (`exp_*`) porque o registro de categorias é
#' GLOBAL: uma categoria `planejar` aqui sobrescreveria em silêncio a de outra
#' coleção que usasse o mesmo id.
#' @export
trama_collection <- function() {
  nos <- c(.tr_experiments_nos_planejar(), .tr_experiments_nos_simular(),
           # Os nós de análise moram em arquivos de outro autor; a checagem
           # deixa a coleção carregar mesmo antes de eles existirem.
           if (exists(".tr_experiments_nos_analisar", mode = "function")) .tr_experiments_nos_analisar())
  trama::tr_collection(
    id = "experiments", version = "0.1.0", label = "Experimentos",
    types = list(experiments_plan_type()),
    adapters = .tr_experiments_adapters(),
    categories = list(
      trama::tr_category("exp_planejar", "Planejar", role = "preparacao"),
      trama::tr_category("exp_analisar", "Analisar", role = "leitura")
    ),
    nodes = nos
  )
}
