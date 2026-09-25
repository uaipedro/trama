#' Coleção de aprendizado de máquina supervisionado.
#'
#' Carregue `trama.data`, `trama.view` e `trama.models` antes de `trama.ml`: os
#' ajustes saem no tipo `models/fit`, e prever, avaliar, a matriz de confusão, a
#' ROC e a importância são os blocos da `trama.models`. Motores de modelos são
#' opcionais.
#' @return Uma declaração `tr_collection`.
#' @export
trama_collection <- function() {
  # 0.2.0 veio da main (padrões de ml/roc e ml/cart mudaram); o tipo `ml/fit`
  # não volta: os ajustes saem em `models/fit` (contrato da Fase 4).
  trama::tr_collection("ml", version = "0.2.0", label = "Machine learning",
    transitions = trama::tr_transitions_read(system.file("trama/transicoes.json", package = "trama.ml")),
    categories = list(
      trama::tr_category("ml_dados", "Preparar", role = "preparacao"),
      trama::tr_category("ml_simples", "Interpret\u{E1}veis", role = "ajuste"),
      trama::tr_category("ml_ensembles", "Conjuntos de \u{E1}rvores", role = "ajuste"),
      trama::tr_category("ml_margem", "Vetores de suporte", role = "ajuste"),
      trama::tr_category("ml_avaliar", "Prever e avaliar", role = "avaliacao"),
      trama::tr_category("ml_inspecionar", "Inspecionar", role = "leitura")),
    nodes = c(.tr_ml_workflow_nodes(), .tr_ml_model_nodes(), .tr_ml_analysis_nodes()),
    migrations = list(params = .tr_ml_migracoes()))
}

# Glossário de params (docs/glossario-parametros.md): `alvo` virou `resposta`
# em todo nó que o recebia, e `cols` virou `preditores` nos ajustes. Fluxos
# salvos com os nomes antigos abrem com os novos.
#
# `ml/predict`, `ml/evaluate`, `ml/confusion`, `ml/roc` e `ml/importance` foram
# para a `trama.models` na Fase 4 (contrato de modelo), e as migrações deles
# (o nó e os params) moram LÁ, sob o id novo. Aqui fica o `ml/residuals`, que
# lê a tabela da `models/predict`: `predito = ".pred"` gravado à mão vira
# `previsto`, a coluna de lá. `when` torna a conversão idempotente.
.tr_ml_migracoes <- function() {
  resposta <- list(alvo = list(to = "resposta"))
  ajuste <- c(resposta, list(cols = list(to = "preditores")))
  # `nested_cv` nasceu na main com `alvo`/`cols`; documentos gravados lá abrem
  # com os nomes do glossário. (A `ml/pr_curve` foi para a `models` na 9.2, e
  # a migração dela mora lá, com a das outras leitoras de previsão.)
  ajustes <- c("linear", "cart", "figs", "forest", "svm", "xgboost", "tune", "nested_cv")
  predito <- list(predito = list(to = "predito", when = function(v) identical(v, ".pred"),
                                 value = function(v) "previsto"))
  c(stats::setNames(rep(list(ajuste), length(ajustes)), paste0("ml/", ajustes)),
    list("ml/split" = resposta, "ml/residuals" = c(resposta, predito)))
}

.tr_ml_workflow_nodes <- function() {
  resposta <- .tr_ml_target_param()
  list(
    trama::tr_node("ml/example", tr_ml_example, label = "Dados para aprender",
      description = "Iris, iris bin\u{E1}ria ou mtcars para explorar modelos e avalia\u{E7}\u{E3}o.",
      category = "ml_dados", icon = trama::tr_icon("database"), outputs = list(out = "data/table"),
      params = list(nome = trama::tr_param_enum("iris", c("iris", "iris_binaria", "mtcars"), label = "Conjunto")),
      help = .tr_ml_help("Conjuntos do pacote datasets do R. Iris cont\u{E9}m medidas de flores; mtcars cont\u{E9}m caracter\u{ED}sticas de autom\u{F3}veis. S\u{E3}o exemplos did\u{E1}ticos, n\u{E3}o benchmarks conclusivos.",
        "`nome`: iris (tr\u{EA}s classes), iris_binaria (duas esp\u{E9}cies) ou mtcars (regress\u{E3}o).",
        "Tabela `data/table`.", "trama.ml::tr_ml_example('iris_binaria')",
        "[Iris](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/iris.html), [mtcars](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html). Pr\u{F3}ximo: `ml/split`.")),
    trama::tr_node("ml/split", tr_ml_split, label = "Separar treino / teste",
      description = "Divide as linhas de forma reproduz\u{ED}vel, com estratifica\u{E7}\u{E3}o opcional por classe.",
      category = "ml_dados", icon = trama::tr_icon("scissors"), inputs = list(dados = "data/table"),
      outputs = list(treino = "data/table", teste = "data/table"),
      params = list(resposta = resposta, proporcao = trama::tr_param_num(0.75, min = 0.01, max = 0.99, label = "Fra\u{E7}\u{E3}o para treino"),
        estratificar = trama::tr_param_bool(TRUE, label = "Estratificar classes"),
        estrategia = .tr_ml_estrategia_param(), ordem = .tr_ml_ordem_param(), grupo = .tr_ml_grupo_param(),
        seed = .tr_ml_seed_param()),
      pressupostos = .tr_ml_doc("ml/split")$pressupostos, referencias = .tr_ml_doc("ml/split")$referencias,
      help = .tr_ml_help("Reserve o teste antes de ajustar modelos. A sa\u{ED}da treino alimenta os ajustes; teste alimenta apenas Prever. A divis\u{E3}o aleat\u{F3}ria sup\u{F5}e observa\u{E7}\u{F5}es independentes; com tempo use `estrategia = temporal` (teste inteiro depois do treino) e com indiv\u{ED}duos, lotes ou \u{E1}reas repetidos use `estrategia = grupo` (nenhum grupo dos dois lados).",
        "`resposta`: coluna a prever; quando categ\u{F3}rica, permite estratificar. `proporcao`: fra\u{E7}\u{E3}o aproximada do treino (dos grupos, na estrat\u{E9}gia grupo); arredondamento por classe pode mudar a fra\u{E7}\u{E3}o final. `estratificar`: mant\u{E9}m cada classe nos dois conjuntos; classes unit\u{E1}rias s\u{E3}o recusadas; ignorado nas estrat\u{E9}gias temporal e grupo. Para classes num\u{E9}ricas, converta a fator antes. `estrategia`: aleatoria, temporal (treino = linhas at\u{E9} o instante da linha floor(n \u{B7} propor\u{E7}\u{E3}o) na ordem do tempo, empates juntos) ou grupo (sorteia grupos inteiros). `ordem`: coluna de tempo (n\u{FA}mero, data ou data-hora). `grupo`: coluna de grupo. `seed`: reproduz a divis\u{E3}o sem modificar o gerador da sess\u{E3}o.",
        "Duas tabelas, treino e teste, sem duplicar nem perder linhas.",
        "trama.ml::tr_ml_split(trama.ml::tr_ml_example(), resposta = 'Species')",
        "`ml/cart`, `ml/figs`, `models/predict`. Ajuste imputa\u{E7}\u{E3}o, sele\u{E7}\u{E3}o de vari\u{E1}veis e escalas somente no treino.")),
    trama::tr_node("ml/rules", tr_ml_rules, label = "Ler regras das \u{E1}rvores",
      description = "Exp\u{F5}e caminhos e valores das folhas de CART ou das \u{E1}rvores de FIGS.",
      category = "ml_inspecionar", icon = trama::tr_icon("list-tree"), inputs = list(modelo = "models/fit"), outputs = list(out = "data/table"),
      help = .tr_ml_help("CART escolhe uma folha. FIGS soma uma contribui\u{E7}\u{E3}o por \u{E1}rvore. Regras descrevem o modelo ajustado; n\u{E3}o provam causas. Limiares s\u{E3}o mostrados com precis\u{E3}o suficiente para n\u{E3}o deslocar as decis\u{F5}es.",
        "Entrada: `modelo` CART ou FIGS da ml; outro modelo (um `lm` da models, por exemplo) \u{E9} recusado.", "Tabela de regras que pode ser exportada com a cole\u{E7}\u{E3}o data.",
        "m <- trama.ml::tr_ml_cart(trama.ml::tr_ml_example(), resposta = 'Species')\ntrama.ml::tr_ml_rules(m)", "`ml/cart`, `ml/figs`, `models/importance`.")))
}
