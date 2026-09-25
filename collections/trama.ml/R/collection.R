#' Coleção de aprendizado de máquina supervisionado.
#'
#' Carregue `trama.data` antes de `trama.ml`. Motores de modelos são opcionais.
#' @return Uma declaração `tr_collection`.
#' @export
trama_collection <- function() {
  trama::tr_collection("ml", version = "0.1.0", label = "Machine learning",
    transitions = trama::tr_transitions_read(system.file("trama/transicoes.json", package = "trama.ml")),
    types = list(.tr_ml_model_type()),
    categories = list(
      trama::tr_category("ml_dados", "Preparar", role = "preparacao"),
      trama::tr_category("ml_simples", "Interpret\u{E1}veis", role = "ajuste"),
      trama::tr_category("ml_ensembles", "Conjuntos de \u{E1}rvores", role = "ajuste"),
      trama::tr_category("ml_margem", "Vetores de suporte", role = "ajuste"),
      trama::tr_category("ml_avaliar", "Prever e avaliar", role = "avaliacao"),
      trama::tr_category("ml_inspecionar", "Inspecionar", role = "leitura")),
    nodes = c(.tr_ml_workflow_nodes(), .tr_ml_model_nodes(), .tr_ml_analysis_nodes()))
}

.tr_ml_workflow_nodes <- function() {
  alvo <- .tr_ml_target_param()
  predito <- trama::tr_param("text", ".pred", label = "Coluna prevista", example = ".pred")
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
      params = list(alvo = alvo, proporcao = trama::tr_param_num(0.75, min = 0.01, max = 0.99, label = "Fra\u{E7}\u{E3}o para treino"),
        estratificar = trama::tr_param_bool(TRUE, label = "Estratificar classes"), seed = .tr_ml_seed_param()),
      help = .tr_ml_help("Reserve o teste antes de ajustar modelos. A sa\u{ED}da treino alimenta os ajustes; teste alimenta apenas Prever. A divis\u{E3}o aleat\u{F3}ria sup\u{F5}e observa\u{E7}\u{F5}es independentes: para tempo, indiv\u{ED}duos repetidos ou grupos, separe por tempo/grupo fora deste bloco.",
        "`alvo`: resposta; quando categ\u{F3}rica, permite estratificar. `proporcao`: fra\u{E7}\u{E3}o aproximada do treino; arredondamento por classe pode mudar a fra\u{E7}\u{E3}o final. `estratificar`: mant\u{E9}m cada classe nos dois conjuntos; classes unit\u{E1}rias s\u{E3}o recusadas. Para classes num\u{E9}ricas, converta a fator antes. `seed`: reproduz a divis\u{E3}o sem modificar o gerador da sess\u{E3}o.",
        "Duas tabelas, treino e teste, sem duplicar nem perder linhas.",
        "trama.ml::tr_ml_split(trama.ml::tr_ml_example(), alvo = 'Species')",
        "`ml/cart`, `ml/figs`, `ml/predict`. Ajuste imputa\u{E7}\u{E3}o, sele\u{E7}\u{E3}o de vari\u{E1}veis e escalas somente no treino.")),
    trama::tr_node("ml/predict", role = "leitura", tr_ml_predict, label = "Prever",
      description = "Aplica o modelo a novas linhas, preservando a ordem e as colunas.",
      category = "ml_avaliar", icon = trama::tr_icon("target"), inputs = list(modelo = "ml/fit", dados = "data/table"), outputs = list(out = "data/table"),
      help = .tr_ml_help("Use dados de teste ou dados novos com os mesmos preditores do treino. A resposta n\u{E3}o \u{E9} necess\u{E1}ria para prever. Colunas de sa\u{ED}da existentes s\u{E3}o recusadas para n\u{E3}o sobrescrever dados.",
        "Entradas: `modelo` ajustado e `dados` a prever; sem par\u{E2}metros adicionais.",
        "Tabela original com `.pred` e, quando dispon\u{ED}veis, probabilidades `.prob_<classe>`.",
        "d <- trama.ml::tr_ml_split(trama.ml::tr_ml_example(), alvo = 'Species')\nm <- trama.ml::tr_ml_cart(d$treino, alvo = 'Species')\ntrama.ml::tr_ml_predict(m, d$teste)", "`ml/evaluate`, `ml/confusion`.")),
    trama::tr_node("ml/evaluate", tr_ml_evaluate,
      pressupostos = .tr_ml_doc("ml/evaluate")$pressupostos, referencias = .tr_ml_doc("ml/evaluate")$referencias, label = "Avaliar previs\u{F5}es",
      description = "Calcula erros de regress\u{E3}o ou m\u{E9}tricas de classifica\u{E7}\u{E3}o nas linhas fornecidas.",
      category = "ml_avaliar", icon = trama::tr_icon("gauge"), inputs = list(dados = "data/table"), outputs = list(out = "data/table"),
      params = list(alvo = alvo, predito = predito, tarefa = .tr_ml_task_param()),
      help = .tr_ml_help("Mede a compara\u{E7}\u{E3}o entre resposta observada e previs\u{E3}o.",
        "`alvo`: resposta observada. `predito`: coluna prevista. `tarefa`: auto, regressao ou classificacao.",
        "Tabela de m\u{E9}tricas: RMSE, MAE e R\u{B2} para regress\u{E3}o; acur\u{E1}cia, acur\u{E1}cia balanceada e macro F1 para classifica\u{E7}\u{E3}o. R\u{B2} \u{E9} indefinido para resposta constante. M\u{E9}dias macro usam classes observadas; consulte a matriz de confus\u{E3}o.",
        "d <- data.frame(y = c(1, 2, 3), .pred = c(1, 2, 4))\ntrama.ml::tr_ml_evaluate(d, alvo = 'y')", "`ml/predict`, `ml/confusion`.")),
    trama::tr_node("ml/confusion", tr_ml_confusion,
      pressupostos = .tr_ml_doc("ml/confusion")$pressupostos, referencias = .tr_ml_doc("ml/confusion")$referencias, label = "Matriz de confus\u{E3}o",
      description = "Conta acertos e confus\u{F5}es entre classes observadas e previstas.",
      category = "ml_avaliar", icon = trama::tr_icon("grid-3x3"), inputs = list(dados = "data/table"), outputs = list(out = "data/table"),
      params = list(alvo = alvo, predito = predito),
      help = .tr_ml_help("Mostra todas as combina\u{E7}\u{F5}es de classes, inclusive contagens zero.",
        "`alvo`: classe observada. `predito`: classe prevista.", "Tabela observado, previsto e n.",
        "d <- data.frame(y = c('a', 'a', 'b'), .pred = c('a', 'b', 'b'))\ntrama.ml::tr_ml_confusion(d, alvo = 'y')", "`ml/evaluate`, `ml/predict`.")),
    trama::tr_node("ml/rules", tr_ml_rules, label = "Ler regras das \u{E1}rvores",
      description = "Exp\u{F5}e caminhos e valores das folhas de CART ou das \u{E1}rvores de FIGS.",
      category = "ml_inspecionar", icon = trama::tr_icon("list-tree"), inputs = list(modelo = "ml/fit"), outputs = list(out = "data/table"),
      help = .tr_ml_help("CART escolhe uma folha. FIGS soma uma contribui\u{E7}\u{E3}o por \u{E1}rvore. Regras descrevem o modelo ajustado; n\u{E3}o provam causas. Limiares s\u{E3}o mostrados com precis\u{E3}o suficiente para n\u{E3}o deslocar as decis\u{F5}es.",
        "Entrada: `modelo` CART ou FIGS.", "Tabela de regras que pode ser exportada com a cole\u{E7}\u{E3}o data.",
        "m <- trama.ml::tr_ml_cart(trama.ml::tr_ml_example(), alvo = 'Species')\ntrama.ml::tr_ml_rules(m)", "`ml/cart`, `ml/figs`.")),
    trama::tr_node("ml/importance", tr_ml_importance, label = "Import\u{E2}ncia de vari\u{E1}veis",
      description = "Resume a import\u{E2}ncia interna nos modelos de \u{E1}rvore.",
      category = "ml_inspecionar", icon = trama::tr_icon("chart-bar-decreasing"), inputs = list(modelo = "ml/fit"), outputs = list(out = "data/table"),
      help = .tr_ml_help("Import\u{E2}ncias dependem do motor: redu\u{E7}\u{E3}o de impureza, erro ou ganho conforme o m\u{E9}todo. N\u{E3}o compare suas magnitudes entre motores. Preditores correlacionados podem repartir ou deslocar import\u{E2}ncia; import\u{E2}ncia n\u{E3}o indica sinal do efeito nem causalidade.",
        "Entrada: `modelo` CART, FIGS, random forest ou XGBoost. SVM e refer\u{EA}ncia linear n\u{E3}o t\u{EA}m esta import\u{E2}ncia de \u{E1}rvore.",
        "Tabela variavel/importancia. Valores descrevem o ajuste, n\u{E3}o garantem relev\u{E2}ncia fora da amostra.",
        "m <- trama.ml::tr_ml_cart(trama.ml::tr_ml_example(), alvo = 'Species')\ntrama.ml::tr_ml_importance(m)", "`ml/rules`, `ml/evaluate`.")))
}
