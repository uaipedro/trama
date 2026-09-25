# Wrappers públicos mantêm cada card restrito aos controles do seu método.

#' Árvore CART para regressão ou classificação.
#' @inheritParams tr_ml_fit
#' @return Um modelo `tr_ml_fit`.
#' @export
tr_ml_cart <- function(dados, alvo = "", cols = "", tarefa = "auto",
                       max_depth = 3L, min_n = 5L, seed = 42L) {
  tr_ml_fit(dados, alvo, cols, "cart", tarefa, seed = seed,
            max_depth = max_depth, min_n = min_n)
}

#' Soma de árvores interpretáveis FIGS.
#' @inheritParams tr_ml_fit
#' @return Um modelo `tr_ml_fit`.
#' @export
tr_ml_figs <- function(dados, alvo = "", cols = "", tarefa = "auto",
                       max_splits = 6L, min_n = 5L, seed = 42L) {
  tr_ml_fit(dados, alvo, cols, "figs", tarefa, seed = seed,
            max_splits = max_splits, min_n = min_n)
}

#' Floresta aleatória com ranger.
#' @inheritParams tr_ml_fit
#' @return Um modelo `tr_ml_fit`.
#' @export
tr_ml_forest <- function(dados, alvo = "", cols = "", tarefa = "auto",
                         trees = 200L, mtry = 0L, min_n = 5L, max_depth = 3L, seed = 42L) {
  tr_ml_fit(dados, alvo, cols, "forest", tarefa, seed = seed,
            trees = trees, mtry = mtry, min_n = min_n, max_depth = max_depth)
}

#' Máquina de vetores de suporte com e1071.
#' @inheritParams tr_ml_fit
#' @return Um modelo `tr_ml_fit`.
#' @export
tr_ml_svm <- function(dados, alvo = "", cols = "", tarefa = "auto",
                      cost = 1, gamma = 0.1, kernel = "radial", seed = 42L) {
  tr_ml_fit(dados, alvo, cols, "svm", tarefa, seed = seed,
            cost = cost, gamma = gamma, kernel = kernel)
}

#' Árvores impulsionadas por gradiente com XGBoost.
#' @inheritParams tr_ml_fit
#' @return Um modelo `tr_ml_fit`.
#' @export
tr_ml_xgboost <- function(dados, alvo = "", cols = "", tarefa = "auto",
                          nrounds = 100L, max_depth = 3L, eta = 0.1, seed = 42L) {
  tr_ml_fit(dados, alvo, cols, "xgboost", tarefa, seed = seed,
            nrounds = nrounds, max_depth = max_depth, eta = eta)
}

#' Referência linear ou logística binária.
#' @inheritParams tr_ml_fit
#' @return Um modelo `tr_ml_fit`.
#' @export
tr_ml_linear <- function(dados, alvo = "", cols = "", tarefa = "auto", seed = 42L) {
  tr_ml_fit(dados, alvo, cols, "linear", tarefa, seed = seed)
}

.tr_ml_help <- function(descricao, parametros, valor, exemplo, veja) {
  paste0("## Descri\u{E7}\u{E3}o\n\n", descricao, "\n\n## Par\u{E2}metros\n\n", parametros,
    "\n\n## Valor\n\n", valor, "\n\n## Exemplos\n\n```r\n", exemplo,
    "\n```\n\n## Veja tamb\u{E9}m\n\n", veja)
}

.tr_ml_target_param <- function() trama::tr_param("text", "", label = "Resposta", example = "Species")
.tr_ml_cols_param <- function() trama::tr_param("cols", "", label = "Preditores", example = "Sepal.Length, Petal.Length")
.tr_ml_task_param <- function() trama::tr_param_enum("auto", c("auto", "regressao", "classificacao"), label = "Tarefa")
.tr_ml_seed_param <- function() trama::tr_param_int(42L, min = 0L, label = "Semente")

.tr_ml_model_nodes <- function() {
  common <- list(alvo = .tr_ml_target_param(), cols = .tr_ml_cols_param(), tarefa = .tr_ml_task_param())
  depth <- trama::tr_param_int(3L, min = 1L, max = 30L, label = "Profundidade m\u{E1}xima")
  min_n <- trama::tr_param_int(5L, min = 1L, label = "M\u{ED}nimo por n\u{F3}")
  configs <- list(
    linear = list(fn = tr_ml_linear, icon = "chart-spline", label = "Linear / log\u{ED}stica", category = "ml_simples",
      desc = "Refer\u{EA}ncia simples: regress\u{E3}o linear ou log\u{ED}stica bin\u{E1}ria.",
      details = "Ajusta m\u{ED}nimos quadrados (`stats::lm`) ou log\u{ED}stica bin\u{E1}ria (`stats::glm`). N\u{E3}o \u{E9} rede neural. Compare com os modelos de \u{E1}rvore usando o mesmo teste. A log\u{ED}stica suporta duas classes.",
      params = list(), extra = ""),
    cart = list(fn = tr_ml_cart, icon = "git-fork", label = "CART \u{B7} \u{E1}rvore de decis\u{E3}o", category = "ml_simples",
      desc = "Uma \u{E1}rvore pequena para seguir cada decis\u{E3}o at\u{E9} a previs\u{E3}o.",
      details = "Usa `rpart`. Cada caminho da raiz at\u{E9} uma folha forma uma regra. Profundidade pequena facilita a leitura; \u{E1}rvores grandes podem sobreajustar. A tabela do card mostra regras e as previs\u{F5}es das folhas.",
      params = list(max_depth = depth, min_n = min_n),
      extra = "`max_depth`: profundidade m\u{E1}xima. `min_n`: m\u{ED}nimo de observa\u{E7}\u{F5}es por folha."),
    figs = list(fn = tr_ml_figs, icon = "git-branch-plus", label = "FIGS \u{B7} soma de \u{E1}rvores", category = "ml_simples",
      desc = "Soma poucas \u{E1}rvores pequenas com um or\u{E7}amento total de divis\u{F5}es.",
      details = "Usa `figsr`, de Jo\u{E3}o Paulo Assis Bonif\u{E1}cio, Geraldo Magela da Cruz Pereira, Pedro Mambelli Fernandes e Jo\u{E3}o Vitor Andrade Alves de Souza. FIGS escolhe entre crescer uma \u{E1}rvore e iniciar outra. Suporta regress\u{E3}o e classifica\u{E7}\u{E3}o **bin\u{E1}ria**; some as contribui\u{E7}\u{F5}es das \u{E1}rvores, n\u{E3}o fa\u{E7}a vota\u{E7}\u{E3}o. O limite de divis\u{F5}es controla a complexidade global.",
      params = list(max_splits = trama::tr_param_int(6L, min = 1L, label = "Total m\u{E1}ximo de divis\u{F5}es"), min_n = min_n),
      extra = "`max_splits`: or\u{E7}amento total de divis\u{F5}es da soma. `min_n`: tamanho m\u{ED}nimo dos n\u{F3}s conforme figsr."),
    forest = list(fn = tr_ml_forest, icon = "trees", label = "Random forest", category = "ml_ensembles",
      desc = "Combina \u{E1}rvores aleatorizadas para regress\u{E3}o e classifica\u{E7}\u{E3}o.",
      details = "Usa `ranger`. \u{C1}rvores treinadas com bootstrap e subconjuntos de preditores s\u{E3}o agregadas. Import\u{E2}ncia ajuda a resumir a floresta, mas n\u{E3}o equivale a uma regra individual nem indica causalidade.",
      params = list(trees = trama::tr_param_int(200L, min = 1L, label = "\u{C1}rvores"),
        mtry = trama::tr_param_int(0L, min = 0L, label = "Vari\u{E1}veis por divis\u{E3}o (0 = autom\u{E1}tico)"), min_n = min_n, max_depth = depth),
      extra = "`trees`: n\u{FA}mero de \u{E1}rvores. `mtry`: preditores candidatos por divis\u{E3}o; zero usa a raiz quadrada do n\u{FA}mero de preditores, arredondada para baixo. `min_n`: tamanho m\u{ED}nimo do n\u{F3} a dividir conforme ranger, n\u{E3}o tamanho m\u{ED}nimo das folhas. `max_depth`: profundidade m\u{E1}xima de cada \u{E1}rvore."),
    svm = list(fn = tr_ml_svm, icon = "move-diagonal", label = "SVM \u{B7} vetores de suporte", category = "ml_margem",
      desc = "Ajusta uma margem linear ou n\u{E3}o linear, com escala aprendida no treino.",
      details = "Usa `e1071`/LIBSVM. A padroniza\u{E7}\u{E3}o \u{E9} estimada somente no treino e reaplicada na previs\u{E3}o. Kernels n\u{E3}o lineares tornam a regra menos transparente.",
      params = list(cost = trama::tr_param_num(1, min = 0.000001, label = "Custo"),
        gamma = trama::tr_param_num(0.1, min = 0.000001, label = "Gamma"),
        kernel = trama::tr_param_enum("radial", c("linear", "radial", "polynomial", "sigmoid"), label = "Kernel")),
      extra = "`cost`: penalidade dos erros. `gamma`: escala do kernel; n\u{E3}o \u{E9} usado no kernel linear. `kernel`: geometria da fronteira."),
    xgboost = list(fn = tr_ml_xgboost, icon = "rocket", label = "XGBoost", category = "ml_ensembles",
      desc = "Acrescenta \u{E1}rvores sequencialmente para corrigir os erros do conjunto.",
      details = "Usa `xgboost::xgb.train`. Suporta regress\u{E3}o, classifica\u{E7}\u{E3}o bin\u{E1}ria e multiclasse. Mais rodadas e profundidade podem sobreajustar. A import\u{E2}ncia por ganho \u{E9} um resumo do ajuste, n\u{E3}o uma explica\u{E7}\u{E3}o causal. N\u{E3}o h\u{E1} sele\u{E7}\u{E3}o autom\u{E1}tica de rodadas nem uso do teste para parada antecipada.",
      params = list(nrounds = trama::tr_param_int(100L, min = 1L, label = "Rodadas"), max_depth = depth,
        eta = trama::tr_param_num(0.1, min = 0.000001, max = 1, label = "Taxa de aprendizado")),
      extra = "`nrounds`: rodadas de boosting. `max_depth`: profundidade por \u{E1}rvore. `eta`: taxa de aprendizado.")
  )
  lapply(names(configs), function(id) {
    cfg <- configs[[id]]
    doc <- .tr_ml_doc(paste0("ml/", id))
    trama::tr_node(paste0("ml/", id), fn = cfg$fn, label = cfg$label,
      pressupostos = doc$pressupostos, referencias = doc$referencias,
      description = cfg$desc, category = cfg$category, icon = trama::tr_icon(cfg$icon),
      inputs = list(dados = "data/table"), outputs = list(out = "ml/fit"),
      params = c(common, cfg$params, list(seed = .tr_ml_seed_param())),
      help = .tr_ml_help(cfg$details,
        paste("`alvo`: coluna resposta. `cols`: preditores num\u{E9}ricos separados por v\u{ED}rgula; vazio usa os num\u{E9}ricos exceto a resposta. `tarefa`: auto interpreta n\u{FA}meros como regress\u{E3}o e fator/texto como classifica\u{E7}\u{E3}o. Para classes codificadas com n\u{FA}meros, selecione classificacao. `seed`: semente reproduz\u{ED}vel, sem alterar a sess\u{E3}o.", cfg$extra),
        "Modelo `ml/fit`. Ligue-o a `ml/predict`; conecte a tabela de teste na outra entrada. Use `ml/evaluate` para medir desempenho fora do treino.",
        paste0("trama.ml::tr_ml_", id, "(trama.ml::tr_ml_example('iris_binaria'), alvo = 'Species')"),
        "`ml/split`, `ml/tune`, `ml/predict` e `ml/evaluate`."))
  })
}

.tr_ml_analysis_nodes <- function() {
  T <- "data/table"; G <- "view/plot"; M <- "ml/fit"
  visual <- function(...) trama.view::tr_view_props(...)
  list(
    trama::tr_node("ml/tune", role = "ajuste", tr_ml_tune, label = "Ajustar hiperpar\u{E2}metros",
      description = "Seleciona hiperpar\u{E2}metros por valida\u{E7}\u{E3}o cruzada e reajusta o vencedor no treino completo.",
      category = "ml_avaliar", icon = trama::tr_icon("sliders-horizontal"), inputs = list(dados = T),
      outputs = list(modelo = M, historico = T),
      params = list(alvo = .tr_ml_target_param(), cols = .tr_ml_cols_param(),
        modelo = trama::tr_param_enum("cart", c("cart", "figs", "forest", "svm", "xgboost"), label = "Modelo"),
        tarefa = .tr_ml_task_param(),
        metrica = trama::tr_param_enum("auto", c("auto", "mae", "rmse", "r2", "accuracy", "balanced_accuracy", "macro_f1"), label = "M\u{E9}trica"),
        tentativas = trama::tr_param_int(20L, min = 1L, label = "Tentativas"),
        folds = trama::tr_param_int(5L, min = 2L, label = "Folds"),
        amplitude = trama::tr_param_enum("conservadora", c("conservadora", "ampla"), label = "Espa\u{E7}o de busca"),
        seed = .tr_ml_seed_param()),
      help = .tr_ml_help("Avalia configura\u{E7}\u{F5}es nos mesmos folds, escolhe pela m\u{E9}dia e reajusta o vencedor em todas as linhas recebidas. Conecte somente treino; preserve o teste para a avalia\u{E7}\u{E3}o final.",
        "`modelo`: fam\u{ED}lia a ajustar. `metrica`: auto usa RMSE em regress\u{E3}o e macro F1 em classifica\u{E7}\u{E3}o. `tentativas`: or\u{E7}amento da busca aleat\u{F3}ria. `folds`: parti\u{E7}\u{F5}es internas. `amplitude`: limites conservadores ou amplos. `seed`: reproduz folds, configura\u{E7}\u{F5}es e ajustes.",
        "Duas sa\u{ED}das: o melhor `ml/fit` reajustado e uma tabela com todas as tentativas.",
        "d <- trama.ml::tr_ml_example('iris_binaria')\ntrama.ml::tr_ml_tune(d, alvo = 'Species', tentativas = 3, folds = 3)",
        "`ml/tuning_plot`, `ml/predict`, `ml/evaluate`.")),
    trama::tr_node("ml/tree_plot", tr_ml_tree_plot, label = "Visualizar \u{E1}rvores",
      description = "Desenha a \u{E1}rvore CART ou uma \u{E1}rvore da soma FIGS.",
      category = "ml_inspecionar", icon = trama::tr_icon("network"), inputs = list(modelo = M), outputs = list(out = G),
      params = visual(arvore = trama::tr_param_int(1L, min = 1L, label = "\u{C1}rvore FIGS"),
        mostrar_n = trama::tr_param_bool(TRUE, label = "Mostrar amostras"),
        mostrar_impureza = trama::tr_param_bool(FALSE, label = "Mostrar impureza / ganho"),
        casas = trama::tr_param_int(3L, min = 0L, max = 6L, label = "Casas decimais")),
      help = .tr_ml_help("No CART, mostra a \u{E1}rvore de decis\u{E3}o completa. No FIGS, mostra uma \u{E1}rvore por vez; a previs\u{E3}o final continua sendo a soma das contribui\u{E7}\u{F5}es.",
        "`arvore`: \u{ED}ndice da \u{E1}rvore no FIGS; \u{E9} ignorado pelo CART. `mostrar_n`: inclui o n\u{FA}mero de observa\u{E7}\u{F5}es. `mostrar_impureza`: inclui impureza no CART ou ganho no FIGS. `casas`: precis\u{E3}o dos valores.", "Um gr\u{E1}fico `view/plot`.",
        "m <- trama.ml::tr_ml_cart(mtcars, alvo = 'mpg', cols = 'wt, hp')\ntrama.ml::tr_ml_tree_plot(m)",
        paste("`ml/rules`, `ml/cart`, `ml/figs`.", trama.view::tr_view_help_appearance()))),
    trama::tr_node("ml/tuning_plot", tr_ml_tuning_plot, label = "Visualizar tuning",
      description = "Mostra cada tentativa e a evolu\u{E7}\u{E3}o do melhor resultado.",
      category = "ml_inspecionar", icon = trama::tr_icon("chart-no-axes-combined"), inputs = list(dados = T), outputs = list(out = G),
      params = visual(hiperparametro = trama::tr_param("text", "", label = "Hiperpar\u{E2}metro", example = "max_depth")),
      help = .tr_ml_help("Leia a dispers\u{E3}o das tentativas e se o melhor valor ainda melhora perto do fim do or\u{E7}amento.",
        "Entrada: sa\u{ED}da hist\u{F3}rico de Ajustar hiperpar\u{E2}metros. `hiperparametro`: coluna num\u{E9}rica a relacionar com a m\u{E9}trica; vazio mostra a evolu\u{E7}\u{E3}o da busca.", "Um gr\u{E1}fico `view/plot`.",
        "z <- trama.ml::tr_ml_tune(mtcars, 'mpg', tentativas = 3, folds = 3)\ntrama.ml::tr_ml_tuning_plot(z$historico)",
        paste("`ml/tune`.", trama.view::tr_view_help_appearance()))),
    trama::tr_node("ml/residuals", tr_ml_residuals, label = "Analisar res\u{ED}duos",
      description = "Compara res\u{ED}duos de regress\u{E3}o com os valores previstos.",
      category = "ml_inspecionar", icon = trama::tr_icon("chart-scatter"), inputs = list(dados = T), outputs = list(out = G),
      params = visual(alvo = .tr_ml_target_param(),
        predito = trama::tr_param("text", ".pred", label = "Coluna prevista", example = ".pred")),
      help = .tr_ml_help("Padr\u{F5}es, curvas ou abertura dos res\u{ED}duos sugerem erros sistem\u{E1}ticos ou vari\u{E2}ncia desigual. \u{C9} um diagn\u{F3}stico, n\u{E3}o uma prova isolada.",
        "`alvo`: resposta observada. `predito`: previs\u{E3}o num\u{E9}rica.", "Um gr\u{E1}fico `view/plot`.",
        "d <- data.frame(y = 1:4, .pred = c(1.1, 1.8, 3.2, 3.7))\ntrama.ml::tr_ml_residuals(d, 'y')",
        paste("`ml/predict`, `ml/evaluate`.", trama.view::tr_view_help_appearance()))),
    trama::tr_node("ml/roc", role = "avaliacao", tr_ml_roc, label = "Curva ROC",
      description = "Mostra sensibilidade contra falsos positivos em classifica\u{E7}\u{E3}o bin\u{E1}ria.",
      category = "ml_inspecionar", icon = trama::tr_icon("chart-line"), inputs = list(dados = T), outputs = list(out = G),
      params = visual(alvo = .tr_ml_target_param(),
        probabilidade = trama::tr_param("text", "", label = "Probabilidade", example = ".prob_sim"),
        positiva = trama::tr_param("text", "", label = "Classe positiva", example = "sim")),
      help = .tr_ml_help("Ordena as linhas pela probabilidade da classe positiva e exibe a curva ROC com sua AUC. Use somente classifica\u{E7}\u{E3}o bin\u{E1}ria.",
        "`alvo`: classe observada. `probabilidade`: coluna `.prob_<classe>` criada por Prever. `positiva`: classe correspondente; vazio usa a segunda classe observada.",
        "Um gr\u{E1}fico `view/plot`.",
        "d <- data.frame(y = factor(c('nao','sim','nao','sim')), .prob_sim = c(.1,.8,.4,.7))\ntrama.ml::tr_ml_roc(d, 'y', '.prob_sim', 'sim')",
        paste("`ml/predict`, `ml/confusion`.", trama.view::tr_view_help_appearance())))
  )
}
