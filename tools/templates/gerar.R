# Gera os templates de exemplo de cada coleção.
#
# Cada exemplo é escrito em R (`tr_flow() |> tr_add(...)`, a mesma API dos
# exemplos do site) e gravado em `collections/<pkg>/inst/templates/<slug>.json`.
# Os JSONs nunca são editados à mão: mudou o exemplo, roda este script de novo.
#
# Uso (na raiz do repositório): Rscript tools/templates/gerar.R
#
# Só dados embutidos (`*/example`): template descarta param de caminho, então
# um exemplo que lê arquivo chegaria vazio a quem o cola.

`%||%` <- function(a, b) if (is.null(a)) b else a

suppressMessages({
  pkgload::load_all(".", quiet = TRUE)  # o trama da árvore, não o instalado
  pkgs <- c("trama.data", "trama.view", "trama.models", "trama.ml",
            "trama.multi", "trama.series", "trama.sampling")
  reg <- tr_registry()
  for (p in pkgs) tr_use(p, registry = reg)
})

# Colunas por profundidade topológica (fontes à esquerda). Sem isso, todos
# nasceriam no mesmo ponto.
#
# O passo é o do card de verdade, no modo completo (o padrão ao colar): 240px
# de largura (`.tr-node`, trama.css) e até ~360px de altura — cabeçalho, 132px
# de preview, abas, uns cinco params e as portas. `dx = 360` deixa 120px de
# vão entre colunas; `dy = 440`, uns 80px entre linhas no card mais alto.
#
# Linha por RAMO: o nó herda a linha do primeiro pai (uma cadeia fica reta) e,
# se ela já estiver ocupada na coluna, desce para a próxima livre — dois
# destinos do mesmo pai ficam um embaixo do outro, nunca por cima.
dispor <- function(flow, dx = 360, dy = 440) {
  doc <- flow$doc
  ids <- names(doc$nodes)
  prof <- setNames(rep(0L, length(ids)), ids)
  repeat {
    mudou <- FALSE
    for (e in doc$edges) {
      d <- prof[[e$from$node]] + 1L
      if (d > prof[[e$to$node]]) { prof[[e$to$node]] <- d; mudou <- TRUE }
    }
    if (!mudou) break
  }
  pais <- lapply(setNames(ids, ids), function(id)
    unique(vapply(Filter(function(e) e$to$node == id, doc$edges),
                  function(e) e$from$node, "")))
  linha <- setNames(rep(NA_integer_, length(ids)), ids)
  ocupadas <- list()
  # Por profundidade, e dentro dela na ordem de entrada no fluxo: o pai
  # sempre tem linha quando o filho é disposto.
  for (id in ids[order(prof, seq_along(ids))]) {
    k <- as.character(prof[[id]])
    usadas <- ocupadas[[k]] %||% integer()
    alvo <- if (length(pais[[id]])) min(linha[pais[[id]]]) else 0L
    while (alvo %in% usadas) alvo <- alvo + 1L
    linha[[id]] <- alvo
    ocupadas[[k]] <- c(usadas, alvo)
  }
  for (id in ids) {
    doc$ui$positions[[id]] <- c(prof[[id]] * dx, linha[[id]] * dy)
    # Seed derivada do id: `tr_add()` sorteia uma nova a cada execução, e
    # regerar sem mudar o exemplo não pode sujar o diff.
    doc$nodes[[id]]$seed <- strtoi(substr(digest::digest(id, algo = "md5"), 1, 7), 16L)
  }
  flow$doc <- doc
  flow
}

exemplos <- list(
  list(pkg = "trama.data", nome = "Resumo de uma tabela",
       descricao = "Carros de mtcars com mais de 20 milhas por galão, resumidos coluna a coluna.",
       flow = tr_flow(reg) |>
         tr_add("carros", "data/example", dataset = "mtcars") |>
         tr_add("economicos", "data/filter", expr = "mpg > 20", from = "carros") |>
         tr_add("resumo", "data/summary", from = "economicos")),
  list(pkg = "trama.data", nome = "Média por grupo",
       descricao = "Ozônio médio e número de dias medidos em cada mês de airquality.",
       flow = tr_flow(reg) |>
         tr_add("ar", "data/example", dataset = "airquality") |>
         tr_add("medidos", "data/filter", expr = "!is.na(Ozone)", from = "ar") |>
         tr_add("mensal", "data/group_summarise", by = "Month",
                name = "ozonio_medio, dias",
                expr = "mean(Ozone), dplyr::n()", from = "medidos")),
  list(pkg = "trama.view", nome = "Dispersão com cor por grupo",
       descricao = "Pétalas de iris em um gráfico de dispersão, coloridas pela espécie.",
       flow = tr_flow(reg) |>
         tr_add("flores", "data/example", dataset = "iris") |>
         tr_add("dispersao", "view/points", x = "Petal.Length", y = "Petal.Width",
                cor = "Species", from = "flores")),
  list(pkg = "trama.view", nome = "Distribuição por grupo",
       descricao = "Comprimento da sépala de iris: histograma e boxplot por espécie.",
       flow = tr_flow(reg) |>
         tr_add("flores", "data/example", dataset = "iris") |>
         tr_add("histograma", "view/histogram", x = "Sepal.Length", cor = "Species", from = "flores") |>
         tr_add("caixas", "view/boxplot", y = "Sepal.Length", x = "Species", from = "flores")),
  list(pkg = "trama.models", nome = "Regressão linear com diagnóstico",
       descricao = "Consumo (mpg) explicado por peso e potência em mtcars, com coeficientes e gráficos de resíduos.",
       flow = tr_flow(reg) |>
         tr_add("dados", "models/example", dataset = "mtcars") |>
         tr_add("ajuste", "models/lm", formula = "mpg ~ wt + hp", from = "dados") |>
         tr_add("coeficientes", "models/coefficients", from = "ajuste") |>
         tr_add("diagnostico", "models/plot_diagnostics", from = "ajuste")),
  list(pkg = "trama.models", nome = "ANOVA em blocos com comparação de médias",
       descricao = "Produção de híbridos de milho em blocos casualizados: ANOVA, médias ajustadas e comparações par a par.",
       flow = tr_flow(reg) |>
         tr_add("dados", "models/example", dataset = "milho_dbc") |>
         tr_add("ajuste", "models/anova_dbc", resposta = "producao",
                tratamento = "hibrido", bloco = "bloco", from = "dados") |>
         tr_add("medias", "models/emmeans", especs = "hibrido", from = "ajuste") |>
         tr_add("pares", "models/pairwise", from = "medias")),
  list(pkg = "trama.ml", nome = "Classificação com floresta aleatória",
       descricao = "Espécie de iris prevista por floresta aleatória: separa treino e teste, avalia e mostra a matriz de confusão.",
       flow = tr_flow(reg) |>
         tr_add("dados", "ml/example", nome = "iris") |>
         tr_add("separar", "ml/split", alvo = "Species", from = "dados") |>
         tr_set("separar", seed = 42L) |>
         tr_add("floresta", "ml/forest", alvo = "Species", trees = 200L, from = "separar:treino") |>
         tr_add("prever", "ml/predict", from = c("floresta", "separar:teste")) |>
         tr_add("avaliar", "ml/evaluate", alvo = "Species", from = "prever") |>
         tr_add("confusao", "ml/confusion", alvo = "Species", from = "prever")),
  list(pkg = "trama.ml", nome = "Árvore de decisão",
       descricao = "Árvore de classificação para iris binária, com o desenho da árvore e as regras.",
       flow = tr_flow(reg) |>
         tr_add("dados", "ml/example", nome = "iris_binaria") |>
         tr_add("arvore", "ml/cart", alvo = "Species", cols = "Petal.Length, Petal.Width", from = "dados") |>
         tr_add("desenho", "ml/tree_plot", from = "arvore") |>
         tr_add("regras", "ml/rules", from = "arvore")),
  list(pkg = "trama.multi", nome = "PCA com biplot",
       descricao = "Componentes principais de USArrests padronizado: variância explicada e biplot.",
       flow = tr_flow(reg) |>
         tr_add("dados", "multi/example", dataset = "USArrests") |>
         tr_add("pca", "multi/pca", cols = "Murder, Assault, UrbanPop, Rape",
                padronizar = TRUE, from = "dados") |>
         tr_add("variancia", "multi/pca_variance", from = "pca") |>
         tr_add("mapa", "multi/biplot", from = "pca")),
  list(pkg = "trama.series", nome = "Decomposição de série",
       descricao = "AirPassengers em log decomposto por STL, com o gráfico dos componentes.",
       flow = tr_flow(reg) |>
         tr_add("pax", "series/example", dataset = "AirPassengers") |>
         tr_add("log", "series/transform", metodo = "log", from = "pax") |>
         tr_add("decomp", "series/stl", from = "log") |>
         tr_add("grafico", "series/plot_decomposition", from = "decomp")),
  list(pkg = "trama.series", nome = "Previsão com suavização exponencial",
       descricao = "Modelo ETS para AirPassengers, com previsão de 24 meses e gráfico.",
       flow = tr_flow(reg) |>
         tr_add("pax", "series/example", dataset = "AirPassengers") |>
         tr_add("modelo", "series/ets", from = "pax") |>
         tr_add("previsao", "series/forecast", horizonte = 24L, from = "modelo") |>
         tr_add("grafico", "series/plot_forecast", from = "previsao")),
  list(pkg = "trama.sampling", nome = "Amostra estratificada",
       descricao = "Amostra de 240 fazendas estratificada por região e média de produção por estrato.",
       flow = tr_flow(reg) |>
         tr_add("pop", "sampling/example", dataset = "fazendas") |>
         tr_add("amostra", "sampling/stratified", estrato = "regiao", n = 240L, from = "pop") |>
         tr_add("media", "sampling/mean", variavel = "producao_t", por = "regiao", from = "amostra") |>
         tr_add("grafico", "sampling/plot_estimates", from = "media"))
)

raiz <- if (dir.exists("collections")) "." else stop("Rode na raiz do repositório.")
for (ex in exemplos) {
  tpl <- tr_template(dispor(ex$flow)$doc, nome = ex$nome, descricao = ex$descricao, registry = reg)
  path <- tr_template_save(tpl, file.path(raiz, "collections", ex$pkg, "inst", "templates"),
                           overwrite = TRUE)
  message("gravado: ", path)
}
