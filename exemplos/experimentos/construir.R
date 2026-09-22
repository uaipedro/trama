# Gera `flows/main.json` do projeto de experimentos.
#
# O fluxo é escrito pela DSL, e não montado à mão no editor, para que refazê-lo
# depois de uma mudança na coleção seja rodar este arquivo — e para que a
# leitura do fluxo em texto sirva de roteiro da aula. Rodar de novo SOBRESCREVE
# o que foi arrumado no editor.
#
#   Rscript exemplos/experimentos/construir.R
setwd(local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a[[1]]))) else normalizePath(getwd())
}))
suppressMessages({
  pkgload::load_all("../..", quiet = TRUE, attach = TRUE)
  pkgload::load_all("../../collections/trama.data", quiet = TRUE, attach = FALSE)
  pkgload::load_all("../../collections/trama.view", quiet = TRUE, attach = FALSE)
  pkgload::load_all("../../collections/trama.models", quiet = TRUE, attach = FALSE)
})

pr <- tr_project(".", collections = c("trama.data", "trama.view", "trama.models"))

# A grade: cada experimento é uma LINHA de frame, e dentro dela os cards vão
# em colunas pela etapa da análise (dados → ajuste → pressupostos → quadro →
# médias). `em(linha, coluna, sub)` dá a posição; `sub` empilha dois cards na
# mesma coluna.
COL <- 330; LIN <- 1120; SUB <- 330; X0 <- 40; Y0 <- 90
# A coluna 3 é a do QUADRO da ANOVA, que abre com a tabela inteira (FV, GL, SQ,
# QM, Fc, Pr > F) e precisa de largura: as colunas depois dela andam `LARGO`.
LARGO <- 250; QW <- 540; QH <- 300
em <- function(linha, coluna, sub = 0) c(X0 + coluna * COL + (coluna >= 4) * LARGO, Y0 + linha * LIN + sub * SUB)

f <- tr_flow(pr$registry)
ops <- list()
frame <- function(linha, colunas, titulo, cor) {
  ops[[length(ops) + 1L]] <<- list(op = "add_frame", x = X0 - 30, y = Y0 + linha * LIN - 70,
                                   w = colunas * COL + 40 + (colunas >= 4) * LARGO, h = LIN - 80, title = titulo,
                                   aspect = "livre", color = cor)
}
tamanho <- function(no, w, h) ops[[length(ops) + 1L]] <<- list(op = "resize", node = no, w = w, h = h)
vista <- function(no, v) ops[[length(ops) + 1L]] <<- list(op = "set_view", node = no, view = v)

# ---- 1. DIC: PlantGrowth ------------------------------------------------------
frame(0, 6, "1 · DIC — peso seco de plantas (PlantGrowth)", "verde")
f <- f |>
  tr_add("dic_dados", "models/example", dataset = "PlantGrowth", label = "Dados", position = em(0, 0)) |>
  tr_add("dic_box", "view/boxplot", y = "weight", x = "group", titulo = "Peso seco por tratamento",
         from = "dic_dados", label = "Distribuição", position = em(0, 0, 1.3)) |>
  tr_add("dic", "models/anova_dic", resposta = "weight", tratamento = "group",
         from = "dic_dados", label = "ANOVA · DIC", position = em(0, 1)) |>
  tr_add("dic_sw", "models/shapiro_residuals", from = "dic", label = "Normalidade", position = em(0, 2)) |>
  tr_add("dic_lev", "models/levene", from = "dic", label = "Homogeneidade", position = em(0, 2, 1.3)) |>
  tr_add("dic_quadro", "models/anova_table", from = "dic", label = "Quadro", position = em(0, 3)) |>
  tr_add("dic_medias", "models/emmeans", especs = "group", from = "dic", label = "Tukey", position = em(0, 4)) |>
  tr_add("dic_dunnett", "models/pairwise", metodo = "contra controle", controle = "ctrl",
         ajuste = "dunnett", from = "dic_medias", label = "Dunnett vs. controle", position = em(0, 5)) |>
  tr_add("dic_kw", "models/kruskal", resposta = "weight", grupo = "group",
         from = "dic_dados", label = "Alternativa não paramétrica", position = em(0, 1, 1.3)) |>
  tr_add("dic_contr", "models/linear_hypothesis", fator = "group",
         hipoteses = "controle vs tratados: 2 -1 -1; trt1 vs trt2: trt1 - trt2",
         from = "dic", label = "Contrastes ortogonais", position = em(0, 3, 1.3))
tamanho("dic_box", 280, 200)

# ---- 2. DBC: milho --------------------------------------------------------------
frame(1, 7, "2 · DBC — híbridos de milho em blocos (simulado: H3 plantado acima)", "azul")
f <- f |>
  tr_add("dbc_dados", "models/example", dataset = "milho_dbc", label = "Dados", position = em(1, 0)) |>
  tr_add("dbc", "models/anova_dbc", resposta = "producao", tratamento = "hibrido", bloco = "bloco",
         from = "dbc_dados", label = "ANOVA · DBC", position = em(1, 1)) |>
  tr_add("dbc_medidas", "models/fit_stats", from = "dbc", label = "CV e R²", position = em(1, 1, 1.3)) |>
  tr_add("dbc_sw", "models/shapiro_residuals", from = "dbc", label = "Normalidade", position = em(1, 2)) |>
  tr_add("dbc_bart", "models/bartlett", from = "dbc", label = "Homogeneidade", position = em(1, 2, 1)) |>
  tr_add("dbc_adit", "models/tukey_additivity", from = "dbc", label = "Aditividade", position = em(1, 2, 2)) |>
  tr_add("dbc_quadro", "models/anova_table", from = "dbc", label = "Quadro", position = em(1, 3)) |>
  tr_add("dbc_medias", "models/emmeans", especs = "hibrido", from = "dbc", label = "Tukey", position = em(1, 4)) |>
  tr_add("dbc_contr", "models/linear_hypothesis", fator = "hibrido",
         hipoteses = "H3 vs demais: H3 - (H1 + H2 + H4 + H5) / 4",
         from = "dbc", label = "O H3 supera a média dos outros?", position = em(1, 3, 1.3)) |>
  tr_add("dbc_graf", "models/plot_means", titulo = "Produção dos híbridos (t/ha)",
         from = "dbc_medias", label = "Gráfico para o artigo", position = em(1, 5)) |>
  tr_add("dbc_tabela", "data/arrange", cols = "grupo", from = "dbc_medias",
         label = "Tabela de médias", position = em(1, 5, 1.5)) |>
  tr_add("dbc_duncan", "models/duncan", tratamento = "hibrido", from = "dbc",
         label = "Duncan (separa o H5)", position = em(1, 6)) |>
  tr_add("dbc_waller", "models/waller_duncan", tratamento = "hibrido", from = "dbc",
         label = "Waller-Duncan (K = 100)", position = em(1, 6, 1.5))
tamanho("dbc_medias", 280, 200); tamanho("dbc_graf", 280, 200)
tamanho("dbc_duncan", 280, 200); tamanho("dbc_waller", 280, 200)

# ---- 3. DQL: rações ---------------------------------------------------------------
frame(2, 5, "3 · DQL — rações em períodos × lotes (simulado: R5 plantada acima)", "amarelo")
f <- f |>
  tr_add("dql_dados", "models/example", dataset = "racao_dql", label = "Dados", position = em(2, 0)) |>
  tr_add("dql", "models/anova_dql", resposta = "ganho_peso", tratamento = "racao", linha = "periodo",
         coluna = "lote", from = "dql_dados", label = "ANOVA · DQL", position = em(2, 1)) |>
  tr_add("dql_sw", "models/shapiro_residuals", from = "dql", label = "Normalidade", position = em(2, 2)) |>
  tr_add("dql_quadro", "models/anova_table", from = "dql", label = "Quadro", position = em(2, 3)) |>
  tr_add("dql_medias", "models/emmeans", especs = "racao", from = "dql", label = "Tukey", position = em(2, 4))
tamanho("dql_medias", 280, 200)

# ---- 4. Fatorial com interação: ToothGrowth ------------------------------------------
frame(3, 6, "4 · Fatorial 2 × 3 com interação — suplemento × dose (ToothGrowth)", "laranja")
f <- f |>
  tr_add("fat_dados", "models/example", dataset = "ToothGrowth", label = "Dados", position = em(3, 0)) |>
  tr_add("fat_box", "view/boxplot", y = "len", x = "supp", cor = "supp", titulo = "Comprimento por suplemento",
         from = "fat_dados", label = "Distribuição", position = em(3, 0, 1.3)) |>
  tr_add("fat", "models/anova_factorial", resposta = "len", fatores = "supp, dose",
         from = "fat_dados", label = "ANOVA · fatorial", position = em(3, 1)) |>
  tr_add("fat_diag", "models/plot_diagnostics", from = "fat", label = "Diagnóstico", position = em(3, 2)) |>
  tr_add("fat_quadro", "models/anova_table", tipo_sq = "II", from = "fat", label = "Quadro (interação!)",
         position = em(3, 3)) |>
  tr_add("fat_dose_supp", "models/emmeans", especs = "dose", por = "supp", from = "fat",
         label = "Doses dentro de cada suplemento", position = em(3, 4)) |>
  tr_add("fat_supp_dose", "models/emmeans", especs = "supp", por = "dose", from = "fat",
         label = "Suplementos dentro de cada dose", position = em(3, 4, 1.3)) |>
  tr_add("fat_graf", "models/plot_means", titulo = "Desdobramento da interação",
         from = "fat_dose_supp", label = "Gráfico", position = em(3, 5))
tamanho("fat_diag", 280, 280); tamanho("fat_dose_supp", 280, 200); tamanho("fat_supp_dose", 280, 200)
tamanho("fat_graf", 280, 200); tamanho("fat_box", 280, 200)

# ---- 5. Fatorial 2³ em blocos: npk -----------------------------------------------------
frame(4, 5, "5 · Fatorial 2³ em blocos — N, P e K em ervilha (npk)", "roxo")
f <- f |>
  tr_add("npk_dados", "models/example", dataset = "npk", label = "Dados", position = em(4, 0)) |>
  tr_add("npk", "models/anova_factorial", resposta = "yield", fatores = "N, P, K", bloco = "block",
         from = "npk_dados", label = "ANOVA · fatorial em DBC", position = em(4, 1)) |>
  tr_add("npk_sw", "models/shapiro_residuals", from = "npk", label = "Normalidade", position = em(4, 2)) |>
  tr_add("npk_quadro", "models/anova_table", from = "npk", label = "Quadro", position = em(4, 3)) |>
  tr_add("npk_n", "models/emmeans", especs = "N", from = "npk", label = "Efeito do nitrogênio", position = em(4, 4)) |>
  tr_add("npk_k", "models/emmeans", especs = "K", from = "npk", label = "Efeito do potássio", position = em(4, 4, 1.3))


# ---- 6. Parcela subdividida: aveia -------------------------------------------------------
frame(5, 6, "6 · Parcela subdividida — variedade na parcela, nitrogênio na subparcela (Yates)", "rosa")
f <- f |>
  tr_add("sp_dados", "models/example", dataset = "aveia", label = "Dados", position = em(5, 0)) |>
  tr_add("sp", "models/anova_split_plot", resposta = "producao", parcela = "variedade",
         subparcela = "nitrogenio", bloco = "bloco", from = "sp_dados", label = "ANOVA · parcela subdividida",
         position = em(5, 1)) |>
  tr_add("sp_sw", "models/shapiro_residuals", from = "sp", label = "Normalidade (erro b)", position = em(5, 2)) |>
  tr_add("sp_var", "models/random_effects", from = "sp", label = "Variância entre parcelas",
         position = em(5, 2, 1.3)) |>
  tr_add("sp_quadro", "models/anova_table", from = "sp", label = "Quadro com erros (a) e (b)", position = em(5, 3)) |>
  tr_add("sp_contr", "models/linear_hypothesis", fator = "nitrogenio",
         hipoteses = "linear: -3 -1 1 3; quadrático: 1 -1 -1 1; cúbico: -1 3 -3 1",
         from = "sp", label = "Contrastes polinomiais da dose", position = em(5, 3, 1.3)) |>
  tr_add("sp_n", "models/emmeans", especs = "nitrogenio", from = "sp", label = "Tukey do nitrogênio",
         position = em(5, 4)) |>
  tr_add("sp_v", "models/emmeans", especs = "variedade", from = "sp", label = "Tukey das variedades",
         position = em(5, 4, 1.3)) |>
  # A dose como número: o que a ANOVA trata como 4 níveis vira uma curva.
  tr_add("sp_dose", "data/mutate", name = "dose", expr = "as.numeric(sub('cwt', '', nitrogenio))",
         from = "sp_dados", label = "Dose numérica", position = em(5, 0, 1.3)) |>
  tr_add("sp_linear", "models/lmer", formula = "producao ~ variedade + dose + (1 | bloco/variedade)",
         from = "sp_dose", label = "Regressão linear na dose", position = em(5, 5)) |>
  tr_add("sp_quad", "models/lmer", formula = "producao ~ variedade + dose + I(dose^2) + (1 | bloco/variedade)",
         from = "sp_dose", label = "Regressão quadrática na dose", position = em(5, 5, 1)) |>
  tr_add("sp_cmp", "models/compare", from = "sp_linear", label = "O termo quadrático é preciso?",
         position = em(5, 5, 2)) |>
  tr_link("sp_quad", "sp_cmp:outro")
tamanho("sp_n", 280, 200)

# ---- 7. Contagem: InsectSprays -------------------------------------------------------------
frame(6, 7, "7 · Contagem — quando a ANOVA não serve e o GLM resolve (InsectSprays)", "cinza")
f <- f |>
  tr_add("cont_dados", "models/example", dataset = "InsectSprays", label = "Dados", position = em(6, 0)) |>
  tr_add("cont_anova", "models/anova_dic", resposta = "count", tratamento = "spray",
         from = "cont_dados", label = "ANOVA ingênua", position = em(6, 1)) |>
  tr_add("cont_diag", "models/plot_diagnostics", from = "cont_anova", label = "O funil", position = em(6, 2)) |>
  tr_add("cont_lev", "models/levene", from = "cont_anova", label = "Variâncias diferentes", position = em(6, 1, 1.3)) |>
  tr_add("cont_glm", "models/glm", resposta = "count", preditores = "spray", familia = "poisson",
         from = "cont_dados", label = "GLM Poisson", position = em(6, 3)) |>
  tr_add("cont_quadro", "models/anova_table", tipo_sq = "II", from = "cont_glm", label = "Quadro de desvio",
         position = em(6, 4)) |>
  tr_add("cont_medias", "models/emmeans", especs = "spray", from = "cont_glm",
         label = "Médias na escala da contagem", position = em(6, 5.9))
tamanho("cont_diag", 280, 280); tamanho("cont_medias", 280, 200)

# ---- 8. Medidas repetidas: sleepstudy -----------------------------------------------------------
frame(7, 6, "8 · Medidas repetidas — modelo misto (sleepstudy)", "verde")
f <- f |>
  tr_add("rep_dados", "models/example", dataset = "sleepstudy", label = "Dados", position = em(7, 0)) |>
  tr_add("rep_graf", "view/points", x = "Days", y = "Reaction", cor = "Subject",
         titulo = "Tempo de reação ao longo dos dias", legenda = "nenhuma",
         from = "rep_dados", label = "Uma cor por pessoa", position = em(7, 0, 1.3)) |>
  tr_add("rep_int", "models/lmer", formula = "Reaction ~ Days + (1 | Subject)", from = "rep_dados",
         label = "Intercepto aleatório", position = em(7, 1)) |>
  tr_add("rep_incl", "models/lmer", formula = "Reaction ~ Days + (Days | Subject)", from = "rep_dados",
         label = "Inclinação aleatória", position = em(7, 1, 1.3)) |>
  tr_add("rep_cmp", "models/compare", from = "rep_int", label = "A inclinação varia entre pessoas?",
         position = em(7, 2)) |>
  tr_link("rep_incl", "rep_cmp:outro") |>
  tr_add("rep_var", "models/random_effects", from = "rep_incl", label = "Componentes de variância",
         position = em(7, 3)) |>
  tr_add("rep_coef", "models/coefficients", from = "rep_incl", label = "Efeito dos dias", position = em(7, 4)) |>
  tr_add("rep_lagarta", "models/plot_caterpillar", intervalo = "IC 95%", from = "rep_incl",
         label = "Lagarta: cada pessoa", position = em(7, 5))
tamanho("rep_graf", 280, 200); tamanho("rep_lagarta", 300, 400)

for (q in c("dic_quadro", "dbc_quadro", "dql_quadro", "fat_quadro", "npk_quadro", "sp_quadro",
            "cont_quadro")) tamanho(q, QW, QH)
tamanho("npk_quadro", QW, 380)
doc <- tr_flow_doc(f)
# Params RECOLHIDOS em todo card: o projeto é para ler a análise, e um card com
# cinco campos abertos ocupa o dobro da altura do resultado que importa. Um
# clique na barra do card os abre.
for (no in names(doc$nodes)) ops[[length(ops) + 1L]] <- list(op = "set_mode", node = no, modo = "preview")
for (op in ops) doc <- tr_doc_apply(doc, op, pr$registry)
tr_project_save(pr, doc)
cat(sprintf("flows/main.json: %d nós, %d frames\n", length(doc$nodes), length(doc$ui$frames)))
