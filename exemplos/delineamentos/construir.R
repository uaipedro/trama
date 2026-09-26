# Gera `flows/main.json` do projeto de delineamentos.
#
# O caso de validação da coleção `trama.experiments`: uma parcela subdividida
# (irrigação na parcela, variedade na subparcela, 4 blocos) com a resposta
# composta termo a termo, analisada de dois jeitos lado a lado — a análise de
# parcela subdividida e a ingênua, o fatorial em blocos, que testa a irrigação
# contra o resíduo das subparcelas. A irrigação NÃO tem efeito no modelo
# declarado: tudo que a análise disser sobre ela é erro tipo I.
#
# São duas linhas de frame: a de cima COM o erro de parcela (bloco:parcela), a
# de baixo sem ele. Sem erro de parcela, as duas análises concordam; com ele,
# só a de parcela subdividida mantém o tipo I em 5%.
#
# Escrito pela DSL para que refazê-lo depois de uma mudança na coleção seja
# rodar este arquivo. Rodar de novo SOBRESCREVE o que foi arrumado no editor.
#
#   Rscript exemplos/delineamentos/construir.R
setwd(local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a[[1]]))) else normalizePath(getwd())
}))
colecoes <- c("trama.data", "trama.view", "trama.models", "trama.experiments")
suppressMessages({
  pkgload::load_all("../..", quiet = TRUE, attach = TRUE)
  for (p in colecoes) pkgload::load_all(file.path("../../collections", p), quiet = TRUE, attach = FALSE)
})

pr <- tr_project(".", collections = colecoes)

COL <- 330; LIN <- 1000; SUB <- 330; X0 <- 40; Y0 <- 90
em <- function(linha, coluna, sub = 0) c(X0 + coluna * COL, Y0 + linha * LIN + sub * SUB)

f <- tr_flow(pr$registry)
ops <- list()
frame <- function(linha, colunas, titulo, cor) {
  ops[[length(ops) + 1L]] <<- list(op = "add_frame", x = X0 - 30, y = Y0 + linha * LIN - 70,
                                   w = colunas * COL + 40, h = LIN - 80, title = titulo,
                                   aspect = "livre", color = cor)
}
tamanho <- function(no, w, h) ops[[length(ops) + 1L]] <<- list(op = "resize", node = no, w = w, h = h)

# Uma linha do caso: o mesmo plano e os mesmos termos; `parcela` liga ou
# desliga o erro de parcela. `p` é o prefixo dos ids.
caso <- function(f, linha, p, parcela) {
  id <- function(x) paste0(p, "_", x)
  f <- f |>
    tr_add(id("plano"), "experiments/design", estrutura = "parcela_subdividida",
           fatores = "irrigacao: baixa, alta; variedade: A, B, C", repeticoes = 4L,
           label = "Parcela subdividida", position = em(linha, 0)) |>
    tr_add(id("mapa"), "experiments/view", aba = "hierarquia", from = id("plano"),
           label = "Quem é unidade de quê", position = em(linha, 0, 1)) |>
    tr_add(id("mu"), "experiments/effect", tipo = "intercepto", valor = 50, from = id("plano"),
           label = "Intercepto μ = 50", position = em(linha, 1)) |>
    tr_add(id("bloco"), "experiments/effect", tipo = "aleatorio", fator = "bloco", sd = 3,
           from = id("mu"), label = "Bloco aleatório (sd 3)", position = em(linha, 1, 1)) |>
    tr_add(id("irr"), "experiments/effect", tipo = "fixo", fator = "irrigacao",
           efeitos = "baixa = 0, alta = 0", from = id("bloco"),
           label = "Irrigação sem efeito (H0 verdadeira)", position = em(linha, 1, 2))
  ultimo <- id("irr")
  f <- f |>
    tr_add(id("var"), "experiments/effect", tipo = "fixo", fator = "variedade", conjunto = "controle",
           controle = "A", magnitudes = "-3, 2", from = ultimo,
           label = "Variedade: A vs demais = −3, C vs B = 2", position = em(linha, 2))
  ultimo <- id("var")
  if (parcela) {
    f <- f |>
      tr_add(id("parc"), "experiments/effect", tipo = "aleatorio", fator = "bloco:parcela", sd = 4,
             from = ultimo, label = "Erro de parcela (sd 4)", position = em(linha, 2, 1))
    ultimo <- id("parc")
  }
  f |>
    tr_add(id("y"), "experiments/error", resposta = "producao", sd = 1, from = ultimo,
           label = "Erro de subparcela (sd 1)", position = em(linha, 2, 2)) |>
    tr_add(id("split"), "models/anova_split_plot", resposta = "producao", parcela = "irrigacao",
           subparcela = "variedade", bloco = "bloco", from = id("y"),
           label = "Parcela subdividida", position = em(linha, 3)) |>
    tr_add(id("ingenua"), "models/anova_factorial", resposta = "producao",
           fatores = "irrigacao, variedade", bloco = "bloco", from = id("y"),
           label = "Ingênua: fatorial em blocos", position = em(linha, 3, 1.4)) |>
    tr_add(id("contr"), "experiments/contrasts", fator = "variedade", conjunto = "controle",
           controle = "A", from = id("split"),
           label = "Variedades contra A", position = em(linha, 4)) |>
    tr_add(id("comp"), "experiments/view", aba = "componentes", from = id("y"),
           label = "A resposta, termo a termo", position = em(linha, 5))
}

frame(0, 6, "Com erro de parcela — só a análise de parcela subdividida mantém o tipo I", "vermelho")
f <- caso(f, 0, "com", parcela = TRUE)
frame(1, 6, "Sem erro de parcela — as duas análises concordam", "verde")
f <- caso(f, 1, "sem", parcela = FALSE)

doc <- tr_flow_doc(f)
# Sementes fixas: rodar de novo dá o mesmo croqui e a mesma resposta. As duas
# linhas usam as MESMAS sementes, então diferem só pelo erro de parcela.
for (p in c("com", "sem")) {
  sementes <- c(plano = 11L, bloco = 12L, parc = 13L, y = 14L)
  for (n in names(sementes)) {
    no <- paste0(p, "_", n)
    if (!is.null(doc$nodes[[no]])) ops[[length(ops) + 1L]] <- list(op = "set_seed", node = no, value = sementes[[n]])
  }
}
for (p in c("com", "sem")) {
  tamanho(paste0(p, "_split"), 300, 300); tamanho(paste0(p, "_ingenua"), 300, 300)
  tamanho(paste0(p, "_comp"), 320, 320)
}
# Params recolhidos: o card abre no resultado; um clique na barra abre os campos.
for (no in names(doc$nodes)) ops[[length(ops) + 1L]] <- list(op = "set_mode", node = no, modo = "preview")
for (op in ops) doc <- tr_doc_apply(doc, op, pr$registry)
tr_project_save(pr, doc)
cat(sprintf("flows/main.json: %d nós, %d frames\n", length(doc$nodes), length(doc$ui$frames)))
