# MANOVA: os tratamentos diferem no VETOR de médias das respostas?
#
# É a pergunta anterior a uma ANOVA por variável: com cinco caracteres
# medidos, cinco ANOVAs a 5% dão ~23% de chance de um falso positivo; a MANOVA
# testa todos de uma vez e ainda usa a correlação entre eles. Sai no `data/test`
# do núcleo, uma estatística por nó — as quatro concordam quase sempre, e
# quando discordam a de Pillai é a que resiste à heterogeneidade das
# covariâncias (Olson, 1976), por isso é o padrão.

.TR_MULTI_MANOVA <- c(Pillai = "Pillai", Wilks = "Wilks", `Hotelling-Lawley` = "Hotelling-Lawley",
                      Roy = "Roy")

#' MANOVA de um fator, com bloco opcional.
#' @param dados tabela.
#' @param respostas as variáveis resposta (duas ou mais), separadas por vírgula.
#' @param tratamento coluna do tratamento.
#' @param bloco coluna do bloco; em branco, delineamento inteiramente casualizado.
#' @param estatistica `"Pillai"`, `"Wilks"`, `"Hotelling-Lawley"` ou `"Roy"`.
#' @return teste (`trama::tr_test`), no tipo `data/test`.
#' @export
tr_multi_manova <- function(dados, respostas = "", tratamento = "", bloco = "", estatistica = "Pillai") {
  no <- "multi/manova"
  estatistica <- .tr_multi_enum(estatistica, names(.TR_MULTI_MANOVA), "estatistica")
  trat <- .tr_multi_col(dados, tratamento, "tratamento")
  com_bloco <- nzchar(trimws(as.character(bloco)[1L] |> .tr_multi_ou("")))
  blc <- if (com_bloco) .tr_multi_col(dados, bloco, "bloco") else character()
  # Sem o default "todas as numéricas": tratamento e bloco costumam vir
  # codificados como número (1, 2, 3), e entrariam calados como resposta.
  if (!length(.tr_multi_split(respostas))) .tr_multi_obrigatorio("", "respostas")
  ys <- .tr_multi_variaveis(dados, respostas, param = "respostas", minimo = 2L)
  if (any(c(trat, blc) %in% ys)) {
    .tr_multi_abort("tr_multi_error_bad_option",
                    "'%s': tratamento e bloco não podem estar também entre as respostas.", no)
  }
  Y <- .tr_multi_matriz(dados, ys, no)
  fatores <- as.data.frame(dados)[, c(trat, blc), drop = FALSE]
  if (anyNA(fatores)) {
    .tr_multi_abort("tr_multi_error_missing_values",
                    "'%s' não aceita faltantes, e há linhas sem tratamento ou bloco. Ligue um 'data/drop_na' antes.", no)
  }
  tr <- droplevels(as.factor(fatores[[trat]]))
  if (nlevels(tr) < 2L) {
    .tr_multi_abort("tr_multi_error_one_group", "'%s': o tratamento (%s) tem menos de dois níveis.", no, trat)
  }
  # Fórmula com nomes fixos (`.tr`, `.bl`): nome de coluna com espaço ou acento
  # quebraria a fórmula. O bloco vai ANTES, para o tratamento ser testado
  # descontado dele (soma de quadrados sequencial, como no DBC).
  df <- data.frame(.tr = tr)
  if (com_bloco) df$.bl <- droplevels(as.factor(fatores[[blc]]))
  f <- if (com_bloco) Y ~ .bl + .tr else Y ~ .tr
  ajuste <- .tr_multi_ajustar(stats::manova(f, data = df), no)
  tab <- .tr_multi_ajustar(summary(ajuste, test = .TR_MULTI_MANOVA[[estatistica]])$stats, no)
  if (nrow(tab) < 1L || !".tr" %in% rownames(tab) || !is.finite(tab[".tr", 3L])) {
    .tr_multi_abort("tr_multi_error_fit",
                    "'%s': a MANOVA não tem graus de liberdade no resíduo — há mais respostas do que repetições permitem.", no)
  }
  l <- tab[".tr", ]
  trama::tr_test(
    sprintf("MANOVA (%s)", estatistica),
    sprintf("o vetor de médias de %s é o mesmo em todos os níveis de %s", paste(ys, collapse = ", "), trat),
    l[[3L]], "F aprox.", p_valor = l[[6L]],
    gl = sprintf("%s; %s", format(l[[4L]]), format(l[[5L]])),
    conclusao_sim = paste0(
      "Os tratamentos diferem em ao menos uma combinação das respostas. Para saber em ",
      "quais, siga com uma ANOVA por variável ou com a discriminante, que mostra a ",
      "combinação que mais separa os tratamentos."),
    conclusao_nao = paste0(
      "Sem evidência de diferença entre os tratamentos no conjunto das respostas. ",
      "Não rejeitar não prova igualdade; com poucas repetições o teste tem pouco poder."),
    nota = if (com_bloco) sprintf("Bloco (%s) descontado antes do tratamento.", blc) else "",
    fonte = "stats::manova; Pillai (1955), Wilks (1932)",
    extra = stats::setNames(list(l[[2L]]), tolower(gsub("-", "_", estatistica))),
    classe = "tr_multi_test")
}

.tr_multi_nos_manova <- function() {
  P <- trama::tr_param
  list(
    trama::tr_node("multi/manova", role = "avaliacao", fn = tr_multi_manova, label = "MANOVA",
      category = "multi_manova", icon = trama::tr_icon("layers"),
      description = "Análise de variância multivariada: os tratamentos diferem no conjunto das respostas?",
      inputs = list(dados = "data/table"), outputs = list(out = "data/test"),
      params = list(
        respostas = P("cols", "", label = "Respostas", example = "alcool, flavonoides, magnesio"),
        tratamento = P("cols", "", label = "Tratamento", example = "cultivar"),
        bloco = P("cols", "", label = "Bloco", example = "bloco"),
        estatistica = trama::tr_param_enum("Pillai", names(.TR_MULTI_MANOVA), label = "Estatística")),
      help = .tr_multi_ajuda(r"---[
A análise de variância MULTIVARIADA: testa se os tratamentos diferem no vetor
de médias de várias respostas ao mesmo tempo (produção, altura, teor de óleo),
levando em conta a correlação entre elas.

H0: o vetor de médias das respostas é o mesmo em todos os tratamentos.

Por que não uma ANOVA por variável: com cinco respostas a 5%, a chance de ao
menos um falso positivo passa de 20%. A MANOVA faz um teste só; se ela
rejeita, as ANOVAs por variável (ou a discriminante) dizem ONDE está a
diferença.

### Delineamento

- Sem **Bloco**, inteiramente casualizado: `respostas ~ tratamento`.
- Com **Bloco**, blocos casualizados: `respostas ~ bloco + tratamento`, e o
  tratamento é testado descontado o bloco.

### Estatística

As quatro medem a razão entre a variação dos tratamentos e a do resíduo,
resumida de jeitos diferentes; todas viram um F aproximado.

- **Pillai** (padrão) — a mais robusta a covariâncias diferentes entre
  tratamentos e a desvios de normalidade.
- **Wilks** — a clássica dos livros (Λ de Wilks); menor Λ, mais diferença.
- **Hotelling-Lawley** — mais poder quando a diferença se espalha por várias
  direções.
- **Roy** — só a maior raiz: mais poder quando a diferença está numa direção
  só, mas o F dela é um limite superior (o p-valor sai otimista).

O valor da estatística em si vai numa coluna extra (`extra_pillai`,
`extra_wilks`...), para citar no texto.

### Pressupostos

Resíduos normais multivariados e a mesma matriz de covariância em todos os
tratamentos (o `multi/box_m` testa essa). Precisa de mais repetições do que
respostas: o resíduo tem de ter graus de liberdade.
]---", r"---[
- **Respostas** — duas ou mais colunas numéricas, separadas por vírgula.
- **Tratamento** — a coluna do tratamento.
- **Bloco** — a coluna do bloco. Em branco, sem bloco.
- **Estatística** — `Pillai`, `Wilks`, `Hotelling-Lawley` ou `Roy`.
]---", r"---[
Um teste (`data/test`), com a régua do p-valor: o F aproximado é a estatística,
os graus de liberdade vão como "numerador; denominador" e o valor da
estatística escolhida numa coluna extra. Ligado numa entrada de tabela, vira
UMA linha com as colunas de todo teste.
]---", r"---[
tr_flow(reg) |>
  tr_add("v", "multi/example", dataset = "vinhos") |>
  tr_add("man", "multi/manova", respostas = "alcool, flavonoides, magnesio",
         tratamento = "cultivar", estatistica = "Wilks", from = "v")
]---", r"---[
`multi/box_m` para o pressuposto das covariâncias; `multi/discriminant` para a
combinação das respostas que mais separa os tratamentos; `models/anova_dbc`
para a ANOVA de cada resposta.
]---", teste = TRUE))
  )
}
