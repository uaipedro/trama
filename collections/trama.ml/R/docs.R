# Pressupostos e referências dos blocos: o que cada método supõe, como
# conferir no trama e de onde ele vem.
#
# Em aprendizado de máquina os pressupostos são, quase todos, de validação e de
# dados — não de distribuição: o teste isolado do treino e do tuning, linhas
# independentes, métrica adequada ao desequilíbrio das classes.
#
# Um arquivo por grupo (`docs_*.R`), cada um devolvendo uma lista nomeada pelo
# id do nó. As referências foram conferidas na fonte (DOI no Crossref, livros no
# catálogo da editora); a de implementação aponta a função que o bloco
# REALMENTE chama.

.tr_ml_P <- function(texto, verificar = NULL, se_falhar = NULL) {
  trama::tr_pressuposto(texto, verificar = verificar, se_falhar = se_falhar)
}

.tr_ml_impl <- function(pacote, funcao, nota = NULL) {
  trama::tr_ref(papel = "implementacao", pacote = pacote, funcao = funcao, nota = nota)
}

#' Os livros-texto e pressupostos usados em mais de um bloco.
#' @noRd
.tr_ml_livros <- function() {
  R <- trama::tr_ref
  list(
    islr = R(autores = c("James, G.", "Witten, D.", "Hastie, T.", "Tibshirani, R."), ano = 2021,
             titulo = "An Introduction to Statistical Learning: with Applications in R",
             fonte = "2. ed. New York: Springer", doi = "10.1007/978-1-0716-1418-1",
             url = "https://www.statlearning.com/", papel = "livro-texto"),
    esl = R(autores = c("Hastie, T.", "Tibshirani, R.", "Friedman, J."), ano = 2009,
            titulo = "The Elements of Statistical Learning: Data Mining, Inference, and Prediction",
            fonte = "2. ed. New York: Springer", doi = "10.1007/978-0-387-84858-7",
            papel = "livro-texto"),
    roberts = R(autores = c("Roberts, D. R.", "Bahn, V.", "Ciuti, S.", "Boyce, M. S.", "Elith, J.",
                            "Guillera-Arroita, G.", "Hauenstein, S.", "Lahoz-Monfort, J. J.", "Schröder, B.",
                            "Thuiller, W.", "Warton, D. I.", "Wintle, B. A.", "Hartig, F.", "Dormann, C. F."),
                ano = 2017,
                titulo = "Cross-validation strategies for data with temporal, spatial, hierarchical, or phylogenetic structure",
                fonte = "Ecography, 40(8), 913-929", doi = "10.1111/ecog.02881", papel = "teoria"),
    tashman = R(autores = "Tashman, L. J.", ano = 2000,
                titulo = "Out-of-sample tests of forecasting accuracy: an analysis and review",
                fonte = "International Journal of Forecasting, 16(4), 437-450",
                doi = "10.1016/S0169-2070(00)00065-0", papel = "teoria"),
    fpp3 = R(autores = c("Hyndman, R. J.", "Athanasopoulos, G."), ano = 2021,
             titulo = "Forecasting: Principles and Practice", fonte = "3. ed. Melbourne: OTexts, sec. 5.10",
             url = "https://otexts.com/fpp3/tscv.html", papel = "livro-texto"),
    bergmeir18 = R(autores = c("Bergmeir, C.", "Hyndman, R. J.", "Koo, B."), ano = 2018,
                   titulo = "A note on the validity of cross-validation for evaluating autoregressive time series prediction",
                   fonte = "Computational Statistics & Data Analysis, 120, 70-83",
                   doi = "10.1016/j.csda.2017.11.003", papel = "complementar"),
    kuhn = R(autores = c("Kuhn, M.", "Johnson, K."), ano = 2013,
             titulo = "Applied Predictive Modeling", fonte = "New York: Springer",
             doi = "10.1007/978-1-4614-6849-3", papel = "livro-texto")
  )
}

#' Pressupostos de validação comuns aos blocos que ajustam modelos.
#' @noRd
.tr_ml_comuns <- function() {
  P <- .tr_ml_P
  list(
    so_treino = P("O bloco recebe **só o treino**: o teste foi separado antes (no `ml/split`) e não entra em nenhuma escolha — nem de preditores, nem de hiperparâmetros, nem de tratamento de faltantes. O bloco não tem como saber o que recebeu.",
      se_falhar = "Refaça o fluxo com o `ml/split` no início; a saída teste só vai ao `ml/predict`. Hiperparâmetros se escolhem no `ml/tune`, com validação cruzada dentro do treino."),
    sem_vazamento = P("Nenhum preditor **revela a resposta** (identificador, código que a embute, medida tomada depois do desfecho).",
      verificar = "data/summary",
      se_falhar = "Tire essas colunas de `cols` (ou com o `data/select` antes do `ml/split`)."),
    iid = P("As linhas são **independentes e vêm da mesma população** em que o modelo vai ser usado: sem ordem no tempo, sem várias linhas do mesmo indivíduo, lote ou área repartidas entre treino e teste.",
      se_falhar = "Com tempo, use `estrategia = \"temporal\"` no `ml/split` e no `ml/tune` (teste sempre posterior ao treino); com indivíduos, lotes ou áreas repetidos, `estrategia = \"grupo\"` (nenhum grupo dos dois lados)."),
    semente = P("O resultado depende da **semente**: outra semente muda a divisão, os folds ou o próprio ajuste, e com poucos dados a diferença pode ser grande.",
      se_falhar = "Repita com outras sementes e leia a variação; não escolha a semente pelo resultado.")
  )
}

#' Pressupostos e referências de um nó (listas vazias se não houver).
#' @noRd
.tr_ml_doc <- function(id) {
  todos <- c(.tr_ml_docs_modelos(), .tr_ml_docs_avaliacao())
  d <- todos[[id]]
  list(pressupostos = d$pressupostos %||% list(), referencias = d$referencias %||% list())
}
