.tr_models_docs_reamostrar <- function() {
  P <- .tr_models_P; R <- trama::tr_ref; I <- .tr_models_impl
  list("models/permutation" = list(
    pressupostos = list(
      P("A resposta é permutável sob a hipótese nula entre as observações; use **Dentro de** quando a permutabilidade vale apenas dentro de estratos (por exemplo, blocos).",
        se_falhar = "Use o desenho de randomização do experimento em `experiments/randomization_test`, ou especifique os estratos corretos."),
      P("O modelo linear e o **termo escolhido** representam a pergunta testada; o teste é do F sequencial tipo I e depende da ordem dos termos.")),
    referencias = list(
      R(autores = c("Hothorn, T.", "Hornik, K.", "van de Wiel, M. A.", "Zeileis, A."), ano = 2008,
        titulo = "Implementing a Class of Permutation Tests: The coin Package",
        fonte = "Journal of Statistical Software, 28(8), 1-23", doi = "10.18637/jss.v028.i08"),
      R(autores = c("Phipson, B.", "Smyth, G. K."), ano = 2010,
        titulo = "Permutation P-values Should Never Be Zero: Calculating Exact P-values When Permutations Are Randomly Drawn",
        fonte = "Statistical Applications in Genetics and Molecular Biology, 9(1), Article 39", doi = "10.2202/1544-6115.1585"),
      I("stats", "anova.lm; lm", "Quadro tipo I para o F observado e reajuste da resposta permutada."))))
}
