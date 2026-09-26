# Pressupostos e referências: tamanhos de efeito e o post hoc de Dunn.

.tr_models_docs_efeito <- function() {
  P <- .tr_models_P; I <- .tr_models_impl; R <- trama::tr_ref
  L <- .tr_models_livros()
  cohen <- R(autores = "Cohen, J.", ano = 1988, titulo = "Statistical Power Analysis for the Behavioral Sciences",
             fonte = "2. ed. Hillsdale: Lawrence Erlbaum (reedição Routledge, 2013)",
             doi = "10.4324/9780203771587", papel = "livro-texto")
  list(
    "models/cohen_d" = list(
      pressupostos = list(
        P("As observações são **independentes**, dentro e entre os dois grupos (grupos independentes, não pareados).",
          se_falhar = "Com o mesmo indivíduo medido duas vezes, o d de grupos independentes não é o efeito certo: use o `models/paired_t`."),
        P("As **variâncias** dos dois grupos são parecidas: o d divide pelo desvio padrão COMBINADO, o mesmo do t de variâncias iguais.",
          verificar = c("models/levene", "view/boxplot"),
          se_falhar = "Com variâncias muito diferentes, o d combinado mistura duas escalas; relate as médias e os desvios de cada grupo."),
        P("A resposta é aproximadamente **normal** em cada grupo: o intervalo é o normal de Hedges & Olkin, e a leitura em desvios padrão supõe a normal.",
          verificar = c("models/shapiro", "view/qq")),
        P("O intervalo é **aproximado** (normal, com o erro padrão de Hedges & Olkin 1985), bom a partir de uns 10 por grupo; com menos, fica estreito demais.",
          se_falhar = "Com grupos pequenos, prefira o g e leia o intervalo como indicativo.")),
      referencias = list(cohen,
        R(autores = "Hedges, L. V.", ano = 1981,
          titulo = "Distribution theory for Glass's estimator of effect size and related estimators",
          fonte = "Journal of Educational Statistics, 6(2), 107-128", doi = "10.3102/10769986006002107"),
        R(autores = c("Hedges, L. V.", "Olkin, I."), ano = 1985, titulo = "Statistical Methods for Meta-Analysis",
          fonte = "Academic Press", doi = "10.1016/C2009-0-03396-0", papel = "livro-texto"),
        I("trama.models", "tr_models_cohen_d", "Conta própria: d com o DP combinado, g = J·d com J = 1 − 3/(4m − 1) (m = n1 + n2 − 2, Hedges 1981), EP² = (n1 + n2)/(n1 n2) + d²/(2(n1 + n2)). Conferido contra o t de `stats::t.test(var.equal = TRUE)` (d = t·√(1/n1 + 1/n2)) e contra o J exato Γ(m/2)/(√(m/2) Γ((m − 1)/2)) de Hedges (1981), a 1e-3."))),

    "models/effect_size" = list(
      pressupostos = list(
        P("O quadro da ANOVA é **válido**: o modelo está bem especificado e os erros cumprem os pressupostos do bloco que o ajustou (as SQ vêm dele).",
          verificar = c("models/plot_diagnostics", "models/shapiro_residuals", "models/levene")),
        P("Os fatores são **manipulados** (tratamentos do experimento): o ômega² parcial de Olejnik & Algina (2003) é o dessa situação. Com fator medido (sexo, idade), o eta² generalizado deles, que não está aqui, é o comparável entre estudos.",
          se_falhar = "Relate o eta² parcial e diga quais fatores são medidos."),
        P("Com **SQ tipo II ou III** (desbalanceado) as somas não dão o total: o eta² usa a soma das SQ do quadro, e a coluna `base_eta2` avisa.",
          verificar = "models/anova_table")),
      referencias = list(cohen,
        R(autores = c("Olejnik, S.", "Algina, J."), ano = 2003,
          titulo = "Generalized eta and omega squared statistics: measures of effect size for some common research designs",
          fonte = "Psychological Methods, 8(4), 434-447", doi = "10.1037/1082-989X.8.4.434"),
        I("trama.models", "tr_models_effect_size", "Das SQ e QM do `tr_models_anova_table`, com o erro de cada termo (na subdividida, o (a) para a parcela e o (b) para o resto)."))),

    "models/dunn" = list(
      pressupostos = list(
        P("As observações são **independentes**, dentro e entre os grupos.",
          se_falhar = "Medidas repetidas no mesmo indivíduo pedem o `models/friedman`."),
        P("A resposta é ao menos **ordinal**, e os grupos têm distribuições de **mesma forma**: só assim uma diferença de postos médios se lê como deslocamento (de mediana).",
          verificar = c("view/boxplot", "view/density")),
        P("É o **post hoc do Kruskal-Wallis**: faz sentido depois de ele rejeitar, e usa os postos da amostra INTEIRA.",
          verificar = "models/kruskal"),
        P("A estatística z é **normal aproximada**: com grupos muito pequenos (menos de uns 5) o p é só indicativo.")),
      referencias = list(
        R(autores = "Dunn, O. J.", ano = 1964, titulo = "Multiple comparisons using rank sums",
          fonte = "Technometrics, 6(3), 241-252", doi = "10.1080/00401706.1964.10490181"),
        L$siegel,
        I("trama.models", "tr_models_dunn", "Conta própria (z dos postos médios com correção de empates; p ajustado por `stats::p.adjust` ou Šidák). Conferido contra `dunn.test::dunn.test(altp = TRUE)` 1.3.6, com empates, nas quatro correções (z a 1e-9, p a 1e-8).")))
  )
}
