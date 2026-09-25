# Pressupostos e referências dos blocos, num lugar só.
#
# Cada referência daqui foi conferida na fonte (DOI no Crossref, livro no
# catálogo da editora) em 2026-09-25. As de implementação apontam para o motor
# próprio da coleção: nenhum bloco chama o `survey`; a variância é a do
# conglomerado último (`.tr_sampling_var_total`), a mesma de
# `survey::svydesign(ids, strata, fpc)`.

.tr_sampling_refs <- function() {
  R <- trama::tr_ref
  list(
    cochran = R(autores = "Cochran, W. G.", ano = 1977, titulo = "Sampling Techniques",
                fonte = "3. ed. New York: Wiley", papel = "livro-texto"),
    bolfarine = R(autores = c("Bolfarine, H.", "Bussab, W. O."), ano = 2005,
                  titulo = "Elementos de Amostragem", fonte = "São Paulo: Blucher", papel = "livro-texto"),
    lohr = R(autores = "Lohr, S. L.", ano = 2021, titulo = "Sampling: Design and Analysis",
             fonte = "3. ed. Boca Raton: Chapman and Hall/CRC", doi = "10.1201/9780429298899",
             papel = "livro-texto"),
    kish = R(autores = "Kish, L.", ano = 1965, titulo = "Survey Sampling", fonte = "New York: Wiley",
             papel = "teoria"),
    horvitz = R(autores = c("Horvitz, D. G.", "Thompson, D. J."), ano = 1952,
                titulo = "A Generalization of Sampling Without Replacement from a Finite Universe",
                fonte = "Journal of the American Statistical Association, 47(260), 663-685",
                doi = "10.1080/01621459.1952.10483446", papel = "teoria"),
    woodruff = R(autores = "Woodruff, R. S.", ano = 1971,
                 titulo = "A Simple Method for Approximating the Variance of a Complicated Estimate",
                 fonte = "Journal of the American Statistical Association, 66(334), 411-414",
                 doi = "10.1080/01621459.1971.10482279", papel = "teoria"),
    deville = R(autores = c("Deville, J.-C.", "Särndal, C.-E."), ano = 1992,
                titulo = "Calibration Estimators in Survey Sampling",
                fonte = "Journal of the American Statistical Association, 87(418), 376-382",
                doi = "10.1080/01621459.1992.10475217", papel = "teoria"),
    deming = R(autores = c("Deming, W. E.", "Stephan, F. F."), ano = 1940,
               titulo = "On a Least Squares Adjustment of a Sampled Frequency Table When the Expected Marginal Totals are Known",
               fonte = "The Annals of Mathematical Statistics, 11(4)", doi = "10.1214/aoms/1177731829",
               papel = "teoria"),
    neyman = R(autores = "Neyman, J.", ano = 1934,
               titulo = "On the Two Different Aspects of the Representative Method: The Method of Stratified Sampling and the Method of Purposive Selection",
               fonte = "Journal of the Royal Statistical Society, 97(4), 558-625", doi = "10.2307/2342192",
               papel = "teoria"),
    thompson = R(autores = "Thompson, S. K.", ano = 1987, titulo = "Sample Size for Estimating Multinomial Proportions",
                 fonte = "The American Statistician, 41(1), 42-46", doi = "10.1080/00031305.1987.10475440",
                 papel = "teoria"),
    fleiss = R(autores = c("Fleiss, J. L.", "Levin, B.", "Paik, M. C."), ano = 2003,
               titulo = "Statistical Methods for Rates and Proportions", fonte = "3. ed. Hoboken: Wiley",
               doi = "10.1002/0471445428", papel = "livro-texto"),
    korn = R(autores = c("Korn, E. L.", "Graubard, B. I."), ano = 1999, titulo = "Analysis of Health Surveys",
             fonte = "New York: Wiley", doi = "10.1002/9781118032619", papel = "livro-texto"),
    wilson = R(autores = "Wilson, E. B.", ano = 1927,
               titulo = "Probable Inference, the Law of Succession, and Statistical Inference",
               fonte = "Journal of the American Statistical Association, 22(158), 209-212",
               doi = "10.1080/01621459.1927.10502953", papel = "teoria"),
    lumley = R(autores = "Lumley, T.", ano = 2004, titulo = "Analysis of Complex Survey Samples",
               fonte = "Journal of Statistical Software, 9(8)", doi = "10.18637/jss.v009.i08",
               papel = "complementar"),
    eldridge = R(autores = c("Eldridge, S. M.", "Ashby, D.", "Kerry, S."), ano = 2006,
                 titulo = "Sample size for cluster randomized trials: effect of coefficient of variation of cluster size and analysis method",
                 fonte = "International Journal of Epidemiology, 35(5), 1292-1300", doi = "10.1093/ije/dyl129",
                 papel = "teoria"),
    morris = R(autores = c("Morris, T. P.", "White, I. R.", "Crowther, M. J."), ano = 2019,
               titulo = "Using simulation studies to evaluate statistical methods",
               fonte = "Statistics in Medicine, 38(11)", doi = "10.1002/sim.8086", papel = "teoria")
  )
}

.tr_sampling_impl <- function(funcao, nota) {
  trama::tr_ref(papel = "implementacao", pacote = "trama.sampling", funcao = funcao, nota = nota)
}

.TR_SAMPLING_NOTA_MOTOR <- paste(
  "Motor próprio (não chama o survey): variância do conglomerado último com linearização de Taylor,",
  "Σ_h (1 − f_h)·n_h/(n_h − 1)·Σ_c (Z_hc − Z̄_h)², a mesma de survey::svydesign(ids, strata, fpc);",
  "correção finita só quando o desenho a declara (f_h = n_h/N_h de unidades primárias);",
  "intervalo t com gl = unidades primárias − estratos; domínio sem cortar a amostra.")

#' Pressupostos comuns às estimativas pelo desenho.
#' @noRd
.tr_sampling_press_estimar <- function() {
  PR <- trama::tr_pressuposto
  list(
    PR("A amostra é **probabilística**: toda unidade da população tinha probabilidade de inclusão conhecida e maior que zero, e o peso de cada uma é o inverso dela.",
       se_falhar = "Em coleta por adesão ou indicação, calibre os pesos a totais conhecidos (`sampling/rake`, `sampling/poststratify`) e leia o erro como nominal: o viés de quem entrou não está nele."),
    PR("Estratos, unidades primárias e pesos no cabo são os do **sorteio real**.",
       verificar = "sampling/design",
       se_falhar = "Declare o desenho verdadeiro em `sampling/design`: tratar conglomerados como unidades independentes subestima o erro padrão."),
    PR("Unidades primárias sorteadas **independentemente** entre estratos, com pelo menos duas por estrato; a variância do conglomerado último trata o primeiro estágio como com reposição (ou com a correção finita declarada).",
       se_falhar = "Junte estratos com uma unidade primária só; com fração amostral alta no primeiro estágio e sem `fpc`, o erro sai conservador."),
    PR("O intervalo é o da **aproximação normal** do estimador linearizado (t com gl do desenho): pede unidades primárias e casos no domínio em número razoável.",
       verificar = "sampling/simulate",
       se_falhar = "Confira a cobertura com `sampling/simulate`; com poucos graus de liberdade ou domínio raro, junte domínios ou leia o intervalo como aproximado."),
    PR("Faltantes são **ignoráveis**: dentro do desenho, quem não respondeu se parece com quem respondeu.",
       se_falhar = "Ajuste os pesos pela não resposta (`sampling/poststratify`, `sampling/rake`) ou impute antes de estimar.")
  )
}

.tr_sampling_refs_estimar <- function(funcao, extra = list()) {
  r <- .tr_sampling_refs()
  c(list(r$horvitz, r$woodruff), extra, list(r$cochran, r$bolfarine,
    .tr_sampling_impl(funcao, .TR_SAMPLING_NOTA_MOTOR)))
}

#' Pressupostos das contas de margem por fórmula (planejar e precisão).
#' @noRd
.tr_sampling_press_margem <- function(aas = "As entrevistas de cada unidade se comportam como uma **amostra aleatória** dela") {
  PR <- trama::tr_pressuposto
  list(
    PR(paste0(aas, "; em coleta por recrutamento, indicação ou adesão a margem é **nominal** (a de uma amostra aleatória com o mesmo tamanho efetivo), e o viés de quem aparece não entra nela."),
       se_falhar = "Cuide do viés no desenho da coleta e calibre depois com `sampling/rake`; a margem não o corrige."),
    PR("**Aproximação normal** da proporção: n·p e n·(1 − p) não muito pequenos (umas 30 respostas por célula ou mais).",
       se_falhar = "Junte células pequenas; abaixo disso a margem da fórmula não deve ir para o relatório."),
    PR("O **deff** informado é o do desenho real (ponderação e agrupamento); a fórmula não o estima.",
       verificar = "sampling/mean",
       se_falhar = "Use o deff de uma pesquisa anterior ou do piloto (o card de `sampling/mean` o mostra) e compare cenários.")
  )
}
