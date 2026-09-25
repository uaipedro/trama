# Pressupostos e referências dos blocos: o que cada método supõe, como
# conferir no trama e de onde ele vem.
#
# Um arquivo por grupo (`docs_*.R`), cada um devolvendo uma lista nomeada pelo
# id do nó. As referências foram conferidas na fonte (DOI no Crossref, livros no
# catálogo da editora); a de implementação aponta a função que o bloco
# REALMENTE chama — ou a própria função da coleção, quando a conta é feita aqui.

.tr_series_P <- function(texto, verificar = NULL, se_falhar = NULL) {
  trama::tr_pressuposto(texto, verificar = verificar, se_falhar = se_falhar)
}

.tr_series_impl <- function(pacote, funcao, nota = NULL) {
  trama::tr_ref(papel = "implementacao", pacote = pacote, funcao = funcao, nota = nota)
}

#' Os livros-texto usados em mais de um bloco.
#' @noRd
.tr_series_livros <- function() {
  R <- trama::tr_ref
  list(
    morettin = R(autores = c("Morettin, P. A.", "Toloi, C. M. C."), ano = 2006,
                 titulo = "Análise de séries temporais", fonte = "2. ed. São Paulo: Blucher",
                 papel = "livro-texto"),
    fpp3 = R(autores = c("Hyndman, R. J.", "Athanasopoulos, G."), ano = 2021,
             titulo = "Forecasting: Principles and Practice",
             fonte = "3. ed. Melbourne: OTexts", url = "https://otexts.com/fpp3/",
             papel = "livro-texto"),
    box_jenkins = R(autores = c("Box, G. E. P.", "Jenkins, G. M.", "Reinsel, G. C.", "Ljung, G. M."),
                    ano = 2015, titulo = "Time Series Analysis: Forecasting and Control",
                    fonte = "5. ed. Hoboken: Wiley", papel = "livro-texto"),
    siegel = R(autores = c("Siegel, S.", "Castellan, N. J."), ano = 2006,
               titulo = "Estatística não-paramétrica para ciências do comportamento",
               fonte = "2. ed. Porto Alegre: Artmed", papel = "livro-texto")
  )
}

#' Pressupostos e referências de um nó (listas vazias se não houver).
#' @noRd
.tr_series_doc <- function(id) {
  todos <- c(.tr_series_docs_modelar())
  d <- todos[[id]]
  list(pressupostos = d$pressupostos %||% list(), referencias = d$referencias %||% list())
}
