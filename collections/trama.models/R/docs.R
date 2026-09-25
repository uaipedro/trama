# Pressupostos e referências dos blocos: o que cada método supõe, como
# conferir no trama e de onde ele vem.
#
# Um arquivo por grupo (`docs_*.R`), cada um devolvendo uma lista nomeada pelo
# id do nó. As referências foram conferidas na fonte (DOI no Crossref, livros no
# catálogo da editora ou de biblioteca); a de implementação aponta a função que
# o bloco REALMENTE chama.

.tr_models_P <- function(texto, verificar = NULL, se_falhar = NULL) {
  trama::tr_pressuposto(texto, verificar = verificar, se_falhar = se_falhar)
}

.tr_models_impl <- function(pacote, funcao, nota = NULL) {
  trama::tr_ref(papel = "implementacao", pacote = pacote, funcao = funcao, nota = nota)
}

#' Os livros-texto usados em mais de um bloco.
#' @noRd
.tr_models_livros <- function() {
  R <- trama::tr_ref
  list(
    montgomery = R(autores = "Montgomery, D. C.", ano = 2017,
                   titulo = "Design and Analysis of Experiments", fonte = "9. ed. Hoboken: Wiley",
                   papel = "livro-texto"),
    banzatto = R(autores = c("Banzatto, D. A.", "Kronka, S. N."), ano = 2006,
                 titulo = "Experimentação agrícola", fonte = "4. ed. Jaboticabal: Funep",
                 papel = "livro-texto"),
    pimentel = R(autores = "Pimentel-Gomes, F.", ano = 2009,
                 titulo = "Curso de estatística experimental", fonte = "15. ed. Piracicaba: FEALQ",
                 papel = "livro-texto"),
    siegel = R(autores = c("Siegel, S.", "Castellan, N. J."), ano = 2006,
               titulo = "Estatística não-paramétrica para ciências do comportamento",
               fonte = "2. ed. Porto Alegre: Artmed", papel = "livro-texto"),
    dobson = R(autores = c("Dobson, A. J.", "Barnett, A. G."), ano = 2008,
               titulo = "An Introduction to Generalized Linear Models",
               fonte = "3. ed. Boca Raton: Chapman & Hall/CRC", papel = "livro-texto"),
    rencher = R(autores = c("Rencher, A. C.", "Schaalje, G. B."), ano = 2008,
                titulo = "Linear Models in Statistics", fonte = "2. ed. Hoboken: Wiley",
                papel = "livro-texto"),
    searle = R(autores = "Searle, S. R.", ano = 1971, titulo = "Linear Models",
               fonte = "New York: Wiley", papel = "livro-texto"),
    fox = R(autores = c("Fox, J.", "Weisberg, S."), ano = 2019,
            titulo = "An R Companion to Applied Regression", fonte = "3. ed. Thousand Oaks: Sage",
            papel = "livro-texto")
  )
}

#' Pressupostos e referências de um nó (listas vazias se não houver).
#' @noRd
.tr_models_doc <- function(id) {
  todos <- c(.tr_models_docs_testes(), .tr_models_docs_anova())
  d <- todos[[id]]
  list(pressupostos = d$pressupostos %||% list(), referencias = d$referencias %||% list())
}
