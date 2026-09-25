# Pressupostos e referências dos blocos: o que cada método supõe, como
# conferir no trama e de onde ele vem.
#
# Um arquivo por grupo (`docs_*.R`), cada um devolvendo uma lista nomeada pelo
# id do nó. As referências foram conferidas na fonte (DOI no Crossref, livros no
# catálogo da editora ou de biblioteca); a de implementação aponta a função que
# o bloco REALMENTE chama.

.tr_multi_P <- function(texto, verificar = NULL, se_falhar = NULL) {
  trama::tr_pressuposto(texto, verificar = verificar, se_falhar = se_falhar)
}

.tr_multi_impl <- function(pacote, funcao, nota = NULL) {
  trama::tr_ref(papel = "implementacao", pacote = pacote, funcao = funcao, nota = nota)
}

#' Os livros-texto usados em mais de um bloco.
#' @noRd
.tr_multi_livros <- function() {
  R <- trama::tr_ref
  list(
    johnson = R(autores = c("Johnson, R. A.", "Wichern, D. W."), ano = 2007,
                titulo = "Applied Multivariate Statistical Analysis",
                fonte = "6. ed. Upper Saddle River: Pearson Prentice Hall", papel = "livro-texto"),
    mingoti = R(autores = "Mingoti, S. A.", ano = 2005,
                titulo = "Análise de dados através de métodos de estatística multivariada: uma abordagem aplicada",
                fonte = "Belo Horizonte: Editora UFMG", papel = "livro-texto"),
    ferreira = R(autores = "Ferreira, D. F.", ano = 2018, titulo = "Estatística multivariada",
                 fonte = "3. ed. Lavras: Editora UFLA", papel = "livro-texto"),
    hosmer = R(autores = c("Hosmer, D. W.", "Lemeshow, S.", "Sturdivant, R. X."), ano = 2013,
               titulo = "Applied Logistic Regression", fonte = "3. ed. Hoboken: Wiley",
               doi = "10.1002/9781118548387", papel = "livro-texto"),
    mass = R(autores = c("Venables, W. N.", "Ripley, B. D."), ano = 2002,
             titulo = "Modern Applied Statistics with S", fonte = "4. ed. New York: Springer",
             doi = "10.1007/978-0-387-21706-2", papel = "livro-texto"),
    efron_tib = R(autores = c("Efron, B.", "Tibshirani, R. J."), ano = 1993,
                  titulo = "An Introduction to the Bootstrap", fonte = "New York: Chapman & Hall",
                  doi = "10.1007/978-1-4899-4541-9", papel = "livro-texto")
  )
}

#' Pressupostos e referências de um nó (listas vazias se não houver).
#' @noRd
.tr_multi_doc <- function(id) {
  todos <- c(.tr_multi_docs_fatorial(), .tr_multi_docs_classificacao(),
             .tr_multi_docs_jackknife())
  d <- todos[[id]]
  list(pressupostos = if (is.null(d$pressupostos)) list() else d$pressupostos,
       referencias = if (is.null(d$referencias)) list() else d$referencias)
}
