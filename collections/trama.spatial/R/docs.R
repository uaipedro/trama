# Pressupostos e referências dos blocos, num lugar só.
#
# Conferido em 2026-10-06. Os três artigos: DOI no Crossref (título, periódico,
# volume e páginas). Os quatro livros: autor, ano, título, editora e edição no
# Open Library (registro por ISBN) e, onde há DOI de capítulo/livro, no Crossref:
# Cressie (ISBN 0471002550, "Rev. ed.", Wiley, 1993), Isaaks & Srivastava
# (ISBN 0195050126, Oxford University Press, 1989), Diggle & Ribeiro Jr.
# (ISBN 9780387329079, Springer Series in Statistics, 2007; Crossref
# 10.1007/978-0-387-48536-2) e Oliver & Webster (ISBN 9783319158648,
# SpringerBriefs in Agriculture, Cham, 2015; Crossref 10.1007/978-3-319-15865-5).
# Nenhum catálogo da editora foi consultado diretamente. Ressalva: o ANO de
# Isaaks & Srivastava (1989) NÃO foi confirmado; um registro do Open Library
# data uma impressão de janeiro de 1990, e 1989 é o ano de primeira publicação
# listado pelo outro registro e o citado na literatura.

.tr_spatial_refs <- function() {
  R <- trama::tr_ref
  list(
    matheron = R(autores = "Matheron, G.", ano = 1963, titulo = "Principles of Geostatistics",
                 fonte = "Economic Geology, 58(8), 1246-1266",
                 doi = "10.2113/gsecongeo.58.8.1246", papel = "teoria"),
    cressie_hawkins = R(autores = c("Cressie, N.", "Hawkins, D. M."), ano = 1980,
                        titulo = "Robust Estimation of the Variogram: I",
                        fonte = "Journal of the International Association for Mathematical Geology, 12(2), 115-125",
                        doi = "10.1007/BF01035243", papel = "teoria"),
    cressie = R(autores = "Cressie, N.", ano = 1993, titulo = "Statistics for Spatial Data",
                fonte = "Rev. ed. New York: Wiley", papel = "livro-texto"),
    isaaks = R(autores = c("Isaaks, E. H.", "Srivastava, R. M."), ano = 1989,
               titulo = "An Introduction to Applied Geostatistics",
               fonte = "New York: Oxford University Press", papel = "livro-texto"),
    diggle = R(autores = c("Diggle, P. J.", "Ribeiro Jr., P. J."), ano = 2007,
               titulo = "Model-based Geostatistics", fonte = "New York: Springer (Springer Series in Statistics)",
               papel = "livro-texto"),
    oliver = R(autores = c("Oliver, M. A.", "Webster, R."), ano = 2015,
               titulo = "Basic Steps in Geostatistics: The Variogram and Kriging",
               fonte = "Cham: Springer (SpringerBriefs in Agriculture)", papel = "livro-texto"),
    pebesma = R(autores = "Pebesma, E. J.", ano = 2004,
                titulo = "Multivariable Geostatistics in S: the gstat Package",
                fonte = "Computers & Geosciences, 30(7), 683-691",
                doi = "10.1016/j.cageo.2004.03.012", papel = "implementacao",
                pacote = "gstat", funcao = "variogram")
  )
}

.tr_spatial_press_coords <- function() {
  list(
    trama::tr_pressuposto(
      "As coordenadas estão **projetadas** e na mesma unidade nos dois eixos: a distância entre dois pontos é a euclidiana.",
      se_falhar = "Projete para um CRS métrico (o UTM da zona) antes e declare o código EPSG aqui."),
    trama::tr_pressuposto(
      "O **suporte amostral** é constante: todas as observações medem o mesmo volume ou área.",
      se_falhar = "Suporte diferente muda a variância e o patamar; agregue ao suporte comum antes de comparar."))
}

#' Monta a ajuda no formato das irmãs: Descrição, Parâmetros, Valor, Exemplos,
#' Veja também.
#' @noRd
.tr_spatial_ajuda <- function(descricao, parametros, valor, exemplos, veja) {
  paste0("## Descrição\n\n", trimws(descricao),
         "\n\n## Parâmetros\n\n", trimws(parametros),
         "\n\n## Valor\n\n", trimws(valor),
         "\n\n## Exemplos\n\n```r\n", trimws(exemplos), "\n```",
         "\n\n## Veja também\n\n", trimws(veja), "\n")
}

.tr_spatial_press_variograma <- function() {
  list(
    trama::tr_pressuposto(
      "**Hipótese intrínseca**: a esperança da diferença entre dois pontos é zero e a variância dessa diferença depende só do vetor que os separa, não de onde estão.",
      verificar = "spatial/explore",
      se_falhar = "Remova a tendência de larga escala no parâmetro 'Tendência removida' deste bloco."),
    trama::tr_pressuposto(
      "**Isotropia**, quando o variograma é omnidirecional: a dependência é a mesma em todas as direções.",
      verificar = "spatial/variogram",
      se_falhar = "Calcule o variograma em direções diferentes e compare o alcance de cada uma."),
    trama::tr_pressuposto(
      "A **escala da amostragem** alcança a escala da dependência: há pares a distâncias curtas o bastante para enxergar a estrutura.",
      se_falhar = "Variograma plano desde a primeira classe pode ser malha grossa demais, não ausência de estrutura."))
}
