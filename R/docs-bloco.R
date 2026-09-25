#' Texto "i18n-pronto": string única ou lista nomeada por idioma.
#'
#' Hoje quase tudo é escrito só em português, mas pressupostos e referências
#' vão para o editor E para o site — e o site terá versão em inglês. Aceitar
#' `list(pt = , en = )` desde já faz a tradução ser acréscimo de conteúdo, não
#' mudança de API em todas as coleções. A escolha cai para `pt` (a língua em
#' que o conteúdo nasce) e, na falta dela, para o primeiro idioma declarado:
#' um texto só em inglês ainda é melhor que nenhum.
#' @param x string única, ou lista nomeada de strings únicas.
#' @param lang código do idioma pedido.
#' @return string única.
#' @export
tr_text <- function(x, lang = getOption("trama.lang", "pt")) {
  .tr_check_text(x)
  if (!.tr_is_string(lang)) {
    rlang::abort("'lang' tem que ser uma string única, como \"pt\" ou \"en\".",
                 class = "tr_error_bad_text")
  }
  if (is.character(x)) return(x)
  # `[[` exato em nomes: sem casamento parcial ("en" não pega "english").
  if (lang %in% names(x)) return(x[[match(lang, names(x))]])
  if ("pt" %in% names(x)) return(x[[match("pt", names(x))]])
  x[[1L]]
}

.tr_is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))

#' Valida a forma de um texto i18n, sem resolvê-lo.
#' @noRd
.tr_check_text <- function(x, what = "texto") {
  ok <- .tr_is_string(x) ||
    (is.list(x) && length(x) > 0L && !is.null(names(x)) && all(nzchar(names(x))) && !anyNA(names(x)) &&
       !anyDuplicated(names(x)) &&
       all(vapply(x, .tr_is_string, logical(1))))
  if (!ok) {
    rlang::abort(sprintf(
      "%s tem que ser uma string única ou uma lista nomeada por idioma, como list(pt = , en = ).",
      what), class = "tr_error_bad_text")
  }
  invisible(x)
}

#' Pressuposto de um bloco estatístico.
#'
#' `verificar` aponta para BLOCOS (ids `"colecao/nome"`), não para funções R:
#' o que a ajuda oferece é "arraste este bloco para conferir", e só um id de
#' nó vira isso no editor. A existência do bloco não é checada aqui porque a
#' coleção que o verifica pode ser carregada depois (ou nem estar instalada).
#' @param texto o pressuposto (texto i18n, ver [tr_text()]).
#' @param verificar `NULL` ou vetor de ids de nó que ajudam a conferi-lo.
#' @param se_falhar o que fazer se ele não valer (texto i18n, opcional).
#' @export
tr_pressuposto <- function(texto, verificar = NULL, se_falhar = NULL) {
  .tr_check_text(texto, "'texto' do pressuposto")
  if (!is.null(verificar)) {
    if (!is.character(verificar) || length(verificar) == 0L) {
      rlang::abort("'verificar' tem que ser um vetor de ids de nó ('colecao/nome').",
                   class = "tr_error_bad_id")
    }
    for (v in verificar) .tr_check_id(v, "id de nó em 'verificar'")
  }
  if (!is.null(se_falhar)) .tr_check_text(se_falhar, "'se_falhar' do pressuposto")
  structure(list(texto = texto, verificar = verificar, se_falhar = se_falhar),
            class = "tr_pressuposto")
}

.tr_papeis_ref <- c("teoria", "livro-texto", "implementacao", "complementar")

#' Referência de um bloco.
#'
#' O `papel` diz POR QUE a fonte está ali: a teoria do método, o livro-texto
#' que o ensina, o pacote que o implementa. A de implementação é outra
#' espécie — o autor sai de `utils::citation(pacote)` e a versão é lida do
#' pacote instalado na hora do catálogo —, por isso ela exige `pacote` e
#' `funcao` e dispensa autores, ano e título.
#'
#' `doi` vai sem prefixo de URL para haver UMA forma dele: com e sem
#' `https://doi.org/` convivendo, o front teria que adivinhar qual montar.
#' @export
tr_ref <- function(autores = NULL, ano = NULL, titulo = NULL, fonte = NULL, doi = NULL,
                   url = NULL, papel = "teoria",
                   pacote = NULL, funcao = NULL, nota = NULL) {
  bad <- function(msg) rlang::abort(paste0("tr_ref(): ", msg), class = "tr_error_bad_docs")
  if (!.tr_is_string(papel) || !papel %in% .tr_papeis_ref) {
    bad(sprintf("'papel' tem que ser um de: %s.", paste(.tr_papeis_ref, collapse = ", ")))
  }
  # Autores, quando dados, valem a mesma regra em todo papel: a de
  # implementação dispensa, mas não aceita lixo.
  autores_ok <- is.character(autores) && length(autores) > 0L && !anyNA(autores) &&
    all(nzchar(trimws(autores)))
  if (!is.null(autores) && !autores_ok) {
    bad("'autores' tem que ser um vetor de strings não vazias.")
  }

  if (papel == "implementacao") {
    if (!.tr_is_string(pacote) || !.tr_is_string(funcao)) {
      bad("referência de implementação exige 'pacote' e 'funcao' (strings únicas).")
    }
  } else {
    if (is.null(autores)) {
      bad(sprintf("referência de papel '%s' exige 'autores'.", papel))
    }
    if (is.null(titulo)) bad(sprintf("referência de papel '%s' exige 'titulo'.", papel))
    if (is.null(ano)) bad(sprintf("referência de papel '%s' exige 'ano'.", papel))
  }
  if (!is.null(ano)) {
    if (!is.numeric(ano) || length(ano) != 1L || is.na(ano) || ano != round(ano) ||
        ano < 1000 || ano > 2100) {
      bad("'ano' tem que ser um inteiro entre 1000 e 2100.")
    }
    ano <- as.integer(ano)
  }
  if (!is.null(titulo)) .tr_check_text(titulo, "'titulo' da referência")
  if (!is.null(fonte)) .tr_check_text(fonte, "'fonte' da referência")
  if (!is.null(nota)) .tr_check_text(nota, "'nota' da referência")
  if (!is.null(doi) && !(.tr_is_string(doi) && grepl("^10\\.[0-9]{4,9}/\\S*[^[:space:].,;]$", doi))) {
    bad("'doi' tem que ser da forma '10.xxxx/...', sem prefixo de URL nem pontuação final.")
  }
  if (!is.null(url) && !(.tr_is_string(url) && grepl("^https://[^/[:space:]]+\\.[^/[:space:].]+(/\\S*)?$", url))) {
    bad("'url' tem que ser 'https://' seguido de um host com ponto.")
  }

  structure(list(papel = papel, autores = autores, ano = ano, titulo = titulo, fonte = fonte,
                 doi = doi, url = url, pacote = pacote, funcao = funcao, nota = nota),
            class = "tr_ref")
}
