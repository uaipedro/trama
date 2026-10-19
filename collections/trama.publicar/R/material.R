# O que o nó `publicar` devolve: o MATERIAL do pacote (itens, metadados,
# layout, ambiente), puro e cacheável. Rodar o fluxo nunca publica nem grava
# fora do store; a pasta do pacote e a página saem na ação do card
# (`tr_publicar_montar()`), que soma o que só o editor sabe: o fluxo, o
# diagrama e a descrição dos dados de entrada.

.TR_PUBLICAR_LICENCAS <- c("CC-BY-4.0", "CC0-1.0", "CC-BY-SA-4.0", "CC-BY-NC-4.0")
.TR_PUBLICAR_LICENCAS_CODIGO <- c("MIT", "Apache-2.0", "GPL-3.0-only")
.TR_PUBLICAR_VERSOES <- c("menor", "corre\u{E7}\u{E3}o", "maior")

#' Autores no formato do param: `Nome Sobrenome (0000-0000-0000-0000)`,
#' separados por `;`. ORCID opcional; conferido pelo dígito verificador.
#' @noRd
.tr_publicar_autores <- function(texto) {
  partes <- trimws(strsplit(texto %||% "", ";", fixed = TRUE)[[1]])
  partes <- partes[nzchar(partes)]
  lapply(partes, function(p) {
    orcid <- regmatches(p, regexpr("[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]", p))
    nome <- trimws(gsub("\\(?\\s*(https?://orcid.org/)?[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]\\s*\\)?", "", p))
    list(nome = nome, orcid = if (length(orcid)) orcid else NULL,
         orcid_valido = if (length(orcid)) .tr_publicar_orcid_ok(orcid) else NA)
  })
}

#' Dígito verificador do ORCID (ISO 7064 11,2, como na documentação do ORCID).
#' @noRd
.tr_publicar_orcid_ok <- function(orcid) {
  d <- strsplit(gsub("-", "", orcid), "")[[1]]
  total <- 0
  for (ch in d[1:15]) total <- (total + as.integer(ch)) * 2
  resto <- total %% 11
  esperado <- (12 - resto) %% 11
  identical(d[[16]], if (esperado == 10) "X" else as.character(esperado))
}

#' Próxima versão a partir da última publicada (`"0.0.0"` sem nenhuma).
#' @noRd
.tr_publicar_proxima <- function(tipo, ultima = "0.0.0") {
  v <- as.integer(strsplit(ultima, ".", fixed = TRUE)[[1]])
  v <- switch(match(tipo, .TR_PUBLICAR_VERSOES),
              c(v[1], v[2] + 1L, 0L), c(v[1], v[2], v[3] + 1L), c(v[1] + 1L, 0L, 0L))
  paste(v, collapse = ".")
}

#' Layout da página: lê o param (JSON) e confere.
#'
#' Lista de blocos, na ordem da página: `{"tipo": "cabecalho"}`,
#' `{"tipo": "texto", "texto": "..."}` (markdown), `{"tipo": "item", "id":
#' "<nó>", "titulo": "...", "legenda": "..."}` e `{"tipo": "fluxo"}` (o
#' diagrama). Vazio é o automático: cabeçalho, cada item com componente na
#' ordem das arestas e o diagrama. Item ligado que o layout não cita vai só
#' para o pacote.
#' @param layout Texto JSON (o param) ou lista já lida.
#' @param ids Ids dos itens ligados, na ordem da porta.
#' @param com_componente Ids dos itens que têm componente de página.
#' @return `list(blocos, avisos)`.
#' @export
tr_publicar_layout <- function(layout, ids = character(), com_componente = ids) {
  avisos <- character()
  if (is.character(layout)) {
    layout <- trimws(paste(layout, collapse = "\n"))
    if (!nzchar(layout)) {
      blocos <- c(list(list(tipo = "cabecalho")),
                  lapply(intersect(ids, com_componente), function(i) list(tipo = "item", id = i)),
                  list(list(tipo = "fluxo")))
      return(list(blocos = blocos, avisos = avisos))
    }
    layout <- tryCatch(jsonlite::fromJSON(layout, simplifyVector = FALSE), error = function(e) {
      rlang::abort(paste0("O layout n\u{E3}o \u{E9} JSON v\u{E1}lido: ", conditionMessage(e)),
                   class = "tr_publicar_error_bad_layout")
    })
  }
  if (!is.list(layout) || !is.null(names(layout))) {
    rlang::abort("O layout \u{E9} uma lista de blocos (um array JSON).", class = "tr_publicar_error_bad_layout")
  }
  blocos <- list()
  for (b in layout) {
    tipo <- b$tipo %||% ""
    if (!tipo %in% c("cabecalho", "texto", "item", "fluxo")) {
      rlang::abort(sprintf("Bloco de layout com tipo desconhecido: '%s'.", tipo),
                   class = "tr_publicar_error_bad_layout")
    }
    if (tipo == "item" && !(b$id %||% "") %in% ids) {
      avisos <- c(avisos, sprintf("O layout cita '%s', que n\u{E3}o est\u{E1} ligado ao publicar.", b$id %||% ""))
      next
    }
    if (tipo == "item" && !b$id %in% com_componente) {
      avisos <- c(avisos, sprintf("'%s' n\u{E3}o tem componente de p\u{E1}gina: vai s\u{F3} no pacote.", b$id))
      next
    }
    blocos[[length(blocos) + 1L]] <- b
  }
  fora <- setdiff(ids, vapply(Filter(function(b) identical(b$tipo, "item"), blocos), function(b) b$id, ""))
  if (length(fora)) avisos <- c(avisos, sprintf("Fora da p\u{E1}gina (s\u{F3} no pacote): %s.", paste(fora, collapse = ", ")))
  list(blocos = blocos, avisos = avisos)
}

#' Versões de R, do trama, das coleções carregadas e dos pacotes que os
#' itens usam (os do bloco web).
#' @noRd
.tr_publicar_ambiente <- function(itens) {
  ver <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  colecoes <- sort(grep("^trama\\.", loadedNamespaces(), value = TRUE))
  pacotes <- unique(unlist(lapply(itens, function(it) names(it$componente$web$pacotes))))
  pacotes <- sort(setdiff(pacotes, "stats"))
  list(r = paste(R.version$major, R.version$minor, sep = "."),
       trama = ver("trama"),
       colecoes = as.list(stats::setNames(vapply(colecoes, ver, ""), colecoes)),
       pacotes = as.list(stats::setNames(vapply(pacotes, ver, ""), pacotes)))
}

#' Material do pacote: o que o nó `publicar` devolve
#'
#' Puro: depende só dos itens ligados e dos params. A data, o fluxo e os
#' dados de entrada entram na montagem ([tr_publicar_montar()]).
#' @param itens Lista nomeada (pelo nó de origem) de `tr_publicar_item`.
#' @param titulo,descricao,palavras_chave,autores,licenca,licenca_codigo,repositorio,versao,layout
#'   Os params do nó.
#' @param privado Repositório da publicação privado (o padrão é público; Pages
#'   privado exige plano pago do GitHub).
#' @param aceitar_dados_treino Publicar mesmo os itens marcados com
#'   `dados_treino` (modelo que carrega linhas do treino). Desligado, eles são
#'   recusados.
#' @return Objeto `tr_publicar_material`.
#' @export
tr_publicar_material <- function(itens = list(), titulo = "", descricao = "", palavras_chave = "",
                                 autores = "", licenca = "CC-BY-4.0", licenca_codigo = "MIT",
                                 repositorio = "", versao = "menor", layout = "",
                                 aceitar_dados_treino = FALSE, privado = FALSE) {
  if (length(itens) && is.null(names(itens))) names(itens) <- paste0("item", seq_along(itens))
  for (nm in names(itens)) if (!inherits(itens[[nm]], "tr_publicar_item")) {
    rlang::abort(sprintf("A entrada '%s' n\u{E3}o \u{E9} um publicar/item.", nm), class = "tr_publicar_error_bad_item")
  }
  avisos <- character(); erros <- character()
  if (!nzchar(trimws(titulo))) erros <- c(erros, "Falta o t\u{ED}tulo.")
  aut <- .tr_publicar_autores(autores)
  if (!length(aut)) erros <- c(erros, "Falta ao menos um autor.")
  for (a in aut) if (isFALSE(a$orcid_valido)) {
    erros <- c(erros, sprintf("ORCID de %s n\u{E3}o confere (d\u{ED}gito verificador): %s.", a$nome, a$orcid))
  }
  if (!licenca %in% .TR_PUBLICAR_LICENCAS) erros <- c(erros, sprintf("Licen\u{E7}a desconhecida: %s.", licenca))
  if (!licenca_codigo %in% .TR_PUBLICAR_LICENCAS_CODIGO) {
    erros <- c(erros, sprintf("Licen\u{E7}a de c\u{F3}digo desconhecida: %s.", licenca_codigo))
  }
  # Item que carrega linhas do treino: recusado sem o consentimento do param;
  # com ele, segue com aviso (e a marca vai para o RO-Crate na montagem).
  for (nm in names(itens)) {
    dt <- itens[[nm]]$dados_treino
    if (is.null(dt) || !is.null(itens[[nm]]$erro)) next
    if (isTRUE(aceitar_dados_treino)) {
      itens[[nm]]$avisos <- c(itens[[nm]]$avisos, paste("Publicado com dados de treino embutidos:", dt))
    } else {
      itens[[nm]]$erro <- paste0("O modelo carrega linhas do treino: ", dt, " Publicar o modelo publica essas ",
                                 "linhas. Para publicar assim, ligue 'Aceito publicar dados de treino ",
                                 "embutidos' no Publicar.")
    }
  }
  for (nm in names(itens)) {
    it <- itens[[nm]]
    if (!is.null(it$erro)) erros <- c(erros, sprintf("%s (%s): %s", it$titulo, nm, it$erro))
    if (length(it$avisos)) avisos <- c(avisos, sprintf("%s (%s): %s", it$titulo, nm, it$avisos))
    grandes <- names(it$arquivos)[vapply(it$arquivos, length, 1) > 100 * 1024^2]
    if (length(grandes)) erros <- c(erros, sprintf("%s (%s): arquivo acima de 100 MB (%s), o GitHub recusa.",
                                                  it$titulo, nm, paste(grandes, collapse = ", ")))
  }
  ok <- Filter(function(it) is.null(it$erro), itens)
  com_comp <- names(ok)[!vapply(ok, function(it) is.null(it$componente), TRUE)]
  lay <- tr_publicar_layout(layout, names(ok), com_comp)
  structure(list(
    formato = 1L,
    metadados = list(titulo = trimws(titulo), descricao = trimws(descricao),
                     palavras_chave = trimws(strsplit(palavras_chave %||% "", ",", fixed = TRUE)[[1]]) |>
                       (\(k) k[nzchar(k)])(),
                     autores = aut, licenca = licenca, licenca_codigo = licenca_codigo,
                     repositorio = trimws(repositorio), versao = .tr_publicar_proxima(versao),
                     tipo_versao = versao, privado = isTRUE(privado),
                     aceitar_dados_treino = isTRUE(aceitar_dados_treino)),
    layout = lay$blocos, itens = ok, ambiente = .tr_publicar_ambiente(ok),
    avisos = c(avisos, lay$avisos), erros = erros,
    recusados = names(itens)[!names(itens) %in% names(ok)]),
    class = "tr_publicar_material")
}

#' O card: conferência do que sairia (itens, tamanhos, avisos, erros).
#' @noRd
.tr_publicar_conferencia <- function(x) {
  itens <- lapply(names(x$itens), function(id) {
    it <- x$itens[[id]]
    list(id = id, tipo = it$tipo, titulo = it$titulo,
         bytes = sum(vapply(it$arquivos, length, 1)),
         arquivos = as.list(names(it$arquivos)),
         na_pagina = id %in% vapply(Filter(function(b) identical(b$tipo, "item"), x$layout), function(b) b$id, ""),
         avisos = as.list(it$avisos))
  })
  list(titulo = x$metadados$titulo, versao = x$metadados$versao, itens = itens,
       recusados = as.list(x$recusados), avisos = as.list(x$avisos), erros = as.list(x$erros),
       blocos = length(x$layout))
}
