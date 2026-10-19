# Publicar um documento: o que sai (metadados, regiões, avisos, bloqueios)
# e o registro das publicações do projeto.
#
# Portado do `trama.publicar` (`R/material.R`), que lia tudo dos params do
# nó `publicar/publish`. Agora a fonte é o documento: título, autores (com
# ORCID), resumo e palavras-chave vêm do YAML do Quarto; licenças, tipo da
# próxima versão, repositório privado e o aceite de dados de treino vêm de
# `trama: publicar:`. O repositório da publicação NÃO fica no documento (que
# vai no pacote): mora no registro do projeto, no `trama.json`.

.TD_LICENCAS <- c("CC-BY-4.0", "CC0-1.0", "CC-BY-SA-4.0", "CC-BY-NC-4.0")
.TD_LICENCAS_CODIGO <- c("MIT", "Apache-2.0", "GPL-3.0-only")
.TD_VERSOES <- c("menor", "corre\u{E7}\u{E3}o", "maior")

#' O que sairia na publicação de um documento
#'
#' Não toca no GitHub nem grava nada: lê o YAML e o estado das regiões e diz
#' o que vai no pacote, o que merece atenção (`avisos`) e o que impede a
#' publicação (`erros`).
#'
#' Do YAML do documento: `title`, `author` (nomes e, se houver, `orcid`,
#' conferido pelo dígito verificador), `description` (ou `abstract`) e
#' `keywords`. De `trama: publicar:`: `licenca` (do conteúdo, padrão
#' `CC-BY-4.0`), `licenca-codigo` (padrão `MIT`), `versao` (tipo da próxima:
#' `menor`, `correção` ou `maior`), `privado` (repositório da publicação
#' privado; Pages privado exige plano pago) e `aceitar-dados-treino` (publicar
#' modelos que carregam linhas do treino, que de outro modo são recusados).
#'
#' Bloqueiam: falta de título ou de autor, ORCID que não confere, licença
#' desconhecida, região desatualizada (atualize antes), região sem a pasta
#' materializada, modelo com dados de treino sem o aceite, arquivo acima de
#' 100 MB e documento sem formato HTML (a página do GitHub Pages).
#'
#' @inheritParams tr_documento_inserir
#' @return Objeto `tr_documento_publicacao`: lista com `documento`,
#'   `metadados`, `regioes`, `avisos` e `erros`.
#' @export
tr_documento_publicacao <- function(documento, projeto = NULL) {
  projeto <- .td_projeto(projeto, documento)
  d <- .td_ler(documento)
  y <- .td_yaml(d$linhas)
  m <- .td_pub_meta(y)
  pasta_doc <- dirname(normalizePath(documento))
  avisos <- character()
  erros <- m$erros
  if (!length(intersect(tr_documento_formatos(documento), c("html", "revealjs", "dashboard")))) {
    erros <- c(erros, "O documento n\u{E3}o tem formato HTML: a p\u{E1}gina do GitHub Pages precisa de um.")
  }
  e <- tr_documento_estado(documento, projeto)
  regs <- .td_regioes(d$linhas)
  regioes <- list()
  for (i in seq_len(nrow(e))) {
    chave <- e$regiao[[i]]
    est <- e$estado[[i]]
    if (est == "duplicada") next
    rot <- sprintf("#trama-%s", chave)
    if (est == "desatualizada") {
      erros <- c(erros, sprintf("%s est\u{E1} desatualizada: atualize o documento antes de publicar.", rot))
    }
    if (est == "conflito") avisos <- c(avisos, sprintf("%s foi editada \u{E0} m\u{E3}o: sai como est\u{E1} no texto.", rot))
    if (est == "orfa") avisos <- c(avisos, sprintf("%s \u{E9} de um bloco que saiu do fluxo: sai como est\u{E1}.", rot))
    if (est == "indisponivel") avisos <- c(avisos, sprintf("%s \u{E9} de uma cole\u{E7}\u{E3}o que n\u{E3}o carregou: sai como est\u{E1}.", rot))
    pasta <- .td_pasta_regiao(pasta_doc, chave)
    man <- .td_manifesto(pasta_doc, chave)
    if (!dir.exists(pasta) || is.null(man)) {
      erros <- c(erros, sprintf("%s n\u{E3}o tem a pasta _trama/%s/: atualize o documento.", rot, chave))
      next
    }
    arqs <- list.files(pasta, all.files = FALSE)
    tam <- file.info(file.path(pasta, arqs))$size
    grandes <- arqs[tam > 100 * 1024^2]
    if (length(grandes)) {
      erros <- c(erros, sprintf("%s: arquivo acima de 100 MB (%s), o GitHub recusa.", rot, paste(grandes, collapse = ", ")))
    }
    dt <- man$dados_treino
    if (!is.null(dt)) {
      if (m$aceitar_dados_treino) {
        avisos <- c(avisos, sprintf("%s \u{E9} publicada com dados de treino embutidos: %s", rot, dt))
      } else {
        erros <- c(erros, sprintf(paste0(
          "%s carrega linhas do treino: %s Publicar o modelo publica essas linhas. Para publicar assim, ",
          "ponha 'aceitar-dados-treino: true' em 'trama: publicar:' no YAML do documento."), rot, dt))
      }
    }
    completa <- grepl("(^|;)trama-tabela=completa(;|$)", man$opcoes %||% "")
    if (completa) avisos <- c(avisos, sprintf("%s leva a tabela inteira (trama-tabela=\"completa\"): ela fica p\u{FA}blica.", rot))
    regioes[[length(regioes) + 1L]] <- list(
      chave = chave, no = e$no[[i]], estado = est, tipo = man$tipo %||% NA_character_,
      titulo = man$titulo %||% chave, arquivos = as.list(arqs), bytes = sum(tam),
      dados_treino = dt, tabela_completa = completa)
  }
  # O texto da pessoa: chunk R dela roda no GitHub Actions, que só tem knitr.
  fora <- .td_sem_cr(.td_fora(d$linhas, regs))
  if (any(grepl("^\\s*```+\\s*\\{r[ ,}]", fora))) {
    avisos <- c(avisos, paste("O documento tem blocos de c\u{F3}digo R seus: o GitHub Actions s\u{F3} instala",
                              "knitr e rmarkdown, e um bloco que use outro pacote falha l\u{E1}."))
  }
  doc <- tryCatch(trama::tr_project_flow(projeto, .td_fluxo_do(d$linhas)), error = function(e) NULL)
  caminhos <- if (!is.null(doc)) .td_caminhos_absolutos(doc) else character()
  if (length(caminhos)) {
    avisos <- c(avisos, sprintf("O flow.json vai junto e cita caminhos absolutos da sua m\u{E1}quina: %s.",
                                paste(caminhos, collapse = ", ")))
  }
  structure(list(documento = normalizePath(documento), metadados = m[setdiff(names(m), "erros")],
                 regioes = regioes, avisos = avisos, erros = erros),
            class = "tr_documento_publicacao")
}

#' @export
print.tr_documento_publicacao <- function(x, ...) {
  m <- x$metadados
  cat(sprintf("<publica\u{E7}\u{E3}o> %s\n  %d regi\u{F5}es; licen\u{E7}a %s (c\u{F3}digo %s); pr\u{F3}xima vers\u{E3}o: %s\n",
              m$titulo, length(x$regioes), m$licenca, m$licenca_codigo, m$tipo_versao))
  for (a in x$avisos) cat("  aviso:", a, "\n")
  for (a in x$erros) cat("  bloqueia:", a, "\n")
  invisible(x)
}

# ---- metadados do YAML -------------------------------------------------------------

#' Metadados da publicação a partir do YAML do documento, com os erros.
#' @noRd
.td_pub_meta <- function(y) {
  pub <- if (is.list(y$trama) && is.list(y$trama$publicar)) y$trama$publicar else list()
  texto <- function(v) if (is.character(v) && length(v) && !is.na(v[[1]])) trimws(v[[1]]) else ""
  logico <- function(v) isTRUE(v) || identical(tolower(texto(v)), "true")
  titulo <- texto(y$title)
  descricao <- texto(y$description %||% y$abstract)
  chaves <- unlist(y$keywords %||% character())
  chaves <- trimws(unlist(strsplit(as.character(chaves), ",", fixed = TRUE)))
  autores <- .td_pub_autores(y$author)
  licenca <- texto(pub$licenca) %|v|% "CC-BY-4.0"
  licenca_codigo <- texto(pub[["licenca-codigo"]]) %|v|% "MIT"
  tipo_versao <- texto(pub$versao) %|v|% "menor"
  erros <- character()
  if (!nzchar(titulo)) erros <- c(erros, "Falta o t\u{ED}tulo (title no YAML do documento).")
  if (!length(autores)) erros <- c(erros, "Falta ao menos um autor (author no YAML do documento).")
  for (a in autores) if (isFALSE(a$orcid_valido)) {
    erros <- c(erros, sprintf("ORCID de %s n\u{E3}o confere (d\u{ED}gito verificador): %s.", a$nome, a$orcid))
  }
  if (!licenca %in% .TD_LICENCAS) {
    erros <- c(erros, sprintf("Licen\u{E7}a desconhecida: %s (use %s).", licenca, paste(.TD_LICENCAS, collapse = ", ")))
  }
  if (!licenca_codigo %in% .TD_LICENCAS_CODIGO) {
    erros <- c(erros, sprintf("Licen\u{E7}a de c\u{F3}digo desconhecida: %s (use %s).", licenca_codigo,
                              paste(.TD_LICENCAS_CODIGO, collapse = ", ")))
  }
  if (!tipo_versao %in% .TD_VERSOES) {
    erros <- c(erros, sprintf("Tipo de vers\u{E3}o desconhecido: %s (use %s).", tipo_versao, paste(.TD_VERSOES, collapse = ", ")))
  }
  list(titulo = titulo, descricao = descricao, palavras_chave = chaves[nzchar(chaves)], autores = autores,
       licenca = licenca, licenca_codigo = licenca_codigo, tipo_versao = tipo_versao,
       privado = logico(pub$privado), aceitar_dados_treino = logico(pub[["aceitar-dados-treino"]]),
       erros = erros)
}

`%|v|%` <- function(x, y) if (nzchar(x)) x else y

#' `author` do Quarto (texto, lista de textos ou de mapas com `name`, que pode
#' ser mapa com `given`/`family`/`literal`) -> `list(nome, orcid, orcid_valido)`.
#' @noRd
.td_pub_autores <- function(a) {
  if (is.null(a)) return(list())
  if (is.character(a)) a <- as.list(a)
  if (is.list(a) && !is.null(names(a)) && !is.null(a$name)) a <- list(a)
  out <- lapply(a, function(x) {
    nome <- if (is.character(x)) x[[1]] else {
      n <- x$name
      if (is.list(n)) n$literal %||% paste(c(n$given, n$family), collapse = " ") else as.character(n %||% "")
    }
    orcid <- if (is.list(x) && is.character(x$orcid)) {
      regmatches(x$orcid, regexpr("[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]", x$orcid))
    } else character()
    list(nome = trimws(nome), orcid = if (length(orcid)) orcid else NULL,
         orcid_valido = if (length(orcid)) .td_orcid_ok(orcid) else NA)
  })
  Filter(function(x) nzchar(x$nome), out)
}

#' Dígito verificador do ORCID (ISO 7064 11,2, como na documentação do ORCID).
#' @noRd
.td_orcid_ok <- function(orcid) {
  d <- strsplit(gsub("-", "", orcid), "")[[1]]
  total <- 0
  for (ch in d[1:15]) total <- (total + as.integer(ch)) * 2
  resto <- total %% 11
  esperado <- (12 - resto) %% 11
  identical(d[[16]], if (esperado == 10) "X" else as.character(esperado))
}

#' Próxima versão a partir da última publicada (`"0.0.0"` sem nenhuma).
#' @noRd
.td_proxima <- function(tipo, ultima = "0.0.0") {
  v <- as.integer(strsplit(ultima, ".", fixed = TRUE)[[1]])
  v <- switch(match(tipo, .TD_VERSOES),
              c(v[1], v[2] + 1L, 0L), c(v[1], v[2], v[3] + 1L), c(v[1] + 1L, 0L, 0L))
  paste(v, collapse = ".")
}

#' Maior versão `vX.Y.Z` entre as tags (`"0.0.0"` sem nenhuma).
#' @noRd
.td_ultima_versao <- function(tags) {
  v <- sub("^v", "", grep("^v[0-9]+\\.[0-9]+\\.[0-9]+$", tags, value = TRUE))
  if (!length(v)) return("0.0.0")
  as.character(max(numeric_version(v)))
}

.td_licenca_url <- function(spdx) paste0("https://spdx.org/licenses/", spdx)

#' Citação em texto corrido: Autores (ano). Título. Versão X. Repositório.
#' @noRd
.td_citacao <- function(m, versao, repositorio = "", data = Sys.Date()) {
  sprintf("%s (%s). %s. Vers\u{E3}o %s.%s", paste(vapply(m$autores, function(a) a$nome, ""), collapse = "; "),
          format(data, "%Y"), m$titulo, versao, if (nzchar(repositorio)) paste0(" ", repositorio, ".") else "")
}

#' O `_variables.yml` do documento: o que o esqueleto da página web cita com
#' `{{< var ... >}}`. Rascunho no documento; os valores da versão, no pacote.
#' @noRd
.td_variaveis <- function(y, versao, repositorio = "", data = Sys.Date()) {
  m <- .td_pub_meta(y)
  link <- function(l) sprintf("[%s](%s)", l, .td_licenca_url(l))
  list(versao = versao, licenca = link(m$licenca), "licenca-codigo" = link(m$licenca_codigo),
       citacao = .td_citacao(m, versao, repositorio, data))
}

#' Caminhos absolutos citados no fluxo: o `flow.json` vai junto, e eles dizem
#' o nome do usuário e a estrutura da máquina.
#' @noRd
.td_caminhos_absolutos <- function(doc) {
  vals <- unlist(lapply(doc$nodes, function(n) Filter(is.character, n$params)), use.names = FALSE)
  unique(vals[grepl("^(/|~|[A-Za-z]:[\\\\/])", vals)])
}

# ---- registro do projeto -----------------------------------------------------------

.TD_REGISTRO <- "publicacoes"

#' Publicações do projeto
#'
#' Em que repositório cada documento é publicado, a página, a última versão
#' enviada e quando. Mora no `trama.json` (chave `publicacoes`, pelo caminho
#' do documento no projeto): é configuração do PROJETO, não vai no documento
#' nem no pacote publicado. Estado do Actions e versões ao vivo se leem do
#' GitHub (`trama.github`); isto é só o endereço de cada publicação.
#'
#' @param projeto Pasta do projeto (ou um `trama::tr_project()`).
#' @return data.frame com `documento` (caminho relativo ao projeto), `repo`,
#'   `url`, `pagina` (base do Pages), `ultimo`, `versao`, `tag` e `data`;
#'   zero linhas sem publicação.
#' @export
tr_documento_publicacoes <- function(projeto = ".") {
  raiz <- if (inherits(projeto, "tr_project")) projeto$root else projeto
  pubs <- .td_registro(raiz)
  cols <- c("repo", "url", "pagina", "ultimo", "versao", "tag", "data")
  linhas <- lapply(names(pubs), function(k) {
    p <- pubs[[k]]
    c(list(documento = k), stats::setNames(lapply(cols, function(c) as.character(p[[c]] %||% NA_character_)), cols))
  })
  .td_quadro(linhas, c("documento", cols))
}

.td_registro <- function(raiz) {
  pubs <- tryCatch(trama::tr_project_config(raiz, .TD_REGISTRO), error = function(e) NULL) %||% list()
  # Só os de documento: chave antiga (id de nó do trama.publicar) é migrada
  # ao abrir o projeto (`tr_documento_migrar_publicar()`).
  if (!length(pubs)) return(list())
  pubs[startsWith(as.character(names(pubs)), "documentos/")]
}

#' Grava (ou atualiza) a publicação de um documento no `trama.json`.
#' @noRd
.td_registrar <- function(raiz, rel, campos) {
  if (!file.exists(file.path(raiz, "trama.json"))) return(invisible(NULL))
  pubs <- trama::tr_project_config(raiz, .TD_REGISTRO) %||% list()
  pubs[[rel]] <- utils::modifyList(pubs[[rel]] %||% list(), Filter(Negate(is.null), campos))
  trama::tr_project_config_set(raiz, .TD_REGISTRO, pubs)
  invisible(pubs[[rel]])
}
