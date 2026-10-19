# Modelos de documento e a criação de um documento novo.
#
# Um modelo é uma extensão Quarto: `_extension.yml` (formatos), `_brand.yml`
# (cores, fontes, logo), os arquivos de que os formatos precisam e um
# `template.qmd` esqueleto. Os embutidos moram em `inst/modelos/`; os do
# usuário, em `tools::R_user_dir("trama", "data")/modelos/<nome>/`, e têm
# precedência (é assim que se edita um embutido: copia e muda).
#
# O documento novo leva uma cópia da extensão: renderiza sem o trama e no
# GitHub Actions, e mudar o modelo depois não muda documento já criado.

#' Modelos de documento disponíveis
#'
#' A galeria: os embutidos e os do usuário (que têm precedência sobre o
#' embutido de mesmo nome). Modelo inválido aparece, com `valido = FALSE` e o
#' `problema`, em vez de sumir ou quebrar a lista.
#'
#' @return data.frame com `nome`, `titulo`, `formatos` (separados por
#'   vírgula), `origem` (`"embutido"` ou `"usuario"`), `caminho`,
#'   `descricao`, `perfil` (o perfil de autor que o modelo sugere, ou `NA`),
#'   `sobre_embutido`, `valido` e `problema` (os problemas, separados por
#'   `"; "`, ou `NA`).
#' @seealso [tr_documento_modelo()] para um modelo em detalhe e
#'   [tr_documento_modelo_criar()] para criar, editar, duplicar e remover.
#' @export
tr_documento_modelos <- function() {
  dirs <- c(list.dirs(.td_dir_usuario("modelos"), recursive = FALSE),
            list.dirs(system.file("modelos", package = "trama.documentos"), recursive = FALSE))
  # Pasta oculta é cópia em andamento (`.td_instalar_pasta()`).
  dirs <- dirs[file.exists(file.path(dirs, "_extension.yml")) & !startsWith(basename(dirs), ".")]
  dirs <- dirs[!duplicated(basename(dirs))]
  q <- .td_quadro(lapply(dirs, function(d) {
    m <- .td_modelo_ler(d)
    list(nome = m$nome, titulo = m$titulo, formatos = paste(m$formatos, collapse = ", "),
         origem = .td_origem(d), caminho = d, descricao = m$descricao, perfil = m$perfil %||% NA_character_,
         sobre_embutido = .td_sobre_embutido(d), valido = !length(m$problemas),
         problema = if (length(m$problemas)) paste(m$problemas, collapse = "; ") else NA_character_)
  }), c("nome", "titulo", "formatos", "origem", "caminho", "descricao", "perfil", "sobre_embutido", "valido", "problema"))
  if (!nrow(q)) {
    q$sobre_embutido <- logical()
    q$valido <- logical()
  }
  q
}

#' O modelo para criar documento: tem de ser válido.
#' @noRd
.td_modelo <- function(nome) {
  d <- .td_modelo_dir(nome)
  m <- .td_modelo_ler(d)
  if (length(m$problemas)) {
    .td_erro(paste(c(sprintf("O modelo '%s' tem problemas; conserte (ou remova) e tente de novo:", nome),
                     paste("-", m$problemas)), collapse = "\n"), "tr_documento_erro_modelo")
  }
  m
}

#' Cria um documento a partir de um modelo e de um perfil
#'
#' Monta `documentos/<nome>/` no projeto: o `.qmd` (YAML com título, autor
#' do perfil, data, formatos do modelo e o fluxo de origem, mais o esqueleto
#' do modelo), a cópia da extensão em `_extensions/<modelo>/`, o `_brand.yml`
#' e um `_quarto.yml` que manda a saída do render para `_saida/`. É o único
#' momento em que o trama escreve fora das regiões.
#'
#' O YAML do esqueleto pode pedir campos do perfil: um valor
#' `"{{perfil.orientador}}"` vira o orientador do perfil (e some, se o perfil
#' não tem), e `"{{ano}}"` vira o ano corrente. É assim que o modelo UFLA
#' preenche orientador e curso.
#'
#' @param nome Nome do documento (letras, dígitos, `-`, `_`): pasta e
#'   arquivo `.qmd`.
#' @param projeto Pasta do projeto (ou um `trama::tr_project()`).
#' @param modelo Nome do modelo ([tr_documento_modelos()]).
#' @param perfil Nome do perfil de autor; `NULL`, o que o modelo sugere
#'   (se o perfil existe) ou o padrão (se houver); `FALSE`, sem autor.
#' @param titulo Título; `NULL` usa o nome.
#' @param fluxo Fluxo do projeto de onde vêm as regiões.
#' @return O caminho do `.qmd`.
#' @export
tr_documento_criar <- function(nome, projeto = ".", modelo = "basico", perfil = NULL, titulo = NULL,
                               fluxo = "main") {
  raiz <- if (inherits(projeto, "tr_project")) projeto$root else normalizePath(projeto, mustWork = FALSE)
  if (!file.exists(file.path(raiz, "trama.json"))) {
    .td_erro(sprintf("'%s' n\u{E3}o \u{E9} um projeto trama (falta o trama.json).", raiz), "tr_documento_erro_projeto")
  }
  if (!is.character(nome) || length(nome) != 1L || !grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", nome)) {
    .td_erro("O nome do documento usa letras, d\u{ED}gitos, '-' e '_' (ex.: 'relatorio-final').", "tr_documento_erro_nome")
  }
  pasta <- file.path(raiz, "documentos", nome)
  if (dir.exists(pasta) && length(list.files(pasta, all.files = TRUE, no.. = TRUE))) {
    .td_erro(sprintf("J\u{E1} existe o documento '%s' (%s).", nome, pasta), "tr_documento_erro_nome")
  }
  m <- .td_modelo(modelo)
  p <- if (isFALSE(perfil)) NULL else tr_documento_perfil(perfil %||% .td_perfil_sugerido(m))

  esqueleto <- .td_ler(file.path(m$dir, "template.qmd"))$linhas
  y <- .td_preencher(.td_yaml_listas(esqueleto), p) %||% list()
  f <- .td_fim_yaml(esqueleto)
  corpo <- if (f < length(esqueleto)) esqueleto[(f + 1L):length(esqueleto)] else character()
  y$title <- titulo %||% y$title %||% nome
  autor <- .td_autor_yaml(p)
  if (!is.null(autor)) y$author <- autor
  y$date <- y$date %||% "today"
  y$format <- stats::setNames(rep(list("default"), length(m$formatos)), paste0(m$nome, "-", m$formatos))
  y$trama <- Filter(Negate(is.null), list(fluxo = fluxo, modelo = m$nome, perfil = p$perfil))
  ordem <- c("title", "subtitle", "author", "date", setdiff(names(y), c("title", "subtitle", "author", "date", "format", "trama")),
             "format", "trama")
  y <- y[intersect(ordem, names(y))]

  dir.create(file.path(pasta, "_extensions", m$nome), recursive = TRUE, showWarnings = FALSE)
  arqs <- setdiff(list.files(m$dir, all.files = TRUE, no.. = TRUE, recursive = TRUE), c("template.qmd", "_brand.yml", .TD_META_MODELO))
  for (a in arqs) {
    dir.create(dirname(file.path(pasta, "_extensions", m$nome, a)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(m$dir, a), file.path(pasta, "_extensions", m$nome, a))
  }
  if (file.exists(file.path(m$dir, "_brand.yml"))) {
    .td_yaml_gravar(.td_brand_documento(.td_brand_ler(m$dir), m$nome), file.path(pasta, "_brand.yml"))
  }
  writeLines(c("project:", "  output-dir: _saida"), file.path(pasta, "_quarto.yml"))
  writeLines(c("/.quarto/", "/_saida/"), file.path(pasta, ".gitignore"))
  qmd <- file.path(pasta, paste0(nome, ".qmd"))
  writeLines(enc2utf8(c("---", .td_yaml_texto(y), "---", corpo)),
             qmd, useBytes = TRUE)
  qmd
}

#' YAML do esqueleto com lista que continua lista (`filters: [x.lua]` não
#' pode virar `filters: x.lua`; ver `.td_yaml_ler()`).
#' @noRd
.td_yaml_listas <- function(linhas) {
  f <- .td_fim_yaml(linhas)
  if (f < 2L) return(list())
  tryCatch(yaml::yaml.load(paste(.td_sem_cr(linhas[2:(f - 1L)]), collapse = "\n"),
                           handlers = list(seq = function(x) as.list(x))) %||% list(),
           error = function(e) list())
}

#' O perfil que o modelo sugere, se ele existe; senão, o padrão (`NULL`).
#' @noRd
.td_perfil_sugerido <- function(m) {
  if (!is.null(m$perfil) && m$perfil %in% names(tr_documento_perfis()$perfis)) m$perfil
}

#' Troca `"{{perfil.<campo>}}"` e `"{{ano}}"` no YAML do esqueleto. Campo que
#' o perfil não tem tira o item (numa lista, o elemento).
#' @noRd
.td_preencher <- function(x, p) {
  if (is.character(x) && length(x) == 1L) {
    k <- regmatches(x, regexec("^\\{\\{\\s*(perfil\\.[a-z_]+|ano)\\s*\\}\\}$", x))[[1]]
    if (!length(k)) return(x)
    if (k[[2]] == "ano") return(format(Sys.Date(), "%Y"))
    v <- p[[sub("^perfil\\.", "", k[[2]])]]
    return(if (is.character(v) && length(v) == 1L && nzchar(v)) v)
  }
  # Lista de textos no YAML (`instituicao: [UFLA, "{{perfil.curso}}"]`)
  # chega como vetor.
  if (is.character(x) && length(x) > 1L) {
    y <- unlist(lapply(x, .td_preencher, p = p))
    return(if (length(y)) y)
  }
  if (!is.list(x)) return(x)
  nomeada <- !is.null(names(x))
  y <- lapply(x, .td_preencher, p = p)
  vazio <- vapply(y, is.null, TRUE) & !vapply(x, is.null, TRUE)
  y <- y[!vazio]
  if (!nomeada) y <- unname(y)
  if (!length(y) && length(x)) NULL else y
}

#' O `_brand.yml` do documento: caminhos de arquivo (logo, fontes) passam a
#' apontar para a cópia da extensão em `_extensions/<modelo>/`.
#' @noRd
.td_brand_documento <- function(brand, modelo) {
  prefixo <- function(v) {
    if (is.character(v) && length(v) == 1L && grepl("\\.[A-Za-z0-9]+$", v) && !grepl("^(https?:|/)", v)) {
      paste0("_extensions/", modelo, "/", v)
    } else v
  }
  l <- brand$logo
  if (is.character(l)) brand$logo <- prefixo(l)
  if (is.list(l)) {
    if (length(l$images)) brand$logo$images <- lapply(l$images, function(v) if (is.list(v)) lapply(v, prefixo) else prefixo(v))
    for (k in intersect(c("small", "medium", "large"), names(l))) {
      v <- l[[k]]
      if (is.character(v) && is.null(l$images[[v]])) brand$logo[[k]] <- prefixo(v)
      if (is.list(v)) brand$logo[[k]] <- lapply(v, function(w) if (is.null(l$images[[w]])) prefixo(w) else w)
    }
  }
  if (length(brand$typography$fonts)) {
    brand$typography$fonts <- lapply(brand$typography$fonts, function(f) {
      if (is.list(f$files)) f$files <- lapply(f$files, function(x) if (is.list(x)) { x$path <- prefixo(x$path); x } else prefixo(x))
      f
    })
  }
  brand
}
