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
#' @return data.frame com `nome`, `titulo`, `formatos` (separados por
#'   vírgula), `origem` (`"embutido"` ou `"usuario"`) e `caminho`.
#' @export
tr_documento_modelos <- function() {
  dirs <- c(list.dirs(.td_dir_usuario("modelos"), recursive = FALSE),
            list.dirs(system.file("modelos", package = "trama.documentos"), recursive = FALSE))
  dirs <- dirs[file.exists(file.path(dirs, "_extension.yml"))]
  dirs <- dirs[!duplicated(basename(dirs))]
  .td_quadro(lapply(dirs, function(d) {
    m <- .td_modelo_ler(d)
    list(nome = m$nome, titulo = m$titulo, formatos = paste(m$formatos, collapse = ", "),
         origem = if (startsWith(d, .td_dir_usuario())) "usuario" else "embutido", caminho = d)
  }), c("nome", "titulo", "formatos", "origem", "caminho"))
}

.td_modelo <- function(nome) {
  for (d in c(.td_dir_usuario("modelos", nome), system.file("modelos", nome, package = "trama.documentos"))) {
    if (nzchar(d) && file.exists(file.path(d, "_extension.yml"))) return(.td_modelo_ler(d))
  }
  .td_erro(sprintf("N\u{E3}o h\u{E1} modelo '%s'. Modelos: %s.", nome, paste(tr_documento_modelos()$nome, collapse = ", ")),
           "tr_documento_erro_modelo")
}

.td_modelo_ler <- function(d) {
  ext <- yaml::read_yaml(file.path(d, "_extension.yml"))
  list(nome = basename(d), dir = d, titulo = ext$title %||% basename(d),
       formatos = setdiff(names(ext$contributes$formats), "common"))
}

#' Cria um documento a partir de um modelo e de um perfil
#'
#' Monta `documentos/<nome>/` no projeto: o `.qmd` (YAML com título, autor
#' do perfil, data, formatos do modelo e o fluxo de origem, mais o esqueleto
#' do modelo), a cópia da extensão em `_extensions/<modelo>/`, o `_brand.yml`
#' e um `_quarto.yml` que manda a saída do render para `_saida/`. É o único
#' momento em que o trama escreve fora das regiões.
#'
#' @param nome Nome do documento (letras, dígitos, `-`, `_`): pasta e
#'   arquivo `.qmd`.
#' @param projeto Pasta do projeto (ou um `trama::tr_project()`).
#' @param modelo Nome do modelo ([tr_documento_modelos()]).
#' @param perfil Nome do perfil de autor; `NULL`, o padrão (se houver);
#'   `FALSE`, sem autor.
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
  p <- if (isFALSE(perfil)) NULL else tr_documento_perfil(perfil)

  esqueleto <- .td_ler(file.path(m$dir, "template.qmd"))$linhas
  y <- .td_yaml(esqueleto)
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
  arqs <- setdiff(list.files(m$dir, all.files = TRUE, no.. = TRUE, recursive = TRUE), c("template.qmd", "_brand.yml"))
  for (a in arqs) {
    dir.create(dirname(file.path(pasta, "_extensions", m$nome, a)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(m$dir, a), file.path(pasta, "_extensions", m$nome, a))
  }
  if (file.exists(file.path(m$dir, "_brand.yml"))) file.copy(file.path(m$dir, "_brand.yml"), file.path(pasta, "_brand.yml"))
  writeLines(c("project:", "  output-dir: _saida"), file.path(pasta, "_quarto.yml"))
  writeLines(c("/.quarto/", "/_saida/"), file.path(pasta, ".gitignore"))
  qmd <- file.path(pasta, paste0(nome, ".qmd"))
  writeLines(enc2utf8(c("---", sub("\n$", "", yaml::as.yaml(y, indent.mapping.sequence = TRUE)), "---", corpo)),
             qmd, useBytes = TRUE)
  qmd
}
