# Perfis de autor: quem escreve, guardado no nível do usuário (vale para
# todo projeto). Um perfil preenche o YAML do documento novo; o modelo usa o
# que precisar (orientador e logo ficam para os modelos que os mostram).
#
#   <R_user_dir("trama", "data")>/perfis.yml
#     padrao: ufla
#     perfis:
#       ufla: {nome: ..., email: ..., orcid: ..., instituicao: ..., curso: ...,
#              orientador: ..., logo: ...}

.TD_CAMPOS_PERFIL <- c("nome", "email", "orcid", "instituicao", "curso", "orientador", "logo")

.td_dir_usuario <- function(...) file.path(tools::R_user_dir("trama", "data"), ...)

#' Perfis de autor
#'
#' `tr_documento_perfis()` lê todos; `tr_documento_perfil()` devolve um (o
#' padrão, sem nome); `tr_documento_perfil_salvar()` cria ou substitui;
#' `tr_documento_perfil_remover()` apaga. Ficam em
#' `tools::R_user_dir("trama", "data")/perfis.yml`.
#'
#' @param perfil Nome do perfil (letras, dígitos, `-`, `_`), como
#'   `"aluno-ufla"`.
#' @param nome Nome da pessoa, como sai no documento.
#' @param email,orcid,instituicao,curso,orientador,logo Opcionais. O ORCID é
#'   conferido (formato e dígito verificador). `curso` vale também para cargo.
#'   `logo` é o caminho de uma imagem.
#' @param padrao `TRUE` torna este o perfil padrão. O primeiro perfil salvo
#'   vira padrão sozinho.
#' @return `tr_documento_perfis()`: lista com `padrao` (nome ou `NULL`) e
#'   `perfis` (lista nomeada). `tr_documento_perfil()`: o perfil (lista com
#'   `perfil` e os campos) ou `NULL`. As demais, invisível, o perfil salvo ou
#'   o nome removido.
#' @export
tr_documento_perfis <- function() {
  f <- .td_dir_usuario("perfis.yml")
  y <- if (file.exists(f)) tryCatch(yaml::read_yaml(f), error = function(e) {
    .td_erro(sprintf("N\u{E3}o foi poss\u{ED}vel ler '%s': %s", f, conditionMessage(e)), "tr_documento_erro_perfil")
  }) else list()
  perfis <- y$perfis %||% list()
  padrao <- y$padrao
  if (!is.null(padrao) && !padrao %in% names(perfis)) padrao <- NULL
  list(padrao = padrao, perfis = perfis)
}

#' @rdname tr_documento_perfis
#' @export
tr_documento_perfil <- function(perfil = NULL) {
  p <- tr_documento_perfis()
  perfil <- perfil %||% p$padrao
  if (is.null(perfil)) return(NULL)
  campos <- p$perfis[[perfil]]
  if (is.null(campos)) {
    .td_erro(sprintf("N\u{E3}o h\u{E1} perfil '%s'. Perfis: %s.", perfil,
                     if (length(p$perfis)) paste(names(p$perfis), collapse = ", ") else "nenhum"),
             "tr_documento_erro_perfil")
  }
  c(list(perfil = perfil), campos)
}

#' @rdname tr_documento_perfis
#' @export
tr_documento_perfil_salvar <- function(perfil, nome, email = NULL, orcid = NULL, instituicao = NULL, curso = NULL,
                                       orientador = NULL, logo = NULL, padrao = FALSE) {
  ruim <- function(msg) .td_erro(msg, "tr_documento_erro_perfil")
  if (!is.character(perfil) || length(perfil) != 1L || !grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", perfil)) {
    ruim("O nome do perfil usa letras, d\u{ED}gitos, '-' e '_' (ex.: 'aluno-ufla').")
  }
  if (!is.character(nome) || length(nome) != 1L || !nzchar(trimws(nome))) ruim("O perfil precisa do nome da pessoa.")
  if (!is.null(orcid) && !.td_orcid_valido(orcid)) {
    ruim(sprintf("ORCID '%s' inv\u{E1}lido: s\u{E3}o 16 d\u{ED}gitos em 4 grupos (0000-0002-1825-0097), o \u{FA}ltimo pode ser X.", orcid))
  }
  if (!is.null(email) && !grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", email, perl = TRUE)) ruim(sprintf("E-mail '%s' inv\u{E1}lido.", email))
  campos <- Filter(Negate(is.null), list(nome = trimws(nome), email = email, orcid = orcid, instituicao = instituicao,
                                         curso = curso, orientador = orientador, logo = logo))
  campos <- lapply(campos, as.character)
  p <- tr_documento_perfis()
  p$perfis[[perfil]] <- campos
  if (isTRUE(padrao) || is.null(p$padrao)) p$padrao <- perfil
  .td_gravar_perfis(p)
  invisible(c(list(perfil = perfil), campos))
}

#' @rdname tr_documento_perfis
#' @export
tr_documento_perfil_remover <- function(perfil) {
  p <- tr_documento_perfis()
  if (is.null(p$perfis[[perfil]])) .td_erro(sprintf("N\u{E3}o h\u{E1} perfil '%s'.", perfil), "tr_documento_erro_perfil")
  p$perfis[[perfil]] <- NULL
  if (identical(p$padrao, perfil)) p$padrao <- if (length(p$perfis)) names(p$perfis)[[1]] else NULL
  .td_gravar_perfis(p)
  invisible(perfil)
}

.td_gravar_perfis <- function(p) {
  f <- .td_dir_usuario("perfis.yml")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  y <- Filter(Negate(is.null), list(padrao = p$padrao, perfis = p$perfis))
  if (!length(y$perfis)) y$perfis <- NULL
  tmp <- tempfile(tmpdir = dirname(f), fileext = ".tmp")
  writeLines(enc2utf8(yaml::as.yaml(y)), tmp, useBytes = TRUE)
  if (!file.rename(tmp, f)) .td_erro(sprintf("N\u{E3}o foi poss\u{ED}vel gravar '%s'.", f), "tr_documento_erro_perfil")
  invisible(f)
}

#' ORCID: 4 grupos de 4, dígito verificador ISO 7064 MOD 11-2.
#' @noRd
.td_orcid_valido <- function(x) {
  if (!is.character(x) || length(x) != 1L || !grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[\\dX]$", x, perl = TRUE)) return(FALSE)
  d <- strsplit(gsub("-", "", x), "")[[1]]
  total <- 0
  for (c in d[1:15]) total <- (total + as.integer(c)) * 2
  r <- (12 - total %% 11) %% 11
  identical(d[[16]], if (r == 10) "X" else as.character(r))
}

#' O autor no YAML do Quarto (`author:` com `affiliations`).
#' @noRd
.td_autor_yaml <- function(p) {
  if (is.null(p)) return(NULL)
  afil <- if (!is.null(p$instituicao)) list(Filter(Negate(is.null), list(name = p$instituicao, department = p$curso)))
  list(Filter(Negate(is.null), list(name = p$nome, email = p$email, orcid = p$orcid, affiliations = afil)))
}
