# Render local com o Quarto e a prévia servida.
#
# O render roda na pasta do documento, que é um projeto Quarto mínimo
# (`_quarto.yml` com `output-dir: _saida`): a saída não se mistura ao texto.
# PDF é Typst (sem TeX). A prévia é servida por HTTP local, e não aberta como
# arquivo, porque página com WebR (fase 4) não carrega de `file://`.

.TD_FORMATOS <- c(html = "html", pdf = "typst", docx = "docx", revealjs = "revealjs", dashboard = "dashboard")
.TD_EXTENSAO <- c(html = "html", pdf = "pdf", docx = "docx", revealjs = "html", dashboard = "html")

.td_quarto <- function() {
  q <- Sys.getenv("QUARTO_PATH", Sys.which("quarto"))
  if (nzchar(q) && file.exists(q)) q else ""
}

#' Renderiza o documento com o Quarto
#'
#' Usa o formato do modelo que o YAML do documento declara
#' (`basico-typst` para `"pdf"`, por exemplo); sem ele, o formato puro do
#' Quarto. Não atualiza regiões: renderiza o que está no arquivo (chame
#' [tr_documento_atualizar()] antes, se quiser os resultados de agora).
#'
#' @inheritParams tr_documento_inserir
#' @param formato `"html"`, `"pdf"` (via Typst), `"docx"`, `"revealjs"`
#'   (slides) ou `"dashboard"`. Os que o documento declara:
#'   [tr_documento_formatos()].
#' @param tempo Segundos até desistir.
#' @return O caminho do arquivo gerado, em `_saida/`.
#' @export
tr_documento_renderizar <- function(documento, formato = c("html", "pdf", "docx", "revealjs", "dashboard"), tempo = 300) {
  formato <- match.arg(formato)
  quarto <- .td_quarto()
  if (!nzchar(quarto)) {
    .td_erro(paste("O Quarto n\u{E3}o est\u{E1} instalado (ou fora do PATH). Instale pela p\u{E1}gina",
                   "https://quarto.org/docs/get-started/ e tente de novo."), "tr_documento_erro_quarto")
  }
  documento <- normalizePath(documento, mustWork = TRUE)
  alvo <- .td_formato_quarto(.td_ler(documento)$linhas, formato)
  pasta <- dirname(documento)
  r <- processx::run(quarto, c("render", basename(documento), "--to", alvo), wd = pasta,
                     error_on_status = FALSE, timeout = tempo, stderr_to_stdout = TRUE)
  saida <- file.path(pasta, "_saida", paste0(tools::file_path_sans_ext(basename(documento)), ".", .TD_EXTENSAO[[formato]]))
  if (isTRUE(r$timeout)) .td_erro(sprintf("O Quarto n\u{E3}o terminou em %d segundos.", tempo), "tr_documento_erro_render")
  if (!identical(r$status, 0L) || !file.exists(saida)) {
    log <- utils::tail(strsplit(r$stdout, "\n", fixed = TRUE)[[1]], 25L)
    .td_erro(paste(c(sprintf("O Quarto falhou ao renderizar em %s:", formato), log), collapse = "\n"),
             "tr_documento_erro_render")
  }
  saida
}

#' O nome do formato no Quarto: o do modelo, se o YAML o declara.
#' @noRd
.td_formato_quarto <- function(linhas, formato) {
  base <- .TD_FORMATOS[[formato]]
  declarados <- as.character(names(.td_yaml(linhas)$format))
  achado <- declarados[declarados == base | endsWith(declarados, paste0("-", base))]
  if (length(achado)) achado[[1]] else base
}

#' Formatos de saída do documento
#'
#' Os formatos que o YAML do documento declara (vindos do modelo), nos nomes
#' de [tr_documento_renderizar()].
#'
#' @inheritParams tr_documento_inserir
#' @return Vetor de texto, como `c("html", "pdf", "docx")`.
#' @export
tr_documento_formatos <- function(documento) {
  declarados <- as.character(names(.td_yaml(.td_ler(documento)$linhas)$format))
  base <- sub("^.*-", "", declarados)
  names(.TD_FORMATOS)[.TD_FORMATOS %in% base]
}

.td_previas <- new.env(parent = emptyenv())

#' Prévia do documento no navegador
#'
#' Renderiza em HTML (ou nos slides ou no dashboard, se o documento não tem
#' HTML) e serve `_saida/` num servidor local
#' (`http://127.0.0.1:<porta>/`). O servidor fica de pé até
#' `tr_documento_previa_parar()` ou o fim da sessão do R; uma nova prévia do
#' mesmo documento reaproveita o servidor.
#'
#' @inheritParams tr_documento_renderizar
#' @param abrir Abrir no navegador.
#' @return Objeto `tr_documento_previa`: lista com `url`, `pasta` e
#'   `documento`.
#' @export
tr_documento_previa <- function(documento, abrir = interactive(), tempo = 300) {
  fmts <- intersect(tr_documento_formatos(documento), c("html", "revealjs", "dashboard"))
  html <- tr_documento_renderizar(documento, if (length(fmts)) fmts[[1]] else "html", tempo = tempo)
  pasta <- dirname(html)
  chave <- normalizePath(pasta)
  srv <- .td_previas[[chave]]
  if (is.null(srv)) {
    porta <- httpuv::randomPort(host = "127.0.0.1")
    s <- httpuv::startServer("127.0.0.1", porta, list(
      staticPaths = list("/" = httpuv::staticPath(pasta, indexhtml = FALSE, fallthrough = FALSE)),
      call = function(req) list(status = 404L, headers = list("Content-Type" = "text/plain"), body = "")))
    srv <- list(servidor = s, porta = porta)
    assign(chave, srv, envir = .td_previas)
  }
  url <- sprintf("http://127.0.0.1:%d/%s", srv$porta, basename(html))
  if (isTRUE(abrir)) utils::browseURL(url)
  structure(list(url = url, pasta = chave, documento = normalizePath(documento)), class = "tr_documento_previa")
}

#' @rdname tr_documento_previa
#' @param previa Uma prévia; `NULL` para todas. `tr_documento_previa_parar()`
#'   devolve, invisível, quantos servidores parou.
#' @export
tr_documento_previa_parar <- function(previa = NULL) {
  chaves <- intersect(if (is.null(previa)) ls(.td_previas) else previa$pasta, ls(.td_previas))
  for (k in chaves) {
    httpuv::stopServer(.td_previas[[k]]$servidor)
    rm(list = k, envir = .td_previas)
  }
  invisible(length(chaves))
}

#' @export
print.tr_documento_previa <- function(x, ...) {
  cat(sprintf("<pr\u{E9}via> %s\n  %s\n", x$url, x$documento))
  invisible(x)
}
