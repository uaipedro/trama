# Render local com o Quarto e a prévia servida.
#
# O render roda na pasta do documento, que é um projeto Quarto mínimo
# (`_quarto.yml` com `output-dir: _saida`): a saída não se mistura ao texto.
# PDF é Typst (sem TeX). A prévia é servida por HTTP local, e não aberta como
# arquivo, porque página com WebR (fase 4) não carrega de `file://`.

.TD_FORMATOS <- c(html = "html", pdf = "typst", docx = "docx")
.TD_EXTENSAO <- c(html = "html", pdf = "pdf", docx = "docx")

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
#' @param formato `"html"`, `"pdf"` (via Typst) ou `"docx"`.
#' @param tempo Segundos até desistir.
#' @return O caminho do arquivo gerado, em `_saida/`.
#' @export
tr_documento_renderizar <- function(documento, formato = c("html", "pdf", "docx"), tempo = 300) {
  formato <- match.arg(formato)
  r <- .td_render_preparar(documento, formato)
  out <- processx::run(r$quarto, r$args, wd = r$pasta, error_on_status = FALSE, timeout = tempo,
                       stderr_to_stdout = TRUE)
  if (isTRUE(out$timeout)) .td_erro(sprintf("O Quarto n\u{E3}o terminou em %d segundos.", tempo), "tr_documento_erro_render")
  .td_render_conferir(r, out$status, out$stdout)
}

#' O que o render precisa: Quarto, argumentos, pasta e o arquivo que sai.
#' Separado para o painel do editor rodar o mesmo render em segundo plano.
#' @noRd
.td_render_preparar <- function(documento, formato) {
  quarto <- .td_quarto()
  if (!nzchar(quarto)) {
    .td_erro(paste("O Quarto n\u{E3}o est\u{E1} instalado (ou fora do PATH). Instale pela p\u{E1}gina",
                   "https://quarto.org/docs/get-started/ e tente de novo."), "tr_documento_erro_quarto")
  }
  documento <- normalizePath(documento, mustWork = TRUE)
  pasta <- dirname(documento)
  alvo <- .td_formato_quarto(.td_ler(documento)$linhas, formato)
  list(quarto = quarto, args = c("render", basename(documento), "--to", alvo), pasta = pasta, formato = formato,
       saida = file.path(pasta, "_saida", paste0(tools::file_path_sans_ext(basename(documento)), ".",
                                                 .TD_EXTENSAO[[formato]])))
}

#' Deu certo? Devolve o arquivo gerado, ou erro com o fim do log.
#' @noRd
.td_render_conferir <- function(r, status, log) {
  if (!identical(as.integer(status), 0L) || !file.exists(r$saida)) {
    fim <- utils::tail(strsplit(log %||% "", "\n", fixed = TRUE)[[1]], 25L)
    .td_erro(paste(c(sprintf("O Quarto falhou ao renderizar em %s:", r$formato), fim), collapse = "\n"),
             "tr_documento_erro_render")
  }
  r$saida
}

#' Render em segundo plano: começa o processo e confere com `agendar` até
#' acabar; `pronto(saida)` ou `falhou(condição)` no fim. Não trava o editor.
#' @noRd
.td_render_fundo <- function(documento, formato, agendar, pronto, falhou, tempo = 300) {
  r <- tryCatch(.td_render_preparar(documento, formato), tr_documento_erro = function(e) e)
  if (inherits(r, "error")) return(falhou(r))
  log <- tempfile("render-", fileext = ".log")
  p <- processx::process$new(r$quarto, r$args, wd = r$pasta, stdout = log, stderr = "2>&1", cleanup = TRUE)
  inicio <- Sys.time()
  conferir <- function() {
    if (p$is_alive()) {
      if (difftime(Sys.time(), inicio, units = "secs") > tempo) {
        p$kill()
        return(falhou(rlang::catch_cnd(.td_erro(sprintf("O Quarto n\u{E3}o terminou em %d segundos.", tempo),
                                                "tr_documento_erro_render"))))
      }
      return(agendar(conferir, 0.5))
    }
    txt <- if (file.exists(log)) paste(readLines(log, warn = FALSE), collapse = "\n") else ""
    unlink(log)
    res <- tryCatch(.td_render_conferir(r, p$get_exit_status(), txt), tr_documento_erro = function(e) e)
    if (inherits(res, "error")) falhou(res) else pronto(res)
  }
  agendar(conferir, 0.5)
  invisible(p)
}

#' O nome do formato no Quarto: o do modelo, se o YAML o declara.
#' @noRd
.td_formato_quarto <- function(linhas, formato) {
  base <- .TD_FORMATOS[[formato]]
  declarados <- as.character(names(.td_yaml(linhas)$format))
  achado <- declarados[declarados == base | endsWith(declarados, paste0("-", base))]
  if (length(achado)) achado[[1]] else base
}

.td_previas <- new.env(parent = emptyenv())

#' Prévia do documento no navegador
#'
#' Renderiza em HTML e serve `_saida/` num servidor local
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
  html <- tr_documento_renderizar(documento, "html", tempo = tempo)
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
