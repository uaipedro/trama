# A ação "visualizar" do card (`tr_type(acoes = )` do núcleo): monta o
# pacote numa pasta do projeto, renderiza a página com o Quarto local, em
# segundo plano, e a serve pelo próprio editor. Fora da execução do fluxo:
# rodar nunca publica nem renderiza.

#' Os dados que o fluxo lê e que não foram ligados ao publicar: a descrição
#' (colunas, linhas) do resultado em cache e o sha256 do arquivo, quando o nó
#' lê um arquivo do projeto (`path`).
#' @noRd
.tr_publicar_dados_entrada <- function(doc, registry, store, raiz, ligados = character(), settings = NULL) {
  # Na raiz do projeto, como o editor: a impressão digital de quem lê arquivo
  # resolve o caminho relativo, e fora dela o plano não acharia o cache.
  antes <- setwd(raiz)
  on.exit(setwd(antes))
  plano <- tryCatch(trama::tr_plan(doc, registry = registry, store = store, settings = settings),
                    error = function(e) NULL)
  out <- list()
  for (id in names(doc$nodes)) {
    no <- doc$nodes[[id]]
    spec <- registry$nodes[[no$type]]
    if (is.null(spec) || length(spec$inputs) || id %in% ligados) next
    portas <- names(spec$outputs)[vapply(spec$outputs, function(p) identical(p$type, "data/table"), TRUE)]
    if (!length(portas)) next
    h <- plano$units[[id]]$handles[[portas[[1]]]]
    caminho <- no$params$path %||% no$params$caminho %||% ""
    arq <- if (is.character(caminho) && nzchar(caminho) && !grepl("^[a-z]+://", caminho)) {
      p <- if (grepl("^(/|[A-Za-z]:)", caminho)) caminho else file.path(raiz, caminho)
      if (file.exists(p) && !dir.exists(p)) p
    }
    out[[length(out) + 1L]] <- Filter(Negate(is.null), list(
      no = id, rotulo = no$label %||% spec$label, bloco = no$type,
      arquivo = if (!is.null(arq)) basename(arq) else if (nzchar(caminho)) caminho else NULL,
      sha256 = if (!is.null(arq)) .tr_publicar_sha256(arq) else NULL,
      bytes = if (!is.null(arq)) as.character(file.info(arq)$size) else NULL,
      linhas = h$summary$linhas,
      colunas = lapply(h$schema$colunas %||% list(), function(cl) list(nome = cl$nome, classe = cl$classe))))
  }
  out
}

#' Rótulo de cada nó para o diagrama: o do card, ou o do bloco.
#' @noRd
.tr_publicar_rotulos <- function(doc, registry) {
  lapply(doc$nodes, function(n) n$label %||% registry$nodes[[n$type]]$label %||% n$type)
}

#' Caminhos absolutos citados no fluxo: o `flow.json` vai junto, e eles dizem
#' o nome do usuário e a estrutura da máquina.
#' @noRd
.tr_publicar_caminhos_absolutos <- function(doc) {
  vals <- unlist(lapply(doc$nodes, function(n) Filter(is.character, n$params)), use.names = FALSE)
  unique(vals[grepl("^(/|~|[A-Za-z]:[\\\\/])", vals)])
}

.tr_publicar_quarto <- function() {
  q <- Sys.getenv("QUARTO_PATH", Sys.which("quarto"))
  if (nzchar(q) && file.exists(q)) q else ""
}

#' A ação: monta, renderiza (processo à parte, acompanhado por `later`) e
#' responde com a URL da prévia e a conferência.
#' @noRd
.tr_publicar_visualizar <- function(valor, ctx) {
  conf <- .tr_publicar_conferencia(valor)
  caminhos <- .tr_publicar_caminhos_absolutos(ctx$doc)
  if (length(caminhos)) {
    conf$avisos <- c(conf$avisos, list(sprintf(
      "O flow.json vai junto e cita caminhos absolutos da sua m\u{E1}quina: %s.", paste(caminhos, collapse = ", "))))
  }
  quarto <- .tr_publicar_quarto()
  if (!nzchar(quarto)) {
    return(list(conferencia = conf, guia = paste(
      "O Quarto n\u{E3}o est\u{E1} instalado (ou fora do PATH). Instale pela p\u{E1}gina",
      "https://quarto.org/docs/get-started/ e reabra o trama; o envio ao GitHub n\u{E3}o depende dele.")))
  }
  nome <- paste0("previa-", substr(gsub("[^A-Za-z0-9]", "", ctx$chave), 1L, 16L))
  dir <- file.path(ctx$raiz, ".trama", "publicar", nome)
  tr_publicar_montar(valor, dir, doc = ctx$doc, rotulos = .tr_publicar_rotulos(ctx$doc, ctx$registry),
                     dados_entrada = .tr_publicar_dados_entrada(ctx$doc, ctx$registry, ctx$store, ctx$raiz,
                                                                names(valor$itens), ctx$settings))
  log <- file.path(dir, "..", paste0(nome, ".log"))
  p <- processx::process$new(quarto, c("render", "index.qmd"), wd = dir, stdout = log, stderr = "2>&1",
                             cleanup = TRUE)
  inicio <- Sys.time()
  checar <- function() {
    if (p$is_alive()) {
      if (difftime(Sys.time(), inicio, units = "secs") > 300) {
        p$kill()
        ctx$responder(list(erro = "O Quarto n\u{E3}o terminou em 5 minutos.", conferencia = conf))
      } else later::later(checar, 0.5)
      return(invisible())
    }
    status <- p$get_exit_status()
    txt <- if (file.exists(log)) readLines(log, warn = FALSE) else character()
    if (!identical(status, 0L) || !file.exists(file.path(dir, "index.html"))) {
      ctx$responder(list(erro = paste(c("O Quarto falhou ao renderizar a p\u{E1}gina:", utils::tail(txt, 20L)),
                                      collapse = "\n"), conferencia = conf))
      return(invisible())
    }
    url <- ctx$servir(dir, nome)
    ctx$responder(list(url = paste0(url, "index.html"), pasta = normalizePath(dir), conferencia = conf,
                       arquivos = length(.tr_publicar_listar(dir))))
  }
  later::later(checar, 0.5)
  NULL
}
