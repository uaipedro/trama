#' Galeria do projeto: os cards marcados, em qualidade de publicação, numa
#' pasta do projeto, sempre na versão atual.
#'
#' Um item é um card na galeria. Entra por marca explícita (`doc$ui$galeria`)
#' ou, sem marca, pela regra do projeto: resultado com renderer `trama/image`
#' e `galeria$imagens` ligado. Dois tipos de item:
#'   - `imagem`: o núcleo exporta sozinho, re-renderizando o preview em alta
#'     qualidade (`ctx$qualidade`);
#'   - `captura`: outro renderer (tabela, texto); só o navegador sabe desenhar,
#'     então ele manda o PNG via `tr_galeria_captura()`.
#'
#' Um arquivo por card, com nome estável (rótulo em ASCII). A pasta tem
#' `.galeria.json`, o manifesto do que a galeria escreveu: só esses arquivos
#' são reescritos ou removidos. Arquivo alheio na pasta nunca é tocado, e um
#' nome ocupado por ele cede lugar a um sufixo com o id do nó.
#'
#' Todas as funções recebem `root` (raiz do projeto), `store`, `registry`,
#' `doc`, `settings` (de `.tr_settings()`) e `handles` (lista nó -> handle).
#' @name galeria
#' @keywords internal
NULL

#' Itens da galeria, em ordem de fluxo, sem exportar nada.
#'
#' Ordem: topológica pelas arestas; empate pela posição x do card, depois pelo
#' id (para não depender da ordem de inserção). Cada item traz `node`,
#' `rotulo`, `arquivo`, `key`, `tipo` (`"imagem"` ou `"captura"`) e `nome`
#' (base do arquivo, sem extensão). Um nó marcado TRUE sem resultado válido
#' vira captura com `key = NULL`.
#' @param root Raiz do projeto; a pasta da galeria é `<root>/<galeria$pasta>`.
#' @param store Store do projeto (só a exportação o usa; aqui fica pela forma comum).
#' @param registry Registro que resolve os tipos.
#' @param doc Documento.
#' @param settings Settings do projeto; `NULL` usa os padrões.
#' @param handles Lista nó -> handle, como `tr_store_put()` grava.
#' @return Lista de itens (listas nomeadas).
#' @export
tr_galeria_itens <- function(root, store, registry, doc, settings = NULL, handles = list()) {
  g <- .tr_galeria_cfg_de(settings)
  manifesto <- .tr_galeria_ler(file.path(root, g$pasta))
  itens <- .tr_galeria_itens_brutos(root, registry, doc, g, handles, manifesto)
  lapply(itens, function(it) { it$handle <- NULL; it })
}

#' Sincroniza a pasta da galeria com o projeto.
#'
#' Exporta os itens-imagem cuja assinatura (`key` + formato + dpi) mudou ou
#' que não têm arquivo; remove os arquivos do manifesto que saíram da galeria
#' ou trocaram de nome; mantém os itens-captura que já têm PNG. Erro de
#' exportação de um item fica em `erro` e não derruba os outros.
#' @inheritParams tr_galeria_itens
#' @return Itens (como em [tr_galeria_itens()]) com `versao` (assinatura) e
#'   `pronto` (o arquivo existe).
#' @export
tr_galeria_sincronizar <- function(root, store, registry, doc, settings = NULL, handles = list()) {
  g <- .tr_galeria_cfg_de(settings)
  dir <- file.path(root, g$pasta)
  manifesto <- .tr_galeria_ler(dir)
  itens <- .tr_galeria_itens_brutos(root, registry, doc, g, handles, manifesto)
  mantidos <- list()
  for (i in seq_along(itens)) {
    it <- itens[[i]]
    prev <- .tr_galeria_entrada(manifesto, it$node)
    it$versao <- .tr_galeria_versao(it$key, g)
    alvo <- file.path(dir, it$arquivo)
    if (it$tipo == "imagem") {
      if (!is.null(prev) && identical(prev$versao, it$versao) && identical(prev$arquivo, it$arquivo) &&
          file.exists(alvo)) {
        it$pronto <- TRUE
        mantidos[[length(mantidos) + 1L]] <- .tr_galeria_linha(it)
      } else {
        exportado <- tryCatch(.tr_galeria_exporta(store, registry, settings, it, g, dir),
                              error = function(e) e)
        if (inherits(exportado, "error")) {
          it$erro <- conditionMessage(exportado)
          it$pronto <- FALSE
          # Falha mantém o arquivo anterior (se havia): quem perde a versão
          # velha é a próxima tentativa bem-sucedida, não esta.
          if (!is.null(prev)) mantidos[[length(mantidos) + 1L]] <- prev
        } else {
          it$arquivo <- exportado
          it$pronto <- TRUE
          mantidos[[length(mantidos) + 1L]] <- .tr_galeria_linha(it)
        }
      }
    } else {
      # Captura: o PNG veio do navegador. Resultado novo (outra key) deixa a
      # captura velha no disco, mas `pronto = FALSE` pede outra ao editor —
      # sem isso a tabela da galeria ficaria presa na primeira versão.
      existe <- !is.null(prev) && identical(prev$arquivo, it$arquivo) && file.exists(alvo)
      it$pronto <- existe && identical(prev$versao, it$versao)
      if (existe) mantidos[[length(mantidos) + 1L]] <- prev
    }
    itens[[i]] <- it
  }
  if (length(itens) || length(manifesto)) {
    .tr_galeria_limpa(dir, manifesto, mantidos)
    if (length(mantidos) || file.exists(file.path(dir, ".galeria.json")))
      .tr_galeria_escreve(dir, mantidos)
  }
  lapply(itens, function(it) { it$handle <- NULL; it })
}

#' Grava o PNG de uma captura de navegador como item da galeria.
#'
#' O nó tem que estar na galeria como `captura`. `png` é o PNG em base64, cru
#' ou como data URL (`data:image/png;base64,...`). Substitui o arquivo anterior
#' do nó, se o nome mudou, e atualiza o manifesto.
#' @inheritParams tr_galeria_itens
#' @param node Id do nó.
#' @param key Chave do resultado que a captura representa.
#' @param png PNG em base64 (cru ou data URL).
#' @return O item gravado, com `pronto = TRUE`.
#' @export
tr_galeria_captura <- function(root, store, registry, doc, settings = NULL, handles = list(),
                               node, key, png) {
  bytes <- .tr_galeria_png_bytes(png)
  g <- .tr_galeria_cfg_de(settings)
  dir <- file.path(root, g$pasta)
  manifesto <- .tr_galeria_ler(dir)
  itens <- .tr_galeria_itens_brutos(root, registry, doc, g, handles, manifesto)
  idx <- which(vapply(itens, function(it) identical(it$node, node), logical(1)))
  if (!length(idx)) rlang::abort(sprintf("Nó '%s' não está na galeria.", node), class = "tr_error_galeria")
  it <- itens[[idx]]
  if (it$tipo != "captura") {
    rlang::abort(sprintf("Nó '%s' sai da galeria pelo próprio renderer (imagem); captura não se aplica.", node),
                 class = "tr_error_galeria")
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  .tr_galeria_mover_bytes(bytes, file.path(dir, it$arquivo))
  it$key <- key
  it$versao <- .tr_galeria_versao(key, g)
  it$pronto <- TRUE
  # Nome antigo do mesmo nó: sai, se for outro arquivo que o manifesto conhecia.
  antigos <- Filter(function(e) identical(e$node, node), manifesto)
  for (e in antigos) if (!identical(e$arquivo, it$arquivo)) .tr_galeria_apaga(dir, e$arquivo)
  mantidos <- c(Filter(function(e) !identical(e$node, node), manifesto), list(.tr_galeria_linha(it)))
  .tr_galeria_escreve(dir, mantidos)
  it$handle <- NULL
  it
}

# --- Itens ------------------------------------------------------------------

#' Itens com nomes resolvidos, sem exportar (base de sync e captura).
#' @noRd
.tr_galeria_itens_brutos <- function(root, registry, doc, g, handles, manifesto) {
  dir <- file.path(root, g$pasta)
  itens <- list()
  for (id in .tr_galeria_ordem(doc)) {
    h <- handles[[id]]
    if (!.tr_galeria_na_galeria(doc, id, h, g$imagens)) next
    ok <- !is.null(h) && is.null(h$error)
    rotulo <- doc$nodes[[id]]$label %||% doc$nodes[[id]]$type
    if (!is.character(rotulo) || !nzchar(rotulo)) rotulo <- doc$nodes[[id]]$type
    itens[[length(itens) + 1L]] <- list(
      node = id, rotulo = rotulo,
      nome = .tr_galeria_slug(rotulo, fallback = .tr_galeria_slug(id, "card")),
      key = if (ok) h$key else NULL,
      tipo = if (.tr_galeria_eh_imagem(h)) "imagem" else "captura",
      erro = if (!is.null(h) && !ok) h$error$message else NULL,
      handle = h
    )
  }
  .tr_galeria_nomes(itens, dir, manifesto, g)
}

#' Atribui `arquivo` a cada item.
#'
#' Base = rótulo em ASCII. Duas bases iguais na mesma galeria viram `base-id`
#' para os dois. Nome ocupado por arquivo que o manifesto não conhece (alheio)
#' também cede a `base-id` e, se ainda ocupado, a `base-id-N`.
#' @noRd
.tr_galeria_nomes <- function(itens, dir, manifesto, g) {
  if (!length(itens)) return(itens)
  bases <- vapply(itens, function(it) it$nome, "")
  repetidas <- bases %in% bases[duplicated(bases)]
  usados <- character()
  for (i in seq_along(itens)) {
    it <- itens[[i]]
    ext <- if (it$tipo == "imagem") .tr_galeria_ext(g$formato) else "png"
    dono <- .tr_galeria_entrada(manifesto, it$node)
    proprio <- function(arq) !is.null(dono) && identical(dono$arquivo, arq)
    sufixo <- paste0(it$nome, "-", .tr_galeria_slug(it$node, "card"))
    # Ordem de tentativa: base, base-id, base-id-2, base-id-3...
    candidatos <- c(if (!repetidas[[i]]) it$nome, sufixo, paste0(sufixo, "-", 2:999))
    arq <- NULL
    for (nome in candidatos) {
      cand <- paste0(nome, ".", ext)
      ocupado <- cand %in% usados || (file.exists(file.path(dir, cand)) && !proprio(cand))
      if (!ocupado) { arq <- cand; break }
    }
    if (is.null(arq)) rlang::abort("Não achei nome livre para o item da galeria.", class = "tr_error_galeria")
    usados <- c(usados, arq)
    it$nome <- sub(paste0("\\.", ext, "$"), "", arq)
    it$arquivo <- arq
    itens[[i]] <- it
  }
  itens
}

#' Pode ser exportado no servidor: o renderer é o de imagem (PNG do `trama.view`).
#' @noRd
.tr_galeria_eh_imagem <- function(h) {
  !is.null(h) && is.null(h$error) && identical(h$preview$renderer, "trama/image")
}

#' Membro da galeria: a marca do usuário vence; sem marca, a regra do projeto.
#' @noRd
.tr_galeria_na_galeria <- function(doc, id, h, imagens) {
  v <- doc$ui$galeria[[id]]
  if (!is.null(v)) return(isTRUE(v))
  isTRUE(imagens) && .tr_galeria_eh_imagem(h)
}

#' Ordem do fluxo: topológica pelas arestas, empate pela posição x.
#' @noRd
.tr_galeria_ordem <- function(doc) {
  nos <- names(doc$nodes) %||% character()
  if (!length(nos)) return(character())
  preds <- stats::setNames(lapply(nos, function(id) character()), nos)
  for (e in doc$edges %||% list()) {
    a <- e$from$node; b <- e$to$node
    if (a %in% nos && b %in% nos && !identical(a, b)) preds[[b]] <- union(preds[[b]], a)
  }
  xs <- stats::setNames(vapply(nos, function(id) {
    p <- unlist(doc$ui$positions[[id]])
    if (is.numeric(p) && length(p)) as.numeric(p[[1]]) else Inf
  }, numeric(1)), nos)
  ordem <- character()
  restante <- nos
  while (length(restante)) {
    prontos <- restante[vapply(restante, function(id) all(preds[[id]] %in% ordem), logical(1))]
    # Ciclo não entra no documento (`tr_doc_apply` recusa); se entrar, não trava.
    if (!length(prontos)) prontos <- restante
    prox <- prontos[order(xs[prontos], prontos)][[1]]
    ordem <- c(ordem, prox)
    restante <- setdiff(restante, prox)
  }
  ordem
}

# --- Exportação ----------------------------------------------------------------

#' Re-renderiza o preview em alta qualidade e copia o arquivo para a pasta.
#'
#' Chama o MESMO `preview` do tipo, com `ctx$qualidade`. Tipo com tema recebe
#' os temas do projeto, como `tr_store_put()` faria. Devolve o nome final
#' (a extensão é a do arquivo que o preview produziu).
#' @noRd
.tr_galeria_exporta <- function(store, registry, settings, it, g, dir) {
  h <- it$handle
  tipo <- tr_get_type(h$type, registry)
  if (is.null(tipo$preview)) rlang::abort(sprintf("Tipo '%s' não tem preview.", h$type), class = "tr_error_galeria")
  valor <- tr_store_get(store, h$key, tipo)
  tmp <- tempfile("galeria-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  ctx <- list(
    file = function(ext) file.path(tmp, paste0("arte.", ext)),
    qualidade = list(dpi = g$dpi, formato = g$formato)
  )
  tema <- .tr_preview_tema(h$type, registry, settings)
  if (!is.null(tema)) {
    velho <- options(trama.preview_settings = tema)
    on.exit(options(velho), add = TRUE)
  }
  art <- tipo$preview(valor, ctx)
  origem <- if (is.null(art$files)) NULL else unlist(art$files, use.names = FALSE)
  if (!length(origem) || !file.exists(origem[[1]])) {
    rlang::abort(sprintf("Preview de '%s' não produziu arquivo.", it$node), class = "tr_error_galeria")
  }
  origem <- origem[[1]]
  ext <- tolower(tools::file_ext(origem))
  if (!nzchar(ext)) ext <- .tr_galeria_ext(g$formato)
  arq <- paste0(it$nome, ".", ext)
  .tr_galeria_mover_bytes(readBin(origem, "raw", file.size(origem)), file.path(dir, arq))
  arq
}

# --- Manifesto e arquivos ----------------------------------------------------

#' Assinatura: se muda, o arquivo é reescrito.
#' @noRd
.tr_galeria_versao <- function(key, g) {
  paste(if (is.null(key)) "" else key, g$formato, format(g$dpi, scientific = FALSE), sep = "|")
}

#' Linha do manifesto de um item.
#' @noRd
.tr_galeria_linha <- function(it) {
  list(node = it$node, arquivo = it$arquivo, versao = it$versao %||% "", tipo = it$tipo)
}

#' Entrada do manifesto de um nó, ou `NULL`.
#' @noRd
.tr_galeria_entrada <- function(manifesto, node) {
  for (e in manifesto) if (identical(e$node, node)) return(e)
  NULL
}

#' Lê `.galeria.json`. Ilegível ou ausente vale vazio; entrada com caminho
#' (barra, `..`) é descartada, porque nunca se apaga nada fora da pasta.
#' @noRd
.tr_galeria_ler <- function(dir) {
  p <- file.path(dir, ".galeria.json")
  if (!file.exists(p)) return(list())
  m <- tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) NULL)
  itens <- m$itens %||% list()
  Filter(function(e) is.character(e$node) && length(e$node) == 1L &&
           is.character(e$arquivo) && length(e$arquivo) == 1L &&
           identical(basename(e$arquivo), e$arquivo) && nzchar(e$arquivo),
         itens)
}

#' Grava o manifesto por temporário e renomeia.
#' @noRd
.tr_galeria_escreve <- function(dir, entradas) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  p <- file.path(dir, ".galeria.json")
  tmp <- paste0(p, ".part")
  jsonlite::write_json(list(versao = 1L, itens = unname(entradas)), tmp,
                       auto_unbox = TRUE, pretty = TRUE, null = "null")
  .tr_galeria_renomeia(tmp, p)
  invisible(p)
}

#' Remove do disco o que o manifesto conhecia e que não ficou na galeria.
#' @noRd
.tr_galeria_limpa <- function(dir, manifesto, mantidos) {
  protegidos <- vapply(mantidos, function(e) e$arquivo, "")
  for (e in manifesto) if (!(e$arquivo %in% protegidos)) .tr_galeria_apaga(dir, e$arquivo)
  invisible(TRUE)
}

#' Apaga um arquivo da galeria. Só chamado com nomes do manifesto.
#' @noRd
.tr_galeria_apaga <- function(dir, arquivo) {
  p <- file.path(dir, arquivo)
  if (file.exists(p)) unlink(p)
  invisible(TRUE)
}

#' Escreve bytes por temporário e renomeia (o leitor nunca vê arquivo parcial).
#' @noRd
.tr_galeria_mover_bytes <- function(bytes, destino) {
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(destino, ".", .tr_entropy_hex(16L), ".part")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(bytes, tmp)
  .tr_galeria_renomeia(tmp, destino)
  invisible(destino)
}

#' `file.rename` não substitui em Windows; cópia é o plano B.
#' @noRd
.tr_galeria_renomeia <- function(de, para) {
  if (file.rename(de, para)) return(invisible(TRUE))
  ok <- file.copy(de, para, overwrite = TRUE)
  unlink(de)
  if (!ok) rlang::abort(sprintf("Falha ao gravar na galeria: %s.", para), class = "tr_error_galeria")
  invisible(TRUE)
}

#' PNG em base64 (cru ou data URL) para bytes; recusa o que não é PNG.
#' @noRd
.tr_galeria_png_bytes <- function(png) {
  if (!is.character(png) || length(png) != 1L || is.na(png) || !nzchar(png))
    rlang::abort("png precisa ser uma string em base64.", class = "tr_error_galeria")
  b64 <- sub("^data:[^,]*,", "", png)
  bytes <- tryCatch(jsonlite::base64_dec(b64), error = function(e) NULL)
  assinatura <- as.raw(c(0x89, 0x50, 0x4e, 0x47))
  if (is.null(bytes) || length(bytes) < 8L || !identical(bytes[1:4], assinatura))
    rlang::abort("A captura não é um PNG válido.", class = "tr_error_galeria")
  bytes
}

#' Extensão do formato da galeria.
#' @noRd
.tr_galeria_ext <- function(formato) c(png = "png", jpeg = "jpg", tiff = "tif")[[formato]] %||% "png"

#' Slug ASCII: sem acento, minúsculo, separado por hífen. Vazio cai no fallback.
#' @noRd
.tr_galeria_slug <- function(x, fallback) {
  s <- suppressWarnings(iconv(as.character(x)[[1]], to = "ASCII//TRANSLIT"))
  if (is.na(s)) s <- ""
  s <- tolower(gsub("[^A-Za-z0-9]+", "-", s))
  s <- gsub("^-+|-+$", "", s)
  if (!nzchar(s)) fallback else s
}

#' Configuração da galeria vinda dos settings (ou os padrões).
#' @noRd
.tr_galeria_cfg_de <- function(settings) {
  settings$galeria %||% .tr_galeria_cfg(NULL)
}

