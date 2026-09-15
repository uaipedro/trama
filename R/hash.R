#' Impressão digital de uma função de nó.
#'
#' O insumo usava `rlang::hash(body(fn))`, e isso é **insuficiente de um jeito
#' que já mordia lá**: `body()` não enxerga os helpers nem as constantes que a
#' função referencia. Em `insumo/nodes/viewer.R` existem as duas formas lado a
#' lado — `.wm_categorical_palette()` e `.wm_viewer_overlay_rgb` — e mudar
#' qualquer uma servia imagem velha do cache, em silêncio.
#'
#' Aqui o fecho é percorrido: corpo, formals, e recursivamente helpers e
#' constantes **do ambiente da própria coleção**. Entrar em `dplyr::filter`
#' seria inútil (a versão do pacote não muda no meio da sessão) e caro.
#'
#' Gap conhecido e aceito: atualizar uma DEPENDÊNCIA externa não muda a chave.
#' A alternativa — versão da coleção na chave — invalidaria o cache inteiro a
#' cada patch release, inclusive dos nós que não mudaram. `tr_bust()` é a
#' válvula.
#' @noRd
.tr_fn_fingerprint <- function(fn, max_depth = 4L) {
  home <- environment(fn)
  seen <- new.env(parent = emptyenv())
  parts <- c(rlang::hash(list(body(fn), formals(fn))))

  same_home <- function(f) {
    e <- environment(f)
    !is.null(e) && (identical(e, home) ||
      (isNamespace(e) && isNamespace(home) && identical(environmentName(e), environmentName(home))))
  }

  walk <- function(f, depth) {
    if (depth <= 0L) return(invisible())
    found <- tryCatch(.tr_referenced_names(f), error = function(e) NULL)
    if (is.null(found)) return(invisible())
    fenv <- environment(f)
    # `sort` de `character` usa a colação da LOCALE. Sem `method = "radix"`, a
    # mesma coleção geraria chaves diferentes em máquinas com locale distinta
    # — e o coordenador poderia divergir de um daemon que herdou outra.
    for (g in sort(found, method = "radix")) {
      tag <- paste0(g, "@", environmentName(fenv))
      if (!is.null(seen[[tag]])) next
      seen[[tag]] <- TRUE
      obj <- tryCatch(get(g, envir = fenv), error = function(e) NULL)
      if (is.null(obj)) next
      if (is.function(obj)) {
        if (!same_home(obj)) next
        parts <<- c(parts, g, rlang::hash(list(body(obj), formals(obj))))
        walk(obj, depth - 1L)
      } else {
        if (!exists(g, envir = home, inherits = FALSE)) next
        parts <<- c(parts, g, rlang::hash(obj))
      }
    }
  }
  walk(fn, max_depth)
  rlang::hash(parts)
}

#' Nomes referenciados no corpo de uma função — todos os símbolos da árvore
#' sintática, sem distinguir função de variável.
#'
#' Substitui `codetools::findGlobals()`. O motivo é de licença, não técnico:
#' `codetools` é GPL, e o projeto vai ser MIT. Era a única dependência GPL
#' escolhida deliberadamente (a outra, `htmltools`, é inevitável — o próprio
#' Shiny depende dela).
#'
#' Colhe DEMAIS de propósito: pega também variáveis locais, argumentos e nomes
#' de campo. Isso é seguro na direção certa — quem consome filtra por
#' `exists(g, envir = home, inherits = FALSE)`, então um nome a mais só pode
#' fazer o fingerprint cobrir um objeto extra da coleção. O erro possível é
#' invalidar cache à toa, nunca servir resultado velho.
#' @noRd
.tr_referenced_names <- function(f) {
  out <- character()
  walk <- function(e) {
    if (is.symbol(e)) { out <<- c(out, as.character(e)); return(invisible()) }
    if (is.call(e) || is.pairlist(e) || is.expression(e)) {
      for (i in seq_along(e)) {
        el <- tryCatch(e[[i]], error = function(err) NULL)
        if (!is.null(el)) walk(el)
      }
    }
    invisible()
  }
  walk(body(f))
  unique(out)
}

#' Fingerprint memoizado por `(id, version)` no registro.
#'
#' `.tr_fn_fingerprint` percorre o fecho e, num nó de pacote, custa dezenas de
#' ms. Como é chamado por nó (e por aresta com adaptador) a CADA `tr_plan`, e
#' cada tecla digitada gera um plano novo, um grafo de 100 nós pagaria segundos
#' por caractere — em trabalho 100% redundante, já que o fingerprint só muda
#' quando a coleção é recarregada. `tr_use()` limpa o cache, então hot reload
#' continua funcionando.
#' @noRd
.tr_print_cached <- function(registry, id, version, fn) {
  if (is.null(registry$prints)) registry$prints <- new.env(parent = emptyenv())
  k <- paste0(id, "@", version)
  hit <- registry$prints[[k]]
  if (!is.null(hit)) return(hit)
  val <- .tr_fn_fingerprint(fn)
  registry$prints[[k]] <- val
  val
}

#' Impressão digital do TIPO, não só do nó.
#'
#' `preview`, `store`, `restore` e `summary` são código que produz artefato
#' gravado sob a chave — mudar o `preview` de um tipo sem isto serviria o
#' preview velho para sempre, que é exatamente o furo do insumo reintroduzido
#' uma camada acima (e pior: persistido em disco entre sessões).
#' @noRd
.tr_type_fingerprint <- function(registry, type_id) {
  ty <- registry$types[[type_id]]
  if (is.null(ty)) return(type_id)
  k <- paste0("typeprint:", type_id, "@", ty$version)
  if (is.null(registry$prints)) registry$prints <- new.env(parent = emptyenv())
  hit <- registry$prints[[k]]
  if (!is.null(hit)) return(hit)
  val <- rlang::hash(c(
    type_id, ty$version, ty$ext,
    unlist(lapply(list(ty$store, ty$restore, ty$preview, ty$summary), function(f) {
      if (is.function(f)) .tr_fn_fingerprint(f) else "-"
    }))
  ))
  registry$prints[[k]] <- val
  val
}

#' Params efetivos: defaults do registro sobrescritos pelo documento,
#' NORMALIZADOS pelo `kind` declarado e pela aridade.
#'
#' A normalização é o que faz a chave sobreviver ao salvar/reabrir, em duas
#' dimensões que já mordiam:
#'   - TIPO: `jsonlite` reparsa `10` como inteiro, e `hash(10) != hash(10L)`.
#'   - ARIDADE: `simplifyVector = FALSE` faz um array JSON virar `list("a","b")`
#'     na op, mas `.tr_resimplify` (document-io.R) devolve `c("a","b")` ao
#'     reabrir. Param de seleção múltipla (`select`, `group_by`) nasce lista e
#'     renasce vetor — o caminho NORMAL num domínio de dados, não a exceção.
#' Sem as duas, todo documento reaberto dava cache miss universal.
#'
#' Param de tema sai daqui já RESOLVIDO na definição (`settings` do projeto):
#' o nome sozinho no hash serviria gráfico velho depois de editar o tema, e
#' `"padrão"` não mudaria de chave ao trocar o `tema_padrao`.
#' @noRd
.tr_effective_params <- function(spec, node, settings = NULL) {
  p <- lapply(spec$params, function(x) x$default)
  for (nm in intersect(names(node$params), names(p))) p[[nm]] <- node$params[[nm]]
  for (nm in names(p)) {
    v <- .tr_resimplify(p[[nm]])
    p[[nm]] <- tryCatch(.tr_check_param_value(spec$params[[nm]], v, nm), error = function(e) v)
    if (identical(spec$params[[nm]]$kind, "theme")) p[[nm]] <- .tr_theme_resolve(p[[nm]], settings)
  }
  if (length(p) > 0) p[order(names(p), method = "radix")] else p
}

#' Chave de conteúdo de uma unidade (a computação de um nó).
#'
#' Sai do PLANO, não da execução — decidir cache hit não acorda worker nenhum.
#' @noRd
.tr_unit_key <- function(spec, node, input_keys, adapter_prints, type_prints, external,
                         fn_print, nonce = NULL, settings = NULL) {
  rlang::hash(list(
    spec$id, spec$version, fn_print,
    .tr_effective_params(spec, node, settings),
    if (isTRUE(spec$stochastic)) node$seed else NULL,
    input_keys, adapter_prints, type_prints, external, nonce
  ))
}

#' Chave de UMA SAÍDA. O store guarda um artefato por porta de saída, não por
#' nó — sem isso, um nó com duas saídas produziria a mesma chave para as duas,
#' e conectar `split.treino` ou `split.teste` a jusante daria resultado
#' idêntico. Nó de múltiplas saídas (split, partition, train/test) é banal num
#' domínio de dados.
#' @noRd
.tr_out_key <- function(unit_key, port) rlang::hash(list(unit_key, port))

#' Invalida por prefixo de coleção — válvula para o gap de dependência externa.
#' @export
tr_bust <- function(store, collection = NULL) {
  n <- 0L
  for (h in list.files(file.path(store$root, "handles"), pattern = "\\.json$", full.names = TRUE)) {
    key <- sub("\\.json$", "", basename(h))
    meta <- tryCatch(jsonlite::fromJSON(h, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(meta)) { .tr_drop_key(store, key); n <- n + 1L; next }
    if (!is.null(collection)) {
      nt <- meta$node_type
      if (is.null(nt) || .tr_collection_of(nt) != collection) next
    }
    .tr_drop_key(store, key); n <- n + 1L
  }
  invisible(n)
}
