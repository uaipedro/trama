#' Raio-x de um bloco: onde a conta de fato acontece.
#'
#' Percorre o código da função do bloco e das funções internas do mesmo
#' pacote que ela chama, e devolve as chamadas a funções de OUTROS pacotes
#' (`stats::aov(...)`, `lme4::lmer(...)`), com o trecho de código e o arquivo
#' e a linha em que aparecem. É o que a ajuda e o site mostram em
#' "Implementação": a chamada que faz a conta, e não o invólucro do Trama.
#'
#' Pacotes de infraestrutura (base, rlang, tibble, cli...) ficam de fora por
#' padrão: dizem como a saída é montada, não como a conta é feita.
#'
#' @param node Nó, como devolvido por [tr_node()].
#' @param ignorar Pacotes cujas chamadas não entram no raio-x.
#' @return Lista com `funcao` (`"pacote::nome"`), `arquivo`, `linha` e
#'   `chamadas`: uma lista de `list(pacote, funcao, codigo, arquivo, linha,
#'   dentro)`, sem repetir o mesmo `pacote::funcao` no mesmo lugar.
#' @export
tr_node_raiox <- function(node, ignorar = .tr_raiox_ignorar) {
  fn <- node$fn
  ns <- environment(fn)
  pacote <- if (isNamespace(ns)) getNamespaceName(ns) else NA_character_
  nome <- .tr_raiox_nome(fn, ns)
  ignorar <- union(ignorar, c(pacote, "trama"))
  vistas <- character()
  chamadas <- list()

  andar_funcao <- function(f, dentro) {
    src <- utils::getSrcref(f)
    arquivo <- .tr_raiox_arquivo(src)
    andar_expr(body(f), dentro, arquivo, if (is.null(src)) NA_integer_ else src[[1]])
  }
  andar_expr <- function(e, dentro, arquivo, linha) {
    if (!is.call(e)) return(invisible())
    # Linha mais próxima: as instruções de um `{` com srcref trazem a sua.
    refs <- attr(e, "srcref")
    alvo <- .tr_raiox_alvo(e[[1]], ns)
    if (!is.null(alvo)) {
      if (identical(alvo$tipo, "interna")) {
        if (!alvo$funcao %in% vistas) {
          vistas <<- c(vistas, alvo$funcao)
          andar_funcao(get(alvo$funcao, envir = ns), alvo$funcao)
        }
      } else if (!alvo$pacote %in% ignorar) {
        chamadas[[length(chamadas) + 1L]] <<- list(
          pacote = alvo$pacote, funcao = alvo$funcao,
          codigo = .tr_raiox_codigo(e), arquivo = arquivo, linha = linha,
          dentro = dentro
        )
      }
    }
    partes <- as.list(e)[-1]
    for (i in seq_along(partes)) {
      # `partes[[i]]` direto: um argumento vazio (`x[, 1]`) guardado numa
      # variável vira "argumento ausente" na primeira leitura.
      l <- if (!is.null(refs) && length(refs) >= i + 1L) refs[[i + 1L]][[1]] else linha
      if (is.call(partes[[i]])) andar_expr(partes[[i]], dentro, arquivo, l)
      else if (is.function(partes[[i]])) andar_funcao(partes[[i]], dentro)
    }
  }
  andar_funcao(fn, nome)

  chave <- vapply(chamadas, function(x) paste(x$pacote, x$funcao, x$arquivo, x$linha), "")
  src <- utils::getSrcref(fn)
  list(
    funcao = if (is.na(pacote)) nome else paste0(pacote, "::", nome),
    arquivo = .tr_raiox_arquivo(src),
    linha = if (is.null(src)) NA_integer_ else src[[1]],
    chamadas = chamadas[!duplicated(chave)]
  )
}

.tr_raiox_ignorar <- c(
  "base", "methods", "utils", "rlang", "cli", "glue", "tibble", "vctrs",
  "pillar", "lifecycle", "magrittr", "withr", "jsonlite", "digest",
  "grDevices", "tools"
)

# A que se refere a cabeça de uma chamada: `pkg::f` / `pkg:::f`, ou um
# símbolo que existe no namespace do bloco (interna) ou fora dele.
.tr_raiox_alvo <- function(cabeca, ns) {
  if (is.call(cabeca) && is.symbol(cabeca[[1]]) && as.character(cabeca[[1]]) %in% c("::", ":::")) {
    return(list(tipo = "externa", pacote = as.character(cabeca[[2]]),
                funcao = as.character(cabeca[[3]])))
  }
  if (!is.symbol(cabeca)) return(NULL)
  nome <- as.character(cabeca)
  if (is.environment(ns) && !identical(ns, globalenv()) &&
      exists(nome, envir = ns, inherits = FALSE) &&
      is.function(get(nome, envir = ns))) {
    return(list(tipo = "interna", funcao = nome))
  }
  f <- tryCatch(get(nome, envir = ns, mode = "function"), error = function(e) NULL)
  if (is.null(f)) return(NULL)
  amb <- environment(f)
  pacote <- if (is.null(amb)) "base" else if (isNamespace(amb)) getNamespaceName(amb) else NULL
  if (is.null(pacote)) return(NULL)
  list(tipo = "externa", pacote = unname(pacote), funcao = nome)
}

.tr_raiox_nome <- function(fn, ns) {
  if (!isNamespace(ns)) return("fn")
  for (nm in getNamespaceExports(ns)) {
    if (identical(get0(nm, envir = ns, inherits = FALSE), fn)) return(nm)
  }
  for (nm in ls(ns, all.names = TRUE)) {
    if (identical(get0(nm, envir = ns, inherits = FALSE), fn)) return(nm)
  }
  "fn"
}

# Caminho relativo à raiz do repositório quando der (collections/... ou R/...).
.tr_raiox_arquivo <- function(src) {
  if (is.null(src)) return(NA_character_)
  f <- attr(src, "srcfile")$filename
  if (is.null(f) || !nzchar(f)) return(NA_character_)
  m <- regmatches(f, regexpr("(collections/[^/]+/)?R/[^/]+$", f))
  if (length(m)) m else basename(f)
}

.tr_raiox_codigo <- function(e, largura = 70L) {
  txt <- paste(trimws(deparse(e, width.cutoff = largura)), collapse = " ")
  if (nchar(txt) > 160L) paste0(substr(txt, 1L, 157L), "...") else txt
}
