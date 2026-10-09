#' Exporta um fluxo como código R independente do Trama.
#'
#' Diferente de [tr_flow_code()], que gera a DSL usada para reconstruir o
#' documento no editor, esta função gera chamadas diretas às funções R dos nós.
#' O script resultante não cria registro, documento, plano ou store do Trama;
#' ele pode ser aberto e executado como qualquer outro script R, desde que os
#' pacotes das coleções estejam instalados.
#'
#' Cada nó vira uma variável com o nome do card (`correlograma_acf`, e não o
#' id), e um nó de várias saídas é UMA variável: quem a consome lê
#' `ajuste$out`. Os params passados são os efetivos, os mesmos que o executor
#' passa, omitidos só quando coincidem com o default literal da própria
#' função: o script calcula o que o canvas calculou, sem repetir o óbvio.
#'
#' No formato Quarto, cada card é um chunk; os frames viram seções, na ordem
#' de slide, e as notas viram texto no lugar em que estão. As saídas que
#' nenhum fio consome são as que o fluxo produziu para ser LIDAS, e o
#' documento as mostra (pelo `report` do tipo, quando há; senão, o `print`).
#' Uma saída consumida também aparece quando o tipo pede (`report_always` em
#' [tr_type()]), salvo se quem a consome devolve o mesmo tipo.
#'
#' Gráfico que segue o tema "padrão" do projeto sai, no Quarto, num tema claro
#' quando o padrão do projeto é escuro: o relatório é lido em página branca e
#' impresso. Card com tema escolhido à mão fica com o que escolheu.
#'
#' Fluxos ponto a ponto ainda dependem do executor do Trama e, por isso, são
#' recusados em vez de gerar um script que calcularia outra coisa.
#'
#' @param doc Documento ou [tr_flow()] a exportar.
#' @param registry Registro que resolve os nós e adaptadores do documento.
#' @param format `"r"` para um script R ou `"quarto"` para um documento Quarto.
#' @param title Título usado no documento Quarto.
#' @param settings Configurações do projeto, para resolver params de tema como
#'   o executor resolve.
#' @return Uma string única pronta para ser gravada em um arquivo `.R` ou `.qmd`.
#' @export
tr_export_code <- function(doc, registry = .tr_default_registry,
                           format = c("r", "quarto"), title = "Análise",
                           settings = NULL) {
  format <- match.arg(format)
  doc <- .tr_as_doc(doc)
  if (identical(format, "quarto")) settings <- .tr_export_settings_relatorio(settings)
  .tr_export_check_supported(doc, registry)

  plan <- tr_plan(doc, targets = names(doc$nodes), registry = registry)
  vars <- .tr_export_vars(doc, registry)
  consumed <- vapply(doc$edges, function(e) paste(e$from$node, e$from$port), "")
  pkgs <- character()
  ref <- function(fn) {
    r <- .tr_export_fn_ref(fn)
    pkg <- sub("^([A-Za-z0-9.]+):::?.*$", "\\1", r)
    if (!identical(pkg, r) && pkg != "base") pkgs <<- union(pkgs, pkg)
    r
  }

  # Um bloco por nó: o código e as expressões a mostrar no relatório.
  blocks <- list()
  temas <- character()
  for (id in .tr_plan_node_order(plan)) {
    node <- doc$nodes[[id]]
    spec <- tr_get_node(node$type, registry)
    args <- character()

    for (pn in names(spec$inputs)) {
      edges <- Filter(function(e) identical(e$to$node, id) && identical(e$to$port, pn), doc$edges)
      if (!length(edges)) next
      edges <- edges[order(vapply(edges, function(e) as.integer(e$index %||% 1L), integer(1)))]
      value <- vapply(edges, function(e) {
        expr <- .tr_export_out(vars, doc, registry, e$from$node, e$from$port)
        from <- tr_get_node(doc$nodes[[e$from$node]]$type, registry)
        if (!identical(from$outputs[[e$from$port]]$type, spec$inputs[[pn]]$type)) {
          adapter <- tr_adapter_for(from$outputs[[e$from$port]]$type, spec$inputs[[pn]]$type, registry)
          expr <- sprintf("%s(%s)", ref(adapter$fn), expr)
        }
        expr
      }, "")
      rhs <- if (isTRUE(spec$inputs[[pn]]$multiple)) {
        sprintf("list(%s)", paste(value, collapse = ", "))
      } else value[[1]]
      args <- c(args, sprintf("%s = %s", .tr_export_name(pn), rhs))
    }

    params <- .tr_effective_params(spec, node, settings)
    defaults <- formals(spec$fn)
    for (pn in intersect(names(spec$params), names(params))) {
      if (.tr_export_is_default(defaults, pn, params[[pn]])) next
      value <- .tr_export_value(params[[pn]])
      # Tema resolvido é uma lista de vinte campos, e igual em todo gráfico:
      # vira UMA variável no topo, que o leitor troca num lugar só.
      if (identical(spec$params[[pn]]$kind, "theme")) {
        i <- match(value, temas)
        if (is.na(i)) {
          nome <- .tr_export_unique(paste0("tema_", .tr_export_slug(params[[pn]]$nome %||% "", "projeto")),
                                    c(names(temas), vars), "_")
          temas[[nome]] <- value
          i <- length(temas)
        }
        value <- names(temas)[[i]]
      }
      args <- c(args, sprintf("%s = %s", .tr_export_name(pn), value))
    }
    if (isTRUE(spec$stochastic) && ".seed" %in% names(defaults)) {
      args <- c(args, sprintf(".seed = %s", .tr_export_value(node$seed)))
    }

    call <- sprintf("%s(%s)", ref(spec$fn), paste(args, collapse = ", "))
    code <- if (length(spec$outputs)) sprintf("%s <- %s", vars[[id]], call) else call

    show <- character()
    for (pn in names(spec$outputs)) {
      ty <- tr_get_type(spec$outputs[[pn]]$type, registry)
      if (paste(id, pn) %in% consumed && !.tr_export_mostra_consumida(doc, registry, id, pn, ty)) next
      expr <- .tr_export_out(vars, doc, registry, id, pn)
      report <- ty$report
      show <- c(show, if (is.function(report)) sprintf("%s(%s)", ref(report), expr) else expr)
    }
    blocks[[id]] <- list(code = code, show = show)
  }

  preambulo <- c(.tr_export_versions(pkgs),
                 if (length(temas)) c("", sprintf("%s <- %s", names(temas), temas)))
  if (identical(format, "r")) {
    lines <- c("# Código R gerado pelo editor.",
               "# Execute a partir da pasta do projeto para preservar caminhos relativos.",
               preambulo, "")
    for (b in blocks) lines <- c(lines, b$code, "")
    return(paste(lines, collapse = "\n"))
  }
  .tr_export_quarto(doc, blocks, title, preambulo, registry, pkgs)
}

# Monta o .qmd. A ordem é a do plano refeita: entre os nós já liberados pelas
# dependências, vem antes o do frame de menor ordem e, dentro dele, o de cima
# e da esquerda. Assim o documento lê como o canvas, e nunca usa uma variável
# antes de ela existir. Notas não têm dependência e entram na mesma fila,
# pela mesma chave: uma nota no frame 2 sai depois do que o frame 1 calcula.
.tr_export_quarto <- function(doc, blocks, title, preambulo, registry, pkgs = character()) {
  frames <- doc$ui$frames %||% list()
  frames <- frames[order(vapply(frames, function(f) as.numeric(f$order %||% 0), numeric(1)))]
  pos <- doc$ui$positions %||% list()

  dentro <- function(x, y) {
    for (fid in names(frames)) {
      f <- frames[[fid]]
      if (x >= f$x && x <= f$x + f$w && y >= f$y && y <= f$y + f$h) return(fid)
    }
    NA_character_
  }
  chave <- function(x, y, i) {
    fid <- if (is.na(x)) NA_character_ else dentro(x, y)
    list(frame = fid, k = c(if (is.na(fid)) Inf else match(fid, names(frames)),
                            if (is.na(y)) Inf else y, if (is.na(x)) Inf else x, i))
  }

  itens <- list()
  ids <- names(blocks)
  for (i in seq_along(ids)) {
    p <- pos[[ids[[i]]]]
    itens[[length(itens) + 1L]] <- c(list(kind = "node", id = ids[[i]]),
                                     chave(p[[1]] %||% NA, p[[2]] %||% NA, i))
  }
  notes <- doc$ui$notes %||% list()
  for (nid in names(notes)) {
    n <- notes[[nid]]
    itens[[length(itens) + 1L]] <- c(list(kind = "note", note = n),
                                     chave(n$x, n$y, length(itens) + 1L))
  }

  # Dependências entre nós, para a fila não passar ninguém na frente.
  deps <- lapply(setNames(ids, ids), function(id) {
    unique(vapply(Filter(function(e) identical(e$to$node, id), doc$edges),
                  function(e) e$from$node, ""))
  })
  feitos <- character()
  ordem <- list()
  resta <- itens
  while (length(resta)) {
    livre <- vapply(resta, function(it) it$kind == "note" || all(deps[[it$id]] %in% feitos), logical(1))
    if (!any(livre)) livre[[1]] <- TRUE
    cand <- which(livre)
    best <- cand[[1]]
    for (j in cand[-1]) {
      a <- resta[[j]]$k; b <- resta[[best]]$k
      d <- which(a != b)
      if (length(d) && a[[d[[1]]]] < b[[d[[1]]]]) best <- j
    }
    it <- resta[[best]]
    if (it$kind == "node") feitos <- c(feitos, it$id)
    ordem[[length(ordem) + 1L]] <- it
    resta <- resta[-best]
  }

  labels <- character()
  corpo <- character()
  atual <- NULL
  for (it in ordem) {
    fid <- it$frame
    if (length(frames) && !identical(fid, atual)) {
      corpo <- c(corpo, sprintf("## %s", if (is.na(fid)) "Outros passos" else frames[[fid]]$title), "")
      atual <- fid
    }
    if (it$kind == "note") {
      n <- it$note
      txt <- if (identical(n$kind, "imagem")) sprintf("![](imagens/%s)", n$src %||% "") else n$text %||% ""
      if (nzchar(trimws(txt))) corpo <- c(corpo, txt, "")
      next
    }
    b <- blocks[[it$id]]
    node <- doc$nodes[[it$id]]
    rotulo <- node$label %||% tr_get_node(node$type, registry)$label
    label <- .tr_export_unique(gsub("_", "-", .tr_export_slug(rotulo, "bloco")), labels, "-")
    labels <- c(labels, label)
    if (length(b$show)) corpo <- c(corpo, sprintf("### %s", rotulo), "")
    corpo <- c(corpo, "```{r}", sprintf("#| label: %s", label), b$code, b$show, "```", "")
  }

  yaml <- c("---", paste0("title: ", .tr_export_yaml(title)), "date: today", "lang: pt-BR",
            "format:", "  html:", "    toc: true", "    code-fold: show",
            "    code-tools: true", "    embed-resources: true", "---", "")
  setup <- c("```{r}", "#| label: setup", "#| code-fold: true",
             "# Código R gerado pelo editor.",
             "# Renderize a partir da pasta do projeto para preservar caminhos relativos.",
             preambulo, "```", "")
  fim <- c(.tr_export_referencias(doc, registry, pkgs),
           "## Ambiente", "", "```{r}", "#| label: ambiente", "#| code-fold: true",
           "sessionInfo()", "```", "")
  paste(c(yaml, setup, corpo, fim), collapse = "\n")
}

# "Software": o relatório cita as FERRAMENTAS que fizeram a conta, e não o
# método nem o livro-texto que o ensina. Quem fez o Tukey foi o `emmeans`; é a
# ele (e ao R) que o texto deve crédito, e é com ele que o leitor refaz a conta.
# As `tr_ref` de teoria, livro-texto e complementar continuam na ajuda do
# bloco, onde servem para estudar o método — no relatório eram referência por
# referência.
#
# Duas partes: a tabela "bloco → pacote::função" (das `tr_ref` de
# implementação, o que cada bloco usado chamou) e um chunk que cita cada pacote
# com `citation()` NA HORA DE RENDERIZAR — a citação que o autor do pacote
# pediu, na versão instalada de quem renderiza. Os pacotes da base viram uma
# citação só, a do R.
.TR_EXPORT_PACOTES_BASE <- c("base", "stats", "utils", "methods", "graphics", "grDevices", "grid",
                             "splines", "stats4", "tools", "parallel", "compiler", "datasets")

.tr_export_referencias <- function(doc, registry, pkgs = character()) {
  tipos <- unique(vapply(doc$nodes, function(n) n$type, ""))
  usos <- list()
  for (ty in sort(tipos, method = "radix")) {
    spec <- tryCatch(tr_get_node(ty, registry), error = function(e) NULL)
    if (is.null(spec)) next
    rotulo <- spec$label %||% ty
    # Texto de coleção instalada pode vir sem marca de encoding, e a
    # ordenação radix abaixo recusa string não-ASCII sem marca.
    if (validUTF8(rotulo)) Encoding(rotulo) <- "UTF-8"
    for (r in spec$referencias %||% list()) {
      if (!identical(r$papel, "implementacao")) next
      fn <- sprintf("`%s::%s()`", r$pacote, r$funcao)
      usos[[rotulo]] <- unique(c(usos[[rotulo]], fn))
      pkgs <- union(pkgs, r$pacote)
    }
  }
  pkgs <- sort(setdiff(pkgs, c(.TR_EXPORT_PACOTES_BASE, "trama")), method = "radix")
  tabela <- if (length(usos)) {
    nomes <- sort(names(usos), method = "radix")
    c("| Bloco | Ferramenta |", "|:---|:---|",
      sprintf("| %s | %s |", nomes, vapply(usos[nomes], paste, "", collapse = ", ")), "")
  }
  chunk <- c("```{r}", "#| label: software", "#| echo: false", "#| results: asis",
             sprintf("pacotes <- %s", .tr_export_value(pkgs)),
             "citar <- function(p) suppressWarnings(format(if (is.null(p)) citation() else citation(p), style = \"text\"))",
             "cat(paste0(\"- \", c(citar(NULL), unlist(lapply(pacotes, citar)))), sep = \"\\n\")",
             "```", "")
  c("## Software", "", tabela, chunk)
}

# Versões dos pacotes de que o script depende, na hora da exportação: é o que
# deixa refazer a conta daqui a um ano, quando o default de algum nó mudar.
.tr_export_versions <- function(pkgs) {
  if (!length(pkgs)) return(character())
  v <- vapply(sort(pkgs), function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) "?")
  }, "")
  c("# Pacotes usados (versão na exportação):", sprintf("#   %s %s", names(v), v))
}

# Expressão de uma saída: a variável do nó, ou `var$porta` quando há várias.
.tr_export_out <- function(vars, doc, registry, id, port) {
  ports <- names(tr_get_node(doc$nodes[[id]]$type, registry)$outputs)
  if (length(ports) == 1L) vars[[id]] else paste0(vars[[id]], "$", .tr_export_name(port))
}

# Omitir um param só é seguro quando a função tem, no default, o MESMO valor
# literal: chamada ou símbolo no default (`c("a", "b")`, `match.arg`) pode
# depender de contexto, e aí o valor vai escrito.
.tr_export_is_default <- function(defaults, name, value) {
  if (!name %in% names(defaults)) return(FALSE)
  d <- defaults[[name]]
  if (missing(d) || is.null(d) || is.language(d)) return(FALSE)
  identical(d, value) || (is.numeric(d) && is.numeric(value) && length(d) == 1L &&
                            length(value) == 1L && isTRUE(d == value))
}

.tr_export_check_supported <- function(doc, registry) {
  for (node in doc$nodes) {
    spec <- tr_get_node(node$type, registry)
    if (isTRUE(spec$online) || any(vapply(c(spec$inputs, spec$outputs), function(p) isTRUE(p$stream), logical(1)))) {
      rlang::abort(
        sprintf("O nó '%s' usa execução ponto a ponto e ainda não pode ser exportado como script R independente.", node$id),
        class = "tr_error_export_stream"
      )
    }
    context <- formals(spec$fn)[[".ctx"]]
    if (!is.null(context) && identical(context, quote(expr = ))) {
      rlang::abort(
        sprintf("O nó '%s' exige o contexto de execução do Trama e não pode ser exportado como script R independente.", node$id),
        class = "tr_error_export_context"
      )
    }
  }
  invisible(doc)
}

# Nome da variável de cada nó: o rótulo do card quando o nó tem um rótulo de
# verdade, e o id quando o rótulo é só o id do tipo (nó sem `label`). Ids
# gerados pelo editor são ilegíveis; ids escritos à mão na DSL não, e é o
# rótulo ausente, não o id, que diz qual dos dois casos é.
.tr_export_vars <- function(doc, registry) {
  used <- character()
  out <- character()
  for (id in names(doc$nodes)) {
    node <- doc$nodes[[id]]
    spec <- tr_get_node(node$type, registry)
    rotulo <- node$label %||% spec$label
    stem <- if (identical(rotulo, spec$id)) .tr_export_slug(id, "no") else .tr_export_slug(rotulo, "no")
    name <- .tr_export_unique(stem, used, "_")
    used <- c(used, name)
    out[[id]] <- name
  }
  out
}

# "Correlograma (ACF)" -> "correlograma_acf". Até quatro palavras: nome de
# variável é para ler no código, e o rótulo inteiro já vai no título da seção.
.tr_export_slug <- function(x, fallback) {
  s <- iconv(x %||% "", to = "ASCII//TRANSLIT", sub = "")
  if (is.na(s)) s <- ""
  s <- tolower(gsub("[^A-Za-z0-9]+", " ", s))
  w <- strsplit(trimws(s), " +")[[1]]
  w <- w[nzchar(w)]
  s <- paste(utils::head(w, 4L), collapse = "_")
  if (!nzchar(s)) s <- fallback
  if (grepl("^[0-9]", s)) s <- paste0(fallback, "_", s)
  if (s %in% c("if", "else", "repeat", "while", "function", "for", "next", "break",
               "in", "c", "t", "q", "T", "F", "df", "data")) s <- paste0(s, "_", fallback)
  s
}

.tr_export_unique <- function(stem, used, sep) {
  if (!stem %in% used) return(stem)
  i <- 2L
  while (paste0(stem, sep, i) %in% used) i <- i + 1L
  paste0(stem, sep, i)
}

.tr_export_name <- function(name) {
  if (make.names(name) == name && !name %in% c("NA", "NULL", "TRUE", "FALSE", "Inf", "NaN")) name
  else paste0("`", gsub("`", "\\\\`", name, fixed = TRUE), "`")
}

.tr_export_value <- function(value) paste(capture.output(dput(value)), collapse = "\n")

.tr_export_yaml <- function(value) {
  paste0('"', gsub('"', '\\\\"', as.character(value), fixed = TRUE), '"')
}

#' Referência que um script independente pode chamar. Funções exportadas usam
#' `pacote::função`; helpers de coleção ainda usam `pacote:::função`, pois o
#' script continua independente do Trama mesmo quando a coleção escolheu não
#' publicar esse detalhe da sua API.
#' @noRd
.tr_export_fn_ref <- function(fn) {
  env <- environment(fn)
  pkg <- if (is.environment(env) && isNamespace(env)) getNamespaceName(env) else NULL
  if (!is.null(pkg)) {
    names <- ls(env, all.names = TRUE)
    found <- names[vapply(names, function(name) identical(get(name, envir = env), fn), logical(1))]
    if (length(found)) {
      exported <- found[found %in% getNamespaceExports(pkg)]
      if (length(exported)) return(paste0(pkg, "::", exported[[1]]))
      return(paste0(pkg, ":::", found[[1]]))
    }
  }
  if (is.primitive(fn)) {
    base_names <- getNamespaceExports("base")
    found <- base_names[vapply(base_names, function(name) identical(get(name, envir = baseenv()), fn), logical(1))]
    if (length(found)) return(paste0("base::", found[[1]]))
  }
  paste0("(", paste(deparse(fn), collapse = "\n"), ")")
}

# Saída consumida que o tipo pede para mostrar (`report_always`): aparece, a
# menos que algum consumidor devolva o mesmo tipo — aí ela é um passo de uma
# cadeia que refina o objeto, e quem aparece é o último elo.
.tr_export_mostra_consumida <- function(doc, registry, id, pn, ty) {
  if (!isTRUE(ty$report_always)) return(FALSE)
  destinos <- Filter(function(e) identical(e$from$node, id) && identical(e$from$port, pn), doc$edges)
  for (e in destinos) {
    saidas <- tr_get_node(doc$nodes[[e$to$node]]$type, registry)$outputs
    if (ty$id %in% vapply(saidas, function(o) o$type, "")) return(FALSE)
  }
  TRUE
}

# Luminância relativa de uma cor #rrggbb (WCAG), para saber se o tema é escuro.
.tr_export_luminancia <- function(hex) {
  v <- strtoi(substring(hex, c(2L, 4L, 6L), c(3L, 5L, 7L)), 16L) / 255
  v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  sum(c(0.2126, 0.7152, 0.0722) * v)
}

# Settings do relatório: o "padrão" que é escuro troca pelo primeiro tema
# claro do projeto (ou o `claro` embutido, se o projeto só declarou escuros).
.tr_export_settings_relatorio <- function(settings) {
  s <- settings %||% .tr_settings(list())
  escuro <- function(nome) .tr_export_luminancia(s$temas[[nome]]$fundo) < 0.5
  if (!escuro(s$tema_padrao)) return(s)
  claros <- Filter(Negate(escuro), names(s$temas))
  if (length(claros)) {
    s$tema_padrao <- claros[[1]]
  } else {
    s$temas[["claro"]] <- .tr_temas_embutidos()[["claro"]]
    s$tema_padrao <- "claro"
  }
  s
}
