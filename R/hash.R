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

#' Impressão digital do adaptador de UMA aresta, memoizada como as outras.
#'
#' Mora aqui, e não inline em `plan.R`, porque a região precisa da MESMA
#' impressão para as arestas internas dela: duas cópias divergiriam, e a que
#' ficasse atrás daria chave diferente pra mesma conversão.
#' @noRd
.tr_adapter_print <- function(registry, adapter) {
  if (is.null(adapter)) return(NULL)
  .tr_print_cached(registry, paste0("ad:", adapter$from, "->", adapter$to), 1L,
                   tr_adapter_for(adapter$from, adapter$to, registry)$fn)
}

#' Chave de conteúdo de uma REGIÃO de fluxo — a região inteira como uma
#' computação só.
#'
#' Irmã de `.tr_unit_key()`, e mora ao lado dela pelo mesmo motivo: se as duas
#' morassem em arquivos diferentes, "tudo o que pode mudar o resultado entra na
#' chave?" deixaria de ser pergunta que se responde lendo um lugar.
#'
#' Quatro coisas entram aqui que não entram na chave de um nó solto, e cada uma
#' fecha um furo conhecido:
#'   - `init` e `step`: `fn_print` sai só de `spec$fn`, e nó com memória não roda
#'     por `fn`. Sem eles, editar o corpo do acumulador serve o histórico velho
#'     do cache, em silêncio, para sempre.
#'   - `nonce` se algum membro é VOLÁTIL e o `fingerprint()` de cada membro
#'     IMPURO: a liftabilidade isenta nó `online` por completo, então os dois
#'     passam pela validação. Volátil cacheado nunca recomputa; impuro sem
#'     fingerprint serve dado velho quando o mundo externo muda.
#'   - a FIAÇÃO interna (dentro de `members[[id]]$inputs`): `junta(a =, b =)` com
#'     as portas trocadas é outra computação, e não há param nem código que
#'     registre isso.
#'   - os adaptadores das arestas internas, que rodam ponto a ponto e cujo
#'     resultado é o histórico gravado no store.
#'
#' Cadência (`tempo`, `publish_every`, `checkpoint_every`) NÃO é excluída: por
#' decisão de desenho ela é estado de sessão, chega por `ctx_extra` (`run.R`) e
#' não vive no documento, então não há param a tirar. Excluir por nome quebraria
#' no dia em que um nó tiver um param `tempo`.
#'
#' Isso é o que a ausência de `ctx_extra` em `tr_plan()` garante por construção —
#' a chave é calculada antes de haver ajuste de run pra olhar. O teste "cadência
#' NÃO entra na chave" (`test-stream-driver.R`) mede pelo lado de fora: dois runs
#' com cadências diferentes, e o segundo vem do cache. Existe porque o caminho
#' provável pra perder isso é pôr a cadência na unidade e hasheá-la aqui, e aí o
#' botão que protege um fluxo de dez mil pontos passaria a recomputá-lo.
#'
#' A ordem é estável por construção: os membros vêm na ordem topológica da
#' região (com empate alfabético) e os mapas nomeados são ordenados com
#' `method = "radix"`, pela razão de locale já documentada acima.
#'
#' CONDIÇÃO desta chave, e o dia em que ela deixa de valer: `type_prints` cobre
#' só as portas de saída dos COLAPSOS. A impressão digital do tipo das portas
#' INTERNAS fica fora de propósito, e isso é correto exatamente enquanto nenhum
#' valor de dentro da região atravessar o `store`/`restore` de um tipo. No
#' momento em que o driver despejar estado interior em disco por `type$store` —
#' em vez de um RDS cru do estado, ou do caminho em memória de `.ctx$partial` —
#' o código daquele tipo passa a ser INSUMO do histórico gravado sob esta chave,
#' e tem que entrar aqui: sem isso, editar o `store`/`restore` de um tipo
#' interno serve, do cache, um histórico que aquele código não produziria mais.
#' @noRd
.tr_region_key <- function(region, members, specs, registry, ext_keys, ext_prints,
                           type_prints, externals, nonce = NULL) {
  ord <- function(x) if (length(x)) x[order(names(x), method = "radix")] else x

  code <- lapply(region$nodes, function(id) {
    spec <- specs[[id]]
    list(spec$id, spec$version,
         .tr_print_cached(registry, spec$id, spec$version, spec$fn),
         if (is.function(spec$init)) {
           .tr_print_cached(registry, paste0(spec$id, "#init"), spec$version, spec$init)
         },
         if (is.function(spec$step)) {
           .tr_print_cached(registry, paste0(spec$id, "#step"), spec$version, spec$step)
         })
  })

  # `seed` só de quem é estocástico, a mesma regra do nó solto: `set_seed` num
  # membro determinístico não pode invalidar a região inteira.
  # `m$node_version` É o mesmo `spec$version` que já entrou em `code` acima —
  # redundância conhecida, e por isso nenhum teste consegue distinguir a remoção
  # de um dos dois (só a dos dois juntos falha). Fica como está porque tirar um
  # mudaria a chave de TODA região sem fechar furo nenhum.
  membros <- lapply(region$nodes, function(id) {
    m <- members[[id]]
    list(id, m$node_type, m$node_version, m$online, m$role, m$params,
         if (isTRUE(m$stochastic)) m$seed else NULL, m$inputs)
  })

  rlang::hash(list(
    "trama/stream_region", region$source, region$nodes, region$collapse,
    code, membros, .tr_region_adapter_prints(members, registry),
    ord(ext_keys), ord(ext_prints), type_prints, ord(externals), nonce
  ))
}

#' Impressões dos adaptadores das arestas INTERNAS da região, por par de tipos.
#'
#' Por par, e não por aresta, porque é o par que determina a função: duas
#' arestas com a mesma conversão têm a mesma impressão, e a fiação (qual aresta
#' usa qual par) já entrou na chave por `members[[id]]$inputs`.
#' @noRd
.tr_region_adapter_prints <- function(members, registry) {
  pares <- character()
  for (m in members) for (srcs in m$inputs) for (s in srcs) {
    if (identical(s$from, "internal") && !is.null(s$adapter)) {
      pares <- c(pares, paste0(s$adapter$from, "\r", s$adapter$to))
    }
  }
  pares <- sort(unique(pares), method = "radix")
  lapply(stats::setNames(pares, pares), function(p) {
    tp <- strsplit(p, "\r", fixed = TRUE)[[1]]
    .tr_adapter_print(registry, list(from = tp[[1]], to = tp[[2]]))
  })
}

#' Chave de UMA SAÍDA. O store guarda um artefato por porta de saída, não por
#' nó — sem isso, um nó com duas saídas produziria a mesma chave para as duas,
#' e conectar `split.treino` ou `split.teste` a jusante daria resultado
#' idêntico. Nó de múltiplas saídas (split, partition, train/test) é banal num
#' domínio de dados.
#' @noRd
.tr_out_key <- function(unit_key, port) rlang::hash(list(unit_key, port))

#' Invalida por prefixo de coleção — válvula para o gap de dependência externa.
#'
#' Casa se QUALQUER coleção que produziu o artefato for a pedida. Nó comum tem
#' uma só, derivada do `node_type`. A região de fluxo tem o conjunto dos membros,
#' gravado no handle, porque o `node_type` dela é `trama/stream_region` e lia
#' como coleção "trama": `tr_bust(store, "minha_colecao")` não a alcançava, todo
#' nó comum recomputava, e o histórico de dez mil pontos continuava vindo do
#' código de antes do upgrade — calado, pra sempre.
#' @export
tr_bust <- function(store, collection = NULL) {
  n <- 0L
  for (h in list.files(file.path(store$root, "handles"), pattern = "\\.json$", full.names = TRUE)) {
    key <- sub("\\.json$", "", basename(h))
    meta <- tryCatch(jsonlite::fromJSON(h, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(meta)) { .tr_drop_key(store, key); n <- n + 1L; next }
    if (!is.null(collection)) {
      cols <- unlist(meta$collections)
      if (is.null(cols)) {
        nt <- meta$node_type
        cols <- if (is.null(nt)) NULL else .tr_collection_of(nt)
      }
      if (!(collection %in% cols)) next
    }
    .tr_drop_key(store, key); n <- n + 1L
  }
  # Os checkpoints de região saem TODOS, e não só os da coleção pedida.
  #
  # `tr_bust()` significa "o código mudou sem a chave mudar" — é a válvula
  # documentada desse gap. Um checkpoint é justamente estado computado por
  # código cuja impressão digital não mudou: deixá-lo aqui faria a região
  # retomar do passo 5.000 produzido pelo código de ANTES do upgrade e terminar
  # com o de depois — um histórico metade velho, metade novo, gravado sob uma
  # chave válida e servido do cache pra sempre.
  #
  # Todos, e não só os da coleção, porque não há como ir da chave de saída (que
  # é o que o handle guarda) para a chave da unidade, que é o nome do diretório.
  # Errar pra este lado custa recomputação; errar pro outro custa correção.
  unlink(file.path(store$root, "stream"), recursive = TRUE)
  invisible(n)
}
