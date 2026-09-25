#' Registro: onde tipos, nós e adaptadores vivem depois de carregados.
#'
#' É um objeto, não estado global. O insumo escrevia em `globalenv()`, o que
#' (a) impede virar pacote e (b) faz todo teste compartilhar o mesmo estado —
#' um registro por teste é o que permite testar coleções em isolamento.
#' O registro default do processo existe só pra conveniência interativa.
#' @export
tr_registry <- function() {
  reg <- new.env(parent = emptyenv())
  reg$types <- list(); reg$nodes <- list(); reg$adapters <- list()
  reg$categories <- list(); reg$collections <- list()
  reg$migrations <- list(nodes = list(), params = list(), ports = list())
  structure(reg, class = "tr_registry")
}

.tr_default_registry <- tr_registry()

#' Carrega uma coleção num registro.
#'
#' Aceita o objeto `tr_collection` direto, ou o nome de um pacote que exporte
#' `trama_collection()`. Colisão de id é erro alto e cedo — mas agora é
#' improvável por construção, já que id é qualificado por coleção.
#' @export
tr_use <- function(collection, registry = .tr_default_registry) {
  pkg <- NULL
  if (is.character(collection)) {
    pkg <- collection
    fn <- get("trama_collection", envir = asNamespace(collection))
    collection <- fn()
  }
  if (!inherits(collection, "tr_collection")) {
    rlang::abort("'collection' não é uma tr_collection.", class = "tr_error_bad_collection")
  }
  col <- collection
  if (!is.null(registry$collections[[col$id]])) {
    rlang::abort(sprintf("Coleção '%s' já carregada.", col$id), class = "tr_error_duplicate_collection")
  }

  # Monta tudo num staging e só commita no fim.
  #
  # O registro é um environment mutável: escrever antes de validar deixava
  # metade da coleção registrada quando a validação falhava — e, pior, sem
  # entrada em `collections`, o que fazia a PRÓXIMA tentativa de carregar a
  # mesma coleção corrigida falhar com "id duplicado". Na prática o
  # desenvolvedor tinha que reiniciar a sessão a cada erro de digitação, o
  # que mata o ciclo rápido que é o método escolhido pra construir isso.
  types <- registry$types; nodes <- registry$nodes; adapters <- registry$adapters
  add <- function(target, slot, key, value) {
    if (!is.null(target[[key]])) {
      rlang::abort(sprintf("Id duplicado no registro (%s): '%s'.", slot, key),
                   class = "tr_error_duplicate_id")
    }
    target[[key]] <- value
    target
  }
  for (t in col$types)    types    <- add(types, "types", t$id, t)
  for (n in col$nodes)    nodes    <- add(nodes, "nodes", n$id, n)
  for (a in col$adapters) adapters <- add(adapters, "adapters", paste0(a$from, "->", a$to), a)

  # Migrações acumulam entre coleções: o id antigo de um nó que mudou de casa
  # pertence à coleção de ORIGEM, então duas coleções podem reivindicá-lo. Com
  # o mesmo destino é redundância inofensiva; com destinos diferentes o
  # documento abriria de um jeito ou de outro conforme a ordem do manifesto —
  # erro aqui, e não uma escolha silenciosa. `params`/`ports` são indexados por
  # id novo, que o namespace já prende a uma coleção só.
  mig <- registry$migrations %||% list(nodes = list(), params = list(), ports = list())
  cm <- col$migrations %||% list()
  for (old in names(cm$nodes)) {
    prev <- mig$nodes[[old]]
    if (!is.null(prev) && !identical(prev, cm$nodes[[old]])) {
      rlang::abort(sprintf("Migração conflitante: '%s' iria pra '%s' e pra '%s'.",
                           old, prev, cm$nodes[[old]]), class = "tr_error_bad_migration")
    }
    mig$nodes[[old]] <- cm$nodes[[old]]
  }
  for (k in names(cm$params)) mig$params[[k]] <- cm$params[[k]]
  for (k in names(cm$ports))  mig$ports[[k]]  <- cm$ports[[k]]

  # Toda porta de todo nó tem que referenciar um tipo que exista — checado
  # contra o staging, e não durante `tr_node()`, porque um nó pode
  # legitimamente consumir tipo de outra coleção já carregada.
  for (n in col$nodes) {
    for (p in c(n$inputs, n$outputs)) {
      if (is.null(types[[p$type]])) {
        rlang::abort(sprintf("Nó '%s' usa tipo desconhecido: '%s'.", n$id, p$type),
                     class = "tr_error_unknown_type")
      }
    }
  }
  for (a in col$adapters) {
    for (side in c(a$from, a$to)) if (is.null(types[[side]])) {
      rlang::abort(sprintf("Adaptador %s->%s usa tipo desconhecido: '%s'.", a$from, a$to, side),
                   class = "tr_error_unknown_type")
    }
  }

  registry$types <- types; registry$nodes <- nodes; registry$adapters <- adapters
  registry$migrations <- mig
  for (k in col$categories) registry$categories[[k$id]] <- k
  # `package` guardado separado do `id`: os dois só coincidem por acidente
  # (`trama.terrain` traz a coleção `terrain`), e é o nome do PACOTE que um
  # daemon precisa pra reconstruir o registro. NULL = coleção de globalenv,
  # que não é despachável.
  registry$collections[[col$id]] <- list(id = col$id, label = col$label,
                                         version = col$version,
                                         js = col$js, css = col$css, package = pkg)
  # Zera a memoização de fingerprints: carregar coleção é exatamente o momento
  # em que o código dos nós pode ter mudado (hot reload).
  registry$prints <- new.env(parent = emptyenv())
  invisible(registry)
}

#' Nomes de PACOTE das coleções carregadas — a moeda do manifesto.
#'
#' O `trama.json` lista pacotes (`trama.data`) e o registro indexa por id de
#' coleção (`data`): `names(registry$collections)` NÃO responde "o que este
#' editor tem" na moeda em que o manifesto pergunta, e os dois só coincidem por
#' acidente (ver `tr_use()`). Essa ruga é conhecimento do registro, e é por isso
#' que ela mora aqui: a cópia que estava em `tr_server()` punha regra de domínio
#' no barramento, e trocar o campo do manifesto faria criar e abrir projeto
#' divergirem em silêncio — um gravando o que o outro recusa.
#' @noRd
.tr_registry_packages <- function(registry) {
  unlist(lapply(registry$collections, function(col) col$package))
}

#' Busca um `tr_node` pelo id qualificado (`colecao/nome`); erro alto se não registrado.
#' @export
tr_get_node <- function(id, registry = .tr_default_registry) {
  spec <- registry$nodes[[id]]
  if (is.null(spec)) rlang::abort(sprintf("Nó desconhecido: '%s'.", id), class = "tr_error_unknown_node")
  spec
}

#' Busca um `tr_type` pelo id qualificado; erro alto se não registrado.
#' @export
tr_get_type <- function(id, registry = .tr_default_registry) {
  spec <- registry$types[[id]]
  if (is.null(spec)) rlang::abort(sprintf("Tipo desconhecido: '%s'.", id), class = "tr_error_unknown_type")
  spec
}

#' Nível 1 da API: a função do nó, chamável direto.
#'
#' O insumo mantinha essa propriedade (com teste de equivalência nível 1 vs.
#' grafo) e ela é o que impede a ferramenta de virar gaiola — qualquer nó é
#' inspecionável no console, sem documento e sem cache.
#' @export
tr_fn <- function(id, registry = .tr_default_registry) tr_get_node(id, registry)$fn

#' Compatibilidade de porta — CONSULTADA NUM LUGAR SÓ.
#'
#' Hoje: igualdade, ou existe adaptador registrado. O insumo espalhava
#' `identical(a$type, b$type)` por três lugares (validação de aresta,
#' `isValidConnection` no front, inserir-sobre-aresta), o que tornava qualquer
#' evolução de tipagem uma refatoração em três frentes. Sendo função, crescer
#' depois (subtipos, `list<T>`) custa mudar aqui.
#' @export
tr_compatible <- function(from_type, to_type, registry = .tr_default_registry) {
  if (identical(from_type, to_type)) return(TRUE)
  !is.null(registry$adapters[[paste0(from_type, "->", to_type)]])
}

#' Busca o adaptador `from_type -> to_type`, ou `NULL` se não houver (tipos iguais não precisam de adaptador).
#' @export
tr_adapter_for <- function(from_type, to_type, registry = .tr_default_registry) {
  registry$adapters[[paste0(from_type, "->", to_type)]]
}
