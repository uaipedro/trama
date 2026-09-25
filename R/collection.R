#' Agrupa tipos, nós, adaptadores e categorias de um domínio.
#'
#' Uma coleção é o que um pacote de domínio exporta. `js` aponta pra um módulo
#' ES em `inst/` que registra renderers e widgets no front — é o que permite
#' visualização nova sem tocar no núcleo.
#'
#' `label` é o nome que a coleção mostra na aba da paleta; sem ele, a aba
#' escreve o `id`, que serve mas não explica.
#' @param js Caminho, relativo ao `inst/` do pacote, do módulo ES da coleção.
#'   Opcional. Os arquivos vizinhos no mesmo diretório também são servidos, pro
#'   módulo poder importá-los por caminho relativo.
#' @param css Caminho, relativo ao `inst/` do pacote, de uma folha de estilo da
#'   coleção (as classes que os renderers dela usam). Opcional. Entra na página
#'   DEPOIS do `trama.css` do núcleo, então pode refinar regra do núcleo com a
#'   mesma especificidade — e é por isso mesmo que deve prefixar as próprias
#'   classes, pra não refinar sem querer.
#' @param migrations Renomes que a coleção declara pra documentos antigos
#'   abrirem já migrados, sem nó órfão. Lista com até três entradas, todas
#'   opcionais:
#'   * `nodes`: id antigo -> id novo (`list("multi/roc" = "models/roc")`). O id
#'     antigo pode ser de OUTRA coleção — é assim que um bloco muda de casa —,
#'     mas o destino tem que ser desta. Quando o nó novo precisa de um param
#'     pra se comportar como o antigo, o destino pode ser
#'     `list(to = "novo", params = list(nome = valor))`: os params entram SÓ
#'     nos nós cujo tipo era o id antigo, antes dos renomes de `params`, e não
#'     sobrescrevem param já presente. Em cadeia (a -> b -> c) os params de
#'     cada salto se acumulam na ordem. Ex.:
#'     `"multi/logistic_coefficients" = list(to = "models/coefficients",
#'     params = list(exponenciar = TRUE))`.
#'   * `params`: id NOVO do nó -> lista de renomes `antigo = list(to = "novo",
#'     value = function(v) ..., when = function(v) ...)`. `value` é opcional
#'     (identidade); `when` é um predicado sobre o valor antigo, e `value` só
#'     roda quando ele é verdadeiro. Dois casos especiais:
#'     - converter no lugar: `to` igual ao nome antigo. Aí `when` é
#'       obrigatório — é ele que reconhece o valor ainda no formato velho e
#'       deixa intocado o documento já migrado:
#'       `confianca = list(to = "confianca", when = is.character,
#'       value = function(v) as.numeric(sub("%", "", v)) / 100)`.
#'     - derivar vários params: `value` devolve lista NOMEADA, fundida aos
#'       params do nó (param já presente com esse nome vence):
#'       `intervalo = list(to = "intervalo", when = function(v) grepl("^IC ", v),
#'       value = function(v) list(intervalo = "IC", confianca = 0.9))`.
#'   * `ports`: id NOVO do nó -> lista `antiga = "nova"`, valendo pra entradas e
#'     saídas.
#'
#'   Aplicadas por `tr_doc_migrate()` quando um fluxo é aberto.
#' @export
tr_collection <- function(id, version = "0.0.0", label = id, types = list(),
                          nodes = list(), adapters = list(), categories = list(),
                          js = NULL, css = NULL, migrations = list()) {
  if (!grepl("^[a-z][a-z0-9_]*$", id)) {
    rlang::abort(sprintf("Id de coleção inválido: '%s'.", id), class = "tr_error_bad_id")
  }
  # Checado aqui, e não quando o app sobe: `tr_dependency_collection()` só
  # roda ao montar a UI, e um caminho errado ali some em silêncio (o asset
  # simplesmente não carrega). Absoluto é recusado porque o caminho é resolvido
  # por `system.file()` dentro do pacote — um `/home/...` funcionaria na máquina
  # do autor e em nenhuma outra.
  .tr_check_asset(js, "js", id)
  .tr_check_asset(css, "css", id)
  # Tudo que a coleção declara tem que viver sob o namespace dela. Sem esta
  # checagem, uma coleção poderia registrar `outra/coisa` e o conflito só
  # apareceria como comportamento estranho na coleção alheia.
  for (t in types) if (.tr_collection_of(t$id) != id) {
    rlang::abort(sprintf("Coleção '%s' declara tipo fora do próprio namespace: '%s'.", id, t$id),
                 class = "tr_error_foreign_id")
  }
  for (n in nodes) if (.tr_collection_of(n$id) != id) {
    rlang::abort(sprintf("Coleção '%s' declara nó fora do próprio namespace: '%s'.", id, n$id),
                 class = "tr_error_foreign_id")
  }
  migrations <- .tr_check_migrations(migrations, id)
  structure(list(id = id, label = label, version = version, types = types,
                 nodes = nodes, adapters = adapters, categories = categories,
                 js = js, css = css, migrations = migrations),
            class = "tr_collection")
}

#' Destino de uma migração de nó, na forma string ou `list(to =, params =)`.
#' @noRd
.tr_mig_to <- function(x) if (is.list(x)) x$to else x

#' Normaliza e valida `migrations` de `tr_collection()`.
#'
#' O namespace vale pro DESTINO (e pras chaves de `params`/`ports`, que já são
#' ids novos): a origem de um renome de nó é justamente o id que não existe
#' mais, e pode ter morado em outra coleção. Sem a checagem no destino, uma
#' coleção poderia "migrar" documentos pra dentro de um nó alheio.
#' @noRd
.tr_check_migrations <- function(m, id) {
  bad <- function(msg) rlang::abort(sprintf("Coleção '%s': migração inválida: %s", id, msg),
                                    class = "tr_error_bad_migration")
  if (!is.list(m) || length(setdiff(names(m), c("nodes", "params", "ports"))) > 0) {
    bad("esperado list(nodes = , params = , ports = ).")
  }
  own <- function(to, what) {
    if (!is.character(to) || length(to) != 1L || is.na(to)) bad(sprintf("%s tem que ser um id (string).", what))
    if (.tr_collection_of(to) != id) {
      rlang::abort(sprintf("Coleção '%s' declara migração fora do próprio namespace: '%s'.", id, to),
                   class = "tr_error_foreign_id")
    }
  }
  nodes <- m$nodes %||% list()
  # Chave repetida numa lista R não é erro de sintaxe: a segunda entrada
  # simplesmente seria ignorada por `[[`, e o autor nunca saberia qual valeu.
  if (anyDuplicated(names(nodes))) {
    bad(sprintf("'%s' tem dois destinos.", names(nodes)[anyDuplicated(names(nodes))]))
  }
  for (old in names(nodes)) {
    x <- nodes[[old]]
    if (is.list(x)) {
      # Forma com params injetados: só `to` e `params`, e params nomeados —
      # sem nome não há onde injetar, e um campo a mais é quase sempre typo.
      ps <- x$params %||% list()
      if (length(setdiff(names(x), c("to", "params"))) > 0 || is.null(names(x)) ||
          !is.list(ps) || (length(ps) > 0 && (is.null(names(ps)) || !all(nzchar(names(ps)))))) {
        bad(sprintf("'%s' espera \"novo\" ou list(to = \"novo\", params = list(nome = valor)).", old))
      }
      own(x$to, sprintf("o destino de '%s'", old))
    } else {
      own(x, sprintf("o destino de '%s'", old))
    }
  }
  params <- m$params %||% list()
  for (nid in names(params)) {
    own(nid, "a chave de params")
    for (p in names(params[[nid]])) {
      r <- params[[nid]][[p]]
      if (!is.list(r) || !is.character(r$to) || length(r$to) != 1L) {
        bad(sprintf("param '%s' de '%s' precisa de list(to = \"novo\").", p, nid))
      }
      for (f in c("value", "when")) if (!is.null(r[[f]]) && !is.function(r[[f]])) {
        bad(sprintf("'%s' do param '%s' de '%s' tem que ser função.", f, p, nid))
      }
      # Converter no lugar sem `when` rodaria `value` de novo a cada abertura
      # de um doc já migrado (o nome não muda, então nada distingue antigo de
      # novo). Exigir o predicado aqui transforma esse bug silencioso em erro
      # no registro, que o autor da coleção vê na hora.
      if (identical(r$to, p) && (is.null(r$value) || is.null(r$when))) {
        bad(sprintf("param '%s' de '%s' converte no lugar: exige 'value' e 'when'.", p, nid))
      }
    }
  }
  ports <- m$ports %||% list()
  for (nid in names(ports)) {
    own(nid, "a chave de ports")
    for (p in names(ports[[nid]])) if (!is.character(ports[[nid]][[p]])) {
      bad(sprintf("porta '%s' de '%s' tem que ir pra um nome (string).", p, nid))
    }
  }
  list(nodes = nodes, params = params, ports = ports)
}

#' @noRd
.tr_check_asset <- function(path, arg, id) {
  if (is.null(path)) return(invisible())
  ok <- is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path) &&
    !grepl("^(/|~|[A-Za-z]:)", path)
  if (!ok) {
    rlang::abort(sprintf(
      "Coleção '%s': '%s' tem que ser um caminho relativo ao inst/ do pacote (uma string), como \"trama/index.js\".",
      id, arg), class = "tr_error_bad_asset")
  }
  # O diretório do asset é servido INTEIRO (`all_files = TRUE` em
  # `tr_dependency_collection()`), então o caminho decide o que vai pro
  # navegador. `..` sobe pra fora da pasta pensada pra isso; a barra invertida
  # é separador só no Windows, e o mesmo caminho acharia arquivos diferentes
  # conforme a máquina; e um nome solto tem `dirname()` ".", que no
  # `system.file()` é a raiz do pacote instalado — tudo dele viraria público.
  if (any(strsplit(path, "/", fixed = TRUE)[[1]] == "..")) {
    rlang::abort(sprintf(
      "Coleção '%s': '%s' não pode ter '..' (\"%s\"); o caminho tem que ficar dentro do inst/ do pacote.",
      id, arg, path), class = "tr_error_bad_asset")
  }
  if (grepl("\\", path, fixed = TRUE)) {
    rlang::abort(sprintf(
      "Coleção '%s': '%s' usa barra invertida (\"%s\"); separe as pastas com '/', como \"trama/index.js\".",
      id, arg, path), class = "tr_error_bad_asset")
  }
  if (dirname(path) == ".") {
    rlang::abort(sprintf(
      "Coleção '%s': '%s' tem que estar numa subpasta do inst/ (\"%s\" não está), como \"trama/index.js\": a pasta inteira é servida ao navegador.",
      id, arg, path), class = "tr_error_bad_asset")
  }
  invisible()
}

#' Categoria: agrupamento na paleta e cor do cabeçalho dos cards.
#'
#' A cor deve vir do PAPEL do bloco no fluxo, não da coleção: assim um ajuste
#' de modelo e um ARIMA têm a mesma cor em qualquer coleção, e o tipo do que o
#' bloco produz fica nas portas. Os papéis seguem a ordem em que o analista
#' trabalha, e são genéricos, nada de domínio:
#'
#' - `origem`: traz dados para o fluxo (ler arquivo, dados de exemplo).
#' - `preparacao`: limpa, transforma ou reorganiza.
#' - `inspecao`: descreve o que chegou (resumo, gráfico do dado bruto).
#' - `ajuste`: constrói o objeto de análise (modelo, decomposição, PCA).
#' - `leitura`: lê um ajuste que já existe (quadro da ANOVA, coeficientes,
#'   médias, previsões, cargas). São os mostradores do modelo, não um passo a
#'   mais da sequência; por isso a cor é uma versão clara da do ajuste.
#' - `avaliacao`: julga um resultado (teste, pressupostos, métricas).
#' - `saida`: tira algo do fluxo (gravar arquivo).
#'
#' O papel da categoria é o padrão dos blocos dela; um bloco que faz outra
#' coisa declara o próprio em [tr_node()]. A cor de cada papel é do editor e
#' muda com o tema. `color` fica para quem não declara papel: vale nos dois
#' temas, e a tinta do título é escura.
#'
#' @param id Identificador da categoria, único no registro.
#' @param label Rótulo na paleta.
#' @param color Cor fixa, usada só quando `role` não é dado.
#' @param role Papel dos blocos da categoria no fluxo; um dos listados acima.
#' @export
tr_category <- function(id, label, color = "#6366f1", role = NULL) {
  if (is.null(role)) return(list(id = id, label = label, color = color))
  .tr_check_role(role, sprintf("Categoria '%s'", id))
  list(id = id, label = label, role = role)
}

tr_roles <- c("origem", "preparacao", "inspecao", "ajuste", "leitura", "avaliacao", "saida")

.tr_check_role <- function(role, quem) {
  if (!is.character(role) || length(role) != 1L || !role %in% tr_roles) {
    rlang::abort(sprintf(
      "%s: papel \"%s\" desconhecido; use um de: %s.",
      quem, paste(role, collapse = ", "), paste(tr_roles, collapse = ", ")),
      class = "tr_error_bad_role")
  }
  invisible(role)
}
