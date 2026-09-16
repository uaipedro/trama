#' Declara um tipo de dado (o que trafega numa porta).
#'
#' **O tipo carrega a própria persistência** (`store`/`restore`/`ext`). É aqui
#' que "SpatRaster é ponteiro C++ e não sobrevive a saveRDS" se resolve — não
#' no store, que permanece cego ao que guarda.
#'
#' O insumo despachava serialização por classe R (`inherits(x, "SpatRaster")`),
#' e isso não escala nem dentro de um domínio só: `Heightmap` e `Field` são
#' ambos `SpatRaster`, e foi exatamente essa colisão que obrigou a existência
#' de um nó identidade (`height_as_field`) só pra reetiquetar um no outro.
#' Tipo é nominal; classe R é detalhe de implementação de um tipo.
#'
#' `preview(x, ctx)` devolve um artefato — `tr_preview()` — cujo `renderer` é
#' um id de string resolvido no front. Dispatch por id, nunca por `inherits()`:
#' é o que permite uma coleção trazer visualização nova sem tocar no núcleo.
#'
#' Sem `store`/`restore`, cai em RDS — suficiente pra tipo que é objeto R
#' comum (data.frame, lista, escalar).
#' @examples
#' # Sem `store`/`restore` o artefato cai em RDS, que basta para um escalar.
#' num <- tr_type("exemplo/num", label = "Numero", color = "#818cf8",
#'                preview = function(x, ctx) {
#'                  tr_preview("trama/keyvalue", data = list(valor = x))
#'                },
#'                summary = function(x) list(valor = x))
#' num$summary(7)
#'
#' reg <- tr_registry()
#' tr_use("trama", registry = reg)
#' tr_get_type("demo/num", reg)$label
#' @export
tr_type <- function(id, version = 1L, label = NULL, color = "#64748b",
                    store = NULL, restore = NULL, ext = "rds",
                    preview = NULL, summary = NULL) {
  .tr_check_id(id, "id de tipo")
  if (!is.null(store) && !is.function(store)) {
    rlang::abort(.tr_msg("type.bad_store", id), class = "tr_error_bad_type")
  }
  if (is.null(store) != is.null(restore)) {
    rlang::abort(
      .tr_msg("type.store_restore_pair", id),
      class = "tr_error_bad_type"
    )
  }
  structure(list(
    id = id, version = as.integer(version), label = label %||% id, color = color,
    store = store, restore = restore, ext = ext,
    preview = preview, summary = summary
  ), class = "tr_type")
}

#' Artefato de preview: o que o worker grava e o front desenha.
#'
#' `renderer` é o id do componente JS que sabe desenhar isso. `data` viaja no
#' payload (dado já reduzido — downsample, bins, head); `files` são caminhos
#' relativos no store, pra quando o dado não cabe (imagem raster).
#' Regra: **arquivo só quando o dado não cabe.**
#' @examples
#' tr_preview("trama/keyvalue", data = list(valor = 7))
#'
#' # O preview de um tipo registrado sai pronto para virar JSON.
#' reg <- tr_registry()
#' tr_use("trama", registry = reg)
#' tabela <- tr_fn("demo/tabela", reg)()
#' p <- tr_get_type("demo/tabela", reg)$preview(tabela, NULL)
#' p$renderer
#' p$data$columns
#' @export
tr_preview <- function(renderer, data = NULL, files = NULL) {
  list(renderer = renderer, data = data, files = files)
}

#' Adaptador de tipo: conversão registrada entre dois tipos.
#'
#' Inserido pelo motor NA ARESTA (vira marca visual, não nó) e entra na chave
#' de cache. É o que dispensa nós identidade como o `height_as_field` do
#' insumo, mantendo o motor cego a semântica de domínio.
#' @export
tr_adapter <- function(from, to, fn) {
  .tr_check_id(from, "tipo de origem"); .tr_check_id(to, "tipo de destino")
  if (!is.function(fn)) rlang::abort(.tr_msg("type.bad_adapter"), class = "tr_error_bad_adapter")
  structure(list(from = from, to = to, fn = fn), class = "tr_adapter")
}
