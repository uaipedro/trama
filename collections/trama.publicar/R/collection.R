#' Coleção de publicação: o pacote reprodutível do fluxo e a página.
#' @export
trama_collection <- function() {
  item_type <- trama::tr_type(
    "publicar/item", version = 1L, label = "Item de publica\u{E7}\u{E3}o", color = "#0d9488",
    summary = function(x) list(tipo = x$tipo, titulo = x$titulo,
                               arquivos = paste(names(x$arquivos), collapse = ", "),
                               erro = x$erro %||% ""))
  pacote_type <- trama::tr_type(
    "publicar/pacote", version = 1L, label = "Pacote de publica\u{E7}\u{E3}o", color = "#0f766e",
    preview = function(x, ctx) trama::tr_preview("publicar/conferencia", data = .tr_publicar_conferencia(x)),
    summary = function(x) list(itens = length(x$itens), avisos = length(x$avisos), erros = length(x$erros)),
    acoes = list(visualizar = .tr_publicar_visualizar, publicar = .tr_publicar_publicar,
                 acompanhar = .tr_publicar_acompanhar))
  trama::tr_collection(
    "publicar", version = "0.2.0", label = "Publicar",
    js = "trama/index.js", css = "trama/publicar.css",
    types = list(item_type, pacote_type),
    categories = list(trama::tr_category("publish", "Publicar", role = "saida")),
    adapters = list(
      trama::tr_adapter("data/table", "publicar/item", .tr_publicar_de_tabela),
      trama::tr_adapter("view/plot", "publicar/item", .tr_publicar_de_grafico),
      trama::tr_adapter("models/fit", "publicar/item", .tr_publicar_de_modelo)),
    nodes = list(.tr_publicar_no()))
}

.tr_publicar_no <- function() {
  trama::tr_node(
    "publicar/publish", fn = .tr_publicar_fn, version = 1L, label = "Publicar", category = "publish",
    description = "Monta o pacote reprodut\u{ED}vel do fluxo e a p\u{E1}gina com os itens ligados; o bot\u{E3}o Visualizar abre a pr\u{E9}via.",
    inputs = list(itens = trama::tr_port("publicar/item", multiple = TRUE, named = TRUE)),
    outputs = list(out = "publicar/pacote"),
    params = list(
      titulo = trama::tr_param_text("", "T\u{ED}tulo"),
      descricao = trama::tr_param_text("", "Descri\u{E7}\u{E3}o"),
      palavras_chave = trama::tr_param_text("", "Palavras-chave"),
      autores = trama::tr_param_text("", "Autores (ORCID)"),
      licenca = trama::tr_param_enum("CC-BY-4.0", .TR_PUBLICAR_LICENCAS, "Licen\u{E7}a"),
      licenca_codigo = trama::tr_param_enum("MIT", .TR_PUBLICAR_LICENCAS_CODIGO, "Licen\u{E7}a do c\u{F3}digo"),
      repositorio = trama::tr_param_text("", "Reposit\u{F3}rio"),
      privado = trama::tr_param_bool(FALSE, "Reposit\u{F3}rio privado"),
      versao = trama::tr_param_enum("menor", .TR_PUBLICAR_VERSOES, "Pr\u{F3}xima vers\u{E3}o"),
      layout = trama::tr_param("publicar_layout", "", "Layout da p\u{E1}gina"),
      aceitar_dados_treino = trama::tr_param_bool(FALSE, "Aceito publicar dados de treino embutidos")),
    help = .tr_publicar_modelo_texto("ajuda.md"))
}

.tr_publicar_fn <- function(itens = list(), titulo = "", descricao = "", palavras_chave = "", autores = "",
                            licenca = "CC-BY-4.0", licenca_codigo = "MIT", repositorio = "",
                            versao = "menor", layout = "", aceitar_dados_treino = FALSE, privado = FALSE) {
  tr_publicar_material(itens, titulo = titulo, descricao = descricao, palavras_chave = palavras_chave,
                       autores = autores, licenca = licenca, licenca_codigo = licenca_codigo,
                       repositorio = repositorio, versao = versao, layout = layout,
                       aceitar_dados_treino = aceitar_dados_treino, privado = privado)
}
