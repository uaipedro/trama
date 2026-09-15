#' Os nomes de ícone válidos saem do próprio sprite.
#'
#' Uma lista de nomes guardada à parte seria uma segunda cópia da mesma
#' verdade, livre pra divergir na primeira atualização do Lucide. Ler os
#' `<symbol id>` do arquivo que o front de fato usa elimina a divergência por
#' construção: o que a validação aceita é exatamente o que o navegador
#' encontra.
#'
#' Memoizado por sessão porque a leitura é de meio megabyte e a resposta não
#' muda enquanto o pacote não for reinstalado.
#' @noRd
.tr_icon_env <- new.env(parent = emptyenv())

#' Ausência de sprite e sprite ilegível são coisas DIFERENTES, e tratá-las
#' igual desarma a validação inteira em silêncio.
#'
#' Sem arquivo: instalação sem o asset. Devolve vazio, e `tr_icon()` aceita
#' qualquer nome — recusar a carga de uma coleção por causa de um enfeite seria
#' desproporcional, e o front já degrada pra bloco sem ícone.
#'
#' Com arquivo e zero símbolos: o parser e o formato divergiram — uma versão do
#' Lucide que troque aspas ou reordene atributos faz este regex render zero. Aí
#' NÃO se pode devolver vazio, porque vazio significaria "todo nome é válido", e
#' o typo que esta função existe pra pegar passaria batido em cada bloco do
#' catálogo. Erro alto, na carga, dizendo o que investigar.
#' @noRd
.tr_icon_parse <- function(path) {
  txt <- readLines(path, warn = FALSE)
  ids <- regmatches(txt, gregexpr('<symbol id="[^"]+"', txt))
  ids <- sub('^<symbol id="', "", unlist(ids, use.names = FALSE))
  nomes <- sub('"$', "", ids)
  if (!length(nomes)) {
    rlang::abort(sprintf(paste0("Sprite de ícones ilegível: '%s' existe mas nenhum <symbol id> foi ",
                                "encontrado. O formato do arquivo mudou e .tr_icon_parse() precisa ",
                                "acompanhar."), path),
                 class = "tr_error_bad_sprite")
  }
  nomes
}

.tr_icon_names <- function() {
  if (!is.null(.tr_icon_env$names)) return(.tr_icon_env$names)
  path <- system.file("www/vendor/lucide.svg", package = "trama")
  .tr_icon_env$names <- if (!nzchar(path) || !file.exists(path)) character(0) else .tr_icon_parse(path)
  .tr_icon_env$names
}

#' Declara o ícone de um bloco: um nome do conjunto Lucide, ou um SVG próprio.
#'
#' Exatamente um dos dois. `name` é conferido contra os ícones que o sprite
#' realmente tem — sem isso, `tr_icon("filter")` (que não existe; o nome é
#' `list-filter`) viraria um buraco silencioso no canto do card, descoberto
#' meses depois. Mesma régua de `tr_node()` sem `description`: erro alto, na
#' declaração.
#'
#' `svg` é o MIOLO de um `<svg>`, não um documento: o elemento e o `viewBox`
#' de 24×24 são do front, a coleção entrega a geometria. Não é barreira de
#' confiança — uma coleção é um pacote R instalado e já roda JS próprio — é só
#' o que faz o ícone de terceiro herdar cor e tamanho como os outros.
#' @export
tr_icon <- function(name = NULL, svg = NULL) {
  if (is.null(name) == is.null(svg)) {
    rlang::abort("tr_icon() precisa de 'name' OU 'svg', exatamente um dos dois.",
                 class = "tr_error_bad_icon")
  }
  x <- if (is.null(name)) svg else name
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    rlang::abort(sprintf("tr_icon(): '%s' tem que ser uma string única e não vazia.",
                         if (is.null(name)) "svg" else "name"),
                 class = "tr_error_bad_icon")
  }
  if (!is.null(svg)) return(structure(list(kind = "svg", value = svg), class = "tr_icon"))

  conhecidos <- .tr_icon_names()
  if (length(conhecidos) && !(name %in% conhecidos)) {
    rlang::abort(sprintf("Ícone desconhecido: '%s'. Veja os nomes em https://lucide.dev/icons.", name),
                 class = "tr_error_unknown_icon")
  }
  structure(list(kind = "set", value = name), class = "tr_icon")
}
