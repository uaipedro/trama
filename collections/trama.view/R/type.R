#' O tipo `view/plot` — e por que ele guarda o objeto, mas mostra um arquivo.
#'
#' Este é o tipo em que a regra geral do desenho ("arquivo só quando o dado não
#' cabe") cai do outro lado, e de propósito. Numa tabela, o preview são 25
#' linhas de um objeto de um milhão: o card é um RESUMO. Num gráfico, o card É
#' a coisa — e reduzir um gráfico a dado seria reescrever a gramática do
#' ggplot2 em JavaScript pra desenhar de novo, do lado errado, o que o R já
#' desenha bem. O arquivo aqui não é uma concessão: é o valor.
#'
#' O que a imagem CUSTA está pago em outro lugar: o lado maior é sempre 1600 px
#' e o card só a escala, então arrastar a alça não borra nada e não recomputa;
#' e o clique abre a mesma imagem em tela cheia pelo lightbox do `trama/image`.
#' A PROPORÇÃO, essa sim, é param — semântica, dentro da chave de cache, porque
#' mudá-la muda mesmo o desenho (o ggplot recoloca legenda, quebra rótulo de
#' eixo, remede a fonte relativa). Proporção e tamanho do card vivem em eixos
#' diferentes e nunca se cruzam: é o que mantém `ui.sizes` cosmético.
#'
#' O `store` guarda o ggplot com `saveRDS`, e não o PNG, por uma razão só:
#' `restore` no console tem que devolver um gráfico de verdade, editável e
#' somável (`p + labs(...)`). Um ggplot carrega o data.frame que recebeu — que
#' é o mesmo dado que o artefato do nó anterior já guardava — e as quosuras
#' construídas aqui usam `.data[[...]]`, que não depende de ambiente na hora de
#' desenhar. O teste de restauração desenha num device descartável justamente
#' pra travar essa premissa.
#'
#' O `store` é o FUNIL: o guard mora aqui, e não em cada `fn`. Sem ele, um nó
#' que devolvesse a tabela por engano gravaria, o `preview` tentaria imprimir e
#' o card mostraria um erro de device — longe da causa.
view_plot_type <- function() {
  trama::tr_type(
    "view/plot", version = 1L, label = "Gráfico", color = "#f472b6",
    ext = "rds",
    store = function(x, path) {
      if (!inherits(x, "ggplot")) {
        rlang::abort(
          sprintf("O nó produziu um objeto '%s', não um gráfico.", class(x)[[1]]),
          class = "tr_view_error_not_a_plot")
      }
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) {
      d <- attr(x, "tr_view_dim") %||% c(8, 4.5)
      # `%g` e não `%.0f`: metade das proporções tem altura fracionária (16:9 dá
      # 4,5 pol) e `%.0f` arredonda para par, escrevendo "8 x 4 pol" — um resumo
      # que mente sobre a forma da imagem que está logo ao lado dele no card.
      list(proporcao = sprintf("%g x %g pol", d[[1]], d[[2]]),
           camadas = length(x$layers),
           linhas = if (is.data.frame(x$data)) nrow(x$data) else NA_integer_)
    },
    # O PNG mora em `tr_view_render()` (R/api.R), exportado, porque os tipos
    # de outras coleções cujo card é um gráfico gravam o mesmo arquivo.
    preview = function(x, ctx) tr_view_render(x, ctx)
  )
}

# Local, e não `rlang::`%||%`` ou o do base: o `%||%` do base só existe a
# partir do R 4.4 e o DESCRIPTION não exige versão mínima, e importar o do
# rlang custaria uma linha de NAMESPACE por um operador de uma linha. É o mesmo
# que o núcleo faz em `R/utils.R`.
`%||%` <- function(x, y) if (is.null(x)) y else x
