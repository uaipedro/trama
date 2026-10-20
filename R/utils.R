#' O relógio do motor, numa costura só.
#'
#' Existe para ser mockável. `Sys.time()` é função base, e
#' `testthat::local_mocked_bindings()` só substitui binding que o pacote define
#' ou importa — então os testes de cadência e de `duration` só podiam ser
#' escritos com sono de verdade e limite de parede. Isso mede a velocidade da
#' máquina em vez da aritmética, e piscou vermelho uma vez em cinco execuções
#' numa suíte de mil e quinhentos testes: vermelho intermitente sem nome queima
#' o tempo de quem investiga e, pior, ensina a ignorar vermelho.
#'
#' Só o driver da região usa esta costura, porque é lá que o tempo é LÓGICA
#' (estrangulamento de publicação, `duration` que exclui as leituras de fora) e
#' não só registro. O resto do pacote segue chamando `Sys.time()` direto: um
#' `created` de handle não decide nada.
#' @noRd
.tr_now <- function() Sys.time()

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Ids são sempre qualificados por coleção (`coleção/nome`).
#'
#' No insumo o id era global e o registro abortava com "id duplicado" — o que
#' significa que carregar duas coleções razoáveis viraria erro de carga:
#' `normalize` é nome óbvio demais pra pertencer a um domínio só. O id é
#' chave estrangeira em documento salvo, então qualificar depois invalidaria
#' todo documento existente; por isso é regra desde a primeira linha.
#' @noRd
.tr_check_id <- function(id, what = "id") {
  if (!is.character(id) || length(id) != 1L || is.na(id)) {
    rlang::abort(sprintf("%s deve ser uma string.", what), class = "tr_error_bad_id")
  }
  if (!grepl("^[a-z][a-z0-9_]*/[a-z_][a-z0-9_]*$", id)) {
    rlang::abort(
      sprintf("%s inválido: '%s'. Formato esperado: 'colecao/nome' (minúsculas, _ e dígitos).", what, id),
      class = "tr_error_bad_id"
    )
  }
  invisible(id)
}

.tr_collection_of <- function(id) sub("/.*$", "", id)

#' Texto sem acento
#'
#' Troca as letras acentuadas do português (e as do Latin-1 que aparecem em
#' nome de coluna, de pasta ou de bloco) pela letra sem acento e descarta o que
#' ainda não for ASCII. Caixa, espaço e pontuação ficam como estão.
#'
#' Existe porque `iconv(x, to = "ASCII//TRANSLIT")` não é portátil: a
#' transliteração é de cada sistema, e no macOS o slug de "Ação" sai "ac-ao". Aqui o
#' mapa é explícito, então o resultado é o mesmo em Linux, macOS e Windows,
#' em qualquer locale. Coleções e plugins que precisam de slug ou de nome
#' limpo usam esta função.
#'
#' @param x Vetor de texto (outros tipos passam por `as.character()`). `NA`
#'   continua `NA`.
#' @return Vetor de texto do mesmo tamanho, só com caracteres ASCII.
#' @examples
#' tr_sem_acento(c("Ação & Reação!", "Regressão linear", "produção"))
#' @export
tr_sem_acento <- function(x) {
  if (!is.character(x)) x <- as.character(x)
  if (!length(x)) return(character())
  de <- paste0(
    "\u00e1\u00e0\u00e2\u00e3\u00e4\u00e5\u00c1\u00c0\u00c2\u00c3\u00c4\u00c5",
    "\u00e9\u00e8\u00ea\u00eb\u00c9\u00c8\u00ca\u00cb\u00ed\u00ec\u00ee\u00ef",
    "\u00cd\u00cc\u00ce\u00cf\u00f3\u00f2\u00f4\u00f5\u00f6\u00d3\u00d2\u00d4",
    "\u00d5\u00d6\u00fa\u00f9\u00fb\u00fc\u00da\u00d9\u00db\u00dc\u00e7\u00c7",
    "\u00f1\u00d1\u00fd\u00ff\u00dd\u00aa\u00ba"
  )
  x <- chartr(de, "aaaaaaAAAAAAeeeeEEEEiiiiIIIIoooooOOOOOuuuuUUUUcCnNyyYao", enc2utf8(x))
  iconv(x, "UTF-8", "ASCII", sub = "")
}
