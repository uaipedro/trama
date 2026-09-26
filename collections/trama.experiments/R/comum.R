# O que todo nó de planejamento faz antes do trabalho: conferir params, ler a
# lista de fatores, sortear sem mexer na semente de ninguém e montar a página
# de ajuda.
#
# Mora num lugar só porque as recusas precisam ser AS MESMAS em toda
# estrutura: o nome de fator que o DBC recusa não pode passar no fatorial.
# (Os nós de análise têm os seus auxiliares próprios, `.tr_exp_an_*`.)

.TR_EXP_COR <- "#65a30d"
.TR_EXP_CINZA <- "#8b949e"

#' Colunas estruturais: nomes que o plano escreve por conta própria e que,
#' por isso, nenhum fator pode usar. Um fator chamado `bloco` num DBC
#' sobrescreveria a coluna dos blocos em silêncio.
#' @noRd
.TR_EXP_RESERVADOS <- c("unidade", "ordem", "padrao", "bloco", "parcela", "subparcela", "linha",
                        "coluna", "individuo", "tempo", "periodo", "sequencia", "local",
                        "repeticao", "tipo_ponto", "faixa_linha", "faixa_coluna", "posicao", "residual")

#' O campo veio preenchido?
#' @noRd
.tr_exp_preenchido <- function(valor) {
  length(valor) > 0L && !is.na(valor[[1]]) && nzchar(trimws(as.character(valor)[[1]]))
}

#' Confere um enum e devolve o valor. O enum protege pela UI; o `fn` é nível 1
#' e chamável do console, onde um `switch` sem default devolveria NULL.
#' @noRd
.tr_exp_enum <- function(valor, aceitos, param) {
  if (length(valor) != 1L || is.na(valor) || !valor %in% aceitos) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                          "Param '%s': valor inválido '%s'. Aceitos: %s.",
                          param, paste(as.character(valor), collapse = ", "), paste(aceitos, collapse = ", "))
  }
  valor
}

#' Inteiro dentro de uma faixa.
#' @noRd
.tr_exp_int <- function(valor, param, min = 0L, max = Inf) {
  ok <- length(valor) == 1L && is.numeric(valor) && !is.na(valor) && valor == round(valor) &&
    valor >= min && valor <= max
  if (!ok) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                          "Param '%s': tem que ser um inteiro entre %g e %g (veio '%s').",
                          param, min, max, paste(as.character(valor), collapse = ", "))
  }
  as.integer(valor)
}

#' Separa "a, b, c" em partes aparadas, sem vazios.
#' @noRd
.tr_exp_split <- function(x, sep = ",") {
  if (!.tr_exp_preenchido(x)) return(character())
  p <- trimws(unlist(strsplit(as.character(x), sep, fixed = TRUE)))
  p[nzchar(p)]
}

#' Lê a lista de fatores: `"irrigacao: baixa, alta; variedade: A, B, C"`.
#'
#' Um item sem `:` é um fator só com nome (o fracionado e o composto central
#' não precisam de níveis: trabalham na escala codificada −1/+1). Um item com
#' um número só (`"dose: 4"`) vira 4 níveis `1..4`. Devolve uma lista nomeada
#' de vetores de nível (ou `NULL`, sem níveis).
#' @noRd
.tr_exp_fatores <- function(fatores) {
  if (!.tr_exp_preenchido(fatores)) {
    .tr_experiments_abort("tr_experiments_error_blank_param",
                          "Param 'fatores': campo obrigatório em branco. Ex.: 'variedade: A, B, C'.")
  }
  itens <- .tr_exp_split(gsub("\n", ";", fatores, fixed = TRUE), ";")
  out <- list()
  for (it in itens) {
    partes <- strsplit(it, ":", fixed = TRUE)[[1]]
    if (length(partes) > 2L) {
      .tr_experiments_abort("tr_experiments_error_bad_factors",
                            "Param 'fatores': '%s' tem mais de um ':'. Use 'nome: nível, nível'.", it)
    }
    nome <- trimws(partes[[1]])
    if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", nome)) {
      .tr_experiments_abort("tr_experiments_error_bad_factors",
                            "Param 'fatores': '%s' não serve como nome de coluna (letra, depois letras, dígitos ou _).",
                            nome)
    }
    if (nome %in% .TR_EXP_RESERVADOS || nome %in% names(out)) {
      .tr_experiments_abort("tr_experiments_error_reserved_name",
                            "Param 'fatores': '%s' %s. Nomes estruturais: %s.", nome,
                            if (nome %in% names(out)) "aparece duas vezes" else "é nome de coluna estrutural do plano",
                            paste(.TR_EXP_RESERVADOS, collapse = ", "))
    }
    niveis <- if (length(partes) == 2L) .tr_exp_split(partes[[2]]) else NULL
    if (length(niveis) == 1L && grepl("^[0-9]+$", niveis)) niveis <- as.character(seq_len(as.integer(niveis)))
    if (!is.null(niveis) && anyDuplicated(niveis)) {
      .tr_experiments_abort("tr_experiments_error_bad_factors",
                            "Param 'fatores': o fator '%s' repete nível (%s).", nome, paste(niveis, collapse = ", "))
    }
    out[nome] <- list(niveis)
  }
  if (!length(out)) {
    .tr_experiments_abort("tr_experiments_error_blank_param", "Param 'fatores': nenhum fator declarado.")
  }
  out
}

#' Roda `expr` com semente própria, sem mexer na do usuário.
#'
#' O `.seed` vem do núcleo (nó estocástico). Um `set.seed()` solto mudaria o
#' sorteio de todo código que rodasse depois no mesmo processo. O gerador é
#' FIXADO (Mersenne-Twister, Inversion, Rejection) e registrado no plano: a
#' mesma semente com outro `RNGkind` daria outro sorteio.
#' @noRd
.tr_exp_com_semente <- function(seed, expr) {
  tem <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (tem) antigo <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  antigo_kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(antigo_kind))
    if (tem) assign(".Random.seed", antigo, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(as.integer(seed))
  force(expr)
}

.TR_EXP_METODO_RNG <- "Mersenne-Twister / Inversion / Rejection, sample.int()"

#' Permutação aleatória de um vetor. `sample(x)` com `x` numérico de tamanho 1
#' sortearia de `1:x` — a armadilha clássica; `sample.int` não tem esse caso.
#' @noRd
.tr_exp_perm <- function(x) x[sample.int(length(x))]

#' As páginas de ajuda, com as seções na ordem de `?funcao` e, nos nós
#' estocásticos, a do sorteio. Pressupostos e referências vêm estruturados
#' (`.tr_exp_doc`), não da prosa.
#' @noRd
.tr_exp_ajuda <- function(descricao, parametros, valor, exemplos, veja,
                          grafico = FALSE, semente = FALSE) {
  paste0("## Descrição\n\n", trimws(descricao),
         "\n\n## Parâmetros\n\n", trimws(parametros),
         "\n\n## Valor\n\n", trimws(valor),
         "\n\n## Exemplos\n\n```r\n", trimws(exemplos), "\n```",
         "\n\n## Veja também\n\n", trimws(veja),
         if (semente) paste0("\n", .tr_exp_ajuda_semente()) else "",
         if (grafico) paste0("\n", trama.view::tr_view_help_appearance()) else "")
}

#' A seção que explica o sorteio dos nós estocásticos.
#' @noRd
.tr_exp_ajuda_semente <- function() {
  "
### O sorteio

O bloco é **estocástico**: a semente é do card, e não da sessão. O mesmo
documento sorteia sempre a mesma alocação — o croqui que vai a campo amanhã é
o de hoje —, e trocar a semente sorteia outra. O plano guarda a semente, o
gerador e a versão da coleção, que é o que basta para reproduzir o sorteio. A
semente do console (`set.seed()`) não é tocada.
"
}

#' Os cosméticos da `view`, com a proporção padrão própria do gráfico.
#' @noRd
.tr_exp_props <- function(..., .aspecto = "16:9") {
  ps <- trama.view::tr_view_props(...)
  ps$aspecto$default <- .aspecto
  ps
}
