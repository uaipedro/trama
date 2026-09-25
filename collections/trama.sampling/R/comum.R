# O que todo nó da coleção faz antes do trabalho: ler colunas, conferir faixas,
# traduzir a confiança, sortear sem mexer na semente de ninguém e montar a
# página de ajuda.
#
# Mora num lugar só porque as recusas precisam ser AS MESMAS em todo nó: a
# coluna com maiúscula que o `sampling/stratified` recusa não pode passar no
# `sampling/mean`.

.TR_SAMPLING_COR <- "#0891b2"
.TR_SAMPLING_COR_2 <- "#f59e0b"
.TR_SAMPLING_CINZA <- "#8b949e"
.TR_SAMPLING_CONFIANCAS <- c("90%", "95%", "99%")

#' Separa "a, b, c" em nomes, aparando espaços. Vazio vira `character()`.
#' @noRd
.tr_sampling_split <- function(cols) {
  if (!length(cols) || all(is.na(cols))) return(character())
  x <- trimws(unlist(strsplit(paste(as.character(cols), collapse = ","), ",", fixed = TRUE)))
  unique(x[nzchar(x)])
}

#' Confere que as colunas citadas existem, e devolve os nomes.
#'
#' Mesma recusa das irmãs a `any_of()`: descartar nome inexistente em silêncio
#' faria `Regiao` com maiúscula virar uma amostra sem estratos.
#' @noRd
.tr_sampling_cols <- function(dados, cols, param, minimo = 1L, maximo = Inf) {
  nomes <- .tr_sampling_split(cols)
  if (!length(nomes) && minimo > 0L) {
    .tr_sampling_abort("tr_sampling_error_blank_param",
                       "Param '%s': campo obrigatório em branco. Preencha-o no card.", param)
  }
  faltam <- setdiff(nomes, names(dados))
  if (length(faltam)) {
    .tr_sampling_abort("tr_sampling_error_unknown_column",
                       "Param '%s': coluna inexistente: %s. Disponíveis: %s.",
                       param, paste(faltam, collapse = ", "), paste(names(dados), collapse = ", "))
  }
  if (length(nomes) > maximo) {
    .tr_sampling_abort("tr_sampling_error_bad_option",
                       "Param '%s': pede no máximo %d coluna(s), e vieram %d (%s).",
                       param, as.integer(maximo), length(nomes), paste(nomes, collapse = ", "))
  }
  nomes
}

#' UMA coluna obrigatória.
#' @noRd
.tr_sampling_col <- function(dados, col, param) .tr_sampling_cols(dados, col, param, 1L, 1L)

#' UMA coluna opcional: `NULL` quando o campo está em branco.
#' @noRd
.tr_sampling_col_opcional <- function(dados, col, param) {
  nomes <- .tr_sampling_cols(dados, col, param, 0L, 1L)
  if (length(nomes)) nomes else NULL
}

#' Coluna numérica, ou erro dizendo o que fazer.
#' @noRd
.tr_sampling_numerica <- function(dados, col, param) {
  if (!is.numeric(dados[[col]])) {
    .tr_sampling_abort("tr_sampling_error_not_numeric",
                       "Param '%s': a coluna '%s' é %s, e aqui precisa ser numérica. Converta num 'data/convert'.",
                       param, col, class(dados[[col]])[[1]])
  }
  col
}

#' "95%" -> 0,95. A confiança é enum porque 94% não é pergunta de ninguém.
#' @noRd
.tr_sampling_conf <- function(confianca, param = "confianca") {
  confianca <- .tr_sampling_enum(confianca, .TR_SAMPLING_CONFIANCAS, param)
  as.numeric(sub("%", "", confianca, fixed = TRUE)) / 100
}

#' O z bilateral da confiança.
#' @noRd
.tr_sampling_z <- function(conf) stats::qnorm(1 - (1 - conf) / 2)

.TR_SAMPLING_DISTRIBUICOES <- c("t", "z")

#' O quantil bilateral: t com `gl` graus de liberdade, ou z com `gl = Inf`.
#'
#' É o mesmo quantil que as estimativas usam (t com gl = UPAs − estratos): a
#' margem planejada tem de ser a que o card de `sampling/mean` vai mostrar.
#' @noRd
.tr_sampling_q <- function(conf, gl = Inf) {
  ifelse(is.finite(gl), stats::qt(1 - (1 - conf) / 2, pmax(gl, 1)), stats::qnorm(1 - (1 - conf) / 2))
}

#' `"t"` ou `"z"`, validado.
#' @noRd
.tr_sampling_distrib <- function(distribuicao) {
  .tr_sampling_enum(distribuicao, .TR_SAMPLING_DISTRIBUICOES, "distribuicao")
}

#' Resolve o n cujo quantil depende do próprio n (t com gl do desenho).
#'
#' `n_de_q(q)` devolve o n (inteiro) que a fórmula pede com o quantil q;
#' `gl_de_n(n)`, os gl que esse n dá. A resposta é o MENOR n que se sustenta,
#' n ≥ n_de_q(q(gl(n))): como o t cai com n, a condição é monótona, e a busca
#' sobe a partir do n de z (que é sempre um limite inferior), e nunca abaixo
#' do menor n com gl ≥ 1.
#' @return lista `n`, `q`, `gl`.
#' @noRd
.tr_sampling_resolver_t <- function(conf, distribuicao, n_de_q, gl_de_n) {
  q <- .tr_sampling_z(conf)
  n <- n_de_q(q)
  if (distribuicao == "z") return(list(n = n, q = q, gl = Inf))
  # t com 0 gl não existe: começa no menor n que dá gl ≥ 1 (2 na AAS, H + 1
  # na estratificada, 2 conglomerados).
  while (gl_de_n(n) < 1) n <- n + 1L
  repeat {
    q <- .tr_sampling_q(conf, gl_de_n(n))
    if (n_de_q(q) <= n) break
    n <- n + 1L
  }
  list(n = n, q = q, gl = gl_de_n(n))
}

#' Número com vírgula, que é como a nota e os passos falam.
#'
#' Inteiro sai inteiro ("N = 2.400", e não "2.400,00"): é contagem, e a casa
#' decimal sugeriria uma precisão que não existe. O resto, com `digitos`
#' significativos e sem zeros à direita.
#' @noRd
.tr_sampling_fmt <- function(x, digitos = 3L) {
  vapply(x, function(v) {
    if (is.na(v)) return("—")
    if (abs(v - round(v)) < 1e-9 && abs(v) >= 1) {
      return(formatC(round(v), format = "d", big.mark = ".", decimal.mark = ","))
    }
    trimws(formatC(signif(v, digitos), format = "fg", digits = digitos, decimal.mark = ",", big.mark = "."))
  }, "")
}

#' Percentual com vírgula: 0,8 -> "80%".
#' @noRd
.tr_sampling_pct <- function(x) paste0(.tr_sampling_fmt(100 * x), "%")

#' Junta pedaços de nota não vazios com "; ".
#' @noRd
.tr_sampling_nota <- function(...) {
  x <- unlist(list(...))
  x <- x[!is.na(x) & nzchar(x)]
  paste(x, collapse = "; ")
}

#' Roda `expr` com semente própria, sem mexer na do usuário.
#'
#' O `.seed` vem do núcleo (nó estocástico). Sortear com `set.seed()` solto
#' mudaria o sorteio de qualquer outro código da sessão — e, no app, o de todo
#' nó que rodasse depois no mesmo processo.
#' @noRd
.tr_sampling_com_semente <- function(seed, expr) {
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

#' As páginas de ajuda, com as seções na ordem de `?funcao`.
#'
#' Montadas por função, e não à mão, pelo mesmo motivo das irmãs: com vinte
#' nós, a seção esquecida em um deles é certa. `cv` acrescenta a leitura da
#' régua do CV; `semente`, a explicação do sorteio reproduzível.
#' @noRd
.tr_sampling_ajuda <- function(descricao, parametros, valor, exemplos, veja, grafico = FALSE,
                               cv = FALSE, semente = FALSE) {
  paste0("## Descrição\n\n", trimws(descricao),
         "\n\n## Parâmetros\n\n", trimws(parametros),
         "\n\n## Valor\n\n", trimws(valor),
         "\n\n## Exemplos\n\n```r\n", trimws(exemplos), "\n```",
         "\n\n## Veja também\n\n", trimws(veja),
         if (semente) paste0("\n", .tr_sampling_ajuda_semente()) else "",
         if (cv) paste0("\n", .tr_sampling_ajuda_cv()) else "",
         if (grafico) paste0("\n", trama.view::tr_view_help_appearance()) else "")
}

#' A seção que explica o card da estimativa.
#'
#' Segue `inst/trama/index.js`: régua de 0 a 40%, faixas 5/15/30.
#' @noRd
.tr_sampling_ajuda_cv <- function() {
  "
### Como ler o card (comum a todas as estimativas)

- **O número grande** é a estimativa; embaixo, a margem de erro (metade do
  intervalo) na confiança escolhida.
- **A barra do intervalo** mostra onde a estimativa está dentro do intervalo;
  com **Por**, uma linha por domínio, todas na mesma escala.
- **A régua do CV** é o coeficiente de variação da estimativa (erro padrão ÷
  estimativa), de 0 a 40%. As faixas são as que institutos de estatística
  costumam usar: até 5% **ótima**, até 15% **boa**, até 30% **regular**, e
  acima disso **imprecisa**. São convenção, não teorema: um CV de 20% pode
  bastar para decidir e não bastar para publicar.
- **deff** é o efeito do desenho: a variância desta estimativa dividida pela de
  uma amostra aleatória simples do mesmo tamanho. Abaixo de 1, o desenho ganhou
  (estratificação boa); acima, perdeu (conglomerados parecidos por dentro).

Proporção perto de zero tem CV grande por construção — 2% ± 1% é CV de 50% —,
e aí vale ler a margem de erro, e não a régua.
"
}

#' A seção que explica o sorteio dos nós estocásticos.
#' @noRd
.tr_sampling_ajuda_semente <- function() {
  "
### O sorteio

O bloco é **estocástico**: a semente é do card, e não da sessão. O mesmo
documento sorteia sempre a mesma amostra — a análise que se abre amanhã é a de
hoje —, e trocar a semente no card sorteia outra, recalculando só o que vem
depois. A semente do console (`set.seed()`) não é tocada.
"
}

#' Os cosméticos da `view`, com a proporção padrão própria do gráfico.
#' @noRd
.tr_sampling_props <- function(..., .aspecto = "16:9") {
  ps <- trama.view::tr_view_props(...)
  ps$aspecto$default <- .aspecto
  ps
}
