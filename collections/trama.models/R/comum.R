# O que todo modelo da coleção faz antes de ajustar: ler a fórmula ou montá-la
# das colunas, conferir as colunas, descartar à vista as linhas incompletas e
# transformar em fator o que é tratamento.
#
# Mora num lugar só porque as recusas precisam ser AS MESMAS em todo nó. Se o
# `models/lm` aceitasse a coluna com acento que o `models/anova_dbc` recusa, o
# mesmo dado contaria duas histórias.

.TR_MODELS_COR <- "#14b8a6"
.TR_MODELS_COR_2 <- "#f472b6"
.TR_MODELS_CINZA <- "#8b949e"

#' Separa "a, b, c" em nomes, aparando espaços. Vazio vira `character()`.
#' @noRd
.tr_models_split <- function(cols) {
  if (!length(cols) || all(is.na(cols))) return(character())
  x <- trimws(unlist(strsplit(paste(as.character(cols), collapse = ","), ",", fixed = TRUE)))
  unique(x[nzchar(x)])
}

#' Confere que as colunas citadas existem, e devolve os nomes.
#'
#' Mesma recusa das irmãs a `any_of()`: descartar nome inexistente em silêncio
#' faria `Bloco` com maiúscula virar um DIC sem ninguém perceber.
#' @noRd
.tr_models_cols <- function(dados, cols, param, minimo = 1L, maximo = Inf) {
  nomes <- .tr_models_split(cols)
  if (!length(nomes) && minimo > 0L) {
    .tr_models_abort("tr_models_error_blank_param",
                     "Param '%s': campo obrigatório em branco. Preencha-o no card.", param)
  }
  faltam <- setdiff(nomes, names(dados))
  if (length(faltam)) {
    .tr_models_abort("tr_models_error_unknown_column",
                     "Param '%s': coluna inexistente: %s. Disponíveis: %s.",
                     param, paste(faltam, collapse = ", "), paste(names(dados), collapse = ", "))
  }
  if (length(nomes) < minimo || length(nomes) > maximo) {
    .tr_models_abort("tr_models_error_bad_option",
                     "Param '%s': pede %s coluna(s), e vieram %d (%s).", param,
                     if (is.finite(maximo) && maximo != minimo) sprintf("de %d a %d", minimo, maximo)
                     else if (is.finite(maximo)) as.character(minimo) else sprintf("pelo menos %d", minimo),
                     length(nomes), paste(nomes, collapse = ", "))
  }
  nomes
}

#' UMA coluna obrigatória.
#' @noRd
.tr_models_col <- function(dados, col, param) {
  .tr_models_obrigatorio(col, param)
  .tr_models_cols(dados, col, param, minimo = 1L, maximo = 1L)
}

#' Coluna numérica, ou erro dizendo o que fazer.
#' @noRd
.tr_models_numerica <- function(dados, col, param) {
  if (!is.numeric(dados[[col]])) {
    .tr_models_abort("tr_models_error_not_numeric",
                     "Param '%s': a coluna '%s' é %s, e aqui precisa ser numérica. Converta num 'data/convert'.",
                     param, col, class(dados[[col]])[[1]])
  }
  col
}

#' Nome pronto para entrar numa fórmula: crase quando não é nome sintático.
#'
#' `peso seco` e `produção` são nomes comuns em planilha de campo; sem crase a
#' fórmula montada não parseia, e o erro culparia a pessoa por algo que ela não
#' digitou.
#' @noRd
.tr_models_bt <- function(x) {
  ifelse(make.names(x) == x, x, paste0("`", gsub("`", "\\\\`", x), "`"))
}

#' Lê a fórmula digitada e confere as colunas que ela cita.
#'
#' O ambiente da fórmula vira o global: a fórmula criada aqui dentro levaria o
#' ambiente DESTA função para o RDS do modelo — a tabela inteira de novo, e mais
#' o que houvesse por perto. `log()`, `poly()` e `factor()` continuam sendo
#' encontrados, porque vêm do `base` e do `stats`.
#' @noRd
.tr_models_ler_formula <- function(texto, dados, param = "formula") {
  texto <- .tr_models_obrigatorio(texto, param)
  f <- tryCatch(stats::as.formula(texto), error = function(e) {
    .tr_models_abort("tr_models_error_bad_formula",
                     "Param '%s': a fórmula '%s' não parseia. Use a forma 'resposta ~ termo1 + termo2'.",
                     param, texto, parent = e)
  })
  if (!inherits(f, "formula") || length(f) != 3L) {
    .tr_models_abort("tr_models_error_bad_formula",
                     "Param '%s': a fórmula '%s' não tem resposta. Use 'resposta ~ termos'.", param, texto)
  }
  environment(f) <- globalenv()
  faltam <- setdiff(all.vars(f), names(dados))
  if (length(faltam)) {
    .tr_models_abort("tr_models_error_bad_formula",
                     "Param '%s': a fórmula cita coluna inexistente: %s. Disponíveis: %s.",
                     param, paste(faltam, collapse = ", "), paste(names(dados), collapse = ", "))
  }
  f
}

#' Monta `resposta ~ a + b` das colunas.
#' @noRd
.tr_models_montar_formula <- function(resposta, termos) {
  rhs <- if (length(termos)) paste(termos, collapse = " + ") else "1"
  f <- stats::as.formula(paste(.tr_models_bt(resposta), "~", rhs))
  environment(f) <- globalenv()
  f
}

#' A tabela do modelo: só as linhas completas nas colunas usadas, e fatores.
#'
#' Descarta À VISTA: devolve quantas linhas saíram, e o card as mostra. O `lm`
#' sempre descartou calado; o experimento com parcela perdida é o caso normal, e
#' recusar como a `multi` recusa obrigaria a um `data/drop_na` que faria a mesma
#' coisa — só que também sem contar.
#'
#' `fatores` vira fator com os níveis que sobraram. Dose 50/100/150 numa ANOVA
#' é tratamento, e deixada numérica viraria uma reta com 1 grau de liberdade —
#' um quadro plausível e errado, que é o pior tipo de erro.
#' @noRd
.tr_models_preparar <- function(dados, vars, no, fatores = character(), min_linhas = 3L) {
  d <- as.data.frame(dados, stringsAsFactors = FALSE)
  vars <- intersect(unique(vars), names(d))
  completas <- stats::complete.cases(d[, vars, drop = FALSE])
  d <- d[completas, , drop = FALSE]
  rownames(d) <- NULL
  for (f in fatores) {
    d[[f]] <- droplevels(factor(d[[f]]))
    if (nlevels(d[[f]]) < 2L) {
      .tr_models_abort("tr_models_error_one_level",
                       "'%s': a coluna '%s' tem um nível só (%s)%s, e um fator precisa de pelo menos dois.",
                       no, f, paste(levels(d[[f]]), collapse = ""),
                       if (any(!completas)) " depois de tirar as linhas com faltante" else "")
    }
  }
  if (nrow(d) < min_linhas) {
    .tr_models_abort("tr_models_error_too_few_rows",
                     "'%s' precisa de pelo menos %d observações completas, e há %d.",
                     no, as.integer(min_linhas), nrow(d))
  }
  list(dados = tibble::as_tibble(d), descartadas = sum(!completas))
}

#' Colunas de texto e lógicas entre as variáveis viram fator.
#'
#' Nos modelos de fórmula livre só isso: número fica número, porque ali quem
#' decide é a pessoa (`factor(dose)` na fórmula).
#' @noRd
.tr_models_categoricas <- function(dados, vars) {
  vars[vapply(vars, function(v) is.character(dados[[v]]) || is.factor(dados[[v]]) ||
                is.logical(dados[[v]]), TRUE)]
}

#' As estrelas na convenção do `summary.lm`.
#' @noRd
.tr_models_estrelas <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**",
         ifelse(p < 0.05, "*", ifelse(p < 0.1, ".", "ns")))))
}

#' Número com vírgula, que é como a nota e o rodapé falam.
#' @noRd
.tr_models_fmt <- function(x, digitos = 3L) {
  ifelse(is.na(x), "—", formatC(signif(x, digitos), format = "fg", digits = digitos,
                               decimal.mark = ",", flag = "#") |> trimws() |> sub(pattern = ",$", replacement = ""))
}

#' p-valor para texto: notação científica abaixo de 0,001, como no card.
#' @noRd
.tr_models_fmt_p <- function(p) {
  ifelse(p < 0.001, sub(".", ",", formatC(p, format = "e", digits = 1), fixed = TRUE),
         formatC(p, format = "f", digits = 3, decimal.mark = ","))
}

#' Grau de liberdade para ler: inteiro sem casas, fracionário (Satterthwaite,
#' Welch) com uma.
#' @noRd
.tr_models_gl <- function(x) {
  if (abs(x - round(x)) < 1e-6) as.character(round(x))
  else formatC(x, format = "f", digits = 1, decimal.mark = ",")
}

#' "2 linhas com faltante fora", ou vazio.
#' @noRd
.tr_models_nota_descarte <- function(n) {
  if (n > 0L) sprintf("%d linha%s com faltante fora", n, if (n > 1L) "s" else "") else ""
}

#' Junta pedaços de nota não vazios com "; ".
#' @noRd
.tr_models_nota <- function(...) {
  x <- unlist(list(...))
  x <- x[!is.na(x) & nzchar(x)]
  paste(x, collapse = "; ")
}

#' Roda `code` guardando as mensagens e avisos que o card precisa dizer.
#'
#' O `emmeans` avisa "Results may be misleading due to involvement in
#' interactions" por `message()`, e o `lmer` avisa ajuste singular: no console
#' isso aparece; no card sumiria. Aqui vira texto que vai para a nota.
#' @noRd
.tr_models_capturar <- function(code) {
  avisos <- character()
  valor <- withCallingHandlers(force(code),
    message = function(m) { avisos <<- c(avisos, trimws(conditionMessage(m))); invokeRestart("muffleMessage") },
    warning = function(w) { avisos <<- c(avisos, trimws(conditionMessage(w))); invokeRestart("muffleWarning") })
  list(valor = valor, avisos = unique(avisos))
}

#' As páginas de ajuda, com as seções na ordem de `?funcao`.
#'
#' Montadas por função, e não à mão, pelo mesmo motivo das irmãs: com trinta
#' nós, a seção esquecida em um deles é certa. `teste` acrescenta a explicação
#' da régua do p-valor, escrita UMA vez.
#' @noRd
.tr_models_ajuda <- function(descricao, parametros, valor, exemplos, veja, grafico = FALSE,
                             teste = FALSE) {
  paste0("## Descrição\n\n", trimws(descricao),
         "\n\n## Parâmetros\n\n", trimws(parametros),
         "\n\n## Valor\n\n", trimws(valor),
         "\n\n## Exemplos\n\n```r\n", trimws(exemplos), "\n```",
         "\n\n## Veja também\n\n", trimws(veja),
         if (grafico) paste0("\n", trama.view::tr_view_help_appearance()) else "",
         if (teste) paste0("\n", .tr_models_ajuda_regua()) else "")
}

#' A seção que explica o card de teste — a do núcleo, igual em toda coleção.
#' Os quadros de efeitos usam a mesma régua e as mesmas estrelas.
#' @noRd
.tr_models_ajuda_regua <- function() trama::tr_help_test_card()


#' Os cosméticos da `view`, com a proporção padrão própria do gráfico.
#' @noRd
.tr_models_props <- function(..., .aspecto = "16:9", .legenda = "direita") {
  ps <- trama.view::tr_view_props(...)
  ps$aspecto$default <- .aspecto
  ps$legenda$default <- .legenda
  ps
}
