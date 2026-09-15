# Os quatro tipos da coleção, e os adaptadores que os ligam à `data`.
#
# - `sampling/plan`: o tamanho calculado, com a escada e a alocação. Tipo
#   próprio porque a SELEÇÃO o lê: um número solto não diria se era n de
#   unidades ou de conglomerados.
# - `sampling/sample`: a amostra COM o desenho. É a ideia central da coleção —
#   ver `R/desenho.R`.
# - `sampling/estimate`: estimativas com erro do desenho; o card desenha a régua
#   do CV.
# - `sampling/simulation`: as réplicas de um desenho contra a verdade.
#
# O preço de um tipo próprio seria perder a `data` e a `view`, e são os
# ADAPTADORES que o pagam: o motor os insere na aresta, sem caixa na tela.
#
# Todo `store` é FUNIL, na doutrina das irmãs.

#' Uma tabela no formato que o renderer `trama/table` do núcleo lê.
#' @noRd
.tr_sampling_linhas_json <- function(df, max = 60L) {
  df <- utils::head(as.data.frame(df), max)
  list(columns = as.list(names(df)),
       rows = lapply(seq_len(nrow(df)), function(i) {
         lapply(df[i, , drop = FALSE], function(v) {
           v <- v[[1]]
           if (is.factor(v)) as.character(v) else if (length(v) == 1L && is.na(v)) NULL
           else if (is.double(v)) signif(v, 6) else v
         })
       }))
}

#' NA vira NULL no JSON (o front lê `null` como "sem valor").
#' @noRd
.tr_sampling_nulo <- function(x) if (length(x) != 1L || is.na(x)) NULL else x

.tr_sampling_rds_type <- function(id, label, color, classe, conferir, preview, summary = NULL) {
  trama::tr_type(
    id, version = 1L, label = label, color = color, ext = "rds",
    store = function(x, path) {
      conferir(x)
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = summary,
    preview = preview
  )
}

# ---- sampling/plan -------------------------------------------------------------

.tr_sampling_plano_conferir <- function(x) {
  .tr_sampling_guard(x, "tr_sampling_plan", .TR_SAMPLING_CAMPOS_PLANO, "tr_sampling_error_not_a_plan", "um plano")
}

.tr_sampling_plano_preview <- function(x) {
  al <- x$alocacao
  list(
    rotulo = x$rotulo, tipo = x$tipo, n = x$n, confianca = x$confianca,
    erro = .tr_sampling_nulo(x$erro), erro_alcancado = .tr_sampling_nulo(x$erro_alcancado),
    percentual = identical(x$tipo, "proporção"),
    conglomerados = .tr_sampling_nulo(x$conglomerados),
    tamanho_conglomerado = .tr_sampling_nulo(x$tamanho_conglomerado),
    passos = lapply(seq_len(nrow(x$passos)), function(i) {
      list(passo = x$passos$passo[[i]], valor = x$passos$valor[[i]], unidade = x$passos$unidade[[i]])
    }),
    alocacao = if (is.null(al)) NULL else lapply(seq_len(nrow(al)), function(i) {
      list(estrato = al$estrato[[i]], N = .tr_sampling_nulo(al$N[[i]]), n = al$n_final[[i]],
           fracao = .tr_sampling_nulo(al$fracao_amostral[[i]]))
    }),
    nota = x$nota)
}

sampling_plan_type <- function() {
  .tr_sampling_rds_type("sampling/plan", "Plano amostral", "#6366f1", "tr_sampling_plan",
                        .tr_sampling_plano_conferir,
                        function(x, ctx) trama::tr_preview("sampling/plan", data = .tr_sampling_plano_preview(x)))
}

#' Plano -> tabela: a alocação por estrato, ou uma linha.
#' @noRd
.tr_sampling_plano_tabela <- function(x) {
  if (!is.null(x$alocacao)) return(x$alocacao)
  tibble::tibble(tipo = x$tipo, n = x$n, erro = x$erro, confianca = x$confianca,
                 conglomerados = x$conglomerados, tamanho_conglomerado = x$tamanho_conglomerado)
}

# ---- sampling/sample -----------------------------------------------------------

.tr_sampling_amostra_preview <- function(x) {
  des <- x$desenho
  niveis <- unique(des$estrato)
  estratos <- lapply(niveis, function(h) {
    upas <- length(unique(des$psu[des$estrato == h]))
    Nh <- if (h %in% names(des$fpc)) des$fpc[[h]] else NA_real_
    list(estrato = if (h == ".") "população" else h, n = upas, N = .tr_sampling_nulo(Nh))
  })
  w <- x$dados$peso_amostral
  c(list(rotulo = x$rotulo, n = x$n, N = .tr_sampling_nulo(x$N),
         upas = if (is.null(x$conglomerado_col)) NULL else length(unique(des$psu)),
         unidade_primaria = if (is.null(x$conglomerado_col)) "unidades" else x$conglomerado_col,
         peso_min = min(w), peso_max = max(w), soma_pesos = sum(w),
         estratos = utils::head(estratos, 12L), mais_estratos = max(0L, length(estratos) - 12L),
         nota = x$nota),
    .tr_sampling_linhas_json(x$dados))
}

sampling_sample_type <- function() {
  .tr_sampling_rds_type("sampling/sample", "Amostra", .TR_SAMPLING_COR, "tr_sampling_sample",
                        .tr_sampling_amostra_conferir,
                        function(x, ctx) trama::tr_preview("sampling/sample", data = .tr_sampling_amostra_preview(x)))
}

# ---- sampling/estimate ---------------------------------------------------------

.tr_sampling_estimativa_conferir <- function(x) {
  .tr_sampling_guard(x, "tr_sampling_estimate", .TR_SAMPLING_CAMPOS_ESTIMATIVA,
                     "tr_sampling_error_not_an_estimate", "uma estimativa")
  if (!all(c("estimativa", "erro_padrao", "li", "ls", "cv_pct") %in% names(x$tabela))) {
    .tr_sampling_abort("tr_sampling_error_not_an_estimate",
                       "A estimativa precisa das colunas 'estimativa', 'erro_padrao', 'li', 'ls' e 'cv_pct'.")
  }
  invisible(x)
}

.tr_sampling_estimativa_preview <- function(x) {
  t <- x$tabela
  rot <- .tr_sampling_rotulos_linhas(x)
  c(list(titulo = sprintf("%s · %s", x$quantidade, x$variavel), desenho = x$desenho,
         confianca = x$confianca, percentual = isTRUE(x$percentual), nota = x$nota,
         linhas = lapply(seq_len(nrow(t)), function(i) {
           list(rotulo = rot[[i]], estimativa = t$estimativa[[i]], li = t$li[[i]], ls = t$ls[[i]],
                margem = t$margem[[i]], erro_padrao = t$erro_padrao[[i]],
                cv = .tr_sampling_nulo(t$cv_pct[[i]]), deff = .tr_sampling_nulo(t$deff[[i]]), n = t$n[[i]])
         })),
    .tr_sampling_linhas_json(t))
}

sampling_estimate_type <- function() {
  .tr_sampling_rds_type("sampling/estimate", "Estimativa", "#0e7490", "tr_sampling_estimate",
                        .tr_sampling_estimativa_conferir,
                        function(x, ctx) trama::tr_preview("sampling/estimate", data = .tr_sampling_estimativa_preview(x)))
}

#' Estimativa -> tabela: com a quantidade, a variável e o desenho na frente,
#' para `data/bind_rows` montar o relatório de várias.
#' @noRd
.tr_sampling_estimativa_tabela <- function(x) {
  cbind(tibble::tibble(quantidade = rep(x$quantidade, nrow(x$tabela)),
                       variavel = rep(x$variavel, nrow(x$tabela)),
                       desenho = rep(x$desenho, nrow(x$tabela))), x$tabela) |> tibble::as_tibble()
}

# ---- sampling/simulation -------------------------------------------------------

.tr_sampling_simulacao_conferir <- function(x) {
  .tr_sampling_guard(x, "tr_sampling_simulation", .TR_SAMPLING_CAMPOS_SIMULACAO,
                     "tr_sampling_error_not_a_simulation", "uma simulação")
}

sampling_simulation_type <- function() {
  .tr_sampling_rds_type("sampling/simulation", "Simulação", "#a855f7", "tr_sampling_simulation",
                        .tr_sampling_simulacao_conferir,
                        # O card é o HISTOGRAMA com a verdade: a pergunta "o
                        # desenho acerta?" se lê nele sem abrir nada.
                        function(x, ctx) trama.view::tr_view_render(.tr_sampling_plot_uma(x), ctx),
                        summary = function(x) as.list(x$resumo))
}

.tr_sampling_adapters <- function() {
  list(
    trama::tr_adapter("sampling/plan", "data/table", .tr_sampling_plano_tabela),
    trama::tr_adapter("sampling/sample", "data/table", function(x) x$dados),
    trama::tr_adapter("sampling/estimate", "data/table", .tr_sampling_estimativa_tabela),
    trama::tr_adapter("sampling/simulation", "data/table", function(x) x$resumo)
  )
}
