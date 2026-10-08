# Grupo multivariado "impulso": nós e docs (ver mv_registro.R).
#
# Os dois gráficos saem do ajuste `series/var` (VAR em nível ou VECM, na forma
# `vec2var`): a função de impulso-resposta (`series/irf`) e a decomposição da
# variância do erro de previsão (`series/fevd`). Toda a conta é do `vars`; aqui
# só se escolhem as séries, se põe a semente e se desenha.

.tr_series_nos_mv_impulso <- function() {
  VAR <- "series/var"; G <- "view/plot"
  I <- trama::tr_param_int; B <- trama::tr_param_bool; P <- trama::tr_param
  icone <- function(n) trama::tr_icon(n)
  list(
    trama::tr_node("series/irf", fn = tr_series_irf, label = "Impulso-resposta",
      pressupostos = .tr_series_doc("series/irf")$pressupostos,
      referencias = .tr_series_doc("series/irf")$referencias,
      category = "serie_ver", icon = icone("waves-horizontal"),
      description = "Como cada série responde, ao longo do tempo, a um choque numa das outras.",
      inputs = list(modelo = VAR), outputs = list(out = G),
      params = .tr_series_props(
        impulso = P("text", "", label = "Impulso", example = "prod"),
        respostas = P("text", "", label = "Respostas", example = "e, U"),
        horizonte = I(10L, min = 1L, max = 100L, label = "Horizonte"),
        ortogonal = B(TRUE, label = "Ortogonal (Cholesky)"),
        acumulada = B(FALSE, label = "Acumulada"),
        reamostras = I(100L, min = 0L, max = 5000L, label = "Reamostras", example = "0 = sem banda"),
        confianca = trama::tr_param_num(0.95, min = 0.5, max = 0.999, step = 0.01, label = "Confiança")),
      help = .tr_series_ajuda(r"---[
A resposta de cada série a um choque de uma unidade numa das séries, em cada
período depois do choque. É o que o VAR diz sobre a dinâmica conjunta: um
choque no preço se propaga para o consumo, e quanto tempo leva para sumir?

Cada faixa é um par impulso → resposta. A linha zero marca o efeito nulo; a
faixa sombreada é a banda de confiança, por bootstrap dos resíduos.

**Ortogonal (Cholesky)** é o padrão, e o resultado DEPENDE DA ORDEM das séries
no modelo: o choque de uma série atinge a de cima no mesmo período, e a de
baixo só depois. Trocar a ordem muda as respostas. Desligado, o choque é o
próprio erro de cada equação, sem a ortogonalização: como os erros são
correlacionados, a resposta não se atribui a uma só série.

O VECM entra pela mesma forma (`vec2var`), e as respostas são as do nível.
]---", r"---[
- **Impulso** — a série que leva o choque. Vazio: todas.
- **Respostas** — as séries que reagem. Vazio: todas. Separe por vírgula.
- **Horizonte** — quantos períodos depois do choque (1 a 100).
- **Ortogonal (Cholesky)** — separa os choques pela ordem das séries (padrão).
- **Acumulada** — soma as respostas período a período: o efeito total até o
  horizonte, e não o efeito em cada período.
- **Reamostras** — réplicas do bootstrap das bandas. 0 desenha só os pontos.
  O padrão, 100, abre rápido no card; para uma banda estável, use 1000 ou mais.
- **Confiança** — o nível da banda, 0.95 por padrão.

A semente é a do nó: a mesma entrada dá as mesmas bandas.
]---", r"---[
Um gráfico (`view/plot`). No console, um ggplot comum, somável.
]---", r"---[
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac")) |>
  tr_add("v", "series/var", defasagens = 2L, from = "j") |>
  tr_add("irf", "series/irf", impulso = "cac", horizonte = 12L, reamostras = 100L, from = "v")
]---", r"---[
`series/fevd` para a parte da variância explicada por cada choque; `series/var`
para o ajuste; `series/join` para montar as séries.
]---", grafico = TRUE)),

    trama::tr_node("series/fevd", fn = tr_series_fevd, label = "Decomposição da variância",
      pressupostos = .tr_series_doc("series/fevd")$pressupostos,
      referencias = .tr_series_doc("series/fevd")$referencias,
      category = "serie_ver", icon = icone("chart-column-stacked"),
      description = "Quanto da variância do erro de previsão de cada série vem de cada choque, por horizonte.",
      inputs = list(modelo = VAR), outputs = list(out = G),
      params = .tr_series_props(
        horizonte = I(10L, min = 1L, max = 100L, label = "Horizonte"),
        .aspecto = "4:3"),
      help = .tr_series_ajuda(r"---[
De quanto é o erro de previsão de cada série em cada horizonte, e de qual
choque ele vem. No horizonte 1, a variância de uma série vem só dos choques
dela e dos das anteriores na ordem; com o tempo, a fatia das outras cresce.

Cada barra soma 1 (100%): é a fração da variância explicada por cada choque.
Uma faixa por série.

A separação dos choques é a de Cholesky, e a fração que cabe a cada série
DEPENDE DA ORDEM delas no modelo. Uma série posta por último recebe o que sobra
depois das anteriores.
]---", r"---[
- **Horizonte** — até onde prever o erro (1 a 100 períodos).
]---", r"---[
Um gráfico (`view/plot`). No console, um ggplot comum, somável.
]---", r"---[
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac")) |>
  tr_add("v", "series/var", defasagens = 2L, from = "j") |>
  tr_add("fevd", "series/fevd", horizonte = 12L, from = "v")
]---", r"---[
`series/irf` para as respostas que geram essa variância; `series/var`.
]---", grafico = TRUE))
  )
}

.tr_series_docs_mv_impulso <- function() {
  R <- trama::tr_ref
  P <- .tr_series_P
  lutkepohl <- R(autores = "Lütkepohl, H.", ano = 2005, titulo = "New Introduction to Multiple Time Series Analysis",
                 fonte = "Berlin: Springer", doi = "10.1007/978-3-540-27752-1", papel = "livro-texto")
  pfaff <- R(autores = "Pfaff, B.", ano = 2008, titulo = "VAR, SVAR and SVEC Models: Implementation Within R Package vars",
             fonte = "Journal of Statistical Software, 27(4)", doi = "10.18637/jss.v027.i04", papel = "teoria")
  list(
    "series/irf" = list(
      pressupostos = list(
        P("O VAR é estável: as respostas se apagam com o tempo. Com raiz de módulo acima de 1, a resposta não volta a zero.",
          se_falhar = "veja a maior raiz no card do series/var; se passar de 1, diferencie as séries ou use o VECM"),
        P("Com ortogonal ligado, a ordem das séries é uma escolha de quem modela: o resultado muda com ela.",
          se_falhar = "reordene as séries no series/join e compare; se a conclusão mudar, diga isso no relatório")),
      referencias = list(lutkepohl, pfaff, .tr_series_impl("vars", "irf",
        nota = "Bandas por bootstrap dos resíduos (reamostragem com reposição), com o quantil de cada ponto; semente do nó."))),
    "series/fevd" = list(
      pressupostos = list(
        P("A decomposição é de Cholesky, e a fração de cada série depende da ordem delas no modelo.",
          se_falhar = "reordene as séries e compare as frações antes de atribuir a variância a uma delas")),
      referencias = list(lutkepohl, pfaff, .tr_series_impl("vars", "fevd")))
  )
}

#' Escolhe séries pelo nome, na ordem do modelo; vazio é todas.
#' @noRd
.tr_series_escolhidas <- function(valor, aceitos, param) {
  v <- trimws(unlist(strsplit(paste(as.character(valor), collapse = ","), ",", fixed = TRUE)))
  v <- v[nzchar(v)]
  if (!length(v)) return(aceitos)
  fora <- setdiff(v, aceitos)
  if (length(fora)) .tr_series_option(param, fora, aceitos)
  aceitos[aceitos %in% v]
}

#' Um lógico de um param de liga/desliga, recusado se não for um.
#' @noRd
.tr_series_lgl <- function(valor, param) {
  if (!is.logical(valor) || length(valor) != 1L || is.na(valor)) {
    .tr_series_abort("tr_series_error_bad_option",
                     "Param '%s': tem que ser verdadeiro ou falso (veio '%s').",
                     param, paste(as.character(valor), collapse = ", "))
  }
  valor
}

#' Nível de confiança entre 0 e 1, como 0.95.
#' @noRd
.tr_series_confianca <- function(valor) {
  ok <- is.numeric(valor) && length(valor) == 1L && !is.na(valor) && valor > 0 && valor < 1
  if (!ok) {
    .tr_series_abort("tr_series_error_bad_option",
                     "Param 'confianca': um número entre 0 e 1, como 0.95 (veio '%s').",
                     paste(as.character(valor), collapse = ", "))
  }
  as.numeric(valor)
}

#' O `varest` com o call só de valores, para o bootstrap do `vars` refazer o VAR.
#'
#' O `vars` refaz cada réplica com `update()`, que reavalia o call guardado.
#' O `tr_series_var` guarda o call com símbolos (`type = det`, `season = saz`),
#' e na reavaliação eles não existem no frame do `vars` (`det` acha `stats::det`
#' e o `match.arg` falha). Aqui o call é refeito com valores: y, p, type e, se
#' houver dummies sazonais (colunas `sd*`), a frequência. A conta não muda; só
#' o call. (O defeito de origem está em `tr_series_var`, em multivariada.R.)
#' @noRd
.tr_series_ajuste_congelado <- function(modelo) {
  aj <- modelo$ajuste
  if (!inherits(aj, "varest")) return(aj)
  args <- list(y = aj$y, p = aj$p, type = aj$type)
  if (any(grepl("^sd[0-9]+$", colnames(aj$datamat)))) {
    args$season <- as.integer(stats::frequency(modelo$serie))
  }
  aj$call <- as.call(c(list(quote(vars::VAR)), args))
  aj
}

#' Os pontos da IRF escolhidos: uma linha por (impulso, resposta, horizonte).
#'
#' Tudo passa por `vars::irf` com a semente do nó, e as séries saem do objeto
#' completo (todos os impulsos, todas as respostas): o bootstrap é o mesmo
#' qualquer que seja a escolha, e o recorte não muda um ponto sequer. Com
#' `reamostras = 0`, sem banda, e o `vars` nem sorteia.
#' @noRd
.tr_series_irf_dados <- function(modelo, impulso, respostas, horizonte, ortogonal, acumulada,
                                 reamostras, confianca, .seed) {
  .tr_series_guard_var(modelo, "series/irf")
  h <- .tr_series_int(horizonte, "horizonte", min = 1, max = 100)
  reps <- .tr_series_int(reamostras, "reamostras", min = 0, max = 5000)
  orto <- .tr_series_lgl(ortogonal, "ortogonal")
  acum <- .tr_series_lgl(acumulada, "acumulada")
  conf <- .tr_series_confianca(confianca)
  semente <- if (is.null(.seed) || !length(.seed) || is.na(.seed[[1]])) 1L else as.integer(.seed[[1]])
  nomes <- colnames(modelo$serie)
  imps <- .tr_series_escolhidas(impulso, nomes, "impulso")
  resps <- .tr_series_escolhidas(respostas, nomes, "respostas")
  ir <- .tr_series_com_semente(semente, vars::irf(.tr_series_ajuste_congelado(modelo), n.ahead = h, ortho = orto,
                                                  cumulative = acum, boot = reps > 0L, ci = conf,
                                                  runs = max(reps, 1L), seed = semente))
  d <- .tr_series_irf_tabela(ir, imps, resps)
  d$par <- paste(d$impulso, "→", d$resposta)
  d$par <- factor(d$par, levels = unique(d$par))
  d
}

#' Impulso-resposta de um VAR ou VECM: uma faixa por par impulso -> resposta.
#' @export
tr_series_irf <- function(modelo, impulso = "", respostas = "", horizonte = 10L, ortogonal = TRUE,
                          acumulada = FALSE, reamostras = 100L, confianca = 0.95, .seed = NULL,
                          aspecto = "16:9", tema = "padrão", titulo = "", rotulo_x = "", rotulo_y = "",
                          legenda = "direita") {
  d <- .tr_series_irf_dados(modelo, impulso, respostas, horizonte, ortogonal, acumulada,
                            reamostras, confianca, .seed)
  reps <- .tr_series_int(reamostras, "reamostras", min = 0, max = 5000)
  conf <- .tr_series_confianca(confianca)
  banda <- reps > 0L
  subtitulo <- paste0(if (isTRUE(ortogonal)) "ortogonal, ordem de Cholesky" else "não ortogonal",
                      if (isTRUE(acumulada)) ", acumulada" else "",
                      if (banda) sprintf(", banda de %g%% (%d reamostras)", 100 * conf, reps) else ", sem banda")
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["horizonte"]], y = .data[["valor"]]))
  p <- p + ggplot2::geom_hline(yintercept = 0, colour = .TR_SERIES_CINZA, linewidth = .4)
  if (banda) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["li"]], ymax = .data[["ls"]]),
                                  fill = .TR_SERIES_COR_2, alpha = .25, na.rm = TRUE)
  }
  p <- p + ggplot2::geom_line(colour = .TR_SERIES_COR, linewidth = .6, na.rm = TRUE) +
    ggplot2::facet_wrap(ggplot2::vars(.data[["par"]]), scales = "free_y") +
    ggplot2::labs(x = "horizonte", y = "resposta", subtitle = subtitulo)
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Pontos da IRF escolhidos, em tabela: uma linha por (impulso, resposta, horizonte).
#' @noRd
.tr_series_irf_tabela <- function(ir, imps, resps) {
  h <- nrow(ir$irf[[1]]) - 1L
  do.call(rbind, lapply(imps, function(i) {
    do.call(rbind, lapply(resps, function(j) {
      tibble::tibble(impulso = i, resposta = j, horizonte = 0:h,
                     valor = unname(ir$irf[[i]][, j]),
                     li = if (is.null(ir$Lower)) NA_real_ else unname(ir$Lower[[i]][, j]),
                     ls = if (is.null(ir$Upper)) NA_real_ else unname(ir$Upper[[i]][, j]))
    }))
  }))
}

#' A tabela da FEVD: uma linha por (série, horizonte, choque), com a fração.
#' @noRd
.tr_series_fevd_dados <- function(modelo, horizonte) {
  .tr_series_guard_var(modelo, "series/fevd")
  h <- .tr_series_int(horizonte, "horizonte", min = 1, max = 100)
  fv <- vars::fevd(modelo$ajuste, n.ahead = h)
  d <- do.call(rbind, lapply(names(fv), function(s) {
    m <- fv[[s]]
    tibble::tibble(serie = s, horizonte = rep(seq_len(h), times = ncol(m)),
                   choque = rep(colnames(m), each = h), fracao = as.numeric(m))
  }))
  d$serie <- factor(d$serie, levels = names(fv))
  d$choque <- factor(d$choque, levels = colnames(modelo$serie))
  d$horizonte <- factor(d$horizonte, levels = seq_len(h))
  d
}

#' Decomposição da variância do erro de previsão: barras empilhadas, uma faixa por série.
#'
#' `vars::fevd` com Cholesky (não há opção de desligá-la lá): a ordem das
#' séries decide a fração de cada uma. As linhas somam 1 por construção.
#' @export
tr_series_fevd <- function(modelo, horizonte = 10L, aspecto = "4:3", tema = "padrão",
                           titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  d <- .tr_series_fevd_dados(modelo, horizonte)
  choques <- levels(d$choque)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["horizonte"]], y = .data[["fracao"]],
                                       fill = .data[["choque"]])) +
    ggplot2::geom_col(position = "stack", width = .8) +
    ggplot2::facet_wrap(ggplot2::vars(.data[["serie"]]), ncol = min(2L, length(choques))) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::labs(x = "horizonte", y = "fração da variância", fill = "choque",
                  subtitle = "decomposição de Cholesky, ordem das séries")
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}
