# Teste de permutação do F sequencial de um termo em um ajuste linear.

.tr_models_f_termo <- function(ajuste, termo = "") {
  quadro <- stats::anova(ajuste)
  termos <- setdiff(trimws(rownames(quadro)), "Residuals")
  if (!length(termos)) .tr_models_abort("tr_models_error_bad_option", "'models/permutation': o ajuste não tem termos testáveis.")
  if (is.null(termo) || !nzchar(termo)) termo <- termos[[1L]]
  i <- match(termo, termos)
  if (is.na(i)) .tr_models_abort("tr_models_error_bad_option", "'models/permutation': termo '%s' não está no quadro da ANOVA. Escolha: %s.", termo, paste(termos, collapse = ", "))
  list(nome = termo, F = unname(quadro$`F value`[match(termo, trimws(rownames(quadro)))]), p = unname(quadro$`Pr(>F)`[match(termo, trimws(rownames(quadro)))]))
}

#' Testa um termo por permutação da resposta, mantendo fixa a matriz do modelo.
#' @export
tr_models_permutation <- function(modelo, termo = "", grupo = "", reamostras = 9999L,
                                 aspecto = "16:9", tema = "padrão", titulo = "",
                                 rotulo_x = "", rotulo_y = "", legenda = "direita", .seed = 1L) {
  .tr_models_fit_conferir(modelo)
  if (!identical(modelo$classe, "lm")) {
    .tr_models_abort("tr_models_error_bad_option", "'models/permutation' aceita apenas lm; o ajuste recebido é '%s'.", modelo$classe)
  }
  B <- as.integer(reamostras)
  if (length(B) != 1L || is.na(B) || B < 1L) .tr_models_abort("tr_models_error_bad_option", "'Reamostras' deve ser um inteiro positivo.")
  obs <- .tr_models_f_termo(modelo$ajuste, termo)
  dados <- as.data.frame(modelo$dados)
  y <- stats::model.response(stats::model.frame(modelo$ajuste))
  # O frame do lm pode excluir linhas; a tabela guardada já contém somente as
  # linhas completas, mas respeitamos a ordem e o subconjunto efetivos do frame.
  if (length(y) != nrow(dados)) dados <- dados[as.integer(rownames(stats::model.frame(modelo$ajuste))), , drop = FALSE]
  resposta <- all.vars(stats::formula(modelo$ajuste)[[2L]])[[1L]]
  grupos <- if (is.null(grupo) || !nzchar(grupo)) rep.int(1L, length(y)) else {
    if (!grupo %in% names(dados)) .tr_models_abort("tr_models_error_bad_option", "'Dentro de' precisa ser uma coluna dos dados do ajuste: '%s'.", grupo)
    if (anyNA(dados[[grupo]])) .tr_models_abort("tr_models_error_bad_option", "'models/permutation': 'Dentro de' ('%s') tem NA; essas linhas não teriam com quem trocar. Filtre ou recodifique antes.", grupo)
    as.integer(factor(dados[[grupo]]))
  }
  estratos <- split(seq_along(y), grupos)
  f <- stats::formula(modelo$ajuste)
  # A matriz do modelo não muda sob permutação da resposta: a QR é a mesma, e
  # o F sequencial (tipo I) sai dos efeitos `Q'y` como no `anova.lm` — a SQ de
  # cada termo é a soma dos efeitos das colunas dele; a do resíduo, a dos que
  # sobram. Mil vezes mais barato que reajustar e chamar `anova()`.
  aj <- modelo$ajuste
  qr <- aj$qr
  r <- aj$rank
  asgn <- attr(stats::model.matrix(aj), "assign")[qr$pivot[seq_len(r)]]
  nomes <- attr(stats::terms(aj), "term.labels")
  k <- match(obs$nome, nomes)
  gl_termo <- sum(asgn == k); gl_res <- length(y) - r
  f_de <- function(yy) {
    ef <- base::qr.qty(qr, yy)
    (sum(ef[seq_len(r)][asgn == k]^2) / gl_termo) / (sum(ef[-seq_len(r)]^2) / gl_res)
  }
  if (!isTRUE(all.equal(f_de(y), obs$F, tolerance = 1e-10))) {
    .tr_models_abort("tr_models_error_bad_option", "'models/permutation': não consegui reproduzir o F de '%s' pela decomposição do ajuste.", obs$nome)
  }
  simulados <- .tr_models_com_semente(.seed, vapply(seq_len(B), function(b) {
    yp <- y
    for (ii in estratos) yp[ii] <- y[ii][sample.int(length(ii))]
    f_de(yp)
  }, numeric(1)))
  tol <- 1e-8
  excede <- sum(simulados >= obs$F - tol)
  tabela <- tibble::tibble(termo = obs$nome, F_observado = obs$F, p_permutacao = (excede + 1) / (B + 1),
                           p_teorico = obs$p, reamostras = B, excedencias = excede)
  distribuicao <- tibble::tibble(quantidade = paste0("F ", obs$nome), reamostra = seq_len(B), valor = simulados)
  grafico <- ggplot2::ggplot(distribuicao, ggplot2::aes(x = .data[["valor"]])) +
    ggplot2::geom_histogram(bins = 30, fill = "#5B7C99", color = "white") +
    ggplot2::geom_vline(xintercept = obs$F, color = "#B34D4D", linewidth = 1) +
    ggplot2::labs(x = "F sob permutação", y = "Contagem", title = paste("Permutação ·", obs$nome))
  grafico <- trama.view::tr_view_finish(grafico, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
  # Só base: o F observado sai do `stats::anova`; a permutação é deste pacote
  # (decomposição QR), sem `coin`.
  structure(list(out = grafico, tabela = tabela, distribuicao = distribuicao),
            trama_ferramentas = "stats::anova")
}

.tr_models_nos_permutation <- function() {
  list(trama::tr_node("models/permutation", fn = tr_models_permutation,
    pressupostos = .tr_models_doc("models/permutation")$pressupostos,
    referencias = .tr_models_doc("models/permutation")$referencias,
    label = "Permutação", stochastic = TRUE, category = "modelo_testes", icon = trama::tr_icon("shuffle"),
    description = "Testa um termo do ajuste linear permutando a resposta e compara o F observado com a distribuição empírica.",
    inputs = list(modelo = "models/fit"), outputs = list(out = "view/plot", tabela = "data/table", distribuicao = "data/table"),
    params = c(list(termo = trama::tr_param("text", "", label = "Termo", example = "tratamento"),
      grupo = trama::tr_param_col("", label = "Dentro de", role = "categorica", suggest = FALSE, example = "bloco"),
      reamostras = trama::tr_param_int(9999L, min = 99L, max = 100000L, label = "Reamostras")),
      .tr_models_props(.aspecto = "16:9")),
    help = .tr_models_ajuda(r"---[
Permuta os valores da resposta entre as linhas do ajuste, mantendo os
preditores, e recalcula o F em cada repetição (pela mesma decomposição QR do
ajuste, sem reajustar: a matriz do modelo não muda). Compara o F sequencial do **Termo** escolhido (tipo I) com os
F simulados; em branco, usa o primeiro termo do quadro da ANOVA. O p-valor
Monte Carlo é `(excedências + 1) / (reamostras + 1)`, contando empates com
tolerância `1e-8`. A tabela também mostra o p teórico do F.

**Dentro de** restringe as permutações às linhas do mesmo nível (por exemplo,
bloco). Vazio permuta entre todas as linhas. Quando o termo vem depois de
outro que tem efeito (DBC: `bloco + tratamento`), as linhas só são trocáveis
sob H0 dentro do bloco: use **Dentro de** = bloco, senão o teste deixa de ser
exato.

Aplica-se a ajuste linear (`lm`), incluindo ANOVAs de efeitos fixos que geram
`lm`. Quando o experimento tem plano de randomização conhecido, prefira o
teste de randomização da coleção `experiments`; este bloco não substitui a
randomização do delineamento.
]---", r"---[
- **Termo** — rótulo de uma linha do quadro da ANOVA; vazio usa o primeiro.
- **Dentro de** — coluna que define grupos independentes de permutação.
- **Reamostras** — quantidade de permutações (padrão 9999).
- **Aspecto, Tema, Título, Rótulos dos eixos, Legenda** — aparência do gráfico.
]---", r"---[
Gráfico da distribuição dos F permutados, tabela com p de permutação e teórico,
e `distribuicao` longa (`quantidade`, `reamostra`, `valor`), um F por
repetição. A semente é a do card: o mesmo card dá a mesma distribuição.
]---", r"---[
tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "PlantGrowth") |>
  tr_add("ajuste", "models/anova_dic", resposta = "weight", tratamento = "group", from = "dados") |>
  tr_add("perm", "models/permutation", termo = "group", reamostras = 9999, from = "ajuste")
]---", r"---[
O teste de randomização da coleção de experimentos, quando há plano; `models/anova_table`
para o quadro teórico.
]---", grafico = TRUE, teste = TRUE)))
}
