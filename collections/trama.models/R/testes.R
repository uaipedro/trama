# Os testes clássicos: tabela entra, UM TESTE sai.
#
# Cada um descarta os faltantes das colunas que usa, e diz quantos na nota. A
# conclusão escrita segue a alternativa: "média de A menor que a de B" só
# aparece quando foi isso que se testou.

.TR_MODELS_ALTERNATIVAS <- c("bilateral", "menor", "maior")

.tr_models_alt_r <- function(alt) switch(alt, bilateral = "two.sided", menor = "less", maior = "greater")

#' As colunas do teste, sem as linhas incompletas.
#' @noRd
.tr_models_teste_dados <- function(dados, cols, no, min_linhas = 3L) {
  d <- as.data.frame(dados)[, cols, drop = FALSE]
  ok <- stats::complete.cases(d)
  d <- d[ok, , drop = FALSE]
  if (nrow(d) < min_linhas) {
    .tr_models_abort("tr_models_error_too_few_rows",
                     "'%s' precisa de pelo menos %d observações completas, e há %d.",
                     no, as.integer(min_linhas), nrow(d))
  }
  list(d = d, nota = .tr_models_nota_descarte(sum(!ok)))
}

#' A coluna do grupo com exatamente dois níveis.
#' @noRd
.tr_models_dois_grupos <- function(d, grupo, no) {
  g <- droplevels(factor(d[[grupo]]))
  if (nlevels(g) != 2L) {
    .tr_models_abort("tr_models_error_two_groups",
                     paste0("'%s' compara DOIS grupos, e a coluna '%s' tem %d (%s). Filtre dois antes num ",
                            "'data/filter', ou use 'models/kruskal' ou 'models/anova_dic' para mais."),
                     no, grupo, nlevels(g), paste(utils::head(levels(g), 6L), collapse = ", "))
  }
  g
}

#' A frase da conclusão, conforme a alternativa.
#' @noRd
.tr_models_conclusao_alt <- function(alt, a, b, oque = "média") {
  switch(alt,
         bilateral = sprintf("%ss diferentes", oque),
         menor = sprintf("%s de %s menor que a de %s", oque, a, b),
         maior = sprintf("%s de %s maior que a de %s", oque, a, b))
}

#' t de Student / Welch para duas amostras independentes.
#' @param dados tabela.
#' @param resposta coluna numérica.
#' @param grupo coluna com os dois grupos.
#' @param variancias_iguais TRUE para o t de Student clássico; FALSE (padrão)
#'   para o de Welch, que não pressupõe variâncias iguais.
#' @param alternativa `"bilateral"`, `"menor"` ou `"maior"` (o primeiro grupo em
#'   relação ao segundo, na ordem dos níveis).
#' @return objeto `tr_models_test`.
#' @export
tr_models_t_test <- function(dados, resposta = "", grupo = "", variancias_iguais = FALSE,
                             alternativa = "bilateral") {
  no <- "models/t_test"
  alt <- .tr_models_enum(alternativa, .TR_MODELS_ALTERNATIVAS, "alternativa")
  resp <- .tr_models_numerica(dados, .tr_models_col(dados, resposta, "resposta"), "resposta")
  grp <- .tr_models_col(dados, grupo, "grupo")
  td <- .tr_models_teste_dados(dados, c(resp, grp), no, min_linhas = 4L)
  g <- .tr_models_dois_grupos(td$d, grp, no)
  y <- td$d[[resp]]
  a <- levels(g)[[1]]; b <- levels(g)[[2]]
  t <- .tr_models_ajustar(stats::t.test(y[g == a], y[g == b], var.equal = isTRUE(variancias_iguais),
                                        alternative = .tr_models_alt_r(alt)), no)
  .tr_models_teste(
    if (isTRUE(variancias_iguais)) "t de Student" else "t de Welch",
    sprintf("as médias de %s e %s são iguais", a, b), t$statistic, "t", t$p.value,
    gl = .tr_models_gl(t$parameter),
    conclusao_sim = .tr_models_conclusao_alt(alt, a, b),
    conclusao_nao = "não há evidência de diferença entre as médias",
    efeito = list(rotulo = sprintf("diferença de médias (%s − %s)", a, b),
                  valor = unname(t$estimate[[1]] - t$estimate[[2]]),
                  li = t$conf.int[[1]], ls = t$conf.int[[2]]),
    nota = .tr_models_nota(sprintf("n = %d e %d", sum(g == a), sum(g == b)), td$nota),
    fonte = if (isTRUE(variancias_iguais)) "Student (1908)" else "Welch (1947)")
}

#' t pareado.
#' @param dados tabela.
#' @param antes,depois as duas colunas numéricas medidas na mesma unidade.
#' @inheritParams tr_models_t_test
#' @return objeto `tr_models_test`.
#' @export
tr_models_paired_t <- function(dados, antes = "", depois = "", alternativa = "bilateral") {
  no <- "models/paired_t"
  alt <- .tr_models_enum(alternativa, .TR_MODELS_ALTERNATIVAS, "alternativa")
  x1 <- .tr_models_numerica(dados, .tr_models_col(dados, antes, "antes"), "antes")
  x2 <- .tr_models_numerica(dados, .tr_models_col(dados, depois, "depois"), "depois")
  td <- .tr_models_teste_dados(dados, c(x1, x2), no)
  d <- td$d[[x1]] - td$d[[x2]]
  if (stats::sd(d) == 0) {
    .tr_models_abort("tr_models_error_fit", "'%s': as diferenças %s − %s são todas iguais; não há variância.",
                     no, x1, x2)
  }
  t <- stats::t.test(d, alternative = .tr_models_alt_r(alt))
  .tr_models_teste(
    "t pareado", sprintf("a média das diferenças %s − %s é zero", x1, x2), t$statistic, "t", t$p.value,
    gl = as.character(t$parameter),
    conclusao_sim = .tr_models_conclusao_alt(alt, x1, x2),
    conclusao_nao = "não há evidência de diferença entre as medidas",
    efeito = list(rotulo = sprintf("média das diferenças (%s − %s)", x1, x2), valor = unname(t$estimate),
                  li = t$conf.int[[1]], ls = t$conf.int[[2]]),
    nota = .tr_models_nota(sprintf("%d pares", length(d)), td$nota),
    fonte = "Student (1908)")
}

#' t para uma amostra.
#' @param dados tabela.
#' @param coluna coluna numérica.
#' @param mu valor de referência.
#' @inheritParams tr_models_t_test
#' @return objeto `tr_models_test`.
#' @export
tr_models_one_sample_t <- function(dados, coluna = "", mu = 0, alternativa = "bilateral") {
  no <- "models/one_sample_t"
  alt <- .tr_models_enum(alternativa, .TR_MODELS_ALTERNATIVAS, "alternativa")
  mu <- .tr_models_num(mu, "mu")
  x <- .tr_models_numerica(dados, .tr_models_col(dados, coluna, "coluna"), "coluna")
  td <- .tr_models_teste_dados(dados, x, no)
  t <- .tr_models_ajustar(stats::t.test(td$d[[x]], mu = mu, alternative = .tr_models_alt_r(alt)), no)
  mu_txt <- .tr_models_fmt(mu, 6L)
  .tr_models_teste(
    "t para uma amostra", sprintf("a média de %s é %s", x, mu_txt), t$statistic, "t", t$p.value,
    gl = as.character(t$parameter),
    conclusao_sim = switch(alt, bilateral = sprintf("média diferente de %s", mu_txt),
                           menor = sprintf("média menor que %s", mu_txt),
                           maior = sprintf("média maior que %s", mu_txt)),
    conclusao_nao = sprintf("não há evidência de que a média difira de %s", mu_txt),
    efeito = list(rotulo = "média", valor = unname(t$estimate), li = t$conf.int[[1]], ls = t$conf.int[[2]]),
    nota = .tr_models_nota(sprintf("n = %d", nrow(td$d)), td$nota),
    fonte = "Student (1908)")
}

#' Wilcoxon-Mann-Whitney para duas amostras independentes.
#' @inheritParams tr_models_t_test
#' @return objeto `tr_models_test`.
#' @export
tr_models_wilcoxon <- function(dados, resposta = "", grupo = "", alternativa = "bilateral") {
  no <- "models/wilcoxon"
  alt <- .tr_models_enum(alternativa, .TR_MODELS_ALTERNATIVAS, "alternativa")
  resp <- .tr_models_numerica(dados, .tr_models_col(dados, resposta, "resposta"), "resposta")
  grp <- .tr_models_col(dados, grupo, "grupo")
  td <- .tr_models_teste_dados(dados, c(resp, grp), no, min_linhas = 4L)
  g <- .tr_models_dois_grupos(td$d, grp, no)
  y <- td$d[[resp]]
  a <- levels(g)[[1]]; b <- levels(g)[[2]]
  # Os avisos de empate viram nota: com empate o p-valor é o da aproximação
  # normal, e quem lê o card precisa saber que não é o exato.
  r <- .tr_models_ajustar(.tr_models_capturar(
    stats::wilcox.test(y[g == a], y[g == b], alternative = .tr_models_alt_r(alt), conf.int = TRUE)), no)
  t <- r$valor
  .tr_models_teste(
    "Wilcoxon-Mann-Whitney", sprintf("as distribuições de %s e %s têm a mesma locação", a, b),
    t$statistic, "W", t$p.value,
    conclusao_sim = .tr_models_conclusao_alt(alt, a, b, oque = "locação"),
    conclusao_nao = "não há evidência de diferença de locação",
    efeito = if (is.null(t$estimate)) NULL else
      list(rotulo = sprintf("diferença de locação (%s − %s)", a, b), valor = unname(t$estimate),
           li = t$conf.int[[1]], ls = t$conf.int[[2]]),
    nota = .tr_models_nota(sprintf("n = %d e %d", sum(g == a), sum(g == b)), td$nota,
                           if (any(grepl("ties|empate", r$avisos))) "empates: p-valor pela aproximação normal" else ""),
    fonte = "Wilcoxon (1945); Mann & Whitney (1947)")
}

#' Kruskal-Wallis: dois ou mais grupos.
#' @param dados tabela.
#' @param resposta coluna numérica.
#' @param grupo coluna dos grupos.
#' @return objeto `tr_models_test`.
#' @export
tr_models_kruskal <- function(dados, resposta = "", grupo = "") {
  no <- "models/kruskal"
  resp <- .tr_models_numerica(dados, .tr_models_col(dados, resposta, "resposta"), "resposta")
  grp <- .tr_models_col(dados, grupo, "grupo")
  td <- .tr_models_teste_dados(dados, c(resp, grp), no)
  g <- droplevels(factor(td$d[[grp]]))
  if (nlevels(g) < 2L) {
    .tr_models_abort("tr_models_error_one_level", "'%s': a coluna '%s' tem um grupo só.", no, grp)
  }
  t <- .tr_models_ajustar(stats::kruskal.test(td$d[[resp]], g), no)
  .tr_models_teste(
    "Kruskal-Wallis", sprintf("as distribuições de %s são iguais entre os níveis de %s", resp, grp),
    t$statistic, "H", t$p.value, gl = as.character(t$parameter),
    conclusao_sim = "algum grupo difere",
    conclusao_nao = "não há evidência de diferença entre os grupos",
    nota = .tr_models_nota(sprintf("%d grupos, n = %d", nlevels(g), length(g)), td$nota),
    fonte = "Kruskal & Wallis (1952)")
}

#' A tabela de contingência de duas colunas.
#' @noRd
.tr_models_contingencia <- function(dados, linha, coluna, no) {
  l <- .tr_models_col(dados, linha, "linha")
  c <- .tr_models_col(dados, coluna, "coluna")
  td <- .tr_models_teste_dados(dados, c(l, c), no, min_linhas = 2L)
  tab <- table(droplevels(factor(td$d[[l]])), droplevels(factor(td$d[[c]])))
  if (any(dim(tab) < 2L)) {
    .tr_models_abort("tr_models_error_one_level",
                     "'%s': a tabela %s × %s é %d × %d, e o teste pede pelo menos duas categorias em cada.",
                     no, l, c, nrow(tab), ncol(tab))
  }
  list(tab = tab, l = l, c = c, nota = td$nota)
}

#' Qui-quadrado de independência.
#' @param dados tabela.
#' @param linha,coluna as duas colunas categóricas.
#' @param correcao correção de continuidade de Yates (só em 2 × 2). Desligada
#'   por padrão: torna o teste conservador (Agresti 2002).
#' @return objeto `tr_models_test`.
#' @export
tr_models_chisq <- function(dados, linha = "", coluna = "", correcao = FALSE) {
  no <- "models/chisq"
  ct <- .tr_models_contingencia(dados, linha, coluna, no)
  r <- .tr_models_capturar(stats::chisq.test(ct$tab, correct = isTRUE(correcao)))
  t <- r$valor
  poucos <- mean(t$expected < 5)
  .tr_models_teste(
    "Qui-quadrado", sprintf("%s e %s são independentes", ct$l, ct$c), t$statistic, "qui2", t$p.value,
    gl = as.character(t$parameter),
    conclusao_sim = sprintf("%s e %s estão associadas", ct$l, ct$c),
    conclusao_nao = "não há evidência de associação",
    nota = .tr_models_nota(sprintf("tabela %d × %d, n = %d", nrow(ct$tab), ncol(ct$tab), sum(ct$tab)), ct$nota,
                           if (poucos > 0.2) sprintf("%.0f%% das caselas com esperado < 5: prefira 'models/fisher_exact'", 100 * poucos) else ""),
    extra = list(menor_esperado = min(t$expected)),
    fonte = "Pearson (1900)")
}

#' Exato de Fisher.
#' @inheritParams tr_models_chisq
#' @return objeto `tr_models_test`.
#' @export
tr_models_fisher_exact <- function(dados, linha = "", coluna = "") {
  no <- "models/fisher_exact"
  ct <- .tr_models_contingencia(dados, linha, coluna, no)
  t <- .tr_models_ajustar(stats::fisher.test(ct$tab, workspace = 2e6), no)
  e22 <- all(dim(ct$tab) == 2L)
  .tr_models_teste(
    "Exato de Fisher", sprintf("%s e %s são independentes", ct$l, ct$c),
    if (e22) t$estimate else NA_real_, if (e22) "razão de chances" else "—", t$p.value,
    conclusao_sim = sprintf("%s e %s estão associadas", ct$l, ct$c),
    conclusao_nao = "não há evidência de associação",
    efeito = if (e22) list(rotulo = "razão de chances", valor = unname(t$estimate),
                           li = t$conf.int[[1]], ls = t$conf.int[[2]]) else NULL,
    nota = .tr_models_nota(sprintf("tabela %d × %d, n = %d", nrow(ct$tab), ncol(ct$tab), sum(ct$tab)), ct$nota),
    fonte = "Fisher (1922)")
}

#' Teste de correlação.
#' @param dados tabela.
#' @param x,y as duas colunas numéricas.
#' @param metodo `"pearson"`, `"spearman"` ou `"kendall"`.
#' @return objeto `tr_models_test`.
#' @export
tr_models_cor_test <- function(dados, x = "", y = "", metodo = "pearson") {
  no <- "models/cor_test"
  metodo <- .tr_models_enum(metodo, c("pearson", "spearman", "kendall"), "metodo")
  cx <- .tr_models_numerica(dados, .tr_models_col(dados, x, "x"), "x")
  cy <- .tr_models_numerica(dados, .tr_models_col(dados, y, "y"), "y")
  td <- .tr_models_teste_dados(dados, c(cx, cy), no, min_linhas = 4L)
  r <- .tr_models_ajustar(.tr_models_capturar(stats::cor.test(td$d[[cx]], td$d[[cy]], method = metodo)), no)
  t <- r$valor
  simb <- c(pearson = "r", spearman = "rho", kendall = "tau")[[metodo]]
  .tr_models_teste(
    sprintf("Correlação de %s", .tr_models_titulo(metodo)), sprintf("não há correlação entre %s e %s", cx, cy),
    t$statistic, names(t$statistic), t$p.value,
    gl = if (is.null(t$parameter)) NA_character_ else as.character(t$parameter),
    conclusao_sim = sprintf("correlação %s entre %s e %s", if (t$estimate > 0) "positiva" else "negativa", cx, cy),
    conclusao_nao = "não há evidência de correlação",
    efeito = list(rotulo = simb, valor = unname(t$estimate),
                  li = if (is.null(t$conf.int)) NA_real_ else t$conf.int[[1]],
                  ls = if (is.null(t$conf.int)) NA_real_ else t$conf.int[[2]]),
    nota = .tr_models_nota(sprintf("n = %d", nrow(td$d)), td$nota,
                           if (any(grepl("ties|exact", r$avisos))) "empates: p-valor aproximado" else ""),
    fonte = switch(metodo, pearson = "Pearson (1896)", spearman = "Spearman (1904)", kendall = "Kendall (1938)"))
}

.tr_models_titulo <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))

#' Shapiro-Wilk numa coluna.
#' @param dados tabela.
#' @param coluna coluna numérica.
#' @return objeto `tr_models_test`.
#' @export
tr_models_shapiro <- function(dados, coluna = "") {
  no <- "models/shapiro"
  x <- .tr_models_numerica(dados, .tr_models_col(dados, coluna, "coluna"), "coluna")
  td <- .tr_models_teste_dados(dados, x, no)
  if (nrow(td$d) > 5000L) {
    .tr_models_abort("tr_models_error_too_few_rows", "'%s': o Shapiro-Wilk aceita até 5000 valores, e há %d.",
                     no, nrow(td$d))
  }
  t <- .tr_models_ajustar(stats::shapiro.test(td$d[[x]]), no)
  .tr_models_teste(
    "Shapiro-Wilk", sprintf("%s tem distribuição normal", x), t$statistic, "W", t$p.value,
    conclusao_sim = sprintf("%s não é normal", x),
    conclusao_nao = "não há evidência contra a normalidade",
    nota = .tr_models_nota(sprintf("n = %d", nrow(td$d)), td$nota,
                           "para os pressupostos de um modelo, teste os resíduos em 'models/shapiro_residuals'"),
    fonte = "Shapiro & Wilk (1965)")
}
