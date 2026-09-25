# Tamanho de efeito: o QUANTO que o p-valor não diz.
#
# Dois blocos, e não um com entradas opcionais: as perguntas são diferentes
# (quanto da variação cada termo explica, num modelo; quão longe estão duas
# médias, em desvios padrão, numa tabela), as entradas também, e um bloco que
# muda de params conforme a porta ligada é o que confunde quem está começando.
# Os dois devolvem tabela, para ir ao artigo ao lado do quadro.

#' Tamanhos de efeito de cada termo da ANOVA: eta², eta² parcial e ômega².
#'
#' Tudo sai do quadro do `tr_models_anova_table()`, com o erro de CADA termo:
#' o resíduo logo abaixo dele no quadro. Na parcela subdividida isso dá o erro
#' (a) para o fator da parcela e o (b) para a subparcela e a interação, que é o
#' que o F de cada um usa.
#'
#' - eta² = SQ / SQ total: a fração da variação total do termo. Soma no máximo
#'   1 entre os termos, e diminui quando se põem mais termos no modelo.
#' - eta² parcial = SQ / (SQ + SQ do erro): o termo contra o próprio erro, sem
#'   os outros termos no denominador. É o que o SPSS mostra; não soma 1.
#' - ômega² = (SQ − gl · QM do erro) / (SQ total + QM do erro): o eta² corrigido
#'   do viés para cima que ele tem em amostra pequena. Pode sair negativo
#'   quando F < 1 — leia como zero.
#' @param modelo objeto `tr_models_fit` de uma ANOVA (lm, delineamento, subdividida).
#' @param tipo_sq `"I"`, `"II"` ou `"III"`, como no quadro.
#' @return tibble com `termo`, `gl`, `eta2`, `eta2_parcial`, `omega2`.
#' @export
tr_models_effect_size <- function(modelo, tipo_sq = "I") {
  .tr_models_fit_conferir(modelo)
  no <- "models/effect_size"
  .tr_models_exigir(modelo, c("lm", "split"), no,
                    "Eta² e ômega² vêm das somas de quadrados de uma ANOVA; GLM e misto não as têm.")
  q <- as.data.frame(tr_models_anova_table(modelo, tipo_sq = tipo_sq)$tabela)
  residuos <- which(startsWith(q$termo, "Resíduo"))
  termos <- setdiff(which(q$termo != "Total"), residuos)
  # Sem a linha Total (tipo II/III não somam), o total é a soma de todas as SQ.
  sq_total <- if ("Total" %in% q$termo && tipo_sq == "I") q$sq[q$termo == "Total"]
              else sum(q$sq[q$termo != "Total"], na.rm = TRUE)
  erro <- vapply(termos, function(i) residuos[residuos > i][[1]], 1L)
  sq <- q$sq[termos]; gl <- q$gl[termos]
  sq_e <- q$sq[erro]; qm_e <- q$qm[erro]
  tibble::tibble(termo = q$termo[termos], gl = gl,
                 eta2 = sq / sq_total,
                 eta2_parcial = sq / (sq + sq_e),
                 omega2 = (sq - gl * qm_e) / (sq_total + qm_e),
                 erro = q$termo[erro])
}

#' d de Cohen e g de Hedges entre dois grupos, com intervalo.
#'
#' d = (média1 − média2) / DP combinado, com o DP combinado das duas amostras
#' (o do t de Student). g = J · d, com J = 1 − 3 / (4(n1 + n2) − 9), que tira o
#' viés para cima do d em amostra pequena. O intervalo é o normal de Hedges &
#' Olkin (1985): EP(d)² = (n1 + n2) / (n1 n2) + d² / (2(n1 + n2)); o de g é o de
#' d vezes J. Aproximado, e bom a partir de uns 10 por grupo — o exato, pela t
#' não central, difere pouco.
#' @param dados tabela.
#' @param resposta coluna numérica.
#' @param grupo coluna com dois grupos.
#' @param confianca nível do intervalo.
#' @return tibble com `medida`, `estimativa`, `li`, `ls`, `n1`, `n2`.
#' @export
tr_models_cohen_d <- function(dados, resposta = "", grupo = "", confianca = 0.95) {
  no <- "models/cohen_d"
  confianca <- .tr_models_num(confianca, "confianca", min = 0.5, max = 0.999)
  resp <- .tr_models_numerica(dados, .tr_models_col(dados, resposta, "resposta"), "resposta")
  grp <- .tr_models_col(dados, grupo, "grupo")
  td <- .tr_models_teste_dados(dados, c(resp, grp), no, min_linhas = 4L)
  g <- .tr_models_dois_grupos(td$d, grp, no)
  y <- td$d[[resp]]
  a <- y[g == levels(g)[[1]]]; b <- y[g == levels(g)[[2]]]
  n1 <- length(a); n2 <- length(b)
  if (n1 < 2L || n2 < 2L) {
    .tr_models_abort("tr_models_error_too_few_rows", "'%s': cada grupo precisa de pelo menos 2 observações.", no)
  }
  sp <- sqrt(((n1 - 1) * stats::var(a) + (n2 - 1) * stats::var(b)) / (n1 + n2 - 2))
  if (sp == 0) {
    .tr_models_abort("tr_models_error_fit", "'%s': os dois grupos não têm variação; o d não existe.", no)
  }
  d <- (mean(a) - mean(b)) / sp
  ep <- sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)))
  z <- stats::qnorm((1 + confianca) / 2)
  j <- 1 - 3 / (4 * (n1 + n2) - 9)
  tibble::tibble(medida = c(sprintf("d de Cohen (%s − %s)", levels(g)[[1]], levels(g)[[2]]), "g de Hedges"),
                 estimativa = c(d, j * d), li = c(d - z * ep, j * (d - z * ep)),
                 ls = c(d + z * ep, j * (d + z * ep)), confianca = confianca, n1 = n1, n2 = n2)
}
