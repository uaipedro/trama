# Scott & Knott (1974) sobre o erro do quadro da ANOVA.

# O agrupamento à mão, direto das fórmulas do artigo (também em Jelihovschi,
# Faria & Allaman 2014, sec. 4): médias ordenadas, a partição em dois que
# maximiza B, lambda = pi / (2 (pi - 2)) * B0 / sigma0^2, qui-quadrado com
# k / (pi - 2) gl; sigma0^2 = (soma (m - mbarra)^2 + v s2m) / (k + v), s2m = QM / r.
sk_mao <- function(m, s2m, v, alfa = 0.05) {
  m <- sort(m, decreasing = TRUE)
  grupo <- integer(length(m)); prox <- 0L
  parte <- function(idx) {
    k <- length(idx); x <- m[idx]
    if (k == 1L) { prox <<- prox + 1L; grupo[idx] <<- prox; return(invisible()) }
    b <- vapply(seq_len(k - 1L), function(i) {
      t1 <- sum(x[1:i]); t2 <- sum(x[(i + 1):k])
      t1^2 / i + t2^2 / (k - i) - (t1 + t2)^2 / k
    }, 0)
    s0 <- (sum((x - mean(x))^2) + v * s2m) / (k + v)
    lam <- pi / (2 * (pi - 2)) * max(b) / s0
    if (stats::pchisq(lam, k / (pi - 2), lower.tail = FALSE) < alfa) {
      i <- which.max(b); parte(idx[1:i]); parte(idx[(i + 1):k])
    } else { prox <<- prox + 1L; grupo[idx] <<- prox }
  }
  parte(seq_along(m))
  stats::setNames(grupo, names(m))
}

# Grupos iguais = mesma partição dos níveis (os rótulos podem diferir).
mesma_particao <- function(a, b) {
  b <- b[names(a)]
  all(outer(a, a, `==`) == outer(b, b, `==`))
}

test_that("Scott-Knott reproduz o exemplo publicado do sorgo, e o bloco recusa o látice", {
  skip_if_not_installed("ScottKnott")
  # Jelihovschi, Faria & Allaman (2014, TEMA 15(1), Fig. 1): 16 cultivares de
  # sorgo em látice 4 x 4 com 5 repetições, SK a 5% sobre as médias da tabela e
  # o erro de y ~ r/bl + x; dois grupos, de cima 14 8 5 7 9 3 1 4 2.
  sorgo <- get(utils::data("sorghum", package = "ScottKnott", envir = environment()))$dfm
  q <- anova(stats::lm(y ~ r/bl + x, data = sorgo))
  mm <- tapply(sorgo$y, sorgo$x, mean)
  g <- .tr_models_sk(as.vector(mm), q["Residuals", "Mean Sq"] / 5, q["Residuals", "Df"], 0.05)
  expect_setequal(names(mm)[g == 1L], c("14", "8", "5", "7", "9", "3", "1", "4", "2"))
  expect_setequal(names(mm)[g == 2L], c("12", "10", "16", "6", "11", "13", "15"))
  expect_true(mesma_particao(stats::setNames(g, names(mm)),
                             sk_mao(mm, q["Residuals", "Mean Sq"] / 5, q["Residuals", "Df"])))
  # Mas o látice é bloco incompleto: a média da tabela não é a do cultivar (o
  # ScottKnott 1.4 passou a usar as médias ajustadas e agrupa diferente). O
  # bloco recusa em vez de agrupar médias com vício.
  m <- tr_models_lm(sorgo, formula = "y ~ r/bl + x")
  expect_error(tr_models_scott_knott(m, "x"), "ortogonal", class = "tr_models_error_not_applicable")
})

test_that("Scott-Knott no DBC de milho e no DIC bate com a conta e com o pacote", {
  skip_if_not_installed("ScottKnott")
  d <- ex("milho_dbc")
  m <- milho_dbc()
  s <- tr_models_scott_knott(m, "hibrido")
  expect_s3_class(s, "tr_models_emm")
  ref <- ScottKnott::SK(stats::aov(producao ~ bloco + hibrido, data = d), which = "hibrido")
  res <- ref$out$Result
  g_ref <- stats::setNames(apply(res[, -1, drop = FALSE], 1, function(z) which(trimws(z) != "")[[1]]), rownames(res))
  g <- stats::setNames(match(s$tabela$grupo, unique(s$tabela$grupo)), as.character(s$tabela$hibrido))
  expect_true(mesma_particao(g, g_ref))
  # Cada nível tem uma letra só: grupos sem sobreposição.
  expect_true(all(nchar(s$tabela$grupo) == 1L))
  # O grupo "a" é o das maiores médias.
  expect_equal(s$tabela$grupo[which.max(s$tabela$media)], "a")
  # alfa grande separa pelo menos tanto quanto alfa pequeno.
  s1 <- tr_models_scott_knott(m, "hibrido", alfa = 0.01)
  s2 <- tr_models_scott_knott(m, "hibrido", alfa = 0.2)
  expect_lte(length(unique(s1$tabela$grupo)), length(unique(s2$tabela$grupo)))
  # DIC: PlantGrowth.
  p <- tr_models_anova_dic(ex("PlantGrowth"), "weight", "group")
  sp <- tr_models_scott_knott(p, "group")
  q <- anova(stats::lm(weight ~ group, data = PlantGrowth))
  mao <- sk_mao(tapply(PlantGrowth$weight, PlantGrowth$group, mean), q["Residuals", "Mean Sq"] / 10, q["Residuals", "Df"])
  g <- stats::setNames(match(sp$tabela$grupo, unique(sp$tabela$grupo)), as.character(sp$tabela$group))
  expect_true(mesma_particao(g, mao))
})

test_that("Scott-Knott recusa o que não é ANOVA e o desbalanceado", {
  expect_error(tr_models_scott_knott(tr_models_glm(ex("InsectSprays"), "count", "spray"), "spray"),
               class = "tr_models_error_not_applicable")
  d <- ex("PlantGrowth")[-1, ]
  expect_error(tr_models_scott_knott(tr_models_anova_dic(d, "weight", "group"), "group"),
               class = "tr_models_error_not_applicable")
  cov <- tr_models_lm(ex("warpbreaks") |> transform(z = seq_len(54)), formula = "breaks ~ z + tension")
  expect_error(tr_models_scott_knott(cov, "tension"), "covariável", class = "tr_models_error_not_applicable")
})
