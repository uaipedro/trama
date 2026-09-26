# Regressão polinomial nos tratamentos quantitativos da ANOVA (polinômios
# ortogonais: SQ por grau, desvios, falta de ajuste do grau escolhido, curva).
# Desde a 9.2 é o bloco único (absorveu o `models/dose_response`); o quadro
# sai em `$quadro` e a curva em `$modelo`.

algodao <- function() {
  # Dados de Montgomery, Design and Analysis of Experiments (5.ª ed.), tabela
  # 3.1: resistência à tração pela % de algodão, 5 níveis igualmente
  # espaçados, 5 repetições. As SQ por grau abaixo são CALCULADAS desses dados
  # e conferidas à mão pelos contrastes ortogonais — não copiadas de página
  # impressa do livro.
  data.frame(algodao = rep(seq(15, 35, 5), each = 5),
             resistencia = c(7, 7, 15, 11, 9, 12, 17, 12, 18, 18, 14, 18, 18, 19, 19,
                             19, 25, 22, 19, 23, 7, 10, 11, 15, 11))
}

linha <- function(t, termo) t$tabela[t$tabela$termo == termo, ]

test_that("polinomial nos dados de Montgomery (SQ 33,62; 343,21; 64,98; 33,95) e o lm com poly()", {
  d <- algodao()
  m <- tr_models_anova_dic(d, "resistencia", "algodao")
  r <- tr_models_polinomial(m, "algodao", grau_max = 3L)
  t <- r$quadro
  expect_s3_class(t, "tr_models_effects")
  expect_equal(round(linha(t, "Linear")$sq, 2), 33.62)
  expect_equal(round(linha(t, "Quadrático")$sq, 2), 343.21)
  expect_equal(round(linha(t, "Cúbico")$sq, 2), 64.98)
  expect_equal(round(linha(t, "Desvios da regressão")$sq, 2), 33.95)  # o quártico, com grau máximo 3
  expect_equal(linha(t, "Desvios da regressão")$gl, 1)
  # À mão: contrastes ortogonais para 5 níveis, SQ = (sum c T)^2 / (r sum c^2).
  tot <- tapply(d$resistencia, d$algodao, sum)
  cc <- list(c(-2, -1, 0, 1, 2), c(2, -1, -2, -1, 2), c(-1, 2, 0, -2, 1), c(1, -4, 6, -4, 1))
  sq_mao <- vapply(cc, function(k) sum(k * tot)^2 / (5 * sum(k^2)), numeric(1))
  expect_equal(c(linha(t, "Linear")$sq, linha(t, "Quadrático")$sq, linha(t, "Cúbico")$sq,
                 linha(t, "Desvios da regressão")$sq), sq_mao, tolerance = 1e-10)
  # Oráculo: sequencial de lm(y ~ poly(x, 4)) coluna a coluna.
  p <- stats::poly(d$algodao, 4)
  a <- anova(stats::lm(d$resistencia ~ p[, 1] + p[, 2] + p[, 3] + p[, 4]))
  expect_equal(c(linha(t, "Linear")$sq, linha(t, "Quadrático")$sq, linha(t, "Cúbico")$sq,
                 linha(t, "Desvios da regressão")$sq), a$`Sum Sq`[1:4], tolerance = 1e-10)
  expect_equal(linha(t, "Cúbico")$F, a$`F value`[[3]], tolerance = 1e-10)
  expect_equal(linha(t, "Cúbico")$p_valor, a$`Pr(>F)`[[3]], tolerance = 1e-10)
  expect_equal(linha(t, "Resíduo")$qm, a$`Mean Sq`[[5]], tolerance = 1e-10)
  # Oráculo 2: contr.poly com os scores = doses reais, SQ = r (c'ȳ)² / Σc².
  mu <- tapply(d$resistencia, d$algodao, mean)
  C <- stats::contr.poly(5, scores = c(15, 20, 25, 30, 35))
  expect_equal(linha(t, "Linear")$sq, 5 * (sum(C[, 1] * mu))^2 / sum(C[, 1]^2), tolerance = 1e-10)
  # Curva do maior grau significativo (cúbico) e R² = SQ da regressão / SQ de tratamentos.
  cf <- stats::coef(stats::lm(resistencia ~ algodao + I(algodao^2) + I(algodao^3), data = d))
  expect_equal(unname(stats::coef(r$modelo$ajuste)), unname(cf), tolerance = 1e-8)
  expect_equal(r$modelo$grau, 3L)
  expect_equal(r$modelo$r2, sum(a$`Sum Sq`[1:3]) / sum(a$`Sum Sq`[1:4]), tolerance = 1e-10)
  expect_match(t$rodape[["equação"]], "^ŷ = ")
})

test_that("doses não equidistantes: scores reais do contr.poly; DBC desbalanceado: anova do lm", {
  set.seed(7)
  dose <- c(0, 50, 100, 200, 400)
  d <- expand.grid(dose = dose, bloco = paste0("B", 1:4))
  d$y <- 10 + 0.03 * d$dose - 5e-5 * d$dose^2 + as.integer(factor(d$bloco)) + stats::rnorm(nrow(d))
  m <- tr_models_anova_dbc(d, "y", "dose", "bloco")
  t <- tr_models_polinomial(m, "dose", grau = "2", grau_max = 2L)$quadro
  # Balanceado, espaçamento desigual: contr.poly(scores = doses) nas médias.
  mu <- tapply(d$y, d$dose, mean)
  C <- stats::contr.poly(5, scores = dose)
  sq <- as.vector(4 * (t(C) %*% mu)^2 / colSums(C^2))
  expect_equal(linha(t, "Linear")$sq, sq[[1]], tolerance = 1e-10)
  expect_equal(linha(t, "Quadrático")$sq, sq[[2]], tolerance = 1e-10)
  expect_equal(linha(t, "Desvios da regressão")$sq, sq[[3]] + sq[[4]], tolerance = 1e-10)
  expect_equal(linha(t, "Desvios da regressão")$gl, 2)
  q <- tr_models_anova_table(m)$tabela
  expect_equal(linha(t, "Tratamentos")$sq, q$sq[q$termo == "dose"], tolerance = 1e-10)
  # DBC desbalanceado (duas parcelas perdidas): o oráculo é a anova sequencial
  # de lm(y ~ bloco + x + x² + x³ + factor(dose)).
  d2 <- d[-c(1, 7), ]
  m2 <- tr_models_anova_dbc(d2, "y", "dose", "bloco")
  t2 <- tr_models_polinomial(m2, "dose", grau = "3")$quadro
  a2 <- anova(stats::lm(y ~ bloco + dose + I(dose^2) + I(dose^3) + factor(dose), data = d2))
  expect_equal(c(linha(t2, "Linear")$sq, linha(t2, "Quadrático")$sq, linha(t2, "Cúbico")$sq,
                 linha(t2, "Desvios da regressão")$sq), a2$`Sum Sq`[2:5], tolerance = 1e-10)
  expect_equal(linha(t2, "Cúbico")$p_valor, a2$`Pr(>F)`[[4]], tolerance = 1e-10)
  expect_equal(linha(t2, "Resíduo")$gl, a2$Df[[6]])
  expect_match(t2$nota, "repetições desiguais", fixed = TRUE)
})

test_that("polinomial recusa tratamento não numérico, grau alto e delineamento fora do DIC/DBC/DQL", {
  pg <- tr_models_anova_dic(ex("PlantGrowth"), "weight", "group")
  expect_error(tr_models_polinomial(pg, "group"), class = "tr_models_error_not_numeric")
  m <- tr_models_anova_dic(algodao(), "resistencia", "algodao")
  expect_error(tr_models_polinomial(m, "algodao", grau = "5"), class = "tr_models_error_bad_option")
  # O maior grau testado acima de k − 1 desce, com nota, em vez de recusar.
  r <- tr_models_polinomial(m, "algodao", grau_max = 5L)
  expect_true(all(c("Linear", "Quártico") %in% r$quadro$tabela$termo))
  expect_match(r$quadro$nota, "reduzido a 4", fixed = TRUE)
  expect_error(tr_models_polinomial(tr_models_glm(ex("InsectSprays"), "count", "spray"), "spray"),
               class = "tr_models_error_not_applicable")
})

test_that("migração: models/dose_response e o grau numérico da main abrem no polinomial", {
  reg <- models_registry()
  doc <- list(nodes = list(
    a = list(type = "models/polinomial", params = list(tratamento = "dose", grau = 2, alfa = 0.01)),
    b = list(type = "models/dose_response", params = list(tratamento = "dose", grau = "2"))),
    edges = list(list(from = list(node = "a", port = "out"), to = list(node = "x", port = "tabela"))))
  m <- trama::tr_doc_migrate(doc, reg)
  expect_equal(m$nodes$a$params$grau_max, 2L)
  expect_null(m$nodes$a$params[["grau"]])  # [[ ]]: o $ casaria grau_max por prefixo
  expect_equal(m$nodes$a$params$confianca, 0.99)
  expect_equal(m$nodes$b$type, "models/polinomial")
  expect_equal(m$nodes$b$params$grau, "2")
  expect_equal(m$edges[[1]]$from$port, "quadro")
})
