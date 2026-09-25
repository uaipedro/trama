# Oráculo: `rsm::rsm` + `canonical` sobre `rsm::ChemReact` (composto central
# em dois blocos; Myers, Montgomery & Anderson-Cook, 3rd ed., 2009, tabela 7.6,
# como cita a documentação do rsm). Tolerância 1e-8 relativa: mesma conta, mesmo QR.

cr_codificado <- function(d = rsm::ChemReact) {
  d$x1 <- (d$Time - 85) / 5; d$x2 <- (d$Temp - 175) / 5
  d
}

test_that("2ª ordem com blocos: coeficientes, quadro e canônica iguais aos do rsm", {
  skip_if_not_installed("rsm")
  d <- cr_codificado()
  s <- tr_experiments_response_surface(d, "Yield", "x1, x2", "2", "Block")
  expect_s3_class(s$modelo, "tr_models_fit")
  o <- rsm::rsm(Yield ~ Block + SO(x1, x2), data = d)
  expect_equal(unname(sort(coef(s$modelo$ajuste))), unname(sort(coef(o))), tolerance = 1e-8)
  a <- summary(o)$lof
  t <- s$quadro$tabela
  expect_equal(t$sq, unname(a$`Sum Sq`), tolerance = 1e-8)
  expect_equal(t$gl, unname(a$Df), tolerance = 1e-12)
  expect_equal(t$p_valor[t$termo == "Falta de ajuste"], a$`Pr(>F)`[[6]], tolerance = 1e-8)
  cn <- rsm::canonical(o)
  v <- stats::setNames(s$canonica$valor, s$canonica$item)
  expect_equal(as.numeric(v[c("x_s · x1", "x_s · x2")]), unname(cn$xs), tolerance = 1e-5)
  expect_equal(as.numeric(v[c("autovalor 1", "autovalor 2")]), cn$eigen$values, tolerance = 1e-5)
  expect_equal(unname(v["natureza"]), "máximo")
})

test_that("1ª ordem: falta de ajuste igual à do rsm e direção de maior subida", {
  skip_if_not_installed("rsm")
  d <- cr_codificado(rsm::ChemReact1)
  s <- tr_experiments_response_surface(d, "Yield", "x1, x2", "1")
  o <- rsm::rsm(Yield ~ FO(x1, x2), data = d)
  a <- summary(o)$lof
  expect_equal(s$quadro$tabela$sq, unname(a$`Sum Sq`), tolerance = 1e-8)
  b <- coef(o)[2:3]  # FO(x1, x2)x1, FO(x1, x2)x2
  expect_equal(as.numeric(s$canonica$valor), unname(b / sqrt(sum(b^2))), tolerance = 1e-5)
})

test_that("sela é reconhecida, e fatores na unidade original são recusados", {
  set.seed(1)
  g <- expand.grid(x1 = c(-1, 0, 1), x2 = c(-1, 0, 1))
  g$y <- 10 + g$x1^2 - g$x2^2 + stats::rnorm(9, sd = 0.01)
  s <- tr_experiments_response_surface(g, "y", "x1, x2", "2")
  expect_equal(s$canonica$valor[s$canonica$item == "natureza"], "ponto de sela")
  expect_error(tr_experiments_response_surface(cr_codificado(), "Yield", "Time, Temp", "2"),
               class = "tr_experiments_error_bad_option")
})

test_that("com bloco, o erro puro é o resíduo de y ~ bloco + ponto, como no rsm", {
  skip_if_not_installed("rsm")
  # CCD 2³ com 6 centrais, 2 repetições como blocos. Antes da correção o erro
  # puro era calculado dentro de ponto × bloco: EP 10 gl, FA 19 gl.
  Z <- as.data.frame(tr_experiments_design("composto_central", "A; B; C", pontos_centrais = 6,
                                           repeticoes = 2, .seed = 8)$unidades)
  set.seed(31)
  Z$y <- 10 + Z$A - Z$B^2 - Z$C^2 - Z$A^2 + 0.5 * Z$A * Z$C + as.numeric(Z$repeticao) +
    stats::rnorm(nrow(Z), sd = 0.5)
  t <- tr_experiments_response_surface(Z, "y", "A, B, C", "2", "repeticao")$quadro$tabela
  a <- summary(rsm::rsm(y ~ repeticao + SO(A, B, C), data = Z))$lof
  expect_equal(t$gl, unname(a$Df), tolerance = 1e-12)
  expect_equal(t$gl[t$termo == "Erro puro"], 24)
  expect_equal(t$sq, unname(a$`Sum Sq`), tolerance = 1e-8)
  expect_equal(t$p_valor[t$termo == "Falta de ajuste"], a$`Pr(>F)`[[6]], tolerance = 1e-8)
})
