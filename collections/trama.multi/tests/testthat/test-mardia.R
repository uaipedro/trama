# Mardia (1970): assimetria b1,p e curtose b2,p multivariadas, com a
# covariância de divisor n do artigo. Oráculo: `psych::mardia`, que usa a
# covariância de divisor n − 1 — as distâncias dela são as nossas vezes
# (n − 1)/n, então b1 e b2 dela são os nossos vezes ((n − 1)/n)^3 e ^2. As
# estatísticas de teste (χ², χ² corrigido de Mardia 1974 e z) são as mesmas
# fórmulas do `MVN::mardia` (use_population = TRUE), recalculadas à parte. O
# código do `MVN::mardia` 6.3 (sem instalar: depende do gsl do sistema) deu as
# mesmas estatísticas e p a 1e-13 no iris setosa, iris[1:15, 1:3] e USArrests.

oraculo_psych <- function(m) {
  r <- psych::mardia(m, plot = FALSE)
  n <- nrow(m)
  list(b1 = r$b1p / ((n - 1) / n)^3, b2 = r$b2p / ((n - 1) / n)^2)
}

test_that("b1,p e b2,p batem com o psych (reescalado para divisor n)", {
  skip_if_not_installed("psych")
  for (d in list(iris[1:50, 1:4], iris[, 1:4], datasets::USArrests, datasets::mtcars[, c(1, 3:7)])) {
    m <- as.matrix(d)
    r <- tr_multi_mardia(as.data.frame(d))
    o <- oraculo_psych(m)
    expect_equal(r$coeficiente[[1]], o$b1, tolerance = 1e-10)
    expect_equal(r$coeficiente[[3]], o$b2, tolerance = 1e-10)
  }
})

test_that("estatísticas, gl e p conferem com as fórmulas de Mardia (1970, 1974)", {
  m <- as.matrix(iris[1:50, 1:4]); n <- 50; p <- 4
  x <- scale(m, scale = FALSE)
  S <- crossprod(x) / n
  D <- x %*% solve(S) %*% t(x)
  b1 <- sum(D^3) / n^2; b2 <- sum(diag(D)^2) / n
  gl <- p * (p + 1) * (p + 2) / 6
  k <- (p + 1) * (n + 1) * (n + 3) / (n * ((n + 1) * (p + 1) - 6))
  z <- (b2 - p * (p + 2)) * sqrt(n / (8 * p * (p + 2)))
  r <- tr_multi_mardia(iris[1:50, ], cols = "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width")
  expect_equal(names(r), c("medida", "n", "coeficiente", "estatistica", "gl", "p_valor", "leitura"))
  expect_equal(r$medida, c("assimetria", "assimetria (amostra pequena)", "curtose"))
  expect_equal(r$estatistica, c(n * b1 / 6, n * k * b1 / 6, z), tolerance = 1e-10)
  expect_equal(r$gl, c(gl, gl, NA))
  expect_equal(r$p_valor, c(stats::pchisq(n * b1 / 6, gl, lower.tail = FALSE),
                            stats::pchisq(n * k * b1 / 6, gl, lower.tail = FALSE),
                            2 * stats::pnorm(-abs(z))), tolerance = 1e-12)
  # Valores de referência fixos (iris setosa, psych 2.x reescalado): quem mexer
  # na conta vê o número mudar.
  expect_equal(r$coeficiente[[1]], 3.079721342, tolerance = 1e-8)
  expect_equal(r$estatistica[[2]], 27.859728207, tolerance = 1e-8)
})

test_that("por grupo: uma trinca de linhas por grupo, e o grupo sai das variáveis", {
  r <- tr_multi_mardia(iris, grupo = "Species")
  expect_equal(names(r)[1], "Species")
  expect_equal(as.character(r$Species), rep(levels(iris$Species), each = 3))
  s <- tr_multi_mardia(iris[iris$Species == "virginica", ])
  expect_equal(r$estatistica[7:9], s$estatistica, tolerance = 1e-12)
  expect_equal(r$n, rep(50L, 9))
})

test_that("normal multivariada simulada não rejeita; exponencial rejeita", {
  set.seed(3)
  z <- as.data.frame(matrix(stats::rnorm(400 * 3), 400, 3))
  expect_true(all(tr_multi_mardia(z)$p_valor > .05))
  e <- as.data.frame(matrix(stats::rexp(400 * 3), 400, 3))
  r <- tr_multi_mardia(e)
  expect_true(all(r$p_valor < .001))
  expect_match(r$leitura[[1]], "^rejeita")
  expect_match(tr_multi_mardia(z)$leitura[[1]], "^não rejeita")
  expect_match(tr_multi_mardia(z, confianca = 0.9)$leitura[[1]], "10%")
})

test_that("recusas: poucas linhas, grupo pequeno, confiança fora da faixa", {
  expect_error(tr_multi_mardia(datasets::USArrests[1:5, ]), class = "tr_multi_error_too_few_rows")
  d <- iris[c(1:50, 51:54), ]; d$Species <- droplevels(d$Species)
  expect_error(tr_multi_mardia(d, grupo = "Species"), class = "tr_multi_error_small_group")
  expect_error(tr_multi_mardia(iris, confianca = 1.2), class = "tr_multi_error_bad_option")
  expect_error(tr_multi_mardia(iris, grupo = "Especie"), class = "tr_multi_error_unknown_column")
})

test_that("pelo motor", {
  reg <- multi_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("i", "multi/example", dataset = "iris") |>
    trama::tr_add("m", "multi/mardia", grupo = "Species", from = "i")
  expect_equal(nrow(rodar(f, "m")), 9L)
})
