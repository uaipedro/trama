# Jackknife por grupo (apagar-um-grupo; Shao & Tu 1995, sec. 2.3 e 6.2;
# Kott 2001): cada réplica tira um GRUPO inteiro de linhas, e as fórmulas
# são as do jackknife com G réplicas no lugar de n:
# EP = √((G − 1)/G · Σ(θ₍g₎ − θ̄)²). Oráculo: `survey` com os grupos como
# UPAs e réplicas JK1 (`as.svrepdesign(type = "JK1")`), média e razão, a 1e-10.

d_grupos <- function() {
  set.seed(11)
  data.frame(bloco = rep(sprintf("b%02d", 1:9), times = c(3, 5, 2, 4, 4, 6, 3, 5, 4)),
             x = stats::rnorm(36, 10), y = stats::runif(36, 1, 3))
}

test_that("média e razão: EP do jackknife por grupo = survey JK1 (1e-10)", {
  skip_if_not_installed("survey")
  d <- d_grupos()
  ds <- survey::as.svrepdesign(survey::svydesign(ids = ~bloco, weights = ~w, data = transform(d, w = 1)), type = "JK1")
  ex_m <- function(o) c(media = mean(o$x))
  tab <- .tr_multi_jackknife(d, c("x", "y"), d, function(dd) dd, ex_m, "x", "resumo", .95,
                             grupos = d$bloco)
  expect_equal(tab$erro_padrao, unname(survey::SE(survey::svymean(~x, ds))), tolerance = 1e-10)
  ex_r <- function(o) c(razao = sum(o$y) / sum(o$x))
  tab <- .tr_multi_jackknife(d, c("x", "y"), d, function(dd) dd, ex_r, "x", "resumo", .95,
                             grupos = d$bloco)
  expect_equal(tab$erro_padrao, unname(survey::SE(survey::svyratio(~y, ~x, ds))), tolerance = 1e-10)
  expect_equal(tab$estimativa, sum(d$y) / sum(d$x))
  # Grupos de tamanhos diferentes: o IC centra na estimativa (ver o teste abaixo).
  expect_equal(tab$ic_sup - tab$estimativa, stats::qt(.975, 8) * tab$erro_padrao)
})

test_that("à mão: 3 grupos, média", {
  d <- data.frame(g = c("a", "a", "b", "b", "b", "c"), x = c(1, 3, 2, 4, 6, 10))
  tab <- suppressWarnings(.tr_multi_jackknife(d, "x", d, function(dd) dd, function(o) c(m = mean(o$x)),
                                              "x", "resumo", .95, grupos = d$g))
  # Sem a: (2+4+6+10)/4 = 5,5; sem b: (1+3+10)/3 = 14/3; sem c: 16/5 = 3,2.
  th <- c(5.5, 14 / 3, 3.2)
  expect_equal(tab$media_jackknife, mean(th))
  expect_equal(tab$erro_padrao, sqrt(2 / 3 * sum((th - mean(th))^2)))
  ps <- suppressWarnings(.tr_multi_jackknife(d, "x", d, function(dd) dd, function(o) c(m = mean(o$x)), "x",
                                             "pseudovalores", .95, grupos = d$g))
  expect_equal(names(ps)[1:6], c("grupo_removido", "linhas", "estatistica", "sem_ela", "pseudovalor", "influencia"))
  expect_equal(ps$grupo_removido, c("a", "b", "c"))
  expect_equal(ps$linhas, c(2L, 3L, 1L))
  expect_equal(ps$sem_ela, th)
})

test_that("grupos de tamanho igual: viés, corrigida e pseudovalores com G; IC na corrigida", {
  d <- data.frame(g = rep(c("a", "b", "c", "d", "e"), each = 2), x = c(1, 3, 2, 4, 6, 10, 5, 5, 0, 7))
  f <- function(o) c(m = mean(o$x)^2)
  tab <- .tr_multi_jackknife(d, "x", d, function(dd) dd, f, "x", "resumo", .95, grupos = d$g)
  th <- vapply(c("a", "b", "c", "d", "e"), function(k) mean(d$x[d$g != k])^2, 0)
  expect_equal(tab$vies, 4 * (mean(th) - mean(d$x)^2))
  expect_equal(tab$corrigida, mean(d$x)^2 - tab$vies)
  expect_equal(tab$ic_sup - tab$corrigida, stats::qt(.975, 4) * tab$erro_padrao)
  expect_false("nota" %in% names(tab))
  ps <- .tr_multi_jackknife(d, "x", d, function(dd) dd, f, "x", "pseudovalores", .95, grupos = d$g)
  expect_equal(ps$pseudovalor, 5 * mean(d$x)^2 - 4 * unname(th))
  expect_equal(mean(ps$pseudovalor), tab$corrigida)
})

test_that("grupos de tamanhos diferentes: sem viés/corrigida/pseudovalor, IC centrado na estimativa", {
  # As fórmulas de viés e pseudovalor com G supõem grupos do mesmo tamanho;
  # com tamanhos diferentes não há fórmula conferida, então saem NA e o
  # intervalo centra em θ̂ (a amostra toda), com o EP JK1 (que vale assim).
  d <- data.frame(g = rep(c("a", "b", "c", "d", "e"), times = c(1, 2, 3, 2, 2)),
                  x = c(1, 3, 2, 4, 6, 10, 5, 5, 0, 7))
  tab <- .tr_multi_jackknife(d, "x", d, function(dd) dd, function(o) c(m = mean(o$x)), "x", "resumo",
                             .95, grupos = d$g)
  expect_true(is.na(tab$vies) && is.na(tab$corrigida))
  expect_equal(tab$ic_inf, mean(d$x) - stats::qt(.975, 4) * tab$erro_padrao)
  expect_equal(tab$ic_sup, mean(d$x) + stats::qt(.975, 4) * tab$erro_padrao)
  expect_match(tab$nota, "tamanhos diferentes")
  ps <- .tr_multi_jackknife(d, "x", d, function(dd) dd, function(o) c(m = mean(o$x)), "x",
                            "pseudovalores", .95, grupos = d$g)
  expect_true(all(is.na(ps$pseudovalor)))
  expect_true(all(is.finite(ps$influencia)))
  expect_match(ps$nota[[1]], "tamanhos diferentes")
  # Razão de chances: o centro também é exponenciado uma vez só.
  tl <- .tr_multi_jackknife(d, "x", d, function(dd) dd, function(o) c(m = mean(o$x)), "x", "resumo",
                            .95, log = TRUE, grupos = d$g)
  expect_equal(tl$ic_sup, exp(mean(d$x) + stats::qt(.975, 4) * tab$erro_padrao))
})

test_that("menos de 5 grupos: aviso", {
  d <- data.frame(g = rep(c("a", "b", "c", "d"), each = 2), x = c(1, 3, 2, 4, 6, 10, 5, 5))
  expect_warning(.tr_multi_jackknife(d, "x", d, function(dd) dd, function(o) c(m = mean(o$x)), "x",
                                     "resumo", .95, grupos = d$g), "4 grupos")
  d <- rbind(d, data.frame(g = "e", x = c(0, 1)))
  expect_no_warning(.tr_multi_jackknife(d, "x", d, function(dd) dd, function(o) c(m = mean(o$x)), "x",
                                        "resumo", .95, grupos = d$g))
})

test_that("os quatro blocos aceitam `grupo`, e a réplica é o ajuste sem o grupo", {
  ua <- as.data.frame(tr_multi_example("USArrests"))
  ua$regiao <- rep(c("n", "s", "l", "o", "c"), length.out = nrow(ua))
  pca <- tr_multi_pca(ua, cols = "Murder, Assault, UrbanPop, Rape")
  ps <- tr_multi_jackknife_pca(pca, tabela = "pseudovalores", grupo = "regiao")
  sem_n <- tr_multi_pca(ua[ua$regiao != "n", ], cols = "Murder, Assault, UrbanPop, Rape")
  expect_equal(ps$sem_ela[ps$estatistica == "CP1" & ps$grupo_removido == "n"], sem_n$ajuste$sdev[[1]]^2)
  expect_equal(nrow(tr_multi_jackknife_pca(pca, grupo = "regiao")), 4L)
  pima <- as.data.frame(tr_multi_example("pima"))[1:150, ]
  pima$lote <- rep(1:10, each = 15)
  lg <- tr_multi_logistic(pima, grupo = "diabetes", cols = "glicose, imc, pedigree")
  r <- tr_multi_jackknife_logistic(lg, grupo = "lote")
  expect_true(all(is.finite(r$erro_padrao)))
  l <- tr_multi_discriminant(transform(as.data.frame(tr_multi_example("iris")), lote = rep(1:10, 15)),
                             grupo = "Species", cols = "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width")
  expect_equal(nrow(tr_multi_jackknife_discriminant(l, grupo = "lote")), 2L)
  expect_error(tr_multi_jackknife_pca(pca, grupo = "nao_existe"), class = "tr_multi_error_unknown_column")
})

test_that("recusas: faltante, um grupo só, o próprio grupo do classificador", {
  ua <- as.data.frame(tr_multi_example("USArrests"))
  ua$r <- "x"
  pca <- tr_multi_pca(ua, cols = "Murder, Assault, UrbanPop, Rape")
  expect_error(tr_multi_jackknife_pca(pca, grupo = "r"), class = "tr_multi_error_one_group")
  ua$r <- rep(c("a", NA), 25)
  pca <- tr_multi_pca(ua, cols = "Murder, Assault, UrbanPop, Rape")
  expect_error(tr_multi_jackknife_pca(pca, grupo = "r"), class = "tr_multi_error_missing_values")
  l <- tr_multi_discriminant(tr_multi_example("iris"), grupo = "Species")
  expect_error(tr_multi_jackknife_discriminant(l, grupo = "Species"), class = "tr_multi_error_bad_option")
})
