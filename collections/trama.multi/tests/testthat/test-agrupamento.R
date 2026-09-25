# Distância, agrupamento, dendrograma e Tocher: conferidos contra `dist()`,
# `mahalanobis()`, `hclust()`/`cutree()`/`cophenetic()` e `kmeans()`, e o
# Tocher contra um exemplo resolvido à mão.

usa <- function() tr_multi_example("USArrests")
vars_usa <- c("Murder", "Assault", "UrbanPop", "Rape")

test_that("euclidiana e padronizada batem com dist()", {
  X <- as.matrix(as.data.frame(usa())[, vars_usa])
  d <- tr_multi_distance(usa(), metodo = "euclidiana", rotulo = "nome")
  expect_equal(as.vector(d$d), as.vector(stats::dist(X)))
  expect_equal(attr(d$d, "Labels"), usa()$nome)
  d2 <- tr_multi_distance(usa(), rotulo = "nome")
  expect_equal(as.vector(d2$d), as.vector(stats::dist(scale(X))))
})

test_that("Mahalanobis é o D² de mahalanobis() par a par", {
  X <- as.matrix(as.data.frame(usa())[, vars_usa])
  S <- stats::cov(X)
  d <- as.matrix(tr_multi_distance(usa(), metodo = "mahalanobis")$d)
  for (par in list(c(1, 2), c(5, 17), c(30, 49))) {
    ref <- stats::mahalanobis(X[par[1], ], X[par[2], ], S)
    expect_equal(d[par[1], par[2]], ref, tolerance = 1e-8)
  }
})

test_that("Gower aceita mistos e bate com cluster::daisy", {
  df <- data.frame(alt = c(1, 2, 3, 10), cor = c("a", "a", "b", "b"))
  d <- tr_multi_distance(df, metodo = "gower")
  expect_equal(as.vector(d$d), as.vector(cluster::daisy(transform(df, cor = factor(cor)), metric = "gower")))
})

test_that("rótulo repetido e faltante são recusados", {
  df <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(2, 5, 1))
  expect_error(tr_multi_distance(df, rotulo = "g"), class = "tr_multi_error_bad_option")
  df$x[2] <- NA
  expect_error(tr_multi_distance(df), class = "tr_multi_error_missing_values")
})

test_that("hierárquico bate com hclust, cutree e cophenetic", {
  X <- scale(as.matrix(as.data.frame(usa())[, vars_usa]))
  for (m in names(trama.multi:::.TR_MULTI_LIGACOES)) {
    ag <- tr_multi_cluster(usa(), rotulo = "nome", metodo = m, grupos = 4L)
    h <- stats::hclust(stats::dist(X), method = trama.multi:::.TR_MULTI_LIGACOES[[m]])
    expect_equal(ag$arvore$merge, h$merge, info = m)
    expect_equal(ag$cofenetica, stats::cor(stats::cophenetic(h), stats::dist(X)), info = m)
    # Mesma partição que o cutree, só com outra numeração dos grupos.
    ref <- stats::cutree(h, 4L)
    expect_equal(as.vector(table(ag$grupos, ref) > 0) |> sum(), 4L, info = m)
  }
})

test_that("a partir da matriz de distância usa a matriz, e as duas entradas são recusadas", {
  d <- tr_multi_distance(usa(), metodo = "mahalanobis", rotulo = "nome")
  ag <- tr_multi_cluster(distancia = d, grupos = 3L)
  expect_equal(ag$arvore$merge, stats::hclust(d$d, "average")$merge)
  expect_error(tr_multi_cluster(usa(), d), class = "tr_multi_error_bad_input")
  expect_error(tr_multi_cluster(), class = "tr_multi_error_bad_input")
  expect_error(tr_multi_cluster(distancia = d, metodo = "k-means"), class = "tr_multi_error_bad_option")
})

test_that("k-means é reprodutível com a semente e bate com kmeans()", {
  a <- tr_multi_cluster(usa(), metodo = "k-means", grupos = 3L, .seed = 42L)
  b <- tr_multi_cluster(usa(), metodo = "k-means", grupos = 3L, .seed = 42L)
  expect_equal(a$grupos, b$grupos)
  X <- scale(as.matrix(as.data.frame(usa())[, vars_usa]))
  set.seed(42L)
  ref <- stats::kmeans(X, 3L, nstart = 25L, iter.max = 100L)
  expect_equal(a$kmeans$tot.withinss, ref$tot.withinss)
  expect_true(is.na(a$cofenetica))
})

test_that("o adaptador devolve os dados com a coluna grupo", {
  ag <- tr_multi_cluster(usa(), rotulo = "nome", grupos = 4L)
  t <- trama.multi:::.tr_multi_cluster_tabela(ag)
  expect_equal(nrow(t), 50L)
  expect_s3_class(t$grupo, "factor")
  expect_equal(levels(t$grupo), paste0("G", 1:4))
  # G1 é o da esquerda do dendrograma.
  expect_equal(as.character(t$grupo[ag$arvore$order[1]]), "G1")
  larga <- trama.multi:::.tr_multi_dist_tabela(tr_multi_distance(usa(), rotulo = "nome"))
  expect_equal(dim(larga), c(50L, 51L))
})

test_that("o dendrograma desenha o corte e colore os grupos", {
  ag <- tr_multi_cluster(usa(), rotulo = "nome", grupos = 4L)
  p <- tr_multi_plot_dendrogram(ag)
  expect_s3_class(p, "ggplot")
  seg <- p$data
  expect_equal(sum(!is.na(unique(seg$grupo))), 4L)
  expect_s3_class(tr_multi_plot_dendrogram(ag, grupos = 2L, horizontal = TRUE), "ggplot")
  km <- tr_multi_cluster(usa(), metodo = "k-means", .seed = 1L)
  expect_s3_class(tr_multi_plot_dendrogram(km), "ggplot")
})

test_that("Tocher: exemplo resolvido à mão", {
  # Cinco indivíduos. Vizinho mais próximo: A–B 1, C 2 (de B), D 2 (de E), E 2
  # → θ = max(1, 1, 2, 2, 2) = 2.
  # G1 começa em A–B (1). Candidatos: C soma 3+2 = 5 → média 2,5 > 2; D 9+8,
  # E 10+9: ninguém entra. G2 começa no par mais próximo que sobra, D–E (2).
  # C: soma 6+7 = 13 → 6,5 > 2, fica fora. G3 = {C}, o último que sobra.
  D <- matrix(c(0, 1, 3, 9, 10,
                1, 0, 2, 8, 9,
                3, 2, 0, 6, 7,
                9, 8, 6, 0, 2,
                10, 9, 7, 2, 0), 5, dimnames = list(LETTERS[1:5], LETTERS[1:5]))
  r <- trama.multi:::.tr_multi_tocher(D)
  expect_equal(r$theta, 2)
  expect_equal(r$grupos, c(1L, 1L, 3L, 2L, 2L))
  # Com C mais perto de A e B (média 1,5 ≤ 2), entra no G1.
  D[3, 1:2] <- D[1:2, 3] <- c(1.5, 1.5)
  expect_equal(trama.multi:::.tr_multi_tocher(D)$grupos, c(1L, 1L, 1L, 2L, 2L))
})

test_that("Tocher: par de abertura acima de θ vira grupos de um (garlicdist do biotools)", {
  # Valores do biotools::tocher(garlicdist) (algoritmo original), fixados aqui
  # para o teste não depender do pacote: θ = 2,324152; o par 16–17 (d = 5,44)
  # passa de θ e cada um fica sozinho.
  skip_if_not_installed("biotools")
  e <- new.env(); utils::data("garlicdist", package = "biotools", envir = e)
  r <- trama.multi:::.tr_multi_tocher(as.matrix(e$garlicdist))
  expect_equal(r$theta, 2.324152, tolerance = 1e-6)
  esperado <- list(c(8, 9, 12, 4, 10, 2, 7, 15), c(1, 6, 14), c(11, 13), c(3, 5), 16, 17)
  obtido <- lapply(seq_len(max(r$grupos)), function(k) which(r$grupos == k))
  expect_equal(lapply(obtido, sort), lapply(esperado, function(v) sort(as.integer(v))))
})

test_that("Tocher no USArrests (D²): nenhum grupo com par acima de θ", {
  d <- tr_multi_distance(usa(), metodo = "mahalanobis", rotulo = "nome")
  t <- tr_multi_tocher(d)
  expect_true(all(t$distancia_media[t$n == 2L] <= t$theta[1]))
  expect_true(all(c("Alaska", "Vermont", "Florida", "Georgia") %in% t$membros[t$n == 1L]))
})

test_that("Ward sobre Mahalanobis usa D (a raiz do D²)", {
  d <- tr_multi_distance(usa(), metodo = "mahalanobis", rotulo = "nome")
  ag <- tr_multi_cluster(distancia = d, metodo = "Ward.D2")
  expect_equal(ag$arvore$merge, stats::hclust(stats::as.dist(sqrt(d$d)), "ward.D2")$merge)
  expect_equal(ag$arvore$height, stats::hclust(stats::as.dist(sqrt(d$d)), "ward.D2")$height)
})

test_that("Tocher no nó: tabela por grupo, todos os indivíduos uma vez", {
  d <- tr_multi_distance(usa(), rotulo = "nome")
  t <- tr_multi_tocher(d)
  expect_named(t, c("grupo", "n", "membros", "distancia_media", "theta"))
  expect_equal(sum(t$n), 50L)
  nomes <- trimws(unlist(strsplit(t$membros, ",")))
  expect_setequal(nomes, usa()$nome)
  # Grupo de mais de um membro tem distância média dentro de θ... em média.
  expect_true(all(is.na(t$distancia_media[t$n == 1L])))
})

test_that("os nós rodam no motor, com a matriz ligada no agrupamento", {
  reg <- multi_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("usa", "multi/example", dataset = "USArrests") |>
    trama::tr_add("d", "multi/distance", rotulo = "nome", from = "usa") |>
    trama::tr_add("ag", "multi/cluster", grupos = 4L, from = "d") |>
    trama::tr_add("dend", "multi/plot_dendrogram", from = "ag") |>
    trama::tr_add("toc", "multi/tocher", from = "d")
  ag <- rodar(f, "ag")
  expect_s3_class(ag, "tr_multi_cluster")
  expect_equal(ag$distancia$metodo, "euclidiana padronizada")
  expect_s3_class(rodar(f, "toc"), "tbl_df")
})
