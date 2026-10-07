# Comportamento da grade, da krigagem e do tipo. A conta em si (o que a krigagem
# devolve) é vigiada em test-krigagem-oraculo.R; aqui vigia-se o que cerca a conta.
#
# Coordenadas em METROS. Nenhuma distância abaixo é copiada de outra escala:
# cortes e raios saem dos dados.

# Ajusta uma vez por sessão de teste; o ajuste do milho_se leva ~0,5 s.
pm_cache <- new.env()
pm <- function(dataset = "milho_se") {
  if (is.null(pm_cache[[dataset]])) {
    p <- tr_spatial_example(dataset)
    pm_cache[[dataset]] <- list(p = p, m = tr_spatial_variogram_fit(tr_spatial_variogram(p)))
  }
  pm_cache[[dataset]]
}

# 6 pontos dentro de um L ASSIMÉTRICO: o quadrado 0..20 sem o canto x > 14, y > 6.
# Assimétrico de propósito: numa borda simétrica, trocar x por y no recorte
# daria o mesmo número de células.
pontos_l <- function() {
  d <- data.frame(x = c(2, 8, 12, 5, 10, 3), y = c(2, 4, 3, 12, 15, 18), z = c(1, 2, 3, 4, 5, 6))
  borda <- matrix(c(0, 0, 20, 0, 20, 6, 14, 6, 14, 20, 0, 20, 0, 0), ncol = 2, byrow = TRUE)
  tr_spatial_coordinates(d, "x", "y", "z", borda = borda)
}

# ---- a grade --------------------------------------------------------------------

test_that("o recorte na borda tem a contagem exata de células, e deixa o canto de fora", {
  g <- tr_spatial_grid(pontos_l(), 21L)
  # Grade 21 x 21 de passo 1. Saem as células com x em 15..20 e y em 7..20:
  # 6 * 14 = 84. As que caem SOBRE a borda (x = 14, y = 6) ficam.
  expect_equal(nrow(g), 21L * 21L - 6L * 14L)
  expect_false(any(g$x > 14 & g$y > 6))
  expect_true(any(g$x == 14 & g$y == 20))
  expect_true(any(g$x == 20 & g$y == 6))
  expect_equal(names(g), c("x", "y"))
})

test_that("a grade respeita a resolução no lado maior e a proporção da área", {
  d <- data.frame(x = c(0, 40, 20, 5), y = c(0, 10, 5, 9), z = 1:4)
  p <- tr_spatial_coordinates(d, "x", "y", "z")
  g <- tr_spatial_grid(p, 41L)
  expect_length(unique(g$x), 41L)
  expect_length(unique(g$y), 10L)  # round(41 * 10 / 40) = 10
  expect_equal(range(g$x), c(0, 40))
  expect_equal(range(g$y), c(0, 10))
  expect_equal(nrow(g), 41L * 10L)
})

test_that("a grade cobre a borda inteira mesmo quando os pontos ocupam só parte dela", {
  p <- pontos_l()  # pontos de 2 a 12 em x; a borda vai de 0 a 20
  g <- tr_spatial_grid(p, 21L)
  expect_equal(range(g$x), c(0, 20))
  expect_equal(range(g$y), c(0, 20))
})

test_that("os pontos reais: toda célula está dentro da borda, e a grade é menor que o retângulo", {
  p <- pm()$p
  g <- tr_spatial_grid(p, 30L)
  expect_lt(nrow(g), length(unique(g[[1]])) * length(unique(g[[2]])))
  pol <- sf::st_sfc(sf::st_polygon(list(p$borda)))
  dentro <- sf::st_within(sf::st_as_sf(g, coords = names(g)), pol)
  # A célula sobre a borda não é `within`; só vale para as interiores.
  expect_gt(mean(lengths(dentro) > 0L), 0.95)
})

test_that("uma borda em quilômetros contra pontos em metros dá grade vazia, nomeando a borda", {
  # Review Focus 5. Se a extensão viesse só da borda, esta borda produziria uma
  # grade completa na escala errada, e o erro nunca apareceria.
  p <- pm()$p
  p$borda <- p$borda / 1000
  e <- expect_error(tr_spatial_grid(p, 40L), class = "tr_spatial_error_empty_grid")
  expect_match(conditionMessage(e), "borda")
  expect_error(tr_spatial_kriging(p, pm()$m, resolucao = 40L),
               class = "tr_spatial_error_empty_grid")
})

test_that("a borda de um retângulo em escala errada é recusada na entrada, não vira grade de uma célula", {
  # O retângulo é o caso DIFÍCIL e não deve ser trocado por um polígono real: a
  # caixa da borda dividida por 1000 tem o canto inferior esquerdo SOBRE o
  # polígono, e a primeira célula da grade (canto da caixa unida) cai ali.
  # `st_intersects` a mantém, e o recorte devolvia 1 linha e nenhum erro.
  p <- pm()$p
  bb <- rbind(apply(p$borda, 2, min), apply(p$borda, 2, max)) / 1000
  ret <- matrix(c(bb[1, 1], bb[1, 2], bb[2, 1], bb[1, 2], bb[2, 1], bb[2, 2],
                  bb[1, 1], bb[2, 2], bb[1, 1], bb[1, 2]), ncol = 2, byrow = TRUE)
  e <- expect_error(
    tr_spatial_coordinates(p$dados, p$coord_cols[[1]], p$coord_cols[[2]], p$variavel,
                           borda = ret),
    class = "tr_spatial_error_bad_border")
  expect_match(conditionMessage(e), "unidade")
  expect_match(conditionMessage(e), sprintf("%d de %d", nrow(p$dados), nrow(p$dados)), fixed = TRUE)
})

test_that("os três exemplos carregam com a guarda da borda, e a folga de 2 km não se aperta", {
  for (ds in c("milho_pr", "cafe_mg", "milho_se")) {
    p <- tr_spatial_example(ds)
    pol <- sf::st_sfc(sf::st_polygon(list(p$borda)))
    pts <- sf::st_as_sf(as.data.frame(p$coords), coords = 1:2)
    d <- as.numeric(sf::st_distance(pts, pol))
    expect_lte(max(d), .TR_SPATIAL_FOLGA_BORDA, label = ds)
  }
})

test_that("resolução fora da faixa ou que não é inteiro é erro de opção", {
  p <- pm()$p
  for (r in list(1L, 4L, 501L, 5.5, NA, "a", c(10L, 20L))) {
    expect_error(tr_spatial_grid(p, r), class = "tr_spatial_error_bad_option", info = format(r))
  }
  expect_error(tr_spatial_kriging(p, pm()$m, resolucao = 1L),
               class = "tr_spatial_error_bad_option")
  expect_gt(nrow(tr_spatial_grid(p, 5L)), 0L)
})

test_that("a grade recusa o que não é objeto espacial", {
  expect_error(tr_spatial_grid(list(a = 1)), class = "tr_spatial_error_not_points")
})

# ---- a krigagem -----------------------------------------------------------------

test_that("a krigagem ordinária devolve a grade com predito, variância e erro-padrão", {
  x <- pm()
  s <- tr_spatial_kriging(x$p, x$m, resolucao = 20L)
  expect_s3_class(s, "tr_spatial_surface")
  expect_equal(names(s$grade), c(x$p$coord_cols, "predito", "variancia", "erro_padrao"))
  expect_true(all(is.finite(s$grade$predito)))
  expect_true(all(s$grade$variancia >= 0))
  expect_equal(s$grade$erro_padrao, sqrt(s$grade$variancia), tolerance = 1e-12)
  expect_equal(s$tipo, "ordinaria")
  expect_equal(s$vizinhanca, "global")
  expect_equal(s$resolucao, 20L)
  expect_match(s$nota, sprintf("Grade de %d celulas (resolucao 20)", nrow(s$grade)), fixed = TRUE)
  expect_equal(s$variavel, x$p$variavel)
  expect_identical(s$borda, x$p$borda)
  expect_equal(s$modelo$familia, x$m$familia)
  # A grade da superfície É a grade do recorte, na mesma ordem, e a conta é a
  # mesma da função interna: o empacotamento não perde nem reordena célula.
  g <- tr_spatial_grid(x$p, 20L)
  expect_equal(as.data.frame(s$grade[, 1:2]), g, ignore_attr = TRUE)
  em <- tr_spatial_kriging_em(x$p, x$m, g)
  expect_equal(s$grade$predito, em$predito, tolerance = 1e-12)
})

# SANITY CHECK, nao garantia. Um envelope folgado e uma correlacao de postos
# > 0,5 passariam por muitas implementacoes erradas. A cobertura real da conta
# esta nos oraculos resolvidos a mao e no geoR, em test-krigagem-oraculo.R.
test_that("sanidade: a predição fica na escala dos dados e o erro cresce longe dos pontos", {
  x <- pm()
  s <- tr_spatial_kriging(x$p, x$m, resolucao = 25L)
  z <- x$p$dados[[x$p$variavel]]
  # Krigagem ordinária é combinação afim; com pesos moderados fica perto do
  # envelope dos dados. Folga de 25% da amplitude.
  folga <- 0.25 * diff(range(z))
  expect_gte(min(s$grade$predito), min(z) - folga)
  expect_lte(max(s$grade$predito), max(z) + folga)
  # Distância ao ponto mais próximo, calculada à mão, contra o erro-padrão.
  dmin <- vapply(seq_len(nrow(s$grade)), function(i) {
    min(sqrt((x$p$coords[, 1] - s$grade[[1]][[i]])^2 + (x$p$coords[, 2] - s$grade[[2]][[i]])^2))
  }, 0)
  expect_gt(stats::cor(dmin, s$grade$erro_padrao, method = "spearman"), 0.5)
})

# Review Focus 3: o gstat aceita a simples sem beta e resolve com zero.
test_that("krigagem simples sem média é erro antes de chamar o motor", {
  x <- pm()
  # Se o motor fosse chamado, este mock o denunciaria com outra classe de erro.
  local_mocked_bindings(krige = function(...) stop("motor chamado"), .package = "gstat")
  for (m in list(NA, NA_real_, NULL, "a", c(1, 2), Inf)) {
    expect_error(tr_spatial_kriging(x$p, x$m, tipo = "simples", media = m),
                 class = "tr_spatial_error_no_mean", info = format(m))
    expect_error(tr_spatial_kriging_em(x$p, x$m, novos = x$p$coords, tipo = "simples", media = m),
                 class = "tr_spatial_error_no_mean", info = format(m))
  }
  expect_error(tr_spatial_kriging(x$p, x$m, tipo = "simples"),
               class = "tr_spatial_error_no_mean")
})

test_that("a validação das opções vem antes da grade", {
  x <- pm()
  local_mocked_bindings(tr_spatial_grid = function(...) stop("grade chamada"),
                        .package = "trama.spatial")
  expect_error(tr_spatial_kriging(x$p, x$m, tipo = "simples"),
               class = "tr_spatial_error_no_mean")
  expect_error(tr_spatial_kriging(x$p, x$m, tipo = "universal"),
               class = "tr_spatial_error_bad_option")
})

test_that("a krigagem simples roda com média e a registra", {
  x <- pm()
  mu <- mean(x$p$dados[[x$p$variavel]])
  s <- tr_spatial_kriging(x$p, x$m, tipo = "simples", media = mu, resolucao = 15L)
  expect_equal(s$tipo, "simples")
  o <- tr_spatial_kriging(x$p, x$m, resolucao = 15L)
  # Mesma grade, média diferente de zero: predições diferentes das da ordinária
  # (a simples não estima a média) e variância NÃO maior: a ordinária paga a
  # estimação da média, e var_OK = var_SK + (custo >= 0).
  expect_false(isTRUE(all.equal(s$grade$predito, o$grade$predito)))
  expect_true(all(s$grade$variancia <= o$grade$variancia + 1e-6))
  expect_lt(sum(s$grade$variancia), sum(o$grade$variancia))
})

test_that("tipo, vizinhos e raio inválidos são erro de opção", {
  x <- pm()
  r <- function(...) tr_spatial_kriging(x$p, x$m, resolucao = 5L, ...)
  expect_error(r(tipo = "universal"), class = "tr_spatial_error_bad_option")
  expect_error(r(tipo = c("ordinaria", "simples")), class = "tr_spatial_error_bad_option")
  for (v in list(0, -3, 2.5, Inf, "a", c(3, 4))) {
    expect_error(r(vizinhos_max = v), class = "tr_spatial_error_bad_option", info = format(v))
  }
  for (d in list(0, -1, Inf, "a", c(1, 2))) {
    expect_error(r(dist_max = d), class = "tr_spatial_error_bad_option", info = format(d))
  }
})

test_that("a vizinhança local muda o resultado e fica registrada", {
  x <- pm()
  g <- tr_spatial_kriging(x$p, x$m, resolucao = 15L)
  l <- tr_spatial_kriging(x$p, x$m, resolucao = 15L, vizinhos_max = 10L)
  expect_false(isTRUE(all.equal(g$grade$predito, l$grade$predito)))
  expect_equal(l$vizinhanca, "ate 10 vizinhos")
  # O raio sai dos dados: um quarto da diagonal da área dos pontos.
  raio <- 0.25 * sqrt(sum(apply(x$p$coords, 2, function(v) diff(range(v)))^2))
  r <- tr_spatial_kriging(x$p, x$m, resolucao = 15L, dist_max = raio)
  expect_equal(r$vizinhanca, sprintf("raio de %g", raio))
  both <- tr_spatial_kriging(x$p, x$m, resolucao = 15L, vizinhos_max = 10L, dist_max = raio)
  expect_equal(both$vizinhanca, sprintf("ate 10 vizinhos, raio de %g", raio))
})

test_that("célula sem ponto dentro do raio fica sem predição, e a nota conta quantas", {
  x <- pm()
  # Um décimo do menor passo da grade: quase nenhuma célula tem ponto no raio.
  g <- tr_spatial_grid(x$p, 12L)
  passo <- min(diff(sort(unique(g[[1]]))))
  s <- tr_spatial_kriging(x$p, x$m, resolucao = 12L, dist_max = passo / 10)
  sem <- sum(is.na(s$grade$predito))
  expect_gt(sem, 0L)
  expect_equal(is.na(s$grade$predito), is.na(s$grade$variancia))
  expect_match(s$nota, sprintf("%d de %d celulas ficaram sem predicao", sem, nrow(s$grade)),
               fixed = TRUE)
  # Com a vizinhança global não há aviso nenhum.
  expect_false(grepl("sem predicao", tr_spatial_kriging(x$p, x$m, resolucao = 12L)$nota))
})

test_that("a coordenada de `novos` é lida pelo nome, na ordem que vier", {
  x <- pm()
  alvo <- x$p$coords[1:3, ] + 1000
  a <- tr_spatial_kriging_em(x$p, x$m, novos = alvo)
  inv <- data.frame(alvo[, 2], alvo[, 1]); names(inv) <- rev(x$p$coord_cols)
  b <- tr_spatial_kriging_em(x$p, x$m, novos = inv)
  expect_equal(b$predito, a$predito)
  expect_equal(b[[1]], a[[1]])
  # E, sem os nomes, vale a posição.
  c3 <- tr_spatial_kriging_em(x$p, x$m, novos = unname(as.matrix(alvo)))
  expect_equal(c3$predito, a$predito)
})

test_that("variância negativa só vira erro acima do limite relativo à escala do modelo", {
  x <- pm()
  patamar <- x$m$pepita + x$m$contribuicao
  falso <- function(v) {
    function(...) data.frame(var1.pred = rep(1, length(v)), var1.var = v)
  }
  alvo <- x$p$coords[1:2, ]
  # Grosseira: um décimo do patamar.
  local_mocked_bindings(krige = falso(c(-0.1 * patamar, 1)), .package = "gstat")
  expect_error(tr_spatial_kriging_em(x$p, x$m, alvo),
               class = "tr_spatial_error_negative_variance")
})

test_that("o limite da variância negativa acompanha a escala do modelo, nos dois sentidos", {
  x <- pm()
  falso <- function(v) {
    function(...) data.frame(var1.pred = rep(1, length(v)), var1.var = v)
  }
  alvo <- x$p$coords[1:2, ]
  # Escala enorme (patamar 1e12): -1e-3 é ruído de arredondamento (1e-15 do
  # patamar) e NÃO pode virar erro. Um limite absoluto de 1e-8 o recusaria.
  grande <- x$m; grande$contribuicao <- 1e12; grande$pepita <- 0
  local_mocked_bindings(krige = falso(c(-1e-3, 4)), .package = "gstat")
  expect_equal(tr_spatial_kriging_em(x$p, grande, alvo)$variancia, c(0, 4))
  # Escala minúscula (patamar 1e-9): -5e-10 é metade do patamar, absurdo, e TEM
  # de ser erro. Um limite absoluto de 1e-8 o deixaria passar.
  pequeno <- x$m; pequeno$contribuicao <- 1e-9; pequeno$pepita <- 0
  local_mocked_bindings(krige = falso(c(-5e-10, 4e-10)), .package = "gstat")
  expect_error(tr_spatial_kriging_em(x$p, pequeno, alvo),
               class = "tr_spatial_error_negative_variance")
})

test_that("ruído de arredondamento em torno de zero é cortado em zero, sem erro", {
  x <- pm()
  patamar <- x$m$pepita + x$m$contribuicao
  local_mocked_bindings(
    krige = function(...) data.frame(var1.pred = c(1, 1), var1.var = c(-1e-12 * patamar, 4)),
    .package = "gstat")
  r <- tr_spatial_kriging_em(x$p, x$m, x$p$coords[1:2, ])
  expect_equal(r$variancia, c(0, 4))
  expect_equal(r$erro_padrao, c(0, 2))
})

# ---- o tipo, o adaptador e o nó -------------------------------------------------

test_that("o adaptador para data/table dá a grade inteira", {
  x <- pm()
  s <- tr_spatial_kriging(x$p, x$m, resolucao = 12L)
  tab <- .tr_spatial_superficie_tabela(s)
  expect_equal(nrow(tab), nrow(s$grade))
  expect_equal(names(tab), names(s$grade))
})

test_that("o tipo spatial/surface recusa o que não é superfície e guarda a superfície", {
  ty <- spatial_surface_type()
  expect_error(ty$store(list(a = 1), tempfile()), class = "tr_spatial_error_not_a_surface")
  expect_error(ty$store(pm()$p, tempfile()), class = "tr_spatial_error_not_a_surface")
  expect_error(.tr_spatial_superficie_conferir(list(a = 1)),
               class = "tr_spatial_error_not_a_surface")
  x <- pm()
  s <- tr_spatial_kriging(x$p, x$m, resolucao = 10L)
  f <- tempfile(fileext = ".rds")
  ty$store(s, f)
  r <- ty$restore(f)
  expect_equal(r$grade$predito, s$grade$predito)
  expect_s3_class(r, "tr_spatial_surface")
})

test_that("o preview da superfície carrega a grade e os quartis", {
  x <- pm()
  s <- tr_spatial_kriging(x$p, x$m, resolucao = 10L)
  p <- .tr_spatial_superficie_preview(s)
  expect_equal(p$celulas, nrow(s$grade))
  expect_length(p$quartis_predito, 5L)
  expect_equal(p$quartis_predito[[3]], signif(stats::median(s$grade$predito), 6))
})

test_that("o nó registra, declara as portas certas e roda no motor", {
  reg <- spatial_registry()
  nd <- Filter(function(n) n$id == "spatial/kriging", trama_collection()$nodes)[[1]]
  expect_equal(names(nd$inputs), c("pontos", "modelo"))
  expect_equal(nd$version, 1L)
  nomes <- names(nd$params)
  expect_equal(nomes, c("tipo", "media", "resolucao", "vizinhos_max", "dist_max"))
  fl <- trama::tr_flow(reg) |>
    trama::tr_add("p", "spatial/example", dataset = "milho_se") |>
    trama::tr_add("v", "spatial/variogram", from = "p") |>
    trama::tr_add("m", "spatial/variogram_fit", from = "v") |>
    trama::tr_add("k", "spatial/kriging", resolucao = 12L, from = c("p", "m"))
  s <- rodar(fl, "k")
  expect_s3_class(s, "tr_spatial_surface")
  expect_equal(s$resolucao, 12L)
})

test_that("o nó leva a superfície a uma tabela pelo adaptador", {
  reg <- spatial_registry()
  fl <- trama::tr_flow(reg) |>
    trama::tr_add("p", "spatial/example", dataset = "milho_se") |>
    trama::tr_add("v", "spatial/variogram", from = "p") |>
    trama::tr_add("m", "spatial/variogram_fit", from = "v") |>
    trama::tr_add("k", "spatial/kriging", resolucao = 8L, from = c("p", "m")) |>
    trama::tr_add("t", "data/slice_head", from = "k")
  t <- rodar(fl, "t")
  expect_true(all(c("predito", "erro_padrao") %in% names(t)))
})
