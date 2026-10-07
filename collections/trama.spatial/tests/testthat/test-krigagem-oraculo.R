# Oráculos da krigagem, do mais forte ao mais barato.
#
# 1. O SISTEMA DE KRIGAGEM RESOLVIDO À MÃO. Monta-se a matriz de covariâncias e
#    resolve-se o sistema com `solve()`: é a conta do capítulo 12 de Isaaks &
#    Srivastava (1989), e não depende de pacote de geoestatística algum. A
#    covariância de cada família é escrita aqui, à parte do gstat:
#      exponencial  C(h) = c * exp(-h / a)
#      esférico     C(h) = c * (1 - 1,5 h/a + 0,5 (h/a)^3), 0 para h >= a
#      gaussiano    C(h) = c * exp(-(h / a)^2)
#      Matérn       C(h) = c * 2^(1-k) / gamma(k) * (h/a)^k * K_k(h/a)
#    com C(0) = pepita + c, e C(h) sem pepita para h > 0. O parâmetro `a` é o
#    phi do modelo, NÃO o alcance prático; a convenção do Matérn (u = h/a, sem o
#    sqrt(2k) de Stein) é a do gstat e do geoR, e foi conferida contra os dois na
#    Tarefa 5.
#    Os casos são montados para distinguir hipóteses rivais:
#      - pepita > 0 em toda família, porque com pepita zero um erro no tratamento
#        da pepita não aparece;
#      - o modelo carrega `alcance_pratico` diferente de `alcance`, porque usar o
#        prático no lugar de phi seria o erro natural;
#      - Matérn em kappa 1,5, onde as parametrizações rivais divergem (em 0,5
#        coincidem);
#      - esférico com pontos além do alcance, para exercitar C(h) = 0;
#      - alvos fora da ordem e fora de qualquer simetria dos pontos.
#    Valores de referência independentes, rodados antes da tarefa: para os cinco
#    pontos de Isaaks (fixture abaixo, exponencial, pepita 0, c = 10, a = 80,
#    alvo (30, 60)) o sistema à mão, `gstat::krige` e `geoR::krige.conv` deram
#    predito 13,7683147552 e variância 3,5802184923.
# 2. geoR::krige.conv, a referência do material de aula, em dados reais e em
#    metros.
# 3. EXATIDÃO: a krigagem é interpolador exato. Predizer numa coordenada amostral
#    devolve o valor observado e variância zero quando a pepita é zero.
#
# Tolerâncias: `expect_equal(tolerance = 1e-8)` nos oráculos à mão e 1e-6 no
# geoR e na exatidão. O que se mediu de verdade está no relatório da tarefa.

# ---- a conta à mão ----------------------------------------------------------------

cov_mao <- function(h, familia, pepita, contrib, alcance, kappa = NA_real_) {
  u <- h / alcance
  c_h <- switch(familia,
    exponencial = contrib * exp(-u),
    esferico = ifelse(u >= 1, 0, contrib * (1 - 1.5 * u + 0.5 * u^3)),
    gaussiano = contrib * exp(-u^2),
    matern = contrib * 2^(1 - kappa) / gamma(kappa) * u^kappa * besselK(u, kappa))
  ifelse(h <= 0, pepita + contrib, c_h)
}

# Sistema de krigagem em UM alvo. `usar` restringe aos pontos da vizinhança.
# Ordinária: [C 1; 1' 0][w; mu] = [c0; 1]; var = C(0) - w'c0 - mu.
# Simples (média m): w = C^-1 c0; pred = m + w'(z - m); var = C(0) - w'c0.
krig_mao <- function(d, alvo, mod, media = NULL, usar = seq_len(nrow(d))) {
  d <- d[usar, , drop = FALSE]
  n <- nrow(d)
  cv <- function(h) cov_mao(h, mod$familia, mod$pepita, mod$contribuicao, mod$alcance, mod$kappa)
  C <- cv(as.matrix(stats::dist(d[, c("x", "y")])))
  c0 <- cv(sqrt((d$x - alvo[[1]])^2 + (d$y - alvo[[2]])^2))
  c00 <- mod$pepita + mod$contribuicao
  if (is.null(media)) {
    sol <- solve(rbind(cbind(C, 1), c(rep(1, n), 0)), c(c0, 1))
    w <- sol[seq_len(n)]
    list(predito = sum(w * d$z), variancia = c00 - sum(w * c0) - sol[[n + 1L]], pesos = w)
  } else {
    w <- solve(C, c0)
    list(predito = media + sum(w * (d$z - media)), variancia = c00 - sum(w * c0), pesos = w)
  }
}

modelo_mao <- function(familia, pepita, contrib, alcance, kappa = NA_real_) {
  # Os dois alcances são DIFERENTES de propósito (exceto no esférico, onde são
  # o mesmo por definição): quem lesse `alcance_pratico` no lugar de `alcance`
  # erraria a conta.
  pratico <- switch(familia, exponencial = 3 * alcance, gaussiano = sqrt(3) * alcance,
                    esferico = alcance, matern = 4.2 * alcance)
  structure(list(familia = familia, pepita = pepita, contribuicao = contrib,
                 alcance = alcance, alcance_pratico = pratico,
                 patamar = pepita + contrib, kappa = kappa, metodo = "manual",
                 sqr = NA_real_, grau_dependencia = pepita / (pepita + contrib),
                 variograma = NULL, nota = ""), class = "tr_spatial_model")
}

# 12 pontos sem simetria; distâncias de ~14 a ~110, de modo que o esférico de
# alcance 60 deixa pares de fora.
dados_mao <- function() {
  data.frame(
    x = c(12, 85, 47, 3, 66, 91, 28, 74, 55, 19, 38, 97),
    y = c(80, 15, 52, 33, 91, 60, 5, 44, 70, 22, 64, 87),
    z = c(14.2, 9.8, 12.5, 16.1, 11.0, 8.7, 15.3, 10.4, 12.9, 15.8, 13.6, 9.1))
}
alvos_mao <- function() {
  # Fora da ordem e de qualquer simetria; o último fica longe de tudo.
  data.frame(x = c(61, 22, 90, 40, 140), y = c(33, 58, 40, 12, 20))
}

test_that("os cinco pontos de Isaaks reproduzem os três valores de referência", {
  d <- data.frame(x = c(0, 100, 0, 100, 50), y = c(0, 0, 100, 100, 50),
                  z = c(10, 14, 12, 18, 15))
  p <- tr_spatial_coordinates(d, "x", "y", "z")
  m <- modelo_mao("exponencial", 0, 10, 80)
  s <- tr_spatial_kriging_em(p, m, novos = data.frame(x = 30, y = 60))
  # Os dois números vieram de gstat e geoR rodados fora deste teste, e do
  # sistema resolvido à mão; ver o cabeçalho.
  expect_equal(s$predito, 13.7683147552, tolerance = 1e-8)
  expect_equal(s$variancia, 3.5802184923, tolerance = 1e-8)
  mao <- krig_mao(d, c(30, 60), m)
  expect_equal(sum(mao$pesos), 1, tolerance = 1e-12)
  expect_equal(s$predito, mao$predito, tolerance = 1e-8)
  expect_equal(s$variancia, mao$variancia, tolerance = 1e-8)
})

test_that("a krigagem ordinária resolve o mesmo sistema que o cálculo à mão, em quatro famílias", {
  d <- dados_mao()
  p <- tr_spatial_coordinates(d, "x", "y", "z")
  alvos <- alvos_mao()
  casos <- list(
    modelo_mao("exponencial", pepita = 2, contrib = 6, alcance = 35),
    modelo_mao("esferico", pepita = 2, contrib = 6, alcance = 60),
    modelo_mao("gaussiano", pepita = 2, contrib = 6, alcance = 30),
    modelo_mao("matern", pepita = 2, contrib = 6, alcance = 40, kappa = 1.5))
  for (m in casos) {
    s <- tr_spatial_kriging_em(p, m, novos = alvos)
    for (i in seq_len(nrow(alvos))) {
      mao <- krig_mao(d, unlist(alvos[i, ]), m)
      expect_equal(s$predito[[i]], mao$predito, tolerance = 1e-8,
                   info = paste(m$familia, "alvo", i, "predito"))
      expect_equal(s$variancia[[i]], mao$variancia, tolerance = 1e-8,
                   info = paste(m$familia, "alvo", i, "variancia"))
    }
  }
})

test_that("a pepita muda a conta, e a conta à mão a acompanha", {
  # Guarda do próprio oráculo: se pepita 0 e 2 dessem o mesmo resultado, os
  # testes acima não distinguiriam um tratamento errado da pepita.
  d <- dados_mao()
  p <- tr_spatial_coordinates(d, "x", "y", "z")
  a <- tr_spatial_kriging_em(p, modelo_mao("exponencial", 0, 8, 35), alvos_mao())
  b <- tr_spatial_kriging_em(p, modelo_mao("exponencial", 2, 6, 35), alvos_mao())
  expect_gt(max(abs(a$predito - b$predito)), 0.01)
  expect_gt(max(abs(a$variancia - b$variancia)), 0.1)
})

test_that("a krigagem simples resolve o sistema sem a restrição, com média diferente da amostral", {
  d <- dados_mao()
  p <- tr_spatial_coordinates(d, "x", "y", "z")
  alvos <- alvos_mao()
  m <- modelo_mao("exponencial", pepita = 2, contrib = 6, alcance = 35)
  mu <- 20  # longe da média amostral (~12): uma média ignorada aparece.
  expect_gt(abs(mu - mean(d$z)), 5)
  s <- tr_spatial_kriging_em(p, m, novos = alvos, tipo = "simples", media = mu)
  for (i in seq_len(nrow(alvos))) {
    mao <- krig_mao(d, unlist(alvos[i, ]), m, media = mu)
    expect_equal(s$predito[[i]], mao$predito, tolerance = 1e-8, info = paste("alvo", i))
    expect_equal(s$variancia[[i]], mao$variancia, tolerance = 1e-8, info = paste("alvo", i))
  }
  # Longe dos dados a simples volta à média informada, e a ordinária à amostral.
  longe <- data.frame(x = 5000, y = 5000)
  expect_equal(tr_spatial_kriging_em(p, m, longe, "simples", mu)$predito, mu, tolerance = 1e-6)
  expect_equal(tr_spatial_kriging_em(p, m, longe)$predito, mean(d$z), tolerance = 1e-2)
})

test_that("a vizinhança local resolve o sistema só com os pontos da vizinhança", {
  d <- dados_mao()
  p <- tr_spatial_coordinates(d, "x", "y", "z")
  m <- modelo_mao("exponencial", pepita = 2, contrib = 6, alcance = 35)
  alvos <- alvos_mao()[1:4, ]
  dist_a <- function(i) sqrt((d$x - alvos$x[[i]])^2 + (d$y - alvos$y[[i]])^2)

  # Os k mais próximos. Sem empate de distância nesses alvos (conferido).
  k <- 5L
  s <- tr_spatial_kriging_em(p, m, alvos, vizinhos_max = k)
  for (i in seq_len(nrow(alvos))) {
    h <- dist_a(i)
    expect_false(anyDuplicated(round(h, 9)) > 0)
    usar <- order(h)[seq_len(k)]
    mao <- krig_mao(d, unlist(alvos[i, ]), m, usar = usar)
    expect_equal(s$predito[[i]], mao$predito, tolerance = 1e-8, info = paste("nmax alvo", i))
    expect_equal(s$variancia[[i]], mao$variancia, tolerance = 1e-8, info = paste("nmax alvo", i))
  }
  # E a vizinhança muda de fato o resultado, senão o teste acima é vácuo.
  g <- tr_spatial_kriging_em(p, m, alvos)
  expect_gt(max(abs(g$predito - s$predito)), 0.01)

  # Todos os pontos a menos de `raio`. O raio é o da mediana das distâncias ao
  # primeiro alvo, então deixa uns dentro e outros fora, em todos os alvos.
  raio <- stats::median(dist_a(1))
  r <- tr_spatial_kriging_em(p, m, alvos, dist_max = raio)
  for (i in seq_len(nrow(alvos))) {
    h <- dist_a(i)
    usar <- which(h <= raio)
    if (length(usar) < 2L) next
    mao <- krig_mao(d, unlist(alvos[i, ]), m, usar = usar)
    expect_equal(r$predito[[i]], mao$predito, tolerance = 1e-8, info = paste("raio alvo", i))
    expect_equal(r$variancia[[i]], mao$variancia, tolerance = 1e-8, info = paste("raio alvo", i))
  }
  expect_gt(max(abs(g$predito - r$predito), na.rm = TRUE), 0.01)
})

# ---- exatidão ---------------------------------------------------------------------

test_that("a krigagem é interpolador exato nas coordenadas amostrais com pepita zero", {
  d <- dados_mao()
  p <- tr_spatial_coordinates(d, "x", "y", "z")
  for (m in list(modelo_mao("esferico", 0, 10, 150),
                 modelo_mao("exponencial", 0, 10, 40),
                 modelo_mao("matern", 0, 10, 50, kappa = 1.5))) {
    s <- tr_spatial_kriging_em(p, m, novos = d[, c("x", "y")])
    expect_equal(s$predito, d$z, tolerance = 1e-6, info = m$familia)
    expect_equal(s$variancia, rep(0, nrow(d)), tolerance = 1e-6, info = m$familia)
    expect_lt(max(abs(s$variancia)), 1e-6)
  }
})

test_that("a exatidão vale em metros, nos pontos reais, com o modelo ajustado e pepita zero", {
  p <- tr_spatial_example("milho_se")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p))
  m$pepita <- 0  # modelo montado: sem pepita, exato; com a escala dos dados em 1e6.
  s <- tr_spatial_kriging_em(p, m, novos = p$coords)
  z <- p$dados[[p$variavel]]
  # 1e-6 do desvio-padrão dos dados.
  expect_lt(max(abs(s$predito - z)), 1e-6 * stats::sd(z))
  expect_lt(max(s$variancia), 1e-6 * m$contribuicao)
})

# ---- geoR -------------------------------------------------------------------------

krige_geor <- function(p, m, novos, tipo, media = NULL, cov.model, kappa = 0.5) {
  g <- geoR::as.geodata(cbind(p$coords, p$dados[[p$variavel]]), coords.col = 1:2, data.col = 3)
  ctrl <- if (identical(tipo, "simples")) {
    geoR::krige.control(type.krige = "SK", beta = media, cov.model = cov.model,
                        cov.pars = c(m$contribuicao, m$alcance), nugget = m$pepita, kappa = kappa)
  } else {
    geoR::krige.control(type.krige = "OK", cov.model = cov.model,
                        cov.pars = c(m$contribuicao, m$alcance), nugget = m$pepita, kappa = kappa)
  }
  # krige.conv NÃO aceita `messages = FALSE`; imprime progresso, e se silencia assim.
  suppressMessages(utils::capture.output(
    o <- geoR::krige.conv(g, locations = as.matrix(novos), krige = ctrl)))
  o
}

test_that("reproduz geoR::krige.conv, ordinária, nos pontos reais e em metros", {
  skip_if_not_installed("geoR")
  for (ds in c("milho_se", "milho_pr")) {
    p <- tr_spatial_example(ds)
    m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "esferico")
    expect_gt(m$pepita, 0)
    # Uma grade 5 x 5 dentro da extensão dos pontos, não de uma escala de brinquedo.
    novos <- expand.grid(
      x = seq(min(p$coords[, 1]), max(p$coords[, 1]), length.out = 5),
      y = seq(min(p$coords[, 2]), max(p$coords[, 2]), length.out = 5))
    nossa <- tr_spatial_kriging_em(p, m, novos = novos)
    o <- krige_geor(p, m, novos, "ordinaria", cov.model = "spherical")
    expect_equal(nossa$predito, as.numeric(o$predict), tolerance = 1e-6, info = ds)
    expect_equal(nossa$variancia, as.numeric(o$krige.var), tolerance = 1e-6, info = ds)
  }
})

test_that("reproduz geoR::krige.conv, simples, com média diferente da amostral", {
  skip_if_not_installed("geoR")
  p <- tr_spatial_example("milho_se")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "esferico")
  z <- p$dados[[p$variavel]]
  mu <- mean(z) + 2 * stats::sd(z)
  novos <- expand.grid(
    x = seq(min(p$coords[, 1]), max(p$coords[, 1]), length.out = 4),
    y = seq(min(p$coords[, 2]), max(p$coords[, 2]), length.out = 4))
  nossa <- tr_spatial_kriging_em(p, m, novos = novos, tipo = "simples", media = mu)
  o <- krige_geor(p, m, novos, "simples", media = mu, cov.model = "spherical")
  expect_equal(nossa$predito, as.numeric(o$predict), tolerance = 1e-6)
  expect_equal(nossa$variancia, as.numeric(o$krige.var), tolerance = 1e-6)
  # E a média faz diferença: a ordinária dá outra coisa.
  ord <- tr_spatial_kriging_em(p, m, novos = novos)
  expect_gt(max(abs(ord$predito - nossa$predito)), 0.05 * stats::sd(z))
})

test_that("reproduz geoR::krige.conv no Matérn de kappa 1,5, onde as parametrizações divergem", {
  skip_if_not_installed("geoR")
  d <- dados_mao()
  p <- tr_spatial_coordinates(d, "x", "y", "z")
  m <- modelo_mao("matern", pepita = 2, contrib = 6, alcance = 40, kappa = 1.5)
  nossa <- tr_spatial_kriging_em(p, m, novos = alvos_mao())
  o <- krige_geor(p, m, alvos_mao(), "ordinaria", cov.model = "matern", kappa = 1.5)
  expect_equal(nossa$predito, as.numeric(o$predict), tolerance = 1e-6)
  expect_equal(nossa$variancia, as.numeric(o$krige.var), tolerance = 1e-6)
})

test_that("geoR e o cálculo à mão concordam entre si nos cinco pontos de Isaaks", {
  skip_if_not_installed("geoR")
  d <- data.frame(x = c(0, 100, 0, 100, 50), y = c(0, 0, 100, 100, 50),
                  z = c(10, 14, 12, 18, 15))
  p <- tr_spatial_coordinates(d, "x", "y", "z")
  m <- modelo_mao("exponencial", 0, 10, 80)
  o <- krige_geor(p, m, data.frame(x = 30, y = 60), "ordinaria", cov.model = "exponential")
  expect_equal(as.numeric(o$predict), 13.7683147552, tolerance = 1e-8)
  expect_equal(as.numeric(o$krige.var), 3.5802184923, tolerance = 1e-8)
})
