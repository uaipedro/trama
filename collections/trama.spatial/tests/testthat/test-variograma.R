# Distâncias em METROS (UTM): o corte vem dos dados, nunca de um número copiado.

test_that("o variograma sai com as colunas do contrato e classes crescentes", {
  v <- tr_spatial_variogram(tr_spatial_example("milho_pr"))
  expect_s3_class(v, "tr_spatial_variogram")
  expect_true(all(c("u", "gamma", "np") %in% names(v$tabela)))
  expect_true(all(diff(v$tabela$u) > 0))
  expect_true(all(v$tabela$np >= 30L))
  expect_equal(v$estimador, "classico")
  # corte padrão: um terço da diagonal do estado, algumas centenas de km
  expect_gt(v$dist_max, 1e5)
  expect_lt(v$dist_max, 1e6)
  expect_lte(max(v$tabela$u), v$dist_max)
})

test_that("a fórmula traduz cada tendência", {
  p <- tr_spatial_example("milho_pr")
  f <- function(t) deparse1(.tr_spatial_formula(p, t))
  expect_equal(f("constante"), "milho_kg_ha ~ 1")
  expect_equal(f("1a ordem"), "milho_kg_ha ~ leste + norte")
  expect_equal(f("2a ordem"),
               "milho_kg_ha ~ leste + norte + I(leste^2) + I(norte^2) + I(leste * norte)")
  expect_equal(f("covariavel"), "milho_kg_ha ~ soja_kg_ha")
})

# Só exercício: NÃO é oráculo. Nenhuma referência externa confere o valor do
# variograma com tendência por covariável; este teste só garante que o caminho
# roda e que a covariável de fato muda o resultado.
test_that("a tendência por covariável roda em milho_pr e muda o variograma", {
  p <- tr_spatial_example("milho_pr")
  a <- tr_spatial_variogram(p, tendencia = "covariavel")
  b <- tr_spatial_variogram(p)
  expect_true(all(is.finite(a$tabela$gamma)))
  expect_equal(a$tabela$np, b$tabela$np)
  expect_false(isTRUE(all.equal(a$tabela$gamma, b$tabela$gamma)))
})

test_that("tendência por covariável sem covariável declarada é erro", {
  expect_error(tr_spatial_variogram(tr_spatial_example("milho_se"), tendencia = "covariavel"),
               class = "tr_spatial_error_blank_param")
})

test_that("o estimador robusto difere do clássico e ambos são positivos", {
  p <- tr_spatial_example("milho_pr")
  a <- tr_spatial_variogram(p, estimador = "classico")
  b <- tr_spatial_variogram(p, estimador = "robusto")
  expect_true(all(a$tabela$gamma > 0))
  expect_true(all(b$tabela$gamma > 0))
  expect_false(isTRUE(all.equal(a$tabela$gamma, b$tabela$gamma)))
  expect_equal(b$estimador, "robusto")
})

test_that("pares_min alto demais derruba todas as classes e vira erro nomeado", {
  expect_error(tr_spatial_variogram(tr_spatial_example("milho_se"), pares_min = 100000L),
               class = "tr_spatial_error_empty_variogram")
})

test_that("classe cortada por pares_min aparece na nota", {
  v <- tr_spatial_variogram(tr_spatial_example("milho_se"), n_classes = 40L, pares_min = 30L)
  expect_match(v$nota, "classe")
})

test_that("direcional devolve menos pares que o omnidirecional na mesma classe", {
  p <- tr_spatial_example("milho_pr")
  corte <- .tr_spatial_corte_padrao(p$coords)
  o <- tr_spatial_variogram(p, dist_max = corte, n_classes = 10L)
  d <- tr_spatial_variogram(p, dist_max = corte, n_classes = 10L, direcao = 0, tolerancia = 22.5)
  expect_lt(sum(d$tabela$np), sum(o$tabela$np))
  expect_equal(d$direcao, 0)
})

# A tolerância padrão é o que torna "direcional" direcional. Este teste NÃO
# passa `tolerancia`: quem passa o valor não vigia o padrão. Com 90 graus o
# setor é o plano inteiro e o direcional sai idêntico ao omnidirecional.
test_that("o padrão da tolerância angular faz o direcional ter menos pares que o omnidirecional", {
  p <- tr_spatial_example("milho_pr")
  corte <- .tr_spatial_corte_padrao(p$coords)
  o <- tr_spatial_variogram(p, dist_max = corte, n_classes = 10L, pares_min = 1L)
  d <- tr_spatial_variogram(p, dist_max = corte, n_classes = 10L, pares_min = 1L, direcao = 30)
  expect_equal(d$tolerancia, 22.5)
  expect_lt(sum(d$tabela$np), sum(o$tabela$np))
  expect_false(isTRUE(all.equal(d$tabela$gamma, o$tabela$gamma)))
  skip_if_not_installed("geoR")
  g <- geoR::as.geodata(cbind(p$coords, p$dados[[p$variavel]]), coords.col = 1:2, data.col = 3)
  r <- geoR::variog(g, breaks = seq(0, corte, length.out = 11), direction = 30 * pi / 180,
                    tolerance = 22.5 * pi / 180, messages = FALSE)
  expect_equal(as.integer(d$tabela$np), as.integer(r$n))
  expect_equal(d$tabela$gamma, r$v, tolerance = 1e-8)
})

test_that("o adaptador para data/table dá uma linha por classe", {
  v <- tr_spatial_variogram(tr_spatial_example("milho_pr"))
  t <- .tr_spatial_vario_tabela(v)
  expect_equal(nrow(t), nrow(v$tabela))
  expect_true(all(c("u", "gamma", "np") %in% names(t)))
})

test_that("o store recusa objeto que não é variograma", {
  expect_error(.tr_spatial_vario_conferir(list(a = 1)),
               class = "tr_spatial_error_not_a_variogram")
})

test_that("a nota conta as classes pedidas, inclusive as que o gstat já descartou por vazias", {
  se <- tr_spatial_example("milho_se")
  v <- tr_spatial_variogram(se, n_classes = 40L, pares_min = 30L)
  expect_match(v$nota, sprintf("%d das 40 classes", 40L - nrow(v$tabela)))
})

test_that("pares_min e direcao inválidos viram erro nomeado, não erro cru do R", {
  p <- tr_spatial_example("milho_se")
  expect_error(tr_spatial_variogram(p, pares_min = NA), class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_variogram(p, direcao = 400), class = "tr_spatial_error_bad_option")
})
