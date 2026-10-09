# Contrato do `spatial/anisotropy`: o param de direções, a equivalência a 90
# graus de tolerância, e o que acontece quando uma direção não tem pares.

test_that("o param de direções recusa entrada ruim e aceita a boa", {
  p <- tr_spatial_example("milho_se")
  expect_error(tr_spatial_anisotropy(p, direcoes = "0,45,180"),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_anisotropy(p, direcoes = "45"),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_anisotropy(p, direcoes = "0,45,45"),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_anisotropy(p, direcoes = "0,norte"),
               class = "tr_spatial_error_bad_option")
  a <- tr_spatial_anisotropy(p, pares_min = 1L)
  expect_equal(a$direcoes, c(0, 45, 90, 135))
  expect_s3_class(a, "tr_spatial_anisotropy")
})

test_that("as direções pedidas são as que saem na tabela, e vêm ordenadas", {
  p <- tr_spatial_example("milho_pr")
  a <- tr_spatial_anisotropy(p, direcoes = "130,10,70", pares_min = 1L)
  expect_equal(a$direcoes, c(10, 70, 130))
  expect_setequal(unique(a$tabela$direcao), c(10, 70, 130))
  expect_false(is.unsorted(a$tabela$direcao))
})

test_that("a 90 graus de tolerância cada direcional vira o omnidirecional", {
  # A 90 graus a janela angular cobre todo o semicírculo, então não há direção:
  # é a mesma conta do `spatial/variogram` sem direção. Serve de amarra entre os
  # dois blocos.
  p <- tr_spatial_example("milho_se")
  corte <- .tr_spatial_corte_padrao(p$coords)
  a <- tr_spatial_anisotropy(p, direcoes = "0,90", tolerancia = 90,
                             dist_max = corte, n_classes = 6L, pares_min = 1L)
  v <- tr_spatial_variogram(p, dist_max = corte, n_classes = 6L, pares_min = 1L)
  for (d in c(0, 90)) {
    expect_equal(a$tabela$gamma[a$tabela$direcao == d], v$tabela$gamma,
                 tolerance = 1e-8, info = paste("direção", d))
    expect_equal(a$tabela$np[a$tabela$direcao == d], v$tabela$np,
                 info = paste("direção", d))
  }
})

test_that("direção sem pares bastantes sai da tabela e entra na nota", {
  # Fixture medida em 2026-10-09: em `milho_se`, na tolerância padrão, o máximo
  # de pares por direção é 0=59, 45=83, 90=57, 135=43. Com o mínimo em 58, as
  # duas primeiras sobrevivem e as duas últimas saem — é o caso PARCIAL, que é
  # o que a nota existe para contar.
  p <- tr_spatial_example("milho_se")
  a <- tr_spatial_anisotropy(p, direcoes = "0,45,90,135", pares_min = 58L)
  expect_match(a$nota, "Sem pares bastantes")
  expect_setequal(unique(a$tabela$direcao), c(0, 45))
  expect_match(a$nota, "90")
  expect_match(a$nota, "135")
})

test_that("mínimo de pares alto demais não deixa classe nenhuma: erro nomeado", {
  p <- tr_spatial_example("milho_se")
  expect_error(tr_spatial_anisotropy(p, pares_min = 1e6),
               class = "tr_spatial_error_empty_variogram")
})

test_that("a nota diz o número de direções, a tolerância e as classes", {
  p <- tr_spatial_example("milho_se")
  a <- tr_spatial_anisotropy(p, direcoes = "0,90", tolerancia = 30,
                             n_classes = 8L, pares_min = 1L)
  expect_match(a$nota, "2 direções")
  expect_match(a$nota, "30 graus")
  expect_match(a$nota, "8 classes")
})

test_that("classes de menos e tolerância fora da faixa são recusadas", {
  p <- tr_spatial_example("milho_se")
  expect_error(tr_spatial_anisotropy(p, n_classes = 2L),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_anisotropy(p, tolerancia = 0),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_anisotropy(p, tolerancia = 120),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_anisotropy(p, dist_max = -1),
               class = "tr_spatial_error_bad_option")
})

test_that("objeto que não é de pontos é recusado", {
  expect_error(tr_spatial_anisotropy(list(a = 1)),
               class = "tr_spatial_error_not_points")
})

# ---- o tipo spatial/anisotropy --------------------------------------------------

test_that("o tipo guarda e devolve o objeto de anisotropia", {
  a <- tr_spatial_anisotropy(tr_spatial_example("milho_se"), pares_min = 1L)
  tipo <- spatial_anisotropy_type()
  expect_equal(tipo$id, "spatial/anisotropy")
  p <- tempfile(fileext = ".rds")
  tipo$store(a, p)
  volta <- tipo$restore(p)
  expect_s3_class(volta, "tr_spatial_anisotropy")
  expect_equal(volta$tabela, a$tabela)
  expect_equal(volta$direcoes, a$direcoes)
})

test_that("o store do tipo recusa o que não é anisotropia", {
  tipo <- spatial_anisotropy_type()
  expect_error(tipo$store(list(a = 1), tempfile()),
               class = "tr_spatial_error_not_anisotropy")
  expect_error(tipo$store(tr_spatial_example("milho_se"), tempfile()),
               class = "tr_spatial_error_not_anisotropy")
})

test_that("o adaptador para data/table traz direção, distância, gamma e pares", {
  a <- tr_spatial_anisotropy(tr_spatial_example("milho_se"), pares_min = 1L)
  t <- .tr_spatial_aniso_tabela(a)
  expect_s3_class(t, "data.frame")
  expect_true(all(c("direcao", "u", "gamma", "np") %in% names(t)))
  expect_equal(nrow(t), nrow(a$tabela))
})

test_that("a coleção registra o tipo e o nó spatial/anisotropy", {
  co <- trama_collection()
  expect_true("spatial/anisotropy" %in% vapply(co$types, function(x) x$id, ""))
  expect_true("spatial/anisotropy" %in% vapply(co$nodes, function(x) x$id, ""))
  de <- vapply(co$adapters, function(a) a$from, "")
  para <- vapply(co$adapters, function(a) a$to, "")
  expect_true(any(de == "spatial/anisotropy" & para == "data/table"))
})

test_that("o report da anisotropia sai sem erro e cita as direções", {
  a <- tr_spatial_anisotropy(tr_spatial_example("milho_se"), pares_min = 1L)
  r <- tr_spatial_report_anisotropy(a)
  expect_false(is.null(r))
  expect_match(paste(unlist(r), collapse = " "), "45")
})
