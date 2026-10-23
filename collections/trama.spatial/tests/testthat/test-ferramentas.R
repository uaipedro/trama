# Atributo `trama_ferramentas`: o bloco registra a ferramenta que de fato fez a
# conta, para o relatório citá-la. Os pontos são dado (e atributo em dado vaza
# para quem o usa): não registram nada; a geometria do sf não conta.

ferramentas <- function(x) attr(x, "trama_ferramentas", exact = TRUE)

test_that("pontos não registram ferramenta", {
  expect_null(ferramentas(tr_spatial_example("milho_pr")))
})

test_that("ajuste do variograma cita o fit.variogram; krigagem, o krige", {
  p <- tr_spatial_example("milho_pr")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p))
  expect_equal(ferramentas(m), "gstat::fit.variogram")
  expect_equal(ferramentas(tr_spatial_kriging(p, m, resolucao = 20L)), "gstat::krige")
})
