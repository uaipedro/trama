# Metadados do catálogo de bases, declarados à mão, conferidos contra o dado
# real quando o pacote está instalado. Todas carregam por data/public.

test_that("metadados das bases da coleção batem com o dado", {
  for (d in .tr_models_bases()) {
    expect_equal(d$node, "data/public", label = d$id)
    if (!nzchar(system.file(package = d$pacote))) next
    x <- do.call(trama.data::tr_public, d$params)
    expect_equal(nrow(x), d$n, label = d$id)
    expect_true((ncol(x) - d$variaveis) %in% 0:1, label = d$id)
  }
})
