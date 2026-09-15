test_that("ids e seeds criados em sequência não colidem", {
  # Criados num laço apertado, a maioria cai no mesmo milissegundo: o que
  # separa um id do outro é só a parte aleatória, e é ela que está em teste.
  ids <- replicate(200, .tr_new_id())
  expect_false(anyDuplicated(ids) > 0)
  seeds <- replicate(200, .tr_new_seed())
  expect_gt(length(unique(seeds)), 190)
})
