test_that("compatibilidade é igualdade ou adaptador registrado", {
  reg <- test_registry()
  expect_true(tr_compatible("t/num", "t/num", reg))
  expect_true(tr_compatible("t/num", "t/txt", reg))   # adaptador declarado
  expect_false(tr_compatible("t/txt", "t/num", reg))  # não é simétrico
  expect_null(tr_adapter_for("t/txt", "t/num", reg))
  expect_equal(tr_adapter_for("t/num", "t/txt", reg)$fn(1), "1")
})

test_that("nível 1: a função do nó é chamável direto", {
  reg <- test_registry()
  expect_equal(tr_fn("t/add", reg)(1, 2, 3), 6)
})
