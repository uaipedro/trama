test_that("guia de conta: cadastro primeiro, Education no meio, entrar no fim", {
  withr::local_options(trama.github.url = "https://github.com")
  g <- tr_github_guia_conta()
  expect_gt(length(g), 3)
  expect_equal(g[[1]]$link, "https://github.com/signup")
  expect_true(any(vapply(g, function(p) grepl("education.github.com", p$link %||% ""), TRUE)))
  expect_match(g[[length(g)]]$titulo, "entre")
  for (p in g) expect_true(nzchar(p$titulo) && nzchar(p$texto))
})
