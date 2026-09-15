test_that("todo conjunto carrega como tibble, sem id numérico que vire variável", {
  for (nm in .TR_MULTI_EXEMPLOS) {
    d <- tr_multi_example(nm)
    expect_s3_class(d, "tbl_df")
    expect_false(anyNA(d), info = nm)
    expect_gte(sum(vapply(d, is.numeric, TRUE)), 4L)
  }
  expect_false(is.numeric(tr_multi_example("questionario")$respondente))
  expect_error(tr_multi_example("wine"), class = "tr_multi_error_bad_option")
})

test_that("os de Harman reproduzem EXATAMENTE a correlação publicada", {
  h <- tr_multi_example("harman_fisicas")
  expect_equal(nrow(h), 305L)
  expect_equal(unname(stats::cor(h)), unname(datasets::Harman23.cor$cov), tolerance = 1e-10)
  h <- tr_multi_example("harman_24_testes")
  expect_equal(nrow(h), 145L)
  expect_equal(unname(stats::cor(h)), unname(datasets::Harman74.cor$cov), tolerance = 1e-10)
})

test_that("simulados são reprodutíveis e não mexem na semente do usuário", {
  set.seed(42); antes <- stats::runif(1)
  set.seed(42)
  a <- tr_multi_example("questionario")
  depois <- stats::runif(1)
  expect_equal(antes, depois)
  expect_identical(a, tr_multi_example("questionario"))
  expect_identical(tr_multi_example("vinhos"), tr_multi_example("vinhos"))
})

test_that("questionário: itens de 1 a 5, e a correlação segue os fatores plantados", {
  q <- tr_multi_example("questionario")
  itens <- q[-1]
  expect_true(all(unlist(itens) %in% 1:5))
  r <- stats::cor(itens)
  # Dentro do fator a correlação é alta; o item invertido correlaciona negativo.
  expect_gt(r["ans1", "ans2"], .35)
  expect_lt(r["soc1", "soc5"], -.25)
  expect_lt(abs(r["soc1", "org1"]), .2)
})

test_that("vinhos: três cultivares com as médias separadas", {
  v <- tr_multi_example("vinhos")
  expect_equal(as.vector(table(v$cultivar)), c(59L, 71L, 48L))
  med <- tapply(v$flavonoides, v$cultivar, mean)
  expect_true(med[["A"]] > med[["B"]] && med[["B"]] > med[["C"]])
})

test_that("pima junta treino e teste da MASS, com nomes em português", {
  d <- tr_multi_example("pima")
  expect_equal(nrow(d), nrow(MASS::Pima.tr) + nrow(MASS::Pima.te))
  expect_equal(names(d), c("amostra", "gestacoes", "glicose", "pressao", "pele", "imc",
                           "pedigree", "idade", "diabetes"))
  expect_equal(levels(d$diabetes), c("não", "sim"))
  expect_equal(as.integer(table(d$amostra)), c(nrow(MASS::Pima.te), nrow(MASS::Pima.tr)))
  expect_type(d$amostra, "character")
  expect_equal(sum(d$diabetes == "sim"), sum(MASS::Pima.tr$type == "Yes") + sum(MASS::Pima.te$type == "Yes"))
})
