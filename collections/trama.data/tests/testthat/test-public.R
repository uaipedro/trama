# data/public: o bloco que o catálogo de bases insere no canvas.

test_that("carrega base de pacote e aplica a regra de rowname do data/example", {
  x <- tr_public("datasets", "mtcars")
  expect_s3_class(x, "tbl_df")
  expect_equal(names(x)[1], "nome")
  expect_equal(dim(x), c(32L, 12L))
  expect_identical(tr_public("datasets", "iris"), tibble::as_tibble(datasets::iris))
})

test_that("pacote ausente, base inexistente e não-tabela são erros classificados", {
  expect_error(tr_public("pacotequenaoexiste", "x"), class = "tr_data_error_missing_package")
  expect_error(tr_public("datasets", "naoexiste"), class = "tr_data_error_bad_option")
  expect_error(tr_public("datasets", "AirPassengers"), class = "tr_data_error_not_a_table")
  expect_error(tr_public("", "mtcars"), class = "tr_data_error_blank_param")
})

# Os metadados do catálogo são declarados à mão (o pacote pode nem estar
# instalado quando o modal abre); aqui eles são conferidos contra o dado real
# sempre que o pacote existir. Uma coluna a mais é a `nome` do rowname.
confere_bases <- function(bases) {
  for (d in bases) {
    if (!nzchar(system.file(package = d$pacote))) next
    x <- do.call(trama::tr_fn(d$node, data_registry()), d$params)
    expect_equal(nrow(x), d$n, label = d$id)
    expect_true((ncol(x) - d$variaveis) %in% 0:1, label = d$id)
  }
}

test_that("metadados das bases da coleção data batem com o dado", {
  confere_bases(.tr_data_bases())
})
