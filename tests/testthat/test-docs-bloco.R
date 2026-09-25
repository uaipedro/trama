test_that("tr_text escolhe o idioma pedido e cai para pt, depois para o primeiro", {
  expect_equal(tr_text("Normalidade."), "Normalidade.")
  x <- list(pt = "Normalidade.", en = "Normality.")
  expect_equal(tr_text(x, lang = "en"), "Normality.")
  expect_equal(tr_text(x, lang = "es"), "Normalidade.")
  expect_equal(tr_text(list(en = "Only en."), lang = "es"), "Only en.")
  withr::with_options(list(trama.lang = "en"), expect_equal(tr_text(x), "Normality."))
})

test_that("tr_text recusa forma que não é string nem lista nomeada de strings", {
  expect_error(tr_text(NA_character_), class = "tr_error_bad_text")
  expect_error(tr_text(c("a", "b")), class = "tr_error_bad_text")
  expect_error(tr_text(list("a", "b")), class = "tr_error_bad_text")
  expect_error(tr_text(list(pt = 1)), class = "tr_error_bad_text")
  expect_error(tr_text(3), class = "tr_error_bad_text")
})

test_that("tr_pressuposto valida texto, ids de verificação e se_falhar", {
  p <- tr_pressuposto("Resíduos normais.", verificar = "models/shapiro",
                      se_falhar = list(pt = "Transforme.", en = "Transform."))
  expect_s3_class(p, "tr_pressuposto")
  expect_equal(p$verificar, "models/shapiro")
  expect_null(tr_pressuposto("Independência.")$verificar)
  expect_error(tr_pressuposto(NA_character_), class = "tr_error_bad_text")
  expect_error(tr_pressuposto("x", verificar = "sem_barra"), class = "tr_error_bad_id")
  expect_error(tr_pressuposto("x", se_falhar = 1), class = "tr_error_bad_text")
})

test_that("tr_ref exige autores, ano e título fora da implementação", {
  r <- tr_ref("Montgomery, D. C.", 2017, "Design and Analysis of Experiments",
              fonte = "Wiley", doi = "10.1002/abc.123", papel = "livro-texto")
  expect_s3_class(r, "tr_ref")
  expect_equal(r$papel, "livro-texto")
  expect_identical(r$ano, 2017L)
  expect_equal(tr_ref("A", 2000, "T")$papel, "teoria")
  expect_error(tr_ref(ano = 2000, titulo = "T"), class = "tr_error_bad_docs")
  expect_error(tr_ref("A", titulo = "T"), class = "tr_error_bad_docs")
  expect_error(tr_ref("A", 2000), class = "tr_error_bad_docs")
  expect_error(tr_ref("A", 999, "T"), class = "tr_error_bad_docs")
  expect_error(tr_ref("A", 2000.5, "T"), class = "tr_error_bad_docs")
  expect_error(tr_ref("A", 2000, "T", papel = "outro"))
})

test_that("tr_ref de implementação exige pacote e função e dispensa o resto", {
  r <- tr_ref(papel = "implementacao", pacote = "stats", funcao = "lm")
  expect_equal(r$pacote, "stats")
  expect_null(r$autores)
  expect_error(tr_ref(papel = "implementacao", pacote = "stats"), class = "tr_error_bad_docs")
  expect_error(tr_ref(papel = "implementacao", funcao = "lm"), class = "tr_error_bad_docs")
})

test_that("tr_ref valida doi, url e textos i18n", {
  expect_error(tr_ref("A", 2000, "T", doi = "https://doi.org/10.1/x"), class = "tr_error_bad_docs")
  expect_error(tr_ref("A", 2000, "T", url = "http://x.org"), class = "tr_error_bad_docs")
  expect_equal(tr_ref("A", 2000, "T", url = "https://x.org")$url, "https://x.org")
  expect_error(tr_ref("A", 2000, "T", nota = c("a", "b")), class = "tr_error_bad_text")
  n <- tr_ref("A", 2000, "T", nota = list(pt = "Cap. 3.", en = "Ch. 3."))
  expect_equal(tr_text(n$nota, "en"), "Ch. 3.")
})
