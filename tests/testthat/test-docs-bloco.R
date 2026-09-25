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

test_that("tr_node aceita pressupostos e referências e recusa item de outra classe", {
  mk <- function(...) tr_node("t/x", fn = function() 1, description = "X.",
                              outputs = list(out = "t/num"), ...)
  n <- mk(pressupostos = list(tr_pressuposto("P.")), referencias = list(tr_ref("A", 2000, "T")))
  expect_length(n$pressupostos, 1)
  expect_length(n$referencias, 1)
  expect_length(mk()$pressupostos, 0)
  expect_error(mk(pressupostos = list("P.")), class = "tr_error_bad_docs")
  expect_error(mk(referencias = list(tr_pressuposto("P."))), class = "tr_error_bad_docs")
  expect_error(mk(pressupostos = tr_pressuposto("P.")), class = "tr_error_bad_docs")
})

test_that("catálogo publica pressupostos e referências resolvidos no idioma, sem chaves nulas", {
  reg <- tr_registry()
  tr_use(tr_collection("t", types = list(tr_type("t/num")), nodes = list(
    tr_node("t/com", fn = function() 1, description = "Com.", outputs = list(out = "t/num"),
            pressupostos = list(tr_pressuposto(list(pt = "Normal.", en = "Normal (en)."),
                                               verificar = "t/sem", se_falhar = "Transforme.")),
            referencias = list(
              tr_ref("Fisher, R. A.", 1925, "Statistical Methods", doi = "10.1000/x",
                     nota = list(pt = "Cap. 1.", en = "Ch. 1.")),
              tr_ref(papel = "implementacao", pacote = "stats", funcao = "lm"),
              tr_ref(papel = "implementacao", pacote = "pacoteQueNaoExiste", funcao = "f"))),
    tr_node("t/sem", fn = function() 1, description = "Sem.", outputs = list(out = "t/num"))
  )), registry = reg)

  por_id <- function(cat, id) Filter(function(n) n$id == id, cat$nodes)[[1]]
  com <- por_id(tr_catalog(reg), "t/com")
  expect_equal(com$pressupostos[[1]], list(texto = "Normal.", verificar = I("t/sem"),
                                           se_falhar = "Transforme."))
  expect_equal(com$referencias[[1]],
               list(papel = "teoria", autores = I("Fisher, R. A."), ano = 1925L,
                    titulo = "Statistical Methods", doi = "10.1000/x", nota = "Cap. 1."))
  expect_equal(com$referencias[[2]]$versao, as.character(utils::packageVersion("stats")))
  expect_null(com$referencias[[3]]$versao)

  en <- withr::with_options(list(trama.lang = "en"), por_id(tr_catalog(reg), "t/com"))
  expect_equal(en$pressupostos[[1]]$texto, "Normal (en).")
  expect_equal(en$referencias[[1]]$nota, "Ch. 1.")

  js <- jsonlite::fromJSON(tr_catalog_json(reg), simplifyVector = FALSE)
  js_com <- por_id(js, "t/com"); js_sem <- por_id(js, "t/sem")
  expect_equal(js_com$pressupostos[[1]]$verificar, list("t/sem"))
  expect_equal(js_com$referencias[[1]]$autores, list("Fisher, R. A."))
  expect_false(any(c("fonte", "url", "pacote") %in% names(js_com$referencias[[1]])))
  expect_equal(js_sem$pressupostos, list())
  expect_equal(js_sem$referencias, list())
  expect_match(as.character(tr_catalog_json(reg)), '"pressupostos":\\[\\]')
})
