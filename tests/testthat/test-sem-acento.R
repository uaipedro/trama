# `iconv(x, to = "ASCII//TRANSLIT")` dependia do sistema: no macOS "Ação" saía
# "ac-ao" e "Regressão" não casava com "regressao" na busca. `tr_sem_acento()`
# tem o mapa explícito; estes testes rodam o mesmo em qualquer plataforma.

test_that("tira o acento do português sem mexer no resto", {
  expect_equal(tr_sem_acento(c("Ação & Reação!", "Regressão linear", "produção")),
               c("Acao & Reacao!", "Regressao linear", "producao"))
  expect_equal(tr_sem_acento("ÁÉÍÓÚ àâêô ãõ ü ç Ç ñ nº 1ª"), "AEIOU aaeo ao u c C n no 1a")
  expect_equal(tr_sem_acento("Soma Básica"), "Soma Basica")
})

test_that("NA, vazio e não texto", {
  expect_identical(tr_sem_acento(character()), character())
  expect_identical(tr_sem_acento(NA_character_), NA_character_)
  expect_identical(tr_sem_acento(""), "")
  expect_identical(tr_sem_acento(1:2), c("1", "2"))
})

test_that("o que não tem mapa sai, e o resultado é só ASCII", {
  r <- tr_sem_acento("café ☕ — fim")
  expect_equal(r, "cafe   fim")
  expect_true(all(utf8ToInt(r) < 128L))
})

test_that("não depende do locale nem da codificação de entrada", {
  latin1 <- iconv("Regressão", "UTF-8", "latin1")
  expect_equal(tr_sem_acento(latin1), "Regressao")
  withr::with_locale(c(LC_CTYPE = "C"), {
    expect_equal(tr_sem_acento("Ação & Reação!"), "Acao & Reacao!")
    expect_equal(.tr_slug("Ação & Reação!"), "acao-reacao")
    expect_equal(.tr_busca_ponto("regressao", "Regressão linear"), 3)
  })
})

test_that("os slugs do núcleo passam por ela", {
  expect_equal(.tr_slug("Soma Básica"), "soma-basica")
  expect_equal(.tr_galeria_slug("Gráfico de Dispersão", "figura"), "grafico-de-dispersao")
  expect_equal(.tr_export_slug("Correlação (Pearson)", "x"), "correlacao_pearson")
})
