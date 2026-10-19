# O formato do documento, sem projeto: achar regiões, hash, reescrever.

ler <- function(txt) {
  f <- withr::local_tempfile(fileext = ".qmd", .local_envir = parent.frame())
  writeBin(charToRaw(enc2utf8(txt)), f)
  f
}
regs_de <- function(l) trama.documentos:::.td_regioes(l)

test_that("acha regiões com id, chave, porta, hash e atributos, na ordem do texto", {
  l <- c("---", "title: x", "---", "", "Texto.", "",
         '::: {#trama-tab .trama trama-hash="abc" trama-linhas="50"}', "conteudo", ":::", "",
         '::: {#trama-duas--b .trama trama-hash="def" trama-porta="b"}', "outro", ":::")
  r <- regs_de(l)
  expect_length(r, 2L)
  expect_identical(c(r[[1]]$inicio, r[[1]]$fim), c(7L, 9L))
  expect_identical(r[[1]]$chave, "tab")
  expect_identical(r[[1]]$hash, "abc")
  expect_identical(r[[1]]$pares[["trama-linhas"]], "50")
  expect_identical(r[[1]]$conteudo, "conteudo")
  expect_identical(r[[2]]$no, "duas")
  expect_identical(r[[2]]$porta, "b")
})

test_that("div sem .trama ou sem id trama- é da pessoa", {
  l <- c("::: {#trama-x}", "a", ":::", "::: {.trama}", "b", ":::", "::: {#fig-x .trama-nao}", "c", ":::")
  expect_length(regs_de(l), 0L)
})

test_that("região dentro de div da pessoa e div dentro da região", {
  l <- c("::: {.callout-note}", "Nota da pessoa.", "",
         '::: {#trama-t .trama trama-hash="h"}', "::: callout-tip", "gerado", ":::", ":::",
         "", "Mais texto.", ":::")
  r <- regs_de(l)
  expect_length(r, 1L)
  expect_identical(c(r[[1]]$inicio, r[[1]]$fim), c(4L, 8L))
  expect_identical(r[[1]]$conteudo, c("::: callout-tip", "gerado", ":::"))
})

test_that("cerca dentro de bloco de código e no YAML não conta", {
  l <- c("---", "x: |", "  ::: {#trama-y .trama}", "---",
         "```markdown", '::: {#trama-falsa .trama trama-hash="z"}', ":::", "```",
         "~~~~", ":::", "~~~~",
         '::: {#trama-boa .trama trama-hash="h"}', "````", "```", "````", ":::")
  r <- regs_de(l)
  expect_identical(vapply(r, `[[`, "", "chave"), "boa")
  expect_identical(r[[1]]$conteudo, c("````", "```", "````"))
})

test_that("região sem fechamento ou dentro de outra é erro de formato", {
  expect_error(regs_de(c('::: {#trama-a .trama trama-hash="h"}', "x")), class = "tr_documento_erro_formato")
  expect_error(regs_de(c('::: {#trama-a .trama}', '::: {#trama-b .trama}', ":::", ":::")),
               class = "tr_documento_erro_formato")
})

test_that("hash ignora CRLF, espaço no fim e linhas em branco nas pontas; o resto conta", {
  h <- trama.documentos:::.td_hash_conteudo
  base <- h(c("a", "b"))
  expect_identical(h(c("", "a\r", "b   ", "", "")), base)
  expect_false(identical(h(c("a", "", "b")), base))
  expect_false(identical(h(c("a", "B")), base))
  expect_false(identical(h(c(" a", "b")), base))
  expect_identical(nchar(base), 16L)
})

test_that("ler e regravar devolve os mesmos bytes (BOM, CRLF, sem quebra final)", {
  for (txt in c("a\nb\n", "a\r\nb\r\n", "\u{FEFF}---\ntitle: x\n---\nA", "", "\n\n", "só uma linha")) {
    f <- ler(txt)
    d <- trama.documentos:::.td_ler(f)
    expect_identical(trama.documentos:::.td_texto(d), txt, label = encodeString(txt))
  }
})

test_that("linhas de região novas seguem o cabeçalho: id, classe, hash e opções da pessoa", {
  l <- trama.documentos:::.td_regiao_linhas("tab", c("corpo"), list("trama-hash" = "velho", "trama-linhas" = "50"))
  expect_identical(l[[1]], sprintf('::: {#trama-tab .trama trama-hash="%s" trama-linhas="50"}',
                                   trama.documentos:::.td_hash_conteudo("corpo")))
  expect_identical(l[-1], c("corpo", ":::"))
  cr <- trama.documentos:::.td_regiao_linhas("t", "c", cr = TRUE)
  expect_true(all(endsWith(cr, "\r")))
})

test_that("trocar e conferir: fora das regiões não pode mudar", {
  l <- c("Antes.", "", '::: {#trama-a .trama trama-hash="x"}', "velho", ":::", "", "Depois.")
  r <- regs_de(l)
  novas <- trama.documentos:::.td_trocar(l, list(list(regiao = r[[1]], linhas = c("::: {#trama-a .trama}", "novo", "linha", ":::"))))
  expect_identical(novas, c("Antes.", "", "::: {#trama-a .trama}", "novo", "linha", ":::", "", "Depois."))
  expect_true(trama.documentos:::.td_conferir_fora(l, novas))
  mexida <- novas
  mexida[[1]] <- "Antes!"
  expect_error(trama.documentos:::.td_conferir_fora(l, mexida), class = "tr_documento_erro_interno")
  # Sumir com uma região também é mudar o de fora (o marcador some).
  expect_error(trama.documentos:::.td_conferir_fora(l, c("Antes.", "", "", "Depois.")), class = "tr_documento_erro_interno")
})

test_that("onde se pode inserir: nunca no YAML, em região ou em código", {
  l <- c("---", "title: x", "---", "Texto.", "```{r}", "1 + 1", "```",
         '::: {#trama-a .trama trama-hash="x"}', "c", ":::", "Fim.")
  pode <- function(n) trama.documentos:::.td_pode_inserir(l, n)
  expect_false(pode(1)); expect_false(pode(2))
  expect_true(pode(3)); expect_true(pode(4))
  expect_false(pode(5)); expect_false(pode(6))
  expect_true(pode(7))
  expect_false(pode(8)); expect_false(pode(9))
  expect_true(pode(10)); expect_true(pode(11))
  expect_false(pode(12)); expect_false(pode(-1))
})

test_that("inserir põe linha em branco só onde falta", {
  ins <- trama.documentos:::.td_inserir_linhas
  expect_identical(ins(c("a", "b"), "R", 1), c("a", "", "R", "", "b"))
  expect_identical(ins(c("a", "", "b"), "R", 2), c("a", "", "R", "", "b"))
  expect_identical(ins(c("a", ""), "R", 2), c("a", "", "R"))
  expect_identical(ins(character(), "R", 0), "R")
})
