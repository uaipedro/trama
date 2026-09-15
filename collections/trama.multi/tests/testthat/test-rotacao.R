# As rotações daqui contra as de referência. A comparação é até o SINAL e a
# ORDEM das colunas: a convenção da coleção (soma positiva, ordem por soma de
# quadrados) é nossa, e nenhuma referência a segue.

cargas_ml <- function(dataset, k) {
  d <- tr_multi_example(dataset)
  d <- d[vapply(d, is.numeric, TRUE)]
  unclass(stats::factanal(covmat = stats::cor(d), factors = k, n.obs = nrow(d),
                          rotation = "none")$loadings)
}

# Casa cada coluna de `a` com a de `b` de maior |correlação|, e compara.
expect_cargas_iguais <- function(a, b, tolerance = 1e-4) {
  a <- unname(as.matrix(a)); b <- unname(as.matrix(b))
  expect_equal(dim(a), dim(b))
  par <- apply(abs(stats::cor(a, b)), 1L, which.max)
  expect_setequal(par, seq_len(ncol(b)))
  expect_equal_ate_sinal(a, b[, par, drop = FALSE], tolerance = tolerance)
  invisible(par)
}

test_that("ortogonais batem com o GPArotation (varimax, quartimax, equamax)", {
  skip_if_not_installed("GPArotation")
  for (caso in list(list("harman_24_testes", 4L), list("questionario", 3L))) {
    A <- cargas_ml(caso[[1]], caso[[2]])
    ref <- list(varimax = GPArotation::Varimax(A, normalize = TRUE, eps = 1e-8),
                quartimax = GPArotation::quartimax(A, normalize = TRUE, eps = 1e-8),
                equamax = GPArotation::equamax(A, normalize = TRUE, eps = 1e-8))
    for (r in names(ref)) {
      x <- .tr_multi_rotacionar(A, r)
      expect_true(x$convergiu, info = r)
      expect_cargas_iguais(x$cargas, unclass(ref[[r]]$loadings))
      expect_equal(x$phi, diag(ncol(A)), ignore_attr = TRUE)
      expect_equal(unname(A %*% x$rotmat), unname(x$cargas), tolerance = 1e-10)
    }
  }
})

test_that("varimax bate com o stats::varimax", {
  # Tolerância 1e-3, e não 1e-4: o `stats::varimax` para quando o critério
  # melhora menos de 1e-5 RELATIVO, um pouco antes do ótimo (o GPA daqui para
  # no gradiente, e bate com o GPArotation a 1e-6). A diferença é de 1e-4.
  A <- cargas_ml("harman_24_testes", 4L)
  expect_cargas_iguais(.tr_multi_rotacionar(A, "varimax")$cargas,
                       unclass(stats::varimax(A)$loadings), tolerance = 1e-3)
  expect_cargas_iguais(.tr_multi_rotacionar(A, "varimax", normalizar = FALSE)$cargas,
                       unclass(stats::varimax(A, normalize = FALSE)$loadings), tolerance = 1e-3)
})

test_that("oblimin bate com o GPArotation, cargas e Φ", {
  skip_if_not_installed("GPArotation")
  for (caso in list(list("harman_24_testes", 4L), list("questionario", 3L))) {
    A <- cargas_ml(caso[[1]], caso[[2]])
    ref <- GPArotation::oblimin(A, normalize = TRUE, eps = 1e-8)
    x <- .tr_multi_rotacionar(A, "oblimin")
    expect_true(x$convergiu)
    par <- expect_cargas_iguais(x$cargas, unclass(ref$loadings))
    s <- sign(colSums(x$cargas * unclass(ref$loadings)[, par]))
    expect_equal(unname(x$phi), unname(diag(s) %*% ref$Phi[par, par] %*% diag(s)), tolerance = 1e-4)
    # O contrato de T: cargas = A t(T⁻¹), Φ = T'T.
    expect_equal(unname(A %*% t(solve(x$rotmat))), unname(x$cargas), tolerance = 1e-10)
    expect_equal(x$phi, crossprod(x$rotmat), tolerance = 1e-10)
  }
})

test_that("promax bate com o stats::promax, e Φ é a do factanal", {
  A <- cargas_ml("harman_24_testes", 4L)
  ref <- stats::promax(A)
  x <- .tr_multi_rotacionar(A, "promax")
  par <- expect_cargas_iguais(x$cargas, unclass(ref$loadings))
  phi_ref <- solve(crossprod(ref$rotmat))
  s <- sign(colSums(x$cargas * unclass(ref$loadings)[, par]))
  expect_equal(unname(x$phi), unname(diag(s) %*% phi_ref[par, par] %*% diag(s)), tolerance = 1e-4)
  expect_equal(unname(A %*% t(solve(x$rotmat))), unname(x$cargas), tolerance = 1e-10)
})

test_that("convenção: soma positiva, ordem por soma de quadrados, nenhuma = identidade", {
  A <- cargas_ml("questionario", 3L)
  for (r in .TR_MULTI_ROTACOES) {
    x <- .tr_multi_rotacionar(A %*% .tr_multi_orientacao(A), r)
    expect_true(all(colSums(x$cargas) > 0), info = r)
    expect_false(is.unsorted(rev(colSums(x$cargas^2))), info = r)
    expect_equal(colnames(x$cargas), c("F1", "F2", "F3"))
  }
  B <- A %*% .tr_multi_orientacao(A)
  expect_equal(.tr_multi_rotacionar(B, "nenhuma")$rotmat, diag(3), ignore_attr = TRUE)
  expect_error(.tr_multi_rotacionar(A, "varimaxx"), class = "tr_multi_error_bad_option")
})
