# Logística penalizada de Firth (1993): a verossimilhança ganha a penalidade
# de Jeffreys, ½ log|I(β)|, que tira o viés de ordem 1/n do estimador de máxima
# verossimilhança e, de quebra, dá estimativa FINITA mesmo com separação
# completa (Heinze & Schemper 2002). Os intervalos são os da verossimilhança
# penalizada perfilada, como no artigo e no `logistf`: o de Wald supõe a
# log-verossimilhança quadrática, e sob separação ela está longe disso.
#
# Implementação própria (Newton com a informação de Fisher e passo cortado
# pela metade), conferida contra o `logistf` nos testes. Para a ligação logit,
# o escore modificado de Firth, X'(y − π + h(½ − π)), é o gradiente exato da
# log-verossimilhança penalizada, e zerá-lo com um coeficiente fixo é
# maximizá-la com o coeficiente fixo — o que o perfil pede.

#' Pesos e fatoração QR de √W·X.
#'
#' Tudo passa pela QR, e não por `solve(X'WX)`: com separação quase completa
#' muitos pesos são minúsculos e X'WX fica mal condicionada (recíproca de 1e-19
#' nos `vinhos`), enquanto a QR de √W·X continua estável.
#' @noRd
.tr_multi_firth_qr <- function(X, b) {
  eta <- drop(X %*% b)
  lw <- stats::plogis(eta, log.p = TRUE) + stats::plogis(-eta, log.p = TRUE)
  sw <- exp(lw / 2)
  q <- qr(X * sw)
  list(eta = eta, sw = sw, q = q,
       ld = 2 * sum(log(abs(diag(qr.R(q))))))
}

#' Log-verossimilhança penalizada em β.
#' @noRd
.tr_multi_firth_lpen <- function(X, y, b) {
  f <- .tr_multi_firth_qr(X, b)
  if (f$q$rank < ncol(X)) return(-Inf)
  sum(y * stats::plogis(f$eta, log.p = TRUE) + (1 - y) * stats::plogis(-f$eta, log.p = TRUE)) + f$ld / 2
}

#' Maximiza a verossimilhança penalizada nos coeficientes `livres`.
#'
#' Os não livres ficam no valor de `b` (é o perfil). O passo é o de Newton com
#' a informação de Fisher dos livres, limitado a `passo_max` como no `logistf`
#' (evita o salto do primeiro passo sob separação), e cortado pela metade
#' enquanto a penalizada não sobe.
#' @noRd
.tr_multi_firth_ajuste <- function(X, y, b = rep(0, ncol(X)), livres = rep(TRUE, ncol(X)),
                                   maxit = 200L, tol = 1e-10, passo_max = 5) {
  # Com um coeficiente fixado longe demais (no perfil), as probabilidades
  # saturam e a informação fica numericamente singular: a penalizada ali é
  # -Inf na prática, o que põe o ponto do lado de fora do intervalo.
  falha <- function(e) list(coefficients = b, vcov = NULL, lpen = -Inf, convergiu = FALSE)
  r <- tryCatch(.tr_multi_firth_newton(X, y, b, livres, maxit, tol, passo_max), error = falha)
  # Partida ruim (os livres do ótimo global com um coeficiente fixado longe
  # dele saturam as probabilidades): recomeça os livres do zero.
  if (!r$convergiu && any(livres) && any(b[livres] != 0)) {
    b0 <- b; b0[livres] <- 0
    r0 <- tryCatch(.tr_multi_firth_newton(X, y, b0, livres, maxit, tol, passo_max), error = falha)
    if (r0$convergiu || r0$lpen > r$lpen) r <- r0
  }
  r
}

.tr_multi_firth_newton <- function(X, y, b, livres, maxit, tol, passo_max) {
  lp <- .tr_multi_firth_lpen(X, y, b)
  if (!is.finite(lp)) stop("partida degenerada")
  convergiu <- !any(livres)
  if (!convergiu) for (it in seq_len(maxit)) {
    f <- .tr_multi_firth_qr(X, b)
    pi <- stats::plogis(f$eta)
    h <- rowSums(qr.Q(f$q)^2)
    # Passo de Newton-Fisher nos livres: (X_l'WX_l)⁻¹ X_l'(y − π + h(½ − π)),
    # que é o mínimos quadrados de √W·X_l contra (y − π + h(½ − π))/√W.
    z <- (y - pi + h * (0.5 - pi)) / f$sw
    delta <- rep(0, ncol(X))
    dl <- qr.coef(qr(X[, livres, drop = FALSE] * f$sw), z)
    if (anyNA(dl)) stop("informação singular")
    delta[livres] <- dl
    mx <- max(abs(delta))
    if (mx > passo_max) delta <- delta * passo_max / mx
    novo <- b + delta
    lp_novo <- .tr_multi_firth_lpen(X, y, novo)
    k <- 0L
    while (!(lp_novo >= lp - 1e-12) && k < 40L) {
      delta <- delta / 2; novo <- b + delta
      lp_novo <- .tr_multi_firth_lpen(X, y, novo); k <- k + 1L
    }
    b <- novo; lp <- lp_novo
    if (max(abs(delta)) < tol) { convergiu <- TRUE; break }
  }
  f <- .tr_multi_firth_qr(X, b)
  Ri <- backsolve(qr.R(f$q), diag(ncol(X)))
  v <- tcrossprod(Ri)
  o <- f$q$pivot
  v[o, o] <- v
  list(coefficients = b, vcov = v, lpen = lp, convergiu = convergiu)
}

#' O ajuste de Firth de um classificador binário (objeto guardado no modelo).
#' @noRd
.tr_multi_firth <- function(X, g, no) {
  Xd <- cbind(1, unname(X))
  y <- as.numeric(g == levels(g)[[2]])
  a <- .tr_multi_ajustar(.tr_multi_firth_ajuste(Xd, y), no)
  if (!a$convergiu) {
    .tr_multi_abort("tr_multi_error_fit",
                    "'%s': a logística de Firth não convergiu em 200 iterações.", no)
  }
  structure(c(a, list(X = Xd, y = y)), class = "tr_multi_firth")
}

#' Intervalo da verossimilhança penalizada perfilada de um coeficiente.
#'
#' Os limites são os β_j em que 2[l*(β̂) − max l*(β | β_j)] = χ²₁(confiança)
#' (Heinze & Schemper 2002, sec. 2.2). O perfil é monótono de cada lado de β̂;
#' a busca abre o intervalo a partir de β̂ ± 2·EP até trocar de sinal e fecha
#' com `uniroot`.
#' @noRd
.tr_multi_firth_ic <- function(aj, j, confianca) {
  alvo <- stats::qchisq(confianca, 1)
  b0 <- aj$coefficients
  livres <- seq_along(b0) != j
  ini <- b0
  perfil <- function(v) {
    b <- ini; b[j] <- v
    r <- .tr_multi_firth_ajuste(aj$X, aj$y, b, livres)
    if (!is.finite(r$lpen)) return(1e10)
    2 * (aj$lpen - r$lpen) - alvo
  }
  ep <- sqrt(aj$vcov[j, j])
  lado <- function(s) {
    passo <- 2 * ep
    fora <- b0[j] + s * passo
    k <- 0L
    while (perfil(fora) < 0 && k < 60L) {
      passo <- passo * 2; fora <- b0[j] + s * passo; k <- k + 1L
    }
    stats::uniroot(perfil, sort(c(b0[j], fora)), tol = 1e-12, maxiter = 1000L)$root
  }
  c(lado(-1), lado(1))
}

#' p-valor do teste da razão de verossimilhanças penalizadas de β_j = 0.
#' @noRd
.tr_multi_firth_p <- function(aj, j) {
  b <- aj$coefficients; b[j] <- 0
  r <- .tr_multi_firth_ajuste(aj$X, aj$y, b, seq_along(b) != j)
  stats::pchisq(2 * (aj$lpen - r$lpen), 1, lower.tail = FALSE)
}
