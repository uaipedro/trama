# Combinatória dos delineamentos: sortear um quadrado latino entre TODOS os
# de ordem t, e achar um BIB pequeno antes de cair no não reduzido.

# ---- quadrado latino ----------------------------------------------------------
#
# Fisher & Yates (Statistical Tables) e Cochran & Cox (1957, cap. 4): sorteia-se
# um quadrado PADRÃO (primeira linha e primeira coluna em ordem natural) entre
# todos os padrões da ordem t e depois se permutam as t linhas, as t colunas e
# as letras. Todo quadrado latino L vem de exatamente t triplas (padrão,
# permutação de linhas, permutação de colunas) — escolhida a linha de L que
# faz o papel da primeira, o resto é único —, então o resultado é UNIFORME
# entre os t!·(t−1)!·R_t quadrados. Os padrões R_t (1, 1, 1, 4, 56, 9408, OEIS
# A000315) são enumerados uma vez por sessão, até t = 6.
#
# Para t ≥ 7 (R_7 = 16.942.080) a enumeração não cabe: usa-se a cadeia de
# Markov de Jacobson & Matthews (1996), cuja distribuição estacionária é a
# uniforme; com um número finito de passos o sorteio é APROXIMADAMENTE
# uniforme, e a ajuda diz isso.

.tr_exp_cache <- new.env(parent = emptyenv())

#' Todas as permutações de 1..n, uma por linha.
#' @noRd
.tr_exp_permutacoes <- function(n) {
  if (n == 1L) return(matrix(1L, 1L, 1L))
  p <- .tr_exp_permutacoes(n - 1L)
  do.call(rbind, lapply(seq_len(n), function(i) cbind(i, matrix(c(seq_len(n)[-i])[p], nrow(p)))))
}

#' Os quadrados latinos padrão de ordem t (t ≤ 6), um por linha, cada um
#' guardado por linhas (t² números). Enumerados linha a linha: a linha i
#' começa por i e não repete, em nenhuma coluna, o que as linhas acima têm.
#' @noRd
.tr_exp_padroes_latinos <- function(t) {
  chave <- paste0("latino_", t)
  if (!is.null(.tr_exp_cache[[chave]])) return(.tr_exp_cache[[chave]])
  if (t > 6L) stop("enumeração só até t = 6")
  P <- .tr_exp_permutacoes(t)
  por_inicio <- lapply(seq_len(t), function(i) P[P[, 1L] == i, , drop = FALSE])
  achados <- list()
  andar <- function(M) {
    i <- nrow(M) + 1L
    if (i > t) { achados[[length(achados) + 1L]] <<- as.vector(t(M)); return(invisible()) }
    cand <- por_inicio[[i]]
    ok <- rep(TRUE, nrow(cand))
    tc <- t(cand)
    for (j in seq_len(nrow(M))) ok <- ok & colSums(tc == M[j, ]) == 0L
    for (l in which(ok)) andar(rbind(M, cand[l, ]))
  }
  andar(matrix(seq_len(t), 1L))
  out <- do.call(rbind, achados)
  .tr_exp_cache[[chave]] <- out
  out
}

#' Cadeia de Jacobson & Matthews (1996) no cubo de incidência: M[r, c, s] = 1
#' quando a célula (r, c) tem o símbolo s. Um passo a partir de um quadrado
#' próprio escolhe uma célula (r, c, s) com 0 e troca ±1 no "subcubo" 2×2×2;
#' se aparece um −1, o quadrado fica impróprio e os passos seguintes partem
#' da célula −1, escolhendo ao acaso entre as duas linhas, colunas e símbolos
#' com 1, até voltar a um próprio. Conta só os passos próprios.
#' @noRd
.tr_exp_jm_latino <- function(t, passos = t^3) {
  M <- array(0L, c(t, t, t))
  L0 <- outer(seq_len(t) - 1L, seq_len(t) - 1L, "+") %% t + 1L
  M[cbind(rep(seq_len(t), t), rep(seq_len(t), each = t), as.vector(L0))] <- 1L
  improprio <- NULL
  n <- 0L
  um <- function(v) { w <- which(v == 1L); if (length(w) > 1L) w[sample.int(length(w), 1L)] else w }
  while (n < passos || !is.null(improprio)) {
    if (is.null(improprio)) {
      repeat {
        r <- sample.int(t, 1L); c <- sample.int(t, 1L); s <- sample.int(t, 1L)
        if (M[r, c, s] == 0L) break
      }
      r1 <- which(M[, c, s] == 1L); c1 <- which(M[r, , s] == 1L); s1 <- which(M[r, c, ] == 1L)
      n <- n + 1L
    } else {
      r <- improprio[[1]]; c <- improprio[[2]]; s <- improprio[[3]]
      r1 <- um(M[, c, s]); c1 <- um(M[r, , s]); s1 <- um(M[r, c, ])
    }
    M[r, c, s] <- M[r, c, s] + 1L
    M[r, c1, s] <- M[r, c1, s] - 1L; M[r1, c, s] <- M[r1, c, s] - 1L; M[r, c, s1] <- M[r, c, s1] - 1L
    M[r, c1, s1] <- M[r, c1, s1] + 1L; M[r1, c, s1] <- M[r1, c, s1] + 1L; M[r1, c1, s] <- M[r1, c1, s] + 1L
    M[r1, c1, s1] <- M[r1, c1, s1] - 1L
    improprio <- if (M[r1, c1, s1] < 0L) c(r1, c1, s1) else NULL
  }
  idx <- which(M == 1L, arr.ind = TRUE)
  L <- matrix(0L, t, t); L[idx[, 1:2, drop = FALSE]] <- idx[, 3L]
  L
}

#' Um quadrado latino t × t sorteado: uniforme entre todos (t ≤ 6) ou
#' aproximadamente uniforme (t ≥ 7, Jacobson & Matthews). Símbolos 1..t.
#' @noRd
.tr_exp_quadrado_latino <- function(t) {
  if (t <= 6L) {
    P <- .tr_exp_padroes_latinos(t)
    Q <- matrix(P[sample.int(nrow(P), 1L), ], t, t, byrow = TRUE)
  } else {
    Q <- .tr_exp_jm_latino(t)
  }
  letras <- .tr_exp_perm(seq_len(t))
  Q <- Q[.tr_exp_perm(seq_len(t)), .tr_exp_perm(seq_len(t)), drop = FALSE]
  matrix(letras[Q], t, t)
}

# ---- BIB ----------------------------------------------------------------------
#
# Para t tratamentos em blocos de k, as condições necessárias são
# r = λ(t − 1)/(k − 1) e b = rt/k inteiros e b ≥ t (Fisher). Percorre-se λ do
# menor admissível para cima e, em cada um, tenta-se: (1) BIB cíclico com um
# bloco inicial (conjunto de diferenças, b = t); (2) desenvolvimento de 1 ou 2
# blocos iniciais mod t, ou mod (t − 1) com um ponto fixo ∞ (famílias de
# diferenças, Bose 1939); (3) busca exaustiva com orçamento de nós. O primeiro
# que sai é o de menor b que essas construções acham; sem nenhum abaixo de
# C(t, k), fica o não reduzido.

.tr_exp_bib_confere <- function(B, t, lam) {
  if (any(apply(B, 1L, anyDuplicated) > 0L)) return(FALSE)
  N <- matrix(0L, t, nrow(B)); N[cbind(as.vector(B), rep(seq_len(nrow(B)), ncol(B)))] <- 1L
  C <- tcrossprod(N)
  all(C[upper.tri(C)] == lam) && length(unique(diag(C))) == 1L
}

#' Desenvolve blocos iniciais mod m (pontos 0..m−1; ∞ = o ponto m, fixo).
#' Devolve a matriz de blocos em 1..(m + 1).
#' @noRd
.tr_exp_bib_desenvolver <- function(bases, m) {
  do.call(rbind, lapply(bases, function(B) {
    t(vapply(0:(m - 1L), function(i) sort(ifelse(B >= m, B, (B + i) %% m)) + 1L, numeric(length(B))))
  }))
}

#' Quantas vezes cada diferença não nula (1..m−1) sai da parte finita de um
#' bloco inicial.
#' @noRd
.tr_exp_bib_difs <- function(B, m) {
  B <- B[B < m]
  if (length(B) < 2L) return(integer(m - 1L))
  d <- outer(B, B, "-") %% m
  tabulate(d[row(d) != col(d)], m - 1L)
}

#' Família de diferenças: `nb` blocos iniciais mod m cujas diferenças cobrem
#' cada resíduo não nulo λ vezes (Bose 1939). Mod t (b = nb·t), ou mod t − 1
#' com o ponto ∞, que então entra em λ/(k − 1) dos blocos iniciais. O último
#' bloco é achado por tabela de espalhamento (a diferença que falta), não por
#' laço; o resultado é sempre conferido por inteiro.
#' @noRd
.tr_exp_bib_familia <- function(t, k, lam, b) {
  for (inf in c(FALSE, TRUE)) {
    m <- if (inf) t - 1L else t
    if (b %% m != 0L) next
    nb <- b %/% m
    n_inf <- if (inf) lam / (k - 1) else 0
    if (n_inf != round(n_inf) || n_inf > nb || (inf && n_inf < 1)) next
    if (nb > 3L) next
    fin <- utils::combn(m - 1L, k - 1L); fin <- lapply(seq_len(ncol(fin)), function(j) c(0L, fin[, j]))
    com_inf <- if (inf) {
      if (k == 2L) list(c(0L, m)) else lapply(utils::combn(m - 1L, k - 2L, simplify = FALSE), function(x) c(0L, x, m))
    }
    tipos <- c(rep("inf", n_inf), rep("fin", nb - n_inf))
    cand <- list(fin = fin, inf = com_inf)
    D <- lapply(cand, function(cc) if (length(cc)) t(vapply(cc, .tr_exp_bib_difs, integer(m - 1L), m = m)))
    chaves <- lapply(D, function(x) if (!is.null(x)) apply(x, 1L, paste, collapse = ","))
    ult <- tipos[[nb]]
    prefixos <- if (nb == 1L) list(integer()) else {
      g <- do.call(expand.grid, lapply(tipos[-nb], function(tp) seq_along(cand[[tp]])))
      if (nrow(g) > 20000L) next
      lapply(seq_len(nrow(g)), function(i) as.integer(g[i, ]))
    }
    for (pf in prefixos) {
      soma <- integer(m - 1L)
      for (j in seq_along(pf)) soma <- soma + D[[tipos[[j]]]][pf[[j]], ]
      alvo <- lam - soma
      if (any(alvo < 0L)) next
      hit <- match(paste(alvo, collapse = ","), chaves[[ult]])
      if (is.na(hit)) next
      bases <- c(lapply(seq_along(pf), function(j) cand[[tipos[[j]]]][[pf[[j]]]]), list(cand[[ult]][[hit]]))
      X <- .tr_exp_bib_desenvolver(bases, m)
      if (.tr_exp_bib_confere(X, t, lam)) return(X)
    }
  }
  NULL
}

#' BIBs que as construções acima não acham e que existem, conferidos no teste
#' (λ constante). (10, 4, 2), b = 15: blocos obtidos com `crossdes::find.BIB`
#' e conferidos; o complementar dá (10, 6, 5).
#' @noRd
.TR_EXP_BIB_TABELA <- list(
  "10_4" = matrix(c(1, 3, 6, 10, 3, 5, 7, 10, 4, 5, 6, 8, 1, 5, 7, 8, 2, 3, 4, 7, 1, 4, 9, 10, 1, 2, 6, 7,
                    3, 5, 6, 9, 4, 6, 7, 9, 1, 3, 4, 8, 2, 6, 8, 10, 7, 8, 9, 10, 2, 3, 8, 9, 2, 4, 5, 10,
                    1, 2, 5, 9), ncol = 4L, byrow = TRUE))

#' Busca exaustiva: cobre sempre o menor par ainda sem λ encontros, com um
#' bloco que o contenha e não estoure nenhum par nem a repetição r de nenhum
#' ponto. Para ao passar do orçamento de nós.
#' @noRd
.tr_exp_bib_busca <- function(t, k, lam, b, orcamento = 2000L) {
  r <- b * k / t
  C <- matrix(0L, t, t); rep_p <- integer(t); blocos <- list(); nos <- 0L
  andar <- function() {
    nos <<- nos + 1L
    if (nos > orcamento) return(FALSE)
    falta <- which(C < lam & upper.tri(C), arr.ind = TRUE)
    if (!nrow(falta)) return(length(blocos) == b)
    if (length(blocos) >= b) return(FALSE)
    falta <- falta[order(falta[, 1], falta[, 2]), , drop = FALSE]
    i <- falta[1, 1]; j <- falta[1, 2]
    if (rep_p[[i]] >= r || rep_p[[j]] >= r) return(FALSE)
    livres <- setdiff(which(rep_p < r & C[i, ] < lam & C[, i] < lam & C[j, ] < lam & C[, j] < lam), c(i, j))
    livres <- livres[livres > i]
    if (length(livres) < k - 2L) return(FALSE)
    outros <- if (k == 2L) list(integer()) else utils::combn(livres, k - 2L, simplify = FALSE)
    for (o in outros) {
      B <- sort(c(i, j, o))
      pares <- utils::combn(B, 2L)
      if (any(C[t(pares)] >= lam)) next
      C[t(pares)] <<- C[t(pares)] + 1L; rep_p[B] <<- rep_p[B] + 1L
      blocos[[length(blocos) + 1L]] <<- B
      if (andar()) return(TRUE)
      blocos[[length(blocos)]] <<- NULL
      C[t(pares)] <<- C[t(pares)] - 1L; rep_p[B] <<- rep_p[B] - 1L
      if (nos > orcamento) return(FALSE)
    }
    FALSE
  }
  if (andar()) do.call(rbind, blocos) else NULL
}

#' A base de um BIB para `t` tratamentos em blocos de `k`: a matriz b × k de
#' índices (1..t) e como foi obtida. Com k > t/2 constrói o de t − k e toma os
#' complementos (o complementar de um BIB é BIB, com o mesmo b).
#' @noRd
.tr_exp_bib_base <- function(t, k) {
  chave <- sprintf("bib_%d_%d", t, k)
  if (!is.null(.tr_exp_cache[[chave]])) return(.tr_exp_cache[[chave]])
  if (2L * k > t && t - k >= 2L) {
    base <- tryCatch(.tr_exp_bib_base(t, t - k), tr_experiments_error_no_design = function(e) NULL)
    if (!is.null(base) && base$construcao != "não reduzido") {
      X <- t(apply(base$blocos, 1L, function(B) setdiff(seq_len(t), B)))
      out <- list(blocos = X, construcao = paste("complementar de", base$construcao))
      .tr_exp_cache[[chave]] <- out
      return(out)
    }
  }
  total <- choose(t, k)
  out <- NULL
  for (lam in seq_len(ceiling(total * k * (k - 1) / (t * (t - 1))))) {
    r <- lam * (t - 1) / (k - 1); b <- r * t / k
    if (abs(r - round(r)) > 1e-9 || abs(b - round(b)) > 1e-9 || b < t) next
    if (b >= total) break
    b <- as.integer(round(b))
    X <- .TR_EXP_BIB_TABELA[[sprintf("%d_%d", t, k)]]
    como <- "tabela"
    if (is.null(X) || nrow(X) != b) {
      X <- .tr_exp_bib_familia(t, k, lam, b)
      como <- if (b == t) "cíclico" else "família de diferenças"
    }
    if (is.null(X) && b <= 60L) {
      X <- .tr_exp_bib_busca(t, k, lam, b)
      como <- "busca exaustiva"
    }
    if (!is.null(X)) { out <- list(blocos = X, construcao = como); break }
  }
  if (is.null(out)) {
    if (total > 300) {
      .tr_experiments_abort("tr_experiments_error_no_design",
                            "Não achei BIB menor que o não reduzido para t = %d, k = %d, e o não reduzido teria %s blocos. Mude o tamanho do bloco.",
                            t, k, format(total, big.mark = ".", decimal.mark = ","))
    }
    out <- list(blocos = t(utils::combn(t, k)), construcao = "não reduzido")
  }
  storage.mode(out$blocos) <- "integer"
  .tr_exp_cache[[chave]] <- out
  out
}
