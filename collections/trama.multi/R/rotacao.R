# Rotações de fatores: ortogonais (varimax, quartimax, equamax) e oblíquas
# (promax, oblimin).
#
# Implementadas aqui, e não importadas do `GPArotation`, porque o algoritmo é
# um só para toda a família — projeção do gradiente (Jennrich 2001 na
# ortogonal, 2002 na oblíqua) — e muda apenas o CRITÉRIO, que são três linhas
# cada. O `stats` tem `varimax` e `promax`, mas não quartimax, equamax nem
# oblimin; uma dependência nova para três critérios não se paga. O
# `GPArotation` continua sendo a referência: os testes de paridade conferem as
# cargas daqui contra as dele.
#
# A convenção de T é a do `GPArotation` e está no contrato de `multi/fa`
# (`R/type.R`): na ortogonal `cargas = A %*% T`; na oblíqua
# `cargas = A %*% t(solve(T))`, com `phi = t(T) %*% T`. Com ela, uma mesma
# fórmula de Φ serve às duas famílias (na ortogonal T é ortonormal e Φ = I).

.TR_MULTI_ROTACOES <- c("nenhuma", "varimax", "quartimax", "equamax", "promax", "oblimin")
.TR_MULTI_OBLIQUAS <- c("promax", "oblimin")

#' Critério orthomax e o gradiente dele nas cargas.
#'
#' `f = -1/4 Σ_j [Σ_i λ⁴ − γ/p (Σ_i λ²)²]`, minimizado. γ = 0 é o quartimax
#' (simplifica as LINHAS: cada variável num fator só), γ = 1 o varimax
#' (simplifica as COLUNAS: cada fator com poucas cargas grandes) e γ = k/2 o
#' equamax, o meio-termo. É o mesmo `f` do `vgQ.varimax`/`vgQ.quartimax` do
#' `GPArotation`, para que os critérios comparem número a número.
#' @noRd
.tr_multi_orthomax <- function(gama) {
  function(L) {
    L2 <- L^2
    QL <- L2 - gama * matrix(colMeans(L2), nrow(L), ncol(L), byrow = TRUE)
    list(f = -sum(L2 * QL) / 4, Gq = -L * QL)
  }
}

#' Quartimin (oblimin com γ = 0): penaliza a mesma variável carregar em dois
#' fatores ao mesmo tempo, `f = 1/4 Σ_i Σ_{j≠l} λ²_ij λ²_il`.
#'
#' γ = 0 é o default de toda referência (SPSS, `psych`, `GPArotation`) e o
#' único exposto: os outros γ trocam fatores mais correlacionados por mais
#' simples, um botão que confunde mais do que ensina.
#' @noRd
.tr_multi_quartimin <- function(L) {
  X <- L^2 %*% (1 - diag(ncol(L)))
  list(f = sum(L^2 * X) / 4, Gq = L * X)
}

#' GPA ortogonal (Jennrich 2001): desce o gradiente projetado no espaço das
#' matrizes ortonormais, com passo que dobra a cada iteração e cai à metade
#' até o critério melhorar. A projeção de volta é a da SVD (`u v'`).
#'
#' Começa da identidade, e não de partidas aleatórias: o resultado é o mesmo em
#' todo computador e toda vez, sem semente. Em cargas bem estruturadas a
#' identidade cai no ótimo global; o risco de mínimo local existe, e é o mesmo
#' do `GPArotation` com a partida default.
#' @noRd
.tr_multi_gpa_ortogonal <- function(A, criterio, eps, maxit) {
  Tm <- diag(ncol(A))
  vq <- criterio(A)
  f <- vq$f
  G <- crossprod(A, vq$Gq)
  alfa <- 1
  s <- Inf
  for (it in 0:maxit) {
    M <- crossprod(Tm, G)
    Gp <- G - Tm %*% ((M + t(M)) / 2)
    s <- sqrt(sum(Gp^2))
    if (s < eps) break
    alfa <- 2 * alfa
    for (i in 0:10) {
      X <- Tm - alfa * Gp
      udv <- svd(X)
      Tt <- udv$u %*% t(udv$v)
      vqt <- criterio(A %*% Tt)
      if (vqt$f < f - .5 * s^2 * alfa) break
      alfa <- alfa / 2
    }
    Tm <- Tt
    f <- vqt$f
    G <- crossprod(A, vqt$Gq)
  }
  list(T = Tm, convergiu = s < eps)
}

#' GPA oblíqua (Jennrich 2002): as colunas de T têm norma 1 (os fatores têm
#' variância 1) mas não precisam ser ortogonais; a projeção é normalizar as
#' colunas. As cargas de padrão são `A %*% t(solve(T))`.
#' @noRd
.tr_multi_gpa_obliqua <- function(A, criterio, eps, maxit) {
  Tm <- diag(ncol(A))
  Ti <- solve(Tm)
  L <- A %*% t(Ti)
  vq <- criterio(L)
  f <- vq$f
  G <- -t(t(L) %*% vq$Gq %*% Ti)
  alfa <- 1
  s <- Inf
  for (it in 0:maxit) {
    Gp <- G - Tm %*% diag(colSums(Tm * G), ncol(Tm))
    s <- sqrt(sum(Gp^2))
    if (s < eps) break
    alfa <- 2 * alfa
    for (i in 0:10) {
      X <- Tm - alfa * Gp
      Tt <- X %*% diag(1 / sqrt(colSums(X^2)), ncol(X))
      Tti <- solve(Tt)
      L <- A %*% t(Tti)
      vqt <- criterio(L)
      if (vqt$f < f - .5 * s^2 * alfa) break
      alfa <- alfa / 2
    }
    Tm <- Tt
    Ti <- Tti
    f <- vqt$f
    G <- -t(t(L) %*% vqt$Gq %*% Ti)
  }
  list(T = Tm, convergiu = s < eps)
}

#' Promax (Hendrickson e White 1964), com potência 4, como o `stats::promax`.
#'
#' Parte do varimax e busca a transformação oblíqua que mais aproxima as cargas
#' de um ALVO: as próprias cargas varimax elevadas à quarta potência (com o
#' sinal). Elevar encolhe as cargas pequenas muito mais que as grandes, e o
#' alvo é a estrutura simples "exagerada" que a oblíqua tenta alcançar.
#'
#' É o `stats::promax` passo a passo, e não uma chamada a ele, por um detalhe:
#' ele roda o varimax SEMPRE normalizado, e aqui a normalização é um param. Com
#' `normalizar = TRUE` (o default) o resultado é o dele.
#' @noRd
.tr_multi_promax <- function(A, An, eps, maxit, m = 4) {
  # O varimax roda nas cargas NORMALIZADAS, mas o alvo e a regressão, nas
  # cargas de verdade: é a ordem do `stats::promax` (o `varimax` dele devolve
  # as cargas já desnormalizadas). Regredir nas normalizadas daria outra Φ.
  v <- .tr_multi_gpa_ortogonal(An, .tr_multi_orthomax(1), eps, maxit)
  X <- A %*% v$T
  Q <- X * abs(X)^(m - 1)
  U <- stats::lm.fit(X, Q)$coefficients
  d <- diag(solve(crossprod(U)))
  U <- U %*% diag(sqrt(d), ncol(U))
  # Cargas = A %*% (T_varimax %*% U). Na convenção daqui, cargas = A t(T⁻¹):
  # logo T = t((T_varimax U)⁻¹), e Φ = T'T = (U'U)⁻¹ na base varimax — a mesma
  # Φ que o `factanal` calcula para o promax (`tmat %*% t(tmat)` com
  # `tmat = solve(rotmat)`).
  list(T = t(solve(v$T %*% U)), convergiu = v$convergiu)
}

#' A matriz de sinais e permutação que põe cargas na convenção da coleção:
#' soma de cada coluna positiva, colunas em ordem decrescente de soma de
#' quadrados. `L %*% P` são as cargas orientadas.
#'
#' Serve também às cargas NÃO rotacionadas (`R/fatorial.R` as orienta antes de
#' rotacionar): assim, com `rotacao = "nenhuma"`, `P` sai identidade e T também,
#' como o contrato de `multi/fa` promete.
#' @noRd
.tr_multi_orientacao <- function(L) {
  k <- ncol(L)
  sinais <- ifelse(colSums(L) < 0, -1, 1)
  ordem <- order(-colSums(L^2))
  diag(sinais, k)[, ordem, drop = FALSE]
}

#' Rotaciona uma matriz de cargas.
#'
#' Devolve `list(cargas, rotmat, phi, convergiu)`, na convenção de T descrita
#' no topo do arquivo e no contrato de `multi/fa`.
#'
#' **Normalização de Kaiser**: cada linha é dividida pela raiz da comunalidade
#' antes e multiplicada de volta depois. Sem ela, as variáveis de comunalidade
#' alta dominam o critério, e a rotação serve a elas; com ela, toda variável
#' pesa igual. É o default de `stats::varimax`, do SPSS e do `factanal`.
#'
#' **Sinal e ordem**: o sinal de um fator é arbitrário (o mesmo fator com
#' todas as cargas trocadas é a mesma solução) e a ordem que a rotação devolve
#' também. Sem uma convenção, o F2 de hoje seria o −F3 de amanhã num outro
#' computador. Aqui cada fator é virado para que a SOMA das cargas da coluna
#' seja positiva, e os fatores saem em ordem decrescente de soma de quadrados
#' das cargas (de padrão, na oblíqua). T e Φ são ajustados junto: virar e
#' permutar é multiplicar T à direita por uma matriz de sinais e permutação
#' `P`, e então `cargas' = cargas P`, `T' = T P` e `Φ' = P' Φ P` nas duas
#' famílias.
#' @noRd
.tr_multi_rotacionar <- function(A, rotacao, normalizar = TRUE, eps = 1e-5, maxit = 1000L) {
  rotacao <- .tr_multi_enum(rotacao, .TR_MULTI_ROTACOES, "rotacao")
  A <- as.matrix(A)
  k <- ncol(A)
  obliqua <- rotacao %in% .TR_MULTI_OBLIQUAS
  # Um fator só não tem o que rotacionar: toda "rotação" de uma coluna é ela
  # mesma, a menos do sinal.
  if (rotacao == "nenhuma" || k < 2L) {
    res <- list(T = diag(k), convergiu = TRUE)
    obliqua <- FALSE
  } else {
    w <- if (isTRUE(normalizar)) sqrt(rowSums(A^2)) else rep(1, nrow(A))
    # Uma linha toda zero (variável sem comunalidade) dividiria por zero; ela
    # não pesa no critério de qualquer forma.
    w[w == 0] <- 1
    An <- A / w
    res <- switch(rotacao,
      varimax = .tr_multi_gpa_ortogonal(An, .tr_multi_orthomax(1), eps, maxit),
      quartimax = .tr_multi_gpa_ortogonal(An, .tr_multi_orthomax(0), eps, maxit),
      equamax = .tr_multi_gpa_ortogonal(An, .tr_multi_orthomax(k / 2), eps, maxit),
      oblimin = .tr_multi_gpa_obliqua(An, .tr_multi_quartimin, eps, maxit),
      promax = .tr_multi_promax(A, An, eps, maxit))
  }
  Tm <- res$T
  L <- if (obliqua) A %*% t(solve(Tm)) else A %*% Tm
  phi <- if (obliqua) crossprod(Tm) else diag(k)

  P <- .tr_multi_orientacao(L)
  L <- L %*% P
  Tm <- Tm %*% P
  phi <- t(P) %*% phi %*% P
  if (!obliqua) phi <- diag(k)

  nomes <- paste0("F", seq_len(k))
  dimnames(L) <- list(rownames(A), nomes)
  dimnames(Tm) <- dimnames(phi) <- list(nomes, nomes)
  list(cargas = L, rotmat = Tm, phi = phi, convergiu = isTRUE(res$convergiu))
}
