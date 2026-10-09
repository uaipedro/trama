# O ajuste do modelo teórico ao variograma empírico, sobre `gstat::fit.variogram`.
#
# Pontos em que este bloco diverge do padrão do `gstat`, de propósito:
#
# 1. MÉTODO PADRÃO. O `gstat` usa `fit.method = 7` (pesos N_j/h_j^2). O bloco
#    usa `2` (pesos N_j/gamma(h_j)^2), o WLS de Cressie, o da literatura e o do
#    material de aula (`weights = "cressie"` no geoR). Códigos conferidos na
#    documentação do gstat em 2026-10-06.
#    Atenção: o "Cressie" do gstat e o do geoR NÃO dão o mesmo ajuste. Medido no
#    mesmo variograma empírico, o alcance diverge cerca de 25%; ver o teste de
#    oráculo, que por isso compara só a ordem de grandeza.
# 2. ALCANCE PRÁTICO É CAMPO, não conta na hora de mostrar. No esférico é o
#    próprio phi (o modelo ATINGE o patamar em phi); no exponencial é ~3*phi; no
#    gaussiano, ~sqrt(3)*phi. Esconder isso numa fórmula de renderização é
#    pedir que alguém leia phi como alcance.
# 3. O `gstat` NÃO RECUSA ajuste ruim: devolve sill ou alcance negativo calado,
#    marca `singular` num atributo e avisa "No convergence" num warning que um
#    `suppressWarnings` engoliria. Um modelo assim alimenta a krigagem e o mapa
#    sai plausível e errado, então aqui cada ajuste é conferido (sinais, escala
#    do alcance, convergência, singularidade) e o que falha vira erro nomeado.
# 4. VÁRIOS PONTOS DE PARTIDA, DETERMINÍSTICOS. O variograma empírico costuma
#    ser não monótono na origem (poucos pares na 1a classe), e então "pepita =
#    gamma da 1a classe" é um chute ruim. O bloco tenta uma grade fixa de
#    partidas e devolve o ajuste válido de menor critério. Mesma entrada, mesmo
#    modelo, sempre. Valor informado pela pessoa não é substituído pela grade.
# 5. NUNCA REPARAR EM SILÊNCIO. Se o único defeito que sobra é pepita negativa,
#    o erro diz como sair (`pepita_fixa = TRUE`, `pepita_inicial = 0`); refazer
#    o ajuste com pepita zero por conta própria mudaria o modelo sem pedir.

.TR_SPATIAL_FAMILIAS <- c("esferico", "exponencial", "gaussiano", "matern")
.TR_SPATIAL_VGM <- c(esferico = "Sph", exponencial = "Exp", gaussiano = "Gau", matern = "Mat")
.TR_SPATIAL_METODOS <- c(`WLS-Cressie` = 2L, `WLS-np` = 1L, OLS = 6L)
# Escala plausível do alcance PRÁTICO, em múltiplos da maior distância do
# variograma: abaixo da menor distância não há o que ajustar; acima de 10x a
# maior (medido: só os ajustes não identificados, de 13x a 17x) é extrapolação
# sem base. Entre 2x e 10x o ajuste vale, mas a nota avisa (tendência de larga
# escala ainda não removida dá 6x num caso legítimo, então recusar seria errado).
.TR_SPATIAL_PRATICO_MAX <- 10
.TR_SPATIAL_PRATICO_AVISO <- 2

#' O alcance prático: a distância em que o variograma chega a 95% do patamar.
#'
#' Esférico: o próprio phi. O modelo atinge o patamar exatamente em phi, então
#' o critério dos 95% não se aplica: ele só faz sentido para quem nunca chega lá.
#' Exponencial: 3 phi, de `1 - exp(-3) = 0,950`.
#' Gaussiano: sqrt(3) phi, de `1 - exp(-3) = 0,950` com o quadrado no expoente.
#' Matérn: sem forma fechada, resolvido numericamente, na parametrização do
#' `gstat` e do `geoR`: correlação `2^(1-k)/gamma(k) (h/phi)^k K_k(h/phi)`, SEM o
#' `sqrt(2 kappa)` da parametrização de Stein. Conferido contra `variogramLine`
#' em kappa 0,3, 0,5, 1, 2,5 e 10: só em 0,5 as duas fórmulas coincidem.
#' @noRd
.tr_spatial_alcance_pratico <- function(familia, alcance, kappa = 0.5) {
  switch(familia,
    esferico = alcance,
    exponencial = 3 * alcance,
    gaussiano = sqrt(3) * alcance,
    matern = {
      corr <- function(h) {
        u <- h / alcance
        (2^(1 - kappa) / gamma(kappa)) * u^kappa * besselK(u, kappa)
      }
      r <- try(stats::uniroot(function(h) corr(h) - 0.05,
                              interval = c(1e-9 * alcance, 100 * alcance),
                              tol = 1e-12 * alcance)$root,
               silent = TRUE)
      if (inherits(r, "try-error")) NA_real_ else r
    })
}

#' O critério do método, avaliado no modelo ajustado: a soma de quadrados
#' ponderada com os pesos DECLARADOS (N/gamma_modelo^2 no Cressie, N no WLS-np,
#' 1 no OLS). Só se compara entre ajustes do mesmo método e do mesmo variograma.
#' @noRd
.tr_spatial_criterio <- function(t, familia, kappa, pepita, contrib, alcance, metodo) {
  m <- gstat::vgm(psill = contrib, model = unname(.TR_SPATIAL_VGM[[familia]]),
                  range = alcance, nugget = pepita, kappa = kappa)
  g <- gstat::variogramLine(m, dist_vector = t$u)$gamma
  w <- switch(metodo, `WLS-Cressie` = t$np / g^2, `WLS-np` = as.numeric(t$np), OLS = 1)
  sum(w * (t$gamma - g)^2)
}

#' Um ajuste a partir de um ponto de partida. Devolve os números e o que há de
#' errado com eles, sem levantar erro: quem decide é o chamador, com a lista de
#' partidas na mão.
#' @noRd
.tr_spatial_ajustar_um <- function(vg, t, familia, metodo, kappa, p, c, a, fixa) {
  mod <- gstat::vgm(psill = c, model = unname(.TR_SPATIAL_VGM[[familia]]),
                    range = a, nugget = p, kappa = kappa)
  aviso <- character()
  fit <- try(withCallingHandlers(
    gstat::fit.variogram(
      vg, mod, fit.method = unname(.TR_SPATIAL_METODOS[[metodo]]),
      # `fit.sills = FALSE` travaria TAMBÉM a contribuição; o vetor trava só a
      # pepita, que é a primeira componente.
      fit.sills = if (fixa) c(FALSE, TRUE) else TRUE, warn.if.neg = FALSE),
    warning = function(w) {
      aviso <<- c(aviso, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), silent = TRUE)
  if (inherits(fit, "try-error")) {
    return(list(conv = FALSE, sinal = FALSE, motivo = "o gstat falhou"))
  }
  pepita <- fit$psill[[1]]; contrib <- fit$psill[[2]]; alc <- fit$range[[2]]
  nao_conv <- isTRUE(attr(fit, "singular")) || any(grepl("onvergence", aviso))
  tol_p <- 1e-9 * max(t$gamma)
  neg_p <- is.finite(pepita) && pepita < -tol_p
  ruim_c <- !is.finite(contrib) || contrib <= 0
  ruim_a <- !is.finite(alc) || alc <= 0
  # A escala dos dados se confere no ALCANCE PRÁTICO, não em phi. Phi é só um
  # parâmetro: a razão prático/phi vai de 1 (esférico) a ~3 (exponencial) e a
  # mais de 11 (Matérn com kappa 10), então um limite em phi significaria uma
  # distância diferente em cada família, e recusaria Matérn legítimo. É a
  # confusão phi-versus-alcance que o campo `alcance_pratico` existe para evitar.
  # Prático NA (o Matérn não resolveu a raiz) é recusa, não aprovação.
  pr <- if (ruim_a) NA_real_ else .tr_spatial_alcance_pratico(familia, alc, kappa)
  fora <- !ruim_a && (is.na(pr) || pr < min(t$u) || pr > .TR_SPATIAL_PRATICO_MAX * max(t$u))
  list(conv = !nao_conv, pepita = pepita, contrib = contrib, alc = alc,
       so_pepita = neg_p && !ruim_c && !ruim_a && !fora,
       sinal = neg_p || ruim_c || ruim_a || fora || !is.finite(pepita),
       fora = fora,
       motivo = c(if (neg_p || !is.finite(pepita)) "pepita negativa",
                  if (ruim_c) "contribuição não positiva",
                  if (ruim_a) "alcance não positivo",
                  if (fora) sprintf("alcance prático %s fora da escala dos dados",
                                    if (is.na(pr)) "indeterminado" else format(signif(pr, 4)))))
}

#' Ajusta um modelo teórico ao variograma empírico.
#'
#' @param variograma um variograma empírico (`spatial/variogram`).
#' @param familia `"esferico"`, `"exponencial"`, `"gaussiano"` ou `"matern"`.
#' @param metodo `"WLS-Cressie"`, `"WLS-np"` ou `"OLS"`.
#' @param pepita_fixa mantém a pepita no valor inicial em vez de estimá-la.
#' @param pepita_inicial,contribuicao_inicial,alcance_inicial chutes iniciais;
#'   `NA` usa a grade de partidas documentada na ajuda. Valor informado vale
#'   como está e não entra na grade.
#' @param kappa suavidade do Matérn, entre 0,1 e 10.
#' @return um modelo ajustado (`spatial/model`).
#' @export
tr_spatial_variogram_fit <- function(variograma, familia = "esferico",
                                     metodo = "WLS-Cressie", pepita_fixa = FALSE,
                                     pepita_inicial = NA, contribuicao_inicial = NA,
                                     alcance_inicial = NA, kappa = 0.5,
                                     razao = 1, angulo = 0) {
  .tr_spatial_vario_conferir(variograma)
  if (!familia %in% .TR_SPATIAL_FAMILIAS) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Família: escolha um de %s.", paste(.TR_SPATIAL_FAMILIAS, collapse = ", ")))
  }
  if (!metodo %in% names(.TR_SPATIAL_METODOS)) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Método: escolha um de %s.", paste(names(.TR_SPATIAL_METODOS), collapse = ", ")))
  }
  if (!is.numeric(kappa) || length(kappa) != 1L || !is.finite(kappa) ||
      kappa < 0.1 || kappa > 10) {
    .tr_spatial_abort("tr_spatial_error_bad_option", "Kappa: use um número entre 0,1 e 10.")
  }
  # Anisotropia GEOMÉTRICA: a razão é maior eixo / menor eixo, e o ângulo aponta
  # o MAIOR. Aceitar razão < 1 daria duas representações do mesmo modelo, e o
  # card mostraria ângulos diferentes para a mesma anisotropia.
  if (!is.numeric(razao) || length(razao) != 1L || !is.finite(razao) || razao < 1) {
    .tr_spatial_abort("tr_spatial_error_bad_option", paste(
      "Razão de anisotropia: use um número a partir de 1 (1 = isotrópico).",
      "A razão é maior eixo / menor eixo, e o Ângulo aponta o eixo MAIOR —",
      "para pôr o eixo maior na outra direção, gire o Ângulo 90 graus."))
  }
  if (!is.numeric(angulo) || length(angulo) != 1L || !is.finite(angulo) ||
      angulo < 0 || angulo >= 180) {
    .tr_spatial_abort("tr_spatial_error_bad_option", paste(
      "Ângulo de anisotropia: use um número de 0 (inclusive) a 180 (exclusive),",
      "em graus, no sentido horário a partir do Norte."))
  }
  pepita_fixa <- isTRUE(pepita_fixa)
  t <- variograma$tabela
  kap_gstat <- if (identical(familia, "matern")) kappa else 0.5

  # Grade de partidas, tudo derivado da tabela (a distância está na unidade das
  # coordenadas, metros nos exemplos). A grade só preenche o que ficou NA.
  p_heur <- t$gamma[[1]]
  nugs <- if (!is.na(pepita_inicial)) as.numeric(pepita_inicial)
          else if (pepita_fixa) p_heur
          else unique(c(p_heur, 0))
  alcs <- if (!is.na(alcance_inicial)) as.numeric(alcance_inicial)
          else max(t$u) / c(3, 6, 2)
  partidas <- list()
  for (p in nugs) for (a in alcs) {
    c0 <- if (!is.na(contribuicao_inicial)) as.numeric(contribuicao_inicial)
          else max(max(t$gamma) - p, .Machine$double.eps)
    partidas[[length(partidas) + 1L]] <- c(p = p, c = c0, a = a)
  }

  # `np` precisa ser double: com inteiro o gstat falha em C ("REAL() can only
  # be applied to a 'numeric'") e o erro viraria "não convergiu", que mente.
  vg <- data.frame(np = as.numeric(t$np), dist = t$u, gamma = t$gamma,
                   dir.hor = 0, dir.ver = 0, id = factor("var1"))
  class(vg) <- c("gstatVariogram", "data.frame")

  validos <- list(); falhas <- list()
  for (s in partidas) {
    r <- .tr_spatial_ajustar_um(vg, t, familia, metodo, kap_gstat, s[["p"]], s[["c"]],
                                s[["a"]], pepita_fixa)
    if (r$conv && !r$sinal) validos[[length(validos) + 1L]] <- r
    else falhas[[length(falhas) + 1L]] <- r
  }

  if (!length(validos)) {
    sinal <- Filter(function(r) isTRUE(r$sinal) && !is.null(r$alc), falhas)
    if (!length(sinal)) {
      .tr_spatial_abort("tr_spatial_error_no_convergence", paste(
        "O ajuste não convergiu, ou saiu singular, a partir de todos os valores",
        "iniciais tentados. Tente outros chutes de pepita, contribuição e alcance,",
        "outro método ou outra família."))
    }
    # Prefere relatar o defeito que é só a pepita, se algum partida o teve.
    so_p <- Filter(function(r) isTRUE(r$so_pepita), sinal)
    r <- if (length(so_p)) so_p[[1]] else sinal[[1]]
    saida <- sprintf(paste(
      "O ajuste devolveu pepita %.4g, contribuição %.4g e alcance %.4g (distâncias",
      "de %.4g a %.4g): %s, e isso não é um modelo de covariância válido."),
      r$pepita, r$contrib, r$alc, min(t$u), max(t$u), paste(r$motivo, collapse = "; "))
    saida <- paste(saida, if (length(so_p) && !pepita_fixa) paste(
      "O único defeito é a pepita negativa: para ajustar com pepita zero,",
      "marque `pepita_fixa = TRUE` com `pepita_inicial = 0`.")
      else paste("Tente outra família, outro método, ou valores iniciais mais",
                 "próximos do que o gráfico do variograma mostra."))
    .tr_spatial_abort("tr_spatial_error_bad_fit", saida)
  }

  # Entre os ajustes válidos, o de menor critério do método (empate: o primeiro,
  # que é determinístico pela ordem da grade).
  crit <- vapply(validos, function(r) {
    .tr_spatial_criterio(t, familia, kap_gstat, max(r$pepita, 0), r$contrib, r$alc, metodo)
  }, 0)
  k <- which.min(crit)
  r <- validos[[k]]
  pepita <- max(r$pepita, 0); contrib <- r$contrib; alc <- r$alc
  kap <- if (identical(familia, "matern")) kappa else NA_real_
  pratico <- .tr_spatial_alcance_pratico(familia, alc, kap_gstat)
  notas <- character()
  if (pratico > .TR_SPATIAL_PRATICO_AVISO * max(t$u)) {
    notas <- sprintf(paste(
      "O alcance prático (%s) passa de %g vezes a maior distância do variograma (%s):",
      "ele não chega a um patamar dentro das distâncias amostradas, então o alcance",
      "está mal identificado e é uma extrapolação."),
      format(signif(pratico, 4)), .TR_SPATIAL_PRATICO_AVISO, format(signif(max(t$u), 4)))
  }
  if (razao > 1) {
    notas <- c(notas, sprintf(paste(
      "Anisotropia geométrica informada: razão %g, eixo maior a %g graus.",
      "O ajuste não a estima — ela vem do que você leu no bloco de anisotropia."),
      razao, angulo))
  }
  if (nzchar(variograma$nota)) notas <- c(notas, variograma$nota)
  structure(list(
    familia = familia, pepita = pepita, contribuicao = contrib, alcance = alc,
    alcance_pratico = pratico, razao = as.numeric(razao),
    angulo = as.numeric(angulo),
    patamar = pepita + contrib, kappa = kap, metodo = metodo, sqr = crit[[k]],
    # Dependência RELATIVA (Cambardella): pepita/patamar. Menor = dependência
    # espacial mais FORTE. O nome do campo é contrato publicado; o sentido está
    # dito na ajuda do bloco.
    grau_dependencia = pepita / (pepita + contrib),
    variograma = variograma, nota = paste(notas, collapse = " ")),
    class = "tr_spatial_model")
}

#' O objeto `vgm` que a krigagem consome.
#' @noRd
.tr_spatial_vgm_model <- function(x) {
  .tr_spatial_modelo_conferir(x)
  # `anis = c(angulo, 1/razao)`: conferido em 2026-10-09 que o `vgm` guarda
  # ang1 = angulo e anis1 = 1/razao, e que `anis = c(0, 1)` é no-op EXATO
  # (diferença 0 em três direções) — por isso razão 1 preserva o resultado de
  # documento antigo, e não há bump de version nem migração.
  #
  # O ajuste NÃO estima anisotropia: `fit.variogram` não usa `dir.hor` e
  # preserva ang1/anis1 em vez de ajustá-los. Os dois números são do usuário.
  razao <- x$razao %||% 1
  angulo <- x$angulo %||% 0
  gstat::vgm(psill = x$contribuicao, model = unname(.TR_SPATIAL_VGM[[x$familia]]),
             range = x$alcance, nugget = x$pepita,
             kappa = if (is.na(x$kappa)) 0.5 else x$kappa,
             anis = c(angulo, 1 / razao))
}
