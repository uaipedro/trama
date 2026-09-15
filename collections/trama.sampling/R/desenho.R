# O objeto amostra e o motor de estimação: UM estimador de variância para todo
# desenho.
#
# O estimador é o do conglomerado último (ultimate cluster), com linearização
# de Taylor para média, proporção e razão:
#
#   V(t) = Σ_h (1 − f_h) · n_h/(n_h − 1) · Σ_c (Z_hc − Z̄_h)²
#
# em que Z_hc é a soma, na unidade primária c do estrato h, da variável
# linearizada z (para o total, z = w·y). É o que o `survey::svydesign` faz por
# padrão com `ids`, `strata` e `fpc`, e o que os livros dão em forma fechada
# para cada desenho — a AAS, a estratificada e o conglomerado em um estágio
# saem dele como casos particulares, e os testes conferem cada um contra a
# fórmula do livro.
#
# Um motor só, e não uma fórmula por desenho, porque é o que deixa as
# combinações funcionarem sem código novo: domínio dentro de estratificada,
# razão em conglomerados, pós-estratificação de uma PPS.

.TR_SAMPLING_CAMPOS_AMOSTRA <- c("dados", "desenho", "rotulo", "metodo", "estrato_col",
                                 "conglomerado_col", "N", "n", "receita", "populacao", "pos", "nota")

#' Monta o objeto amostra.
#'
#' - `dados`: as linhas sorteadas, com `peso_amostral` e `prob_inclusao`
#'   acrescentadas (sobrescritas, se a população já as tinha).
#' - `desenho`: `estrato` e `psu`, um valor por linha, e `fpc`, o número de
#'   unidades primárias na POPULAÇÃO por estrato (`NA` = com reposição, sem
#'   correção). Ficam fora de `dados` de propósito: são do desenho, e o
#'   adaptador não deve despejar colunas internas na tabela de ninguém.
#' - `receita`: como sortear de novo (`selecao` e os passos de `pos`); `NULL`
#'   na amostra declarada. `populacao`: o cadastro de onde saiu.
#' - `pos`: a calibração aplicada (`colunas`), ou `NULL`.
#' @noRd
.tr_sampling_amostra <- function(dados, pesos, rotulo, metodo, estrato = NULL, psu = NULL, fpc = NULL,
                                 estrato_col = NULL, conglomerado_col = NULL, N = NA_real_,
                                 receita = NULL, populacao = NULL, nota = "") {
  dados <- tibble::as_tibble(dados)
  n <- nrow(dados)
  estrato <- if (is.null(estrato)) rep(".", n) else as.character(estrato)
  psu <- if (is.null(psu)) as.character(seq_len(n)) else as.character(psu)
  if (is.null(fpc)) fpc <- stats::setNames(NA_real_, ".")
  dados$peso_amostral <- as.numeric(pesos)
  dados$prob_inclusao <- 1 / as.numeric(pesos)
  structure(list(
    dados = dados, desenho = list(estrato = estrato, psu = psu, fpc = fpc),
    rotulo = rotulo, metodo = metodo, estrato_col = estrato_col, conglomerado_col = conglomerado_col,
    N = as.numeric(N), n = n, receita = receita, populacao = populacao, pos = NULL, nota = nota
  ), class = "tr_sampling_sample")
}

#' A variância do total de `z` pelo conglomerado último, e os gl do desenho.
#'
#' Estrato com a unidade primária toda sorteada (f = 1) não contribui: é
#' censo. Estrato com UMA unidade primária e f < 1 é erro, e não zero: o `NA`
#' que o R daria vira "variância 0" num relatório, e a estimativa sai com uma
#' precisão que não tem.
#' @noRd
.tr_sampling_var_total <- function(amostra, z) {
  des <- amostra$desenho
  if (!is.null(amostra$pos)) z <- .tr_sampling_residuo_pos(amostra, z)
  chave <- paste(des$estrato, des$psu, sep = "\u001f")
  Zc <- rowsum(z, chave, reorder = FALSE)[, 1]
  hc <- des$estrato[match(names(Zc), chave)]
  v <- 0
  for (h in unique(hc)) {
    Zh <- Zc[hc == h]
    nh <- length(Zh)
    Nh <- if (h %in% names(des$fpc)) des$fpc[[h]] else NA_real_
    f <- if (is.na(Nh)) 0 else nh / Nh
    if (f >= 1) next
    if (nh < 2L) {
      .tr_sampling_abort("tr_sampling_error_lonely_psu",
                         "O estrato '%s' tem uma unidade primária só, e a variância dele não existe. Junte-o a um estrato vizinho, ou sorteie pelo menos duas por estrato.",
                         h)
    }
    v <- v + (1 - f) * nh / (nh - 1) * sum((Zh - mean(Zh))^2)
  }
  list(v = v, gl = length(Zc) - length(unique(hc)))
}

#' Calibração (pós-estratificação ou raking): a variância usa o resíduo de z
#' na regressão ponderada nas variáveis de calibração.
#'
#' Para o total de y pós-estratificado, o resíduo é w·(y − ȳ_g): o que o
#' pós-estrato já explica deixa de contar como incerteza, que é todo o ganho de
#' calibrar. Com várias variáveis (raking) a regressão é nas indicadoras de
#' todas, que é a linearização do estimador calibrado (Deville & Särndal 1992).
#' @noRd
.tr_sampling_residuo_pos <- function(amostra, z) {
  d <- amostra$dados
  w <- as.numeric(d$peso_amostral)
  fatores <- lapply(amostra$pos$colunas, function(g) factor(as.character(d[[g]])))
  X <- do.call(cbind, lapply(fatores, function(f) stats::model.matrix(~ f - 1)))
  u <- as.numeric(z) / w
  r <- stats::lm.wfit(X, u, w)$residuals
  w * r
}

#' Estima média, total, proporção (média de indicador) ou razão, por domínio.
#'
#' O domínio NÃO corta a amostra: fora dele z = 0, e as unidades primárias sem
#' ninguém do domínio continuam no cálculo. Cortar antes (um `data/filter` e
#' depois estimar) subestima a variância, porque finge que o tamanho do domínio
#' na amostra era fixo — e é o erro mais comum com dado de pesquisa.
#'
#' Faltante em `y` (ou `x`) é tratado do mesmo jeito: fica fora do domínio, e a
#' nota conta quantos.
#' @return tibble com uma linha por domínio.
#' @noRd
.tr_sampling_estimar <- function(amostra, y, tipo, conf, x = NULL, dominio = NULL) {
  w <- amostra$dados$peso_amostral
  ok <- !is.na(y)
  if (!is.null(x)) ok <- ok & !is.na(x)
  y0 <- ifelse(ok, y, 0)
  x0 <- if (is.null(x)) NULL else ifelse(ok, x, 0)
  niveis <- if (is.null(dominio)) "." else sort(unique(as.character(dominio[!is.na(dominio)])))
  sem_fpc <- all(is.na(amostra$desenho$fpc))
  linhas <- lapply(niveis, function(d) {
    I <- ok & (if (is.null(dominio)) TRUE else (!is.na(dominio) & as.character(dominio) == d))
    nd <- sum(I)
    if (nd < 2L) {
      .tr_sampling_abort("tr_sampling_error_too_few",
                         "O domínio '%s' tem %d observação(ões) na amostra, e estimar a variância pede pelo menos 2.",
                         d, nd)
    }
    wI <- w * I
    Nd <- sum(wI)
    if (tipo == "total") {
      est <- sum(wI * y0); z <- wI * y0
    } else if (tipo == "razao") {
      X <- sum(wI * x0)
      if (X == 0) {
        .tr_sampling_abort("tr_sampling_error_bad_size",
                           "O denominador da razão soma zero no domínio '%s'.", d)
      }
      est <- sum(wI * y0) / X; z <- wI * (y0 - est * x0) / X
    } else {
      est <- sum(wI * y0) / Nd; z <- wI * (y0 - est) / Nd
    }
    vr <- .tr_sampling_var_total(amostra, z)
    ep <- sqrt(vr$v)
    if (vr$gl < 1L) {
      .tr_sampling_abort("tr_sampling_error_too_few",
                         "O desenho não deixa grau de liberdade para a variância (unidades primárias = estratos).")
    }
    q <- stats::qt(1 - (1 - conf) / 2, vr$gl)
    # deff contra a AAS do mesmo n: S² pelos pesos, com a correção finita só
    # se o desenho tem uma. Razão fica sem: a AAS dela depende de x também, e
    # um número a mais que ninguém sabe ler é pior que nenhum.
    deff <- NA_real_
    if (tipo != "razao") {
      ybar <- sum(wI * y0) / Nd
      S2 <- nd / (nd - 1) * sum(wI * (y0 - ybar)^2) / Nd
      f_srs <- if (sem_fpc) 0 else min(1, nd / Nd)
      v_srs <- (1 - f_srs) * S2 / nd
      if (tipo == "total") v_srs <- Nd^2 * v_srs
      deff <- if (v_srs > 0) vr$v / v_srs else NA_real_
    }
    tibble::tibble(dominio = d, estimativa = est, erro_padrao = ep, li = est - q * ep, ls = est + q * ep,
                   margem = q * ep, cv_pct = if (est != 0) 100 * ep / abs(est) else NA_real_,
                   deff = deff, n = nd, gl = vr$gl)
  })
  t <- do.call(rbind, linhas)
  attr(t, "faltantes") <- sum(!ok)
  t
}

#' Confere que a entrada é amostra da coleção.
#' @noRd
.tr_sampling_amostra_conferir <- function(x) {
  .tr_sampling_guard(x, "tr_sampling_sample", .TR_SAMPLING_CAMPOS_AMOSTRA,
                     "tr_sampling_error_not_a_sample", "uma amostra")
}

#' O guard comum: classe e campos obrigatórios.
#' @noRd
.tr_sampling_guard <- function(x, classe, campos, erro, oque) {
  falta <- if (is.list(x)) setdiff(campos, names(x)) else campos
  if (!inherits(x, classe) || length(falta)) {
    .tr_sampling_abort(erro, "O nó produziu um objeto '%s', não %s%s.", class(x)[[1]], oque,
                       if (inherits(x, classe) && length(falta))
                         sprintf(" (faltam: %s)", paste(falta, collapse = ", ")) else "")
  }
  invisible(x)
}

# ---- declarar e pós-estratificar ----------------------------------------------

#' Declara o desenho de uma amostra já coletada.
#'
#' É a porta de entrada da base de uma pesquisa oficial: ela já vem com o
#' estrato, a unidade primária e o peso, e o que falta é dizer ao trama quais
#' colunas são essas. Sem peso, o peso sai da população do estrato (N_h/n_h),
#' que só vale sem conglomerado: com conglomerado o peso depende do segundo
#' estágio, que a tabela não conta.
#' @param dados a tabela coletada.
#' @param pesos coluna do peso amostral.
#' @param estrato coluna do estrato (em branco: sem estratos).
#' @param conglomerado coluna da unidade primária (em branco: cada linha é uma).
#' @param populacao coluna com o número de unidades primárias do estrato na
#'   população (em branco: variância com reposição, sem correção finita).
#' @return uma amostra (`sampling/sample`).
#' @export
tr_sampling_design <- function(dados, pesos = "", estrato = "", conglomerado = "", populacao = "") {
  d <- tibble::as_tibble(dados)
  if (nrow(d) < 2L) {
    .tr_sampling_abort("tr_sampling_error_too_few", "A tabela tem %d linha(s); uma amostra pede pelo menos 2.", nrow(d))
  }
  col_p <- .tr_sampling_col_opcional(d, pesos, "pesos")
  col_h <- .tr_sampling_col_opcional(d, estrato, "estrato")
  col_c <- .tr_sampling_col_opcional(d, conglomerado, "conglomerado")
  col_N <- .tr_sampling_col_opcional(d, populacao, "populacao")
  h <- if (is.null(col_h)) rep(".", nrow(d)) else as.character(d[[col_h]])
  if (anyNA(h)) {
    .tr_sampling_abort("tr_sampling_error_bad_option", "Param 'estrato': a coluna '%s' tem faltante.", col_h)
  }
  psu <- if (is.null(col_c)) as.character(seq_len(nrow(d))) else as.character(d[[col_c]])
  fpc <- NULL
  if (!is.null(col_N)) {
    .tr_sampling_numerica(d, col_N, "populacao")
    por_h <- tapply(d[[col_N]], h, function(v) unique(v))
    if (any(lengths(por_h) != 1L)) {
      .tr_sampling_abort("tr_sampling_error_bad_option",
                         "Param 'populacao': a coluna '%s' tem de ser constante dentro de cada estrato.", col_N)
    }
    fpc <- unlist(por_h)
    upas <- tapply(psu, h, function(v) length(unique(v)))[names(fpc)]
    if (anyNA(fpc) || any(fpc < upas)) {
      .tr_sampling_abort("tr_sampling_error_bad_size",
                         "Param 'populacao': em algum estrato a população ('%s') é faltante ou menor que as unidades primárias da amostra.",
                         col_N)
    }
  }
  if (!is.null(col_p)) {
    .tr_sampling_numerica(d, col_p, "pesos")
    w <- d[[col_p]]
    if (anyNA(w) || any(w <= 0)) {
      .tr_sampling_abort("tr_sampling_error_bad_size", "Param 'pesos': a coluna '%s' tem peso faltante, zero ou negativo.", col_p)
    }
  } else if (!is.null(col_N) && is.null(col_c)) {
    nh <- table(h)
    w <- as.numeric(fpc[h] / nh[h])
  } else {
    .tr_sampling_abort("tr_sampling_error_no_weights",
                       "A amostra declarada precisa de 'pesos'%s. Sem peso, o total estimado seria o total da AMOSTRA.",
                       if (is.null(col_c)) " ou da 'populacao' do estrato" else " (com conglomerado, o peso não sai só da população)")
  }
  partes <- c(if (!is.null(col_h)) sprintf("estratos em %s", col_h),
              if (!is.null(col_c)) sprintf("conglomerados em %s", col_c))
  rotulo <- paste(c("Declarada", partes), collapse = " · ")
  .tr_sampling_amostra(d, w, rotulo, "declarada", estrato = h, psu = psu, fpc = fpc,
                       estrato_col = col_h, conglomerado_col = col_c, N = sum(w),
                       nota = if (is.null(col_N)) "sem população do estrato: variância com reposição (conservadora)" else "")
}

#' Pós-estratifica: ajusta os pesos para que somem os totais conhecidos.
#'
#' A amostra aleatória simples que por azar veio com Sul demais estima o total
#' com Sul demais. Se o cadastro diz quantas fazendas há em cada região, o peso
#' de cada fazenda é esticado ou encolhido para que as regiões somem o que
#' somam de verdade — e a variância cai junto, porque a parte do erro que era
#' "quantas de cada região vieram" deixa de existir.
#' @param amostra uma amostra (`sampling/sample`).
#' @param totais tabela com uma linha por pós-estrato.
#' @param pos_estrato coluna do pós-estrato, com o MESMO nome nas duas tabelas.
#' @param coluna_total coluna de `totais` com o número de unidades na população.
#' @return a amostra com os pesos ajustados.
#' @export
tr_sampling_poststratify <- function(amostra, totais, pos_estrato = "", coluna_total = "N") {
  .tr_sampling_amostra_conferir(amostra)
  g <- .tr_sampling_col(amostra$dados, pos_estrato, "pos_estrato")
  tt <- tibble::as_tibble(totais)
  .tr_sampling_col(tt, g, "pos_estrato")
  ct <- .tr_sampling_col(tt, coluna_total, "coluna_total")
  .tr_sampling_numerica(tt, ct, "coluna_total")
  s <- .tr_sampling_pos(amostra, tt, g, ct)
  # A amostra declarada não tem receita: não há o que re-sortear.
  if (!is.null(s$receita)) {
    s$receita$pos <- c(s$receita$pos, list(list(totais = tt, coluna = g, coluna_total = ct)))
  }
  s
}

#' O ajuste em si, sem validar params: é o que a simulação reaplica.
#' @noRd
.tr_sampling_pos <- function(amostra, totais, g, ct) {
  Nt <- stats::setNames(as.numeric(totais[[ct]]), as.character(totais[[g]]))
  if (anyDuplicated(names(Nt))) {
    .tr_sampling_abort("tr_sampling_error_bad_option", "A tabela de totais repete pós-estrato na coluna '%s'.", g)
  }
  if (anyNA(Nt) || any(Nt <= 0)) {
    .tr_sampling_abort("tr_sampling_error_bad_size", "A coluna '%s' dos totais tem total faltante, zero ou negativo.", ct)
  }
  gs <- as.character(amostra$dados[[g]])
  sem_total <- setdiff(unique(gs), names(Nt))
  vazios <- setdiff(names(Nt), gs)
  if (anyNA(gs) || length(sem_total) || length(vazios)) {
    .tr_sampling_abort("tr_sampling_error_missing_total",
                       "Pós-estratos e totais não casam em '%s'%s%s%s. Cada pós-estrato precisa de total e de pelo menos uma unidade na amostra; junte os vazios a um vizinho.",
                       g, if (length(sem_total)) sprintf("; sem total: %s", paste(sem_total, collapse = ", ")) else "",
                       if (length(vazios)) sprintf("; sem ninguém na amostra: %s", paste(vazios, collapse = ", ")) else "",
                       if (anyNA(gs)) "; há faltante no pós-estrato" else "")
  }
  w <- amostra$dados$peso_amostral
  Nhat <- tapply(w, gs, sum)
  w2 <- as.numeric(w * (Nt[gs] / as.numeric(Nhat[gs])))
  amostra$dados$peso_amostral <- unname(w2)
  amostra$dados$prob_inclusao <- unname(1 / w2)
  amostra$pos <- list(colunas = g)
  amostra$N <- sum(Nt)
  amostra$rotulo <- paste(amostra$rotulo, "· pós-estratificada")
  amostra
}

#' Calibra os pesos por várias variáveis ao mesmo tempo (raking).
#'
#' A pós-estratificação cruzada (sexo × raça × idade × capital) pede o total de
#' CADA cela, que o Censo raramente publica e a amostra raramente preenche. O
#' raking pede só as margens — o total por sexo, o total por raça, o total por
#' capital — e ajusta os pesos variável por variável, em rodadas, até todas as
#' margens baterem ao mesmo tempo (ajuste proporcional iterativo).
#'
#' É a correção mínima de uma coleta não probabilística: não a torna aleatória,
#' mas faz a amostra ter o perfil conhecido da população.
#' @param amostra uma amostra (`sampling/sample`).
#' @param totais tabela longa: variável, categoria e total na população.
#' @param variavel coluna de `totais` com o nome da variável (que tem de ser coluna da amostra).
#' @param categoria coluna de `totais` com a categoria.
#' @param total coluna de `totais` com o total.
#' @param iteracoes máximo de rodadas.
#' @return a amostra com os pesos calibrados.
#' @export
tr_sampling_rake <- function(amostra, totais, variavel = "variavel", categoria = "categoria", total = "total",
                             iteracoes = 50L) {
  .tr_sampling_amostra_conferir(amostra)
  tt <- tibble::as_tibble(totais)
  cv <- .tr_sampling_col(tt, variavel, "variavel")
  cc <- .tr_sampling_col(tt, categoria, "categoria")
  ct <- .tr_sampling_col(tt, total, "total"); .tr_sampling_numerica(tt, ct, "total")
  it <- as.integer(.tr_sampling_num(iteracoes, "iteracoes", 1, 1000))
  margens <- lapply(split(tt, as.character(tt[[cv]])), function(x) {
    stats::setNames(as.numeric(x[[ct]]), as.character(x[[cc]]))
  })
  .tr_sampling_cols(amostra$dados, paste(names(margens), collapse = ","), "variavel", minimo = 1L)
  s <- .tr_sampling_raking(amostra, margens, it)
  if (!is.null(s$receita)) {
    s$receita$pos <- c(s$receita$pos, list(list(tipo = "rake", margens = margens, iteracoes = it)))
  }
  s
}

#' O raking em si, sem validar params: é o que a simulação reaplica.
#' @noRd
.tr_sampling_raking <- function(amostra, margens, iteracoes) {
  d <- amostra$dados
  somas <- vapply(margens, sum, 0)
  if (diff(range(somas)) > 0.01 * max(somas)) {
    .tr_sampling_abort("tr_sampling_error_bad_option",
                       "Os totais das variáveis não somam a mesma população (%s). Use a mesma fonte e o mesmo recorte.",
                       paste(sprintf("%s = %s", names(somas), .tr_sampling_fmt(somas, 6)), collapse = "; "))
  }
  for (v in names(margens)) {
    Nt <- margens[[v]]
    if (anyNA(Nt) || any(Nt <= 0) || anyDuplicated(names(Nt))) {
      .tr_sampling_abort("tr_sampling_error_bad_size",
                         "Os totais de '%s' têm categoria repetida, ou total faltante, zero ou negativo.", v)
    }
    gs <- as.character(d[[v]])
    sem_total <- setdiff(unique(gs), names(Nt)); vazios <- setdiff(names(Nt), gs)
    if (anyNA(gs) || length(sem_total) || length(vazios)) {
      .tr_sampling_abort("tr_sampling_error_missing_total",
                         "Categorias e totais não casam em '%s'%s%s%s. Junte as vazias a uma vizinha antes.",
                         v, if (length(sem_total)) sprintf("; sem total: %s", paste(sem_total, collapse = ", ")) else "",
                         if (length(vazios)) sprintf("; sem ninguém na amostra: %s", paste(vazios, collapse = ", ")) else "",
                         if (anyNA(gs)) "; há faltante" else "")
    }
  }
  w <- d$peso_amostral
  convergiu <- FALSE
  for (i in seq_len(iteracoes)) {
    for (v in names(margens)) {
      gs <- as.character(d[[v]])
      w <- as.numeric(w * (margens[[v]][gs] / as.numeric(tapply(w, gs, sum)[gs])))
    }
    desvio <- max(vapply(names(margens), function(v) {
      gs <- as.character(d[[v]])
      max(abs(tapply(w, gs, sum)[names(margens[[v]])] / margens[[v]] - 1))
    }, 0))
    if (desvio < 1e-6) { convergiu <- TRUE; break }
  }
  if (!convergiu) {
    .tr_sampling_abort("tr_sampling_error_no_convergence",
                       "O raking não convergiu em %d rodadas (desvio relativo %s). Aumente as rodadas, ou junte categorias com poucas pessoas.",
                       iteracoes, .tr_sampling_fmt(desvio))
  }
  amostra$dados$peso_amostral <- unname(as.numeric(w))
  amostra$dados$prob_inclusao <- unname(1 / as.numeric(w))
  amostra$pos <- list(colunas = names(margens))
  amostra$N <- somas[[1]]
  amostra$rotulo <- paste(amostra$rotulo, "· calibrada (raking)")
  amostra$nota <- .tr_sampling_nota(amostra$nota,
                                    sprintf("pesos calibrados por %s; amplitude dos pesos %s a %s",
                                            paste(names(margens), collapse = ", "),
                                            .tr_sampling_fmt(min(w)), .tr_sampling_fmt(max(w))))
  amostra
}
