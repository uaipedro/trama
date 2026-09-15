# Os seis desenhos de seleção.
#
# Cada desenho tem duas camadas:
#
# - `.tr_sampling_sel_*(populacao, ...)`: o sorteio puro, com os params já
#   resolvidos e sem semente. É o que a RECEITA guarda e o que o
#   `sampling/simulate` chama centenas de vezes.
# - `tr_sampling_*()`: o `fn` do nó. Valida, resolve o n (do campo ou do
#   plano ligado), sorteia dentro da semente do card e guarda a receita.
#
# Separar assim é o que garante que a simulação sorteia EXATAMENTE o desenho do
# card: não há uma segunda implementação para divergir.

# ---- comum --------------------------------------------------------------------

#' A população: tabela com linhas bastantes.
#' @noRd
.tr_sampling_populacao <- function(populacao, minimo = 2L) {
  p <- tibble::as_tibble(populacao)
  if (nrow(p) < minimo) {
    .tr_sampling_abort("tr_sampling_error_too_few",
                       "A população tem %d linha(s); sortear uma amostra pede pelo menos %d.", nrow(p), as.integer(minimo))
  }
  p
}

#' O plano ligado é do tipo que o bloco sabe usar?
#' @noRd
.tr_sampling_plano_do_tipo <- function(plano, tipos, no, sugestao) {
  .tr_sampling_plano_conferir(plano)
  if (!plano$tipo %in% tipos) {
    .tr_sampling_abort("tr_sampling_error_plan_mismatch",
                       "'%s' usa plano de %s, e o ligado é de %s. Ligue este plano em %s.",
                       no, paste(tipos, collapse = " ou "), plano$tipo, sugestao)
  }
  plano
}

#' O n de um desenho por unidades: do plano, do campo n ou da fração.
#'
#' O plano VENCE os campos, como a fórmula vence as colunas na `models`: ligar o
#' cabo é um gesto mais deliberado que um número esquecido no card.
#' @noRd
.tr_sampling_resolver_n <- function(N, n, fracao, plano, no, reposicao = FALSE) {
  if (!is.null(plano)) {
    .tr_sampling_plano_do_tipo(plano, c("média", "proporção"), no,
                               "'sampling/stratified' (estratificado) ou 'sampling/cluster' / 'sampling/two_stage' (conglomerados)")
    n <- plano$n
  } else if (length(n) == 1L && !is.na(n) && n > 0) {
    n <- as.integer(round(n))
  } else if (length(fracao) == 1L && !is.na(fracao) && fracao > 0) {
    .tr_sampling_num(fracao, "fracao", 0, 1, aberto_min = TRUE)
    n <- max(1L, as.integer(round(fracao * N)))
  } else {
    .tr_sampling_abort("tr_sampling_error_blank_param",
                       "'%s': preencha 'n' ou 'fracao', ou ligue um plano.", no)
  }
  if (n < 2L) {
    .tr_sampling_abort("tr_sampling_error_too_few",
                       "'%s': n = %d; estimar a variância pede pelo menos 2 unidades.", no, n)
  }
  if (!reposicao && n > N) {
    .tr_sampling_abort("tr_sampling_error_too_large",
                       "'%s': n = %d é maior que a população (N = %d).", no, n, N)
  }
  as.integer(n)
}

#' Medida de tamanho: numérica, sem faltante, positiva.
#' @noRd
.tr_sampling_tamanho <- function(p, col, param) {
  col <- .tr_sampling_col(p, col, param)
  .tr_sampling_numerica(p, col, param)
  x <- p[[col]]
  if (anyNA(x) || any(x <= 0)) {
    .tr_sampling_abort("tr_sampling_error_bad_size",
                       "Param '%s': a coluna '%s' tem %d valor(es) faltante(s), zero(s) ou negativo(s), e medida de tamanho tem de ser positiva.",
                       param, col, sum(is.na(x) | x <= 0))
  }
  col
}

#' PPS sistemático de Madow, com unidades de certeza.
#'
#' Unidade com n·x/X ≥ 1 entraria "mais de uma vez": vai para a amostra com
#' probabilidade 1 e sai da conta, que é refeita com as restantes até ninguém
#' passar de 1. A lista é percorrida na ORDEM DO CADASTRO: ordenar o cadastro
#' antes (por região, por tamanho) dá estratificação implícita de graça.
#' @return lista com `idx` (sorteados), `pi` (de todos) e `certeza` (lógico).
#' @noRd
.tr_sampling_pps_indices <- function(x, n) {
  N <- length(x)
  cert <- rep(FALSE, N)
  repeat {
    resto <- n - sum(cert)
    if (resto <= 0L) break
    pi <- resto * x / sum(x[!cert])
    novos <- !cert & pi >= 1
    if (!any(novos)) break
    cert <- cert | novos
  }
  resto <- n - sum(cert)
  pi <- rep(1, N)
  idx_resto <- integer()
  if (resto > 0L) {
    pi[!cert] <- resto * x[!cert] / sum(x[!cert])
    cum <- cumsum(pi[!cert])
    pontos <- stats::runif(1L) + 0:(resto - 1L)
    pos <- findInterval(pontos, c(0, cum), left.open = TRUE)
    idx_resto <- which(!cert)[pos]
  }
  list(idx = sort(c(which(cert), idx_resto)), pi = pi, certeza = cert)
}

#' Aloca n entre estratos: proporcional, Neyman, ótima com custo ou igual.
#'
#' Duas travas que a fórmula de livro não tem e o campo tem: nenhum estrato
#' recebe mais que a sua população (o excedente é censo, e o resto é
#' redistribuído), e nenhum recebe menos que `minimo` (com uma unidade só a
#' variância do estrato não existe). O arredondamento é pelos maiores restos,
#' para a soma dar exatamente n.
#' @noRd
.tr_sampling_alocar <- function(N, S, custo, n, metodo, minimo = 2L) {
  H <- length(N)
  if (n > sum(N)) {
    .tr_sampling_abort("tr_sampling_error_too_large",
                       "n = %d é maior que a população somada dos estratos (%d).", as.integer(n), as.integer(sum(N)))
  }
  piso <- pmin(minimo, N)
  if (n < sum(piso)) {
    .tr_sampling_abort("tr_sampling_error_too_few",
                       "n = %d não dá %d unidades por estrato em %d estratos (pede pelo menos %d).",
                       as.integer(n), as.integer(minimo), H, as.integer(sum(piso)))
  }
  peso <- switch(metodo,
    proporcional = N,
    neyman = N * S,
    `ótima` = N * S / sqrt(custo),
    igual = rep(1, H))
  if (!any(peso > 0)) peso <- N
  nh <- numeric(H)
  fixo <- rep(FALSE, H)
  repeat {
    resto <- n - sum(nh[fixo])
    livres <- !fixo
    pl <- peso[livres]
    parte <- if (sum(pl) > 0) resto * pl / sum(pl) else rep(resto / sum(livres), sum(livres))
    cand <- nh; cand[livres] <- parte
    acima <- livres & cand > N
    abaixo <- livres & cand < piso
    if (any(acima)) { nh[acima] <- N[acima]; fixo <- fixo | acima; next }
    if (any(abaixo)) { nh[abaixo] <- piso[abaixo]; fixo <- fixo | abaixo; next }
    nh <- cand
    break
  }
  base <- floor(nh + 1e-9)
  falta <- as.integer(round(n - sum(base)))
  if (falta > 0L) {
    folga <- which(base < N)
    ordem <- folga[order(-(nh - base)[folga])]
    base[ordem[seq_len(falta)]] <- base[ordem[seq_len(falta)]] + 1
  }
  as.integer(base)
}

# ---- AAS ----------------------------------------------------------------------

.tr_sampling_sel_srs <- function(populacao, n, reposicao) {
  N <- nrow(populacao)
  idx <- sample.int(N, n, replace = reposicao)
  .tr_sampling_amostra(populacao[idx, ], rep(N / n, n),
                       if (reposicao) "AAS com reposição" else "AAS sem reposição", "srs",
                       fpc = stats::setNames(if (reposicao) NA_real_ else N, "."), N = N,
                       receita = list(selecao = list(fun = ".tr_sampling_sel_srs", args = list(n = n, reposicao = reposicao))),
                       populacao = populacao)
}

#' Amostra aleatória simples.
#' @param populacao o cadastro.
#' @param n tamanho da amostra (0: usa `fracao`).
#' @param fracao fração da população (0 a 1), se `n` for 0.
#' @param reposicao sortear com reposição.
#' @param plano plano de `sampling/size_mean` ou `sampling/size_proportion` (vence `n`).
#' @param .seed semente (do núcleo).
#' @return uma amostra (`sampling/sample`).
#' @export
tr_sampling_srs <- function(populacao, n = 0L, fracao = 0, reposicao = FALSE, plano = NULL, .seed = 1L) {
  p <- .tr_sampling_populacao(populacao)
  n <- .tr_sampling_resolver_n(nrow(p), n, fracao, plano, "sampling/srs", isTRUE(reposicao))
  .tr_sampling_com_semente(.seed, .tr_sampling_sel_srs(p, n, isTRUE(reposicao)))
}

# ---- sistemática --------------------------------------------------------------

.tr_sampling_sel_systematic <- function(populacao, n, ordenar) {
  p <- if (is.null(ordenar)) populacao else populacao[order(populacao[[ordenar]]), ]
  N <- nrow(p)
  k <- N / n
  # Intervalo FRACIONÁRIO: com N/n não inteiro, o k arredondado daria n − 1 ou
  # n + 1 unidades conforme o começo. Assim sai sempre n.
  idx <- floor(stats::runif(1L) * k + (0:(n - 1L)) * k) + 1L
  .tr_sampling_amostra(p[idx, ], rep(N / n, n),
                       if (is.null(ordenar)) "Sistemática" else sprintf("Sistemática · ordenada por %s", ordenar),
                       "systematic", fpc = stats::setNames(N, "."), N = N,
                       receita = list(selecao = list(fun = ".tr_sampling_sel_systematic", args = list(n = n, ordenar = ordenar))),
                       populacao = populacao,
                       nota = "variância estimada como na AAS (a sistemática não tem estimador próprio sem suposição)")
}

#' Amostra sistemática.
#' @inheritParams tr_sampling_srs
#' @param ordenar coluna pela qual ordenar o cadastro antes (em branco: a ordem dele).
#' @return uma amostra (`sampling/sample`).
#' @export
tr_sampling_systematic <- function(populacao, n = 0L, fracao = 0, ordenar = "", plano = NULL, .seed = 1L) {
  p <- .tr_sampling_populacao(populacao)
  ordenar <- .tr_sampling_col_opcional(p, ordenar, "ordenar")
  n <- .tr_sampling_resolver_n(nrow(p), n, fracao, plano, "sampling/systematic")
  .tr_sampling_com_semente(.seed, .tr_sampling_sel_systematic(p, n, ordenar))
}

# ---- estratificada ------------------------------------------------------------

.tr_sampling_sel_stratified <- function(populacao, estrato, nh, rotulo) {
  h <- as.character(populacao[[estrato]])
  niveis <- names(nh)
  idx <- unlist(lapply(niveis, function(s) {
    linhas <- which(h == s)
    linhas[sample.int(length(linhas), nh[[s]])]
  }), use.names = FALSE)
  Nh <- table(h)[niveis]
  hs <- h[idx]
  .tr_sampling_amostra(populacao[idx, ], as.numeric(Nh[hs] / nh[hs]), rotulo, "stratified",
                       estrato = hs, fpc = stats::setNames(as.numeric(Nh), niveis),
                       estrato_col = estrato, N = nrow(populacao),
                       receita = list(selecao = list(fun = ".tr_sampling_sel_stratified",
                                                     args = list(estrato = estrato, nh = nh, rotulo = rotulo))),
                       populacao = populacao)
}

#' Amostra estratificada.
#' @inheritParams tr_sampling_srs
#' @param estrato coluna do estrato.
#' @param alocacao `"proporcional"`, `"igual"` ou `"neyman"` (ignorada com plano).
#' @param variavel_auxiliar coluna numérica cujo desvio por estrato guia o Neyman.
#' @param plano plano de `sampling/size_stratified` (dá os n por estrato).
#' @return uma amostra (`sampling/sample`).
#' @export
tr_sampling_stratified <- function(populacao, estrato = "", n = 0L, alocacao = "proporcional",
                                   variavel_auxiliar = "", plano = NULL, .seed = 1L) {
  p <- .tr_sampling_populacao(populacao)
  estrato <- .tr_sampling_col(p, estrato, "estrato")
  h <- as.character(p[[estrato]])
  if (anyNA(h)) {
    .tr_sampling_abort("tr_sampling_error_bad_option",
                       "Param 'estrato': %d linha(s) do cadastro sem estrato na coluna '%s'.", sum(is.na(h)), estrato)
  }
  niveis <- unique(h)
  Nh <- as.numeric(table(h)[niveis])
  if (!is.null(plano)) {
    .tr_sampling_plano_do_tipo(plano, "estratificada", "sampling/stratified",
                               "'sampling/srs' ou 'sampling/systematic'")
    al <- plano$alocacao
    so_plano <- setdiff(al$estrato, niveis); so_cadastro <- setdiff(niveis, al$estrato)
    if (length(so_plano) || length(so_cadastro)) {
      .tr_sampling_abort("tr_sampling_error_plan_mismatch",
                         "Os estratos do plano e os da coluna '%s' não casam%s%s.", estrato,
                         if (length(so_plano)) sprintf("; só no plano: %s", paste(so_plano, collapse = ", ")) else "",
                         if (length(so_cadastro)) sprintf("; só no cadastro: %s", paste(so_cadastro, collapse = ", ")) else "")
    }
    nh <- stats::setNames(as.integer(al$n_final[match(niveis, al$estrato)]), niveis)
    grandes <- nh > Nh
    if (any(grandes)) {
      .tr_sampling_abort("tr_sampling_error_too_large",
                         "O plano pede mais unidades que o cadastro tem em: %s. O plano foi feito com outra população?",
                         paste(niveis[grandes], collapse = ", "))
    }
    rotulo <- "Estratificada · do plano"
  } else {
    alocacao <- .tr_sampling_enum(alocacao, c("proporcional", "igual", "neyman"), "alocacao")
    if (!(length(n) == 1L && !is.na(n) && n > 0)) {
      .tr_sampling_abort("tr_sampling_error_blank_param",
                         "'sampling/stratified': preencha 'n' ou ligue um plano de 'sampling/size_stratified'.")
    }
    S <- rep(1, length(niveis))
    if (alocacao == "neyman") {
      aux <- .tr_sampling_col(p, variavel_auxiliar, "variavel_auxiliar")
      .tr_sampling_numerica(p, aux, "variavel_auxiliar")
      S <- vapply(niveis, function(s) {
        v <- stats::sd(p[[aux]][h == s], na.rm = TRUE)
        if (is.na(v)) 0 else v
      }, 0)
    }
    nh <- stats::setNames(.tr_sampling_alocar(Nh, S, rep(1, length(niveis)), as.integer(round(n)), alocacao), niveis)
    rotulo <- sprintf("Estratificada · %s", if (alocacao == "neyman") "Neyman" else alocacao)
  }
  .tr_sampling_com_semente(.seed, .tr_sampling_sel_stratified(p, estrato, nh, rotulo))
}

# ---- PPS ----------------------------------------------------------------------

.tr_sampling_sel_pps <- function(populacao, tamanho, n) {
  s <- .tr_sampling_pps_indices(populacao[[tamanho]], n)
  pi <- s$pi[s$idx]
  cert <- s$certeza[s$idx]
  # A variância é a com reposição (Hansen-Hurwitz) nas não certas; as certas
  # formam um estrato de censo, que não contribui.
  h <- ifelse(cert, "certeza", "sorteadas")
  fpc <- c(sorteadas = NA_real_, certeza = sum(cert))
  .tr_sampling_amostra(populacao[s$idx, ], 1 / pi, sprintf("PPS sistemática · %s", tamanho), "pps",
                       estrato = h, fpc = fpc, N = nrow(populacao),
                       receita = list(selecao = list(fun = ".tr_sampling_sel_pps", args = list(tamanho = tamanho, n = n))),
                       populacao = populacao,
                       nota = .tr_sampling_nota(
                         if (any(cert)) sprintf("%d unidade(s) de certeza (probabilidade 1)", sum(cert)) else "",
                         "variância com reposição (Hansen-Hurwitz), levemente conservadora"))
}

#' Amostra com probabilidade proporcional ao tamanho (PPS sistemática).
#' @inheritParams tr_sampling_srs
#' @param tamanho coluna numérica positiva com a medida de tamanho.
#' @return uma amostra (`sampling/sample`).
#' @export
tr_sampling_pps <- function(populacao, tamanho = "", n = 0L, plano = NULL, .seed = 1L) {
  p <- .tr_sampling_populacao(populacao)
  tamanho <- .tr_sampling_tamanho(p, tamanho, "tamanho")
  n <- .tr_sampling_resolver_n(nrow(p), n, 0, plano, "sampling/pps")
  .tr_sampling_com_semente(.seed, .tr_sampling_sel_pps(p, tamanho, n))
}

# ---- conglomerados ------------------------------------------------------------

#' Sorteia m conglomerados: com probabilidade igual ou proporcional ao número
#' de unidades. Devolve os ids, a probabilidade de cada um e o desenho do
#' primeiro estágio (estrato de certeza e fpc).
#' @noRd
.tr_sampling_primeiro_estagio <- function(ids, m, probabilidade) {
  tam <- table(ids)
  todos <- names(tam)
  M <- length(todos)
  if (probabilidade == "iguais") {
    sorteados <- todos[sort(sample.int(M, m))]
    list(sorteados = sorteados, pi = stats::setNames(rep(m / M, m), sorteados),
         estrato = stats::setNames(rep(".", m), sorteados), fpc = stats::setNames(M, "."))
  } else {
    s <- .tr_sampling_pps_indices(as.numeric(tam), m)
    sorteados <- todos[s$idx]
    list(sorteados = sorteados, pi = stats::setNames(s$pi[s$idx], sorteados),
         estrato = stats::setNames(ifelse(s$certeza[s$idx], "certeza", "sorteados"), sorteados),
         fpc = c(sorteados = NA_real_, certeza = sum(s$certeza)))
  }
}

.tr_sampling_m_conglomerados <- function(ids, conglomerados, plano, no) {
  M <- length(unique(ids))
  if (!is.null(plano)) {
    .tr_sampling_plano_do_tipo(plano, "conglomerados", no, "'sampling/srs', 'sampling/systematic' ou 'sampling/pps'")
    conglomerados <- plano$conglomerados
  }
  if (!(length(conglomerados) == 1L && !is.na(conglomerados) && conglomerados > 0)) {
    .tr_sampling_abort("tr_sampling_error_blank_param",
                       "'%s': preencha 'conglomerados' ou ligue um plano de 'sampling/size_cluster'.", no)
  }
  m <- as.integer(round(conglomerados))
  if (m < 2L) {
    .tr_sampling_abort("tr_sampling_error_too_few", "'%s': sortear %d conglomerado(s) não dá variância; pelo menos 2.", no, m)
  }
  if (m > M) {
    .tr_sampling_abort("tr_sampling_error_too_large", "'%s': %d conglomerados pedidos, e a população tem %d.", no, m, M)
  }
  m
}

.tr_sampling_sel_cluster <- function(populacao, conglomerado, m, probabilidade) {
  ids <- as.character(populacao[[conglomerado]])
  e1 <- .tr_sampling_primeiro_estagio(ids, m, probabilidade)
  idx <- which(ids %in% e1$sorteados)
  cs <- ids[idx]
  .tr_sampling_amostra(populacao[idx, ], 1 / e1$pi[cs],
                       sprintf("Conglomerados · %s%s", conglomerado,
                               if (probabilidade == "iguais") "" else " · PPS"), "cluster",
                       estrato = e1$estrato[cs], psu = cs, fpc = e1$fpc, conglomerado_col = conglomerado,
                       N = nrow(populacao),
                       receita = list(selecao = list(fun = ".tr_sampling_sel_cluster",
                                                     args = list(conglomerado = conglomerado, m = m, probabilidade = probabilidade))),
                       populacao = populacao)
}

#' Amostra de conglomerados em um estágio (todas as unidades do sorteado).
#' @inheritParams tr_sampling_srs
#' @param conglomerado coluna que identifica o conglomerado.
#' @param conglomerados quantos conglomerados sortear.
#' @param probabilidade `"iguais"` ou `"proporcional ao tamanho"` (nº de unidades).
#' @param plano plano de `sampling/size_cluster`.
#' @return uma amostra (`sampling/sample`).
#' @export
tr_sampling_cluster <- function(populacao, conglomerado = "", conglomerados = 0L, probabilidade = "iguais",
                                plano = NULL, .seed = 1L) {
  p <- .tr_sampling_populacao(populacao)
  conglomerado <- .tr_sampling_col(p, conglomerado, "conglomerado")
  probabilidade <- .tr_sampling_enum(probabilidade, c("iguais", "proporcional ao tamanho"), "probabilidade")
  ids <- as.character(p[[conglomerado]])
  if (anyNA(ids)) {
    .tr_sampling_abort("tr_sampling_error_bad_option", "Param 'conglomerado': %d linha(s) sem conglomerado.", sum(is.na(ids)))
  }
  m <- .tr_sampling_m_conglomerados(ids, conglomerados, plano, "sampling/cluster")
  .tr_sampling_com_semente(.seed, .tr_sampling_sel_cluster(p, conglomerado, m, probabilidade))
}

.tr_sampling_sel_two_stage <- function(populacao, conglomerado, m, por, probabilidade) {
  ids <- as.character(populacao[[conglomerado]])
  e1 <- .tr_sampling_primeiro_estagio(ids, m, probabilidade)
  idx <- integer(); pesos <- numeric(); cs <- character()
  for (c in e1$sorteados) {
    linhas <- which(ids == c)
    Nc <- length(linhas)
    nc <- min(por, Nc)
    escolhidas <- linhas[sort(sample.int(Nc, nc))]
    idx <- c(idx, escolhidas)
    pesos <- c(pesos, rep((1 / e1$pi[[c]]) * Nc / nc, nc))
    cs <- c(cs, rep(c, nc))
  }
  .tr_sampling_amostra(populacao[idx, ], pesos,
                       sprintf("Dois estágios · %s%s", conglomerado, if (probabilidade == "iguais") "" else " · PPS"),
                       "two_stage", estrato = e1$estrato[cs], psu = cs, fpc = e1$fpc,
                       conglomerado_col = conglomerado, N = nrow(populacao),
                       receita = list(selecao = list(fun = ".tr_sampling_sel_two_stage",
                                                     args = list(conglomerado = conglomerado, m = m, por = por,
                                                                 probabilidade = probabilidade))),
                       populacao = populacao,
                       nota = "variância pelo conglomerado último (o segundo estágio entra na variação entre conglomerados)")
}

#' Amostra de conglomerados em dois estágios.
#' @inheritParams tr_sampling_cluster
#' @param por_conglomerado unidades sorteadas dentro de cada conglomerado.
#' @param primeiro_estagio `"iguais"` ou `"proporcional ao tamanho"`.
#' @return uma amostra (`sampling/sample`).
#' @export
tr_sampling_two_stage <- function(populacao, conglomerado = "", conglomerados = 0L, por_conglomerado = 0L,
                                  primeiro_estagio = "proporcional ao tamanho", plano = NULL, .seed = 1L) {
  p <- .tr_sampling_populacao(populacao)
  conglomerado <- .tr_sampling_col(p, conglomerado, "conglomerado")
  primeiro_estagio <- .tr_sampling_enum(primeiro_estagio, c("iguais", "proporcional ao tamanho"), "primeiro_estagio")
  ids <- as.character(p[[conglomerado]])
  if (anyNA(ids)) {
    .tr_sampling_abort("tr_sampling_error_bad_option", "Param 'conglomerado': %d linha(s) sem conglomerado.", sum(is.na(ids)))
  }
  m <- .tr_sampling_m_conglomerados(ids, conglomerados, plano, "sampling/two_stage")
  if (!is.null(plano)) por_conglomerado <- plano$tamanho_conglomerado
  if (!(length(por_conglomerado) == 1L && !is.na(por_conglomerado) && por_conglomerado > 0)) {
    .tr_sampling_abort("tr_sampling_error_blank_param",
                       "'sampling/two_stage': preencha 'por_conglomerado' ou ligue um plano de 'sampling/size_cluster'.")
  }
  por <- as.integer(ceiling(por_conglomerado))
  .tr_sampling_com_semente(.seed, .tr_sampling_sel_two_stage(p, conglomerado, m, por, primeiro_estagio))
}

#' Sorteia de novo pela receita guardada (sem semente: quem chama já está
#' dentro de uma).
#' @noRd
.tr_sampling_refazer <- function(amostra) {
  r <- amostra$receita
  if (is.null(r) || is.null(r$selecao) || is.null(amostra$populacao)) {
    .tr_sampling_abort("tr_sampling_error_no_population",
                       "A amostra '%s' não guarda a população de onde saiu (é declarada), e simular pede re-sortear. Simule a partir de um bloco de seleção.",
                       amostra$rotulo)
  }
  s <- do.call(get(r$selecao$fun, mode = "function"), c(list(amostra$populacao), r$selecao$args))
  for (pp in r$pos) {
    s <- if (identical(pp$tipo, "rake")) .tr_sampling_raking(s, pp$margens, pp$iteracoes)
         else .tr_sampling_pos(s, pp$totais, pp$coluna, pp$coluna_total)
  }
  s$receita <- r
  s
}
