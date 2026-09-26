# A precisão ALCANÇÁVEL: o caminho contrário do tamanho da amostra.
#
# O plano clássico pergunta "quanto preciso para ±5 pontos?". A pesquisa que já
# tem o campo definido — 25 jovens por capital, bolsistas contratados — pergunta
# o inverso: "com o que vamos ter, quanto erramos, e em que nível dá para
# afirmar alguma coisa?". Os blocos daqui respondem isso, em tabela, para que
# vários níveis (capital, região, país) e vários cenários (quantos convidados,
# que ICC) caibam num card só e sigam para um gráfico ou um relatório.
#
# Todas as margens são para PROPORÇÃO, com p = 0,5 por padrão: é o pior caso, e
# vale para qualquer pergunta sim/não ou para cada alternativa de uma múltipla
# escolha.

#' A margem de uma proporção: q · √(deff · p(1 − p) / n · (1 − n/N)), com q o
#' t de `gl` graus de liberdade (z com `gl = Inf`).
#' @noRd
.tr_sampling_margem <- function(n, p, conf, deff = 1, N = 0, gl = Inf) {
  fpc <- ifelse(N > 0, pmax(0, 1 - n / N), 1)
  .tr_sampling_q(conf, gl) * sqrt(deff * p * (1 - p) / n * fpc)
}

#' Lista "0, 1, 2" de números, validada.
#' @noRd
.tr_sampling_lista_num <- function(texto, param, min = 0, max = Inf) {
  partes <- .tr_sampling_split(texto)
  v <- suppressWarnings(as.numeric(sub(",", ".", partes, fixed = TRUE)))
  if (!length(partes)) {
    .tr_sampling_abort("tr_sampling_error_blank_param",
                       "Param '%s': campo obrigatório em branco. Preencha-o no card.", param)
  }
  if (anyNA(v) || any(v < min) || any(v > max)) {
    .tr_sampling_abort("tr_sampling_error_bad_option",
                       "Param '%s': lista de números entre %g e %g separados por vírgula, com ponto decimal (veio '%s').",
                       param, min, max, texto)
  }
  unique(v)
}

#' Margem de erro para um n dado.
#' @param n tamanho da amostra (entrevistas previstas).
#' @param proporcao proporção esperada (0,5 é o pior caso).
#' @param confianca nível de confiança, entre 0,5 e 0,999 (0,95 = 95%).
#' @param populacao tamanho da população (0: infinita).
#' @param deff efeito do desenho esperado.
#' @param taxa_resposta fração das entrevistas previstas que vira resposta.
#' @param distribuicao `"t"` (gl = respostas − 1) ou `"z"`.
#' @return um plano (`sampling/plan`) do tipo margem.
#' @export
tr_sampling_margin <- function(n = 400L, proporcao = 0.5, confianca = 0.95, populacao = 0,
                               deff = 1, taxa_resposta = 1, distribuicao = "t") {
  cm <- .tr_sampling_comuns(confianca, populacao, deff, taxa_resposta, distribuicao)
  n <- .tr_sampling_num(n, "n", 2)
  p <- .tr_sampling_num(proporcao, "proporcao", 0, 1, aberto_min = TRUE)
  if (p >= 1) {
    .tr_sampling_abort("tr_sampling_error_bad_option", "Param 'proporcao': tem de ficar abaixo de 1 (veio %s).", p)
  }
  passos <- list(c("entrevistas previstas", n, "entrevistas"))
  nr <- n * cm$resposta
  if (cm$resposta < 1) {
    passos[[length(passos) + 1L]] <- c(sprintf("× taxa de resposta (%s)", .tr_sampling_pct(cm$resposta)), nr, "entrevistas")
  }
  ne <- nr / cm$deff
  if (cm$deff != 1) {
    passos[[length(passos) + 1L]] <- c(sprintf("÷ deff %s = n efetivo", .tr_sampling_fmt(cm$deff)), ne, "entrevistas")
  }
  gl <- if (cm$distribuicao == "z") Inf else max(1, floor(nr) - 1)
  E <- .tr_sampling_margem(nr, p, cm$conf, cm$deff, cm$N, gl)
  if (cm$N > 0) passos[[length(passos) + 1L]] <- c(sprintf("população finita (N = %s)", .tr_sampling_fmt(cm$N)), ne, "entrevistas")
  passos[[length(passos) + 1L]] <- c(sprintf("margem de erro (pontos percentuais%s)", .tr_sampling_rotulo_q(gl)),
                                     100 * E, "pontos")
  .tr_sampling_plano("margem", "Margem · proporção", nr,
                     tibble::tibble(passo = vapply(passos, `[[`, "", 1L),
                                    valor = as.numeric(vapply(passos, `[[`, "", 2L)),
                                    unidade = vapply(passos, `[[`, "", 3L)),
                     cm$conf, E,
                     parametros = list(medida = "proporção", p = p, N = cm$N, deff = cm$deff,
                                       resposta = cm$resposta, n0 = ne, gl = gl,
                                       distribuicao = cm$distribuicao),
                     nota = if (p == 0.5) "p = 0,5: a margem vale para qualquer pergunta sim/não" else "")
}

#' O cálculo por nível, sem validar params: o que `sampling/referral` repete
#' por cenário.
#'
#' Agregar unidades de tamanhos diferentes com o MESMO n em cada uma exige
#' ponderar (Belém pesa mais que Palmas na média do Norte), e ponderar custa
#' precisão. O custo é o deff de Kish, n · Σ W_h²/n_h: 1 quando a amostra é
#' proporcional à população, e maior quanto mais a alocação se afasta dela. A
#' margem de cada agregado sai da variância estratificada exata, com correção
#' finita, vezes o deff de agrupamento.
#' @noRd
.tr_sampling_niveis <- function(u, N, n, grupos, p, conf, deff, distribuicao = "t", upas = n) {
  linha <- function(tipo, nome, idx) {
    Nh <- N[idx]; nh <- n[idx]
    # gl = UPAs − estratos (cada unidade é um estrato; as UPAs são as
    # entrevistas, ou as redes na indicação).
    gl <- if (distribuicao == "z") Inf else max(1, sum(upas[idx]) - length(idx))
    W <- Nh / sum(Nh)
    v <- sum(W^2 * pmax(0, 1 - nh / Nh) * p * (1 - p) / nh)
    kish <- sum(nh) * sum(W^2 / nh)
    # Calculado ANTES do tibble: dentro dele, a coluna `deff` recém-criada
    # sombrearia o argumento, e a margem sairia multiplicada pelo Kish de novo.
    E <- .tr_sampling_q(conf, gl) * sqrt(deff * v)
    tibble::tibble(nivel_tipo = tipo, nivel = nome, unidades = length(idx), populacao = sum(Nh), n = sum(nh),
                   deff_ponderacao = kish, deff = kish * deff, n_efetivo = sum(nh) / (kish * deff),
                   gl = gl, margem = E, margem_pp = 100 * E)
  }
  partes <- list(linha("total", "Total", seq_along(u)))
  for (g in names(grupos)) {
    vals <- grupos[[g]]
    for (k in unique(vals)) partes[[length(partes) + 1L]] <- linha(g, k, which(vals == k))
  }
  for (i in seq_along(u)) partes[[length(partes) + 1L]] <- linha("unidade", u[[i]], i)
  do.call(rbind, partes)
}

#' Lê e confere a tabela de unidades (capitais, municípios).
#' @noRd
.tr_sampling_unidades <- function(unidades, unidade, tamanho, n, grupos) {
  d <- tibble::as_tibble(unidades)
  cu <- .tr_sampling_col(d, unidade, "unidade")
  cN <- .tr_sampling_col(d, tamanho, "tamanho"); .tr_sampling_numerica(d, cN, "tamanho")
  cn <- .tr_sampling_col(d, n, "n"); .tr_sampling_numerica(d, cn, "n")
  cg <- .tr_sampling_cols(d, grupos, "grupos", minimo = 0L)
  u <- as.character(d[[cu]])
  if (anyNA(u) || anyDuplicated(u)) {
    .tr_sampling_abort("tr_sampling_error_bad_option",
                       "Param 'unidade': a coluna '%s' tem de ter um nome por linha, sem repetição nem faltante.", cu)
  }
  N <- as.numeric(d[[cN]]); nn <- as.numeric(d[[cn]])
  if (anyNA(N) || any(N <= 0)) {
    .tr_sampling_abort("tr_sampling_error_bad_size", "A coluna '%s' tem população faltante, zero ou negativa.", cN)
  }
  if (anyNA(nn) || any(nn < 2)) {
    .tr_sampling_abort("tr_sampling_error_too_few",
                       "A coluna '%s' tem unidade com menos de 2 entrevistas (%s): a margem dela não existe.",
                       cn, paste(u[is.na(nn) | nn < 2], collapse = ", "))
  }
  if (any(nn > N)) {
    .tr_sampling_abort("tr_sampling_error_too_large", "Unidade com mais entrevistas que população: %s.",
                       paste(u[nn > N], collapse = ", "))
  }
  grp <- stats::setNames(lapply(cg, function(g) {
    v <- as.character(d[[g]])
    if (anyNA(v)) .tr_sampling_abort("tr_sampling_error_bad_option", "Param 'grupos': a coluna '%s' tem faltante.", g)
    v
  }), cg)
  list(u = u, N = N, n = nn, grupos = grp)
}

#' Margem de erro por nível de agregação, com o custo da ponderação.
#' @param unidades tabela com uma linha por unidade (capital, município).
#' @param unidade coluna do nome da unidade.
#' @param tamanho coluna da população da unidade.
#' @param n coluna com as entrevistas previstas na unidade.
#' @param grupos colunas de agrupamento intermediário, separadas por vírgula (ex.: `regiao`).
#' @param deff efeito de agrupamento além da ponderação (1: nenhum).
#' @inheritParams tr_sampling_margin
#' @return tabela (`data/table`) com uma linha por nível.
#' @export
tr_sampling_margin_levels <- function(unidades, unidade = "", tamanho = "", n = "", grupos = "", deff = 1,
                                      proporcao = 0.5, confianca = 0.95, distribuicao = "t") {
  distribuicao <- .tr_sampling_distrib(distribuicao)
  x <- .tr_sampling_unidades(unidades, unidade, tamanho, n, grupos)
  conf <- .tr_sampling_conf(confianca)
  deff <- .tr_sampling_num(deff, "deff", 0, aberto_min = TRUE)
  p <- .tr_sampling_num(proporcao, "proporcao", 0, 0.999, aberto_min = TRUE)
  .tr_sampling_niveis(x$u, x$N, x$n, x$grupos, p, conf, deff, distribuicao)
}

#' Margem de erro na expansão por indicação, por cenário.
#' @inheritParams tr_sampling_margin_levels
#' @param convidados lista de quantos convidados por participante (ex.: `"0, 1, 2, 3"`).
#' @param adesao fração dos convidados que de fato responde.
#' @param icc lista de correlações intraclasse entre quem convida e os convidados.
#' @param cv_rede coeficiente de variação do tamanho das redes (0: todas do
#'   mesmo tamanho).
#' @param distribuicao `"t"` (gl = participantes da base − unidades) ou `"z"`.
#' @return tabela (`data/table`) com uma linha por cenário e nível.
#' @export
tr_sampling_referral <- function(unidades, unidade = "", tamanho = "", n = "", grupos = "",
                                 convidados = "0, 1, 2, 3, 5", adesao = 1, icc = "0.05, 0.1, 0.2",
                                 proporcao = 0.5, confianca = 0.95, cv_rede = 0, distribuicao = "t") {
  x <- .tr_sampling_unidades(unidades, unidade, tamanho, n, grupos)
  conf <- .tr_sampling_conf(confianca)
  cv <- .tr_sampling_num(cv_rede, "cv_rede", 0, 10)
  distribuicao <- .tr_sampling_distrib(distribuicao)
  p <- .tr_sampling_num(proporcao, "proporcao", 0, 0.999, aberto_min = TRUE)
  ks <- .tr_sampling_lista_num(convidados, "convidados", 0, 1000)
  rhos <- .tr_sampling_lista_num(icc, "icc", 0, 1)
  ad <- .tr_sampling_num(adesao, "adesao", 0, 1, aberto_min = TRUE)
  partes <- list()
  for (rho in rhos) for (k in ks) {
    # Cada participante da base vira uma "rede" de 1 + k·adesão respostas. As
    # respostas da mesma rede se parecem (amigos, vizinhos): o custo é o deff
    # de conglomerado, 1 + (m − 1)·ICC, ou, com redes de tamanho desigual,
    # 1 + ((CV² + 1)·m − 1)·ICC (Eldridge, Ashby & Kerry 2006). Sem convidados
    # a rede é de uma pessoa só, e não há tamanho a variar. As UPAs são as
    # redes (as sementes): é delas que saem os gl.
    m <- 1 + k * ad
    dc <- .tr_sampling_deff_cv(m, if (k > 0) cv else 0, rho)
    nn <- pmin(x$N, x$n * m)
    t <- .tr_sampling_niveis(x$u, x$N, nn, x$grupos, p, conf, dc, distribuicao, upas = x$n)
    t <- tibble::add_column(t, convidados = k, adesao = ad, icc = rho, tamanho_rede = m, deff_agrupamento = dc,
                            .before = 1L)
    partes[[length(partes) + 1L]] <- t
  }
  do.call(rbind, partes)
}

#' Tamanho da amostra para a margem valer dentro de cada grupo, sem cotas.
#'
#' Sem cota, o grupo aparece na coleta na proporção em que existe (se a coleta
#' não o favorecer nem o espantar). Para ele ter os n de que precisa, o TOTAL
#' tem de ser n ÷ participação — e quem manda no tamanho da pesquisa é o menor
#' grupo que se quer ler.
#' @param composicao tabela longa: variável, grupo e participação na população.
#' @param variavel coluna com o nome da variável de perfil.
#' @param grupo coluna com a categoria.
#' @param participacao coluna com a participação do grupo (0 a 1).
#' @param erro margem de erro desejada dentro de cada grupo (0,05 = 5 pontos).
#' @param participacao_minima grupos abaixo disso ficam fora da conta (e a nota diz).
#' @inheritParams tr_sampling_margin
#' @return um plano (`sampling/plan`) com o n por grupo.
#' @export
tr_sampling_size_domains <- function(composicao, variavel = "", grupo = "", participacao = "",
                                     erro = 0.05, proporcao = 0.5, confianca = 0.95, deff = 1,
                                     taxa_resposta = 1, participacao_minima = 0, distribuicao = "t") {
  d <- tibble::as_tibble(composicao)
  cv <- .tr_sampling_col(d, variavel, "variavel")
  cg <- .tr_sampling_col(d, grupo, "grupo")
  cp <- .tr_sampling_col(d, participacao, "participacao"); .tr_sampling_numerica(d, cp, "participacao")
  cm <- .tr_sampling_comuns(confianca, 0, deff, taxa_resposta, distribuicao)
  E <- .tr_sampling_num(erro, "erro", 0, 1, aberto_min = TRUE)
  p <- .tr_sampling_num(proporcao, "proporcao", 0, 0.999, aberto_min = TRUE)
  pmin_ <- .tr_sampling_num(participacao_minima, "participacao_minima", 0, 1)
  sh <- as.numeric(d[[cp]])
  if (anyNA(sh) || any(sh <= 0) || any(sh > 1)) {
    .tr_sampling_abort("tr_sampling_error_bad_size",
                       "A coluna '%s' tem participação faltante, zero ou acima de 1.", cp)
  }
  somas <- tapply(sh, as.character(d[[cv]]), sum)
  if (any(abs(somas - 1) > 0.02)) {
    .tr_sampling_abort("tr_sampling_error_bad_option",
                       "As participações têm de somar 1 dentro de cada variável; não somam em: %s.",
                       paste(sprintf("%s (%s)", names(somas)[abs(somas - 1) > 0.02],
                                     .tr_sampling_fmt(somas[abs(somas - 1) > 0.02])), collapse = ", "))
  }
  # n por grupo com t de ng − 1 gl (o domínio lido sozinho), por iteração.
  n0_de_q <- function(q) q^2 * p * (1 - p) / E^2
  sol <- .tr_sampling_resolver_t(cm$conf, cm$distribuicao,
                                 function(q) as.integer(ceiling(n0_de_q(q) * cm$deff - 1e-9)), function(n) n - 1)
  n0 <- n0_de_q(sol$q)
  ng <- sol$n
  usa <- sh >= pmin_
  if (!any(usa)) {
    .tr_sampling_abort("tr_sampling_error_bad_option", "Nenhum grupo passa da participação mínima (%s).", pmin_)
  }
  total_g <- ceiling(ng / sh - 1e-9)
  i_lim <- which(usa)[which.max(total_g[usa])]
  n_total <- max(total_g[usa])
  n_contatos <- ceiling(n_total / cm$resposta - 1e-9)
  rotulos <- paste0(d[[cv]], ": ", d[[cg]])
  passos <- list(
    c(sprintf("n por grupo para ± %s pontos%s", .tr_sampling_fmt(100 * E), .tr_sampling_rotulo_q(sol$gl)), n0,
      "entrevistas"))
  if (cm$deff != 1) passos[[length(passos) + 1L]] <- c(sprintf("× deff %s", .tr_sampling_fmt(cm$deff)), ng, "entrevistas")
  passos[[length(passos) + 1L]] <- c(sprintf("÷ participação do grupo limitante, %s (%s)", rotulos[[i_lim]],
                                             .tr_sampling_pct(sh[[i_lim]])), n_total, "entrevistas")
  if (cm$resposta < 1) passos[[length(passos) + 1L]] <- c(sprintf("÷ taxa de resposta (%s)", .tr_sampling_pct(cm$resposta)),
                                                         n_contatos, "entrevistas")
  passos[[length(passos) + 1L]] <- c("n total", n_contatos, "entrevistas")
  esperado <- floor(n_total * sh)
  al <- tibble::tibble(estrato = rotulos, variavel = as.character(d[[cv]]), grupo = as.character(d[[cg]]),
                       participacao = sh, N = NA_real_, n = ng, n_total_necessario = total_g,
                       n_final = as.integer(esperado),
                       margem_esperada_pp = 100 * .tr_sampling_margem(pmax(esperado, 1), p, cm$conf, cm$deff,
                                                                        gl = if (cm$distribuicao == "z") Inf else pmax(esperado, 1) - 1),
                       fracao_amostral = NA_real_, considerado = usa, limitante = seq_along(sh) == i_lim)
  fora <- rotulos[!usa]
  .tr_sampling_plano("domínios", "Tamanho · grupos sem cota", n_contatos,
                     tibble::tibble(passo = vapply(passos, `[[`, "", 1L),
                                    valor = as.numeric(vapply(passos, `[[`, "", 2L)),
                                    unidade = vapply(passos, `[[`, "", 3L)),
                     cm$conf, E, parametros = list(medida = "proporção", p = p, deff = cm$deff, resposta = cm$resposta,
                                                   N = 0, n0 = n0, q = sol$q, gl = sol$gl,
                                                   distribuicao = cm$distribuicao),
                     alocacao = al,
                     nota = .tr_sampling_nota(
                       sprintf("limitante: %s; com n = %s, o maior grupo passa de %s entrevistas", rotulos[[i_lim]],
                               .tr_sampling_fmt(n_total), .tr_sampling_fmt(max(esperado))),
                       if (length(fora)) sprintf("fora da conta (abaixo de %s): %s", .tr_sampling_pct(pmin_),
                                                 paste(fora, collapse = ", ")) else "",
                       "supõe que a coleta traga cada grupo na proporção da população"))
}

#' A menor diferença δ detectável a partir de `pa`, num sentido.
#'
#' Fleiss, Levin & Paik (2003, cap. 4), duas proporções independentes com n
#' desiguais, sem correção de continuidade: a diferença p_b − p_a = δ é
#' detectável com nível α e poder 1 − β quando
#'
#'   δ = q_α·√(deff·p̄q̄·(c_a/n_a + c_b/n_b)) + q_β·√(deff·(p_a q_a c_a/n_a + p_b q_b c_b/n_b)),
#'
#' com p̄ = (n_a p_a + n_b p_b)/(n_a + n_b) e c = 1 − n/N a correção finita
#' (1 sem população). Com n iguais, deff = 1 e sem correção é a equação de
#' `stats::power.prop.test`. `sentido` −1 procura p_b abaixo de p_a.
#' @return δ (Inf se nem p_b no extremo é detectável).
#' @noRd
.tr_sampling_dmd <- function(pa, na, nb, ca, cb, qa, qb, deff, sentido = 1) {
  h <- function(d) {
    pb <- pa + sentido * d
    pbar <- (na * pa + nb * pb) / (na + nb)
    d - qa * sqrt(deff * pbar * (1 - pbar) * (ca / na + cb / nb)) -
      qb * sqrt(deff * (pa * (1 - pa) * ca / na + pb * (1 - pb) * cb / nb))
  }
  lim <- if (sentido > 0) 1 - pa else pa
  if (lim <= 0 || h(lim) < 0) return(Inf)
  stats::uniroot(h, c(0, lim), tol = 1e-12)$root
}

#' Diferença mínima detectável entre grupos de um mesmo recorte.
#'
#' Duas margens de ±10 pontos não dizem se dá para comparar os grupos: a
#' pergunta de comparação é "qual a MENOR diferença entre os dois que a pesquisa
#' conseguiria detectar?". A conta é a de duas proporções de Fleiss, Levin &
#' Paik (2003), com a proporção de referência de cada grupo e, se a população
#' do grupo é dada, a correção finita. A tabela traz o pior caso: as duas
#' referências (a e b) e os dois sentidos (acima e abaixo).
#'
#' Se a diferença que importa (10 pontos, por padrão) é menor que a DMD, a
#' comparação não se sustenta: um "não há diferença" seria falta de amostra.
#' @param grupos tabela longa com variável, grupo e tamanho (contagem ou participação).
#' @param variavel,grupo colunas da variável e da categoria.
#' @param tamanho coluna com o n do grupo, ou a participação se `n_total` > 0.
#' @param n_total total da amostra (0: `tamanho` já é contagem).
#' @param poder `"80%"` ou `"90%"`.
#' @param diferenca_relevante diferença que importa detectar (0,10 = 10 pontos).
#' @param proporcao proporção de referência comum (0,5 é o pior caso).
#' @param proporcao_grupo coluna com a proporção de referência de cada grupo
#'   (em branco: `proporcao` em todos).
#' @param populacao coluna com a população de cada grupo, para a correção
#'   finita (em branco: sem correção).
#' @param distribuicao `"t"` (gl = n_a + n_b − 2) ou `"z"`.
#' @inheritParams tr_sampling_margin
#' @return tabela (`data/table`) com uma linha por par de grupos.
#' @export
tr_sampling_detectable_difference <- function(grupos, variavel = "", grupo = "", tamanho = "", n_total = 0,
                                              proporcao = 0.5, confianca = 0.95, poder = "80%", deff = 1,
                                              diferenca_relevante = 0.10, proporcao_grupo = "",
                                              populacao = "", distribuicao = "t") {
  d <- tibble::as_tibble(grupos)
  cv <- .tr_sampling_col(d, variavel, "variavel")
  cg <- .tr_sampling_col(d, grupo, "grupo")
  ct <- .tr_sampling_col(d, tamanho, "tamanho"); .tr_sampling_numerica(d, ct, "tamanho")
  cp <- .tr_sampling_col_opcional(d, proporcao_grupo, "proporcao_grupo")
  cN <- .tr_sampling_col_opcional(d, populacao, "populacao")
  conf <- .tr_sampling_conf(confianca)
  pod <- .tr_sampling_enum(poder, c("80%", "90%"), "poder")
  pod <- as.numeric(sub("%", "", pod, fixed = TRUE)) / 100
  deff <- .tr_sampling_num(deff, "deff", 0, aberto_min = TRUE)
  p <- .tr_sampling_num(proporcao, "proporcao", 0, 0.999, aberto_min = TRUE)
  nt <- .tr_sampling_num(n_total, "n_total", 0)
  rel <- .tr_sampling_num(diferenca_relevante, "diferenca_relevante", 0, 1, aberto_min = TRUE)
  distribuicao <- .tr_sampling_distrib(distribuicao)
  tam <- as.numeric(d[[ct]])
  ns <- if (nt > 0) tam * nt else tam
  if (anyNA(ns) || any(ns < 0)) {
    .tr_sampling_abort("tr_sampling_error_bad_size", "A coluna '%s' tem tamanho faltante ou negativo.", ct)
  }
  ps <- rep(p, nrow(d))
  if (!is.null(cp)) {
    .tr_sampling_numerica(d, cp, "proporcao_grupo")
    ps <- as.numeric(d[[cp]])
    if (anyNA(ps) || any(ps <= 0 | ps >= 1)) {
      .tr_sampling_abort("tr_sampling_error_bad_size",
                         "A coluna '%s' tem proporção faltante ou fora de (0, 1).", cp)
    }
  }
  cs <- rep(1, nrow(d))
  if (!is.null(cN)) {
    .tr_sampling_numerica(d, cN, "populacao")
    Ng <- as.numeric(d[[cN]])
    if (anyNA(Ng) || any(Ng <= 0) || any(ns > Ng)) {
      .tr_sampling_abort("tr_sampling_error_bad_size",
                         "A coluna '%s' tem população faltante, zero ou menor que o n do grupo.", cN)
    }
    cs <- 1 - ns / Ng
  }
  vars <- as.character(d[[cv]]); gs <- as.character(d[[cg]])
  partes <- list()
  for (v in unique(vars)) {
    idx <- which(vars == v)
    if (length(idx) < 2L) next
    pares <- utils::combn(idx, 2L)
    for (j in seq_len(ncol(pares))) {
      a <- pares[1, j]; b <- pares[2, j]
      gl <- if (distribuicao == "z") Inf else max(1, ns[a] + ns[b] - 2)
      qa <- .tr_sampling_q(conf, gl)
      qb <- if (is.finite(gl)) stats::qt(pod, gl) else stats::qnorm(pod)
      dmd <- if (ns[a] > 0 && ns[b] > 0) {
        # O sentido que não cabe em (0, 1) (acima de 0,95 não há +10 pontos) não
        # entra no pior caso.
        ds <- c(.tr_sampling_dmd(ps[a], ns[a], ns[b], cs[a], cs[b], qa, qb, deff, 1),
            .tr_sampling_dmd(ps[a], ns[a], ns[b], cs[a], cs[b], qa, qb, deff, -1),
            .tr_sampling_dmd(ps[b], ns[b], ns[a], cs[b], cs[a], qa, qb, deff, 1),
            .tr_sampling_dmd(ps[b], ns[b], ns[a], cs[b], cs[a], qa, qb, deff, -1))
        if (any(is.finite(ds))) max(ds[is.finite(ds)]) else Inf
      } else Inf
      partes[[length(partes) + 1L]] <- tibble::tibble(
        variavel = v, grupo_a = gs[a], grupo_b = gs[b], n_a = ns[a], n_b = ns[b],
        p_a = ps[a], p_b = ps[b], gl = gl,
        # Acima de 100 pontos nenhuma diferença é detectável; o número maior que
        # isso só confundiria a leitura da tabela.
        diferenca_detectavel_pp = 100 * min(dmd, 1), sustentavel = dmd <= rel)
    }
  }
  if (!length(partes)) {
    .tr_sampling_abort("tr_sampling_error_too_few", "Nenhuma variável tem dois grupos para comparar.")
  }
  do.call(rbind, partes)
}

#' Gráfico das margens por nível, com a meta.
#' @param margens tabela de `sampling/margin_levels` ou `sampling/referral`.
#' @param meta margem máxima aceitável, em pontos percentuais.
#' @param aspecto,tema,titulo,rotulo_x,rotulo_y,legenda cosméticos (ver `trama.view`).
#' @return um ggplot (`view/plot`).
#' @export
tr_sampling_plot_margins <- function(margens, meta = 5, aspecto = "16:9", tema = "padrão", titulo = "",
                                     rotulo_x = "", rotulo_y = "", legenda = "direita") {
  d <- as.data.frame(margens)
  falta <- setdiff(c("nivel_tipo", "nivel", "margem_pp"), names(d))
  if (length(falta)) {
    .tr_sampling_abort("tr_sampling_error_unknown_column",
                       "A tabela precisa das colunas de 'sampling/margin_levels' (faltam: %s).", paste(falta, collapse = ", "))
  }
  meta <- .tr_sampling_num(meta, "meta", 0, 100)
  tipos <- unique(d$nivel_tipo)
  d$nivel_tipo <- factor(d$nivel_tipo, levels = c("total", setdiff(tipos, c("total", "unidade")),
                                                  intersect("unidade", tipos)))
  ordem <- stats::aggregate(margem_pp ~ nivel, d, max)
  d$nivel <- factor(d$nivel, levels = ordem$nivel[order(-ordem$margem_pp)])
  cenario <- "convidados" %in% names(d)
  if (cenario) {
    d$cenario <- factor(sprintf("%s convidado(s)", .tr_sampling_fmt(d$convidados)),
                        levels = sprintf("%s convidado(s)", .tr_sampling_fmt(sort(unique(d$convidados)))))
    d$icc_rot <- paste("ICC", .tr_sampling_fmt(d$icc))
  } else {
    d$cenario <- factor("previsto")
  }
  # Deitado: os nomes das unidades (27 capitais) só são legíveis no eixo y, e
  # o tema da `view` desfaz rotação de rótulo do eixo x. Uma linha de painéis
  # por tipo de nível, uma coluna por ICC.
  varios_icc <- cenario && length(unique(d$icc)) > 1L
  p <- ggplot2::ggplot(d, ggplot2::aes(y = .data[["nivel"]], x = .data[["margem_pp"]], colour = .data[["cenario"]])) +
    ggplot2::geom_vline(xintercept = meta, linetype = "dashed", linewidth = .6) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_grid(if (varios_icc) nivel_tipo ~ icc_rot else nivel_tipo ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(y = NULL, x = "margem de erro (pontos percentuais)", colour = NULL)
  if (!cenario) p <- p + ggplot2::guides(colour = "none")
  # Rótulo da faixa DEPOIS do tema da `view`, que o giraria de volta: na
  # vertical, "total" numa faixa de uma linha só sai cortado.
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda) +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, hjust = 0))
}

.TR_SAMPLING_TIPOS_PERGUNTA <- c("binária", "única", "múltipla", "escala", "numérica", "aberta")

#' Margem de erro de pior caso para cada pergunta do questionário, em cada nível.
#'
#' Cada tipo de pergunta tem o seu pior caso:
#'
#' - **binária** (sim/não) e **múltipla** (marque todas; cada opção é um sim/não
#'   independente): p = 0,5, margem z·√(deff·0,25/n).
#' - **única** (uma alternativa entre k): cada alternativa isolada tem a mesma
#'   margem da binária; mas quem lê a DISTRIBUIÇÃO inteira quer que todas as k
#'   proporções fiquem dentro da margem ao mesmo tempo. Essa margem simultânea é
#'   a de Thompson (1987): max sobre m ≤ k de z_{α/(2m)}·√((1/m)(1 − 1/m)/n).
#' - **diferença entre alternativas** (única, escala, múltipla): A contra B tem
#'   variância de pior caso 1/n, o dobro da margem de uma proporção; todos os
#'   pares de uma vez, com Bonferroni sobre k(k − 1)/2.
#' - **escala** (Likert de k pontos): cada ponto como alternativa, e a MÉDIA da
#'   escala, cujo pior desvio é (k − 1)/2 (metade no mínimo, metade no máximo).
#' - **numérica**: o pior caso depende da amplitude; a tabela dá a margem das
#'   faixas, se categorizada, como na única.
#' - **aberta**: sem margem (vira categoria só depois de codificada).
#'
#' A **base** é a fração da amostra que responde a pergunta: 1 nas que todos
#' respondem, menos nas condicionais ("se trabalha…") e descontada a não
#' resposta esperada. É ela que faz uma pergunta de filtro ter margem muito
#' pior que a do questionário.
#' @param perguntas tabela com uma linha por pergunta.
#' @param margens tabela de níveis (de `sampling/margin_levels` ou `sampling/referral`).
#' @param pergunta,tipo,opcoes,base colunas de `perguntas`.
#' @param nao_resposta fração esperada de "prefiro não responder" em toda pergunta fechada.
#' @inheritParams tr_sampling_margin
#' @return tabela (`data/table`): uma linha por pergunta e nível.
#' @export
tr_sampling_question_margins <- function(perguntas, margens, pergunta = "pergunta", tipo = "tipo",
                                         opcoes = "opcoes", base = "base", nao_resposta = 0,
                                         confianca = 0.95, distribuicao = "t") {
  distribuicao <- .tr_sampling_distrib(distribuicao)
  q <- tibble::as_tibble(perguntas)
  m <- tibble::as_tibble(margens)
  cq <- .tr_sampling_col(q, pergunta, "pergunta")
  ct <- .tr_sampling_col(q, tipo, "tipo")
  co <- .tr_sampling_col_opcional(q, opcoes, "opcoes")
  cb <- .tr_sampling_col_opcional(q, base, "base")
  faltam <- setdiff(c("nivel", "n", "deff"), names(m))
  if (length(faltam)) {
    .tr_sampling_abort("tr_sampling_error_unknown_column",
                       "A tabela de margens precisa das colunas de 'sampling/margin_levels' (faltam: %s).",
                       paste(faltam, collapse = ", "))
  }
  conf <- .tr_sampling_conf(confianca)
  nr <- .tr_sampling_num(nao_resposta, "nao_resposta", 0, 0.9)
  tipos <- as.character(q[[ct]])
  ruins <- setdiff(unique(tipos), .TR_SAMPLING_TIPOS_PERGUNTA)
  if (length(ruins)) {
    .tr_sampling_abort("tr_sampling_error_bad_option",
                       "Coluna '%s': tipo desconhecido: %s. Aceitos: %s.", ct, paste(ruins, collapse = ", "),
                       paste(.TR_SAMPLING_TIPOS_PERGUNTA, collapse = ", "))
  }
  k <- if (is.null(co)) rep(NA_real_, nrow(q)) else suppressWarnings(as.numeric(q[[co]]))
  b <- if (is.null(cb)) rep(1, nrow(q)) else suppressWarnings(as.numeric(q[[cb]]))
  b[is.na(b)] <- 1
  if (any(b <= 0 | b > 1)) {
    .tr_sampling_abort("tr_sampling_error_bad_size", "A coluna '%s' tem base fora de (0, 1].", cb)
  }
  alfa <- 1 - conf
  cenarios <- intersect(c("convidados", "adesao", "icc"), names(m))
  partes <- lapply(seq_len(nrow(m)), function(i) {
    fechada <- tipos != "aberta"
    nq <- m$n[[i]] * b * ifelse(fechada, 1 - nr, 1)
    de <- m$deff[[i]]
    # gl: os do nível (coluna `gl` de `sampling/margin_levels`) — o domínio da
    # pergunta não corta o desenho —, ou n − 1 numa tabela sem ela.
    gl <- if (distribuicao == "z") Inf else if ("gl" %in% names(m)) m$gl[[i]] else max(1, m$n[[i]] - 1)
    qq <- function(pr) if (is.finite(gl)) stats::qt(pr, gl) else stats::qnorm(pr)
    z <- qq(1 - alfa / 2)
    ind <- ifelse(fechada, 100 * z * sqrt(de * 0.25 / nq), NA_real_)
    simul <- vapply(seq_along(tipos), function(j) {
      if (!tipos[[j]] %in% c("única", "escala") || is.na(k[[j]]) || k[[j]] < 3) return(NA_real_)
      ms <- 2:k[[j]]
      100 * max(qq(1 - alfa / (2 * ms)) * sqrt(de * (1 / ms) * (1 - 1 / ms) / nq[[j]]))
    }, 0)
    media <- ifelse(tipos == "escala" & !is.na(k), z * sqrt(de) * ((k - 1) / 2) / sqrt(nq), NA_real_)
    # Uma alternativa CONTRA outra da mesma pergunta. As duas vêm das mesmas
    # pessoas e competem pelo mesmo voto (covariância −p_A·p_B/n), e numa
    # diferença a covariância negativa SOMA: Var(p̂_A − p̂_B) = [p_A + p_B −
    # (p_A − p_B)²]/n, cujo pior caso (0,5 e 0,5) é 1/n — o dobro da margem de
    # uma proporção. Na múltipla o pior caso é o mesmo (opções nunca marcadas
    # juntas). Todos os pares de uma vez pedem Bonferroni sobre k(k − 1)/2.
    compara <- tipos %in% c("única", "escala", "múltipla") & !is.na(k) & k >= 2
    dif <- ifelse(compara, 100 * z * sqrt(de / nq), NA_real_)
    pares <- ifelse(compara, k * (k - 1) / 2, NA_real_)
    todos <- ifelse(compara & k >= 3, 100 * qq(1 - alfa / (2 * pares)) * sqrt(de / nq), NA_real_)
    t <- tibble::tibble(pergunta = as.character(q[[cq]]), tipo = tipos, opcoes = k, base = b,
                        nivel_tipo = if ("nivel_tipo" %in% names(m)) m$nivel_tipo[[i]] else NA_character_,
                        nivel = m$nivel[[i]], n_respondentes = nq, deff = de,
                        margem_pp = ind, margem_simultanea_pp = simul,
                        margem_diferenca_pp = dif, margem_todos_pares_pp = todos, margem_media_escala = media)
    for (cn in rev(cenarios)) t <- tibble::add_column(t, !!!stats::setNames(list(m[[cn]][[i]]), cn), .before = 1L)
    t
  })
  out <- do.call(rbind, partes)
  # Um nível com n de sobra em alguma pergunta e uma pergunta de filtro com 3
  # respondentes podem sair da mesma tabela: abaixo de 30 a aproximação normal
  # já não vale, e a coluna diz isso em vez de publicar um ± enganoso.
  out$confiavel <- out$tipo == "aberta" | out$n_respondentes >= 30
  out
}
