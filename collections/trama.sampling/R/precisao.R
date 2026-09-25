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

#' A margem de uma proporção: z · √(deff · p(1 − p) / n · (1 − n/N)).
#' @noRd
.tr_sampling_margem <- function(n, p, conf, deff = 1, N = 0) {
  fpc <- ifelse(N > 0, pmax(0, 1 - n / N), 1)
  .tr_sampling_z(conf) * sqrt(deff * p * (1 - p) / n * fpc)
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
#' @return um plano (`sampling/plan`) do tipo margem.
#' @export
tr_sampling_margin <- function(n = 400L, proporcao = 0.5, confianca = 0.95, populacao = 0,
                               deff = 1, taxa_resposta = 1) {
  cm <- .tr_sampling_comuns(confianca, populacao, deff, taxa_resposta)
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
  E <- .tr_sampling_margem(nr, p, cm$conf, cm$deff, cm$N)
  if (cm$N > 0) passos[[length(passos) + 1L]] <- c(sprintf("população finita (N = %s)", .tr_sampling_fmt(cm$N)), ne, "entrevistas")
  passos[[length(passos) + 1L]] <- c("margem de erro (pontos percentuais)", 100 * E, "pontos")
  .tr_sampling_plano("margem", "Margem · proporção", nr,
                     tibble::tibble(passo = vapply(passos, `[[`, "", 1L),
                                    valor = as.numeric(vapply(passos, `[[`, "", 2L)),
                                    unidade = vapply(passos, `[[`, "", 3L)),
                     cm$conf, E,
                     parametros = list(medida = "proporção", p = p, N = cm$N, deff = cm$deff,
                                       resposta = cm$resposta, n0 = ne),
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
.tr_sampling_niveis <- function(u, N, n, grupos, p, conf, deff) {
  linha <- function(tipo, nome, idx) {
    Nh <- N[idx]; nh <- n[idx]
    W <- Nh / sum(Nh)
    v <- sum(W^2 * pmax(0, 1 - nh / Nh) * p * (1 - p) / nh)
    kish <- sum(nh) * sum(W^2 / nh)
    # Calculado ANTES do tibble: dentro dele, a coluna `deff` recém-criada
    # sombrearia o argumento, e a margem sairia multiplicada pelo Kish de novo.
    E <- .tr_sampling_z(conf) * sqrt(deff * v)
    tibble::tibble(nivel_tipo = tipo, nivel = nome, unidades = length(idx), populacao = sum(Nh), n = sum(nh),
                   deff_ponderacao = kish, deff = kish * deff, n_efetivo = sum(nh) / (kish * deff),
                   margem = E, margem_pp = 100 * E)
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
                                      proporcao = 0.5, confianca = 0.95) {
  x <- .tr_sampling_unidades(unidades, unidade, tamanho, n, grupos)
  conf <- .tr_sampling_conf(confianca)
  deff <- .tr_sampling_num(deff, "deff", 0, aberto_min = TRUE)
  p <- .tr_sampling_num(proporcao, "proporcao", 0, 0.999, aberto_min = TRUE)
  .tr_sampling_niveis(x$u, x$N, x$n, x$grupos, p, conf, deff)
}

#' Margem de erro na expansão por indicação, por cenário.
#' @inheritParams tr_sampling_margin_levels
#' @param convidados lista de quantos convidados por participante (ex.: `"0, 1, 2, 3"`).
#' @param adesao fração dos convidados que de fato responde.
#' @param icc lista de correlações intraclasse entre quem convida e os convidados.
#' @return tabela (`data/table`) com uma linha por cenário e nível.
#' @export
tr_sampling_referral <- function(unidades, unidade = "", tamanho = "", n = "", grupos = "",
                                 convidados = "0, 1, 2, 3, 5", adesao = 1, icc = "0.05, 0.1, 0.2",
                                 proporcao = 0.5, confianca = 0.95) {
  x <- .tr_sampling_unidades(unidades, unidade, tamanho, n, grupos)
  conf <- .tr_sampling_conf(confianca)
  p <- .tr_sampling_num(proporcao, "proporcao", 0, 0.999, aberto_min = TRUE)
  ks <- .tr_sampling_lista_num(convidados, "convidados", 0, 1000)
  rhos <- .tr_sampling_lista_num(icc, "icc", 0, 1)
  ad <- .tr_sampling_num(adesao, "adesao", 0, 1, aberto_min = TRUE)
  partes <- list()
  for (rho in rhos) for (k in ks) {
    # Cada participante da base vira uma "rede" de 1 + k·adesão respostas. As
    # respostas da mesma rede se parecem (amigos, vizinhos): o custo é o deff
    # de conglomerado, 1 + (m − 1)·ICC.
    m <- 1 + k * ad
    dc <- 1 + (m - 1) * rho
    nn <- pmin(x$N, x$n * m)
    t <- .tr_sampling_niveis(x$u, x$N, nn, x$grupos, p, conf, dc)
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
                                     taxa_resposta = 1, participacao_minima = 0) {
  d <- tibble::as_tibble(composicao)
  cv <- .tr_sampling_col(d, variavel, "variavel")
  cg <- .tr_sampling_col(d, grupo, "grupo")
  cp <- .tr_sampling_col(d, participacao, "participacao"); .tr_sampling_numerica(d, cp, "participacao")
  cm <- .tr_sampling_comuns(confianca, 0, deff, taxa_resposta)
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
  n0 <- .tr_sampling_z(cm$conf)^2 * p * (1 - p) / E^2
  ng <- ceiling(n0 * cm$deff - 1e-9)
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
    c(sprintf("n por grupo para ± %s pontos", .tr_sampling_fmt(100 * E)), n0, "entrevistas"))
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
                       margem_esperada_pp = 100 * .tr_sampling_margem(pmax(esperado, 1), p, cm$conf, cm$deff),
                       fracao_amostral = NA_real_, considerado = usa, limitante = seq_along(sh) == i_lim)
  fora <- rotulos[!usa]
  .tr_sampling_plano("domínios", "Tamanho · grupos sem cota", n_contatos,
                     tibble::tibble(passo = vapply(passos, `[[`, "", 1L),
                                    valor = as.numeric(vapply(passos, `[[`, "", 2L)),
                                    unidade = vapply(passos, `[[`, "", 3L)),
                     cm$conf, E, parametros = list(medida = "proporção", p = p, deff = cm$deff, resposta = cm$resposta,
                                                   N = 0, n0 = n0),
                     alocacao = al,
                     nota = .tr_sampling_nota(
                       sprintf("limitante: %s; com n = %s, o maior grupo passa de %s entrevistas", rotulos[[i_lim]],
                               .tr_sampling_fmt(n_total), .tr_sampling_fmt(max(esperado))),
                       if (length(fora)) sprintf("fora da conta (abaixo de %s): %s", .tr_sampling_pct(pmin_),
                                                 paste(fora, collapse = ", ")) else "",
                       "supõe que a coleta traga cada grupo na proporção da população"))
}

#' Diferença mínima detectável entre grupos de um mesmo recorte.
#'
#' Duas margens de ±10 pontos não dizem se dá para comparar os grupos: a
#' pergunta de comparação é "qual a MENOR diferença entre os dois que a pesquisa
#' conseguiria detectar?". Para duas proporções independentes, com nível α e
#' poder 1 − β:
#'
#'   DMD = (z_{1−α/2} + z_{1−β}) · √(deff · p(1 − p) · (1/n_a + 1/n_b))
#'
#' Se a diferença que importa (10 pontos, por padrão) é menor que a DMD, a
#' comparação não se sustenta: um "não há diferença" seria falta de amostra.
#' @param grupos tabela longa com variável, grupo e tamanho (contagem ou participação).
#' @param variavel,grupo colunas da variável e da categoria.
#' @param tamanho coluna com o n do grupo, ou a participação se `n_total` > 0.
#' @param n_total total da amostra (0: `tamanho` já é contagem).
#' @param poder `"80%"` ou `"90%"`.
#' @param diferenca_relevante diferença que importa detectar (0,10 = 10 pontos).
#' @inheritParams tr_sampling_margin
#' @return tabela (`data/table`) com uma linha por par de grupos.
#' @export
tr_sampling_detectable_difference <- function(grupos, variavel = "", grupo = "", tamanho = "", n_total = 0,
                                              proporcao = 0.5, confianca = 0.95, poder = "80%", deff = 1,
                                              diferenca_relevante = 0.10) {
  d <- tibble::as_tibble(grupos)
  cv <- .tr_sampling_col(d, variavel, "variavel")
  cg <- .tr_sampling_col(d, grupo, "grupo")
  ct <- .tr_sampling_col(d, tamanho, "tamanho"); .tr_sampling_numerica(d, ct, "tamanho")
  conf <- .tr_sampling_conf(confianca)
  pod <- .tr_sampling_enum(poder, c("80%", "90%"), "poder")
  pod <- as.numeric(sub("%", "", pod, fixed = TRUE)) / 100
  deff <- .tr_sampling_num(deff, "deff", 0, aberto_min = TRUE)
  p <- .tr_sampling_num(proporcao, "proporcao", 0, 0.999, aberto_min = TRUE)
  nt <- .tr_sampling_num(n_total, "n_total", 0)
  rel <- .tr_sampling_num(diferenca_relevante, "diferenca_relevante", 0, 1, aberto_min = TRUE)
  tam <- as.numeric(d[[ct]])
  ns <- if (nt > 0) tam * nt else tam
  if (anyNA(ns) || any(ns < 0)) {
    .tr_sampling_abort("tr_sampling_error_bad_size", "A coluna '%s' tem tamanho faltante ou negativo.", ct)
  }
  k <- .tr_sampling_z(conf) + stats::qnorm(pod)
  vars <- as.character(d[[cv]]); gs <- as.character(d[[cg]])
  partes <- list()
  for (v in unique(vars)) {
    idx <- which(vars == v)
    if (length(idx) < 2L) next
    pares <- utils::combn(idx, 2L)
    for (j in seq_len(ncol(pares))) {
      a <- pares[1, j]; b <- pares[2, j]
      dmd <- if (ns[a] > 0 && ns[b] > 0) k * sqrt(deff * p * (1 - p) * (1 / ns[a] + 1 / ns[b])) else Inf
      partes[[length(partes) + 1L]] <- tibble::tibble(
        variavel = v, grupo_a = gs[a], grupo_b = gs[b], n_a = ns[a], n_b = ns[b],
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
                                         confianca = 0.95) {
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
  z <- .tr_sampling_z(conf); alfa <- 1 - conf
  cenarios <- intersect(c("convidados", "adesao", "icc"), names(m))
  partes <- lapply(seq_len(nrow(m)), function(i) {
    fechada <- tipos != "aberta"
    nq <- m$n[[i]] * b * ifelse(fechada, 1 - nr, 1)
    de <- m$deff[[i]]
    ind <- ifelse(fechada, 100 * z * sqrt(de * 0.25 / nq), NA_real_)
    simul <- vapply(seq_along(tipos), function(j) {
      if (!tipos[[j]] %in% c("única", "escala") || is.na(k[[j]]) || k[[j]] < 3) return(NA_real_)
      ms <- 2:k[[j]]
      100 * max(stats::qnorm(1 - alfa / (2 * ms)) * sqrt(de * (1 / ms) * (1 - 1 / ms) / nq[[j]]))
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
    todos <- ifelse(compara & k >= 3, 100 * stats::qnorm(1 - alfa / (2 * pares)) * sqrt(de / nq), NA_real_)
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
