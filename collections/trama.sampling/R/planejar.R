# Os tamanhos de amostra.
#
# Todo cálculo sai como ESCADA de passos, e não só como o n final: "n = 522"
# não responde a pergunta de quem planeja, que é "por que tanto?". A escada
# responde — 384 pela fórmula, ×1,5 pelo desenho, 278 porque a população é
# pequena, ÷0,8 pela não resposta —, e diz qual degrau vale a pena discutir.

.TR_SAMPLING_CAMPOS_PLANO <- c("tipo", "rotulo", "n", "passos", "confianca", "erro", "erro_alcancado",
                               "parametros", "alocacao", "conglomerados", "tamanho_conglomerado", "nota")

#' Monta o plano.
#'
#' - `tipo`: `"média"`, `"proporção"`, `"estratificada"` ou `"conglomerados"` —
#'   é por ele que a seleção sabe se o plano serve.
#' - `passos`: tibble `passo`, `valor`, `unidade` (a escada).
#' - `parametros`: o que foi usado, para `sampling/size_cluster` e
#'   `sampling/size_curve` refazerem a conta com outro erro ou outro desenho.
#' - `alocacao`: tibble por estrato (só na estratificada).
#' @noRd
.tr_sampling_plano <- function(tipo, rotulo, n, passos, confianca, erro, erro_alcancado = NA_real_,
                               parametros = list(), alocacao = NULL, conglomerados = NA_integer_,
                               tamanho_conglomerado = NA_real_, nota = "") {
  structure(list(tipo = tipo, rotulo = rotulo, n = as.integer(n), passos = passos, confianca = confianca,
                 erro = erro, erro_alcancado = erro_alcancado, parametros = parametros, alocacao = alocacao,
                 conglomerados = as.integer(conglomerados), tamanho_conglomerado = tamanho_conglomerado,
                 nota = nota), class = "tr_sampling_plan")
}

#' Os ajustes comuns de um n de AAS: deff, população finita, não resposta.
#'
#' A ordem é a de Cochran: o deff multiplica o n₀ da população infinita, a
#' correção finita vem depois (ela depende de quanto da população a amostra
#' já cobre), e a não resposta por último — ela infla o número de CONTATOS, não
#' a informação.
#' @return lista com `passos` e `n` (inteiro, arredondado para cima).
#' @noRd
.tr_sampling_ajustes <- function(n0, deff, N, resposta, unidade = "unidades") {
  passos <- list(c("n₀ pela fórmula (população infinita)", n0))
  n <- n0
  if (deff != 1) {
    n <- n * deff
    passos[[length(passos) + 1L]] <- c(sprintf("× efeito do desenho (deff %s)", .tr_sampling_fmt(deff)), n)
  }
  if (N > 0) {
    n <- n / (1 + n / N)
    passos[[length(passos) + 1L]] <- c(sprintf("correção de população finita (N = %s)", .tr_sampling_fmt(N, 6)), n)
  }
  if (resposta < 1) {
    n <- n / resposta
    passos[[length(passos) + 1L]] <- c(sprintf("÷ taxa de resposta (%s)", .tr_sampling_pct(resposta)), n)
  }
  final <- ceiling(n - 1e-9)
  nota <- ""
  if (N > 0 && final > N) {
    final <- N
    nota <- "a não resposta levou o n acima da população: é censo"
  }
  passos[[length(passos) + 1L]] <- c("n final (arredondado para cima)", final)
  list(passos = tibble::tibble(passo = vapply(passos, `[[`, "", 1L),
                               valor = as.numeric(vapply(passos, `[[`, "", 2L)),
                               unidade = unidade),
       n = as.integer(final), nota = nota)
}

#' Os params comuns aos dois tamanhos simples.
#' @noRd
.tr_sampling_comuns <- function(confianca, populacao, deff, taxa_resposta) {
  list(conf = .tr_sampling_conf(confianca),
       N = .tr_sampling_num(populacao, "populacao", 0),
       deff = .tr_sampling_num(deff, "deff", 0, aberto_min = TRUE),
       resposta = .tr_sampling_num(taxa_resposta, "taxa_resposta", 0, 1, aberto_min = TRUE))
}

#' O n de AAS para um erro dado, com os ajustes do plano. É o que a curva e o
#' tamanho de conglomerados refazem.
#' @noRd
.tr_sampling_n_para_erro <- function(par, E, conf) {
  z <- .tr_sampling_z(conf)
  n0 <- if (par$medida == "média") (z * par$S / E)^2 else z^2 * par$p * (1 - par$p) / E^2
  .tr_sampling_ajustes(n0, par$deff, par$N, par$resposta)$n
}

#' Tamanho da amostra para estimar uma média.
#' @param piloto tabela de um piloto (opcional): o desvio e a média saem dela.
#' @param variavel coluna do piloto.
#' @param desvio_padrao desvio padrão da variável (sem piloto).
#' @param media média esperada (só para erro relativo, sem piloto).
#' @param erro margem de erro: na unidade da variável, ou % da média.
#' @param tipo_erro `"absoluto"` ou `"relativo"`.
#' @param confianca `"90%"`, `"95%"` ou `"99%"`.
#' @param populacao tamanho da população (0: infinita).
#' @param deff efeito do desenho esperado (1: AAS).
#' @param taxa_resposta proporção esperada de respondentes (0 a 1).
#' @return um plano (`sampling/plan`).
#' @export
tr_sampling_size_mean <- function(piloto = NULL, variavel = "", desvio_padrao = 10, media = 0, erro = 1,
                                  tipo_erro = "absoluto", confianca = "95%", populacao = 0,
                                  deff = 1, taxa_resposta = 1) {
  cm <- .tr_sampling_comuns(confianca, populacao, deff, taxa_resposta)
  tipo_erro <- .tr_sampling_enum(tipo_erro, c("absoluto", "relativo"), "tipo_erro")
  erro <- .tr_sampling_num(erro, "erro", 0, aberto_min = TRUE)
  nota <- ""
  if (!is.null(piloto)) {
    col <- .tr_sampling_col(piloto, variavel, "variavel")
    .tr_sampling_numerica(piloto, col, "variavel")
    v <- piloto[[col]][!is.na(piloto[[col]])]
    if (length(v) < 2L) {
      .tr_sampling_abort("tr_sampling_error_too_few",
                         "O piloto tem %d valor(es) em '%s'; o desvio pede pelo menos 2.", length(v), col)
    }
    S <- stats::sd(v); media <- mean(v)
    nota <- sprintf("desvio (%s) e média (%s) do piloto, n = %d", .tr_sampling_fmt(S), .tr_sampling_fmt(media), length(v))
  } else {
    S <- .tr_sampling_num(desvio_padrao, "desvio_padrao", 0, aberto_min = TRUE)
  }
  E <- erro
  if (tipo_erro == "relativo") {
    if (!(is.numeric(media) && length(media) == 1L && !is.na(media) && media > 0)) {
      .tr_sampling_abort("tr_sampling_error_bad_option",
                         "Erro relativo é %% da média: preencha 'media' (> 0) ou ligue um piloto.")
    }
    E <- erro / 100 * media
  }
  n0 <- (.tr_sampling_z(cm$conf) * S / E)^2
  aj <- .tr_sampling_ajustes(n0, cm$deff, cm$N, cm$resposta)
  .tr_sampling_plano("média", "Tamanho · média", aj$n, aj$passos, cm$conf, E,
                     parametros = list(medida = "média", S = S, media = media, erro_informado = erro,
                                       tipo_erro = tipo_erro, N = cm$N, deff = cm$deff,
                                       resposta = cm$resposta, n0 = n0),
                     nota = .tr_sampling_nota(nota, aj$nota,
                                              if (tipo_erro == "relativo") sprintf("erro de %s%% da média = %s", .tr_sampling_fmt(erro), .tr_sampling_fmt(E)) else ""))
}

#' Tamanho da amostra para estimar uma proporção.
#' @inheritParams tr_sampling_size_mean
#' @param proporcao proporção esperada (0,5 é o pior caso).
#' @param erro margem de erro em pontos de proporção (0,05 = 5 pontos).
#' @return um plano (`sampling/plan`).
#' @export
tr_sampling_size_proportion <- function(proporcao = 0.5, erro = 0.05, confianca = "95%", populacao = 0,
                                        deff = 1, taxa_resposta = 1) {
  cm <- .tr_sampling_comuns(confianca, populacao, deff, taxa_resposta)
  p <- .tr_sampling_num(proporcao, "proporcao", 0, 1, aberto_min = TRUE)
  if (p >= 1) {
    .tr_sampling_abort("tr_sampling_error_bad_option",
                       "Param 'proporcao': tem de ficar entre 0 e 1, sem os extremos (veio %s).", p)
  }
  E <- .tr_sampling_num(erro, "erro", 0, 1, aberto_min = TRUE)
  n0 <- .tr_sampling_z(cm$conf)^2 * p * (1 - p) / E^2
  aj <- .tr_sampling_ajustes(n0, cm$deff, cm$N, cm$resposta)
  .tr_sampling_plano("proporção", "Tamanho · proporção", aj$n, aj$passos, cm$conf, E,
                     parametros = list(medida = "proporção", p = p, N = cm$N, deff = cm$deff,
                                       resposta = cm$resposta, n0 = n0),
                     nota = .tr_sampling_nota(if (p == 0.5) "p = 0,5: o pior caso, o n que serve para qualquer proporção" else "",
                                              aj$nota))
}

#' Tamanho e alocação de uma amostra estratificada.
#' @param estratos tabela com uma linha por estrato.
#' @param estrato coluna do nome do estrato.
#' @param tamanho coluna com N_h, as unidades do estrato na população.
#' @param desvio coluna com S_h, o desvio da variável no estrato (para
#'   proporção, raiz de p_h(1 − p_h)).
#' @param custo coluna com o custo por unidade (só na alocação ótima).
#' @param alocacao `"proporcional"`, `"neyman"`, `"ótima"` ou `"igual"`.
#' @param erro margem de erro da média geral (0: usa `n_total`).
#' @param n_total n fixo a alocar (vence `erro` quando > 0).
#' @inheritParams tr_sampling_size_mean
#' @return um plano (`sampling/plan`) com a alocação.
#' @export
tr_sampling_size_stratified <- function(estratos, estrato = "", tamanho = "", desvio = "", custo = "",
                                        alocacao = "neyman", erro = 0, n_total = 0L,
                                        confianca = "95%", taxa_resposta = 1) {
  e <- tibble::as_tibble(estratos)
  ce <- .tr_sampling_col(e, estrato, "estrato")
  cN <- .tr_sampling_col(e, tamanho, "tamanho"); .tr_sampling_numerica(e, cN, "tamanho")
  cS <- .tr_sampling_col(e, desvio, "desvio"); .tr_sampling_numerica(e, cS, "desvio")
  alocacao <- .tr_sampling_enum(alocacao, c("proporcional", "neyman", "ótima", "igual"), "alocacao")
  conf <- .tr_sampling_conf(confianca)
  resposta <- .tr_sampling_num(taxa_resposta, "taxa_resposta", 0, 1, aberto_min = TRUE)
  nomes <- as.character(e[[ce]])
  if (anyNA(nomes) || anyDuplicated(nomes)) {
    .tr_sampling_abort("tr_sampling_error_bad_option",
                       "Param 'estrato': a coluna '%s' tem de ter um nome por linha, sem repetição nem faltante.", ce)
  }
  if (length(nomes) < 2L) {
    .tr_sampling_abort("tr_sampling_error_too_few", "A tabela tem %d estrato(s); estratificar pede pelo menos 2.", length(nomes))
  }
  N <- as.numeric(e[[cN]]); S <- as.numeric(e[[cS]])
  if (anyNA(N) || any(N <= 0) || anyNA(S) || any(S < 0)) {
    .tr_sampling_abort("tr_sampling_error_bad_size",
                       "As colunas '%s' (N > 0) e '%s' (desvio >= 0) têm valor faltante ou fora da faixa.", cN, cS)
  }
  C <- rep(1, length(N))
  if (alocacao == "ótima") {
    cc <- .tr_sampling_col(e, custo, "custo"); .tr_sampling_numerica(e, cc, "custo")
    C <- as.numeric(e[[cc]])
    if (anyNA(C) || any(C <= 0)) {
      .tr_sampling_abort("tr_sampling_error_bad_size", "A coluna '%s' tem custo faltante, zero ou negativo.", cc)
    }
  }
  Nt <- sum(N); W <- N / Nt
  a <- switch(alocacao, proporcional = W, neyman = W * S, `ótima` = W * S / sqrt(C), igual = rep(1, length(N)))
  a <- if (sum(a) > 0) a / sum(a) else W
  z <- .tr_sampling_z(conf)
  passos <- list()
  if (length(n_total) == 1L && !is.na(n_total) && n_total > 0) {
    n <- as.numeric(round(n_total)); E <- NA_real_
    passos[[1]] <- c("n total informado", n)
  } else if (length(erro) == 1L && !is.na(erro) && erro > 0) {
    E <- erro
    # Var(ȳ_st) = Σ W²S²/n_h − Σ W S²/N, com n_h = a_h·n, igualada a (E/z)².
    n <- sum(W^2 * S^2 / a) / ((E / z)^2 + sum(W * S^2) / Nt)
    passos[[1]] <- c(sprintf("n pela fórmula da alocação %s", alocacao), n)
  } else {
    .tr_sampling_abort("tr_sampling_error_blank_param",
                       "'sampling/size_stratified': preencha 'erro' (a margem da média) ou 'n_total'.")
  }
  nh <- .tr_sampling_alocar(N, S, C, as.integer(ceiling(n - 1e-9)), alocacao)
  passos[[length(passos) + 1L]] <- c("alocado nos estratos (inteiros, mínimo 2, até N_h)", sum(nh))
  f <- nh / N
  alcancado <- z * sqrt(sum(W^2 * (1 - f) * S^2 / nh))
  n_final <- pmin(N, ceiling(nh / resposta - 1e-9))
  if (resposta < 1) passos[[length(passos) + 1L]] <- c(sprintf("÷ taxa de resposta (%s)", .tr_sampling_pct(resposta)), sum(n_final))
  passos[[length(passos) + 1L]] <- c("n final", sum(n_final))
  al <- tibble::tibble(estrato = nomes, N = N, desvio = S, custo = C, peso_estrato = W,
                       fracao_alocada = nh / sum(nh), n = nh, n_final = as.integer(n_final),
                       fracao_amostral = n_final / N)
  .tr_sampling_plano("estratificada", sprintf("Tamanho · estratificada · %s", alocacao), sum(n_final),
                     tibble::tibble(passo = vapply(passos, `[[`, "", 1L),
                                    valor = as.numeric(vapply(passos, `[[`, "", 2L)), unidade = "unidades"),
                     conf, E, erro_alcancado = alcancado,
                     parametros = list(medida = "média", alocacao = alocacao, resposta = resposta, N = Nt),
                     alocacao = al,
                     nota = .tr_sampling_nota(
                       if (any(nh >= N)) sprintf("censo em: %s", paste(nomes[nh >= N], collapse = ", ")) else "",
                       sprintf("margem alcançada com a alocação inteira: %s", .tr_sampling_fmt(alcancado))))
}

#' Tamanho de uma amostra de conglomerados, pelo ICC.
#' @param plano plano de `sampling/size_mean` ou `sampling/size_proportion`.
#' @param tamanho_conglomerado unidades por conglomerado (média, m̄).
#' @param icc correlação intraclasse esperada (0 a 1).
#' @param conglomerados conglomerados na população (0: infinitos).
#' @return um plano (`sampling/plan`) de conglomerados.
#' @export
tr_sampling_size_cluster <- function(plano, tamanho_conglomerado = 20, icc = 0.05, conglomerados = 0) {
  .tr_sampling_plano_do_tipo(plano, c("média", "proporção"), "sampling/size_cluster",
                             "'sampling/stratified' — o plano estratificado já aloca por unidades")
  m <- .tr_sampling_num(tamanho_conglomerado, "tamanho_conglomerado", 1)
  rho <- .tr_sampling_num(icc, "icc", 0, 1)
  M <- .tr_sampling_num(conglomerados, "conglomerados", 0)
  par <- plano$parametros
  deff <- 1 + (m - 1) * rho
  passos <- list(c("n₀ da AAS (população infinita)", par$n0, "unidades"))
  n1 <- par$n0 * deff
  passos[[2]] <- c(sprintf("× deff = 1 + (m̄ − 1)·ICC = %s", .tr_sampling_fmt(deff)), n1, "unidades")
  cg <- n1 / m
  passos[[3]] <- c(sprintf("÷ %s unidades por conglomerado", .tr_sampling_fmt(m)), cg, "conglomerados")
  if (M > 0) {
    cg <- cg / (1 + cg / M)
    passos[[length(passos) + 1L]] <- c(sprintf("correção finita (M = %s)", .tr_sampling_fmt(M, 6)), cg, "conglomerados")
  }
  if (par$resposta < 1) {
    cg <- cg / par$resposta
    passos[[length(passos) + 1L]] <- c(sprintf("÷ taxa de resposta (%s)", .tr_sampling_pct(par$resposta)), cg, "conglomerados")
  }
  final <- max(2, ceiling(cg - 1e-9))
  if (M > 0) final <- min(final, M)
  passos[[length(passos) + 1L]] <- c("conglomerados (arredondado para cima)", final, "conglomerados")
  passos[[length(passos) + 1L]] <- c("unidades na amostra (conglomerados × m̄)", final * m, "unidades")
  .tr_sampling_plano("conglomerados", "Tamanho · conglomerados", ceiling(final * m - 1e-9),
                     tibble::tibble(passo = vapply(passos, `[[`, "", 1L),
                                    valor = as.numeric(vapply(passos, `[[`, "", 2L)),
                                    unidade = vapply(passos, `[[`, "", 3L)),
                     plano$confianca, plano$erro,
                     parametros = c(par, list(icc = rho, deff_conglomerado = deff, M = M)),
                     conglomerados = final, tamanho_conglomerado = m,
                     nota = .tr_sampling_nota(
                       if (par$deff != 1) sprintf("o deff do plano (%s) foi trocado pelo dos conglomerados (%s)",
                                                  .tr_sampling_fmt(par$deff), .tr_sampling_fmt(deff)) else "",
                       if (par$N > 0) "a correção finita do plano de unidades não entra: aqui ela é sobre os conglomerados (M)" else ""))
}

#' Curva do tamanho da amostra contra a margem de erro.
#' @param plano plano de `sampling/size_mean` ou `sampling/size_proportion`.
#' @param aspecto,tema,titulo,rotulo_x,rotulo_y,legenda cosméticos (ver `trama.view`).
#' @return um ggplot (`view/plot`).
#' @export
tr_sampling_size_curve <- function(plano, aspecto = "16:9", tema = "padrão", titulo = "",
                                   rotulo_x = "", rotulo_y = "", legenda = "direita") {
  .tr_sampling_plano_do_tipo(plano, c("média", "proporção"), "sampling/size_curve",
                             "um bloco de seleção; a curva é para planos de média ou proporção")
  par <- plano$parametros
  E0 <- plano$erro
  grade <- seq(E0 * 0.4, E0 * 2.5, length.out = 60L)
  confs <- c(0.90, 0.95, 0.99)
  d <- do.call(rbind, lapply(confs, function(cf) {
    data.frame(erro = grade, n = vapply(grade, function(E) .tr_sampling_n_para_erro(par, E, cf), 0),
               confianca = paste0(round(100 * cf), "%"))
  }))
  d$confianca <- factor(d$confianca, levels = c("90%", "95%", "99%"))
  ponto <- data.frame(erro = E0, n = plano$n,
                      rotulo = sprintf("n = %s", format(plano$n, big.mark = ".", decimal.mark = ",")))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["erro"]], y = .data[["n"]], colour = .data[["confianca"]])) +
    ggplot2::geom_line(linewidth = .8) +
    ggplot2::geom_point(data = ponto, ggplot2::aes(x = .data[["erro"]], y = .data[["n"]]), inherit.aes = FALSE,
                        size = 3, colour = .TR_SAMPLING_COR_2) +
    ggplot2::geom_text(data = ponto, ggplot2::aes(x = .data[["erro"]], y = .data[["n"]], label = .data[["rotulo"]]),
                       inherit.aes = FALSE, hjust = -0.15, vjust = -0.6, size = 3.8) +
    ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
    ggplot2::labs(x = if (par$medida == "média") "margem de erro (unidade da variável)" else "margem de erro (proporção)",
                  y = "tamanho da amostra", colour = "confiança")
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}
