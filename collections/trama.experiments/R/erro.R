# O nó `experiments/error`: somar os termos, sortear o resíduo e fechar a
# resposta. Antes dele só existem componentes (`.ef_*`); depois dele, uma
# coluna de resposta que qualquer nó de `trama.models` lê pelo adaptador.
#
# Escalas: na normal, a resposta é Σ termos + resíduo. Nas outras famílias os
# termos somam o PREDITOR LINEAR η e a resposta é sorteada da família com média
# g⁻¹(η): Poisson e gama na ligação log, binomial na logit — as ligações que o
# `models/glm` usa (a gama de lá é log, não a inversa canônica).
#
# Também mora aqui a cadeia pura (`tr_experiments_simulate`), que o poder por
# simulação e o teste de aleatorização vão repetir N vezes.

.TR_EXP_ER_DIST <- c("normal", "poisson", "binomial", "gama")
.TR_EXP_ER_COR <- c("independente", "simetria_composta", "ar1")

.tr_exp_er_abort <- function(fmt, ...) {
  .tr_experiments_abort("tr_experiments_error_bad_response", paste0("'experiments/error': ", fmt), ...)
}

#' Um número finito dentro de uma faixa.
#' @noRd
.tr_exp_er_num <- function(x, param, min = -Inf, max = Inf) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < min || x > max) {
    .tr_exp_er_abort("'%s' tem de ser um número entre %g e %g (veio '%s').", param, min, max,
                     paste(as.character(x), collapse = ", "))
  }
  as.numeric(x)
}

#' Inovações de média 0 e variância 1: normal, t escalada ou gama padronizada.
#' @noRd
.tr_exp_er_inovacoes <- function(n, caudas_gl, assimetria) {
  if (caudas_gl > 0) return(stats::rt(n, caudas_gl) * sqrt((caudas_gl - 2) / caudas_gl))
  if (assimetria != 0) {
    # Gama de forma k padronizada: assimetria 2/√k, então k = 4/γ².
    k <- 4 / assimetria^2
    return(sign(assimetria) * (stats::rgamma(n, shape = k) - k) / sqrt(k))
  }
  stats::rnorm(n)
}

#' Correlaciona as inovações dentro de cada indivíduo, na ordem do tempo.
#' @noRd
.tr_exp_er_correlacionar <- function(z, u, correlacao, rho) {
  if (!"individuo" %in% names(u)) {
    .tr_exp_er_abort("correlação '%s' é entre medidas do mesmo indivíduo, e este plano não tem 'individuo' (use a estrutura medidas_repetidas ou crossover).",
                     correlacao)
  }
  tempo <- if ("tempo" %in% names(u)) u$tempo else if ("periodo" %in% names(u)) u$periodo else u$ordem
  pos <- if (is.factor(tempo)) as.integer(tempo) else rank(tempo, ties.method = "first")
  for (ids in split(seq_len(nrow(u)), u$individuo)) {
    ids <- ids[order(pos[ids])]
    m <- length(ids)
    R <- if (correlacao == "ar1") rho^abs(outer(seq_len(m), seq_len(m), `-`))
         else { R <- matrix(rho, m, m); diag(R) <- 1; R }
    ch <- tryCatch(chol(R), error = function(e) {
      .tr_exp_er_abort("com %d medidas por indivíduo, rho = %g não dá matriz de correlação válida.", m, rho)
    })
    z[ids] <- drop(t(ch) %*% z[ids])
  }
  z
}

#' O lado direito do modelo que o plano sugere, como fórmula.
#' @noRd
.tr_exp_er_rhs <- function(analise) {
  p <- analise$params
  if (!is.null(p$formula)) return(p$formula)
  fts <- if (!is.null(p$fatores)) .tr_exp_split(p$fatores) else character()
  blc <- if (.tr_exp_preenchido(p$bloco)) p$bloco else character()
  switch(analise$no,
    "models/anova_dic" = paste("~", p$tratamento),
    "models/anova_dbc" = paste("~", p$bloco, "+", p$tratamento),
    "models/anova_dql" = paste("~", p$linha, "+", p$coluna, "+", p$tratamento),
    "models/anova_factorial" = paste("~", paste(c(blc, paste(fts, collapse = " * ")), collapse = " + ")),
    "models/anova_split_plot" = sprintf("~ %s%s * %s + (1 | %s)", if (length(blc)) paste(blc, "+ ") else "",
                                        p$parcela, p$subparcela,
                                        if (length(blc)) paste0(blc, ":", p$parcela) else p$parcela),
    NULL)
}

#' A análise do plano com a resposta: o mesmo nó, com a resposta prefixada; um
#' `models/lm`/`models/lmer` quando há covariável; `models/glm`/`models/glmer`
#' fora da normal.
#' @noRd
.tr_exp_er_analise <- function(plano, resp, dist, ensaios) {
  a <- plano$analise
  covs <- unlist(lapply(plano$termos, function(t) if (t$tipo == "covariavel") t$fator))
  avisos <- character()
  if (dist == "normal" && !length(covs)) {
    if (!is.null(a$params$formula)) a$params$formula <- paste(resp, a$params$formula)
    else a$params <- c(list(resposta = resp), a$params)
    return(list(analise = a, avisos = avisos))
  }
  rhs <- .tr_exp_er_rhs(a)
  if (is.null(rhs)) return(list(analise = a, avisos = "não há fórmula sugerida para esta análise; monte o modelo à mão."))
  if (length(covs)) rhs <- paste(rhs, "+", paste(covs, collapse = " + "))
  aleatorio <- grepl("|", rhs, fixed = TRUE)
  lhs <- if (dist == "binomial" && ensaios > 1L) sprintf("cbind(%s, %s_fracassos)", resp, resp) else resp
  if (dist == "normal") {
    return(list(analise = list(no = if (aleatorio) "models/lmer" else "models/lm",
                               params = list(formula = paste(lhs, rhs))), avisos = avisos))
  }
  familia <- c(poisson = "poisson", binomial = "binomial", gama = "gama")[[dist]]
  if (aleatorio && dist == "gama") {
    rhs <- trimws(gsub("\\s*\\+\\s*\\(1 \\| [^)]*\\)", "", rhs))
    avisos <- "gama com termo aleatório: 'models/glmer' não tem a família gama; a análise sugerida é o GLM só com os fixos (o erro da parcela fica de fora, e o teste do fator da parcela sai liberal: taxa de erro tipo I acima de α)."
    aleatorio <- FALSE
  }
  list(analise = list(no = if (aleatorio) "models/glmer" else "models/glm",
                      params = list(formula = paste(lhs, rhs), familia = familia)), avisos = avisos)
}

#' Fecha a resposta simulada.
#'
#' Nível 1 do nó `experiments/error`.
#' @param plano objeto `tr_experiments_plan` com termos de `experiments/effect`.
#' @param resposta nome da coluna da resposta.
#' @param distribuicao `normal`, `poisson`, `binomial` ou `gama`.
#' @param sd desvio-padrão do resíduo (normal).
#' @param sd_por fator cujo nível muda o sd (heterocedasticidade), ou vazio.
#' @param sds sd por nível de `sd_por` (`"baixa = 1, alta = 3"`).
#' @param ensaios nº de ensaios da binomial.
#' @param forma forma da gama.
#' @param correlacao `independente`, `simetria_composta` ou `ar1`.
#' @param rho correlação entre medidas do mesmo indivíduo.
#' @param caudas_gl graus de liberdade da t (0 = sem caudas pesadas).
#' @param assimetria coeficiente de assimetria do resíduo (0 = simétrico).
#' @param perdidas proporção de unidades perdidas ao acaso (MCAR).
#' @param .seed semente (vem do card).
#' @return o plano com a coluna da resposta e `plano$analise` completa.
#' @export
tr_experiments_error <- function(plano, resposta = "y", distribuicao = "normal", sd = 1, sd_por = "", sds = "",
                                 ensaios = 10L, forma = 2, correlacao = "independente", rho = 0.5,
                                 caudas_gl = 0, assimetria = 0, perdidas = 0, .seed = 1L) {
  .tr_exp_plano_conferir(plano)
  dist <- .tr_exp_enum(distribuicao, .TR_EXP_ER_DIST, "distribuicao")
  correlacao <- .tr_exp_enum(correlacao, .TR_EXP_ER_COR, "correlacao")
  if (!is.null(plano$resposta)) {
    .tr_experiments_abort("tr_experiments_error_response_closed",
                          "'experiments/error': a resposta '%s' já foi fechada; um plano leva um erro só.", plano$resposta$nome)
  }
  u <- plano$unidades
  N <- nrow(u)
  termos <- grep("^\\.ef_", names(u), value = TRUE)
  if (!length(termos)) {
    .tr_experiments_abort("tr_experiments_error_no_terms",
                          "'experiments/error': o plano não tem termos. Ligue ao menos um 'experiments/effect' (o intercepto) antes.")
  }
  if (!.tr_exp_preenchido(resposta) || !grepl("^[A-Za-z][A-Za-z0-9_]*$", resposta)) {
    .tr_exp_er_abort("'resposta' tem de ser um nome de coluna (letra, depois letras, dígitos ou _).")
  }
  if (resposta %in% names(u)) .tr_exp_er_abort("o plano já tem a coluna '%s'; dê outro nome à resposta.", resposta)
  sd <- .tr_exp_er_num(sd, "sd", 0)
  rho <- .tr_exp_er_num(rho, "rho", -1, 1)
  caudas_gl <- .tr_exp_er_num(caudas_gl, "caudas_gl", 0)
  assimetria <- .tr_exp_er_num(assimetria, "assimetria", -10, 10)
  perdidas <- .tr_exp_er_num(perdidas, "perdidas", 0, 0.9)
  forma <- .tr_exp_er_num(forma, "forma", 1e-6)
  ensaios <- .tr_exp_int(ensaios, "ensaios", 1L, 1e6)
  if (caudas_gl > 0 && caudas_gl <= 2) .tr_exp_er_abort("'caudas_gl' tem de passar de 2 (a t de gl <= 2 não tem variância).")
  if (caudas_gl > 0 && assimetria != 0) .tr_exp_er_abort("escolha caudas pesadas OU assimetria, não as duas.")
  perturba <- caudas_gl > 0 || assimetria != 0 || correlacao != "independente" || .tr_exp_preenchido(sd_por)
  if (dist != "normal" && perturba) {
    .tr_exp_er_abort("caudas, assimetria, correlação e sd por nível são do resíduo normal; na '%s' a variância vem da família.", dist)
  }
  eta <- rowSums(as.matrix(u[, termos, drop = FALSE]))
  sdi <- rep(sd, N)
  if (.tr_exp_preenchido(sd_por)) {
    if (!sd_por %in% names(u) || is.numeric(u[[sd_por]])) .tr_exp_er_abort("'sd_por': '%s' não é fator do plano.", sd_por)
    niv <- .tr_exp_ef_niveis(u[[sd_por]])
    s <- .tr_exp_ef_pares(sds, "sds")
    if (is.null(names(s)) || !setequal(names(s), niv) || any(s < 0)) {
      .tr_exp_er_abort("'sds' tem de dar um sd >= 0 a cada nível de '%s' (%s).", sd_por, paste(niv, collapse = ", "))
    }
    sdi <- unname(s[as.character(u[[sd_por]])])
  }
  sorteio <- .tr_exp_com_semente(.seed, {
    y <- switch(dist,
      normal = {
        z <- .tr_exp_er_inovacoes(N, caudas_gl, assimetria)
        if (correlacao != "independente") z <- .tr_exp_er_correlacionar(z, u, correlacao, rho)
        eta + sdi * z
      },
      poisson = as.numeric(stats::rpois(N, exp(eta))),
      binomial = as.numeric(stats::rbinom(N, ensaios, stats::plogis(eta))),
      gama = stats::rgamma(N, shape = forma, rate = forma / exp(eta)))
    perd <- if (perdidas > 0) sample.int(N, round(perdidas * N)) else integer()
    list(y = y, perd = perd)
  })
  y <- sorteio$y
  if (dist == "normal") u$.ef_residuo <- y - eta
  y[sorteio$perd] <- NA
  u[[resposta]] <- y
  if (dist == "binomial" && ensaios > 1L) u[[paste0(resposta, "_fracassos")]] <- ensaios - y
  an <- .tr_exp_er_analise(plano, resposta, dist, ensaios)
  plano$unidades <- u
  plano$analise <- an$analise
  plano$resposta <- list(nome = resposta, distribuicao = dist,
                         parametros = list(sd = sd, sd_por = sd_por, sds = sds, ensaios = ensaios, forma = forma,
                                           correlacao = correlacao, rho = rho, caudas_gl = caudas_gl,
                                           assimetria = assimetria, perdidas = perdidas),
                         perdidas = sort(sorteio$perd), semente = as.integer(.seed))
  plano$avisos <- c(plano$avisos, an$avisos)
  plano$nota <- paste(c(plano$nota, sprintf("resposta %s (%s); análise: %s", resposta, dist, plano$analise$no)),
                      collapse = "; ")
  plano
}

#' Roda a cadeia inteira — termos e erro — com uma semente só.
#'
#' Função pura para quem repete a simulação N vezes (poder, teste de
#' aleatorização): cada termo e o erro recebem sementes DERIVADAS de `.seed`,
#' sorteadas com o gerador fixo da coleção. A mesma `.seed` dá o mesmo dado.
#' @param plano objeto `tr_experiments_plan` (de `tr_experiments_design`).
#' @param termos lista de listas: cada uma, os argumentos de
#'   [tr_experiments_effect()] sem `plano` e sem `.seed`.
#' @param erro lista com os argumentos de [tr_experiments_error()] sem `plano`
#'   e sem `.seed`.
#' @param .seed semente da cadeia.
#' @return o plano com a resposta.
#' @export
tr_experiments_simulate <- function(plano, termos = list(), erro = list(), .seed = 1L) {
  .tr_exp_plano_conferir(plano)
  sementes <- .tr_exp_com_semente(.seed, sample.int(.Machine$integer.max - 1L, length(termos) + 1L))
  for (i in seq_along(termos)) {
    plano <- do.call(tr_experiments_effect, c(list(plano), termos[[i]], list(.seed = sementes[[i]])))
  }
  do.call(tr_experiments_error, c(list(plano), erro, list(.seed = sementes[[length(termos) + 1L]])))
}
