# Tudo é contraste: a ANOVA "aberta" que se desenha no quadro.
#
# Com k tratamentos, o SQ do tratamento é a soma de k − 1 contrastes
# ortogonais; efeito principal, interação, tendência linear e "controle contra o
# resto" são todos contrastes. Este bloco pega um `models/fit` que já existe e
# devolve UMA LINHA POR CONTRASTE — estimativa, SQ, F e p —, a conferência de
# que o conjunto soma o SQ do tratamento e a matriz que diz por que, quando não
# soma, não soma.
#
# Decisões que valem para o arquivo todo:
#
# - A estimativa e o erro padrão vêm do `emmeans` sobre o modelo certo (na
#   parcela subdividida, o misto auxiliar do `models/fit`, com gl de
#   Satterthwaite). É ele que escolhe o TERMO DE ERRO: contraste entre níveis da
#   parcela usa o erro a; entre níveis da subparcela, o erro b. Refazer essa
#   escolha aqui seria duplicar o que o models já acerta.
# - O SQ da linha é o SQ extra do teste de 1 gl, F × QM do erro (o do
#   `car::linearHypothesis`). O F é t², da covariância do modelo. No `lm`, o QM
#   do erro é o do resíduo, igual em toda linha; no misto e na parcela
#   subdividida, é o erro efetivo daquele contraste. No balanceado esse SQ é o
#   do livro, est² / Σ(cᵢ²/rᵢ); no desbalanceado, o do livro sai em `sq_livro`.
# - A sintaxe dos contrastes digitados é a do `models/linear_hypothesis`, lida
#   pelas MESMAS funções de lá (ver `.tr_exp_an_do_models`), para que um texto
#   que funciona num bloco funcione no outro.

.TR_EXP_AN_CONJUNTOS <- c("polinomiais", "helmert", "controle", "fatorial 2^k", "digitados")
#' As classes de erro dos nós de análise, para a tabela de `tr_experiments_errors()`.
#' @noRd
.tr_experiments_errors_analisar <- function() {
  data.frame(class = c("tr_experiments_error_not_applicable", "tr_experiments_error_not_numeric"),
             when = c("o modelo ou a resposta não servem à análise (GLM num contraste, y <= 0 no Box-Cox)",
                      "coluna ou nível que tem de ser número não é (doses, fatores codificados)"),
             stringsAsFactors = FALSE)
}

.TR_EXP_AN_GRAUS <- c("Linear", "Quadrático", "Cúbico", "Quártico")

#' Uma função interna do `trama.models`.
#'
#' Por `getFromNamespace`, e não por `:::`, que o R CMD check reprova. É um
#' acoplamento consciente e restrito ao leitor de contrastes: a alternativa era
#' copiar o parser, e dois parsers da mesma sintaxe divergem no primeiro ajuste.
#' @noRd
.tr_exp_an_do_models <- function(nome) utils::getFromNamespace(nome, "trama.models")

#' Colunas separadas por vírgula, conferidas contra a tabela do modelo.
#' @noRd
.tr_exp_an_cols <- function(dados, texto, param, no) {
  cols <- trimws(unlist(strsplit(as.character(texto), ",")))
  cols <- cols[nzchar(cols)]
  falta <- setdiff(cols, names(dados))
  if (length(falta)) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                     "'%s': a coluna %s de '%s' não está no modelo. Disponíveis: %s.", no,
                     paste(sprintf("'%s'", falta), collapse = ", "), param, paste(names(dados), collapse = ", "))
  }
  cols
}

#' O ajuste sobre o qual o `emmeans` trabalha, e os gl que ele deve usar.
#'
#' GLM fica de fora de propósito: lá o contraste vive na escala da ligação e o
#' teste é qui-quadrado de Wald, sem SQ que some — o `models/linear_hypothesis`
#' já faz esse caso.
#' @noRd
.tr_exp_an_emm <- function(modelo, specs, by, no) {
  obj <- switch(modelo$classe, lm = modelo$ajuste, lmer = modelo$ajuste, split = modelo$aux_misto,
    .tr_experiments_abort("tr_experiments_error_not_applicable",
                     paste0("'%s': contrastes com SQ e F são de modelo linear com erro normal (ANOVA, lm, ",
                            "misto ou parcela subdividida); o modelo é '%s'. No GLM, use 'models/linear_hypothesis'."),
                     no, modelo$classe))
  args <- list(obj, specs = specs, data = as.data.frame(modelo$dados))
  if (length(by)) args$by <- by
  if (modelo$classe %in% c("lmer", "split")) args$lmer.df <- "satterthwaite"
  suppressMessages(do.call(emmeans::emmeans, args))
}

#' Coeficientes em inteiros pequenos, quando existem.
#'
#' O SQ não muda com a escala do contraste, mas `-3 -1 1 3` se confere contra a
#' tabela do livro e `-0.67 -0.22 0.22 0.67` não.
#' @noRd
.tr_exp_an_inteiros <- function(v) {
  nz <- abs(v[abs(v) > 1e-10])
  if (!length(nz)) return(v)
  w <- v / min(nz)
  for (m in 1:24) {
    if (all(abs(m * w - round(m * w)) < 1e-6)) {
      z <- round(m * w)
      g <- Reduce(function(a, b) { while (b) { t <- b; b <- a %% b; a <- t }; a }, abs(z[z != 0]))
      return(z / g)
    }
  }
  v / max(abs(v))
}

#' Polinômios ortogonais para níveis quaisquer, com réplicas quaisquer.
#'
#' `stats::contr.poly` supõe níveis igualmente espaçados e réplicas iguais. Aqui
#' o polinômio é o de `stats::poly()` sobre as OBSERVAÇÕES (cada nível repetido
#' rᵢ vezes), que é ortogonal no produto Σ rᵢ pⱼ(xᵢ) pₗ(xᵢ). Como contraste de
#' médias, o coeficiente do nível i é cᵢ = rᵢ pⱼ(xᵢ): então Σ cᵢ dᵢ / rᵢ =
#' Σ rᵢ pⱼ pₗ = 0 — ortogonal no critério que vale para médias com réplicas
#' desiguais — e o SQ do contraste é exatamente o SQ sequencial do termo de grau
#' j da regressão em `poly()`. No caso igualmente espaçado e balanceado, sai
#' proporcional ao `contr.poly`, e os inteiros são os das tabelas.
#' @noRd
.tr_exp_an_polinomiais <- function(x, r) {
  k <- length(x)
  P <- stats::poly(rep(x, r), k - 1L)
  idx <- cumsum(r)  # uma linha de cada nível
  L <- t(vapply(seq_len(k - 1L), function(j) .tr_exp_an_inteiros(r * P[idx, j]), numeric(k)))
  rownames(L) <- c(.TR_EXP_AN_GRAUS, paste0("Grau ", 5:20))[seq_len(k - 1L)]
  L
}

#' Cada nível contra a média dos anteriores (o `contr.helmert`).
#' @noRd
.tr_exp_an_helmert <- function(niveis) {
  k <- length(niveis)
  L <- t(vapply(seq_len(k - 1L), function(j) c(rep(-1, j), j, rep(0, k - j - 1L)), numeric(k)))
  rownames(L) <- vapply(seq_len(k - 1L), function(j) {
    ant <- niveis[seq_len(j)]
    sprintf("%s vs %s", niveis[[j + 1L]], if (j == 1L) ant else sprintf("média(%s)", paste(ant, collapse = ", ")))
  }, "")
  L
}

#' Cada tratamento contra o controle: `B vs A`, `C vs A`... (k − 1 linhas).
#' @noRd
.tr_exp_an_controle <- function(niveis, controle, no) {
  k <- length(niveis)
  i <- match(controle, niveis)
  if (is.na(i)) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                     "'%s': o controle '%s' não é nível do fator (%s).", no, controle, paste(niveis, collapse = ", "))
  }
  outros <- niveis[-i]
  # Cada tratamento contra o controle (as comparações de Dunnett, 1955):
  # k − 1 contrastes linearmente independentes, NÃO ortogonais entre si.
  L <- matrix(0, k - 1L, k, dimnames = list(sprintf("%s vs %s", outros, controle), niveis))
  L[, i] <- -1
  for (j in seq_along(outros)) L[j, outros[[j]]] <- 1
  L
}

#' Efeitos principais e interações de um 2^k como contrastes nas células.
#' @noRd
.tr_exp_an_fatorial <- function(grade, fatores, no) {
  for (f in fatores) {
    if (nlevels(factor(grade[[f]])) != 2L) {
      .tr_experiments_abort("tr_experiments_error_bad_option",
                       "'%s': no conjunto '2^k' todo fator tem dois níveis, e '%s' tem %d.", no, f,
                       nlevels(factor(grade[[f]])))
    }
  }
  sinal <- lapply(fatores, function(f) ifelse(grade[[f]] == levels(factor(grade[[f]]))[[2]], 1, -1))
  names(sinal) <- fatores
  efeitos <- unlist(lapply(seq_along(fatores), function(m) {
    lapply(utils::combn(fatores, m, simplify = FALSE), identity)
  }), recursive = FALSE)
  L <- t(vapply(efeitos, function(s) Reduce(`*`, sinal[s]), numeric(nrow(grade))))
  rownames(L) <- vapply(efeitos, paste, "", collapse = ":")
  L
}

#' Os níveis do fator como números: o param `doses` ou os próprios nomes.
#' @noRd
.tr_exp_an_x <- function(niveis, doses, fator, no) {
  if (.tr_exp_preenchido(doses)) {
    toks <- unlist(strsplit(trimws(gsub(";", ",", doses)), "[, ]+"))
    x <- suppressWarnings(as.numeric(toks))
    if (anyNA(x) || length(x) != length(niveis)) {
      .tr_experiments_abort("tr_experiments_error_bad_option",
                       "'%s': 'doses' precisa de %d números, um por nível de '%s' (%s), na ordem.", no,
                       length(niveis), fator, paste(niveis, collapse = ", "))
    }
    return(x)
  }
  x <- suppressWarnings(as.numeric(gsub(",", ".", niveis, fixed = TRUE)))
  if (anyNA(x)) {
    .tr_experiments_abort("tr_experiments_error_not_numeric",
                     paste0("'%s': polinômios precisam da distância entre os níveis, e os nomes de '%s' (%s) ",
                            "não são números. Digite os valores em 'doses', na ordem dos níveis."),
                     no, fator, paste(niveis, collapse = ", "))
  }
  if (anyDuplicated(x)) {
    .tr_experiments_abort("tr_experiments_error_bad_option", "'%s': há doses repetidas em '%s'.", no, fator)
  }
  x
}

#' Os contrastes digitados, lidos pelo parser do `models/linear_hypothesis`.
#' @noRd
.tr_exp_an_digitados <- function(texto, niveis, no) {
  if (!.tr_exp_preenchido(texto)) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                     "'%s': com o conjunto 'digitados', escreva os contrastes em 'contrastes'.", no)
  }
  linhas_hip <- .tr_exp_an_do_models(".tr_models_linhas_hip")
  rotulo_hip <- .tr_exp_an_do_models(".tr_models_rotulo_hip")
  coef_contraste <- .tr_exp_an_do_models(".tr_models_coef_contraste")
  ls <- linhas_hip(texto)
  lin <- lapply(seq_along(ls), function(i) rotulo_hip(ls[[i]], i))
  nomes <- vapply(lin, `[[`, "", "nome")
  if (anyDuplicated(nomes)) {
    .tr_experiments_abort("tr_experiments_error_bad_option", "'%s': rótulo repetido: %s.", no,
                     paste(unique(nomes[duplicated(nomes)]), collapse = ", "))
  }
  L <- do.call(rbind, lapply(lin, function(l) coef_contraste(l$corpo, niveis, no)))
  rownames(L) <- nomes
  L
}

#' Σ cᵢ dᵢ / rᵢ entre todos os pares: zero é ortogonal.
#'
#' É a covariância entre dois contrastes de médias, a menos de σ². Com réplicas
#' desiguais, dois contrastes de coeficientes "ortogonais" no papel (Σ cᵢdᵢ = 0)
#' deixam de ser, e é aqui que se vê.
#' @noRd
.tr_exp_an_ortogonalidade <- function(L, r) {
  M <- L %*% diag(1 / r, length(r)) %*% t(L)
  # Resto de ponto flutuante (1e-17) vira zero, para a tabela ler "0".
  escala <- sqrt(outer(abs(diag(M)), abs(diag(M))))
  M[abs(M) <= 1e-10 * escala] <- 0
  dimnames(M) <- list(rownames(L), rownames(L))
  M
}

#' Pares não ortogonais, com a escala de cada contraste para a tolerância.
#' @noRd
.tr_exp_an_pares_nao_ortogonais <- function(M) {
  n <- nrow(M)
  if (n < 2L) return(character())
  out <- character()
  for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
    if (abs(M[i, j]) > 1e-8 * sqrt(M[i, i] * M[j, j])) out <- c(out, sprintf("%s × %s", rownames(M)[[i]], rownames(M)[[j]]))
  }
  out
}

#' O SQ que o conjunto deveria reproduzir, lido do quadro do próprio modelo.
#'
#' Soma os termos cujas variáveis estão todas em `alvo` e que contêm `fator`:
#' sem desdobramento, só o termo do fator; desdobrado dentro de B, fator + fator:B;
#' no 2^k, todos os termos do tratamento. Na parcela subdividida, procura em
#' todos os estratos do `aov` com `Error()`. No misto genérico não há quadro de
#' SQ comparável, e a conferência fica em branco.
#' @noRd
.tr_exp_an_sq_alvo <- function(modelo, fatores, alvo) {
  quadro <- switch(modelo$classe,
    lm = { a <- as.data.frame(stats::anova(modelo$ajuste)); list(a) },
    split = lapply(summary(modelo$ajuste), function(s) as.data.frame(s[[1]])),
    NULL)
  if (is.null(quadro)) return(list(sq = NA_real_, gl = NA_real_))
  sq <- 0; gl <- 0; achou <- FALSE
  for (a in quadro) {
    termos <- trimws(rownames(a))
    for (i in seq_along(termos)) {
      vs <- gsub("`", "", strsplit(termos[[i]], ":", fixed = TRUE)[[1]])
      if (all(vs %in% alvo) && any(vs %in% fatores)) {
        sq <- sq + a[i, "Sum Sq"]; gl <- gl + a[i, "Df"]; achou <- TRUE
      }
    }
  }
  if (!achou) return(list(sq = NA_real_, gl = NA_real_))
  list(sq = sq, gl = gl)
}

#' SQ sequencial de cada grau em `poly()`, no mesmo modelo: a regressão.
#'
#' Reajusta o `lm` com o fator trocado por `poly(x, k − 1)` ANTES dos termos que
#' o contêm (interações), e depois dos controles. O SQ de cada grau tem de ser o
#' do contraste polinomial correspondente — é a equivalência que o bloco mostra.
#' @noRd
.tr_exp_an_sq_regressao <- function(modelo, fator, x) {
  if (modelo$classe != "lm") return(NULL)
  d <- as.data.frame(modelo$dados)
  k <- length(x)
  d$.x_dose <- x[as.integer(factor(d[[fator]]))]
  termos <- attr(stats::terms(stats::as.formula(modelo$formula)), "term.labels")
  contem <- vapply(termos, function(t) fator %in% gsub("`", "", strsplit(t, ":", fixed = TRUE)[[1]]), NA)
  bt <- function(v) ifelse(make.names(v) == v, v, paste0("`", v, "`"))
  rhs <- c(termos[!contem], "poly(.x_dose, degree)", termos[contem])
  f <- stats::as.formula(paste(bt(modelo$resposta), "~", paste(rhs, collapse = " + ")))
  env <- new.env(parent = globalenv()); assign("degree", k - 1L, envir = env); environment(f) <- env
  aj <- stats::lm(f, data = d)
  # `anova()` junta os graus de `poly()` num termo só; a partição por grau sai
  # dos efeitos ortogonais do QR, na ordem das colunas.
  ef <- stats::effects(aj)
  cols <- grep("^poly\\(\\.x_dose", names(stats::coef(aj)))
  as.numeric(ef[cols]^2)
}

#' Contrastes de um `models/fit`: uma linha por contraste.
#'
#' @param modelo objeto `tr_models_fit` (lm, ANOVA, misto ou parcela subdividida).
#' @param fator a coluna-fator cujas médias se contrastam; no conjunto `"fatorial
#'   2^k"`, de 2 a 5 colunas de dois níveis, separadas por vírgula.
#' @param conjunto `"polinomiais"`, `"helmert"`, `"controle"`, `"fatorial 2^k"`
#'   ou `"digitados"`.
#' @param contrastes texto dos contrastes digitados, na sintaxe do
#'   `models/linear_hypothesis` (`"linear: -3 -1 1 3; B - A"`).
#' @param controle o nível controle (conjunto `"controle"`); em branco, o primeiro.
#' @param doses os valores numéricos dos níveis, na ordem (polinomiais); em
#'   branco, os nomes dos níveis lidos como número.
#' @param dentro fator em cujos níveis os contrastes são desdobrados (interação).
#' @return lista com `out` (`tr_models_effects`, um contraste por linha) e
#'   `ortogonalidade` (tibble com a matriz Σ cᵢdᵢ/rᵢ).
#' @export
tr_experiments_contrasts <- function(modelo, fator = "", conjunto = "polinomiais", contrastes = "",
                                     controle = "", doses = "", dentro = "") {
  no <- "experiments/contrasts"
  if (!inherits(modelo, "tr_models_fit")) {
    .tr_experiments_abort("tr_experiments_error_bad_option", "'%s' lê um modelo ajustado (models/fit).", no)
  }
  conjunto <- as.character(conjunto)
  if (!conjunto %in% .TR_EXP_AN_CONJUNTOS) {
    .tr_experiments_abort("tr_experiments_error_bad_option", "'%s': conjunto '%s' não existe. Use: %s.", no,
                     conjunto, paste(.TR_EXP_AN_CONJUNTOS, collapse = ", "))
  }
  dados <- as.data.frame(modelo$dados)
  if (!.tr_exp_preenchido(fator)) {
    .tr_experiments_abort("tr_experiments_error_bad_option", "'%s': diga em 'fator' de quem são as médias.", no)
  }
  fts <- .tr_exp_an_cols(dados, fator, "fator", no)
  by <- if (.tr_exp_preenchido(dentro)) .tr_exp_an_cols(dados, dentro, "dentro", no) else character()
  if (length(by) > 1L || length(intersect(by, fts))) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                     "'%s': 'dentro' é UM fator, diferente do que se contrasta.", no)
  }
  if (conjunto == "fatorial 2^k") {
    if (length(fts) < 2L || length(fts) > 5L) {
      .tr_experiments_abort("tr_experiments_error_bad_option",
                       "'%s': o conjunto '2^k' pede de 2 a 5 fatores em 'fator', separados por vírgula.", no)
    }
    if (length(by)) {
      .tr_experiments_abort("tr_experiments_error_bad_option",
                       "'%s': no '2^k' as interações já são contrastes; deixe 'dentro' em branco.", no)
    }
  } else if (length(fts) != 1L) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                     "'%s': um fator só em 'fator' (vários só no conjunto 'fatorial 2^k').", no)
  }
  for (f in c(fts, by)) {
    if (!is.factor(dados[[f]])) {
      .tr_experiments_abort("tr_experiments_error_not_applicable",
                       "'%s': '%s' é numérica no modelo; contraste é entre níveis de fator.", no, f)
    }
  }

  emm <- .tr_exp_an_emm(modelo, fts, by, no)
  grade <- as.data.frame(summary(emm))
  if (length(by)) grade1 <- grade[grade[[by]] == grade[[by]][[1]], , drop = FALSE] else grade1 <- grade
  niveis <- as.character(grade1[[fts[[1]]]])

  # Réplicas de cada média contrastada: as do fator (ou da célula, no 2^k). No
  # desdobramento, as da CÉLULA em cada nível de `dentro`: os polinômios de um
  # grupo são construídos com as réplicas daquele grupo, e ficam ortogonais
  # dentro da célula mesmo quando as células têm tamanhos diferentes.
  chave <- function(df, cols) do.call(paste, c(lapply(cols, function(cc) as.character(df[[cc]])), sep = "\r"))
  conta <- table(chave(dados, c(fts, by)))
  r_de <- function(g) as.numeric(conta[chave(g, c(fts, by))])
  grupos <- if (length(by)) unique(as.character(grade[[by]])) else ""
  linhas_g <- lapply(grupos, function(b) if (length(by)) which(as.character(grade[[by]]) == b) else seq_len(nrow(grade)))

  x <- NULL
  monta_L <- function(g, r) switch(conjunto,
    polinomiais = { x <<- .tr_exp_an_x(niveis, doses, fts, no); .tr_exp_an_polinomiais(x, r) },
    helmert = .tr_exp_an_helmert(niveis),
    controle = .tr_exp_an_controle(niveis, if (.tr_exp_preenchido(controle)) controle else niveis[[1]], no),
    `fatorial 2^k` = .tr_exp_an_fatorial(g, fts, no),
    digitados = .tr_exp_an_digitados(contrastes, niveis, no))
  r_g <- lapply(linhas_g, function(i) r_de(grade[i, , drop = FALSE]))
  L_g <- lapply(seq_along(grupos), function(j) monta_L(grade[linhas_g[[j]], , drop = FALSE], r_g[[j]]))
  L <- L_g[[1]]
  if (qr(L)$rank < nrow(L)) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                     "'%s': os contrastes (%s) são linearmente dependentes — algum é combinação dos outros.",
                     no, paste(rownames(L), collapse = "; "))
  }

  # Cada contraste vira um vetor sobre a grade TODA (zero fora do seu grupo),
  # para que cada grupo de `dentro` tenha os seus próprios coeficientes.
  metodo <- list(); info <- list()
  for (j in seq_along(grupos)) for (i in seq_len(nrow(L_g[[j]]))) {
    v <- numeric(nrow(grade)); v[linhas_g[[j]]] <- L_g[[j]][i, ]
    nm <- if (length(by)) sprintf("%s | %s = %s", rownames(L_g[[j]])[[i]], by, grupos[[j]]) else rownames(L_g[[j]])[[i]]
    metodo[[nm]] <- v
    info[[nm]] <- list(c = L_g[[j]][i, ], r = r_g[[j]])
  }
  ct <- emmeans::contrast(emm, method = metodo, by = NULL, adjust = "none")
  s <- as.data.frame(summary(ct))
  info <- info[as.character(s$contrast)]
  soma_c2r <- vapply(info, function(z) sum(z$c^2 / z$r), 1)
  est <- s$estimate
  f_est <- (est / s$SE)^2
  # O SQ da linha é o SQ EXTRA do teste de 1 gl (o do `car::linearHypothesis`):
  # F × QM do erro. No `lm`, o QM do erro é o do resíduo, o mesmo em toda
  # linha; no misto e na parcela subdividida, é o erro efetivo (combinado) que
  # o `emmeans` usou para aquele contraste, SE² / Σ(cᵢ²/rᵢ). No balanceado, os
  # dois coincidem com o SQ do livro, est² / Σ(cᵢ²/rᵢ); no desbalanceado, o do
  # livro vai à coluna `sq_livro`.
  qm <- if (modelo$classe == "lm") rep(stats::sigma(modelo$ajuste)^2, nrow(s)) else s$SE^2 / soma_c2r
  sq <- f_est * qm
  sq_livro <- est^2 / soma_c2r
  tab <- data.frame(termo = as.character(s$contrast),
                    coeficientes = vapply(info, function(z) paste(format(signif(z$c, 4), trim = TRUE), collapse = " "), ""),
                    estimativa = est, erro_padrao = s$SE, gl = 1L, sq = sq, F = f_est,
                    gl_erro = s$df, qm_erro = qm,
                    p_valor = stats::pf(f_est, 1, s$df, lower.tail = FALSE),
                    stringsAsFactors = FALSE)
  livro_difere <- any(abs(sq_livro - sq) > 1e-8 * pmax(1, abs(sq)))
  if (livro_difere) tab$sq_livro <- sq_livro
  i_l <- match(sub(" \\| .*$", "", as.character(s$contrast)), rownames(L))
  if (conjunto == "fatorial 2^k") tab$efeito <- est / 2^(length(fts) - 1L)

  nota <- character()
  if (conjunto == "polinomiais") {
    sqr <- .tr_exp_an_sq_regressao(modelo, fts, x)
    if (!is.null(sqr) && !length(by)) {
      tab$sq_regressao <- sqr[i_l]
      nota <- c(nota, "sq_regressao: o SQ sequencial de cada grau em poly(dose) no mesmo modelo — igual ao do contraste")
    }
  }

  # Conferência da soma: só faz sentido para um conjunto COMPLETO (k − 1 por
  # grupo) e ortogonal. Fora disso, diz por que não se espera que some.
  Ms <- lapply(seq_along(grupos), function(j) .tr_exp_an_ortogonalidade(L_g[[j]], r_g[[j]]))
  nao_ort <- unique(unlist(lapply(seq_along(grupos), function(j) {
    p <- .tr_exp_an_pares_nao_ortogonais(Ms[[j]])
    if (length(p) && length(by)) sprintf("%s (%s = %s)", p, by, grupos[[j]]) else p
  })))
  alvo <- .tr_exp_an_sq_alvo(modelo, fts, c(fts, by))
  soma <- sum(sq)
  completo <- nrow(L) == ncol(L) - 1L
  rodape <- list(`Σ SQ contrastes` = format(signif(soma, 7)))
  if (!is.na(alvo$sq)) rodape[[if (length(by)) sprintf("SQ %s + %s:%s", fts, fts, by) else "SQ tratamento"]] <- format(signif(alvo$sq, 7))
  confere <- !is.na(alvo$sq) && completo && !length(nao_ort) && abs(soma - alvo$sq) <= 1e-6 * max(1, alvo$sq)
  rodape$soma <- if (is.na(alvo$sq)) "sem quadro de SQ para conferir (misto)"
                 else if (confere) "confere: o conjunto é completo e ortogonal"
                 else if (!completo) sprintf("não se espera: %d contraste(s) para %d gl", nrow(L), ncol(L) - 1L)
                 else if (length(nao_ort)) "não soma: há pares não ortogonais"
                 else "não soma: o modelo é desbalanceado (médias ajustadas)"
  if (length(nao_ort)) nota <- c(nota, sprintf("pares não ortogonais (Σ cᵢdᵢ/rᵢ ≠ 0): %s — os SQ se sobrepõem e não somam", paste(nao_ort, collapse = ", ")))
  nao_soma <- rownames(L)[abs(rowSums(L)) > 1e-8]
  if (length(nao_soma)) nota <- c(nota, sprintf("não somam zero (não são contrastes): %s", paste(nao_soma, collapse = ", ")))
  if (livro_difere) nota <- c(nota, "desbalanceado: sq é o SQ extra do teste (F × qm_erro); sq_livro é est² / Σ(cᵢ²/rᵢ), que aqui não é o SQ do teste")
  if (modelo$classe %in% c("split", "lmer")) nota <- c(nota, "erro de cada contraste pelo modelo misto (gl de Satterthwaite): na parcela subdividida balanceada, erro a para a parcela e erro b para a subparcela")

  ort <- do.call(rbind, lapply(seq_along(grupos), function(j) {
    M <- Ms[[j]]
    o <- cbind(data.frame(contraste = rownames(M), stringsAsFactors = FALSE), as.data.frame(M, optional = TRUE))
    if (length(by)) o <- cbind(stats::setNames(data.frame(rep(grupos[[j]], nrow(o)), stringsAsFactors = FALSE), by), o)
    o
  }))
  ort <- tibble::as_tibble(ort)
  titulo <- sprintf("Contrastes (%s) · %s%s", conjunto, paste(fts, collapse = " × "),
                    if (length(by)) sprintf(" dentro de %s", by) else "")
  quadro <- trama.models::tr_models_effects(
    tab, titulo, coluna_estat = "F", rodape = rodape, nota = paste(nota, collapse = "; "),
    fonte = "Montgomery (2017); Pimentel-Gomes (2009); Searle (1971); Lenth (emmeans)")
  list(out = quadro, ortogonalidade = ort)
}
