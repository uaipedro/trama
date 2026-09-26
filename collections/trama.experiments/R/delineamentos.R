# As construções: uma função por estrutura, todas com a mesma forma.
#
# Cada `.tr_exp_estr_<id>(s)` recebe a especificação já conferida (`s`, de
# `.tr_exp_spec()`), SORTEIA com o gerador corrente (a semente é posta por
# fora, em `tr_experiments_design()`) e devolve uma lista com `unidades`,
# `fatores`, `hierarquia`, `posicoes`, `eixos`, `tipo_geo`, `analise`,
# `extras`, `avisos`, `rotulo`. Quem monta o objeto plano é
# `.tr_exp_montar_plano()`, num lugar só.
#
# O sorteio segue o escopo que a unidade de atribuição determina: livre entre
# todas as unidades (DIC), restrito ao bloco (DBC), às linhas e colunas
# (quadrado latino), em estágios (parcela no bloco, depois subparcela na
# parcela). É isso que o metadado `fatores` descreve, e o que os testes
# conferem contando tratamentos por bloco, linha e coluna.

# ---- auxiliares ---------------------------------------------------------------

#' Uma linha da tabela de fatores.
#' @noRd
.tr_exp_fator <- function(nome, papel, niveis, unidade, escopo, mecanismo, justificativa = "") {
  d <- data.frame(nome = nome, papel = papel, unidade = unidade, escopo = escopo,
                  mecanismo = mecanismo, justificativa = justificativa, stringsAsFactors = FALSE)
  d$niveis <- list(as.character(niveis))
  d[, c("nome", "papel", "niveis", "unidade", "escopo", "mecanismo", "justificativa")]
}

.tr_exp_hier <- function(nivel, coluna, dentro_de, n) {
  data.frame(nivel = nivel, coluna = coluna, dentro_de = dentro_de, n = as.integer(n),
             stringsAsFactors = FALSE)
}

#' Fator do R com níveis "1".."n" — as colunas estruturais.
#' @noRd
.tr_exp_id <- function(x, n = max(x)) factor(as.character(x), levels = as.character(seq_len(n)))

#' Grade automática: preenche linha a linha, com `cg` colunas (0 = quadrada).
#' @noRd
.tr_exp_grade_auto <- function(n, cg) {
  nc <- if (cg > 0L) cg else ceiling(sqrt(n))
  i <- seq_len(n) - 1L
  data.frame(linha = i %/% nc + 1L, coluna = i %% nc + 1L)
}

#' As combinações de vários fatores, o primeiro variando mais devagar.
#' @noRd
.tr_exp_combinacoes <- function(f) {
  g <- do.call(expand.grid, c(rev(f), list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)))
  g <- g[, rev(names(g)), drop = FALSE]
  for (nm in names(f)) g[[nm]] <- factor(g[[nm]], levels = f[[nm]])
  g
}

#' Sorteio completamente casualizado de `nt` tratamentos com `r` repetições.
#' @noRd
.tr_exp_sorteio_dic <- function(nt, r) .tr_exp_perm(rep(seq_len(nt), each = r))

#' Sorteio em blocos completos: uma permutação independente por bloco.
#' @noRd
.tr_exp_sorteio_dbc <- function(nt, r) {
  data.frame(bloco = rep(seq_len(r), each = nt),
             posicao = rep(seq_len(nt), r),
             trat = unlist(lapply(seq_len(r), function(b) .tr_exp_perm(seq_len(nt)))))
}

.tr_exp_bt <- function(x) ifelse(make.names(x) == x, x, paste0("`", x, "`"))

.tr_exp_sem_residuo <- function(estrutura, porque) {
  .tr_experiments_abort("tr_experiments_error_no_residual",
                        "Estrutura '%s': %s — sobra 0 grau de liberdade para o resíduo.", estrutura, porque)
}

#' Tratamentos (fatores de papel tratamento) como uma combinação só, para os
#' rótulos das vistas.
#' @noRd
.tr_exp_rotulo_trat <- function(u, nomes) {
  if (!length(nomes)) return(rep("", nrow(u)))
  partes <- lapply(nomes, function(nm) {
    v <- u[[nm]]
    if (is.numeric(v)) format(round(v, 2), trim = TRUE) else as.character(v)
  })
  do.call(paste, c(partes, sep = ":"))
}

# ---- DIC ----------------------------------------------------------------------

.tr_exp_estr_dic <- function(s) {
  nome <- names(s$fatores); niv <- s$fatores[[1]]; t <- length(niv); r <- s$r
  if (r < 2L) .tr_exp_sem_residuo("dic", "uma repetição por tratamento")
  idx <- .tr_exp_sorteio_dic(t, r)
  u <- tibble::tibble(unidade = seq_along(idx))
  u[[nome]] <- factor(niv[idx], levels = niv)
  list(unidades = u,
       fatores = .tr_exp_fator(nome, "tratamento", niv, "parcela", "todas as parcelas", "livre"),
       hierarquia = .tr_exp_hier("parcela", "unidade", NA, nrow(u)),
       posicoes = .tr_exp_grade_auto(nrow(u), s$colunas_grade), eixos = c("linha da grade", "coluna da grade"),
       analise = list(no = "models/anova_dic", params = list(tratamento = nome)),
       rotulo = sprintf("DIC · %d tratamentos × %d repetições", t, r))
}

# ---- DBC ----------------------------------------------------------------------

.tr_exp_estr_dbc <- function(s) {
  nome <- names(s$fatores); niv <- s$fatores[[1]]; t <- length(niv); r <- s$r
  if (r < 2L) .tr_exp_sem_residuo("dbc", "um bloco só")
  d <- .tr_exp_sorteio_dbc(t, r)
  u <- tibble::tibble(unidade = seq_len(nrow(d)), bloco = .tr_exp_id(d$bloco, r))
  u[[nome]] <- factor(niv[d$trat], levels = niv)
  list(unidades = u,
       fatores = rbind(
         .tr_exp_fator("bloco", "bloco", seq_len(r), "bloco", "—", "sem sorteio",
                       "o bloco agrupa parcelas parecidas; ele é escolhido, não sorteado"),
         .tr_exp_fator(nome, "tratamento", niv, "parcela", "dentro do bloco", "restrito")),
       hierarquia = .tr_exp_hier(c("bloco", "parcela"), c("bloco", "unidade"), c(NA, "bloco"), c(r, nrow(u))),
       posicoes = data.frame(linha = d$bloco, coluna = d$posicao), eixos = c("bloco", "posição no bloco"),
       analise = list(no = "models/anova_dbc", params = list(tratamento = nome, bloco = "bloco")),
       rotulo = sprintf("DBC · %d tratamentos × %d blocos", t, r))
}

# ---- quadrado latino ----------------------------------------------------------

.tr_exp_estr_dql <- function(s) {
  nome <- names(s$fatores); niv <- s$fatores[[1]]; t <- length(niv)
  if (t < 3L) .tr_exp_sem_residuo("dql", "quadrado latino 2 × 2")
  # Fisher & Yates: um quadrado padrão sorteado entre todos, depois linhas,
  # colunas e letras permutadas (R/combinatoria.R). Uniforme até t = 6;
  # aproximadamente uniforme (Jacobson & Matthews) de t = 7 em diante.
  M <- .tr_exp_quadrado_latino(t)
  rot <- seq_len(t)
  lin <- rep(seq_len(t), each = t); col <- rep(seq_len(t), t)
  u <- tibble::tibble(unidade = seq_len(t * t), linha = .tr_exp_id(lin, t), coluna = .tr_exp_id(col, t))
  u[[nome]] <- factor(niv[rot[M[cbind(lin, col)]]], levels = niv)
  avisos <- c(
    if (s$r_dado && s$r != t) {
      sprintf("No quadrado latino o número de repetições é o de tratamentos (%d); 'repeticoes' foi ignorado.", t)
    },
    if (t >= 7L) {
      "Com t ≥ 7 o quadrado vem da cadeia de Jacobson & Matthews: sorteio aproximadamente uniforme entre todos os quadrados latinos, não exatamente."
    })
  list(unidades = u,
       fatores = rbind(
         .tr_exp_fator("linha", "bloco", seq_len(t), "linha", "—", "sem sorteio",
                       "a linha é uma fonte de variação controlada, não um tratamento"),
         .tr_exp_fator("coluna", "bloco", seq_len(t), "coluna", "—", "sem sorteio",
                       "a coluna é uma fonte de variação controlada, não um tratamento"),
         .tr_exp_fator(nome, "tratamento", niv, "parcela", "cada linha e cada coluna", "restrito")),
       hierarquia = .tr_exp_hier(c("linha × coluna", "parcela"), c(NA, "unidade"), c(NA, "linha × coluna"),
                                 c(t * t, t * t)),
       posicoes = data.frame(linha = lin, coluna = col), eixos = c("linha", "coluna"),
       analise = list(no = "models/anova_dql", params = list(tratamento = nome, linha = "linha", coluna = "coluna")),
       avisos = avisos,
       rotulo = sprintf("Quadrado latino %d × %d", t, t))
}

# ---- fatorial -----------------------------------------------------------------

.tr_exp_formula_fatores <- function(nomes, prefixo = NULL) {
  paste0("~ ", paste(c(prefixo, paste(.tr_exp_bt(nomes), collapse = " * ")), collapse = " + "))
}

.tr_exp_analise_fatorial <- function(nomes, bloco) {
  if (length(nomes) <= 3L) {
    p <- list(fatores = paste(nomes, collapse = ", "))
    if (!is.null(bloco)) p$bloco <- bloco
    return(list(no = "models/anova_factorial", params = p))
  }
  list(no = "models/lm", params = list(formula = .tr_exp_formula_fatores(nomes, bloco)))
}

.tr_exp_estr_fatorial <- function(s) {
  f <- s$fatores; nomes <- names(f); r <- s$r
  cmb <- .tr_exp_combinacoes(f); nt <- nrow(cmb)
  base <- s$delineamento_base
  if (r < 2L) .tr_exp_sem_residuo("fatorial", "uma repetição por combinação")
  fat_meta <- do.call(rbind, lapply(nomes, function(nm) {
    .tr_exp_fator(nm, "tratamento", f[[nm]], "parcela",
                  if (base == "dbc") "dentro do bloco (a combinação inteira)" else "todas as parcelas (a combinação inteira)",
                  if (base == "dbc") "restrito" else "livre")
  }))
  if (base == "dic") {
    idx <- .tr_exp_sorteio_dic(nt, r)
    u <- tibble::tibble(unidade = seq_along(idx))
    pos <- .tr_exp_grade_auto(length(idx), s$colunas_grade); eix <- c("linha da grade", "coluna da grade")
    hier <- .tr_exp_hier("parcela", "unidade", NA, length(idx)); blc <- NULL
  } else {
    d <- .tr_exp_sorteio_dbc(nt, r); idx <- d$trat
    u <- tibble::tibble(unidade = seq_along(idx), bloco = .tr_exp_id(d$bloco, r))
    pos <- data.frame(linha = d$bloco, coluna = d$posicao); eix <- c("bloco", "posição no bloco")
    hier <- .tr_exp_hier(c("bloco", "parcela"), c("bloco", "unidade"), c(NA, "bloco"), c(r, length(idx)))
    fat_meta <- rbind(.tr_exp_fator("bloco", "bloco", seq_len(r), "bloco", "—", "sem sorteio",
                                    "o bloco agrupa parcelas parecidas; ele é escolhido, não sorteado"), fat_meta)
    blc <- "bloco"
  }
  for (nm in nomes) u[[nm]] <- cmb[[nm]][idx]
  avisos <- if (length(nomes) > 3L) {
    "Com mais de 3 fatores, 'models/anova_factorial' não serve; a análise sugerida é 'models/lm' com a fórmula completa."
  }
  list(unidades = u, fatores = fat_meta, hierarquia = hier, posicoes = pos, eixos = eix,
       analise = .tr_exp_analise_fatorial(nomes, blc), avisos = avisos,
       rotulo = sprintf("Fatorial %s em %s · %d repetições", paste(lengths(f), collapse = " × "),
                        toupper(base), r))
}

# ---- 2^k: letras, palavras e relação de definição -----------------------------

#' Lê uma palavra ("ABC", "-ABD") em índices de fator e sinal.
#' @noRd
.tr_exp_palavra <- function(txt, k, param) {
  txt <- gsub("\\s+", "", toupper(txt))
  sinal <- if (startsWith(txt, "-")) -1L else 1L
  txt <- sub("^[+-]", "", txt)
  letras <- strsplit(txt, "")[[1]]
  validas <- LETTERS[seq_len(k)]
  if (!length(letras) || !all(letras %in% validas) || anyDuplicated(letras)) {
    .tr_experiments_abort("tr_experiments_error_bad_generator",
                          "Param '%s': '%s' não é uma palavra válida; use as letras %s (uma por fator, na ordem declarada), sem repetir.",
                          param, txt, paste(validas, collapse = ""))
  }
  list(idx = sort(match(letras, LETTERS)), sinal = sinal)
}

.tr_exp_nome_palavra <- function(idx) paste(LETTERS[sort(idx)], collapse = "")

#' Todos os produtos (diferença simétrica) de um conjunto de palavras: o
#' subgrupo gerado, sem a identidade. Com q palavras independentes saem 2^q − 1.
#' @noRd
.tr_exp_subgrupo <- function(palavras) {
  q <- length(palavras); out <- list()
  for (m in seq_len(2^q - 1)) {
    usa <- which(bitwAnd(m, 2^(seq_len(q) - 1)) > 0)
    idx <- integer(); sinal <- 1L
    for (j in usa) {
      idx <- c(setdiff(idx, palavras[[j]]$idx), setdiff(palavras[[j]]$idx, idx))
      sinal <- sinal * palavras[[j]]$sinal
    }
    out[[m]] <- list(idx = sort(idx), sinal = sinal)
  }
  out
}

.tr_exp_romano <- function(n) as.character(utils::as.roman(n))

# ---- fatorial com confundimento -----------------------------------------------

.tr_exp_estr_confundimento <- function(s) {
  f <- s$fatores; nomes <- names(f); k <- length(f); r <- s$r
  if (!.tr_exp_preenchido(s$confundir)) {
    .tr_experiments_abort("tr_experiments_error_blank_param",
                          "Param 'confundir': diga qual efeito vai para os blocos (ex.: 'ABC').")
  }
  pal <- lapply(.tr_exp_split(s$confundir, ";"), .tr_exp_palavra, k = k, param = "confundir")
  grupo <- .tr_exp_subgrupo(pal)
  chaves <- vapply(grupo, function(g) .tr_exp_nome_palavra(g$idx), "")
  if (any(!nzchar(chaves)) || anyDuplicated(chaves)) {
    .tr_experiments_abort("tr_experiments_error_bad_generator",
                          "Param 'confundir': os efeitos '%s' não são independentes (um é produto dos outros).",
                          s$confundir)
  }
  if (any(lengths(lapply(grupo, `[[`, "idx")) < 2L)) {
    .tr_experiments_abort("tr_experiments_error_bad_generator",
                          "Param 'confundir': confundir %s com blocos sacrifica um efeito principal. Escolha interações.",
                          paste(chaves[lengths(lapply(grupo, `[[`, "idx")) < 2L], collapse = ", "))
  }
  q <- length(pal); nb <- 2L^q
  cmb <- .tr_exp_combinacoes(f)
  x01 <- sapply(nomes, function(nm) as.integer(cmb[[nm]]) - 1L)
  if (is.null(dim(x01))) x01 <- matrix(x01, nrow = 1L)
  grp <- 1L + as.integer(Reduce(`+`, lapply(seq_len(q), function(j) {
    (rowSums(x01[, pal[[j]]$idx, drop = FALSE]) %% 2L) * 2L^(j - 1L)
  })))
  por_bloco <- 2L^k / nb
  linhas <- list()
  for (rep_i in seq_len(r)) {
    fisico <- .tr_exp_perm(seq_len(nb))
    for (g in seq_len(nb)) {
      b <- (rep_i - 1L) * nb + fisico[[g]]
      membros <- .tr_exp_perm(which(grp == g))
      linhas[[length(linhas) + 1L]] <- data.frame(repeticao = rep_i, bloco = b, posicao = seq_along(membros),
                                                  comb = membros)
    }
  }
  d <- do.call(rbind, linhas); d <- d[order(d$bloco, d$posicao), ]
  u <- tibble::tibble(unidade = seq_len(nrow(d)), repeticao = .tr_exp_id(d$repeticao, r),
                      bloco = .tr_exp_id(d$bloco, r * nb))
  for (nm in nomes) u[[nm]] <- cmb[[nm]][d$comb]
  n_ef <- 2L^k - 1L - length(grupo)
  gl_res <- nrow(u) - r * nb - n_ef
  avisos <- c(
    if (any(lengths(lapply(grupo, `[[`, "idx")) == 2L)) {
      sprintf("Uma interação dupla (%s) fica confundida com blocos: ela não poderá ser testada.",
              paste(chaves[lengths(lapply(grupo, `[[`, "idx")) == 2L], collapse = ", "))
    },
    if (gl_res <= 0L) {
      "Sem repetição suficiente, não sobra resíduo: use as interações de ordem alta como erro, ou aumente 'repeticoes'."
    })
  letras <- stats::setNames(nomes, LETTERS[seq_len(k)])
  fat_meta <- rbind(
    .tr_exp_fator("bloco", "bloco", seq_len(r * nb), "bloco", "—", "sem sorteio",
                  "o bloco agrupa parcelas parecidas; ele é escolhido, não sorteado"),
    do.call(rbind, lapply(nomes, function(nm) .tr_exp_fator(
      nm, "tratamento", f[[nm]], "parcela",
      "dentro do bloco (qual combinação vai a qual bloco é fixado pelo confundimento)", "restrito"))))
  list(unidades = u, fatores = fat_meta,
       hierarquia = .tr_exp_hier(c("repetição", "bloco", "parcela"), c("repeticao", "bloco", "unidade"),
                                 c(NA, "repetição", "bloco"), c(r, r * nb, nrow(u))),
       posicoes = data.frame(linha = d$bloco, coluna = d$posicao), eixos = c("bloco", "posição no bloco"),
       analise = .tr_exp_analise_fatorial(nomes, "bloco"), avisos = avisos,
       extras = list(letras = letras, confundidos = chaves, blocos_por_repeticao = nb,
                     parcelas_por_bloco = por_bloco),
       rotulo = sprintf("2^%d em %d blocos de %d · confundido %s", k, nb, por_bloco, paste(chaves, collapse = ", ")))
}

# ---- fatorial fracionado 2^(k-p) ----------------------------------------------

#' A matriz padrão (ordem de Yates, primeiro fator alternando mais rápido) de
#' um fracionado, com a relação de definição e a resolução. Exposta à parte
#' porque é determinística — é ela que o teste confere contra o `FrF2`.
#' @noRd
.tr_exp_fracao <- function(k, geradores) {
  gs <- .tr_exp_split(geradores, ";")
  if (!length(gs)) {
    .tr_experiments_abort("tr_experiments_error_blank_param",
                          "Param 'geradores': diga os geradores do fracionado (ex.: 'D = ABC').")
  }
  lados <- lapply(gs, function(g) {
    p <- trimws(strsplit(g, "=", fixed = TRUE)[[1]])
    if (length(p) != 2L || !grepl("^[A-Za-z]$", p[[1]])) {
      .tr_experiments_abort("tr_experiments_error_bad_generator",
                            "Param 'geradores': '%s' não segue 'LETRA = PALAVRA' (ex.: 'D = ABC').", g)
    }
    list(lhs = match(toupper(p[[1]]), LETTERS), rhs = .tr_exp_palavra(p[[2]], k, "geradores"))
  })
  lhs <- vapply(lados, `[[`, 0L, "lhs")
  p <- length(lhs)
  if (any(is.na(lhs)) || any(lhs > k) || anyDuplicated(lhs) || p >= k) {
    .tr_experiments_abort("tr_experiments_error_bad_generator",
                          "Param 'geradores': cada gerador define um fator diferente entre %s, e sobra ao menos um fator básico.",
                          paste(LETTERS[seq_len(k)], collapse = ""))
  }
  basicos <- setdiff(seq_len(k), lhs)
  for (l in lados) {
    if (!all(l$rhs$idx %in% basicos) || length(l$rhs$idx) < 2L) {
      .tr_experiments_abort("tr_experiments_error_bad_generator",
                            "Param 'geradores': '%s = %s' tem de usar só fatores básicos (%s), ao menos dois.",
                            LETTERS[l$lhs], .tr_exp_nome_palavra(l$rhs$idx),
                            paste(LETTERS[basicos], collapse = ""))
    }
  }
  n <- 2L^length(basicos)
  X <- matrix(0L, n, k)
  for (j in seq_along(basicos)) X[, basicos[[j]]] <- ifelse(((seq_len(n) - 1L) %/% 2L^(j - 1L)) %% 2L == 0L, -1L, 1L)
  for (l in lados) X[, l$lhs] <- l$rhs$sinal * apply(X[, l$rhs$idx, drop = FALSE], 1L, prod)
  palavras <- lapply(lados, function(l) list(idx = sort(c(l$lhs, l$rhs$idx)), sinal = l$rhs$sinal))
  def <- .tr_exp_subgrupo(palavras)
  comp <- vapply(def, function(w) length(w$idx), 0L)
  rel <- paste(c("I", paste0(ifelse(vapply(def, `[[`, 0L, "sinal") < 0L, "-", ""),
                             vapply(def, function(w) .tr_exp_nome_palavra(w$idx), ""))), collapse = " = ")
  # Aliases dos efeitos principais e das interações duplas: E × palavra.
  efeitos <- c(as.list(seq_len(k)), utils::combn(k, 2L, simplify = FALSE))
  aliases <- vapply(efeitos, function(e) {
    outros <- vapply(def, function(w) .tr_exp_nome_palavra(c(setdiff(e, w$idx), setdiff(w$idx, e))), "")
    paste(c(.tr_exp_nome_palavra(e), outros), collapse = " = ")
  }, "")
  list(X = X, relacao = rel, palavras = vapply(def, function(w) .tr_exp_nome_palavra(w$idx), ""),
       resolucao = min(comp), aliases = aliases, n = n, p = p)
}

.tr_exp_estr_fracionado <- function(s) {
  f <- s$fatores; nomes <- names(f); k <- length(f); r <- s$r
  fr <- .tr_exp_fracao(k, s$geradores)
  N <- fr$n * r
  padrao <- rep(seq_len(fr$n), r)
  ordem <- .tr_exp_perm(seq_len(N))
  u <- tibble::tibble(unidade = seq_len(N), padrao = padrao[ordem])
  if (r > 1L) u$repeticao <- .tr_exp_id(rep(seq_len(r), each = fr$n)[ordem], r)
  for (j in seq_len(k)) u[[nomes[[j]]]] <- as.numeric(fr$X[padrao[ordem], j])
  gl_efeitos <- k + if (fr$resolucao >= 5L) choose(k, 2) else 0
  termos <- if (fr$resolucao >= 5L) paste0("(", paste(.tr_exp_bt(nomes), collapse = " + "), ")^2")
            else paste(.tr_exp_bt(nomes), collapse = " + ")
  avisos <- c(
    if (fr$resolucao <= 3L) "Resolução III: efeitos principais confundidos com interações duplas.",
    if (N - 1L - gl_efeitos <= 0L) "O modelo sugerido não deixa resíduo: sem repetição, leia os efeitos num gráfico normal (Daniel) ou aumente 'repeticoes'.")
  fat_meta <- do.call(rbind, lapply(seq_len(k), function(j) {
    niv <- if (is.null(f[[j]])) c("-1", "1") else f[[j]]
    .tr_exp_fator(nomes[[j]], "tratamento", niv, "corrida", "ordem de todas as corridas", "livre",
                  if (!is.null(f[[j]])) sprintf("coluna codificada: -1 = %s, +1 = %s", f[[j]][[1]], f[[j]][[2]]) else "")
  }))
  codificacao <- do.call(rbind, lapply(seq_len(k), function(j) {
    niv <- if (is.null(f[[j]])) c("-1", "1") else f[[j]]
    data.frame(fator = nomes[[j]], menos1 = niv[[1]], mais1 = niv[[2]], stringsAsFactors = FALSE)
  }))
  list(unidades = u, fatores = fat_meta,
       hierarquia = .tr_exp_hier("corrida", "unidade", NA, N),
       posicoes = .tr_exp_grade_auto(N, s$colunas_grade), eixos = c("ordem de execução", "ordem de execução"),
       tipo_geo = "sequencia",
       analise = list(no = "models/lm", params = list(formula = paste("~", termos))),
       avisos = avisos,
       extras = list(letras = stats::setNames(nomes, LETTERS[seq_len(k)]), relacao_definicao = fr$relacao,
                     palavras = fr$palavras, resolucao = fr$resolucao, aliases = fr$aliases, geradores = s$geradores,
                     codificacao = codificacao),
       rotulo = sprintf("2^(%d−%d) resolução %s · %d corridas", k, fr$p, .tr_exp_romano(fr$resolucao), N))
}

# ---- composto central ---------------------------------------------------------

.tr_exp_estr_composto_central <- function(s) {
  f <- s$fatores; nomes <- names(f); k <- length(f); r <- s$r
  if (k < 2L || k > 6L) {
    .tr_experiments_abort("tr_experiments_error_bad_factors",
                          "O composto central aqui é para 2 a 6 fatores (vieram %d).", k)
  }
  # Porção fatorial: o 2^k completo, ou a fração dos 'geradores' (resolução
  # V ou mais, para que efeitos principais e interações duplas fiquem livres).
  fr <- NULL
  if (.tr_exp_preenchido(s$geradores)) {
    fr <- .tr_exp_fracao(k, s$geradores)
    if (fr$resolucao < 5L) {
      .tr_experiments_abort("tr_experiments_error_bad_generator",
                            "Param 'geradores': no composto central a porção fatorial precisa de resolução V ou mais (a fração dada tem %s, %s).",
                            .tr_exp_romano(fr$resolucao), fr$relacao)
    }
    cubo <- fr$X * 1; nf <- fr$n
  } else {
    nf <- 2L^k
    cubo <- sapply(seq_len(k), function(j) ifelse(((seq_len(nf) - 1L) %/% 2L^(j - 1L)) %% 2L == 0L, -1, 1))
  }
  da <- s$distancia_axial
  alfa <- if (is.numeric(da)) da else if (da == "rotacional") nf^(1 / 4) else 1
  tipo_alfa <- if (is.numeric(da)) "dado" else da
  axial <- matrix(0, 2L * k, k)
  for (j in seq_len(k)) axial[2L * j - 1L, j] <- -alfa; for (j in seq_len(k)) axial[2L * j, j] <- alfa
  centro <- matrix(0, s$pontos_centrais, k)
  X <- rbind(cubo, axial, centro)
  tipo <- rep(c("fatorial", "axial", "central"), c(nf, 2L * k, s$pontos_centrais))
  n1 <- nrow(X); N <- n1 * r
  padrao <- rep(seq_len(n1), r)
  ordem <- .tr_exp_perm(seq_len(N))
  u <- tibble::tibble(unidade = seq_len(N), padrao = padrao[ordem], tipo_ponto = tipo[padrao[ordem]])
  if (r > 1L) u$repeticao <- .tr_exp_id(rep(seq_len(r), each = n1)[ordem], r)
  for (j in seq_len(k)) u[[nomes[[j]]]] <- X[padrao[ordem], j]
  bt <- .tr_exp_bt(nomes)
  formula <- paste("~", paste(c(bt, sprintf("I(%s^2)", bt),
                                if (k > 1L) apply(utils::combn(bt, 2L), 2L, paste, collapse = ":")), collapse = " + "))
  avisos <- if (s$pontos_centrais == 0L) "Sem pontos centrais não há erro puro para testar a falta de ajuste."
  list(unidades = u,
       fatores = do.call(rbind, lapply(seq_len(k), function(j) .tr_exp_fator(
         nomes[[j]], "tratamento", c(-alfa, -1, 0, 1, alfa), "corrida", "ordem de todas as corridas", "livre"))),
       hierarquia = .tr_exp_hier("corrida", "unidade", NA, N),
       posicoes = .tr_exp_grade_auto(N, s$colunas_grade), eixos = c("ordem de execução", "ordem de execução"),
       tipo_geo = "sequencia",
       analise = list(no = "models/lm", params = list(formula = formula)), avisos = avisos,
       extras = list(alfa = alfa, tipo_alfa = tipo_alfa, n_fatorial = nf, n_axial = 2L * k,
                     n_central = s$pontos_centrais,
                     relacao_definicao = if (!is.null(fr)) fr$relacao, resolucao = if (!is.null(fr)) fr$resolucao),
       rotulo = sprintf("Composto central · %d fatores%s, α = %s (%s), %d centrais", k,
                        if (!is.null(fr)) sprintf(", fatorial 2^(%d−%d)", k, fr$p) else "",
                        format(signif(alfa, 4)), tipo_alfa, s$pontos_centrais))
}

# ---- parcelas subdivididas ----------------------------------------------------

.tr_exp_estr_parcela_subdividida <- function(s) {
  f <- s$fatores; nomes <- names(f); A <- nomes[[1]]; B <- nomes[[2]]
  na <- length(f[[A]]); nb <- length(f[[B]]); r <- s$r; base <- s$delineamento_base
  if (r < 2L) .tr_exp_sem_residuo("parcela_subdividida", "uma repetição só")
  # Estágio 1: o fator A nas parcelas (dentro do bloco, ou entre todas).
  pa <- if (base == "dbc") .tr_exp_sorteio_dbc(na, r)
        else data.frame(bloco = NA, posicao = seq_len(na * r), trat = .tr_exp_sorteio_dic(na, r))
  # Estágio 2: o fator B nas subparcelas, sorteado DENTRO de cada parcela.
  d <- do.call(rbind, lapply(seq_len(nrow(pa)), function(i) {
    data.frame(bloco = pa$bloco[[i]], parcela = i, a = pa$trat[[i]], sub = seq_len(nb), b = .tr_exp_perm(seq_len(nb)),
               pos_parc = pa$posicao[[i]])
  }))
  u <- tibble::tibble(unidade = seq_len(nrow(d)))
  if (base == "dbc") u$bloco <- .tr_exp_id(d$bloco, r)
  u$parcela <- .tr_exp_id(d$parcela, nrow(pa)); u$subparcela <- .tr_exp_id(d$sub, nb)
  u[[A]] <- factor(f[[A]][d$a], levels = f[[A]]); u[[B]] <- factor(f[[B]][d$b], levels = f[[B]])
  if (base == "dbc") {
    pos <- data.frame(linha = d$bloco, coluna = (d$pos_parc - 1L) * nb + d$sub)
    eix <- c("bloco", "subparcela (parcelas lado a lado)")
    hier <- .tr_exp_hier(c("bloco", "parcela", "subparcela"), c("bloco", "parcela", "unidade"),
                         c(NA, "bloco", "parcela"), c(r, nrow(pa), nrow(u)))
    analise <- list(no = "models/anova_split_plot", params = list(parcela = A, subparcela = B, bloco = "bloco"))
  } else {
    g <- .tr_exp_grade_auto(nrow(pa), s$colunas_grade)
    pos <- data.frame(linha = g$linha[d$parcela], coluna = (g$coluna[d$parcela] - 1L) * nb + d$sub)
    eix <- c("linha da grade", "subparcela (parcelas lado a lado)")
    hier <- .tr_exp_hier(c("parcela", "subparcela"), c("parcela", "unidade"), c(NA, "parcela"), c(nrow(pa), nrow(u)))
    analise <- list(no = "models/lmer", params = list(
      formula = sprintf("~ %s * %s + (1 | parcela)", .tr_exp_bt(A), .tr_exp_bt(B))))
  }
  fat_meta <- rbind(
    if (base == "dbc") .tr_exp_fator("bloco", "bloco", seq_len(r), "bloco", "—", "sem sorteio",
                                     "o bloco agrupa parcelas parecidas; ele é escolhido, não sorteado"),
    .tr_exp_fator(A, "tratamento", f[[A]], "parcela",
                  if (base == "dbc") "parcelas dentro do bloco" else "todas as parcelas", "em estágios"),
    .tr_exp_fator(B, "tratamento", f[[B]], "subparcela", "subparcelas dentro da parcela", "em estágios"))
  list(unidades = u, fatores = fat_meta, hierarquia = hier, posicoes = pos, eixos = eix, analise = analise,
       rotulo = sprintf("Parcela subdividida · %s (%d) na parcela, %s (%d) na subparcela, %d %s",
                        A, na, B, nb, r, if (base == "dbc") "blocos" else "repetições"))
}

# ---- faixas -------------------------------------------------------------------

.tr_exp_estr_faixas <- function(s) {
  f <- s$fatores; nomes <- names(f); A <- nomes[[1]]; B <- nomes[[2]]
  na <- length(f[[A]]); nb <- length(f[[B]]); r <- s$r
  if (r < 2L) .tr_exp_sem_residuo("faixas", "um bloco só")
  d <- do.call(rbind, lapply(seq_len(r), function(b) {
    pa <- .tr_exp_perm(seq_len(na)); pb <- .tr_exp_perm(seq_len(nb))
    data.frame(bloco = b, fl = rep(seq_len(na), each = nb), fc = rep(seq_len(nb), na),
               a = rep(pa, each = nb), b = rep(pb, na))
  }))
  u <- tibble::tibble(unidade = seq_len(nrow(d)), bloco = .tr_exp_id(d$bloco, r),
                      faixa_linha = .tr_exp_id(d$fl, na), faixa_coluna = .tr_exp_id(d$fc, nb))
  u[[A]] <- factor(f[[A]][d$a], levels = f[[A]]); u[[B]] <- factor(f[[B]][d$b], levels = f[[B]])
  fA <- .tr_exp_bt(A); fB <- .tr_exp_bt(B)
  list(unidades = u,
       fatores = rbind(
         .tr_exp_fator("bloco", "bloco", seq_len(r), "bloco", "—", "sem sorteio",
                       "o bloco agrupa parcelas parecidas; ele é escolhido, não sorteado"),
         .tr_exp_fator(A, "tratamento", f[[A]], "faixa horizontal", "faixas horizontais dentro do bloco", "restrito"),
         .tr_exp_fator(B, "tratamento", f[[B]], "faixa vertical",
                       "faixas verticais dentro do bloco (sorteio independente do de linhas)", "restrito")),
       hierarquia = .tr_exp_hier(c("bloco", "faixa horizontal × faixa vertical", "parcela"),
                                 c("bloco", NA, "unidade"), c(NA, "bloco", "cruzamento das faixas"),
                                 c(r, r * (na + nb), nrow(u))),
       posicoes = data.frame(linha = (d$bloco - 1L) * (na + 1L) + d$fl, coluna = d$fc),
       eixos = c("faixa horizontal (blocos empilhados)", "faixa vertical"),
       analise = list(no = "models/lmer", params = list(formula = sprintf(
         "~ %s * %s + (1 | bloco) + (1 | bloco:%s) + (1 | bloco:%s)", fA, fB, fA, fB))),
       rotulo = sprintf("Faixas · %s (%d) × %s (%d), %d blocos", A, na, B, nb, r))
}

# ---- blocos incompletos balanceados -------------------------------------------

# A base do BIB (cíclico, família de diferenças, tabela, busca ou não
# reduzido) está em R/combinatoria.R.

.tr_exp_estr_bib <- function(s) {
  nome <- names(s$fatores); niv <- s$fatores[[1]]; t <- length(niv); k <- s$tamanho_bloco; rr <- s$r
  if (k < 2L || k >= t) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                          "Param 'tamanho_bloco': num BIB fica entre 2 e t − 1 = %d (veio %d).", t - 1L, k)
  }
  base <- .tr_exp_bib_base(t, k)
  bl <- base$blocos[rep(seq_len(nrow(base$blocos)), rr), , drop = FALSE]
  b <- nrow(bl); r <- b * k / t; lam <- r * (k - 1) / (t - 1)
  rot <- .tr_exp_perm(seq_len(t)); ordem_b <- .tr_exp_perm(seq_len(b))
  d <- do.call(rbind, lapply(seq_len(b), function(j) {
    data.frame(bloco = j, posicao = seq_len(k), trat = rot[.tr_exp_perm(bl[ordem_b[[j]], ])])
  }))
  u <- tibble::tibble(unidade = seq_len(nrow(d)), bloco = .tr_exp_id(d$bloco, b))
  u[[nome]] <- factor(niv[d$trat], levels = niv)
  avisos <- if (base$construcao == "não reduzido") {
    sprintf("Nenhuma construção achou BIB menor para t = %d e k = %d: usado o não reduzido, com todos os %d blocos possíveis.",
            t, k, b / rr)
  }
  list(unidades = u,
       fatores = rbind(
         .tr_exp_fator("bloco", "bloco", seq_len(b), "bloco", "—", "sem sorteio",
                       "o bloco agrupa parcelas parecidas; qual conjunto de tratamentos vai a qual bloco é sorteado"),
         .tr_exp_fator(nome, "tratamento", niv, "parcela",
                       "dentro do bloco (e os conjuntos entre os blocos)", "restrito")),
       hierarquia = .tr_exp_hier(c("bloco", "parcela"), c("bloco", "unidade"), c(NA, "bloco"), c(b, nrow(u))),
       posicoes = data.frame(linha = d$bloco, coluna = d$posicao), eixos = c("bloco", "posição no bloco"),
       analise = list(no = "models/lm", params = list(formula = sprintf("~ bloco + %s", .tr_exp_bt(nome)))),
       avisos = avisos,
       extras = list(t = t, k = k, b = b, r = r, lambda = lam, eficiencia = lam * t / (r * k),
                     ciclico = base$construcao == "cíclico", construcao = base$construcao),
       rotulo = sprintf("BIB · t = %d, k = %d, b = %d, r = %g, λ = %g", t, k, b, r, lam))
}

# ---- medidas repetidas --------------------------------------------------------

.tr_exp_estr_medidas_repetidas <- function(s) {
  nome <- names(s$fatores); niv <- s$fatores[[1]]; t <- length(niv); r <- s$r
  tempos <- .tr_exp_split(s$tempos)
  if (length(tempos) < 2L) {
    .tr_experiments_abort("tr_experiments_error_blank_param",
                          "Param 'tempos': diga ao menos dois tempos de medida (ex.: '0, 30, 60').")
  }
  if (r < 2L) .tr_exp_sem_residuo("medidas_repetidas", "um indivíduo por tratamento")
  ind <- .tr_exp_sorteio_dic(t, r); ni <- length(ind); nt <- length(tempos)
  u <- tibble::tibble(unidade = seq_len(ni * nt), individuo = .tr_exp_id(rep(seq_len(ni), each = nt), ni))
  u[[nome]] <- factor(niv[rep(ind, each = nt)], levels = niv)
  u$tempo <- factor(rep(tempos, ni), levels = tempos)
  list(unidades = u,
       fatores = rbind(
         .tr_exp_fator(nome, "tratamento", niv, "indivíduo", "todos os indivíduos", "livre"),
         .tr_exp_fator("tempo", "tempo", tempos, "medida", "—", "sem sorteio",
                       "o tempo transcorre na mesma ordem para todos; não há como sorteá-lo")),
       hierarquia = .tr_exp_hier(c("indivíduo", "medida"), c("individuo", "unidade"), c(NA, "indivíduo"),
                                 c(ni, nrow(u))),
       posicoes = data.frame(linha = rep(seq_len(ni), each = nt), coluna = rep(seq_len(nt), ni)),
       eixos = c("indivíduo", "tempo"), tipo_geo = "tempo",
       analise = list(no = "models/lmer", params = list(
         formula = sprintf("~ %s * tempo + (1 | individuo)", .tr_exp_bt(nome)))),
       rotulo = sprintf("Medidas repetidas · %d tratamentos × %d indivíduos × %d tempos", t, r, nt))
}

# ---- crossover (Williams) -----------------------------------------------------

#' As sequências de Williams (1949): com t par, um quadrado; com t ímpar, o
#' quadrado e o seu espelho (2t sequências). Cada tratamento é precedido por
#' cada outro o mesmo número de vezes — o balanço para o efeito residual.
#' @noRd
.tr_exp_williams <- function(t) {
  s0 <- integer(t); lo <- 1L; hi <- t - 1L
  for (i in seq_len(t)[-1L]) {
    if (i %% 2L == 0L) { s0[[i]] <- lo; lo <- lo + 1L } else { s0[[i]] <- hi; hi <- hi - 1L }
  }
  W <- t(sapply(0:(t - 1L), function(i) (s0 + i) %% t + 1L))
  if (t == 1L) W <- matrix(1L)
  if (t %% 2L == 1L) W <- rbind(W, W[, t:1, drop = FALSE])
  W
}

.tr_exp_estr_crossover <- function(s) {
  nome <- names(s$fatores); niv <- s$fatores[[1]]; t <- length(niv); r <- s$r
  if (nome == "residual") {
    .tr_experiments_abort("tr_experiments_error_reserved_name",
                          "Fator 'residual': no crossover esse nome é a coluna do tratamento do período anterior. Use outro nome.")
  }
  W <- .tr_exp_williams(t); ns <- nrow(W)
  rot <- .tr_exp_perm(seq_len(t))
  seq_ind <- .tr_exp_perm(rep(seq_len(ns), each = r)); ni <- length(seq_ind)
  u <- tibble::tibble(unidade = seq_len(ni * t), individuo = .tr_exp_id(rep(seq_len(ni), each = t), ni),
                      sequencia = .tr_exp_id(rep(seq_ind, each = t), ns),
                      periodo = .tr_exp_id(rep(seq_len(t), ni), t))
  u[[nome]] <- factor(niv[rot[as.vector(t(W[seq_ind, , drop = FALSE]))]], levels = niv)
  # Efeito residual (carryover): o tratamento do período anterior. No 1º
  # período não há residual (λ = 0 em Jones & Kenward). Um nível "nenhum"
  # coincidiria com o 1º período (coluna redundante, que o lme4 descarta com
  # aviso). Como os residuais só são estimáveis como diferenças e o efeito do
  # período absorve qualquer constante do 1º período, a coluna leva ali o
  # nível de REFERÊNCIA (o 1º tratamento): o espaço ajustado, o F de t − 1 gl
  # e as diferenças λⱼ − λₖ são os mesmos, e a matriz tem posto completo.
  ant <- c(NA, as.character(u[[nome]])[-nrow(u)])
  ant[u$periodo == "1"] <- niv[[1]]
  u$residual <- factor(ant, levels = niv)
  seqs <- apply(W, 1L, function(w) paste(niv[rot[w]], collapse = " → "))
  list(unidades = u,
       fatores = rbind(
         .tr_exp_fator("sequencia", "tratamento", seq_len(ns), "indivíduo", "todos os indivíduos", "livre"),
         .tr_exp_fator(nome, "tratamento", niv, "período do indivíduo",
                       "fixado pela sequência sorteada ao indivíduo", "restrito"),
         .tr_exp_fator("periodo", "tempo", seq_len(t), "período", "—", "sem sorteio",
                       "os períodos se sucedem no tempo; o balanço vem das sequências"),
         .tr_exp_fator("residual", "residual", niv, "período do indivíduo", "—", "sem sorteio",
                       sprintf("o tratamento do período anterior (efeito residual); no 1º período, a referência '%s'", niv[[1]]))),
       hierarquia = .tr_exp_hier(c("sequência", "indivíduo", "período"), c("sequencia", "individuo", "unidade"),
                                 c(NA, "sequência", "indivíduo"), c(ns, ni, nrow(u))),
       posicoes = data.frame(linha = rep(seq_len(ni), each = t), coluna = rep(seq_len(t), ni)),
       eixos = c("indivíduo", "período"), tipo_geo = "tempo",
       analise = list(no = "models/lmer", params = list(
         formula = sprintf("~ periodo + %s + residual + (1 | individuo)", .tr_exp_bt(nome)))),
       extras = list(sequencias = seqs, williams = W),
       rotulo = sprintf("Crossover de Williams · %d tratamentos, %d sequências, %d indivíduos", t, ns, ni))
}

# ---- grupos de experimentos ---------------------------------------------------

.tr_exp_estr_grupos <- function(s) {
  L <- s$locais
  if (L < 2L) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                          "Param 'locais': um grupo de experimentos pede ao menos 2 locais (veio %d).", L)
  }
  # O MESMO delineamento em cada local, com sorteio independente: a
  # construção de um local é chamada L vezes.
  sub <- if (length(s$fatores) > 1L) .tr_exp_estr_fatorial
         else if (s$delineamento_base == "dbc") .tr_exp_estr_dbc else .tr_exp_estr_dic
  partes <- lapply(seq_len(L), function(l) sub(s))
  alt <- max(vapply(partes, function(p) max(p$posicoes$linha), 0))
  u <- do.call(rbind, lapply(seq_len(L), function(l) {
    x <- partes[[l]]$unidades; x$local <- l; x
  }))
  u$local <- .tr_exp_id(u$local, L); u$unidade <- seq_len(nrow(u))
  u <- u[, c("unidade", "local", setdiff(names(u), c("unidade", "local")))]
  pos <- do.call(rbind, lapply(seq_len(L), function(l) {
    p <- partes[[l]]$posicoes; p$linha <- p$linha + (l - 1L) * (alt + 1L); p
  }))
  p1 <- partes[[1]]
  trat <- p1$fatores$nome[p1$fatores$papel == "tratamento"]
  bloco <- "bloco" %in% names(u)
  fat_meta <- rbind(
    .tr_exp_fator("local", "agrupamento", seq_len(L), "local", "—", "sem sorteio",
                  "o local é escolhido (ou amostrado), não sorteado às parcelas"),
    transform(p1$fatores, escopo = paste(p1$fatores$escopo, "(em cada local, sorteio independente)")))
  fat_meta$escopo[fat_meta$mecanismo == "sem sorteio"] <- "—"
  hier <- rbind(.tr_exp_hier("local", "local", NA, L),
                transform(p1$hierarquia, n = p1$hierarquia$n * L,
                          dentro_de = ifelse(is.na(p1$hierarquia$dentro_de), "local", p1$hierarquia$dentro_de)))
  ft <- paste(.tr_exp_bt(trat), collapse = " * ")
  # Cada termo de tratamento (efeitos principais e TODAS as interações)
  # interage com o local: A:B é testado contra local:A:B, não contra o resíduo.
  termos <- unlist(lapply(seq_along(trat), function(m) {
    apply(utils::combn(.tr_exp_bt(trat), m), 2L, paste, collapse = ":")
  }))
  formula <- sprintf("~ %s + (1 | local)%s + %s", ft, if (bloco) " + (1 | local:bloco)" else "",
                     paste(sprintf("(1 | local:%s)", termos), collapse = " + "))
  list(unidades = tibble::as_tibble(u), fatores = fat_meta, hierarquia = hier, posicoes = pos,
       eixos = c(paste(p1$eixos[[1]], "(locais empilhados)"), p1$eixos[[2]]),
       analise = list(no = "models/lmer", params = list(formula = formula)),
       avisos = unique(unlist(lapply(partes, `[[`, "avisos"))),
       rotulo = sprintf("Grupo de %d experimentos · %s", L, p1$rotulo))
}
