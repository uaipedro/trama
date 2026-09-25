# Teste F da hipótese linear geral: H0: Lβ = c.
#
# Um bloco, uma hipótese nula — a CONJUNTA. Os contrastes digitados são as
# linhas de L, e o F testa todas ao mesmo tempo, com tantos graus de liberdade
# no numerador quanto linhas independentes. Cada contraste sozinho, com a sua
# estimativa, vai para o detalhe: é o que se lê depois de rejeitar a conjunta.
#
# Dois jeitos de escrever L, porque são duas perguntas de quem modela:
#
# - **nas médias de um fator** (`fator` preenchido): os contrastes clássicos da
#   experimentação — `ctrl vs trat: 2 -1 -1`, ou pelos nomes dos níveis,
#   `H3 - (H1 + H2) / 2`. Passa pelo `emmeans`, que conhece a média ajustada de
#   cada nível em qualquer modelo da coleção.
# - **nos coeficientes** (`fator` em branco): `wt = 0; hp = 0`, `sprayB = sprayF`,
#   na sintaxe do `car::linearHypothesis`, com os nomes que o
#   `models/coefficients` mostra.

#' Separa o texto em linhas de hipótese: `;` ou quebra de linha.
#' @noRd
.tr_models_linhas_hip <- function(texto) {
  x <- trimws(unlist(strsplit(texto, "[;\n]")))
  x[nzchar(x)]
}

#' "nome: corpo" -> list(nome, corpo). O nome é opcional.
#'
#' Só o PRIMEIRO `:` separa, e só quando o que vem antes não parece expressão
#' — `trt1:trt2` numa fórmula de coeficientes é interação, não rótulo. Por isso
#' o rótulo não pode conter operador nem crase.
#' @noRd
.tr_models_rotulo_hip <- function(linha, i) {
  m <- regmatches(linha, regexec("^([^:=`*/+()-][^:=`*/+()]*?)\\s*:\\s*(.+)$", linha))[[1]]
  if (length(m) == 3L && !grepl("^[-0-9., ]+$", m[[2]])) return(list(nome = trimws(m[[2]]), corpo = trimws(m[[3]])))
  list(nome = sprintf("contraste %d", i), corpo = linha)
}

#' Um contraste nas médias: números, ou expressão nos nomes dos níveis.
#'
#' Na expressão, cada nível é o vetor unitário da sua posição: `H3 - H1` vira
#' `(-1, 0, 1, 0, 0)` sem parser próprio — a aritmética do R faz o resto, com
#' frações e parênteses de graça. Nível com nome não sintático (`0.2cwt`) vai
#' entre crases.
#' @noRd
.tr_models_coef_contraste <- function(corpo, niveis, no) {
  k <- length(niveis)
  if (grepl("^[-+0-9.,/ \t]+$", corpo)) {
    toks <- strsplit(trimws(corpo), "[, \t]+")[[1]]
    v <- vapply(toks, function(t) {
      r <- tryCatch(eval(parse(text = t), baseenv()), error = function(e) NA_real_)
      if (is.numeric(r) && length(r) == 1L) r else NA_real_
    }, 1)
    if (anyNA(v) || length(v) != k) {
      .tr_models_abort("tr_models_error_bad_option",
                       "'%s': o contraste '%s' precisa de %d números, um por nível (%s).",
                       no, corpo, k, paste(niveis, collapse = ", "))
    }
    return(unname(v))
  }
  env <- new.env(parent = baseenv())
  for (i in seq_len(k)) assign(niveis[[i]], replace(numeric(k), i, 1), envir = env)
  r <- tryCatch(eval(parse(text = corpo), env), error = function(e) {
    .tr_models_abort("tr_models_error_bad_option",
                     paste0("'%s': o contraste '%s' não se lê (%s). Use números, um por nível, ou os ",
                            "nomes dos níveis (%s), com crase nos que não são nomes simples."),
                     no, corpo, conditionMessage(e), paste(niveis, collapse = ", "))
  })
  if (!is.numeric(r) || length(r) != k) {
    .tr_models_abort("tr_models_error_bad_option",
                     "'%s': o contraste '%s' não cita nenhum nível de %s.", no, corpo, paste(niveis, collapse = ", "))
  }
  r
}

#' Confere que as linhas de L são independentes.
#'
#' Linha que é combinação das outras não acrescenta hipótese, e o F calculado
#' com ela teria um grau de liberdade que não existe. O `emmeans` descarta em
#' silêncio; aqui o card diz qual escrever de outro jeito.
#' @noRd
.tr_models_posto <- function(L, nomes, no) {
  if (qr(L)$rank < nrow(L)) {
    .tr_models_abort("tr_models_error_bad_option",
                     paste0("'%s': os contrastes (%s) são linearmente dependentes — algum é ",
                            "combinação dos outros. Tire o redundante."), no, paste(nomes, collapse = "; "))
  }
  invisible(L)
}

#' Teste F da hipótese linear geral.
#' @param modelo objeto `tr_models_fit`.
#' @param hipoteses as linhas de L, separadas por `;` ou quebra de linha, cada
#'   uma com rótulo opcional (`"nome: ..."`).
#' @param fator coluna-fator cujas médias os contrastes combinam; em branco, as
#'   hipóteses são sobre os coeficientes.
#' @return objeto `tr_models_test`.
#' @export
tr_models_linear_hypothesis <- function(modelo, hipoteses = "", fator = "") {
  .tr_models_fit_conferir(modelo)
  no <- "models/linear_hypothesis"
  texto <- .tr_models_obrigatorio(hipoteses, "hipoteses")
  linhas <- lapply(seq_along(.tr_models_linhas_hip(texto)),
                   function(i) .tr_models_rotulo_hip(.tr_models_linhas_hip(texto)[[i]], i))
  nomes <- vapply(linhas, `[[`, "", "nome")
  if (anyDuplicated(nomes)) {
    .tr_models_abort("tr_models_error_bad_option", "'%s': rótulo repetido: %s.", no,
                     paste(unique(nomes[duplicated(nomes)]), collapse = ", "))
  }
  if (.tr_models_preenchido(fator)) .tr_models_hip_medias(modelo, linhas, nomes, fator, no)
  else .tr_models_hip_coef(modelo, linhas, nomes, no)
}

#' O rótulo curto da H0 conjunta, para o card.
#' @noRd
.tr_models_h0_conjunta <- function(nomes, corpos) {
  partes <- ifelse(grepl("^contraste [0-9]+$", nomes), corpos, nomes)
  sprintf("%s = 0", paste(partes, collapse = " = "))
}

.tr_models_hip_medias <- function(modelo, linhas, nomes, fator, no) {
  f <- .tr_models_col(modelo$dados, fator, "fator")
  if (!is.factor(modelo$dados[[f]])) {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'%s': '%s' é numérica no modelo, e contraste de médias é entre níveis de ",
                            "fator. Escreva a hipótese nos coeficientes (deixe 'fator' em branco)."), no, f)
  }
  aj <- .tr_models_modelo_emm(modelo, no)
  args <- list(aj, specs = f, data = modelo$dados)
  if (modelo$classe %in% c("lmer", "split")) args$lmer.df <- "satterthwaite"
  r <- .tr_models_ajustar(.tr_models_capturar(do.call(emmeans::emmeans, args)), no)
  niveis <- as.character(summary(r$valor)[[f]])
  L <- do.call(rbind, lapply(linhas, function(l) .tr_models_coef_contraste(l$corpo, niveis, no)))
  rownames(L) <- nomes; colnames(L) <- niveis
  .tr_models_posto(L, nomes, no)
  ct <- .tr_models_ajustar(emmeans::contrast(r$valor, stats::setNames(lapply(seq_len(nrow(L)), function(i) L[i, ]), nomes)), no)
  cada <- as.data.frame(summary(ct))
  j <- as.data.frame(emmeans::test(ct, joint = TRUE))
  # O F sai da covariância dos contrastes, e não da tabela do `test()`: ela
  # arredonda o F.ratio em três casas, e o card mostraria 4,846 para um F de
  # 4,846088 que o quadro da ANOVA mostra inteiro. Os gl vêm de lá.
  est <- cada$estimate
  q <- length(est)
  wald <- as.numeric(t(est) %*% solve(stats::vcov(ct), est))
  assintotico <- is.infinite(j$df2[[1]])
  fstat <- wald / q
  pval <- if (assintotico) stats::pchisq(wald, q, lower.tail = FALSE)
          else stats::pf(fstat, q, j$df2[[1]], lower.tail = FALSE)
  nao_soma <- nomes[abs(rowSums(L)) > 1e-8]
  extra <- stats::setNames(as.list(sprintf("%s (p = %s)", .tr_models_fmt(cada$estimate, 4L),
                                           .tr_models_fmt_p(cada$p.value))), nomes)
  .tr_models_teste(
    "F da hipótese linear geral", .tr_models_h0_conjunta(nomes, vapply(linhas, `[[`, "", "corpo")),
    if (assintotico) wald else fstat, if (assintotico) "qui2" else "F", pval,
    gl = if (assintotico) as.character(q) else sprintf("%d; %s", q, .tr_models_gl(j$df2[[1]])),
    conclusao_sim = "algum contraste é diferente de zero",
    conclusao_nao = "não há evidência de que os contrastes difiram de zero",
    nota = .tr_models_nota(sprintf("%d contraste(s) nas médias de %s", nrow(L), f),
                           if (length(nao_soma)) sprintf("não somam zero (não são contrastes): %s", paste(nao_soma, collapse = ", ")) else "",
                           if (any(grepl("interaction", r$avisos))) "o fator participa de interação: as médias somam sobre o outro fator" else ""),
    extra = extra, fonte = "Searle (1971)")
}

.tr_models_hip_coef <- function(modelo, linhas, nomes, no) {
  .tr_models_exigir(modelo, c("lm", "glm", "lmer", "glmer"), no,
                    "Na parcela subdividida, escreva os contrastes nas médias de um fator (preencha 'fator').")
  aj <- modelo$ajuste
  beta <- if (modelo$classe %in% c("lmer", "glmer")) lme4::fixef(aj) else stats::coef(aj)
  corpos <- vapply(linhas, `[[`, "", "corpo")
  M <- tryCatch(suppressWarnings(car::makeHypothesis(names(beta), corpos)), error = function(e) {
    .tr_models_abort("tr_models_error_bad_option",
                     paste0("'%s': a hipótese não se lê nos coeficientes (%s). Os nomes disponíveis são: %s. ",
                            "Para contrastar níveis de um fator, preencha 'fator'."),
                     no, conditionMessage(e), paste(names(beta), collapse = ", "))
  })
  M <- matrix(M, nrow = length(corpos))
  L <- M[, seq_along(beta), drop = FALSE]; rhs <- M[, ncol(M)]
  colnames(L) <- names(beta); rownames(L) <- nomes
  .tr_models_posto(L, nomes, no)
  est <- as.vector(L %*% beta) - rhs
  extra <- stats::setNames(as.list(.tr_models_fmt(est, 4L)), nomes)
  h0 <- paste(corpos, collapse = "; ")
  base <- list(conclusao_sim = "a hipótese é rejeitada: algum contraste difere do valor dado",
               conclusao_nao = "não há evidência contra a hipótese", extra = extra,
               fonte = "Searle (1971); Fox & Weisberg (2019)")
  if (modelo$classe == "lmer") {
    if (any(abs(rhs) > 1e-12)) {
      .tr_models_abort("tr_models_error_not_applicable",
                       "'%s': no misto, escreva a hipótese com o lado direito zero (ex.: 'Days - 10 = 0' não; 'Days = 0').", no)
    }
    a <- .tr_models_ajustar(lmerTest::contest(aj, L, joint = TRUE), no)
    return(do.call(.tr_models_teste, c(list("F da hipótese linear geral", h0, a$`F value`[[1]], "F", a$`Pr(>F)`[[1]],
      gl = sprintf("%d; %s", as.integer(a$NumDF[[1]]), .tr_models_gl(a$DenDF[[1]])),
      nota = "gl do denominador por Satterthwaite"), base)))
  }
  usa_f <- modelo$classe == "lm" || stats::family(aj)$family %in% c("gaussian", "Gamma", "quasipoisson", "quasibinomial")
  a <- .tr_models_ajustar(as.data.frame(car::linearHypothesis(aj, L, rhs, test = if (usa_f) "F" else "Chisq")), no)
  if (usa_f) {
    do.call(.tr_models_teste, c(list("F da hipótese linear geral", h0, a$F[[2]], "F", a$`Pr(>F)`[[2]],
      gl = sprintf("%d; %d", as.integer(a$Df[[2]]), as.integer(a$Res.Df[[2]])), nota = ""), base))
  } else {
    do.call(.tr_models_teste, c(list("Wald da hipótese linear geral", h0, a$Chisq[[2]], "qui2", a$`Pr(>Chisq)`[[2]],
      gl = as.character(a$Df[[2]]), nota = "qui-quadrado de Wald: família de dispersão fixa"), base))
  }
}
