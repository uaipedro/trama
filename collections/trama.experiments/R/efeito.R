# O nó `experiments/effect`: somar UM termo à resposta que ainda não existe.
#
# Decisão de tipo: o plano com termos continua sendo `experiments/plan`. Um
# termo não muda o que o plano é (a tabela de unidades com o metadado); ele só
# acrescenta uma coluna `.ef_<nome>` às unidades e uma entrada em
# `plano$termos`. Assim a cadeia `design -> effect -> effect -> error` passa o
# mesmo tipo de ponta a ponta, a vista lê qualquer elo dela e o adaptador para
# `data/table` continua o mesmo. Os campos `termos` e `resposta` são OPCIONAIS
# no contrato (ver `R/type.R`): um plano recém-sorteado não os tem.
#
# Um nó só, com o tipo como param (um verbo, um bloco): intercepto, fixo,
# aleatório, interação, quantitativo e covariável são todos "somar uma
# contribuição". Os params são planos, como no `experiments/design`: cada tipo
# lê os seus, e a ajuda diz quais.
#
# ESCALA DOS CONTRASTES (o ponto em que se erra sem perceber): a magnitude de
# um contraste é o VALOR do contraste nos coeficientes mostrados, Σ cᵢ τᵢ. Os
# coeficientes são os inteiros que o `experiments/contrasts` também mostra
# (`-3 -1 1 3` para o linear de quatro níveis): quem declara "linear = 4" lê
# estimativa 4 na análise. A conversão em efeitos por nível é a solução de
# norma mínima de L τ = m, τ = Lᵀ (L Lᵀ)⁻¹ m; com contrastes ortogonais ela é
# Σⱼ mⱼ cⱼ / Σ cⱼ², e, como toda linha soma zero, os efeitos somam zero.

.TR_EXP_EF_TIPOS <- c("intercepto", "fixo", "aleatorio", "interacao", "quantitativo", "covariavel")
.TR_EXP_EF_CONJUNTOS <- c("por nível", "polinomiais", "helmert", "controle", "digitados")

.tr_exp_ef_abort <- function(fmt, ...) {
  .tr_experiments_abort("tr_experiments_error_bad_term", paste0("'experiments/effect': ", fmt), ...)
}

#' Chave de comparação de nomes: sem caixa, sem acento, sem espaço nas pontas.
#' @noRd
.tr_exp_ef_chave <- function(x) {
  chartr("áàâãéêíóôõúüç", "aaaaeeiooouuc", tolower(trimws(as.character(x))))
}

#' Lê `"a = 1, b = -2"` (ou separado por `;`) num vetor nomeado; `"4, 1, 0"`
#' vira vetor sem nomes.
#' @noRd
.tr_exp_ef_pares <- function(texto, param) {
  if (!.tr_exp_preenchido(texto)) return(numeric())
  txt <- gsub("\n", ";", as.character(texto), fixed = TRUE)
  itens <- .tr_exp_split(txt, if (grepl(";", txt, fixed = TRUE)) ";" else ",")
  nomes <- character(length(itens)); vals <- numeric(length(itens))
  for (i in seq_along(itens)) {
    partes <- strsplit(itens[[i]], "=", fixed = TRUE)[[1]]
    if (length(partes) > 2L) .tr_exp_ef_abort("param '%s': '%s' tem mais de um '='.", param, itens[[i]])
    v <- suppressWarnings(as.numeric(trimws(partes[[length(partes)]])))
    if (is.na(v)) {
      .tr_exp_ef_abort("param '%s': '%s' não termina num número (use ponto decimal: 'alta = 2.5').",
                       param, itens[[i]])
    }
    nomes[[i]] <- if (length(partes) == 2L) trimws(partes[[1]]) else ""
    vals[[i]] <- v
  }
  if (all(nzchar(nomes))) return(stats::setNames(vals, nomes))
  if (any(nzchar(nomes))) .tr_exp_ef_abort("param '%s': ou todos os itens têm nome ('a = 1'), ou nenhum.", param)
  vals
}

#' As colunas de um termo: `"bloco:parcela"` ou `"a, b"`, conferidas contra o
#' plano.
#' @noRd
.tr_exp_ef_colunas <- function(plano, fator) {
  if (!.tr_exp_preenchido(fator)) .tr_exp_ef_abort("o param 'fator' está em branco.")
  cols <- .tr_exp_split(gsub(":", ",", fator, fixed = TRUE))
  u <- plano$unidades
  falta <- setdiff(cols, names(u))
  if (length(falta)) {
    disp <- setdiff(names(u), c("unidade", "ordem", grep("^\\.", names(u), value = TRUE)))
    .tr_exp_ef_abort("'%s' não é coluna do plano (estrutura '%s'). Colunas: %s.%s",
                     paste(falta, collapse = ", "), plano$estrutura, paste(disp, collapse = ", "),
                     if (any(falta %in% .TR_EXP_RESERVADOS))
                       " Essa unidade não existe nesta estrutura (ex.: 'parcela' só há na parcela subdividida)." else "")
  }
  if (anyDuplicated(cols)) .tr_exp_ef_abort("'fator' repete coluna: %s.", fator)
  cols
}

#' Níveis de uma coluna, na ordem declarada.
#' @noRd
.tr_exp_ef_niveis <- function(v) if (is.factor(v)) levels(droplevels(v)) else as.character(sort(unique(v)))

#' Matriz de contrastes (linhas = contrastes, colunas = níveis) de um fator,
#' pelas MESMAS construções do `experiments/contrasts`.
#' @noRd
.tr_exp_ef_matriz <- function(niveis, conjunto, doses, controle, contrastes, fator) {
  no <- "experiments/effect"
  L <- switch(conjunto,
    polinomiais = .tr_exp_an_polinomiais(.tr_exp_an_x(niveis, doses, fator, no), rep(1L, length(niveis))),
    helmert = .tr_exp_an_helmert(niveis),
    controle = .tr_exp_an_controle(niveis, controle, no),
    digitados = .tr_exp_an_digitados(contrastes, niveis, no))
  L <- matrix(L, nrow = nrow(L), dimnames = list(rownames(L), niveis))
  if (any(abs(rowSums(L)) > 1e-8)) {
    .tr_exp_ef_abort("os coeficientes de %s não somam zero: não são contrastes.",
                     paste(rownames(L)[abs(rowSums(L)) > 1e-8], collapse = ", "))
  }
  if (qr(L)$rank < nrow(L)) .tr_exp_ef_abort("os contrastes de '%s' são linearmente dependentes.", fator)
  L
}

#' Magnitudes por nome (sem acento/caixa) ou por posição; as que faltam são 0.
#' @noRd
.tr_exp_ef_magnitudes <- function(L, magnitudes) {
  m <- .tr_exp_ef_pares(magnitudes, "magnitudes")
  if (!length(m)) .tr_exp_ef_abort("com conjunto de contrastes, diga as magnitudes (ex.: 'linear = 4, quadratico = 1').")
  if (is.null(names(m))) {
    if (length(m) != nrow(L)) {
      .tr_exp_ef_abort("vieram %d magnitudes sem nome para %d contrastes (%s).", length(m), nrow(L),
                       paste(rownames(L), collapse = ", "))
    }
    return(stats::setNames(m, rownames(L)))
  }
  i <- match(.tr_exp_ef_chave(names(m)), .tr_exp_ef_chave(rownames(L)))
  if (anyNA(i)) {
    .tr_exp_ef_abort("magnitude de contraste que não existe: %s. Contrastes: %s.",
                     paste(names(m)[is.na(i)], collapse = ", "), paste(rownames(L), collapse = ", "))
  }
  out <- stats::setNames(numeric(nrow(L)), rownames(L))
  out[i] <- m
  out
}

#' De magnitudes a efeitos: τ = Lᵀ (L Lᵀ)⁻¹ m, e a tabela da conversão.
#' @noRd
.tr_exp_ef_converter <- function(L, m) {
  tau <- drop(t(L) %*% solve(L %*% t(L), m))
  conv <- data.frame(contraste = rownames(L),
                     coeficientes = apply(L, 1, function(v) paste(format(v, trim = TRUE), collapse = " ")),
                     magnitude = unname(m), soma_c2 = rowSums(L^2),
                     conferencia = drop(L %*% tau), stringsAsFactors = FALSE)
  rownames(conv) <- NULL
  list(efeitos = stats::setNames(tau, colnames(L)), conversao = conv)
}

#' Efeito por nível digitado, cobrindo exatamente os níveis.
#' @noRd
.tr_exp_ef_por_nivel <- function(efeitos, niveis, oque) {
  e <- .tr_exp_ef_pares(efeitos, "efeitos")
  if (!length(e) || is.null(names(e))) {
    .tr_exp_ef_abort("diga o efeito de cada %s em 'efeitos' ('%s = 0, %s = 2').", oque, niveis[[1]],
                     niveis[[min(2L, length(niveis))]])
  }
  fora <- setdiff(names(e), niveis)
  falta <- setdiff(niveis, names(e))
  if (length(fora) || length(falta) || anyDuplicated(names(e))) {
    .tr_exp_ef_abort("'efeitos' tem de cobrir cada %s uma vez. %s%s%s", oque,
                     if (length(falta)) sprintf("Faltam: %s. ", paste(falta, collapse = ", ")) else "",
                     if (length(fora)) sprintf("Não existem no plano: %s. ", paste(fora, collapse = ", ")) else "",
                     if (anyDuplicated(names(e))) "Há repetidos." else "")
  }
  e[niveis]
}

#' Valores numéricos de uma coluna: numérica como está; fator pelos nomes dos
#' níveis ou pelas `doses`.
#' @noRd
.tr_exp_ef_numerico <- function(v, doses, col) {
  if (is.numeric(v)) return(as.numeric(v))
  niv <- .tr_exp_ef_niveis(v)
  x <- .tr_exp_an_x(niv, doses, col, "experiments/effect")
  x[match(as.character(v), niv)]
}

#' Doses por fator: `"dose: 0, 50, 100; irrigacao: 0, 1"`, ou uma lista só
#' quando há um fator.
#' @noRd
.tr_exp_ef_doses <- function(doses, cols) {
  if (!.tr_exp_preenchido(doses)) return(stats::setNames(rep(list(""), length(cols)), cols))
  if (!grepl(":", doses, fixed = TRUE)) {
    if (length(cols) > 1L) .tr_exp_ef_abort("com mais de um fator, escreva 'doses' como 'fator: 0, 50; outro: 1, 2'.")
    return(stats::setNames(list(doses), cols))
  }
  out <- stats::setNames(rep(list(""), length(cols)), cols)
  for (it in .tr_exp_split(doses, ";")) {
    p <- strsplit(it, ":", fixed = TRUE)[[1]]
    nm <- trimws(p[[1]])
    if (!nm %in% cols) .tr_exp_ef_abort("'doses' fala de '%s', que não está em 'fator'.", nm)
    out[[nm]] <- trimws(p[[2]])
  }
  out
}

#' Adiciona um termo à resposta simulada.
#'
#' Nível 1 do nó `experiments/effect`: a ajuda do nó diz quais params cada tipo
#' lê. Ver o comentário no topo de `R/efeito.R` para a escala dos contrastes.
#' @param plano objeto `tr_experiments_plan` (de `experiments/design` ou de
#'   outro `experiments/effect`).
#' @param tipo `intercepto`, `fixo`, `aleatorio`, `interacao`, `quantitativo`
#'   ou `covariavel`.
#' @param fator coluna(s) do plano: `"irrigacao"`, `"bloco:parcela"`,
#'   `"dose:irrigacao"`.
#' @param valor o intercepto.
#' @param efeitos efeito por nível (`"baixa = 0, alta = 2"`) ou por célula
#'   (`"baixa:A = 1, ..."`).
#' @param conjunto `por nível` ou um conjunto de contrastes (`polinomiais`,
#'   `helmert`, `controle`, `digitados`).
#' @param magnitudes valor de cada contraste (`"linear = 4, quadratico = 1"`).
#' @param doses valores numéricos dos níveis (polinomiais e quantitativo).
#' @param controle nível controle (conjunto `controle`).
#' @param contrastes contrastes digitados, na sintaxe do
#'   `models/linear_hypothesis`.
#' @param sd desvio-padrão do efeito aleatório, ou da covariável gerada.
#' @param coeficientes `"b1"` (linear) ou `"b1, b2"` (quadrática)...
#' @param inclinacao inclinação da covariável.
#' @param media média da covariável gerada, e o centro da contribuição.
#' @param nome nome do termo (coluna `.ef_<nome>`); em branco, vem do fator.
#' @param .seed semente (vem do card).
#' @return o plano com a coluna `.ef_<nome>` e o termo em `plano$termos`.
#' @export
tr_experiments_effect <- function(plano, tipo = "intercepto", fator = "", valor = 0, efeitos = "",
                                  conjunto = "por nível", magnitudes = "", doses = "", controle = "",
                                  contrastes = "", sd = 1, coeficientes = "", inclinacao = 0, media = 0,
                                  nome = "", .seed = 1L) {
  .tr_exp_plano_conferir(plano)
  tipo <- .tr_exp_enum(tipo, .TR_EXP_EF_TIPOS, "tipo")
  conjunto <- .tr_exp_enum(conjunto, .TR_EXP_EF_CONJUNTOS, "conjunto")
  if (!is.null(plano$resposta)) {
    .tr_experiments_abort("tr_experiments_error_response_closed",
                          "'experiments/effect': a resposta '%s' já foi fechada por 'experiments/error'. Ligue os termos ANTES do erro.",
                          plano$resposta$nome)
  }
  u <- plano$unidades
  N <- nrow(u)
  cols <- if (tipo == "intercepto") character() else .tr_exp_ef_colunas(plano, fator)
  if (!.tr_exp_preenchido(nome)) nome <- if (tipo == "intercepto") "intercepto" else paste(cols, collapse = "_")
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", nome)) .tr_exp_ef_abort("'%s' não serve como nome de termo.", nome)
  coluna <- paste0(".ef_", nome)
  if (coluna %in% names(u)) {
    .tr_exp_ef_abort("já há um termo chamado '%s'. Dê outro 'nome' a este.", nome)
  }
  if (tipo %in% c("fixo", "covariavel") && length(cols) != 1L) {
    .tr_exp_ef_abort("o tipo '%s' é de UM fator; para vários, use 'interacao' (ou 'aleatorio' com 'a:b').", tipo)
  }
  if (tipo == "interacao" && length(cols) < 2L) .tr_exp_ef_abort("a interação pede ao menos dois fatores ('a:b').")
  if (!is.numeric(sd) || length(sd) != 1L || is.na(sd) || sd < 0) .tr_exp_ef_abort("'sd' tem de ser um número >= 0.")

  conversao <- NULL
  avisos <- character()
  params <- list()
  r <- switch(tipo,
    intercepto = {
      params <- list(valor = valor)
      list(contrib = rep(as.numeric(valor), N), verdadeiro = data.frame(nivel = "(todas)", efeito = as.numeric(valor)))
    },
    fixo = {
      v <- u[[cols]]
      if (is.numeric(v)) .tr_exp_ef_abort("'%s' é numérica; efeito por nível é de fator. Use o tipo 'quantitativo'.", cols)
      niv <- .tr_exp_ef_niveis(v)
      if (conjunto == "por nível") {
        tau <- .tr_exp_ef_por_nivel(efeitos, niv, sprintf("nível de '%s'", cols))
        params <- list(efeitos = tau)
      } else {
        L <- .tr_exp_ef_matriz(niv, conjunto, doses, controle, contrastes, cols)
        cv <- .tr_exp_ef_converter(L, .tr_exp_ef_magnitudes(L, magnitudes))
        tau <- cv$efeitos; conversao <- cv$conversao
        params <- list(conjunto = conjunto, magnitudes = stats::setNames(conversao$magnitude, conversao$contraste))
      }
      list(contrib = unname(tau[as.character(v)]), verdadeiro = data.frame(nivel = niv, efeito = unname(tau)))
    },
    aleatorio = {
      chave <- do.call(paste, c(lapply(cols, function(cc) as.character(u[[cc]])), sep = ":"))
      niv <- unique(chave)
      if (length(niv) == N) {
        avisos <- sprintf("o efeito aleatório de '%s' tem um nível por unidade: confunde-se com o resíduo.",
                          paste(cols, collapse = ":"))
      }
      ef <- .tr_exp_com_semente(.seed, stats::rnorm(length(niv), 0, sd))
      params <- list(sd = sd)
      list(contrib = ef[match(chave, niv)], verdadeiro = data.frame(nivel = niv, efeito = ef))
    },
    interacao = {
      for (cc in cols) if (is.numeric(u[[cc]])) {
        .tr_exp_ef_abort("'%s' é numérica; o produto de fatores numéricos é o tipo 'quantitativo' com 'a:b'.", cc)
      }
      nivs <- lapply(stats::setNames(cols, cols), function(cc) .tr_exp_ef_niveis(u[[cc]]))
      chave <- do.call(paste, c(lapply(cols, function(cc) as.character(u[[cc]])), sep = ":"))
      grade <- expand.grid(rev(nivs), stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)[, cols, drop = FALSE]
      celulas <- do.call(paste, c(grade, sep = ":"))
      if (conjunto == "por nível") {
        presentes <- celulas[celulas %in% chave]
        tau <- .tr_exp_ef_por_nivel(efeitos, presentes, sprintf("célula %s presente no plano", paste(cols, collapse = ":")))
        params <- list(efeitos = tau)
      } else {
        if (!conjunto %in% c("polinomiais", "helmert")) {
          .tr_exp_ef_abort("na interação, o produto de contrastes usa 'polinomiais' ou 'helmert' em cada fator.")
        }
        ds <- .tr_exp_ef_doses(doses, cols)
        Ls <- lapply(cols, function(cc) .tr_exp_ef_matriz(nivs[[cc]], conjunto, ds[[cc]], "", "", cc))
        combos <- expand.grid(lapply(Ls, function(L) rev(seq_len(nrow(L)))), KEEP.OUT.ATTRS = FALSE)
        combos <- combos[nrow(combos):1, , drop = FALSE]
        L <- t(apply(combos, 1, function(ix) {
          apply(grade, 1, function(cel) prod(vapply(seq_along(cols), function(j) Ls[[j]][ix[[j]], cel[[j]]], 0)))
        }))
        L <- matrix(L, nrow = nrow(combos), dimnames = list(
          apply(combos, 1, function(ix) paste(vapply(seq_along(cols), function(j) rownames(Ls[[j]])[ix[[j]]], ""),
                                              collapse = ":")), celulas))
        cv <- .tr_exp_ef_converter(L, .tr_exp_ef_magnitudes(L, magnitudes))
        tau <- cv$efeitos; conversao <- cv$conversao
        params <- list(conjunto = conjunto, magnitudes = stats::setNames(conversao$magnitude, conversao$contraste))
      }
      list(contrib = unname(tau[chave]), verdadeiro = data.frame(nivel = names(tau), efeito = unname(tau)))
    },
    quantitativo = {
      b <- .tr_exp_ef_pares(coeficientes, "coeficientes")
      if (!length(b) || !is.null(names(b))) .tr_exp_ef_abort("diga os coeficientes sem nome: 'b1' (linear) ou 'b1, b2' (quadrática).")
      ds <- .tr_exp_ef_doses(doses, cols)
      xs <- lapply(cols, function(cc) .tr_exp_ef_numerico(u[[cc]], ds[[cc]], cc))
      if (length(cols) > 1L && length(b) != 1L) {
        .tr_exp_ef_abort("o produto '%s' leva um coeficiente só (b · %s).", paste(cols, collapse = ":"),
                         paste(cols, collapse = " · "))
      }
      x <- Reduce(`*`, xs)
      contrib <- if (length(cols) > 1L) b * x else drop(outer(x, seq_along(b), `^`) %*% b)
      params <- list(coeficientes = b)
      vx <- sort(unique(x))
      list(contrib = contrib, verdadeiro = data.frame(nivel = format(vx, trim = TRUE),
                                                      efeito = contrib[match(vx, x)]))
    },
    covariavel = {
      v <- u[[cols]]
      if (!is.numeric(v)) .tr_exp_ef_abort("a covariável '%s' tem de ser numérica.", cols)
      if (all(is.na(v))) {
        v <- .tr_exp_com_semente(.seed, stats::rnorm(N, media, sd))
        u[[cols]] <- v
        avisos <- sprintf("covariável '%s' gerada: normal de média %g e sd %g.", cols, media, sd)
      } else if (anyNA(v)) {
        .tr_exp_ef_abort("a covariável '%s' tem valores faltando; preencha-a toda ou deixe-a toda vazia para gerar.", cols)
      }
      params <- list(inclinacao = inclinacao, media = media, gerada = length(avisos) > 0L)
      list(contrib = inclinacao * (v - media), verdadeiro = data.frame(nivel = "inclinação", efeito = inclinacao))
    })
  u[[coluna]] <- as.numeric(r$contrib)
  plano$unidades <- u
  # `argumentos`: a chamada como veio (sem plano e sem semente), para o
  # `experiments/power` refazer a cadeia em outro sorteio ou outro tamanho.
  argumentos <- list(tipo = tipo, fator = fator, valor = valor, efeitos = efeitos, conjunto = conjunto,
                     magnitudes = magnitudes, doses = doses, controle = controle, contrastes = contrastes,
                     sd = sd, coeficientes = coeficientes, inclinacao = inclinacao, media = media, nome = nome)
  termo <- list(nome = nome, coluna = coluna, tipo = tipo, fator = cols, parametros = params,
                verdadeiro = r$verdadeiro, conversao = conversao, semente = if (tipo %in% c("aleatorio", "covariavel")) as.integer(.seed),
                argumentos = argumentos)
  plano$termos <- c(plano$termos, list(termo))
  plano$avisos <- c(plano$avisos, avisos)
  plano$nota <- paste(c(plano$nota, sprintf("termo %s (%s%s)", nome, tipo,
                                            if (length(cols)) paste0(": ", paste(cols, collapse = ":")) else "")),
                      collapse = "; ")
  plano
}
