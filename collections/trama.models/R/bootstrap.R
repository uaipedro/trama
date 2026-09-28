# Bootstrap não paramétrico de casos sobre um ajuste linear.
#
# Toda quantidade é uma combinação linear dos coeficientes, `L %*% beta`: os
# coeficientes (L = identidade), as médias marginais de um fator (as linhas do
# `emmeans` sobre o ajuste original) e as diferenças entre elas. A matriz L é
# montada UMA vez, na grade de referência dos dados originais (covariáveis na
# média original), e cada reamostra só troca o `beta`. Com isso o bloco reajusta
# por `lm.fit` na matriz do modelo reamostrada, sem `lm()` nem `emmeans` no laço.

.TR_MODELS_BOOT_QUANTIDADES <- c("coeficientes", "médias", "diferenças")

#' Bootstrap de casos de um ajuste linear.
#'
#' Reamostra as linhas com reposição (dentro de **grupo**, quando dado),
#' reajusta e resume cada quantidade: estimativa, viés, erro-padrão, intervalos
#' percentil e BCa (`boot::boot.ci`) e a fração de reamostras com o mesmo sinal.
#' @param modelo um `models/fit` da classe lm.
#' @param quantidade `coeficientes`, `médias` (marginais de `especs`) ou
#'   `diferenças` (pares de médias, a de nível posterior menos a anterior).
#' @param especs o fator das médias; em branco, o primeiro fator do modelo.
#' @param grupo coluna dos estratos; em branco, `especs` nas médias e
#'   diferenças (cada nível mantém seu n) e nenhum nos coeficientes.
#' @param reamostras B.
#' @param confianca nível dos intervalos.
#' @param .seed semente (vem do card).
#' @return `list(out, tabela, distribuicao)`.
#' @export
tr_models_bootstrap <- function(modelo, quantidade = "coeficientes", especs = "", grupo = "",
                                reamostras = 1999L, confianca = 0.95,
                                aspecto = "16:9", tema = "padrão", titulo = "",
                                rotulo_x = "", rotulo_y = "", legenda = "direita", .seed = 1L) {
  no <- "models/bootstrap"
  .tr_models_fit_conferir(modelo)
  if (!identical(modelo$classe, "lm")) {
    .tr_models_abort("tr_models_error_bad_option", "'%s' aceita ajuste lm (inclui as ANOVAs de efeitos fixos); recebeu '%s'.", no, modelo$classe)
  }
  quantidade <- match.arg(quantidade, .TR_MODELS_BOOT_QUANTIDADES)
  B <- as.integer(reamostras)
  if (length(B) != 1L || is.na(B) || B < 99L) .tr_models_abort("tr_models_error_bad_option", "'%s': Reamostras deve ser pelo menos 99.", no)
  aj <- modelo$ajuste
  mf <- stats::model.frame(aj)
  X <- stats::model.matrix(aj)
  y <- stats::model.response(mf)
  beta <- stats::coef(aj)
  if (anyNA(beta)) .tr_models_abort("tr_models_error_bad_option", "'%s': o ajuste tem coeficiente não estimável (posto incompleto).", no)

  fatores <- names(mf)[-1L][vapply(mf[-1L], function(v) is.factor(v) || is.character(v), logical(1))]
  if (quantidade != "coeficientes") {
    if (!nzchar(especs)) {
      if (!length(fatores)) .tr_models_abort("tr_models_error_bad_option", "'%s': o modelo não tem fator para as médias; use Quantidade = coeficientes.", no)
      especs <- fatores[[1L]]
    }
    if (!especs %in% fatores) .tr_models_abort("tr_models_error_bad_option", "'%s': '%s' não é fator do modelo. Fatores: %s.", no, especs, paste(fatores, collapse = ", "))
    grade <- suppressMessages(emmeans::emmeans(aj, especs))
    L <- grade@linfct
    rotulos <- as.character(grade@grid[[especs]])
    if (quantidade == "diferenças") {
      pares <- utils::combn(nrow(L), 2L)
      L <- L[pares[2L, ], , drop = FALSE] - L[pares[1L, ], , drop = FALSE]
      rotulos <- paste(rotulos[pares[2L, ]], "-", rotulos[pares[1L, ]])
    }
    if (!nzchar(grupo)) grupo <- especs
  } else {
    L <- diag(length(beta))
    rotulos <- names(beta)
  }
  L <- unname(L)

  estrato <- if (nzchar(grupo)) {
    if (!grupo %in% names(modelo$dados)) .tr_models_abort("tr_models_error_bad_option", "'%s': Grupo '%s' não é coluna dos dados do ajuste.", no, grupo)
    g <- if (grupo %in% names(mf)) mf[[grupo]] else modelo$dados[[grupo]][as.integer(rownames(mf))]
    if (anyNA(g)) .tr_models_abort("tr_models_error_bad_option", "'%s': 'Reamostrar dentro de' ('%s') tem NA. Filtre ou recodifique antes.", no, grupo)
    as.integer(factor(g))
  } else rep(1L, length(y))

  # Reamostra que perde um nível (ou fica com posto incompleto) não estima
  # todos os coeficientes: devolve NA e é descartada, com a contagem na tabela.
  estatistica <- function(dados, i) {
    ft <- stats::lm.fit(X[i, , drop = FALSE], y[i])
    if (ft$rank < ncol(X)) return(rep(NA_real_, nrow(L)))
    as.numeric(L %*% ft$coefficients)
  }
  obj <- .tr_models_com_semente(.seed, boot::boot(seq_along(y), estatistica, R = B, strata = estrato))
  validas <- stats::complete.cases(obj$t)
  descartadas <- sum(!validas)
  if (sum(validas) < 99L) .tr_models_abort("tr_models_error_bad_option", "'%s': só %d reamostras estimaram o modelo; o ajuste é frágil demais para o bootstrap.", no, sum(validas))
  # Sem descartes, o `boot.ci` recebe o objeto como o `boot` o devolveu (a
  # aceleração do BCa sai da regressão nas frequências, o padrão do pacote).
  # Com descartes, o arranjo de frequências já não casa com `t`: a influência
  # vai pelo jackknife (`empinf(type = "jack")`), a outra estimativa do livro.
  if (descartadas) {
    obj$t <- obj$t[validas, , drop = FALSE]
    obj$R <- nrow(obj$t)
  }
  est <- as.numeric(L %*% beta)
  # O viés do BCa conta `t < t0`. Com resposta discreta, muitas reamostras dão
  # EXATAMENTE a estimativa (outra ordem das mesmas somas), e o ruído de 1e-15
  # decidiria se contam: o valor a menos de 1e-9 (relativo) de t0 vira t0,
  # que é o empate que a aritmética exata daria.
  obj$t0 <- est
  obj$t <- .tr_models_boot_empates(obj$t, est)
  linhas <- lapply(seq_len(nrow(L)), function(j) {
    z <- obj$t[, j]
    # Percentil e BCa em chamadas separadas: o BCa pode não sair (jackknife
    # que remove o único caso de um nível) sem levar o percentil junto.
    ic <- function(tipo, campo) {
      args <- list(obj, conf = confianca, type = tipo, index = j)
      if (tipo == "bca" && descartadas) {
        args$L <- tryCatch(boot::empinf(obj, index = j, type = "jack"), error = function(e) NULL)
        if (is.null(args$L) || anyNA(args$L)) return(c(NA_real_, NA_real_))
      }
      ci <- tryCatch(suppressWarnings(do.call(boot::boot.ci, args)), error = function(e) NULL)
      if (is.null(ci[[campo]])) c(NA_real_, NA_real_) else ci[[campo]][4:5]
    }
    perc <- ic("perc", "percent")
    bca <- ic("bca", "bca")
    tibble::tibble(quantidade = rotulos[[j]], estimativa = est[[j]], vies = mean(z) - est[[j]],
                   erro_padrao = stats::sd(z), li_perc = perc[[1]], ls_perc = perc[[2]],
                   li_bca = bca[[1]], ls_bca = bca[[2]],
                   mesmo_sinal = mean(sign(z) == sign(est[[j]])),
                   reamostras = length(z), descartadas = descartadas)
  })
  tabela <- do.call(rbind, linhas)
  distribuicao <- tibble::tibble(quantidade = factor(rep(rotulos, each = nrow(obj$t)), levels = rotulos),
                                 reamostra = rep(which(validas), times = nrow(L)),
                                 valor = as.numeric(obj$t))
  marcas <- tibble::tibble(quantidade = factor(rotulos, levels = rotulos), estimativa = est,
                           li = tabela$li_perc, ls = tabela$ls_perc)
  out <- ggplot2::ggplot(distribuicao, ggplot2::aes(x = .data[["valor"]])) +
    ggplot2::geom_histogram(bins = 30, fill = "#5B7C99", color = "white") +
    ggplot2::geom_vline(data = marcas, ggplot2::aes(xintercept = .data[["li"]]), linetype = 2, color = "#B34D4D") +
    ggplot2::geom_vline(data = marcas, ggplot2::aes(xintercept = .data[["ls"]]), linetype = 2, color = "#B34D4D") +
    ggplot2::geom_vline(data = marcas, ggplot2::aes(xintercept = .data[["estimativa"]]), color = "#B34D4D", linewidth = 1) +
    ggplot2::facet_wrap(ggplot2::vars(.data[["quantidade"]]), scales = "free") +
    ggplot2::labs(x = "Estimativa na reamostra", y = "Contagem",
                  subtitle = sprintf("B = %d; linha cheia = estimativa, tracejadas = IC percentil %g%%", nrow(obj$t), 100 * confianca))
  out <- trama.view::tr_view_finish(out, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
  distribuicao$quantidade <- as.character(distribuicao$quantidade)
  list(out = out, tabela = tabela, distribuicao = distribuicao)
}

#' Valores a menos de 1e-9 (relativo) da estimativa viram a estimativa.
#' @noRd
.tr_models_boot_empates <- function(t, t0) {
  for (j in seq_len(ncol(t))) {
    perto <- abs(t[, j] - t0[[j]]) <= 1e-9 * max(1, abs(t0[[j]]))
    t[perto, j] <- t0[[j]]
  }
  t
}
