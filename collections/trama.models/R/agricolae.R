# Duncan e Waller-Duncan, pelo `agricolae`.
#
# Os dois são procedimentos de AGRUPAMENTO: dizem quais médias formam grupos, e
# a saída que se usa é a letra. Por isso saem no mesmo tipo das médias do
# `emmeans` (`models/emm`) — o card é o mesmo gráfico com letras, e a tabela
# vai para o artigo pelo mesmo adaptador. Sem a grade do `emmeans` (`grade` é
# NULL), o `models/pairwise` não se aplica a eles, e diz isso.
#
# - **Duncan**: amplitude múltipla, com nível de proteção que cresce com a
#   distância entre as médias ordenadas. Mais poder que o Tukey, e por isso mais
#   falsos positivos quando há muitas médias.
# - **Waller-Duncan**: bayesiano, com a razão K entre os custos dos erros tipo I
#   e II no lugar do alfa. K = 100 equivale mais ou menos a 5%, K = 500 a 1%. A
#   diferença crítica diminui quando o F do tratamento é grande — o teste se
#   adapta à evidência do quadro.
#
# As médias do `agricolae` são as da tabela, e não as ajustadas: iguais no
# balanceado; no desbalanceado a nota avisa e aponta o `models/emmeans`.

#' O que os dois testes precisam do modelo: resposta, tratamento, QM e gl do erro
#' certo, e o F do tratamento.
#'
#' Na parcela subdividida o erro depende do fator: a parcela usa o erro (a), a
#' subparcela e a interação o (b). A combinação dos dois fatores não tem um erro
#' só, e o bloco recusa.
#' @noRd
.tr_models_agricolae_base <- function(modelo, tratamento, no) {
  .tr_models_exigir(modelo, c("lm", "split"), no,
                    "Os testes de agrupamento (Duncan, Waller-Duncan, Scott-Knott) pedem o erro de uma ANOVA; para GLM e misto, use 'models/emmeans'.")
  trat <- .tr_models_cols(modelo$dados, tratamento, "tratamento", minimo = 1L, maximo = 3L)
  for (v in trat) {
    if (!is.factor(modelo$dados[[v]])) {
      .tr_models_abort("tr_models_error_not_applicable",
                       "'%s': '%s' é numérica no modelo; o teste compara níveis de fator.", no, v)
    }
  }
  q <- tr_models_anova_table(modelo)$tabela
  termo <- paste(trat, collapse = ":")
  if (!termo %in% q$termo) {
    .tr_models_abort("tr_models_error_not_applicable",
                     "'%s': '%s' não é termo do modelo (termos: %s).", no, termo,
                     paste(setdiff(q$termo, c("Total", "Resíduo", "Resíduo (a)", "Resíduo (b)")), collapse = ", "))
  }
  residuo <- if (modelo$classe == "lm") "Resíduo" else {
    if (identical(trat, modelo$tratamentos[[1]])) "Resíduo (a)"
    else if (length(trat) == 1L) "Resíduo (b)"
    else .tr_models_abort("tr_models_error_not_applicable",
                          paste0("'%s': a combinação parcela × subparcela não tem um erro só na parcela ",
                                 "subdividida. Compare as subparcelas dentro de cada parcela em 'models/emmeans' ",
                                 "(com 'por')."), no)
  }
  list(y = modelo$dados[[modelo$resposta]],
       trt = if (length(trat) == 1L) modelo$dados[[trat]] else interaction(modelo$dados[, trat, drop = FALSE], sep = ":", drop = TRUE),
       trat = trat, gl = q$gl[q$termo == residuo], qm = q$qm[q$termo == residuo],
       fc = q$F[q$termo == termo], residuo = residuo,
       balanceado = length(unique(table(interaction(modelo$dados[, trat, drop = FALSE], drop = TRUE)))) == 1L)
}

#' Resultado do `agricolae` -> objeto `tr_models_emm`, com IC t pelo erro usado.
#' @noRd
.tr_models_agricolae_emm <- function(res, b, modelo, alfa, rotulo, nota) {
  medias <- res$means
  grupos <- res$groups
  niveis <- rownames(medias)
  tab <- data.frame(nivel = niveis, media = medias[[1]], erro_padrao = sqrt(b$qm / medias$r),
                    gl = b$gl, stringsAsFactors = FALSE)
  tq <- stats::qt(1 - alfa / 2, b$gl)
  tab$li <- tab$media - tq * tab$erro_padrao
  tab$ls <- tab$media + tq * tab$erro_padrao
  tab$grupo <- trimws(as.character(grupos$groups[match(niveis, rownames(grupos))]))
  if (length(b$trat) == 1L) {
    names(tab)[[1]] <- b$trat
    tab[[b$trat]] <- factor(tab[[b$trat]], levels = levels(modelo$dados[[b$trat]]))
    tab <- tab[order(tab[[b$trat]]), ]
  } else {
    partes <- do.call(rbind, strsplit(tab$nivel, ":", fixed = TRUE))
    for (i in seq_along(b$trat)) tab[[b$trat[[i]]]] <- factor(partes[, i], levels = levels(modelo$dados[[b$trat[[i]]]]))
    tab <- tab[do.call(order, tab[b$trat]), c(b$trat, "media", "erro_padrao", "gl", "li", "ls", "grupo")]
  }
  rownames(tab) <- NULL
  nota <- .tr_models_nota(nota, if (modelo$classe == "split") sprintf("erro usado: %s", tolower(b$residuo)) else "",
                          if (!b$balanceado) "desbalanceado: médias da tabela, não ajustadas — prefira 'models/emmeans'" else "")
  .tr_models_emm_obj(NULL, tibble::as_tibble(tab), b$trat, character(), rotulo, alfa, modelo$resposta, nota)
}

#' Teste de Duncan.
#' @param modelo objeto `tr_models_fit` de uma ANOVA.
#' @param tratamento fator (ou até 3, para as combinações).
#' @param confianca nível de confiança; o Duncan roda a alfa = 1 - confianca.
#' @return objeto `tr_models_emm`.
#' @export
tr_models_duncan <- function(modelo, tratamento = "", confianca = 0.95) {
  .tr_models_fit_conferir(modelo)
  no <- "models/duncan"
  confianca <- .tr_models_num(confianca, "confianca", min = 0.5, max = 0.999)
  # O agricolae pede o alfa; o param segue o glossário (confiança).
  alfa <- 1 - confianca
  b <- .tr_models_agricolae_base(modelo, tratamento, no)
  res <- .tr_models_ajustar(agricolae::duncan.test(b$y, b$trt, DFerror = b$gl, MSerror = b$qm,
                                                   alpha = alfa, group = TRUE, console = FALSE), no)
  .tr_models_agricolae_emm(res, b, modelo, alfa, "duncan",
                           sprintf("letras: Duncan a %s%%", formatC(100 * alfa, format = "fg", decimal.mark = ",")))
}

#' Teste de Waller-Duncan (razão K).
#' @inheritParams tr_models_duncan
#' @param k razão de custos dos erros tipo I e II: 100 (~5%), 500 (~1%), 50 (~10%).
#' @return objeto `tr_models_emm`.
#' @export
tr_models_waller_duncan <- function(modelo, tratamento = "", k = 100L) {
  .tr_models_fit_conferir(modelo)
  no <- "models/waller_duncan"
  k <- .tr_models_num(k, "k", min = 2, max = 10000)
  b <- .tr_models_agricolae_base(modelo, tratamento, no)
  res <- .tr_models_ajustar(agricolae::waller.test(b$y, b$trt, DFerror = b$gl, MSerror = b$qm, Fc = b$fc,
                                                   K = k, group = TRUE, console = FALSE), no)
  crit <- res$statistics$CriticalDifference
  .tr_models_agricolae_emm(res, b, modelo, 0.05, "waller-duncan",
                           sprintf("letras: Waller-Duncan, K = %s; diferença crítica %s",
                                   formatC(k, format = "fg"), .tr_models_fmt(crit, 4L)))
}

# ---- Scott-Knott -------------------------------------------------------------
#
# Implementado aqui, e não pelo pacote `ScottKnott` (leve: emmeans + xtable):
# o algoritmo cabe em trinta linhas, e o que pesa é escolher o erro — que já
# está em `.tr_models_agricolae_base` (parcela subdividida com erro (a) ou (b),
# combinações de fatores). Pelo pacote, a subdividida iria por outro caminho, e
# as médias seriam as do `emmeans` num e as da tabela nos vizinhos. Os testes
# conferem os grupos contra os do pacote nos exemplos documentados dele.

#' Partição recursiva de Scott & Knott (1974).
#'
#' Ordena as médias; entre os k − 1 cortes possíveis escolhe o que maximiza a
#' soma de quadrados entre os dois grupos (B0). A razão
#' lambda = pi / (2 (pi − 2)) · B0 / sigma0², com
#' sigma0² = [soma (y − ybar)² + v · s²] / (k + v) e s² = média de QM / rᵢ
#' sobre as médias DO GRUPO que se está partindo (recalculada a cada nível da
#' recursão, como o `ScottKnott:::MaxValue`; no balanceado é QM / r), segue
#' qui-quadrado com k / (pi − 2) gl sob H0. Se rejeita, cada metade é partida de
#' novo; se não, as médias formam um grupo. Os grupos NÃO se sobrepõem — é o
#' que o distingue de Tukey e Duncan, e o que o tornou padrão nas revistas de
#' agrárias quando há muitos tratamentos.
#' @return vetor inteiro de grupo (1 = o das maiores médias), na ordem de `medias`.
#' @noRd
.tr_models_sk_grupos <- function(medias, qm, gl, r, alfa) {
  ord <- order(medias, decreasing = TRUE)
  y <- medias[ord]
  r <- rep_len(r, length(medias))[ord]
  grupo <- integer(length(y))
  proximo <- 0L
  partir <- function(i) {
    k <- length(i)
    if (k > 1L) {
      yi <- y[i]
      b0 <- vapply(seq_len(k - 1L), function(c) {
        a <- yi[seq_len(c)]; b <- yi[-seq_len(c)]
        sum(a)^2 / length(a) + sum(b)^2 / length(b) - sum(yi)^2 / k
      }, 0)
      corte <- which.max(b0)
      sigma0 <- (sum((yi - mean(yi))^2) + gl * mean(qm / r[i])) / (k + gl)
      lambda <- pi / (2 * (pi - 2)) * b0[[corte]] / sigma0
      if (lambda > stats::qchisq(1 - alfa, k / (pi - 2))) {
        partir(i[seq_len(corte)]); partir(i[-seq_len(corte)])
        return(invisible())
      }
    }
    # Recursão da esquerda para a direita: os grupos nascem já em ordem
    # decrescente de média, e o número do grupo vira a letra direto.
    proximo <<- proximo + 1L
    grupo[i] <<- proximo
  }
  partir(seq_along(y))
  grupo[order(ord)]
}

#' Teste de Scott-Knott.
#' @inheritParams tr_models_duncan
#' @return objeto `tr_models_emm`.
#' @export
tr_models_scott_knott <- function(modelo, tratamento = "", confianca = 0.95) {
  .tr_models_fit_conferir(modelo)
  no <- "models/scott_knott"
  confianca <- .tr_models_num(confianca, "confianca", min = 0.5, max = 0.999)
  alfa <- 1 - confianca
  b <- .tr_models_agricolae_base(modelo, tratamento, no)
  trt <- droplevels(factor(b$trt))
  medias <- tapply(b$y, trt, mean, na.rm = TRUE)
  n <- tapply(!is.na(b$y), trt, sum)
  # Desbalanceado: cada média leva a sua repetição, e o s² do sigma0 é a média
  # de QM / rᵢ do grupo em partição, como o pacote `ScottKnott`.
  g <- .tr_models_sk_grupos(as.vector(medias), b$qm, b$gl, as.vector(n), alfa)
  alfabeto <- c(letters, LETTERS)
  res <- list(means = data.frame(media = as.vector(medias), r = as.vector(n), row.names = names(medias)),
              groups = data.frame(groups = alfabeto[g], row.names = names(medias)))
  .tr_models_agricolae_emm(res, b, modelo, alfa, "scott-knott",
                           sprintf("grupos: Scott-Knott a %s%% (sem sobreposição)",
                                   formatC(100 * alfa, format = "fg", decimal.mark = ",")))
}
