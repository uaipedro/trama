# Análise discriminante: separar grupos JÁ CONHECIDOS e classificar caso novo.
#
# A PCA e a fatorial olham as variáveis sem saber de grupo nenhum; a
# discriminante recebe o grupo e procura as combinações das medidas que MAIS o
# separam. É por isso que ela acha nos `caranguejos` a espécie que a PCA não
# acha: o primeiro componente é o tamanho, e a espécie está na forma.
#
# O ajuste é o do `MASS` (`lda`/`qda`), que vem com todo R. O que ele não dá
# pronto — o lambda de Wilks por função, a correlação canônica, o M de Box, a
# matriz de confusão legível — é fórmula de livro, e mora aqui.
#
# Tudo recebe a MATRIZ (`lda(x, grouping)`), e nunca uma fórmula: nome de coluna
# com espaço ou acento quebraria a fórmula, e a matriz é o que as outras
# técnicas da coleção já montam com as mesmas recusas.

.TR_MULTI_METODOS_LDA <- c("linear", "quadrática")
.TR_MULTI_PRIORS <- c("proporcionais", "iguais")
.TR_MULTI_VALIDACOES <- c("cruzada", "resubstituição")
.TR_MULTI_TABELAS_LDA <- c("funções", "coeficientes", "padronizados", "estrutura")

# ---------------------------------------------------------------------------
# Peças comuns

#' O grupo e a matriz dos preditores, com as recusas que valem para todo nó.
#'
#' O grupo vira fator SEM os níveis vazios: um fator vindo de um `data/filter`
#' guarda os níveis filtrados, e a `lda` contaria um grupo de zero observações
#' (com prior zero e média NaN). Faltante no grupo é recusado como nas medidas —
#' uma linha sem grupo não tem onde entrar na conta das covariâncias.
#' @noRd
.tr_multi_grupos <- function(dados, grupo, cols, no) {
  grupo <- .tr_multi_col(dados, grupo, "grupo")
  preditores <- .tr_multi_variaveis(dados, cols, excluir = grupo)
  if (grupo %in% preditores) {
    .tr_multi_abort("tr_multi_error_bad_option",
                    "'%s': a coluna do grupo (%s) não pode estar também entre os preditores.",
                    no, grupo)
  }
  g <- dados[[grupo]]
  if (anyNA(g)) {
    .tr_multi_abort("tr_multi_error_missing_values",
                    paste0("'%s' não aceita faltantes, e %d linha(s) não têm grupo (coluna: %s). ",
                           "Ligue um 'data/drop_na' antes."),
                    no, sum(is.na(g)), grupo)
  }
  g <- droplevels(as.factor(g))
  if (nlevels(g) < 2L) {
    .tr_multi_abort("tr_multi_error_one_group",
                    paste0("'%s': a coluna '%s' tem %d grupo(s) (%s). Discriminar pede pelo menos ",
                           "dois — confira se um filtro antes não deixou só um."),
                    no, grupo, nlevels(g), paste(levels(g), collapse = ", "))
  }
  X <- .tr_multi_matriz(dados, preditores, no)
  list(grupo = grupo, g = g, X = X, preditores = preditores)
}

#' "setosa (50), versicolor (50)": o tamanho de cada grupo, para mensagem e card.
#' @noRd
.tr_multi_tamanhos <- function(g) {
  n <- table(g)
  paste(sprintf("%s (%d)", names(n), as.integer(n)), collapse = ", ")
}

#' Recusa grupo menor que `minimo`, nomeando quem e dizendo por quê.
#' @noRd
.tr_multi_grupo_minimo <- function(g, minimo, no, motivo) {
  n <- table(g)
  pequenos <- n[n < minimo]
  if (length(pequenos)) {
    .tr_multi_abort("tr_multi_error_small_group",
                    "'%s': grupo(s) com menos de %d observações — %s. %s Tamanhos: %s.",
                    no, as.integer(minimo),
                    paste(sprintf("'%s' tem %d", names(pequenos), as.integer(pequenos)),
                          collapse = "; "),
                    motivo, .tr_multi_tamanhos(g))
  }
  invisible(g)
}

#' Uma matriz de covariância é (numericamente) singular?
#'
#' Testa a CORRELAÇÃO correspondente, e não o determinante: o determinante
#' depende da unidade (medir em mm em vez de cm o multiplica por 10^p), e um
#' corte absoluto recusaria dado bom. Variância zero é singular por definição —
#' é o caso da variável constante DENTRO de um grupo, comum em dado real.
#' @noRd
.tr_multi_cov_singular <- function(S, tol = 1e-10) {
  d <- diag(S)
  if (any(!is.finite(d)) || any(d <= 0)) return(TRUE)
  R <- S / sqrt(outer(d, d))
  ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
  min(ev) < tol * max(ev)
}

#' Somas de quadrados dentro (W) e entre (B) grupos.
#' @noRd
.tr_multi_sscp <- function(X, g) {
  medias <- rowsum(X, g) / as.vector(table(g))
  R <- X - medias[as.integer(g), , drop = FALSE]
  W <- crossprod(R)
  Tt <- crossprod(scale(X, center = TRUE, scale = FALSE))
  list(W = W, B = Tt - W, medias = medias)
}

#' A covariância combinada dentro dos grupos, recusada se singular.
#'
#' É a matriz que a LDA inverte. A `lda` só AVISA "variables are collinear" e
#' segue com funções instáveis; aqui vira card vermelho antes do ajuste.
#' @noRd
.tr_multi_within <- function(X, g, no) {
  S <- .tr_multi_sscp(X, g)$W / (nrow(X) - nlevels(g))
  if (.tr_multi_cov_singular(S)) {
    .tr_multi_abort("tr_multi_error_singular_matrix",
                    paste0("'%s': a covariância combinada dentro dos grupos é singular. Alguma ",
                           "variável é combinação exata das outras, ou é constante dentro de ",
                           "todo grupo. Tire a redundante da lista de preditores."), no)
  }
  S
}

#' A covariância de cada grupo, recusando a singular com o nome do grupo.
#' @noRd
.tr_multi_cov_grupos <- function(X, g, no) {
  lapply(stats::setNames(levels(g), levels(g)), function(l) {
    S <- stats::cov(X[g == l, , drop = FALSE])
    if (.tr_multi_cov_singular(S)) {
      .tr_multi_abort("tr_multi_error_singular_matrix",
                      paste0("'%s': a covariância do grupo '%s' é singular — alguma variável é ",
                             "constante, ou combinação das outras, DENTRO desse grupo. A ",
                             "quadrática precisa invertê-la; use a linear, ou tire a variável."),
                      no, l)
    }
    S
  })
}

#' As priors no formato da `lda`: vetor na ordem dos níveis.
#' @noRd
.tr_multi_prior_vetor <- function(g, priors) {
  n <- as.vector(table(g))
  p <- if (identical(priors, "iguais")) rep(1 / length(n), length(n)) else n / sum(n)
  stats::setNames(p, levels(g))
}

#' A matriz de treino e o grupo de volta, a partir do objeto.
#' @noRd
.tr_multi_treino <- function(modelo) {
  list(X = .tr_multi_matriz(modelo$dados, modelo$preditores, "multi/lda"),
       g = droplevels(as.factor(modelo$dados[[modelo$grupo]])))
}

#' Confere o objeto que chega pela porta, para o `fn` chamado no console.
#' @noRd
.tr_multi_modelo <- function(modelo) {
  .tr_multi_guard(modelo, "tr_multi_lda", .TR_MULTI_CAMPOS_LDA, "tr_multi_error_not_a_lda",
                  "uma análise discriminante")
}

#' O ajuste, com o aviso de colinearidade do MASS promovido a erro.
#'
#' A checagem da covariância antes pega quase tudo; o aviso cobre o que escapar
#' dela (a `lda` usa um critério próprio), para nunca sair função instável verde.
#' @noRd
.tr_multi_lda_ajuste <- function(X, g, metodo, prior, no, CV = FALSE) {
  .tr_multi_ajustar(withCallingHandlers(
    if (identical(metodo, "linear")) MASS::lda(X, g, prior = prior, CV = CV)
    else MASS::qda(X, g, prior = prior, CV = CV),
    warning = function(w) {
      if (grepl("collinear", conditionMessage(w), fixed = TRUE)) {
        .tr_multi_abort("tr_multi_error_singular_matrix",
                        "'%s': os preditores são colineares dentro dos grupos (aviso da MASS: %s).",
                        no, conditionMessage(w))
      }
    }), no)
}

#' Nome de coluna a partir do nome de um grupo: `azul fêmea` -> `azul_fêmea`.
#'
#' Mantém letra acentuada (a tabela é em português) e troca o resto por `_`,
#' para a coluna ser citável em `data/select` sem crase.
#' @noRd
.tr_multi_nome_coluna <- function(x) {
  x <- gsub("(*UCP)[^\\p{L}\\p{N}]+", "_", x, perl = TRUE)
  x <- gsub("^_+|_+$", "", x)
  x[!nzchar(x)] <- "grupo"
  make.unique(x, sep = "_")
}

.TR_MULTI_VALIDACOES_CLASSIFY <- c("resubstituição", "cruzada")

#' Classe e probabilidades do TREINO, por resubstituição ou deixa-um-fora.
#'
#' Comum a `classify`, `confusion` e `roc`, para que os três contem a mesma
#' história. Na LDA/QDA a cruzada é o `CV = TRUE` da MASS, exato e sem
#' reajuste. Na logística não há atalho: são n ajustes, cada um sem uma linha —
#' rápido no `glm`, alguns segundos no `multinom` com centenas de linhas.
#' @return `list(prob, classe, g)` com `prob` n × grupos (colunas nos níveis).
#' @noRd
.tr_multi_prever <- function(modelo, validacao, no) {
  qual <- .tr_multi_classificador(modelo)
  g <- droplevels(as.factor(modelo$dados[[modelo$grupo]]))
  if (qual == "lda") {
    X <- .tr_multi_matriz(modelo$dados, modelo$preditores, no)
    if (identical(validacao, "resubstituição")) {
      pr <- stats::predict(modelo$ajuste, newdata = X)
      return(list(prob = pr$posterior, classe = factor(pr$class, levels = levels(g)), g = g))
    }
    if (identical(modelo$metodo, "quadrática")) {
      # Deixando uma fora, o grupo dela fica com n - 1; a covariância só é
      # inversível se ainda sobrarem p + 1.
      .tr_multi_grupo_minimo(g, ncol(X) + 2L, no,
                             "A validação cruzada da quadrática tira uma observação do grupo e ainda precisa inverter a covariância dele.")
    }
    cv <- .tr_multi_lda_ajuste(X, g, modelo$metodo, .tr_multi_prior_vetor(g, modelo$priors), no,
                               CV = TRUE)
    return(list(prob = cv$posterior, classe = factor(cv$class, levels = levels(g)), g = g))
  }
  X <- .tr_multi_logit_X(modelo, modelo$dados)
  if (identical(validacao, "resubstituição")) return(c(.tr_multi_logit_prever(modelo, X), list(g = g)))
  .tr_multi_grupo_minimo(g, 3L, no,
                         "Deixando uma observação de fora, o grupo dela ainda precisa de duas para entrar no ajuste.")
  n <- nrow(X)
  prob <- matrix(NA_real_, n, nlevels(g), dimnames = list(NULL, levels(g)))
  classe <- character(n)
  for (i in seq_len(n)) {
    sem <- modelo
    # Sem hessiana: o reajuste só prevê a observação deixada de fora.
    sem$ajuste <- .tr_multi_logit_ajuste(X[-i, , drop = FALSE], g[-i], no, hess = FALSE)
    pr <- .tr_multi_logit_prever(sem, X[i, , drop = FALSE])
    prob[i, ] <- pr$prob[1, ]
    classe[i] <- as.character(pr$classe)
  }
  list(prob = prob, classe = factor(classe, levels = levels(g)), g = g)
}

# ---------------------------------------------------------------------------
# Nós

#' Análise discriminante linear (LDA) ou quadrática (QDA).
#' @param dados tabela.
#' @param grupo coluna com o grupo conhecido.
#' @param cols preditores; em branco, todas as numéricas menos o grupo.
#' @param metodo `"linear"` ou `"quadrática"`.
#' @param priors `"proporcionais"` (às frequências observadas) ou `"iguais"`.
#' @return objeto `tr_multi_lda`.
#' @export
tr_multi_discriminant <- function(dados, grupo = "", cols = "", metodo = "linear",
                                  priors = "proporcionais") {
  no <- "multi/discriminant"
  metodo <- .tr_multi_enum(metodo, .TR_MULTI_METODOS_LDA, "metodo")
  priors <- .tr_multi_enum(priors, .TR_MULTI_PRIORS, "priors")
  gr <- .tr_multi_grupos(dados, grupo, cols, no)
  p <- ncol(gr$X)
  if (identical(metodo, "linear")) {
    .tr_multi_grupo_minimo(gr$g, 2L, no,
                           "Com uma observação só, o grupo não tem variação para contribuir.")
    .tr_multi_within(gr$X, gr$g, no)
  } else {
    # A QDA estima uma covariância POR GRUPO: com p preditores, cada grupo
    # precisa de p + 1 observações só para ela ser inversível.
    .tr_multi_grupo_minimo(gr$g, p + 1L, no, sprintf(
      "A quadrática estima uma covariância por grupo e, com %d preditores, precisa de %d observações em cada. Use a linear, ou menos preditores.",
      p, p + 1L))
    .tr_multi_cov_grupos(gr$X, gr$g, no)
  }
  ajuste <- .tr_multi_lda_ajuste(gr$X, gr$g, metodo, .tr_multi_prior_vetor(gr$g, priors), no)
  .tr_multi_lda_obj(ajuste, metodo, gr$grupo, gr$preditores, tibble::as_tibble(dados), priors)
}

#' Classifica: a classe prevista e as probabilidades a posteriori.
#' @param modelo objeto `tr_multi_lda` ou `tr_multi_logit`.
#' @param novos tabela a classificar; `NULL` classifica o próprio treino.
#' @param validacao `"resubstituição"` ou `"cruzada"` (só sem `novos`).
#' @return tibble: as colunas da tabela, `previsto`, `prob_<grupo>` e, na
#'   discriminante linear, `LD1..`.
#' @export
tr_multi_classify <- function(modelo, novos = NULL, validacao = "resubstituição") {
  no <- "multi/classify"
  qual <- .tr_multi_classificador(modelo)
  validacao <- .tr_multi_enum(validacao, .TR_MULTI_VALIDACOES_CLASSIFY, "validacao")
  preds <- modelo$preditores
  if (!is.null(novos) && identical(validacao, "cruzada")) {
    .tr_multi_abort("tr_multi_error_bad_option",
                    paste0("'%s': validação cruzada só vale para o TREINO — cada linha é prevista ",
                           "pelo modelo ajustado sem ela, e uma tabela nova nunca esteve no ajuste. ",
                           "Desligue a porta 'novos' ou use validacao = \"resubstituição\"."), no)
  }
  tab <- if (is.null(novos)) modelo$dados else tibble::as_tibble(novos)
  faltam <- setdiff(preds, names(tab))
  if (length(faltam)) {
    .tr_multi_abort("tr_multi_error_new_data_columns",
                    paste0("'%s': a tabela a classificar não tem colunas usadas no treino: %s. ",
                           "O modelo foi ajustado com: %s."),
                    no, paste(faltam, collapse = ", "), paste(preds, collapse = ", "))
  }
  texto <- preds[!vapply(tab[preds], is.numeric, TRUE)]
  if (length(texto)) {
    .tr_multi_abort("tr_multi_error_not_numeric",
                    "'%s': na tabela a classificar, coluna não numérica entre os preditores: %s.",
                    no, paste(texto, collapse = ", "))
  }
  # Não é `.tr_multi_matriz`: ela recusa variável constante, e classificar UMA
  # linha nova (desvio padrão NA) ou duas iguais é legítimo.
  X <- as.matrix(as.data.frame(tab)[, preds, drop = FALSE])
  storage.mode(X) <- "double"
  incompletas <- !stats::complete.cases(X)
  if (any(incompletas)) {
    .tr_multi_abort("tr_multi_error_missing_values",
                    paste0("'%s' não classifica linha com faltante, e %d linha(s) têm (colunas: %s). ",
                           "Ligue um 'data/drop_na' antes."),
                    no, sum(incompletas), paste(preds[colSums(is.na(X)) > 0], collapse = ", "))
  }
  if (is.null(novos) && identical(validacao, "cruzada")) {
    pr <- .tr_multi_prever(modelo, "cruzada", no)
    prob <- pr$prob; classe <- pr$classe; escores <- NULL
  } else if (qual == "lda") {
    p <- stats::predict(modelo$ajuste, newdata = X)
    prob <- p$posterior; classe <- p$class; escores <- p$x
  } else {
    p <- .tr_multi_logit_prever(modelo, X)
    prob <- p$prob; classe <- p$classe; escores <- NULL
  }
  post <- as.data.frame(prob)
  names(post) <- paste0("prob_", .tr_multi_nome_coluna(colnames(prob)))
  novas <- tibble::tibble(previsto = classe)
  novas <- cbind(novas, post)
  # Os escores LD saem mesmo na cruzada (são as coordenadas no plano do modelo
  # completo): o `view/points` colorido pelo previsto honesto precisa deles.
  if (qual == "lda" && is.null(escores) && identical(modelo$metodo, "linear")) {
    escores <- stats::predict(modelo$ajuste, newdata = X)$x
  }
  if (!is.null(escores)) novas <- cbind(novas, as.data.frame(escores))
  # Classificar a SAÍDA de um classify (ou reclassificar) sobrescreve as colunas
  # antigas, em vez de criar `previsto.1` ao lado da velha.
  tab <- tab[, setdiff(names(tab), names(novas)), drop = FALSE]
  tibble::as_tibble(cbind(as.data.frame(tab), novas))
}

#' Matriz de confusão: grupo real × grupo previsto.
#' @param modelo objeto `tr_multi_lda` ou `tr_multi_logit`.
#' @param validacao `"cruzada"` (deixa-um-fora) ou `"resubstituição"`.
#' @return tibble: `real`, uma coluna de contagem por grupo previsto, `total`,
#'   `acertos`, `taxa_acerto`; a última linha, `real = "total"`, é o geral.
#' @export
tr_multi_confusion <- function(modelo, validacao = "cruzada") {
  no <- "multi/confusion"
  validacao <- .tr_multi_enum(validacao, .TR_MULTI_VALIDACOES, "validacao")
  pr <- .tr_multi_prever(modelo, validacao, no)
  previsto <- pr$classe
  niveis <- levels(pr$g)
  m <- table(factor(pr$g, levels = niveis), factor(previsto, levels = niveis))
  cont <- matrix(as.integer(m), nrow(m), dimnames = list(NULL, niveis))
  # Um grupo chamado "total" colidiria com a coluna do total: ganha o sufixo.
  reservados <- c("real", "total", "acertos", "taxa_acerto")
  colnames(cont) <- ifelse(niveis %in% reservados, paste(niveis, "(previsto)"), niveis)
  total <- as.integer(rowSums(cont))
  acertos <- as.integer(diag(cont))
  corpo <- data.frame(real = niveis, cont, total = total, acertos = acertos,
                      taxa_acerto = acertos / total, check.names = FALSE)
  ultima <- data.frame(real = "total", t(as.integer(colSums(cont))), total = sum(total),
                       acertos = sum(acertos), taxa_acerto = sum(acertos) / sum(total),
                       check.names = FALSE)
  names(ultima) <- names(corpo)
  tibble::as_tibble(rbind(corpo, ultima))
}

#' Autovalores W^-1 B: a análise canônica dos grupos.
#'
#' Calculados dos DADOS, e não do `svd` da `lda`: o `svd` pondera a dispersão
#' entre grupos pelas priors, e com priors iguais em grupos desiguais deixaria
#' de ser o autovalor de livro — e o lambda de Wilks é um teste sobre os dados,
#' que não sabe de prior nenhuma. Com priors proporcionais os dois coincidem.
#' @noRd
.tr_multi_canonica <- function(X, g) {
  s <- .tr_multi_sscp(X, g)
  L <- chol(s$W)
  Li <- backsolve(L, diag(ncol(X)))
  M <- t(Li) %*% s$B %*% Li
  r <- min(ncol(X), nlevels(g) - 1L)
  ev <- eigen((M + t(M)) / 2, symmetric = TRUE, only.values = TRUE)$values[seq_len(r)]
  pmax(ev, 0)
}

#' Funções discriminantes: autovalor, separação, correlação canônica, Wilks.
#' @param modelo objeto `tr_multi_lda` (linear).
#' @param tabela `"funções"` (os testes), `"coeficientes"` (brutos),
#'   `"padronizados"` ou `"estrutura"` (correlações variável-função).
#' @return tibble.
#' @export
tr_multi_discriminant_functions <- function(modelo, tabela = "funções") {
  no <- "multi/discriminant_functions"
  .tr_multi_modelo(modelo)
  tabela <- .tr_multi_enum(tabela, .TR_MULTI_TABELAS_LDA, "tabela")
  if (!identical(modelo$metodo, "linear")) {
    .tr_multi_abort("tr_multi_error_bad_option",
                    paste0("'%s': o modelo é quadrático, e a QDA não tem funções discriminantes — a ",
                           "fronteira entre grupos é curva, e não uma combinação linear das medidas. ",
                           "Ajuste um 'multi/discriminant' com metodo = \"linear\" para ver as funções."),
                    no)
  }
  tr <- .tr_multi_treino(modelo)
  X <- tr$X; g <- tr$g
  n <- nrow(X); p <- ncol(X); k <- nlevels(g)
  esc <- modelo$ajuste$scaling
  r <- ncol(esc)
  funcoes <- paste0("LD", seq_len(r))
  if (identical(tabela, "funções")) {
    lambda <- .tr_multi_canonica(X, g)[seq_len(r)]
    wilks <- vapply(seq_len(r), function(j) prod(1 / (1 + lambda[j:r])), 0)
    # Bartlett: a partir da função j, as que sobram ainda separam alguma coisa?
    qui <- -(n - 1 - (p + k) / 2) * log(wilks)
    gl <- (p - seq_len(r) + 1) * (k - seq_len(r))
    return(tibble::tibble(
      funcao = funcoes, autovalor = lambda, proporcao = lambda / sum(lambda),
      correlacao_canonica = sqrt(lambda / (1 + lambda)), lambda_wilks = wilks,
      qui_quadrado = qui, gl = as.integer(gl),
      p_valor = stats::pchisq(qui, gl, lower.tail = FALSE)))
  }
  Sw <- .tr_multi_sscp(X, g)$W / (n - k)
  sdw <- sqrt(diag(Sw))
  m <- switch(tabela,
    coeficientes = esc,
    padronizados = esc * sdw,
    # Escores da `lda` têm covariância dentro dos grupos IDENTIDADE; então a
    # correlação dentro dos grupos entre variável e função é R_w %*% padronizados.
    estrutura = (Sw / outer(sdw, sdw)) %*% (esc * sdw))
  dimnames(m) <- list(NULL, funcoes)
  tibble::as_tibble(data.frame(variavel = modelo$preditores, m, check.names = FALSE))
}

#' M de Box: as matrizes de covariância dos grupos são iguais?
#' @param dados tabela.
#' @param grupo coluna do grupo.
#' @param cols variáveis; em branco, as numéricas menos o grupo.
#' @return tibble de uma linha.
#' @export
tr_multi_box_m <- function(dados, grupo = "", cols = "") {
  no <- "multi/box_m"
  gr <- .tr_multi_grupos(dados, grupo, cols, no)
  X <- gr$X; g <- gr$g
  p <- ncol(X); N <- nrow(X); k <- nlevels(g)
  .tr_multi_grupo_minimo(g, p + 1L, no, sprintf(
    "O teste compara o determinante da covariância de cada grupo, e com %d variáveis ela só é inversível com %d observações.",
    p, p + 1L))
  S <- .tr_multi_cov_grupos(X, g, no)
  ni <- as.vector(table(g))
  Sp <- Reduce(`+`, Map(function(s, n) (n - 1) * s, S, ni)) / (N - k)
  logdet <- function(A) as.numeric(determinant(A, logarithm = TRUE)$modulus)
  M <- (N - k) * logdet(Sp) - sum((ni - 1) * vapply(S, logdet, 0))
  c1 <- (2 * p^2 + 3 * p - 1) / (6 * (p + 1) * (k - 1)) * (sum(1 / (ni - 1)) - 1 / (N - k))
  qui <- M * (1 - c1)
  gl <- p * (p + 1) * (k - 1) / 2
  pv <- stats::pchisq(qui, gl, lower.tail = FALSE)
  rejeita <- pv < .05
  tibble::tibble(
    teste = "M de Box",
    h0 = "as matrizes de covariância são iguais em todos os grupos",
    m = M, qui_quadrado = qui, gl = as.integer(gl), p_valor = pv,
    decisao_5 = if (rejeita) "rejeita H0" else "não rejeita H0",
    leitura = if (rejeita) {
      paste0("As covariâncias parecem diferir entre os grupos: a quadrática é candidata. ",
             "Mas o M de Box é muito sensível a caudas pesadas e, com muitas observações, ",
             "rejeita por diferenças pequenas — compare a taxa de acerto em validação ",
             "cruzada das duas antes de trocar.")
    } else {
      paste0("Sem evidência de covariâncias diferentes: a linear é adequada, e é mais ",
             "estável (estima uma covariância só). Não rejeitar não prova igualdade; com ",
             "grupos pequenos o teste tem pouco poder.")
    })
}

#' O plano discriminante: escores por grupo, com centróides.
#' @param modelo objeto `tr_multi_lda`.
#' @param x,y as funções nos eixos (1 = LD1).
#' @param elipses desenhar as elipses normais de 95% de cada grupo.
#' @inheritParams trama.view::tr_view_finish
#' @return ggplot.
#' @export
tr_multi_plot_discriminant <- function(modelo, x = 1L, y = 2L, elipses = TRUE, aspecto = "16:9",
                                       tema = "padrão", titulo = "", rotulo_x = "",
                                       rotulo_y = "", legenda = "direita") {
  no <- "multi/plot_discriminant"
  .tr_multi_modelo(modelo)
  tr <- .tr_multi_treino(modelo)
  # A QDA não tem funções discriminantes. Para ter EIXOS, uma LDA auxiliar nos
  # mesmos dados: as cores e os grupos são os mesmos, mas a fronteira que a
  # QDA usa é curva, e o subtítulo diz isso.
  aux <- !identical(modelo$metodo, "linear")
  ajuste <- if (aux) {
    .tr_multi_lda_ajuste(tr$X, tr$g, "linear", .tr_multi_prior_vetor(tr$g, modelo$priors), no)
  } else modelo$ajuste
  esc <- stats::predict(ajuste, newdata = tr$X)$x
  r <- ncol(esc)
  sep <- ajuste$svd^2 / sum(ajuste$svd^2)
  rotulo <- function(j) sprintf("LD%d (%s%% da separação)", j,
                                formatC(100 * sep[[j]], format = "f", digits = 1, decimal.mark = ","))
  subtitulo <- if (aux) "Eixos de uma LDA auxiliar: a quadrática não tem funções discriminantes" else NULL
  nome_g <- modelo$grupo
  if (r == 1L) {
    # Dois grupos dão UMA função: não há plano, e a pergunta vira "as duas
    # distribuições se sobrepõem ao longo de LD1?".
    d <- tibble::tibble(LD1 = esc[, 1], grupo = tr$g)
    cent <- stats::aggregate(d$LD1, list(grupo = d$grupo), mean)
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["LD1"]], fill = .data[["grupo"]],
                                         colour = .data[["grupo"]])) +
      ggplot2::geom_density(alpha = .35, linewidth = .6) +
      ggplot2::geom_rug(alpha = .6, show.legend = FALSE) +
      ggplot2::geom_vline(data = cent, ggplot2::aes(xintercept = .data[["x"]],
                                                    colour = .data[["grupo"]]),
                          linetype = "dashed", show.legend = FALSE) +
      ggplot2::labs(x = rotulo(1L), y = "densidade", fill = nome_g, colour = nome_g,
                    subtitle = subtitulo)
  } else {
    x <- .tr_multi_int(x, "x", min = 1, max = r)
    y <- .tr_multi_int(y, "y", min = 1, max = r)
    if (x == y) {
      .tr_multi_abort("tr_multi_error_bad_option",
                      "'%s': os eixos x e y são a mesma função (LD%d). Escolha duas diferentes.", no, x)
    }
    d <- tibble::tibble(ex = esc[, x], ey = esc[, y], grupo = tr$g)
    cent <- stats::aggregate(d[c("ex", "ey")], list(grupo = d$grupo), mean)
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["ex"]], y = .data[["ey"]],
                                         colour = .data[["grupo"]])) +
      ggplot2::geom_hline(yintercept = 0, colour = .TR_MULTI_CINZA, linewidth = .3) +
      ggplot2::geom_vline(xintercept = 0, colour = .TR_MULTI_CINZA, linewidth = .3) +
      ggplot2::geom_point(size = 1.6, alpha = .75)
    if (isTRUE(elipses)) {
      # Elipse de grupo com menos de 4 pontos é um aviso do ggplot e nenhum
      # desenho: esses grupos ficam só com os pontos.
      grandes <- names(which(table(d$grupo) >= 4L))
      p <- p + ggplot2::stat_ellipse(data = d[d$grupo %in% grandes, ], type = "norm",
                                     level = .95, linewidth = .6)
    }
    p <- p +
      ggplot2::geom_point(data = cent, shape = 4, size = 5, stroke = 1.8, show.legend = FALSE) +
      ggplot2::labs(x = rotulo(x), y = rotulo(y), colour = nome_g, subtitle = subtitulo)
  }
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

# ---------------------------------------------------------------------------
# Tipo: resumo do card e adaptador

#' O resumo do card de `multi/lda`.
#' @noRd
.tr_multi_lda_resumo <- function(x) {
  tr <- .tr_multi_treino(x)
  acerto <- mean(stats::predict(x$ajuste, newdata = tr$X)$class == tr$g)
  list(metodo = x$metodo, grupos = .tr_multi_tamanhos(tr$g),
       preditores = length(x$preditores),
       acerto_resubstituicao = round(acerto, 4), priors = x$priors)
}

#' `multi/lda` -> `data/table`: o treino classificado, com os escores.
#' @noRd
.tr_multi_lda_tabela <- function(x) tr_multi_classify(x)

# ---------------------------------------------------------------------------
# Declarações

.tr_multi_nos_discriminante <- function() {
  P <- trama::tr_param
  L <- "multi/lda"
  TB <- "data/table"
  list(
    trama::tr_node("multi/discriminant", fn = tr_multi_discriminant, label = "Discriminante",
      category = "multi_discriminante", icon = trama::tr_icon("split"),
      description = "Ajusta uma análise discriminante linear (LDA) ou quadrática (QDA) para separar grupos conhecidos.",
      inputs = list(dados = TB), outputs = list(out = L),
      params = list(
        grupo = P("cols", "", label = "Grupo", example = "Species"),
        cols = P("cols", "", label = "Preditores", example = "Sepal.Length, Petal.Length"),
        metodo = trama::tr_param_enum("linear", .TR_MULTI_METODOS_LDA, label = "Método"),
        priors = trama::tr_param_enum("proporcionais", .TR_MULTI_PRIORS, label = "Priors")),
      help = .tr_multi_ajuda(r"---[
Recebe observações cujo GRUPO já se conhece (a espécie, o cultivar) e procura
as combinações das medidas que melhor separam os grupos. Serve para duas
coisas: ENTENDER o que distingue os grupos (quais medidas pesam nas funções) e
CLASSIFICAR caso novo, cujo grupo não se sabe.

### Linear ou quadrática

- **Linear (LDA, Fisher)** — supõe que os grupos têm a MESMA matriz de
  covariância e só diferem na média. A fronteira entre dois grupos é um
  hiperplano, e as funções discriminantes (`LD1`, `LD2`, ...) existem: são no
  máximo min(p, g − 1), com p preditores e g grupos.
- **Quadrática (QDA)** — cada grupo tem a sua covariância. A fronteira vira
  curva e não há funções discriminantes. Estima muito mais (uma covariância
  por grupo): com poucas observações por grupo, erra mais que a linear mesmo
  quando as covariâncias diferem de fato.

O `multi/box_m` testa se as covariâncias são iguais. A regra prática é
começar pela linear e trocar só se a quadrática ACERTAR mais em validação
cruzada (`multi/confusion`) — o M de Box rejeita com facilidade.

### Priors

A probabilidade de um caso pertencer a cada grupo ANTES de olhar as medidas.
**Proporcionais** usa a frequência dos grupos na tabela: se 90% dos casos são
de um grupo, a regra puxa os duvidosos para ele — certo quando a tabela
representa a população. **Iguais** dá 1/g a cada grupo: use quando a
amostragem fixou os tamanhos (50 de cada espécie) e a proporção da tabela não
diz nada sobre o mundo. As priors mudam a CLASSIFICAÇÃO, e não o teste de
Wilks nem as correlações canônicas.

### O exemplo dos caranguejos

Nos `caranguejos`, o primeiro componente de uma PCA explica 98% da variância —
e é só TAMANHO: caranguejo grande tem todas as medidas grandes. A espécie e o
sexo mal aparecem nele. A discriminante, que sabe dos grupos, procura a
direção que os separa, e ela está na FORMA (a largura traseira em relação à
carapaça): acerta cerca de 95% dos quatro grupos em validação cruzada.

### Recusas

Grupo com faltante, grupo único e preditor colinear viram erro. Na linear,
todo grupo precisa de 2 observações; na quadrática, de p + 1, e a covariância
de cada grupo tem de ser inversível (o erro nomeia o grupo).
]---", r"---[
- **Grupo** — a coluna com o grupo conhecido. Texto, fator ou número; os
  níveis sem nenhuma linha são descartados.
- **Preditores** — as medidas, separadas por vírgula. Em branco, todas as
  colunas numéricas menos o grupo.
- **Método** — `linear` (LDA) ou `quadrática` (QDA).
- **Priors** — `proporcionais` às frequências da tabela, ou `iguais`.
]---", r"---[
Um modelo `multi/lda`. O card mostra o plano discriminante e a taxa de acerto
por resubstituição. Ligado a um nó de tabela, vira o treino classificado
(como `multi/classify`), com `LD1`, `LD2`, ... para um `view/points`.
]---", r"---[
tr_flow(reg) |>
  tr_add("cr", "multi/example", dataset = "caranguejos") |>
  tr_add("lda", "multi/discriminant", grupo = "grupo", from = "cr") |>
  tr_add("cv", "multi/confusion", validacao = "cruzada", from = "lda")
]---", r"---[
`multi/confusion` para a taxa de acerto honesta; `multi/discriminant_functions`
para Wilks e correlações canônicas; `multi/classify` para caso novo;
`multi/box_m` para escolher entre linear e quadrática; `multi/pca` para
comparar com o que se vê sem o grupo.
]---")),

    trama::tr_node("multi/classify", fn = tr_multi_classify, label = "Classificar",
      category = "multi_discriminante", icon = trama::tr_icon("tags"),
      description = "Classe prevista e probabilidade de cada grupo, do treino ou de uma tabela nova.",
      inputs = list(modelo = "multi/classifier", novos = trama::tr_port(TB, required = FALSE)),
      outputs = list(out = TB),
      params = list(validacao = trama::tr_param_enum("resubstituição",
                                                     .TR_MULTI_VALIDACOES_CLASSIFY,
                                                     label = "Validação")),
      help = .tr_multi_ajuda(r"---[
Aplica a regra de um `multi/discriminant` ou de uma `multi/logistic` e diz,
para cada linha, o grupo PREVISTO e a probabilidade de cada grupo — a
probabilidade de a linha ser daquele grupo, dadas as medidas (e, na
discriminante, as priors; a logística não tem priors).

Sem nada na porta `novos`, classifica a própria tabela de treino. Com uma
tabela ligada em `novos`, classifica ELA: é o uso de verdade, o caso cujo
grupo não se sabe. A tabela nova precisa ter todas as colunas usadas como
preditores (com o mesmo nome), sem faltante; as outras colunas passam intactas,
e a do grupo pode nem existir.

Leia a probabilidade junto com a classe: `previsto = versicolor` com
`prob_versicolor = 0,51` é um empate, e não um acerto convicto. Casos com a
maior probabilidade abaixo de 0,7 merecem ser olhados. Isso vale para a regra
do grupo mais provável; na logística binária a classe vem do `corte`, e o
empate é a probabilidade perto dele.

Classificar o treino dá uma visão OTIMISTA do acerto — cada caso ajudou a
fazer a regra que o classifica. A taxa de acerto honesta é a da validação
cruzada, em `multi/confusion`.

### Validação cruzada

Com **cruzada**, cada linha do treino recebe a classe e as probabilidades do
modelo ajustado SEM ela (deixa-um-fora, *leave-one-out*, o jackknife da
classificação). É a tabela para ver QUAIS casos erram em caso novo, e com que
probabilidade. Só vale sem tabela em `novos`.
]---", r"---[
- **Validação** — `resubstituição` (padrão) ou `cruzada`.

Entradas: **modelo** (`multi/lda` ou `multi/logit`) e, opcional, **novos**
(`data/table`), a tabela a classificar.
]---", r"---[
Uma tabela (`data/table`): as colunas da tabela classificada; `previsto`
(fator); `prob_<grupo>`, uma por grupo (o nome do grupo com espaço e
pontuação trocados por `_`); e, na discriminante linear, os escores `LD1`, `LD2`, ...
Colunas com esses nomes que já existiam na tabela são substituídas.
]---", r"---[
tr_flow(reg) |>
  tr_add("v", "multi/example", dataset = "vinhos") |>
  tr_add("lda", "multi/discriminant", grupo = "cultivar", from = "v") |>
  tr_add("amostra", "data/slice_head", n = 5L, from = "v") |>
  tr_add("cl", "multi/classify", from = "lda") |>
  tr_link("amostra", "cl:novos")
]---", r"---[
`multi/confusion` para resumir os acertos; `view/points` com `cor = previsto`
para ver os erros no plano discriminante; `data/filter` para separar os casos
de probabilidade baixa.
]---")),

    trama::tr_node("multi/confusion", fn = tr_multi_confusion, label = "Matriz de confusão",
      category = "multi_discriminante", icon = trama::tr_icon("grid-3x3"),
      description = "Grupo real × previsto e taxa de acerto, por validação cruzada ou resubstituição.",
      inputs = list(modelo = "multi/classifier"), outputs = list(out = TB),
      params = list(validacao = trama::tr_param_enum("cruzada", .TR_MULTI_VALIDACOES,
                                                     label = "Validação")),
      help = .tr_multi_ajuda(r"---[
Cruza o grupo REAL de cada observação com o grupo que a regra PREVÊ, e conta.
A diagonal são os acertos; fora dela, quem foi confundido com quem — e é aí
que se aprende: na `iris`, os erros são sempre entre versicolor e virginica,
nunca com setosa.

### Por que validação cruzada

**Resubstituição** classifica os mesmos dados que ajustaram a regra. A taxa de
acerto aparente sai OTIMISTA: cada caso puxou a fronteira para o próprio lado.
O viés cresce com muitos preditores e poucos casos — e na quadrática, que
estima mais, é maior ainda.

**Cruzada** (deixa-um-fora, *leave-one-out*) reajusta a regra n vezes, cada
vez sem uma observação, e classifica a que ficou de fora. Estima o acerto em
caso NOVO, que é o que interessa. É exata e rápida na LDA e na QDA (a MASS a
calcula sem reajustar de verdade), então é o padrão aqui. Para comparar
linear com quadrática, compare as taxas CRUZADAS. Na logística não há atalho:
o modelo é reajustado n vezes (segundos com centenas de linhas na multinomial).

Na `iris`, a linear acerta 98% por resubstituição e também 98% na cruzada:
com 4 preditores e 50 flores por espécie, quase não há otimismo. Com 20
preditores e 15 casos por grupo, a diferença seria grande.
]---", r"---[
- **Validação** — `cruzada` (deixa-um-fora, o padrão) ou `resubstituição`.
]---", r"---[
Uma tabela (`data/table`) com uma linha por grupo REAL: a coluna `real`; uma
coluna por grupo PREVISTO, com a contagem; `total` (quantos há no grupo),
`acertos` (a diagonal) e `taxa_acerto` (acertos / total, a sensibilidade do
grupo). A última linha, `real = "total"`, soma as colunas: as contagens são
quantos foram previstos em cada grupo, e `taxa_acerto` é o acerto GERAL.
]---", r"---[
tr_flow(reg) |>
  tr_add("iris", "multi/example", dataset = "iris") |>
  tr_add("lda", "multi/discriminant", grupo = "Species", from = "iris") |>
  tr_add("cv", "multi/confusion", validacao = "cruzada", from = "lda")
]---", r"---[
`multi/discriminant` para o modelo; `multi/classify` para ver QUAIS casos
erraram e com que probabilidade; `multi/roc` para a troca entre sensibilidade e
especificidade; `multi/logistic` para o outro classificador.
]---")),

    trama::tr_node("multi/discriminant_functions", fn = tr_multi_discriminant_functions,
      label = "Funções discriminantes",
      category = "multi_discriminante", icon = trama::tr_icon("sigma"),
      description = "Autovalor, % de separação, correlação canônica e lambda de Wilks de cada função; ou os coeficientes.",
      inputs = list(modelo = L), outputs = list(out = TB),
      params = list(tabela = trama::tr_param_enum("funções", .TR_MULTI_TABELAS_LDA,
                                                  label = "Tabela")),
      help = .tr_multi_ajuda(r"---[
A LDA com g grupos e p preditores encontra até r = min(p, g − 1) funções
discriminantes, cada uma uma combinação linear das medidas. A primeira é a que
mais separa os grupos; a segunda, a que mais separa entre as que não se
correlacionam com a primeira; e assim por diante. Esta tabela diz QUANTO cada
uma separa e se vale a pena olhar para ela. Só existe no modelo linear.

### A tabela `funções`

- **autovalor** (λ) — dispersão ENTRE grupos dividida pela dispersão DENTRO,
  ao longo da função. É o autovalor de W⁻¹B (somas de quadrados dentro e entre).
- **proporcao** — a fração da separação total (λ / Σλ). Na `iris`, LD1 tem
  99,1%: a segunda função quase não acrescenta.
- **correlacao_canonica** — √(λ / (1 + λ)): a correlação entre a função e o
  grupo. Ao quadrado, a fração da variância da função explicada pelos grupos.
  Na `iris`, 0,985 e 0,471.
- **lambda_wilks** — Λ = ∏ 1/(1 + λⱼ), da função k em diante: a fração da
  variância que os grupos NÃO explicam nas funções k..r. Perto de 0, separam
  muito; perto de 1, nada. Na `iris`, Λ das duas funções é 0,0234.
- **qui_quadrado**, **gl**, **p_valor** — a aproximação de Bartlett,
  χ² = −(n − 1 − (p + g)/2) ln Λ, com (p − k + 1)(g − k) graus de liberdade.
  A linha k testa se as funções DE k EM DIANTE ainda separam alguma coisa: a
  primeira linha é o teste global (os grupos diferem?), e a última que der
  significativa diz quantas funções interpretar.

Os autovalores e os testes vêm dos dados, e não dependem das priors. Com
priors iguais e grupos de tamanhos diferentes, os eixos `LD` da `lda` são
ponderados pelas priors e podem diferir levemente dos canônicos de livro.

### As tabelas de coeficientes

Uma linha por preditor, uma coluna por função:

- **coeficientes** — os brutos (o `scaling` da `lda`): multiplicam as medidas
  na unidade original. Não se comparam entre si se as unidades diferem.
- **padronizados** — os brutos vezes o desvio padrão dentro dos grupos: dizem
  o peso de cada medida com as escalas igualadas. Com preditores
  correlacionados, os sinais podem enganar (uma medida compensa a outra).
- **estrutura** — a correlação, dentro dos grupos, entre cada medida e a
  função. É a leitura mais estável para NOMEAR a função ("LD1 é tamanho de
  pétala"), porque não sofre com a colinearidade.
]---", r"---[
- **Tabela** — `funções` (os testes), `coeficientes`, `padronizados` ou
  `estrutura`.
]---", r"---[
Uma tabela (`data/table`). Em `funções`: `funcao`, `autovalor`, `proporcao`,
`correlacao_canonica`, `lambda_wilks`, `qui_quadrado`, `gl`, `p_valor`. Nas
outras: `variavel` e uma coluna por função (`LD1`, `LD2`, ...).
]---", r"---[
tr_flow(reg) |>
  tr_add("iris", "multi/example", dataset = "iris") |>
  tr_add("lda", "multi/discriminant", grupo = "Species", from = "iris") |>
  tr_add("fun", "multi/discriminant_functions", tabela = "estrutura", from = "lda")
]---", r"---[
`multi/plot_discriminant` para ver as funções; `multi/confusion` para o acerto,
que é outra pergunta — funções significativas não garantem classificar bem.
]---")),

    trama::tr_node("multi/box_m", fn = tr_multi_box_m, label = "M de Box",
      category = "multi_discriminante", icon = trama::tr_icon("scale"),
      description = "Testa se as matrizes de covariância dos grupos são iguais: linear ou quadrática?",
      inputs = list(dados = TB), outputs = list(out = TB),
      params = list(
        grupo = P("cols", "", label = "Grupo", example = "cultivar"),
        cols = P("cols", "", label = "Variáveis", example = "alcool, flavonoides")),
      help = .tr_multi_ajuda(r"---[
A discriminante LINEAR supõe que todos os grupos têm a mesma matriz de
covariância — a mesma forma e inclinação da nuvem de pontos, só deslocada. O M
de Box testa essa hipótese; rejeitá-la aponta para a QUADRÁTICA.

H0: as matrizes de covariância são iguais em todos os grupos.

M = (N − g) ln|Sₚ| − Σ (nᵢ − 1) ln|Sᵢ|, com Sᵢ a covariância do grupo i e Sₚ a
combinada. Se as Sᵢ são iguais, os determinantes batem e M fica perto de 0. O
teste usa a aproximação qui-quadrado: χ² = M(1 − c), com
c = (2p² + 3p − 1) / (6(p + 1)(g − 1)) · (Σ 1/(nᵢ − 1) − 1/(N − g)) e
p(p + 1)(g − 1)/2 graus de liberdade.

### Cuidado ao ler

O M de Box é notoriamente SENSÍVEL: rejeita tanto por covariância diferente
quanto por falta de normalidade (caudas pesadas), e com amostra grande rejeita
por diferenças que não mudam a classificação. Por isso a decisão entre linear
e quadrática não deve ser só dele: compare as duas pela taxa de acerto em
validação cruzada (`multi/confusion`). A linear é mais robusta e costuma
ganhar com grupos pequenos, mesmo com H0 rejeitada.

Na `iris`, χ² ≈ 140,9 com 20 gl (p < 0,001): rejeita. Nos `vinhos`, simulados
com covariância igual de propósito, não rejeita.

Não rejeitar não é provar que as covariâncias são iguais: com grupos pequenos
o teste tem pouco poder.
]---", r"---[
- **Grupo** — a coluna do grupo.
- **Variáveis** — as medidas. Em branco, as numéricas menos o grupo — as
  mesmas que um `multi/discriminant` com os preditores em branco usaria.
]---", r"---[
Uma tabela (`data/table`) de uma linha: `teste`, `h0`, `m`, `qui_quadrado`,
`gl`, `p_valor`, `decisao_5` (a 5%) e `leitura`. Todo grupo precisa de pelo
menos p + 1 observações e covariância inversível (o erro nomeia o grupo).
]---", r"---[
tr_flow(reg) |>
  tr_add("v", "multi/example", dataset = "vinhos") |>
  tr_add("box", "multi/box_m", grupo = "cultivar", from = "v")
]---", r"---[
`multi/discriminant` para ajustar a linear ou a quadrática; `multi/confusion`
para compará-las pelo acerto.
]---")),

    trama::tr_node("multi/plot_discriminant", fn = tr_multi_plot_discriminant,
      label = "Plano discriminante",
      category = "multi_discriminante", icon = trama::tr_icon("chart-scatter"),
      description = "Escores das funções discriminantes por grupo, com centróides e elipses.",
      inputs = list(modelo = L), outputs = list(out = "view/plot"),
      params = .tr_multi_props(
        x = trama::tr_param_int(1L, min = 1L, label = "Função no eixo X"),
        y = trama::tr_param_int(2L, min = 1L, label = "Função no eixo Y"),
        elipses = trama::tr_param_bool(TRUE, label = "Elipses de 95%"),
        .aspecto = "16:9"),
      help = .tr_multi_ajuda(r"---[
Cada observação no plano de duas funções discriminantes (LD1 × LD2, por
padrão), colorida pelo grupo REAL. O X marca o centróide de cada grupo, e as
elipses são as normais de 95% — a região onde cairiam 95% dos casos do grupo se
ele fosse normal.

É o gráfico que mostra o que os números de `multi/discriminant_functions`
dizem: grupos que se afastam ao longo de LD1 são os que a primeira função
separa; elipses que se sobrepõem são as confusões de `multi/confusion`. O
rótulo de cada eixo traz a porcentagem da separação daquela função.

Na LDA as elipses dos grupos deveriam ter formas parecidas (é a hipótese da
covariância comum); elipses de formas muito diferentes são o que o
`multi/box_m` testa.

- **Dois grupos** dão uma função só: o gráfico vira a DENSIDADE de LD1 por
  grupo, com o centróide tracejado, e `x`/`y` são ignorados.
- **Modelo quadrático**: a QDA não tem funções discriminantes. Os eixos são de
  uma LDA auxiliar ajustada nos mesmos dados, e o subtítulo avisa — os grupos e
  as cores são os mesmos, mas a fronteira que a QDA usa é curva e não aparece.
]---", r"---[
- **Função no eixo X** / **Função no eixo Y** — qual função em cada eixo
  (1 = LD1). Não podem passar do número de funções, nem ser iguais.
- **Elipses de 95%** — desenhar a elipse normal de cada grupo (grupos com
  menos de 4 observações ficam sem ela).
]---", r"---[
Um gráfico (`view/plot`). É também o card de todo modelo `multi/lda`.
]---", r"---[
tr_flow(reg) |>
  tr_add("iris", "multi/example", dataset = "iris") |>
  tr_add("lda", "multi/discriminant", grupo = "Species", from = "iris") |>
  tr_add("plano", "multi/plot_discriminant", x = 1L, y = 2L, elipses = TRUE, from = "lda")
]---", r"---[
`multi/discriminant_functions` para os números por trás dos eixos; `view/points`
(ligado direto ao modelo, com `x = LD1`, `y = LD2`, `cor = previsto`) para
colorir pelo grupo PREVISTO e ver os erros; `multi/biplot` para o equivalente
na PCA.
]---", grafico = TRUE))
  )
}
