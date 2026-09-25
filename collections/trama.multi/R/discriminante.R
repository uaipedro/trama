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
# `param` é o nome do parâmetro nas mensagens: o M de Box chama a coluna de
# `grupo`, e discriminante/logística a chamam de `resposta` (glossário).
.tr_multi_grupos <- function(dados, grupo, cols, no, param = "grupo") {
  grupo <- .tr_multi_col(dados, grupo, param)
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
  list(X = .tr_multi_matriz(modelo$dados, modelo$preditores, "multi/discriminant"),
       g = droplevels(as.factor(modelo$dados[[modelo$grupo]])))
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

# ---------------------------------------------------------------------------
# Nós

#' Análise discriminante linear (LDA) ou quadrática (QDA).
#' @param dados tabela.
#' @param resposta coluna com o grupo conhecido (a resposta a classificar).
#' @param preditores colunas preditoras; em branco, todas as numéricas menos a resposta.
#' @param metodo `"linear"` ou `"quadrática"`.
#' @param priors `"proporcionais"` (às frequências observadas) ou `"iguais"`.
#' @return modelo `models/fit` de classe `tr_multi_lda`.
#' @export
tr_multi_discriminant <- function(dados, resposta = "", preditores = "", metodo = "linear",
                                  priors = "proporcionais") {
  no <- "multi/discriminant"
  metodo <- .tr_multi_enum(metodo, .TR_MULTI_METODOS_LDA, "metodo")
  priors <- .tr_multi_enum(priors, .TR_MULTI_PRIORS, "priors")
  gr <- .tr_multi_grupos(dados, resposta, preditores, no, param = "resposta")
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
#' @param modelo uma discriminante (`multi/discriminant`, linear).
#' @param tabela `"funções"` (os testes), `"coeficientes"` (brutos),
#'   `"padronizados"` ou `"estrutura"` (correlações variável-função).
#' @return tibble.
#' @export
tr_multi_discriminant_functions <- function(modelo, tabela = "funções") {
  no <- "multi/discriminant_functions"
  .tr_multi_exigir(modelo, "lda", no)
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
#' @return teste (`trama::tr_test`), que sai no tipo `data/test`.
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
  # Sai no tipo único de teste (`data/test`): o card da régua em vez de uma
  # tabela de uma linha, e a mesma linha de volta pelo adaptador — com `m` em
  # coluna extra, porque o M cru é o número que o livro-texto reporta.
  trama::tr_test(
    "M de Box", "as matrizes de covariância são iguais em todos os grupos",
    qui, "qui2", p_valor = pv, gl = as.character(as.integer(gl)),
    conclusao_sim = paste0(
      "As covariâncias parecem diferir entre os grupos: a quadrática é candidata. ",
      "Mas o M de Box é muito sensível a caudas pesadas e, com muitas observações, ",
      "rejeita por diferenças pequenas — compare a taxa de acerto em validação ",
      "cruzada das duas antes de trocar."),
    conclusao_nao = paste0(
      "Sem evidência de covariâncias diferentes: a linear é adequada, e é mais ",
      "estável (estima uma covariância só). Não rejeitar não prova igualdade; com ",
      "grupos pequenos o teste tem pouco poder."),
    fonte = "Box (1949)", extra = list(m = M), classe = "tr_multi_test")
}

#' O plano discriminante: escores por grupo, com centróides.
#' @param modelo uma discriminante (`multi/discriminant`).
#' @param x,y as funções nos eixos (1 = LD1).
#' @param elipses desenhar as elipses normais de 95% de cada grupo.
#' @inheritParams trama.view::tr_view_finish
#' @return ggplot.
#' @export
tr_multi_plot_discriminant <- function(modelo, x = 1L, y = 2L, elipses = TRUE, aspecto = "16:9",
                                       tema = "padrão", titulo = "", rotulo_x = "",
                                       rotulo_y = "", legenda = "direita") {
  no <- "multi/plot_discriminant"
  .tr_multi_exigir(modelo, "lda", no)
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
# Declarações

.tr_multi_nos_discriminante <- function() {
  P <- trama::tr_param
  L <- "models/fit"
  TB <- "data/table"
  list(
    trama::tr_node("multi/discriminant", fn = tr_multi_discriminant, label = "Discriminante",
      category = "multi_discriminante", icon = trama::tr_icon("split"),
      description = "Ajusta uma análise discriminante linear (LDA) ou quadrática (QDA) para separar grupos conhecidos.",
      inputs = list(dados = TB), outputs = list(out = L),
      params = list(
        resposta = P("cols", "", label = "Resposta (grupo)", example = "Species"),
        preditores = P("cols", "", label = "Preditores", example = "Sepal.Length, Petal.Length"),
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
cruzada (`models/confusion`) — o M de Box rejeita com facilidade.

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
- **Resposta** (`resposta`) — a coluna com o grupo conhecido. Texto, fator ou número; os
  níveis sem nenhuma linha são descartados.
- **Preditores** (`preditores`) — as medidas, separadas por vírgula. Em branco, todas as
  colunas numéricas menos o grupo.
- **Método** — `linear` (LDA) ou `quadrática` (QDA).
- **Priors** — `proporcionais` às frequências da tabela, ou `iguais`.
]---", r"---[
Um modelo (`models/fit`). O card mostra o plano discriminante. O modelo
entra nos blocos de previsão da coleção de modelos: `models/predict` para
classificar (o treino, ou caso novo em `dados`), `models/confusion` e
`models/roc` para medir o acerto. Ligado a um nó de tabela, vira o treino
classificado (como o `models/predict`), com `LD1`, `LD2`, ... para um
`view/points`.
]---", r"---[
tr_flow(reg) |>
  tr_add("cr", "multi/example", dataset = "caranguejos") |>
  tr_add("lda", "multi/discriminant", resposta = "grupo", from = "cr") |>
  tr_add("cv", "models/confusion", validacao = "cruzada", from = "lda")
]---", r"---[
`models/confusion` para a taxa de acerto honesta; `multi/discriminant_functions`
para Wilks e correlações canônicas; `models/predict` para caso novo;
`multi/box_m` para escolher entre linear e quadrática; `multi/pca` para
comparar com o que se vê sem o grupo.
]---")),

    trama::tr_node("multi/discriminant_functions", role = "leitura", fn = tr_multi_discriminant_functions,
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
  tr_add("lda", "multi/discriminant", resposta = "Species", from = "iris") |>
  tr_add("fun", "multi/discriminant_functions", tabela = "estrutura", from = "lda")
]---", r"---[
`multi/plot_discriminant` para ver as funções; `models/confusion` para o acerto,
que é outra pergunta — funções significativas não garantem classificar bem.
]---")),

    trama::tr_node("multi/box_m", role = "avaliacao", fn = tr_multi_box_m, label = "M de Box",
      category = "multi_discriminante", icon = trama::tr_icon("scale"),
      description = "Testa se as matrizes de covariância dos grupos são iguais: linear ou quadrática?",
      inputs = list(dados = TB), outputs = list(out = "data/test"),
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
validação cruzada (`models/confusion`). A linear é mais robusta e costuma
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
Um teste (`data/test`), com a régua do p-valor: o qui-quadrado é a
estatística, e o M cru vai numa coluna extra (`m`). Ligado numa entrada de
tabela, vira UMA linha, com as colunas de todo teste (`teste`, `h0`,
`estatistica`, `gl`, `p_valor`, `decisao_5`, `conclusao`...). Todo grupo
precisa de pelo menos p + 1 observações e covariância inversível (o erro nomeia
o grupo).
]---", r"---[
tr_flow(reg) |>
  tr_add("v", "multi/example", dataset = "vinhos") |>
  tr_add("box", "multi/box_m", grupo = "cultivar", from = "v")
]---", r"---[
`multi/discriminant` para ajustar a linear ou a quadrática; `models/confusion`
para compará-las pelo acerto.
]---", teste = TRUE)),

    trama::tr_node("multi/plot_discriminant", role = "leitura", fn = tr_multi_plot_discriminant,
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
separa; elipses que se sobrepõem são as confusões de `models/confusion`. O
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
Um gráfico (`view/plot`). É também o card de toda discriminante.
]---", r"---[
tr_flow(reg) |>
  tr_add("iris", "multi/example", dataset = "iris") |>
  tr_add("lda", "multi/discriminant", resposta = "Species", from = "iris") |>
  tr_add("plano", "multi/plot_discriminant", x = 1L, y = 2L, elipses = TRUE, from = "lda")
]---", r"---[
`multi/discriminant_functions` para os números por trás dos eixos; `view/points`
(ligado direto ao modelo, com `x = LD1`, `y = LD2`, `cor = previsto`) para
colorir pelo grupo PREVISTO e ver os erros; `multi/biplot` para o equivalente
na PCA.
]---", grafico = TRUE))
  )
}
