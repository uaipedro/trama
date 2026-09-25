# Jackknife: o que muda numa estatística quando cada observação sai.
#
# Reajusta a técnica n vezes, cada vez sem uma linha (θ₍ᵢ₎), e compara com a
# estimativa da amostra toda (θ). Dá três coisas que a fórmula de livro não dá
# para cargas, correlações canônicas ou razões de chances:
#
# - o VIÉS, (n − 1)(θ̄₍.₎ − θ), e a estimativa corrigida;
# - o ERRO PADRÃO, √((n − 1)/n · Σ(θ₍ᵢ₎ − θ̄₍.₎)²), sem supor normalidade;
# - a INFLUÊNCIA de cada linha, (n − 1)(θ̄₍.₎ − θ₍ᵢ₎): a observação que sozinha
#   move uma carga aparece aqui, e em nenhum outro lugar.
#
# O motor é um só; cada técnica diz como REAJUSTAR a partir de uma tabela e o
# que EXTRAIR (um vetor nomeado) do ajuste. O alinhamento das cargas mora no
# extrator: tirar uma linha pode inverter o sinal de um componente ou trocar a
# ordem de dois fatores rotacionados, e sem desfazer isso a média das réplicas
# mistura soluções diferentes e o erro padrão sai sem sentido.

.TR_MULTI_JK_TABELAS <- c("resumo", "pseudovalores")
.TR_MULTI_JK_MAX <- 5000L

#' O laço do jackknife e as duas tabelas.
#' @param dados a tabela de treino do modelo.
#' @param variaveis as colunas que entraram (as outras vão para os pseudovalores).
#' @param completo o objeto ajustado na amostra toda.
#' @param reajustar `function(tabela)` -> objeto.
#' @param extrair `function(objeto)` -> vetor numérico nomeado.
#' @param log se TRUE, estimativa, média, corrigida e IC saem exponenciados
#'   (razões de chances); viés e erro padrão ficam na escala log.
#' @noRd
.tr_multi_jackknife <- function(dados, variaveis, completo, reajustar, extrair, no, tabela, nivel,
                                log = FALSE) {
  tabela <- .tr_multi_enum(tabela, .TR_MULTI_JK_TABELAS, "tabela")
  nivel <- .tr_multi_num(nivel, "nivel", min = 0.5, max = 0.999)
  dados <- as.data.frame(dados)
  n <- nrow(dados)
  if (n > .TR_MULTI_JK_MAX) {
    .tr_multi_abort("tr_multi_error_too_many_rows",
                    paste0("'%s': o jackknife reajusta a técnica uma vez por linha, e a tabela tem %d ",
                           "(o limite é %d). Sorteie uma amostra das linhas antes."),
                    no, n, .TR_MULTI_JK_MAX)
  }
  theta <- extrair(completo)
  nomes <- names(theta)
  reps <- matrix(NA_real_, n, length(theta), dimnames = list(NULL, nomes))
  for (i in seq_len(n)) {
    r <- tryCatch(extrair(reajustar(dados[-i, , drop = FALSE])), error = function(e) {
      .tr_multi_abort("tr_multi_error_jackknife_replicate",
                      "'%s': a réplica sem a linha %d falhou: %s", no, i, conditionMessage(e),
                      parent = e)
    })
    r <- r[nomes]
    if (anyNA(r)) {
      .tr_multi_abort("tr_multi_error_jackknife_replicate",
                      paste0("'%s': a réplica sem a linha %d não produziu %s — com uma linha a menos ",
                             "a técnica deu uma solução de outro tamanho."),
                      no, i, paste(nomes[is.na(r)], collapse = ", "))
    }
    reps[i, ] <- r
  }
  media <- colMeans(reps)
  tr <- if (isTRUE(log)) exp else identity
  if (identical(tabela, "resumo")) {
    vies <- (n - 1) * (media - theta)
    corrigida <- theta - vies
    ep <- sqrt((n - 1) / n * colSums(sweep(reps, 2L, media)^2))
    q <- stats::qt((1 + nivel) / 2, n - 1)
    # Os limites saem ANTES do tibble: dentro dele, `corrigida` já seria a coluna
    # exponenciada, e a razão de chances sairia com o intervalo exp(exp(.)).
    ic_inf <- corrigida - q * ep
    ic_sup <- corrigida + q * ep
    return(tibble::tibble(estatistica = nomes, estimativa = tr(unname(theta)),
                          media_jackknife = tr(unname(media)), vies = unname(vies),
                          corrigida = tr(unname(corrigida)), erro_padrao = unname(ep),
                          ic_inf = tr(unname(ic_inf)), ic_sup = tr(unname(ic_sup))))
  }
  # Coluna de entrada com o nome de uma coluna calculada sai: as calculadas
  # vencem (como em tr_multi_classify), e o tibble não aceita nome repetido.
  reservados <- c("obs", "estatistica", "sem_ela", "pseudovalor", "influencia")
  resto <- dados[, setdiff(names(dados), c(variaveis, reservados)), drop = FALSE]
  linhas <- rep(seq_len(n), times = length(nomes))
  sem_ela <- as.vector(reps)
  th <- rep(unname(theta), each = n)
  md <- rep(unname(media), each = n)
  tibble::as_tibble(cbind(
    data.frame(obs = linhas),
    resto[linhas, , drop = FALSE],
    data.frame(estatistica = rep(nomes, each = n), sem_ela = sem_ela,
               pseudovalor = n * th - (n - 1) * sem_ela, influencia = (n - 1) * (md - sem_ela),
               stringsAsFactors = FALSE)))
}

#' Matriz em vetor nomeado `linha:coluna`, na ordem das colunas.
#' @noRd
.tr_multi_achatar <- function(M) {
  stats::setNames(as.vector(M), paste(rep(rownames(M), times = ncol(M)),
                                      rep(colnames(M), each = nrow(M)), sep = ":"))
}

#' Troca o sinal de cada coluna de M para concordar com a de `ref`.
#' @noRd
.tr_multi_alinhar_sinal <- function(M, ref) {
  s <- sign(colSums(M * ref)); s[s == 0] <- 1
  M <- sweep(M, 2L, s, "*")
  dimnames(M) <- dimnames(ref)
  M
}

#' Todas as permutações de 1..k, uma por linha.
#' @noRd
.tr_multi_permutacoes <- function(k) {
  if (k == 1L) return(matrix(1L, 1L, 1L))
  sub <- .tr_multi_permutacoes(k - 1L)
  do.call(rbind, lapply(seq_len(k), function(i) {
    cbind(i, matrix(setdiff(seq_len(k), i)[sub], nrow(sub)))
  }))
}

#' Casa os fatores de L com os de `ref` por congruência de Tucker, e alinha o sinal.
#'
#' A congruência φ(a, b) = Σab / √(Σa² Σb²) é o cosseno entre duas colunas de
#' cargas. Até 5 fatores testa as 120 permutações e fica com a de maior Σ|φ|;
#' acima disso, casa gulosamente o par de maior |φ| e repete — com fatores bem
#' definidos dá o mesmo, e 8! permutações por réplica seriam minutos.
#' @noRd
.tr_multi_casar_fatores <- function(L, ref) {
  k <- ncol(ref)
  cong <- crossprod(L, ref) / sqrt(outer(colSums(L^2), colSums(ref^2)))
  # Coluna de cargas toda nula dá 0/0: sem direção, não é congruente com nada.
  # Sem isso o NaN estraga a escolha da permutação e o erro sai críptico.
  cong[is.na(cong)] <- 0
  if (k <= 5L) {
    perms <- .tr_multi_permutacoes(k)
    soma <- apply(perms, 1L, function(pm) sum(abs(cong[cbind(pm, seq_len(k))])))
    ordem <- perms[which.max(soma), ]
  } else {
    ordem <- integer(k)
    A <- abs(cong)
    for (t in seq_len(k)) {
      w <- which(A == max(A), arr.ind = TRUE)[1L, ]
      ordem[w[[2]]] <- w[[1]]
      A[w[[1]], ] <- -1; A[, w[[2]]] <- -1
    }
  }
  .tr_multi_alinhar_sinal(L[, ordem, drop = FALSE], ref)
}

.TR_MULTI_JK_PCA <- c("autovalores", "proporção", "cargas")
.TR_MULTI_JK_FA <- c("cargas", "comunalidades")
.TR_MULTI_JK_LDA <- c("correlação canônica", "autovalores", "coeficientes padronizados")
.TR_MULTI_JK_LOGIT <- c("coeficientes", "razões de chances")

#' "a, b, c": as colunas de um modelo de volta para o param `cols`.
#' @noRd
.tr_multi_cols_de <- function(x) paste(x, collapse = ", ")

#' Jackknife da PCA.
#' @param pca objeto `tr_multi_pca`.
#' @param estatistica `"autovalores"`, `"proporção"` ou `"cargas"` (correlações
#'   variável-componente).
#' @param tabela `"resumo"` ou `"pseudovalores"`.
#' @param nivel nível do intervalo.
#' @return tibble.
#' @export
tr_multi_jackknife_pca <- function(pca, estatistica = "autovalores", tabela = "resumo", nivel = 0.95) {
  no <- "multi/jackknife_pca"
  .tr_multi_pca_conferir(pca)
  estatistica <- .tr_multi_enum(estatistica, .TR_MULTI_JK_PCA, "estatistica")
  # O sinal de um autovetor é arbitrário: cada réplica é virada para concordar
  # com a amostra toda antes de entrar na média.
  ref <- .tr_multi_pca_cor(pca)
  extrair <- function(obj) {
    ev <- obj$ajuste$sdev^2
    switch(estatistica,
      autovalores = stats::setNames(ev, colnames(obj$ajuste$rotation)),
      "proporção" = stats::setNames(ev / sum(ev), colnames(obj$ajuste$rotation)),
      cargas = .tr_multi_achatar(.tr_multi_alinhar_sinal(.tr_multi_pca_cor(obj), ref)))
  }
  .tr_multi_jackknife(pca$dados, pca$variaveis, pca,
                      function(d) tr_multi_pca(d, cols = .tr_multi_cols_de(pca$variaveis),
                                               padronizar = pca$padronizado),
                      extrair, no, tabela, nivel)
}

#' Jackknife da análise fatorial.
#' @param fa objeto `tr_multi_fa`.
#' @param estatistica `"cargas"` ou `"comunalidades"`.
#' @inheritParams tr_multi_jackknife_pca
#' @return tibble.
#' @export
tr_multi_jackknife_fa <- function(fa, estatistica = "cargas", tabela = "resumo", nivel = 0.95) {
  no <- "multi/jackknife_fa"
  .tr_multi_guard(fa, "tr_multi_fa", setdiff(.TR_MULTI_CAMPOS_FA, "normalizar"),
                  "tr_multi_error_not_a_fa", "uma análise fatorial")
  estatistica <- .tr_multi_enum(estatistica, .TR_MULTI_JK_FA, "estatistica")
  # Objeto de cache antigo não tem `normalizar`; o default do nó é TRUE.
  normalizar <- if (is.null(fa$normalizar)) TRUE else isTRUE(fa$normalizar)
  extrair <- function(obj) {
    if (estatistica == "comunalidades") return(obj$comunalidade)
    # Rotacionada, uma réplica pode trazer os fatores em outra ordem, além do
    # sinal: casa por congruência com as cargas da amostra toda.
    .tr_multi_achatar(.tr_multi_casar_fatores(obj$cargas, fa$cargas))
  }
  .tr_multi_jackknife(fa$dados, fa$variaveis, fa,
                      function(d) tr_multi_factor_analysis(d, cols = .tr_multi_cols_de(fa$variaveis),
                                                           fatores = ncol(fa$cargas), metodo = fa$metodo,
                                                           rotacao = fa$rotacao, normalizar = normalizar,
                                                           escores = "nenhum"),
                      extrair, no, tabela, nivel)
}

#' Jackknife da discriminante linear.
#' @param modelo objeto `tr_multi_lda` (linear).
#' @param estatistica `"correlação canônica"`, `"autovalores"` ou
#'   `"coeficientes padronizados"`.
#' @inheritParams tr_multi_jackknife_pca
#' @return tibble.
#' @export
tr_multi_jackknife_discriminant <- function(modelo, estatistica = "correlação canônica",
                                            tabela = "resumo", nivel = 0.95) {
  no <- "multi/jackknife_discriminant"
  .tr_multi_modelo(modelo)
  estatistica <- .tr_multi_enum(estatistica, .TR_MULTI_JK_LDA, "estatistica")
  if (!identical(modelo$metodo, "linear")) {
    .tr_multi_abort("tr_multi_error_bad_option",
                    paste0("'%s': o modelo é quadrático, e a QDA não tem funções discriminantes para ",
                           "reamostrar. Ajuste um 'multi/discriminant' com metodo = \"linear\"."),
                    no)
  }
  padronizados <- function(obj) {
    t <- tr_multi_discriminant_functions(obj, "padronizados")
    M <- as.matrix(t[, -1L]); rownames(M) <- t$variavel
    M
  }
  ref <- if (estatistica == "coeficientes padronizados") padronizados(modelo) else NULL
  extrair <- function(obj) {
    if (estatistica == "coeficientes padronizados") {
      return(.tr_multi_achatar(.tr_multi_alinhar_sinal(padronizados(obj), ref)))
    }
    f <- tr_multi_discriminant_functions(obj, "funções")
    stats::setNames(if (estatistica == "autovalores") f$autovalor else f$correlacao_canonica, f$funcao)
  }
  # Só os preditores saem dos pseudovalores: o grupo fica ao lado da influência.
  .tr_multi_jackknife(modelo$dados, modelo$preditores, modelo,
                      function(d) tr_multi_discriminant(d, grupo = modelo$grupo,
                                                        cols = .tr_multi_cols_de(modelo$preditores),
                                                        metodo = modelo$metodo, priors = modelo$priors),
                      extrair, no, tabela, nivel)
}

#' Jackknife da regressão logística.
#' @param modelo objeto `tr_multi_logit`.
#' @param estatistica `"coeficientes"` ou `"razões de chances"`.
#' @inheritParams tr_multi_jackknife_pca
#' @return tibble; no resumo, também `erro_padrao_wald`.
#' @export
tr_multi_jackknife_logistic <- function(modelo, estatistica = "coeficientes", tabela = "resumo",
                                        nivel = 0.95) {
  no <- "multi/jackknife_logistic"
  .tr_multi_guard(modelo, "tr_multi_logit", .TR_MULTI_CAMPOS_LOGIT, "tr_multi_error_not_a_logit",
                  "uma regressão logística")
  estatistica <- .tr_multi_enum(estatistica, .TR_MULTI_JK_LOGIT, "estatistica")
  .tr_multi_sem_separacao(modelo, no)
  rotulos <- function(cf) {
    if (identical(modelo$tipo, "binária")) cf$termo else paste(cf$grupo, cf$termo, sep = ":")
  }
  extrair <- function(obj) {
    # Réplica que separa tem coeficiente arbitrário: vira erro da réplica.
    .tr_multi_sem_separacao(obj, no)
    cf <- .tr_multi_logit_coefs(obj)
    stats::setNames(cf$coeficiente, rotulos(cf))
  }
  tab <- .tr_multi_jackknife(modelo$dados, modelo$preditores, modelo,
                             function(d) tr_multi_logistic(d, grupo = modelo$grupo,
                                                           cols = .tr_multi_cols_de(modelo$preditores),
                                                           corte = if (is.na(modelo$corte)) 0.5 else modelo$corte,
                                                           metodo = if (is.null(modelo$metodo)) "ml" else modelo$metodo),
                             extrair, no, tabela, nivel, log = estatistica == "razões de chances")
  if (identical(tabela, "resumo")) {
    # O EP de Wald ao lado: quando os dois discordam muito, a curvatura da
    # verossimilhança não descreve bem a incerteza (amostra pequena, influência).
    cf <- .tr_multi_logit_coefs(modelo)
    tab$erro_padrao_wald <- unname(stats::setNames(cf$erro_padrao, rotulos(cf))[tab$estatistica])
  }
  tab
}

# --- Os nós --------------------------------------------------------------------

.TR_MULTI_JK_AJUDA_TABELAS <- r"---[
### As duas tabelas

**resumo** — uma linha por estatística: `estimativa` (a amostra toda),
`media_jackknife` (a média das n réplicas), `vies` = (n − 1)(média −
estimativa), `corrigida` = estimativa − viés, `erro_padrao` =
√((n − 1)/n · Σ(θ₍ᵢ₎ − média)²), e `ic_inf`/`ic_sup`, a corrigida ± t(n − 1) ·
erro padrão.

**pseudovalores** — uma linha por observação × estatística: `obs` (o número da
linha), as colunas que não entraram na técnica (o nome, o grupo), `sem_ela`
(θ₍ᵢ₎, a estatística sem a linha), `pseudovalor` = nθ − (n − 1)θ₍ᵢ₎ e
`influencia` = (n − 1)(média − θ₍ᵢ₎). A média dos pseudovalores é a corrigida;
a linha de influência muito maior que as outras é a observação que sozinha
move a estatística — ligue um `data/arrange` por `influencia`.

O intervalo supõe que a estatística é aproximadamente normal e suave; a tabela
de pseudovalores mostra as réplicas que fogem da curva.

O jackknife reajusta a técnica uma vez por linha: até 5000 linhas. Com mais,
sorteie uma amostra das linhas antes.
]---"

#' Os params comuns aos quatro nós, com o enum de `estatistica` de cada técnica.
#' @noRd
.tr_multi_jk_params <- function(padrao, opcoes) {
  list(
    estatistica = trama::tr_param_enum(padrao, opcoes, label = "Estatística"),
    tabela = trama::tr_param_enum("resumo", .TR_MULTI_JK_TABELAS, label = "Tabela"),
    nivel = trama::tr_param_num(0.95, min = 0.5, max = 0.999, label = "Nível do intervalo"))
}

#' A página de um nó: descrição própria + as duas tabelas; params e valor comuns.
#' @noRd
.tr_multi_jk_ajuda <- function(descricao, estatisticas, exemplo, veja) {
  .tr_multi_ajuda(
    paste(trimws(descricao), .TR_MULTI_JK_AJUDA_TABELAS, sep = "\n\n"),
    paste0("- **Estatística** — ", estatisticas, "\n",
           "- **Tabela** — `resumo` ou `pseudovalores`.\n",
           "- **Nível do intervalo** — 0,95 por padrão."),
    "Uma tabela (`data/table`), no formato descrito em \"As duas tabelas\".",
    exemplo, veja)
}

.tr_multi_nos_jackknife <- function() {
  TB <- "data/table"
  no <- function(id, fn, label, description, inputs, params, help) {
    trama::tr_node(id, fn = fn,
                   pressupostos = .tr_multi_doc(id)$pressupostos,
                   referencias = .tr_multi_doc(id)$referencias,
                   label = label, category = "multi_jackknife",
                   icon = trama::tr_icon("repeat"), description = description,
                   inputs = inputs, outputs = list(out = TB), params = params, help = help)
  }
  list(
    no("multi/jackknife_pca", tr_multi_jackknife_pca, "Jackknife da PCA",
      "Viés, erro padrão e influência de cada observação nos autovalores e cargas da PCA.",
      list(pca = "multi/pca"), .tr_multi_jk_params("autovalores", .TR_MULTI_JK_PCA),
      .tr_multi_jk_ajuda(r"---[
Tira cada observação, refaz a `multi/pca` com as mesmas colunas e a mesma
padronização, e mede quanto os **autovalores**, as **proporção**es de variância
ou as **cargas** (correlações variável-componente, as de `multi/pca_loadings`)
mudam.

Nas cargas, o sinal de cada componente da réplica é alinhado ao da amostra
toda — o sinal de um componente é arbitrário, e sem isso metade das réplicas
teria a carga com sinal trocado. Rótulos: `CP1` nos autovalores e proporções,
`variavel:CP1` nas cargas (`Murder:CP1`).

Com autovalores muito próximos entre si, dois componentes podem trocar de
ordem entre réplicas. Nas cargas, alinhadas só pelo sinal e não pela ordem,
isso aparece nos pseudovalores como `sem_ela` saltando entre dois patamares, e
o erro padrão sai inflado.

Nos `USArrests`, o erro padrão da carga de `Murder` em CP1 fica perto de 0,04,
e o de `UrbanPop` em CP2 perto de 0,10: o segundo componente é menos estável
que o primeiro. A carga mais incerta é a de `UrbanPop` em CP1 (perto de 0,19),
a variável que o primeiro componente mal representa.
]---", "`autovalores` (padrão), `proporção` ou `cargas`.", r"---[
tr_flow(reg) |>
  tr_add("ua", "multi/example", dataset = "USArrests") |>
  tr_add("pca", "multi/pca", from = "ua") |>
  tr_add("jk", "multi/jackknife_pca", estatistica = "cargas", from = "pca")
]---", r"---[
`multi/pca_loadings` para as cargas; `multi/biplot`.
]---")),

    no("multi/jackknife_fa", tr_multi_jackknife_fa, "Jackknife da fatorial",
      "Erro padrão e influência nas cargas rotacionadas e nas comunalidades.",
      list(fa = "multi/fa"), .tr_multi_jk_params("cargas", .TR_MULTI_JK_FA),
      .tr_multi_jk_ajuda(r"---[
Refaz a `multi/factor_analysis` sem cada linha, com o mesmo número de fatores,
método, rotação e normalização (sem escores), e mede as **cargas**
rotacionadas ou as **comunalidades**.

Rotacionar pode trocar a ORDEM dos fatores entre réplicas, além do sinal de
cada um: os fatores da réplica são casados com os da amostra toda pela
congruência de Tucker (a permutação de maior soma de |congruências|, gulosa
acima de 5 fatores) antes de alinhar o sinal. Rótulos: `variavel:F1` nas
cargas, o nome da variável nas comunalidades.

Uma réplica que cai num caso Heywood ou que não identifica o modelo vira erro
que nomeia a linha.
]---", "`cargas` (padrão) ou `comunalidades`.", r"---[
tr_flow(reg) |>
  tr_add("hf", "multi/example", dataset = "harman_fisicas") |>
  tr_add("af", "multi/factor_analysis", fatores = 2L, metodo = "paf", from = "hf") |>
  tr_add("jk", "multi/jackknife_fa", estatistica = "comunalidades", from = "af")
]---", r"---[
`multi/fa_loadings` para as cargas; `multi/parallel` para o número de fatores.
]---")),

    no("multi/jackknife_discriminant", tr_multi_jackknife_discriminant, "Jackknife da discriminante",
      "Erro padrão das correlações canônicas, autovalores e coeficientes padronizados.",
      list(modelo = "multi/lda"),
      .tr_multi_jk_params("correlação canônica", .TR_MULTI_JK_LDA),
      .tr_multi_jk_ajuda(r"---[
Refaz a `multi/discriminant` (linear) sem cada linha e mede a **correlação
canônica** e o **autovalor** de cada função (os de
`multi/discriminant_functions`, rótulos `LD1`, `LD2`, ...), ou os
**coeficientes padronizados** (rótulos `variavel:LD1`), com o sinal de cada
função alinhado ao da amostra toda.

A taxa de acerto não está aqui de propósito: o jackknife da classificação é a
validação cruzada, em `multi/confusion` e `multi/classify`.

A quadrática não tem funções discriminantes e é recusada.
]---", "`correlação canônica` (padrão), `autovalores` ou `coeficientes padronizados`.", r"---[
tr_flow(reg) |>
  tr_add("iris", "multi/example", dataset = "iris") |>
  tr_add("lda", "multi/discriminant", grupo = "Species", from = "iris") |>
  tr_add("jk", "multi/jackknife_discriminant", from = "lda")
]---", r"---[
`multi/discriminant_functions` para as funções; `multi/confusion` para o
jackknife da classificação.
]---")),

    no("multi/jackknife_logistic", tr_multi_jackknife_logistic, "Jackknife da logística",
      "Erro padrão dos coeficientes sem a aproximação de Wald, e a influência de cada linha.",
      list(modelo = "multi/logit"), .tr_multi_jk_params("coeficientes", .TR_MULTI_JK_LOGIT),
      .tr_multi_jk_ajuda(r"---[
Refaz a `multi/logistic` sem cada linha e mede os **coeficientes** (rótulo
`termo`, ou `grupo:termo` na multinomial).

O resumo traz também `erro_padrao_wald`, o erro padrão de Wald do coeficiente
(escala log), ao lado do jackknife: quando os dois discordam muito, a
aproximação de Wald de `multi/logistic_coefficients` não é confiável.

Em **razões de chances** o cálculo é feito no coeficiente (escala log) e
exponenciado no fim: estimativa, média, corrigida e intervalo saem em escala
de chances; viés, erro padrão e a tabela de pseudovalores ficam na escala log.

Modelo com separação é recusado, e uma réplica que separa vira erro que nomeia
a linha.
]---", "`coeficientes` (padrão) ou `razões de chances`.", r"---[
tr_flow(reg) |>
  tr_add("pima", "multi/example", dataset = "pima") |>
  tr_add("lg", "multi/logistic", grupo = "diabetes", cols = "glicose, imc, pedigree", from = "pima") |>
  tr_add("jk", "multi/jackknife_logistic", estatistica = "razões de chances", from = "lg")
]---", r"---[
`multi/logistic_coefficients` para os erros de Wald; `multi/classify` com
validação cruzada.
]---"))
  )
}
