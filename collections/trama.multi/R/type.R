# Os três tipos da coleção, e os adaptadores que os ligam à `data`.
#
# Tipo próprio, e não `data/table`, porque cada técnica produz VÁRIAS tabelas
# que só fazem sentido juntas: a PCA tem autovalores, cargas e escores; a
# fatorial tem cargas, comunalidades e a correlação entre fatores; a
# discriminante tem funções, escores e o modelo que classifica caso novo. Um
# nó que devolvesse só uma delas obrigaria a refazer o ajuste para cada outra.
#
# O preço de um tipo próprio seria perder a `data` e a `view`, e são os
# ADAPTADORES que o pagam: o motor os insere na aresta, sem caixa na tela. Cada
# tipo vira a tabela que se quer levar adiante — os escores da PCA e da
# discriminante (para `view/points`), as cargas da fatorial (para exportar).
#
# Os objetos são listas com classe própria, e não o `prcomp`/`factanal`/`lda`
# cru, porque carregam a TABELA de origem: os escores saem ao lado das colunas
# que não entraram na técnica (a espécie, o nome do estado), e é isso que deixa
# colorir um biplot pelo grupo sem um `data/join` no meio.
#
# Todo `store` é FUNIL, na doutrina das irmãs.

#' O guard comum: classe e campos obrigatórios.
#' @noRd
.tr_multi_guard <- function(x, classe, campos, erro, oque) {
  falta <- if (is.list(x)) setdiff(campos, names(x)) else campos
  if (!inherits(x, classe) || length(falta)) {
    .tr_multi_abort(erro, "O nó produziu um objeto '%s', não %s%s.", class(x)[[1]], oque,
                    if (inherits(x, classe) && length(falta))
                      sprintf(" (faltam: %s)", paste(falta, collapse = ", ")) else "")
  }
  invisible(x)
}

.TR_MULTI_CAMPOS_PCA <- c("ajuste", "dados", "variaveis", "padronizado")

#' Monta o objeto da PCA.
#'
#' - `ajuste`: o `prcomp` (com `x`, os escores).
#' - `dados`: a tabela de entrada inteira, na ordem das linhas.
#' - `variaveis`: as colunas que entraram.
#' - `padronizado`: se a PCA foi na correlação (TRUE) ou na covariância.
#' @noRd
.tr_multi_pca_obj <- function(ajuste, dados, variaveis, padronizado) {
  structure(list(ajuste = ajuste, dados = dados, variaveis = variaveis,
                 padronizado = padronizado), class = "tr_multi_pca")
}

multi_pca_type <- function() {
  trama::tr_type(
    "multi/pca", version = 1L, label = "PCA", color = .TR_MULTI_COR, ext = "rds",
    store = function(x, path) {
      .tr_multi_guard(x, "tr_multi_pca", .TR_MULTI_CAMPOS_PCA, "tr_multi_error_not_a_pca",
                      "uma análise de componentes principais")
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) .tr_multi_pca_resumo(x),
    # O card de uma PCA é a VARIÂNCIA EXPLICADA: é o que decide quantos
    # componentes olhar, e é a primeira pergunta antes de qualquer biplot.
    preview = function(x, ctx) trama.view::tr_view_render(tr_multi_scree(x), ctx)
  )
}

.TR_MULTI_CAMPOS_FA <- c("cargas", "cargas_brutas", "rotacao", "rotmat", "phi", "estrutura",
                         "comunalidade", "unicidade", "metodo", "escores", "dados",
                         "variaveis", "correlacao", "n", "ajuste_ml", "convergiu", "normalizar")

#' Monta o objeto da análise fatorial.
#'
#' - `cargas`: matriz p × k das cargas ROTACIONADAS (na oblíqua, o padrão),
#'   linhas nomeadas pelas variáveis, colunas `F1..Fk`.
#' - `cargas_brutas`: as mesmas, antes da rotação.
#' - `rotacao`: nome da rotação (`"nenhuma"`, `"varimax"`, ...).
#' - `rotmat`: matriz k × k T com `cargas = cargas_brutas %*% t(solve(T))` na
#'   oblíqua e `cargas_brutas %*% T` na ortogonal; identidade sem rotação.
#' - `phi`: correlação entre fatores (k × k); identidade na ortogonal.
#' - `estrutura`: `cargas %*% phi` (correlação variável-fator).
#' - `comunalidade`, `unicidade`: vetores nomeados por variável.
#' - `metodo`: `"ml"` ou `"paf"`.
#' - `escores`: matriz n × k, ou NULL quando não pedidos.
#' - `dados`, `variaveis`, `correlacao` (p × p), `n` (observações).
#' - `ajuste_ml`: `list(estatistica, gl, p_valor)` no ML, NULL na PAF.
#' - `convergiu`: lógico (iterações da PAF e da rotação).
#' - `normalizar`: se a rotação usou a normalização de Kaiser — o jackknife precisa
#'   para reajustar igual.
#' @noRd
.tr_multi_fa_obj <- function(...) {
  x <- list(...)
  structure(x[.TR_MULTI_CAMPOS_FA], class = "tr_multi_fa")
}

multi_fa_type <- function() {
  trama::tr_type(
    "multi/fa", version = 1L, label = "Análise fatorial", color = "#c084fc", ext = "rds",
    store = function(x, path) {
      .tr_multi_guard(x, "tr_multi_fa", .TR_MULTI_CAMPOS_FA, "tr_multi_error_not_a_fa",
                      "uma análise fatorial")
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) .tr_multi_fa_resumo(x),
    # O card é o MAPA DAS CARGAS: é nele que se lê que fator é qual.
    preview = function(x, ctx) trama.view::tr_view_render(tr_multi_plot_loadings(x), ctx)
  )
}

.TR_MULTI_CAMPOS_LDA <- c("ajuste", "metodo", "grupo", "preditores", "dados", "priors")

#' Monta o objeto da discriminante.
#'
#' - `ajuste`: o `MASS::lda` ou `MASS::qda`.
#' - `metodo`: `"linear"` ou `"quadrática"`.
#' - `grupo`: nome da coluna do grupo; `preditores`: as variáveis.
#' - `dados`: a tabela de treino inteira (sem as linhas descartadas: não há
#'   descarte, faltante é erro).
#' - `priors`: `"proporcionais"` ou `"iguais"`.
#' @noRd
.tr_multi_lda_obj <- function(ajuste, metodo, grupo, preditores, dados, priors) {
  structure(list(ajuste = ajuste, metodo = metodo, grupo = grupo, preditores = preditores,
                 dados = dados, priors = priors), class = "tr_multi_lda")
}

multi_lda_type <- function() {
  trama::tr_type(
    "multi/lda", version = 1L, label = "Discriminante", color = "#fb923c", ext = "rds",
    store = function(x, path) {
      .tr_multi_guard(x, "tr_multi_lda", .TR_MULTI_CAMPOS_LDA, "tr_multi_error_not_a_lda",
                      "uma análise discriminante")
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) .tr_multi_lda_resumo(x),
    preview = function(x, ctx) trama.view::tr_view_render(tr_multi_plot_discriminant(x), ctx)
  )
}

.TR_MULTI_CAMPOS_LOGIT <- c("ajuste", "tipo", "grupo", "preditores", "niveis", "dados",
                            "corte", "separacao")

#' Monta o objeto da regressão logística.
#'
#' - `ajuste`: o `glm` binomial (2 grupos) ou o `nnet::multinom` (3+), ajustado
#'   com os preditores renomeados `v1..vp` — nome de coluna com espaço ou
#'   acento quebraria a fórmula, e o mapa volta pela ordem de `preditores`.
#' - `tipo`: `"binária"` ou `"multinomial"`.
#' - `grupo`, `preditores`, `niveis` (o primeiro é a referência), `dados`.
#' - `corte`: probabilidade do SEGUNDO nível a partir da qual se prevê ele
#'   (só na binária; `NA` na multinomial).
#' - `separacao`: os grupos separados sem sobreposição (`character()` se nenhum).
#' @noRd
.tr_multi_logit_obj <- function(ajuste, tipo, grupo, preditores, niveis, dados, corte, separacao) {
  structure(list(ajuste = ajuste, tipo = tipo, grupo = grupo, preditores = preditores,
                 niveis = niveis, dados = dados, corte = corte, separacao = separacao),
            class = "tr_multi_logit")
}

multi_logit_type <- function() {
  trama::tr_type(
    "multi/logit", version = 1L, label = "Logística", color = "#f59e0b", ext = "rds",
    store = function(x, path) {
      .tr_multi_guard(x, "tr_multi_logit", .TR_MULTI_CAMPOS_LOGIT, "tr_multi_error_not_a_logit",
                      "uma regressão logística")
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) .tr_multi_logit_resumo(x),
    preview = function(x, ctx) trama.view::tr_view_render(.tr_multi_logit_preview(x), ctx)
  )
}

#' O gráfico do card de uma logística, igual em `multi/logit` e `multi/classifier`.
#'
#' O card é o das RAZÕES DE CHANCES; com separação elas não existem (vão ao
#' infinito), e o card cai para a ROC por resubstituição, que continua honesta.
#' Num helper só para os dois tipos não divergirem.
#' @noRd
.tr_multi_logit_preview <- function(x) {
  if (length(x$separacao)) tr_multi_roc(x, validacao = "resubstituição") else tr_multi_plot_odds(x)
}

#' O tipo-porta dos nós que classificam.
#'
#' Não é um modelo novo: é a porta que aceita os DOIS classificadores. Porta no
#' trama tem um tipo só, e a compatibilidade é igualdade ou adaptador; os
#' adaptadores identidade `multi/lda -> multi/classifier` e
#' `multi/logit -> multi/classifier` deixam `multi/confusion` receber a LDA e a
#' logística sem duplicar nó nem mexer no núcleo. O valor é o objeto intacto.
#' @noRd
multi_classifier_type <- function() {
  trama::tr_type(
    "multi/classifier", version = 1L, label = "Classificador", color = "#fb923c", ext = "rds",
    store = function(x, path) {
      .tr_multi_classificador(x)
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) {
      if (inherits(x, "tr_multi_logit")) .tr_multi_logit_resumo(x) else .tr_multi_lda_resumo(x)
    },
    preview = function(x, ctx) {
      p <- if (inherits(x, "tr_multi_logit")) .tr_multi_logit_preview(x)
           else tr_multi_plot_discriminant(x)
      trama.view::tr_view_render(p, ctx)
    }
  )
}

#' Confere que o valor é um classificador, e diz qual.
#' @return `"lda"` ou `"logit"`.
#' @noRd
.tr_multi_classificador <- function(x) {
  if (inherits(x, "tr_multi_lda")) {
    .tr_multi_guard(x, "tr_multi_lda", .TR_MULTI_CAMPOS_LDA, "tr_multi_error_not_a_lda",
                    "uma análise discriminante")
    return("lda")
  }
  if (inherits(x, "tr_multi_logit")) {
    .tr_multi_guard(x, "tr_multi_logit", .TR_MULTI_CAMPOS_LOGIT, "tr_multi_error_not_a_logit",
                    "uma regressão logística")
    return("logit")
  }
  .tr_multi_abort("tr_multi_error_not_a_classifier",
                  "O nó produziu um objeto '%s', e a porta espera um classificador (discriminante ou logística).",
                  class(x)[[1]])
}

.tr_multi_adapters <- function() {
  list(
    trama::tr_adapter("multi/pca", "data/table", .tr_multi_pca_tabela),
    trama::tr_adapter("multi/fa", "data/table", .tr_multi_fa_tabela),
    trama::tr_adapter("multi/lda", "data/table", .tr_multi_lda_tabela),
    trama::tr_adapter("multi/logit", "data/table", .tr_multi_logit_tabela),
    trama::tr_adapter("multi/lda", "multi/classifier", function(x) x),
    trama::tr_adapter("multi/logit", "multi/classifier", function(x) x)
  )
}
