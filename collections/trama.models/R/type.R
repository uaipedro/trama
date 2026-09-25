# Os quatro tipos da coleção, e os adaptadores que os ligam à `data`.
#
# - `models/fit`: o modelo ajustado. Tipo próprio porque dele saem VÁRIAS
#   coisas — quadro, coeficientes, médias, resíduos —, e um nó que devolvesse só
#   uma obrigaria a reajustar para cada outra.
# - `models/effects`: um quadro em que cada linha tem um p-valor (quadro da
#   ANOVA, coeficientes, comparações). Tipo próprio, e não `data/table`, pelo
#   CARD: o que se quer ver num quadro é quais linhas são significativas, e isso
#   pede um renderer que desenhe a régua do p-valor por linha. Registrar esse
#   renderer sob `data/table` mudaria toda tabela do app.
# - `models/test`: um teste, uma hipótese nula (doutrina da `series`).
# - `models/emm`: as médias ajustadas, com as letras.
#
# O preço de um tipo próprio seria perder a `data` e a `view`, e são os
# ADAPTADORES que o pagam: o motor os insere na aresta, sem caixa na tela.
#
# Todo `store` é FUNIL, na doutrina das irmãs.

#' O guard comum: classe e campos obrigatórios.
#' @noRd
.tr_models_guard <- function(x, classe, campos, erro, oque) {
  falta <- if (is.list(x)) setdiff(campos, names(x)) else campos
  if (!inherits(x, classe) || length(falta)) {
    .tr_models_abort(erro, "O nó produziu um objeto '%s', não %s%s.", class(x)[[1]], oque,
                     if (inherits(x, classe) && length(falta))
                       sprintf(" (faltam: %s)", paste(falta, collapse = ", ")) else "")
  }
  invisible(x)
}

#' Uma tabela no formato que o renderer `trama/table` do núcleo lê.
#' @noRd
.tr_models_linhas_json <- function(df, max = 60L) {
  df <- utils::head(as.data.frame(df), max)
  list(columns = as.list(names(df)),
       rows = lapply(seq_len(nrow(df)), function(i) {
         lapply(df[i, , drop = FALSE], function(v) {
           v <- v[[1]]
           if (is.factor(v)) as.character(v) else if (is.numeric(v) && is.na(v)) NULL else v
         })
       }))
}

# ---- models/fit --------------------------------------------------------------

.TR_MODELS_CAMPOS_FIT <- c("ajuste", "classe", "rotulo", "formula", "dados", "resposta",
                           "delineamento", "tratamentos", "bloco", "aux_lm", "aux_misto",
                           "descartadas")

#' Monta o objeto do modelo.
#'
#' - `ajuste`: o `lm`/`aov`, `glm`, `lmerModLmerTest` ou, na parcela
#'   subdividida, o `aovlist`.
#' - `classe`: `"lm"`, `"glm"`, `"lmer"` ou `"split"`. A classe S3 é
#'   `c("tr_models_<classe>", "tr_models_fit")`, e é por ela que os genéricos do
#'   contrato (`contrato.R`) despacham; o campo fica para os leitores que não são
#'   do contrato e para promover RDS antigos.
#' - `rotulo`: o que o card escreve no topo ("ANOVA · DBC").
#' - `formula`: a fórmula em texto, como foi ajustada.
#' - `dados`: as linhas USADAS, já com os fatores. `emmeans` e o SQ tipo III
#'   precisam reencontrar a tabela, e procurá-la no ambiente da chamada falha
#'   depois do RDS.
#' - `resposta`; `delineamento` (`NULL` fora dos blocos de ANOVA);
#'   `tratamentos` (os fatores de tratamento, sem o bloco); `bloco`.
#' - `aux_lm`: `lm` com os mesmos resíduos do erro de dentro (só na parcela
#'   subdividida); `aux_misto`: o `lmer` equivalente, para as médias.
#' - `descartadas`: linhas com faltante que ficaram fora.
#' @noRd
.tr_models_fit_obj <- function(ajuste, classe, rotulo, formula, dados, resposta,
                               delineamento = NULL, tratamentos = character(), bloco = NULL,
                               aux_lm = NULL, aux_misto = NULL, descartadas = 0L) {
  structure(list(ajuste = ajuste, classe = classe, rotulo = rotulo,
                 formula = paste(deparse(formula, width.cutoff = 500L), collapse = " "),
                 dados = dados, resposta = resposta, delineamento = delineamento,
                 tratamentos = tratamentos, bloco = bloco, aux_lm = aux_lm,
                 aux_misto = aux_misto, descartadas = as.integer(descartadas)),
            class = c(paste0("tr_models_", classe), "tr_models_fit"))
}

.tr_models_fit_conferir <- function(fit) {
  .tr_models_guard(fit, "tr_models_fit", .TR_MODELS_CAMPOS_FIT, "tr_models_error_not_a_fit",
                   "um modelo ajustado")
}

models_fit_type <- function() {
  trama::tr_type(
    "models/fit", version = 1L, label = "Modelo", color = .TR_MODELS_COR, ext = "rds",
    # O funil é o CONTRATO, não os campos: qualquer `tr_models_fit` com um
    # `tr_models_info()` válido entra (os campos fixos só se conferem nas
    # classes daqui). Grava-se o que `tr_models_serialize()` devolve — o objeto
    # inteiro, salvo em quem guarda ponteiro externo.
    store = function(x, path) {
      .tr_models_modelo_conferir(x)
      saveRDS(tr_models_serialize(x), path, compress = FALSE)
    },
    # RDS de antes do contrato volta com a subclasse derivada de `$classe`; o de
    # uma coleção que não está carregada para aqui, dizendo qual carregar.
    restore = function(path) {
      x <- .tr_models_promover(readRDS(path))
      .tr_models_exigir_metodos(x)
      tr_models_unserialize(x)
    },
    preview = function(x, ctx) tr_models_card(x, ctx)
  )
}

#' O que o card do modelo mostra: topo, fórmula, destaques, teste global e as
#' linhas de efeitos (coeficientes ou quadro tipo I).
#' @noRd
.tr_models_fit_preview <- function(fit) {
  est <- tr_models_fit_stats(fit)
  destaques <- list()
  add <- function(rotulo, valor, barra = FALSE, pct = FALSE) {
    if (length(valor) && !is.na(valor)) {
      destaques[[length(destaques) + 1L]] <<- list(rotulo = rotulo, valor = valor, barra = barra, pct = pct)
    }
  }
  add("R²", est$r2, barra = TRUE); add("R² aj.", est$r2_ajustado, barra = TRUE)
  add("R² marg.", est$r2_marginal, barra = TRUE); add("R² cond.", est$r2_condicional, barra = TRUE)
  add("desvio expl.", est$desvio_explicado, barra = TRUE)
  add("CV", est$cv_pct, pct = TRUE); add("AIC", est$aic)
  efeitos <- tryCatch(.tr_models_efeitos_do_fit(fit), error = function(e) NULL)
  list(
    rotulo = fit$rotulo, formula = fit$formula, n = est$n, descartadas = fit$descartadas,
    destaques = destaques,
    global = .tr_models_teste_global(fit),
    efeitos_titulo = if (is.null(efeitos)) NULL else efeitos$titulo,
    linhas = if (is.null(efeitos)) list() else .tr_models_linhas_sig(efeitos)
  )
}

# ---- models/effects ----------------------------------------------------------

.TR_MODELS_CAMPOS_EFEITOS <- c("tabela", "titulo", "coluna_estat", "rodape", "nota", "fonte")

#' Monta o quadro de efeitos.
#'
#' - `tabela`: tibble com `termo` e `p_valor` obrigatórios; o resto é do quadro.
#' - `titulo`: o que o card escreve no topo ("Quadro da ANOVA · SQ tipo I").
#' - `coluna_estat`: o nome da coluna da estatística (`"F"`, `"t"`), ou NULL.
#' - `rodape`: lista nomeada de textos curtos (`CV = "8,2%"`).
#' @noRd
.tr_models_efeitos <- function(tabela, titulo, coluna_estat = NULL, rodape = list(), nota = "",
                               fonte = "") {
  structure(list(tabela = tibble::as_tibble(tabela), titulo = titulo, coluna_estat = coluna_estat,
                 rodape = rodape, nota = nota, fonte = fonte), class = "tr_models_effects")
}

.tr_models_efeitos_conferir <- function(x) {
  .tr_models_guard(x, "tr_models_effects", .TR_MODELS_CAMPOS_EFEITOS,
                   "tr_models_error_not_effects", "um quadro de efeitos")
  if (!all(c("termo", "p_valor") %in% names(x$tabela))) {
    .tr_models_abort("tr_models_error_not_effects",
                     "O quadro de efeitos precisa das colunas 'termo' e 'p_valor'.")
  }
  invisible(x)
}

#' As linhas da régua: termo, p, estrelas e um detalhe curto.
#'
#' O detalhe é a estatística com o nome dela ("F 37,7"): o quadro inteiro está
#' na vista `tabela`, e aqui só o que ajuda a ler a régua.
#' @noRd
.tr_models_linhas_sig <- function(ef) {
  t <- ef$tabela
  lapply(seq_len(nrow(t)), function(i) {
    p <- t$p_valor[[i]]
    est <- if (!is.null(ef$coluna_estat) && ef$coluna_estat %in% names(t)) t[[ef$coluna_estat]][[i]] else NA
    # Com `grupo` (logística multinomial: um coeficiente por classe), o termo
    # sozinho repetiria "peso" três vezes na régua sem dizer de qual classe.
    termo <- as.character(t$termo[[i]])
    if ("grupo" %in% names(t) && !is.na(t$grupo[[i]])) termo <- paste0(t$grupo[[i]], " · ", termo)
    list(termo = termo,
         p = if (is.na(p)) NULL else p,
         estrelas = .tr_models_estrelas(p),
         detalhe = if (is.na(est)) NULL else paste(ef$coluna_estat, .tr_models_fmt(est)))
  })
}

#' Os cabeçalhos do quadro, na notação dos livros de experimentação.
#'
#' Um dicionário por NOME de coluna, e não um campo do objeto: toda tabela da
#' coleção usa os mesmos nomes (`gl`, `sq`, `p_valor`), e o cabeçalho de "gl" é
#' "GL" em qualquer quadro. Coluna que não está aqui sai com o próprio nome.
#' @noRd
.TR_MODELS_CABECALHOS <- c(
  termo = "FV", gl = "GL", gl_den = "GL den.", sq = "SQ", qm = "QM", F = "Fc", qui2 = "χ²",
  desvio = "Desvio", t = "t", z = "z", p_valor = "Pr > F", estimativa = "Estimativa",
  erro_padrao = "EP", li_95 = "LI 95%", ls_95 = "LS 95%", parametros = "Parâm.",
  log_verossimilhanca = "log-veross.", aic = "AIC")

#' O quadro para a vista `quadro`: colunas com cabeçalho e linhas cruas.
#'
#' O cabeçalho do p muda com a estatística: "Pr > F" no quadro da ANOVA,
#' "Pr > |t|" nos coeficientes, "Pr > χ²" na razão de verossimilhança.
#' @noRd
.tr_models_quadro_json <- function(ef) {
  t <- as.data.frame(ef$tabela)
  cab <- ifelse(names(t) %in% names(.TR_MODELS_CABECALHOS), .TR_MODELS_CABECALHOS[names(t)], names(t))
  names(cab) <- names(t)
  if ("termo" %in% names(t) && !grepl("ANOVA", ef$titulo)) cab[["termo"]] <- "termo"
  est <- ef$coluna_estat %||% "F"
  cab[["p_valor"]] <- switch(est, t = "Pr > |t|", z = "Pr > |z|", qui2 = "Pr > χ²", "Pr > F")
  list(colunas = lapply(names(t), function(n) list(chave = n, rotulo = unname(cab[[n]]))),
       linhas = .tr_models_linhas_json(t, max = 200L)$rows,
       estrelas = as.list(.tr_models_estrelas(t$p_valor)))
}

models_effects_type <- function() {
  trama::tr_type(
    "models/effects", version = 2L, label = "Quadro de efeitos", color = "#eab308", ext = "rds",
    store = function(x, path) {
      .tr_models_efeitos_conferir(x)
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    preview = function(x, ctx) {
      trama::tr_preview("models/effects", data = c(
        list(titulo = x$titulo, linhas = .tr_models_linhas_sig(x),
             rodape = if (length(x$rodape)) as.list(x$rodape) else NULL,
             nota = x$nota, fonte = x$fonte, quadro = .tr_models_quadro_json(x))))
    }
  )
}

#' Arredonda para a vista `tabela`: 4 significativos, p-valor intacto.
#' @noRd
.tr_models_arredondar <- function(df) {
  df <- as.data.frame(df)
  for (n in names(df)) if (is.double(df[[n]]) && n != "p_valor") df[[n]] <- signif(df[[n]], 4)
  df
}

# ---- models/test -------------------------------------------------------------

.TR_MODELS_CAMPOS_TESTE <- c("teste", "h0", "estatistica", "rotulo_estat", "gl", "p_valor",
                             "decisao_5", "conclusao", "efeito", "nota", "fonte")

#' O registro de um teste.
#'
#' Diferente da `series`, TODO teste daqui tem p-valor — e é isso que deixa o
#' card trocar os pontinhos de tabela pela régua. Um bloco que chegasse sem
#' p-valor seria um bug, e vira erro aqui em vez de card com "não rejeita"
#' confiante.
#'
#' `efeito`: `list(rotulo, valor, li, ls)` — a diferença de médias, a
#' correlação — ou NULL. O p-valor diz SE há efeito; o efeito diz de QUANTO, e é
#' a pergunta que o p-valor sozinho deixa sem resposta.
#' @noRd
.tr_models_teste <- function(teste, h0, estatistica, rotulo_estat, p_valor, gl = NA_character_,
                             conclusao_sim, conclusao_nao, efeito = NULL, nota = "", fonte,
                             extra = NULL) {
  p <- suppressWarnings(as.numeric(p_valor))
  if (length(p) != 1L || is.na(p)) {
    .tr_models_abort("tr_models_error_fit",
                     "'%s': o teste não devolveu p-valor (os dados são constantes?).", teste)
  }
  rejeita <- p < 0.05
  structure(list(
    teste = teste, h0 = h0, estatistica = as.numeric(estatistica), rotulo_estat = rotulo_estat,
    gl = as.character(gl), p_valor = p,
    decisao_5 = if (rejeita) "rejeita H0" else "não rejeita H0",
    conclusao = if (rejeita) conclusao_sim else conclusao_nao,
    efeito = efeito, nota = nota, fonte = fonte, extra = extra
  ), class = "tr_models_test")
}

.tr_models_teste_conferir <- function(x) {
  .tr_models_guard(x, "tr_models_test", .TR_MODELS_CAMPOS_TESTE, "tr_models_error_not_a_test",
                   "o resultado de um teste")
}

.tr_models_teste_json <- function(x) {
  list(teste = x$teste, h0 = x$h0, estatistica = x$estatistica, rotulo_estat = x$rotulo_estat,
       gl = if (is.na(x$gl)) NULL else x$gl, p_valor = x$p_valor,
       estrelas = .tr_models_estrelas(x$p_valor), decisao_5 = x$decisao_5,
       conclusao = x$conclusao,
       efeito = if (is.null(x$efeito)) NULL else lapply(x$efeito, function(v) if (is.numeric(v) && is.na(v)) NULL else v),
       nota = x$nota, fonte = x$fonte,
       extra = if (is.null(x$extra)) NULL else as.list(x$extra))
}

models_test_type <- function() {
  trama::tr_type(
    "models/test", version = 1L, label = "Teste", color = "#ef4444", ext = "rds",
    store = function(x, path) {
      .tr_models_teste_conferir(x)
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    preview = function(x, ctx) trama::tr_preview("models/test", data = .tr_models_teste_json(x))
  )
}

#' Teste -> tabela: UMA linha, para `data/bind_rows` montar o relatório.
#' @noRd
.tr_models_teste_tabela <- function(x) {
  ef <- x$efeito
  base <- tibble::tibble(
    teste = x$teste, h0 = x$h0, estatistica = x$estatistica, rotulo_estat = x$rotulo_estat,
    gl = x$gl, p_valor = x$p_valor, significancia = .tr_models_estrelas(x$p_valor),
    decisao_5 = x$decisao_5, conclusao = x$conclusao,
    efeito = if (is.null(ef)) NA_character_ else ef$rotulo,
    efeito_valor = if (is.null(ef)) NA_real_ else as.numeric(ef$valor),
    efeito_li_95 = if (is.null(ef) || is.null(ef$li)) NA_real_ else as.numeric(ef$li),
    efeito_ls_95 = if (is.null(ef) || is.null(ef$ls)) NA_real_ else as.numeric(ef$ls),
    nota = x$nota, fonte = x$fonte)
  base
}

# ---- models/emm --------------------------------------------------------------

.TR_MODELS_CAMPOS_EMM <- c("grade", "tabela", "especs", "por", "ajuste", "alfa", "resposta", "nota")

#' Monta as médias ajustadas.
#'
#' - `grade`: o `emmGrid` (é dele que `models/pairwise` tira os contrastes).
#' - `tabela`: tibble com os fatores, `media`, `erro_padrao`, `gl`, `li`, `ls` e
#'   `grupo` (as letras).
#' - `especs`, `por`: os fatores das médias e os de condição.
#' - `ajuste`, `alfa`: com que correção e nível as letras foram feitas.
#' - `resposta`: o nome da resposta, para o eixo do gráfico.
#' @noRd
.tr_models_emm_obj <- function(grade, tabela, especs, por, ajuste, alfa, resposta, nota = "") {
  structure(list(grade = grade, tabela = tabela, especs = especs, por = por, ajuste = ajuste,
                 alfa = alfa, resposta = resposta, nota = nota), class = "tr_models_emm")
}

.tr_models_emm_conferir <- function(x) {
  .tr_models_guard(x, "tr_models_emm", .TR_MODELS_CAMPOS_EMM, "tr_models_error_not_emm",
                   "uma grade de médias ajustadas")
}

models_emm_type <- function() {
  trama::tr_type(
    "models/emm", version = 1L, label = "Médias ajustadas", color = "#8b5cf6", ext = "rds",
    store = function(x, path) {
      .tr_models_emm_conferir(x)
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) list(medias = nrow(x$tabela), fatores = paste(x$especs, collapse = ", "),
                               por = paste(x$por, collapse = ", "), letras = x$ajuste,
                               nota = x$nota),
    # O card é o GRÁFICO das médias com as letras: é a figura que vai para o
    # artigo, e a pergunta "quem difere de quem" se lê nela sem abrir nada.
    preview = function(x, ctx) trama.view::tr_view_render(tr_models_plot_means(x), ctx)
  )
}

.tr_models_adapters <- function() {
  list(
    trama::tr_adapter("models/fit", "data/table", tr_models_as_table),
    trama::tr_adapter("models/effects", "data/table", function(x) x$tabela),
    trama::tr_adapter("models/test", "data/table", .tr_models_teste_tabela),
    trama::tr_adapter("models/emm", "data/table", function(x) x$tabela)
  )
}
