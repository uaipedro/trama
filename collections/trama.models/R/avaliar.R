# Avaliar a previsão: matriz de confusão, curva ROC, métricas e importância.
#
# Até a Fase 4 cada coleção tinha os seus: a `multi` lia o seu LDA/logit (com
# validação cruzada), a `ml` lia uma TABELA com `.pred` (o teste da divisão). O
# contrato junta os dois caminhos num bloco só, com as duas entradas
# opcionais, porque as duas perguntas são legítimas:
#
# - só `modelo` → quanto ele acerta no TREINO, por `tr_models_predict_cv()`
#   (a cruzada é a estimativa honesta sem separar teste);
# - `modelo` + `dados` → quanto ele acerta numa tabela que não viu (o teste da
#   `ml/split`), prevendo-a aqui mesmo;
# - só `dados` → a tabela já tem real e previsto (de um `models/predict`, ou de
#   fora do trama), e o bloco só conta.
#
# Os três modos terminam no MESMO par (real, previsto/probabilidade), e daí em
# diante a conta é uma só — é o que garante que a confusão pela cruzada e a da
# tabela que um `models/predict` cruzado gerou dão o mesmo número.

.TR_MODELS_VALIDACAO_PADRAO <- "cruzada"

#' Qual dos três modos, pelas entradas ligadas; nenhuma é erro com classe.
#' @noRd
.tr_models_modo <- function(modelo, dados, no) {
  if (is.null(modelo) && is.null(dados)) {
    .tr_models_abort("tr_models_error_no_input",
                     paste0("'%s' precisa de uma entrada: ligue um modelo (avalia no treino, ou em ",
                            "'dados' se ela também estiver ligada) ou uma tabela com a resposta e o ",
                            "previsto (modo tabela)."), no)
  }
  if (is.null(modelo)) "tabela" else if (is.null(dados)) "treino" else "novos"
}

#' O par (real, previsão) de um modelo, no treino ou numa tabela nova.
#'
#' `real` sai como TEXTO na classificação (0/1 e lógico viram "0"/"TRUE", os
#' mesmos rótulos de `info$niveis`), e número na regressão.
#' @return list(real, previsto, prob, niveis, tarefa, corte, resposta, origem)
#' @noRd
.tr_models_par_modelo <- function(modelo, dados, validacao, no) {
  .tr_models_modelo_conferir(modelo)
  info <- tr_models_info(modelo)
  if (is.null(dados)) {
    validacao <- .tr_models_validacao(validacao)
    real <- if (is.data.frame(modelo$dados)) modelo$dados[[info$resposta]] else NULL
    if (is.null(real)) {
      .tr_models_abort("tr_models_error_not_applicable",
                       "'%s': o modelo de classe '%s' não guarda a tabela do ajuste ($dados), e sem ela não há o real do treino. Ligue também 'dados'.",
                       no, class(modelo)[[1]])
    }
    p <- tr_models_predict_cv(modelo, validacao)
    origem <- sprintf("validação %s", validacao)
  } else {
    if (!info$resposta %in% names(dados)) {
      .tr_models_abort("tr_models_error_unknown_column",
                       "'%s': 'dados' não tem a coluna '%s', a resposta do modelo — sem ela não há o que comparar com o previsto.",
                       no, info$resposta)
    }
    real <- dados[[info$resposta]]
    p <- tr_models_predict_raw(modelo, .tr_models_novos(modelo, dados, no))
    origem <- "dados novos"
  }
  classif <- info$tarefa == "classificacao"
  list(real = if (classif) as.character(real) else as.numeric(real),
       previsto = if (classif) as.character(p$previsto) else as.numeric(p$previsto),
       prob = p$prob, niveis = info$niveis, tarefa = info$tarefa,
       corte = modelo$corte %||% 0.5, resposta = info$resposta, origem = origem)
}

#' Uma coluna da tabela, pelo nome do param, com a mensagem que nomeia o param.
#' @noRd
.tr_models_coluna_tabela <- function(dados, valor, param, no) {
  nome <- .tr_models_obrigatorio(valor, param)
  if (!nome %in% names(dados)) {
    .tr_models_abort("tr_models_error_unknown_column",
                     "'%s': param '%s' nomeia a coluna '%s', que 'dados' não tem. Colunas: %s.",
                     no, param, nome, paste(names(dados), collapse = ", "))
  }
  nome
}

#' Os níveis de uma classe lida de tabela: os do fator, ou os valores em ordem.
#' @noRd
.tr_models_niveis_tabela <- function(...) {
  xs <- list(...)
  niv <- unlist(lapply(xs, function(x) if (is.factor(x)) levels(x) else sort(unique(as.character(x[!is.na(x)])))))
  unique(niv)
}

#' Tira os pares com NA (a cruzada deixa NA onde um nível sumiu do treino).
#' @noRd
.tr_models_sem_na <- function(par) {
  ok <- !is.na(par$real) & !is.na(par$previsto)
  if (!is.null(par$prob)) ok <- ok & stats::complete.cases(par$prob)
  par$real <- par$real[ok]; par$previsto <- par$previsto[ok]
  if (!is.null(par$prob)) par$prob <- par$prob[ok, , drop = FALSE]
  par
}

.tr_models_exigir_classif <- function(par, no) {
  if (par$tarefa != "classificacao") {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'%s' é de classificação, e o modelo prevê um número (regressão). Para ",
                            "medir o erro de uma regressão, use 'models/evaluate'."), no)
  }
  invisible(par)
}

# ---- Matriz de confusão -------------------------------------------------------

#' A matriz no formato LARGO: real × uma coluna por previsto, total e acerto.
#'
#' O formato é o da `multi` (porte de `tr_multi_confusion`): lê-se como a
#' matriz do livro, e a última linha (`real = "total"`) dá o acerto geral. O
#' formato longo da `ml` (observado, previsto, n) é um `data/pivot_longer`
#' daqui — o inverso exigiria o `pivot_wider` a todo mundo que só quer ler.
#' @noRd
.tr_models_matriz_larga <- function(real, previsto, niveis) {
  m <- table(factor(real, levels = niveis), factor(previsto, levels = niveis))
  cont <- matrix(as.integer(m), nrow(m), dimnames = list(NULL, niveis))
  # Um nível chamado "total" colidiria com a coluna do total: ganha o sufixo.
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

#' Matriz de confusão: classe real × classe prevista.
#'
#' @param modelo um `models/fit` de classificação, ou `NULL` (modo tabela).
#' @param dados tabela: sem `modelo`, já com a resposta e o previsto; com
#'   `modelo`, a tabela a prever (precisa da resposta do modelo).
#' @param validacao só com `modelo` sem `dados`: `"cruzada"` (cada linha
#'   prevista sem ela) ou `"resubstituição"`.
#' @param resposta,predito só no modo tabela: as colunas da classe real e da
#'   prevista.
#' @return tibble: `real`, uma coluna de contagem por classe prevista, `total`,
#'   `acertos`, `taxa_acerto`; a última linha, `real = "total"`, é o geral.
#' @export
tr_models_confusion <- function(modelo = NULL, dados = NULL, validacao = "cruzada", resposta = "",
                                predito = "previsto") {
  no <- "models/confusion"
  if (.tr_models_modo(modelo, dados, no) == "tabela") {
    r <- .tr_models_coluna_tabela(dados, resposta, "resposta", no)
    pc <- .tr_models_coluna_tabela(dados, predito, "predito", no)
    niv <- .tr_models_niveis_tabela(dados[[r]], dados[[pc]])
    par <- list(real = as.character(dados[[r]]), previsto = as.character(dados[[pc]]))
  } else {
    par <- .tr_models_exigir_classif(.tr_models_par_modelo(modelo, dados, validacao, no), no)
    niv <- par$niveis
  }
  par <- .tr_models_sem_na(par)
  .tr_models_matriz_larga(par$real, par$previsto, niv)
}

# ---- ROC ----------------------------------------------------------------------

#' Pontos da curva e AUC para um escore e um vetor lógico de positivos.
#'
#' Porte de `.tr_multi_roc_curva`: os cortes são os escores distintos, do
#' maior ao menor; empates andam na diagonal. A AUC é a de Mann-Whitney com
#' meio ponto por empate — exatamente a área trapezoidal dessa curva, que é a
#' que a `ml/roc` somava; as duas coleções davam o mesmo número por contas
#' diferentes, e agora é uma conta só.
#' @noRd
.tr_models_roc_curva <- function(score, positivo) {
  n1 <- sum(positivo); n0 <- sum(!positivo)
  if (n1 == 0L || n0 == 0L) {
    .tr_models_abort("tr_models_error_one_level",
                     "'models/roc': a curva pede casos das duas classes, e só há %s.",
                     if (n1 == 0L) "negativos" else "positivos")
  }
  cortes <- sort(unique(score), decreasing = TRUE)
  tpr <- vapply(cortes, function(k) sum(score >= k & positivo) / n1, 0)
  fpr <- vapply(cortes, function(k) sum(score >= k & !positivo) / n0, 0)
  auc <- (sum(rank(score)[positivo]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  list(pontos = data.frame(fpr = c(0, fpr), tpr = c(0, tpr)), auc = auc)
}

.tr_models_virgula <- function(x, d = 3L) formatC(x, format = "f", digits = d, decimal.mark = ",")

#' As curvas: uma (binária, ou a `positiva` contra as outras) ou uma por classe.
#'
#' `corte` só marca ponto na binária: com três classes a regra é "a mais
#' provável", e não um corte na probabilidade de uma delas.
#' @return list(curvas = data.frame(fpr, tpr, classe, auc), ponto, positiva)
#' @noRd
.tr_models_roc_dados <- function(real, prob, niveis, positiva = "", corte = 0.5) {
  binaria <- length(niveis) == 2L
  if (.tr_models_preenchido(positiva)) {
    positiva <- .tr_models_enum(trimws(positiva), niveis, "positiva")
  } else if (binaria) {
    positiva <- niveis[[2]]  # como na logística: o segundo nível é o "sucesso"
  } else {
    positiva <- ""
  }
  classes <- if (nzchar(positiva)) positiva else niveis
  curvas <- lapply(classes, function(l) {
    cur <- .tr_models_roc_curva(prob[, l], real == l)
    cbind(cur$pontos, classe = l, auc = cur$auc)
  })
  ponto <- NULL
  if (binaria) {
    # A regra do modelo é "p(2º nível) >= corte" (como na logística), e não um
    # corte na probabilidade da positiva: com a positiva no 1º nível e corte
    # != 0,5, `p(1º) >= corte` marcaria um ponto que o modelo nunca usa.
    s <- prob[, niveis[[2]]] >= corte
    if (positiva != niveis[[2]]) s <- !s
    ponto <- data.frame(fpr = mean(s[real != positiva]), tpr = mean(s[real == positiva]))
  }
  list(curvas = do.call(rbind, curvas), ponto = ponto, positiva = positiva, corte = corte)
}

#' Curva ROC: sensibilidade × especificidade em todos os cortes, com a AUC.
#'
#' @inheritParams tr_models_confusion
#' @param positiva a classe positiva; vazio = o segundo nível (binária) ou uma
#'   curva por classe, cada uma contra as outras (três ou mais).
#' @param probabilidade só no modo tabela: a coluna da probabilidade da
#'   positiva; vazio = `prob_<positiva>` (ou todas as `prob_<nivel>` na
#'   multiclasse).
#' @inheritParams trama.view::tr_view_finish
#' @return ggplot.
#' @export
tr_models_roc <- function(modelo = NULL, dados = NULL, validacao = "cruzada", resposta = "",
                          probabilidade = "", positiva = "", aspecto = "1:1", tema = "padrão",
                          titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  no <- "models/roc"
  par <- .tr_models_par_prob(modelo, dados, validacao, resposta, probabilidade, positiva, no)
  positiva <- par$positiva
  rd <- .tr_models_roc_dados(par$real, par$prob, par$niveis, positiva, par$corte)
  .tr_models_roc_grafico(rd, par, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Curva precisão-revocação, com a precisão média (AP) e a área de Davis & Goadrich.
#'
#' Veio da `ml/pr_curve` (main) na integração 9.2, com os três modos da
#' `models/roc`: modelo (validação no treino), modelo + dados novos, ou só a
#' tabela com a probabilidade. As contas não mudaram (`.tr_models_pr_pontos`,
#' a mesma da `ml`): um ponto por limiar distinto, empates num degrau; AP =
#' Σ ΔR·P, sem interpolação; `area` com a interpolação não linear de Davis &
#' Goadrich (2006), integrada em forma fechada (Keilwagen, Grosse & Grau 2014).
#' A referência do acaso é a prevalência da positiva (Saito & Rehmsmeier 2015).
#' @inheritParams tr_models_roc
#' @param positiva classe de interesse. Vazia: no modo tabela, a do nome da
#'   coluna `prob_<classe>` (ou o segundo nível sem coluna); com modelo, o
#'   segundo nível. Com três ou mais classes é obrigatória (a curva é ela
#'   contra as outras).
#' @return ggplot; os dados trazem `limiar`, `recall`, `precision`, `ap`,
#'   `area` e `prevalencia`.
#' @export
tr_models_pr_curve <- function(modelo = NULL, dados = NULL, validacao = "cruzada", resposta = "",
                               probabilidade = "", positiva = "", aspecto = "16:9", tema = "padrão",
                               titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  no <- "models/pr_curve"
  par <- .tr_models_par_prob(modelo, dados, validacao, resposta, probabilidade, positiva, no)
  pos <- par$positiva
  if (!nzchar(pos)) {
    if (length(par$niveis) > 2L) {
      .tr_models_abort("tr_models_error_blank_param",
                       "'%s': com %d classes, diga em 'positiva' qual é a classe de interesse (a curva é ela contra as outras).",
                       no, length(par$niveis))
    }
    pos <- par$niveis[[2]]
  }
  pos <- .tr_models_enum(pos, par$niveis, "positiva")
  prob <- as.numeric(par$prob[, pos])
  positivo <- par$real == pos
  if (length(unique(positivo)) < 2L) {
    .tr_models_abort("tr_models_error_one_level",
                     "'%s': as linhas avaliadas têm uma classe só; a curva pede positivos e negativos.", no)
  }
  if (any(!is.finite(prob)) || any(prob < 0 | prob > 1)) {
    .tr_models_abort("tr_models_error_not_applicable",
                     "'%s': a probabilidade tem de ser finita e entre 0 e 1.", no)
  }
  d <- .tr_models_pr_pontos(positivo, prob)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$recall, y = .data$precision)) +
    ggplot2::geom_hline(yintercept = d$prevalencia[[1]], linetype = 2, colour = "#94a3b8") +
    ggplot2::geom_step(direction = "vh", linewidth = 1, colour = .TR_MODELS_COR) +
    ggplot2::geom_point(size = 1.6, colour = .TR_MODELS_COR) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::annotate("text", x = .3, y = .08,
                      label = sprintf("AP = %.3f  (acaso = %.3f)", d$ap[[1]], d$prevalencia[[1]])) +
    ggplot2::labs(x = "Revocação", y = "Precisão",
                  subtitle = sprintf("AP %s · área %s · %s · positivo: %s", .tr_models_virgula(d$ap[[1]]),
                                     .tr_models_virgula(d$area[[1]]), par$origem, pos))
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Os pontos da curva PR: um por limiar distinto (decrescente), prevendo
#' positivo quando prob >= limiar. Porte literal da `ml` (main).
#' @noRd
.tr_models_pr_pontos <- function(positivo, prob) {
  ord <- order(prob, decreasing = TRUE)
  positivo <- positivo[ord]; prob <- prob[ord]
  grupos <- cumsum(c(TRUE, diff(prob) != 0))
  tp <- cumsum(as.numeric(rowsum(as.integer(positivo), grupos)))
  fp <- cumsum(as.numeric(rowsum(as.integer(!positivo), grupos)))
  recall <- tp / sum(positivo); precision <- tp / (tp + fp)
  ap <- sum(diff(c(0, recall)) * precision)
  # Área com a interpolação de Davis & Goadrich (2006), contínua (Keilwagen,
  # Grosse & Grau 2014): entre dois cortes, cada positivo a mais traz
  # s = dFP/dTP falsos positivos, e a precisão (a + x)/(c + k x), com
  # a = TP, c = TP + FP e k = 1 + s, é integrada em forma fechada.
  a <- c(0, tp[-length(tp)]); b <- c(0, fp[-length(fp)])
  dtp <- tp - a; dfp <- fp - b
  area <- 0
  for (i in which(dtp > 0)) {
    k <- 1 + dfp[[i]] / dtp[[i]]; c0 <- a[[i]] + b[[i]]
    area <- area + if (c0 == 0) dtp[[i]] / k else
      dtp[[i]] / k + (a[[i]] - c0 / k) / k * log((c0 + k * dtp[[i]]) / c0)
  }
  tibble::tibble(limiar = prob[!duplicated(grupos)], recall = recall, precision = precision,
                 ap = ap, area = area / sum(positivo), prevalencia = mean(positivo))
}

#' O par (real, probabilidades) das curvas (`models/roc`, `models/pr_curve`)
#' nos três modos. No modo tabela, a classe positiva sai do param, do nome da
#' coluna `prob_<classe>` ou do segundo nível; no de modelo fica como veio
#' (vazia = decide quem chama).
#' @return o par de `.tr_models_par_modelo`, com `positiva`.
#' @noRd
.tr_models_par_prob <- function(modelo, dados, validacao, resposta, probabilidade, positiva, no) {
  if (.tr_models_modo(modelo, dados, no) == "tabela") {
    r <- .tr_models_coluna_tabela(dados, resposta, "resposta", no)
    niv <- .tr_models_niveis_tabela(dados[[r]])
    if (length(niv) < 2L) {
      .tr_models_abort("tr_models_error_one_level",
                       "'%s': a coluna '%s' tem uma classe só (%s); a curva pede duas.", no, r, paste(niv, collapse = ""))
    }
    pos <- if (.tr_models_preenchido(positiva)) .tr_models_enum(trimws(positiva), niv, "positiva") else ""
    if (.tr_models_preenchido(probabilidade) || length(niv) == 2L || nzchar(pos)) {
      # Uma coluna só: a da positiva. Vazia, o nome que o `models/predict` dá.
      # Coluna informada e positiva vazia: a classe vem do nome `prob_<classe>`
      # (a correção da main na `ml/roc`). Um nome que não indica classe é
      # recusado em vez de adivinhado — a classe errada espelha a curva.
      if (!nzchar(pos) && .tr_models_preenchido(probabilidade)) {
        pos <- .tr_models_roc_positiva(niv, trimws(probabilidade), no)
      }
      if (!nzchar(pos)) {
        if (length(niv) > 2L) {
          .tr_models_abort("tr_models_error_blank_param",
                           "'%s': com %d classes e uma coluna de probabilidade, diga em 'positiva' de qual classe ela é.",
                           no, length(niv))
        }
        pos <- niv[[2]]
      }
      col <- if (.tr_models_preenchido(probabilidade)) probabilidade else .tr_models_colunas_prob(niv)[match(pos, niv)]
      col <- .tr_models_coluna_tabela(dados, col, "probabilidade", no)
      # A outra coluna é o complemento: a curva de `pos` só lê a dela.
      prob <- matrix(0, nrow(dados), length(niv), dimnames = list(NULL, niv))
      prob[, pos] <- as.numeric(dados[[col]])
    } else {
      cols <- .tr_models_colunas_prob(niv)
      faltam <- setdiff(cols, names(dados))
      if (length(faltam)) {
        .tr_models_abort("tr_models_error_unknown_column",
                         "'%s': a curva de cada classe lê as colunas %s, e faltam %s. Diga a 'positiva' e a 'probabilidade' para uma curva só.",
                         no, paste(cols, collapse = ", "), paste(faltam, collapse = ", "))
      }
      prob <- as.matrix(as.data.frame(dados)[, cols, drop = FALSE])
      colnames(prob) <- niv
    }
    par <- list(real = as.character(dados[[r]]), previsto = rep("", nrow(dados)), prob = prob,
                niveis = niv, corte = 0.5, origem = "tabela", resposta = r)
    par$positiva <- pos
  } else {
    par <- .tr_models_exigir_classif(.tr_models_par_modelo(modelo, dados, validacao, no), no)
    par$positiva <- if (.tr_models_preenchido(positiva)) trimws(positiva) else ""
    if (is.null(par$prob)) {
      .tr_models_abort("tr_models_error_not_applicable",
                       "'%s': o modelo de classe '%s' prevê a classe sem probabilidade, e a curva precisa dela.",
                       no, class(modelo)[[1]])
    }
  }
  .tr_models_sem_na(par)
}

#' A classe que a coluna `prob_<classe>` nomeia (também `.prob_<classe>`, o
#' nome da antiga `ml/predict`), comparando com o nome saneado de cada nível.
#' @noRd
.tr_models_roc_positiva <- function(niveis, coluna, no) {
  cand <- sub("^\\.?prob_", "", coluna)
  if (grepl("^\\.?prob_", coluna)) {
    if (cand %in% niveis) return(cand)
    i <- match(cand, tr_models_clean_name(niveis))
    if (!is.na(i)) return(niveis[[i]])
  }
  .tr_models_abort("tr_models_error_positive_required",
                   "'%s': a coluna '%s' não indica a classe (esperado 'prob_<classe>'); informe 'positiva' com a classe cuja probabilidade ela contém.",
                   no, coluna)
}

.tr_models_roc_grafico <- function(rd, par, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda) {
  diag_df <- data.frame(x = c(0, 1), y = c(0, 1))
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = diag_df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
                       colour = "#8b949e", linetype = "dashed")
  cur <- rd$curvas
  # geom_path, e não geom_step: um empate entre positivo e negativo anda na
  # DIAGONAL até o próximo ponto — é essa a curva cuja área é a AUC.
  if (length(unique(cur$classe)) == 1L) {
    auc <- cur$auc[[1]]
    p <- p + ggplot2::geom_path(data = cur, ggplot2::aes(x = .data[["fpr"]], y = .data[["tpr"]]),
                                colour = .TR_MODELS_COR, linewidth = .9)
    sub <- sprintf("AUC %s · %s · positivo: %s", .tr_models_virgula(auc), par$origem, rd$positiva)
    if (!is.null(rd$ponto)) {
      p <- p + ggplot2::geom_point(data = rd$ponto, ggplot2::aes(x = .data[["fpr"]], y = .data[["tpr"]]),
                                   colour = .TR_MODELS_COR_2, size = 3)
      sub <- paste(sub, "· ponto: corte", .tr_models_virgula(rd$corte, 2L))
    }
    p <- p + ggplot2::labs(subtitle = sub)
  } else {
    # O rótulo leva a AUC, e rótulo de texto sairia em ordem ALFABÉTICA na
    # legenda; o fator nos níveis da classe a mantém.
    aucs <- tapply(cur$auc, cur$classe, `[`, 1)[par$niveis]
    rotulos <- sprintf("%s (AUC %s)", par$niveis, .tr_models_virgula(aucs))
    cur$grupo <- factor(rotulos[match(cur$classe, par$niveis)], levels = rotulos)
    p <- p + ggplot2::geom_path(data = cur, ggplot2::aes(x = .data[["fpr"]], y = .data[["tpr"]],
                                                        colour = .data[["grupo"]], group = .data[["grupo"]]),
                                linewidth = .8) +
      ggplot2::labs(colour = par$resposta, subtitle = sprintf("Cada classe contra as outras · %s", par$origem))
  }
  p <- p + ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(x = "1 − especificidade (falsos positivos)", y = "sensibilidade (verdadeiros positivos)")
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

# ---- Métricas -----------------------------------------------------------------

#' As métricas de classificação, com os nomes e as contas da `ml/evaluate`.
#'
#' Porte da `.tr_ml_classification_metrics` da main (a `ml/evaluate` com kappa,
#' precisão/revocação/F1 macro, ponderados e por classe), para que um fluxo
#' que migrou de `ml/evaluate` veja os mesmos números. As médias macro são
#' sobre as classes OBSERVADAS; as ponderadas usam o suporte de cada classe;
#' precisão de classe nunca prevista e F1 sem acerto valem 0 (a convenção
#' `zero_division = 0` do scikit-learn). Somam-se, na binária,
#' `sensitivity`/`specificity` da positiva (o que a `multi` mostrava).
#' @return tibble `metrica`, `classe`, `valor`, `n`: `classe` é NA nas globais
#'   e nomeia a classe nas por classe (`precision`, `recall`, `f1`), com o
#'   suporte dela em `n`.
#' @noRd
.tr_models_metricas_classif <- function(y, p, positiva = "") {
  ys <- as.character(y); ps <- as.character(p)
  classes <- unique(ys)
  n <- length(ys)
  acc <- mean(ys == ps)
  por <- vapply(classes, function(k) {
    tp <- sum(ys == k & ps == k); fp <- sum(ys != k & ps == k); fn <- sum(ys == k & ps != k)
    c(precision = if (tp + fp == 0) 0 else tp / (tp + fp),
      recall = tp / (tp + fn),
      f1 = if (tp == 0) 0 else 2 * tp / (2 * tp + fp + fn),
      suporte = tp + fn)
  }, numeric(4))
  w <- por["suporte", ] / n
  # Kappa de Cohen (1960): concordância observada contra a esperada pelas
  # marginais da tabela real × previsto (rótulos dos dois lados).
  todas <- union(classes, unique(ps))
  pe <- sum(vapply(todas, function(k) mean(ys == k) * mean(ps == k), numeric(1)))
  kappa <- if (pe == 1) NA_real_ else (acc - pe) / (1 - pe)
  metrica <- c("accuracy", "balanced_accuracy", "macro_f1", "kappa",
               "macro_precision", "macro_recall", "weighted_precision",
               "weighted_recall", "weighted_f1")
  valor <- c(acc, mean(por["recall", ]), mean(por["f1", ]), kappa,
             mean(por["precision", ]), mean(por["recall", ]),
             sum(w * por["precision", ]), sum(w * por["recall", ]), sum(w * por["f1", ]))
  if (nzchar(positiva)) {
    metrica <- c(metrica, "sensitivity", "specificity")
    valor <- c(valor, sum(ys == positiva & ps == positiva) / sum(ys == positiva),
               sum(ys != positiva & ps != positiva) / sum(ys != positiva))
  }
  globais <- tibble::tibble(metrica = metrica, classe = NA_character_, valor = valor, n = n)
  k <- length(classes)
  por_classe <- tibble::tibble(
    metrica = rep(c("precision", "recall", "f1"), times = k),
    classe = rep(classes, each = 3L),
    valor = as.numeric(por[c("precision", "recall", "f1"), ]),
    n = rep(as.integer(por["suporte", ]), each = 3L))
  rbind(globais, por_classe)
}

#' As de regressão, idem: `mae`, `rmse`, `r2` (de PREVISÃO: 1 − SQE/SQT sobre
#' as linhas avaliadas, que pode ser negativo quando o modelo prevê pior que a
#' média — é o sinal a ler, e não um erro).
#' @noRd
.tr_models_metricas_regressao <- function(y, p) {
  erro <- p - y
  sst <- sum((y - mean(y))^2)
  tibble::tibble(metrica = c("mae", "rmse", "r2"),
                 valor = c(mean(abs(erro)), sqrt(mean(erro^2)),
                           if (sst == 0) NA_real_ else 1 - sum(erro^2) / sst),
                 n = length(y))
}

#' Métricas de previsão: o erro (regressão) ou o acerto (classificação).
#'
#' @inheritParams tr_models_confusion
#' @param positiva classificação binária: a classe das `sensitivity` e
#'   `specificity`; vazio = o segundo nível.
#' @return tibble `metrica`, `valor`, `n` (as linhas que contaram); na
#'   classificação também `classe` (NA nas globais; a classe nas `precision`,
#'   `recall` e `f1` por classe, com o suporte em `n`).
#' @export
tr_models_evaluate <- function(modelo = NULL, dados = NULL, validacao = "cruzada", resposta = "",
                               predito = "previsto", positiva = "") {
  no <- "models/evaluate"
  if (.tr_models_modo(modelo, dados, no) == "tabela") {
    r <- .tr_models_coluna_tabela(dados, resposta, "resposta", no)
    pc <- .tr_models_coluna_tabela(dados, predito, "predito", no)
    y <- dados[[r]]; p <- dados[[pc]]
    # A mesma regra do `tarefa = "auto"` da `ml/evaluate`: qualquer lado
    # categórico faz classificação; dois números, regressão.
    cat <- function(x) is.factor(x) || is.character(x) || is.logical(x)
    tarefa <- if (cat(y) || cat(p)) "classificacao" else "regressao"
    par <- list(real = if (tarefa == "regressao") as.numeric(y) else as.character(y),
                previsto = if (tarefa == "regressao") as.numeric(p) else as.character(p),
                tarefa = tarefa, niveis = if (tarefa == "classificacao") .tr_models_niveis_tabela(y, p))
  } else {
    par <- .tr_models_par_modelo(modelo, dados, validacao, no)
    par$prob <- NULL
  }
  par <- .tr_models_sem_na(par)
  if (!length(par$real)) {
    .tr_models_abort("tr_models_error_too_few_rows", "'%s': nenhuma linha com real e previsto para avaliar.", no)
  }
  if (par$tarefa == "regressao") return(.tr_models_metricas_regressao(par$real, par$previsto))
  pos <- if (.tr_models_preenchido(positiva)) {
    .tr_models_enum(trimws(positiva), par$niveis, "positiva")
  } else if (length(par$niveis) == 2L) par$niveis[[2]] else ""
  .tr_models_metricas_classif(par$real, par$previsto, pos)
}

# ---- Importância ----------------------------------------------------------------

#' Importância das preditoras, pelo contrato.
#'
#' Cada classe diz a sua medida (coluna `medida`): |t| nos modelos daqui,
#' redução de impureza ou ganho nas árvores da `ml`. Não se compara entre
#' medidas.
#' @param modelo um `models/fit`.
#' @return tibble `termo`, `importancia`, `medida`, do maior para o menor.
#' @export
tr_models_importance_table <- function(modelo) {
  .tr_models_modelo_conferir(modelo)
  tr_models_importance(modelo)
}
