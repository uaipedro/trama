# Avaliar o plano antes (poder) e o dado depois (teste de aleatorização).
#
# Os dois nós repetem uma análise de `trama.models` muitas vezes: o poder,
# sobre respostas simuladas pela cadeia `effect -> error` declarada no plano; o
# teste de aleatorização, sobre a resposta observada com a alocação re-sorteada
# pela receita do `experiments/design`. Nenhum dos dois tem fórmula fechada por
# delineamento: a análise é o nó de models de verdade, chamado pelo seu `fn`, e
# a estatística é a linha do quadro de ANOVA (ou do `experiments/contrasts`)
# que o usuário leria no card. Custo: um ajuste por réplica (ms cada na ANOVA).

#' O `fn` de um nó de `trama.models`, pelo id.
#' @noRd
.tr_exp_av_fn <- function(id, no) {
  nos <- trama.models::trama_collection()$nodes
  n <- Filter(function(z) identical(z$id, id), nos)
  if (!length(n)) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                          "'%s': a análise '%s' não é nó da trama.models (ex.: models/anova_dbc).", no, id)
  }
  n[[1]]$fn
}

#' `"fatores = a, b; bloco = bloco"` -> list(fatores = "a, b", bloco = "bloco").
#' @noRd
.tr_exp_av_pares <- function(texto, no) {
  if (!.tr_exp_preenchido(texto)) return(list())
  itens <- .tr_exp_split(gsub("\n", ";", texto, fixed = TRUE), ";")
  out <- list()
  for (it in itens) {
    i <- regexpr("=", it, fixed = TRUE)
    if (i < 2L) {
      .tr_experiments_abort("tr_experiments_error_bad_option",
                            "'%s': 'parametros' é 'nome = valor; nome = valor' ('%s' não segue).", no, it)
    }
    out[[trimws(substr(it, 1L, i - 1L))]] <- trimws(substr(it, i + 1L, nchar(it)))
  }
  out
}

#' A análise que se repete: id, fn e params (com a resposta).
#'
#' Sem `analise`, é a do plano (`plano$analise`, que o `experiments/error` já
#' completou com a resposta). Com outro nó, os params partem de zero e só a
#' `resposta` é posta, quando o nó a tem; `parametros` sobrescreve os dois.
#' @noRd
.tr_exp_av_analise <- function(plano, analise, parametros, resposta, no) {
  id <- if (.tr_exp_preenchido(analise)) trimws(analise) else plano$analise$no
  if (is.null(id)) .tr_experiments_abort("tr_experiments_error_bad_option", "'%s': o plano não sugere análise; diga uma em 'analise'.", no)
  fn <- .tr_exp_av_fn(id, no)
  fm <- formals(fn)
  params <- if (identical(id, plano$analise$no)) plano$analise$params else list()
  if ("resposta" %in% names(fm) && is.null(params$resposta) && is.null(params$formula)) params$resposta <- resposta
  extra <- .tr_exp_av_pares(parametros, no)
  for (nm in names(extra)) {
    if (!nm %in% names(fm)) {
      .tr_experiments_abort("tr_experiments_error_bad_option", "'%s': '%s' não tem o param '%s'. Tem: %s.",
                            no, id, nm, paste(setdiff(names(fm), c("dados", "modelo")), collapse = ", "))
    }
    d <- tryCatch(eval(fm[[nm]]), error = function(e) NULL)
    v <- extra[[nm]]
    if (is.numeric(d)) v <- as.numeric(v) else if (is.logical(d)) v <- as.logical(toupper(v))
    params[[nm]] <- v
  }
  list(id = id, fn = fn, params = params)
}

.TR_EXP_AV_CONJUNTOS <- c("nenhum", "polinomiais", "helmert", "controle", "digitados")

#' Ajusta e devolve c(estatistica, p) do termo (ou do contraste).
#' @noRd
.tr_exp_av_estat <- function(dados, an, alvo) {
  fit <- suppressMessages(suppressWarnings(do.call(an$fn, c(list(dados), an$params))))
  if (!inherits(fit, "tr_models_fit") && is.list(fit)) fit <- Find(function(z) inherits(z, "tr_models_fit"), fit)
  if (alvo$conjunto == "nenhum") {
    a <- trama.models::tr_models_anova_table(fit)
    t <- a$tabela
    i <- match(alvo$termo, t$termo)
    if (is.na(i)) {
      .tr_experiments_abort("tr_experiments_error_bad_option", "'%s': o quadro de '%s' não tem o termo '%s'. Tem: %s.",
                            alvo$no, an$id, alvo$termo, paste(t$termo[!is.na(t$p_valor)], collapse = ", "))
    }
    return(list(estat = t[[a$coluna_estat]][[i]], p = t$p_valor[[i]], nome = a$coluna_estat, coef = NULL))
  }
  ct <- tr_experiments_contrasts(fit, alvo$termo, alvo$conjunto, alvo$contrastes, alvo$controle, alvo$doses)$out$tabela
  i <- if (.tr_exp_preenchido(alvo$contraste)) match(.tr_exp_ef_chave(alvo$contraste), .tr_exp_ef_chave(ct$termo))
       else if (nrow(ct) == 1L) 1L else NA_integer_
  if (is.na(i)) {
    .tr_experiments_abort("tr_experiments_error_bad_option", "'%s': diga em 'contraste' qual linha testar: %s.",
                          alvo$no, paste(ct$termo, collapse = ", "))
  }
  list(estat = ct$F[[i]], p = ct$p_valor[[i]], nome = "F", coef = as.numeric(strsplit(ct$coeficientes[[i]], " ")[[1]]))
}

#' O alvo do teste (termo ou contraste), conferido.
#' @noRd
.tr_exp_av_alvo <- function(plano, termo, conjunto, contraste, contrastes, controle, doses, no) {
  conjunto <- .tr_exp_enum(conjunto, .TR_EXP_AV_CONJUNTOS, "conjunto")
  if (!.tr_exp_preenchido(termo)) {
    tr <- plano$fatores$nome[plano$fatores$papel == "tratamento"]
    if (!length(tr)) .tr_experiments_abort("tr_experiments_error_blank_param", "'%s': diga o 'termo' testado.", no)
    termo <- tr[[1]]
  }
  list(no = no, termo = trimws(termo), conjunto = conjunto, contraste = contraste, contrastes = contrastes,
       controle = controle, doses = doses)
}

#' A hipótese nula é verdadeira no modelo declarado?
#'
#' Lê a parte FIXA da resposta simulada (os termos intercepto, fixo, interação
#' e quantitativo) e pergunta se ela tem o efeito testado: médias por nível
#' iguais (efeito principal), médias de célula aditivas (interação `a:b`) ou
#' Σ cᵢ mᵢ = 0 (contraste). NA quando o termo não é fator do plano.
#' @noRd
.tr_exp_av_h0 <- function(sim, alvo, coef) {
  u <- sim$unidades
  fixos <- vapply(sim$termos, function(t) t$tipo %in% c("intercepto", "fixo", "interacao", "quantitativo"), TRUE)
  cols <- vapply(sim$termos[fixos], function(t) t$coluna, "")
  eta <- if (length(cols)) rowSums(as.matrix(u[, cols, drop = FALSE])) else rep(0, nrow(u))
  fts <- strsplit(alvo$termo, ":", fixed = TRUE)[[1]]
  if (!all(fts %in% names(u)) || any(vapply(fts, function(f) is.numeric(u[[f]]), TRUE))) return(NA)
  tol <- 1e-9 * max(1, abs(eta))
  if (length(fts) == 1L) {
    m <- tapply(eta, u[[fts]], mean)
    m <- m[!is.na(m)]
    if (!is.null(coef)) return(abs(sum(coef * m)) < tol * length(m))
    return(diff(range(m)) < tol)
  }
  cel <- stats::aggregate(list(m = eta), lapply(u[fts], as.character), mean)
  aditivo <- stats::lm(stats::reformulate(fts, "m"), data = cel)
  max(abs(stats::residuals(aditivo))) < tol
}

#' IC de Clopper-Pearson (o do `binom.test`).
#' @noRd
.tr_exp_av_cp <- function(x, n, confianca) {
  if (n == 0L) return(c(NA_real_, NA_real_))
  as.numeric(stats::binom.test(x, n, conf.level = confianca)$conf.int)
}

.tr_exp_av_num <- function(x, param, min, max, no) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || !is.finite(x) || x <= min || x >= max) {
    .tr_experiments_abort("tr_experiments_error_bad_option", "'%s': '%s' tem de estar entre %g e %g (exclusive).",
                          no, param, min, max)
  }
  x
}

#' Poder (ou erro tipo I) por simulação da cadeia declarada no plano.
#'
#' @param plano plano com termos e resposta (`experiments/effect` ...
#'   `experiments/error`).
#' @param analise id do nó de models que analisa cada réplica; em branco, a
#'   análise do plano.
#' @param parametros `"nome = valor; ..."`: params da análise (sobrescrevem os
#'   do plano).
#' @param termo o termo do quadro de ANOVA testado (`irrigacao`, `a:b`); em
#'   branco, o primeiro fator de tratamento. Com `conjunto`, o fator contrastado.
#' @param conjunto `"nenhum"` (F do termo) ou um conjunto do
#'   `experiments/contrasts`.
#' @param contraste a linha do conjunto testada (`linear`).
#' @param contrastes,controle,doses como no `experiments/contrasts`.
#' @param replicas réplicas de Monte Carlo por ponto da grade.
#' @param significancia α do teste: rejeita quando p < α.
#' @param confianca nível do IC de Clopper-Pearson da taxa.
#' @param repeticoes grade `"3, 4, 6"` de repetições (o param `repeticoes` do
#'   design); em branco, só o tamanho do plano.
#' @param aspecto,tema,titulo,rotulo_x,rotulo_y,legenda cosméticos do gráfico.
#' @param .seed semente (vem do card).
#' @return lista com `out` (gráfico) e `tabela`.
#' @export
tr_experiments_power <- function(plano, analise = "", parametros = "", termo = "", conjunto = "nenhum",
                                 contraste = "", contrastes = "", controle = "", doses = "", replicas = 200L,
                                 significancia = 0.05, confianca = 0.95, repeticoes = "",
                                 aspecto = "4:3", tema = "padrão", titulo = "", rotulo_x = "", rotulo_y = "",
                                 legenda = "direita", .seed = 1L) {
  no <- "experiments/power"
  .tr_exp_plano_conferir(plano)
  if (is.null(plano$resposta)) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                          "'%s': o plano não tem resposta simulada; ligue-o depois de 'experiments/error'.", no)
  }
  if (!all(vapply(plano$termos, function(t) !is.null(t$argumentos), TRUE))) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                          "'%s': o plano é de uma versão que não guardava os argumentos dos termos; rode a cadeia de novo.", no)
  }
  replicas <- .tr_exp_int(replicas, "replicas", 10L, 100000L)
  significancia <- .tr_exp_av_num(significancia, "significancia", 0, 0.5, no)
  confianca <- .tr_exp_av_num(confianca, "confianca", 0.5, 1, no)
  alvo <- .tr_exp_av_alvo(plano, termo, conjunto, contraste, contrastes, controle, doses, no)
  an <- .tr_exp_av_analise(plano, analise, parametros, plano$resposta$nome, no)
  grade <- .tr_exp_split(repeticoes)
  grade <- if (length(grade)) vapply(grade, function(g) .tr_exp_int(suppressWarnings(as.numeric(g)), "repeticoes", 2L, 10000L), 1L)
           else NA_integer_
  targs <- lapply(plano$termos, function(t) t$argumentos)
  eargs <- c(list(resposta = plano$resposta$nome, distribuicao = plano$resposta$distribuicao), plano$resposta$parametros)
  # Sementes derivadas: as MESMAS em todo ponto da grade (números aleatórios
  # comuns), o que deixa a curva lisa sem mudar o que cada ponto estima.
  sementes <- .tr_exp_com_semente(.seed, sample.int(.Machine$integer.max - 1L, replicas))

  linhas <- lapply(grade, function(r) {
    rec <- plano$receita
    if (!is.na(r)) rec$repeticoes <- r
    d <- do.call(tr_experiments_design, c(rec, list(.seed = plano$semente)))
    h0 <- NA
    p <- vapply(seq_len(replicas), function(i) {
      sim <- tr_experiments_simulate(d, targs, eargs, .seed = sementes[[i]])
      # A primeira réplica roda sem rede: erro de configuração (termo que não
      # existe, contraste sem nome) aparece como erro, e não como poder NA.
      e <- if (i == 1L) .tr_exp_av_estat(sim$unidades, an, alvo)
           else tryCatch(.tr_exp_av_estat(sim$unidades, an, alvo), error = function(err) NULL)
      if (i == 1L) h0 <<- .tr_exp_av_h0(sim, alvo, e$coef)
      if (is.null(e) || !is.finite(e$p)) NA_real_ else e$p
    }, 1)
    n <- sum(!is.na(p)); x <- sum(p < significancia, na.rm = TRUE)
    ic <- .tr_exp_av_cp(x, n, confianca)
    tibble::tibble(repeticoes = if (is.na(r)) as.integer(plano$receita$repeticoes) else r, unidades = nrow(d$unidades),
                   replicas = n, falhas = replicas - n, rejeicoes = x, taxa = if (n) x / n else NA_real_,
                   li = ic[[1]], ls = ic[[2]], confianca = confianca, significancia = significancia,
                   hipotese = if (is.na(h0)) "não conferida" else if (h0) "H0 verdadeira: taxa = erro tipo I" else "H0 falsa: taxa = poder",
                   analise = an$id,
                   teste = if (alvo$conjunto == "nenhum") alvo$termo else sprintf("%s (%s) de %s", alvo$contraste, alvo$conjunto, alvo$termo))
  })
  tab <- do.call(rbind, linhas)

  sub <- sprintf("%s · %s · %d réplicas · α = %g · IC %g%% (Clopper-Pearson)", tab$analise[[1]], tab$teste[[1]],
                 replicas, significancia, 100 * confianca)
  x_col <- if (length(grade) > 1L) "repeticoes" else "unidades"
  g <- ggplot2::ggplot(tab, ggplot2::aes(x = .data[[x_col]], y = .data[["taxa"]])) +
    ggplot2::geom_hline(yintercept = significancia, linetype = "dashed", colour = "grey45") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[["li"]], ymax = .data[["ls"]]), width = 0) +
    ggplot2::geom_point(size = 2.5, colour = .TR_EXP_COR) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = if (x_col == "repeticoes") "repetições" else "unidades", y = "taxa de rejeição",
                  subtitle = paste0(sub, "\n", tab$hipotese[[1]]))
  if (length(grade) > 1L) g <- g + ggplot2::geom_line(colour = .TR_EXP_COR)
  grafico <- trama.view::tr_view_finish(g, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
  list(out = grafico, tabela = tab)
}

# ---------------------------------------------------------------------------
# Teste de aleatorização
# ---------------------------------------------------------------------------

.TR_EXP_AV_METODOS <- c("automático", "monte carlo", "exato")
.TR_EXP_AV_MAX_EXATO <- 100000

#' As permutações DISTINTAS de um multiconjunto (linhas de uma matriz).
#' @noRd
.tr_exp_av_perm_multi <- function(x) {
  if (length(x) <= 1L) return(matrix(x, nrow = 1L))
  u <- unique(x)
  do.call(rbind, lapply(u, function(v) cbind(v, .tr_exp_av_perm_multi(x[-match(v, x)]), deparse.level = 0)))
}

#' Onde se sabe enumerar: DIC e DBC (e o fatorial neles), em que o conjunto de
#' alocações admissíveis é o produto, por estrato (o experimento todo, ou cada
#' bloco), das permutações distintas dos rótulos de tratamento. Devolve NULL nas
#' outras estruturas.
#' @noRd
.tr_exp_av_estratos <- function(plano) {
  base <- plano$receita$delineamento_base
  ok <- plano$estrutura %in% c("dic", "dbc") || (plano$estrutura == "fatorial" && base %in% c("dic", "dbc"))
  if (!ok) return(NULL)
  u <- plano$unidades
  trat <- plano$fatores$nome[plano$fatores$papel == "tratamento"]
  rot <- do.call(paste, c(lapply(trat, function(f) as.character(u[[f]])), sep = "\r"))
  est <- if ("bloco" %in% names(u)) as.character(u$bloco) else rep("1", nrow(u))
  idx <- split(seq_len(nrow(u)), factor(est, levels = unique(est)))
  n_log <- sum(vapply(idx, function(i) lgamma(length(i) + 1) - sum(lgamma(table(rot[i]) + 1)), 1))
  list(trat = trat, rot = rot, idx = idx, n = exp(n_log))
}

#' Todas as alocações admissíveis, como matriz (alocação × unidade) de rótulos.
#' @noRd
.tr_exp_av_enumerar <- function(e) {
  por <- lapply(e$idx, function(i) .tr_exp_av_perm_multi(e$rot[i]))
  g <- expand.grid(lapply(por, function(m) seq_len(nrow(m))), KEEP.OUT.ATTRS = FALSE)
  out <- matrix("", nrow(g), length(e$rot))
  for (j in seq_along(por)) out[, e$idx[[j]]] <- por[[j]][g[[j]], , drop = FALSE]
  out
}

#' As unidades observadas com a alocação de um re-sorteio pela receita.
#'
#' A resposta, os termos simulados e a covariável observada ficam com a
#' unidade; só o que o sorteio escreve (os fatores e, no composto central, a
#' ordem padrão dos pontos) vem do plano re-sorteado. As colunas estruturais
#' (bloco, parcela...) saem iguais em todo sorteio da mesma receita.
#' @noRd
.tr_exp_av_realocar <- function(plano, u, resposta, semente) {
  nov <- tr_experiments_randomize(plano, semente)$unidades
  fixas <- c("unidade", "ordem", resposta, grep("^\\.ef_", names(u), value = TRUE))
  cols <- setdiff(intersect(names(nov), names(u)), fixas)
  cols <- cols[!vapply(cols, function(cc) is.numeric(nov[[cc]]) && all(is.na(nov[[cc]])), TRUE)]
  u[cols] <- nov[cols]
  u
}

#' Teste de aleatorização (Fisher) pela receita do delineamento.
#'
#' @param plano o plano sorteado; com a resposta nas unidades, ou com `dados`.
#' @param dados tabela opcional com `unidade` e a resposta observada.
#' @param resposta coluna da resposta; em branco, a do plano.
#' @param analise,parametros,termo,conjunto,contraste,contrastes,controle,doses
#'   como em [tr_experiments_power()].
#' @param replicas re-sorteios de Monte Carlo.
#' @param metodo `"automático"` (exato quando há até `replicas` alocações
#'   admissíveis e a estrutura é enumerável), `"monte carlo"` ou `"exato"`.
#' @param aspecto,tema,titulo,rotulo_x,rotulo_y,legenda cosméticos do gráfico.
#' @param .seed semente (vem do card).
#' @return lista com `out` (histograma), `tabela` e `distribuicao`.
#' @export
tr_experiments_randomization_test <- function(plano, dados = NULL, resposta = "", analise = "", parametros = "",
                                              termo = "", conjunto = "nenhum", contraste = "", contrastes = "",
                                              controle = "", doses = "", replicas = 999L, metodo = "automático",
                                              aspecto = "4:3", tema = "padrão", titulo = "", rotulo_x = "",
                                              rotulo_y = "", legenda = "direita", .seed = 1L) {
  no <- "experiments/randomization_test"
  .tr_exp_plano_conferir(plano)
  metodo <- .tr_exp_enum(metodo, .TR_EXP_AV_METODOS, "metodo")
  replicas <- .tr_exp_int(replicas, "replicas", 19L, 100000L)
  if (!.tr_exp_preenchido(resposta)) {
    resposta <- if (!is.null(plano$resposta)) plano$resposta$nome else if (!is.null(plano$analise$params$resposta)) plano$analise$params$resposta else ""
  }
  if (!.tr_exp_preenchido(resposta)) .tr_experiments_abort("tr_experiments_error_blank_param", "'%s': diga a 'resposta'.", no)
  u <- plano$unidades
  trat <- plano$fatores$nome[plano$fatores$papel == "tratamento"]
  if (!is.null(dados)) {
    dados <- as.data.frame(dados)
    if (!all(c("unidade", resposta) %in% names(dados)) || anyDuplicated(dados$unidade) || !all(u$unidade %in% dados$unidade)) {
      .tr_experiments_abort("tr_experiments_error_bad_option",
                            "'%s': 'dados' precisa das colunas 'unidade' (uma linha por unidade do plano) e '%s'.", no, resposta)
    }
    dados <- dados[match(u$unidade, dados$unidade), , drop = FALSE]
    for (f in intersect(trat, names(dados))) {
      if (!identical(as.character(dados[[f]]), as.character(u[[f]]))) {
        .tr_experiments_abort("tr_experiments_error_bad_option",
                              "'%s': em 'dados', '%s' não é a alocação do plano; o teste re-sorteia a partir da alocação que foi a campo.", no, f)
      }
    }
    extra <- setdiff(names(dados), names(u))
    u[extra] <- dados[extra]
    if (!resposta %in% extra) u[[resposta]] <- dados[[resposta]]
  }
  if (!resposta %in% names(u) || !is.numeric(u[[resposta]])) {
    .tr_experiments_abort("tr_experiments_error_bad_option", "'%s': o plano não tem a resposta numérica '%s'; ligue 'dados'.", no, resposta)
  }
  alvo <- .tr_exp_av_alvo(plano, termo, conjunto, contraste, contrastes, controle, doses, no)
  an <- .tr_exp_av_analise(plano, analise, parametros, resposta, no)
  obs <- .tr_exp_av_estat(u, an, alvo)

  est <- .tr_exp_av_estratos(plano)
  exato <- switch(metodo, `monte carlo` = FALSE,
                  `automático` = !is.null(est) && est$n <= replicas,
                  exato = {
                    if (is.null(est)) .tr_experiments_abort("tr_experiments_error_bad_option",
                      "'%s': a enumeração exata é do DIC e do DBC (e do fatorial neles); '%s' vai por Monte Carlo.", no, plano$estrutura)
                    if (est$n > .TR_EXP_AV_MAX_EXATO) .tr_experiments_abort("tr_experiments_error_bad_option",
                      "'%s': %.0f alocações admissíveis passam do limite de %d para enumerar; use Monte Carlo.", no, est$n, .TR_EXP_AV_MAX_EXATO)
                    TRUE
                  })
  estat_de <- function(d) {
    # Um ajuste que falha numa alocação (célula vazia, posto incompleto) conta
    # como falha e sai da distribuição; o número vai para a tabela.
    e <- tryCatch(.tr_exp_av_estat(d, an, alvo), error = function(err) NULL)
    if (is.null(e) || !is.finite(e$estat)) NA_real_ else e$estat
  }
  if (exato) {
    al <- .tr_exp_av_enumerar(est)
    niv <- lapply(stats::setNames(est$trat, est$trat), function(f) levels(u[[f]]))
    dist <- vapply(seq_len(nrow(al)), function(k) {
      d <- u
      partes <- do.call(rbind, strsplit(al[k, ], "\r", fixed = TRUE))
      for (j in seq_along(est$trat)) d[[est$trat[[j]]]] <- factor(partes[, j], levels = niv[[j]])
      estat_de(d)
    }, 1)
  } else {
    sementes <- .tr_exp_com_semente(.seed, sample.int(.Machine$integer.max - 1L, replicas))
    dist <- vapply(sementes, function(s) estat_de(.tr_exp_av_realocar(plano, u, resposta, s)), 1)
  }
  ok <- dist[!is.na(dist)]
  tol <- 1e-8 * max(1, abs(obs$estat))
  b <- sum(ok >= obs$estat - tol)
  p <- if (exato) b / length(ok) else (b + 1) / (length(ok) + 1)
  escopo <- paste(sprintf("%s: %s", plano$fatores$nome[plano$fatores$papel == "tratamento"],
                          plano$fatores$escopo[plano$fatores$papel == "tratamento"]), collapse = "; ")
  tab <- tibble::tibble(
    teste = if (alvo$conjunto == "nenhum") alvo$termo else sprintf("%s (%s) de %s", alvo$contraste, alvo$conjunto, alvo$termo),
    estatistica = obs$nome, observado = obs$estat, p_valor = p,
    metodo = if (exato) "exato (enumeração)" else "Monte Carlo, (b + 1)/(R + 1)",
    alocacoes = length(ok), admissiveis = if (is.null(est)) NA_real_ else est$n, maiores_ou_iguais = b,
    falhas = length(dist) - length(ok), p_parametrico = obs$p, analise = an$id, escopo = escopo)
  distrib <- tibble::tibble(estatistica = dist)
  sub <- sprintf("%s = %.4g · p = %.4g (%s, %d alocações) · p do quadro = %.4g", obs$nome, obs$estat, p,
                 if (exato) "exato" else "Monte Carlo", length(ok), obs$p)
  g <- ggplot2::ggplot(data.frame(estatistica = ok), ggplot2::aes(x = .data[["estatistica"]])) +
    ggplot2::geom_histogram(bins = 30, fill = .TR_EXP_CINZA, colour = "white") +
    ggplot2::geom_vline(xintercept = obs$estat, colour = "#d97706", linewidth = 1) +
    ggplot2::labs(x = sprintf("%s de %s sob re-sorteio", obs$nome, tab$teste), y = "alocações", subtitle = sub)
  grafico <- trama.view::tr_view_finish(g, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
  list(out = grafico, tabela = tab, distribuicao = distrib)
}
