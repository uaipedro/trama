# O nó `experiments/design`: declarar e sortear num passo só.
#
# O sorteio mora DENTRO do nó (decisão em aberto na visão): assim o plano que
# sai já é o croqui de campo, e o teste de aleatorização re-sorteia pelo mesmo
# caminho (`tr_experiments_randomize`). Mostrar "plano pretendido × alocação
# sorteada" continua possível: a aba `combinacoes` da vista é o pretendido, o
# `mapa` é o sorteado.
#
# Os params são PLANOS (o núcleo não tem param condicional): cada estrutura lê
# os que lhe dizem respeito, e a ajuda diz quais. É o mesmo arranjo de
# `sampling/stratified`, cuja "Variável do Neyman" só vale no Neyman.

.TR_EXP_ESTRUTURAS <- c("dic", "dbc", "dql", "fatorial", "confundimento", "fracionado", "composto_central",
                        "parcela_subdividida", "faixas", "bib", "medidas_repetidas", "crossover", "grupos")

#' Repetições padrão quando o card deixa 0: o que a estrutura costuma usar.
#' @noRd
.TR_EXP_R_PADRAO <- c(dic = 4L, dbc = 4L, dql = 1L, fatorial = 3L, confundimento = 1L, fracionado = 1L,
                      composto_central = 1L, parcela_subdividida = 4L, faixas = 4L, bib = 1L,
                      medidas_repetidas = 4L, crossover = 1L, grupos = 3L)

#' Quantos fatores cada estrutura aceita, e se precisam de níveis.
#' @noRd
.tr_exp_conferir_fatores <- function(estrutura, f) {
  k <- length(f)
  faixa <- switch(estrutura,
    dic = , dbc = , dql = , bib = , medidas_repetidas = , crossover = c(1L, 1L),
    fatorial = c(2L, Inf), confundimento = c(2L, 10L), fracionado = c(3L, 15L),
    composto_central = c(2L, 6L), parcela_subdividida = , faixas = c(2L, 2L), grupos = c(1L, Inf))
  if (k < faixa[[1]] || k > faixa[[2]]) {
    .tr_experiments_abort("tr_experiments_error_bad_factors",
                          "Estrutura '%s': pede %s fator(es) de tratamento, e vieram %d (%s).", estrutura,
                          if (faixa[[1]] == faixa[[2]]) faixa[[1]] else paste(faixa[[1]], "a", faixa[[2]]),
                          k, paste(names(f), collapse = ", "))
  }
  dois_niveis <- estrutura %in% c("confundimento", "fracionado")
  for (nm in names(f)) {
    niv <- f[[nm]]
    if (estrutura == "composto_central") next
    if (is.null(niv)) {
      if (estrutura == "fracionado") next
      .tr_experiments_abort("tr_experiments_error_bad_factors",
                            "Fator '%s': diga os níveis (ex.: '%s: A, B, C').", nm, nm)
    }
    if (dois_niveis && length(niv) != 2L) {
      .tr_experiments_abort("tr_experiments_error_bad_factors",
                            "Estrutura '%s': cada fator tem 2 níveis (baixo, alto); '%s' tem %d.",
                            estrutura, nm, length(niv))
    }
    if (length(niv) < 2L) {
      .tr_experiments_abort("tr_experiments_error_bad_factors",
                            "Fator '%s': um fator precisa de ao menos 2 níveis.", nm)
    }
  }
  if (estrutura == "fracionado") {
    for (nm in names(f)) if (is.null(f[[nm]])) f[nm] <- list(NULL)
  }
  invisible(f)
}

#' Planeja e sorteia um experimento.
#'
#' Nível 1 do nó `experiments/design`. Ver a ajuda do nó para o sentido de cada
#' param por estrutura, e `R/type.R` para o contrato do objeto devolvido.
#' @param estrutura uma de `dic`, `dbc`, `dql`, `fatorial`, `confundimento`,
#'   `fracionado`, `composto_central`, `parcela_subdividida`, `faixas`, `bib`,
#'   `medidas_repetidas`, `crossover`, `grupos`.
#' @param fatores `"nome: nível, nível; nome: nível, ..."`.
#' @param repeticoes repetições (blocos, indivíduos por grupo, réplicas...);
#'   0 = o padrão da estrutura.
#' @param delineamento_base `dbc` ou `dic` (fatorial, parcela subdividida,
#'   grupos).
#' @param confundir efeito(s) confundido(s) com blocos, em letras (`"ABC"`).
#' @param geradores geradores do fracionado (`"D = ABC; E = ABD"`).
#' @param alfa `rotacional` ou `face` (composto central).
#' @param pontos_centrais pontos centrais do composto central.
#' @param tamanho_bloco k, parcelas por bloco no BIB.
#' @param tempos tempos das medidas repetidas (`"0, 30, 60"`).
#' @param locais número de locais do grupo de experimentos.
#' @param colunas_grade colunas da grade quando ela é automática (0 = quadrada).
#' @param covariaveis nomes de covariáveis observadas (colunas `NA` a medir).
#' @param .seed semente do sorteio (vem do card).
#' @return objeto `tr_experiments_plan`.
#' @export
tr_experiments_design <- function(estrutura = "dbc", fatores = "tratamento: A, B, C, D", repeticoes = 0L,
                                  delineamento_base = "dbc", confundir = "", geradores = "",
                                  alfa = "rotacional", pontos_centrais = 4L, tamanho_bloco = 3L,
                                  tempos = "", locais = 3L, colunas_grade = 0L, covariaveis = "",
                                  .seed = 1L) {
  receita <- list(estrutura = estrutura, fatores = fatores, repeticoes = repeticoes,
                  delineamento_base = delineamento_base, confundir = confundir, geradores = geradores,
                  alfa = alfa, pontos_centrais = pontos_centrais, tamanho_bloco = tamanho_bloco,
                  tempos = tempos, locais = locais, colunas_grade = colunas_grade, covariaveis = covariaveis)
  estrutura <- .tr_exp_enum(estrutura, .TR_EXP_ESTRUTURAS, "estrutura")
  f <- .tr_exp_fatores(fatores)
  .tr_exp_conferir_fatores(estrutura, f)
  rep_in <- .tr_exp_int(repeticoes, "repeticoes", 0L, 1000L)
  s <- list(
    fatores = f, r = if (rep_in > 0L) rep_in else .TR_EXP_R_PADRAO[[estrutura]], r_dado = rep_in > 0L,
    delineamento_base = .tr_exp_enum(delineamento_base, c("dbc", "dic"), "delineamento_base"),
    confundir = confundir, geradores = geradores,
    alfa = .tr_exp_enum(alfa, c("rotacional", "face"), "alfa"),
    pontos_centrais = .tr_exp_int(pontos_centrais, "pontos_centrais", 0L, 50L),
    tamanho_bloco = .tr_exp_int(tamanho_bloco, "tamanho_bloco", 2L, 100L),
    tempos = tempos, locais = .tr_exp_int(locais, "locais", 1L, 100L),
    colunas_grade = .tr_exp_int(colunas_grade, "colunas_grade", 0L, 1000L))
  covs <- .tr_exp_split(covariaveis)
  ruins <- covs[!grepl("^[A-Za-z][A-Za-z0-9_]*$", covs) | covs %in% c(.TR_EXP_RESERVADOS, names(f)) |
                  duplicated(covs)]
  if (length(ruins)) {
    .tr_experiments_abort("tr_experiments_error_reserved_name",
                          "Param 'covariaveis': '%s' repete um fator ou coluna estrutural, ou não serve como nome.",
                          paste(ruins, collapse = ", "))
  }
  construir <- get(paste0(".tr_exp_estr_", estrutura), mode = "function")
  e <- .tr_exp_com_semente(.seed, construir(s))
  .tr_exp_montar_plano(e, estrutura, covs, receita, .seed)
}

#' Junta o que a construção devolveu no objeto plano (ver o contrato em
#' `R/type.R`), com as covariáveis e a reprodutibilidade.
#' @noRd
.tr_exp_montar_plano <- function(e, estrutura, covs, receita, seed) {
  u <- tibble::as_tibble(e$unidades)
  u$ordem <- seq_len(nrow(u))
  u <- u[, c("unidade", "ordem", setdiff(names(u), c("unidade", "ordem")))]
  fina <- utils::tail(e$hierarquia$nivel, 1L)
  fatores <- e$fatores
  for (cv in covs) {
    u[[cv]] <- NA_real_
    fatores <- rbind(fatores, .tr_exp_fator(cv, "covariavel", character(), fina, "—", "sem sorteio",
                                            "medida na unidade, não designada: não tem leitura causal"))
  }
  rownames(fatores) <- NULL
  pos <- data.frame(unidade = u$unidade, linha = as.integer(e$posicoes$linha), coluna = as.integer(e$posicoes$coluna))
  avisos <- as.character(unlist(e$avisos))
  versao <- tryCatch(as.character(utils::packageVersion("trama.experiments")), error = function(err) NA_character_)
  nota <- paste(c(e$rotulo, sprintf("%d unidades", nrow(u)), sprintf("análise sugerida: %s", e$analise$no),
                  avisos), collapse = "; ")
  structure(list(
    unidades = u, estrutura = estrutura, rotulo = e$rotulo, fatores = fatores, hierarquia = e$hierarquia,
    geometria = list(posicoes = pos, eixo_linha = e$eixos[[1]], eixo_coluna = e$eixos[[2]],
                     tipo = if (is.null(e$tipo_geo)) "campo" else e$tipo_geo),
    analise = e$analise, extras = if (is.null(e$extras)) list() else e$extras, receita = receita,
    semente = as.integer(seed), metodo = .TR_EXP_METODO_RNG, versao = versao, avisos = avisos, nota = nota
  ), class = "tr_experiments_plan")
}

#' Sorteia de novo o mesmo plano, com outra semente.
#'
#' A base do teste de aleatorização: a estrutura, os fatores e a geometria
#' ficam; só a alocação muda. Nível 1, sem nó próprio.
#' @param plano objeto `tr_experiments_plan`.
#' @param .seed a nova semente.
#' @return objeto `tr_experiments_plan`.
#' @export
tr_experiments_randomize <- function(plano, .seed = 1L) {
  .tr_exp_plano_conferir(plano)
  do.call(tr_experiments_design, c(plano$receita, list(.seed = .seed)))
}

#' @export
print.tr_experiments_plan <- function(x, ...) {
  cat("<plano de experimento>", x$rotulo, "\n")
  cat("semente", x$semente, "·", nrow(x$unidades), "unidades · análise:", x$analise$no, "\n")
  for (a in x$avisos) cat("aviso:", a, "\n")
  for (t in x$termos) {
    cat(sprintf("termo %s (%s%s)\n", t$nome, t$tipo,
                if (length(t$fator)) paste0(": ", paste(t$fator, collapse = ":")) else ""))
    if (!is.null(t$conversao)) print(t$conversao, row.names = FALSE)
  }
  if (!is.null(x$resposta)) cat("resposta", x$resposta$nome, "·", x$resposta$distribuicao, "\n")
  print(x$unidades, ...)
  invisible(x)
}
