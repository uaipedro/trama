# Variogramas direcionais num objeto só, para diagnosticar anisotropia.
#
# Motor: `gstat::variogram(alpha = c(...))`, que devolve as N direções numa
# tabela com a coluna `dir.hor`. Nenhum motor novo; a tendência passa pelo mesmo
# `.tr_spatial_dados_tendencia()` do variograma.
#
# O bloco NÃO estima razão nem ângulo de anisotropia, e isso é decisão MEDIDA,
# não omissão. Três implementações foram testadas em 2026-10-09 e as três
# reprovaram (detalhe em `docs/revisao-metodologica.md`):
#   1. ajuste do alcance direção por direção, patamar livre: o otimizador fugiu
#      (alcance 31.393 num campo de alcance 150), razão 116,7 contra verdade 3;
#   2. busca em grade sobre (ângulo, razão) comparando `SSErr`: num variograma
#      SEM RUÍDO, gerado do próprio modelo, devolveu 90/5,5 em vez de 60/3 —
#      porque `fit.variogram` não usa `dir.hor` e trata as curvas como nuvem só;
#   3. regra descritiva (cruzamento de 95% da variância): em campos
#      anisotrópicos deu 3,02 / 3,11 / 2,64 / 3,06 / 1,25, e em campos
#      ISOTRÓPICOS deu 1,49 / 1,70 / 1,92 / 2,25 / 3,01 — as duas distribuições
#      se sobrepõem por inteiro.
# Razão e ângulo são DIGITADOS no `spatial/variogram_fit`, lidos deste card.

.TR_SPATIAL_CAMPOS_ANISO <- c("tabela", "direcoes", "estimador", "dist_max",
                              "n_classes", "tolerancia", "tendencia", "envelope",
                              "n_sim", "semente", "variavel", "unidade", "pontos",
                              "nota")

#' Lê o param `direcoes` ("0,45,90,135") num vetor de graus.
#' @noRd
.tr_spatial_direcoes <- function(direcoes) {
  bruto <- strsplit(paste(as.character(direcoes), collapse = ","), ",",
                    fixed = TRUE)[[1]]
  v <- suppressWarnings(as.numeric(trimws(bruto)))
  if (!length(v) || anyNA(v) || any(v < 0 | v >= 180)) {
    .tr_spatial_abort("tr_spatial_error_bad_option", paste(
      "Direções: use números de 0 (inclusive) a 180 (exclusive), separados por",
      "vírgula, por exemplo '0,45,90,135'. O variograma não distingue uma",
      "direção da oposta, então 180 em diante repetiria curva."))
  }
  if (length(v) < 2L) {
    .tr_spatial_abort("tr_spatial_error_bad_option", paste(
      "Direções: com uma direção só não há o que comparar. Use ao menos duas,",
      "ou o bloco `spatial/variogram`, que tem o param Direção."))
  }
  if (anyDuplicated(v) > 0L) {
    .tr_spatial_abort("tr_spatial_error_bad_option",
      "Direções: há direção repetida na lista.")
  }
  sort(v)
}

#' Faixa de referência: mínimo e máximo por direção e classe.
#'
#' Aritmética pura sobre a matriz de gammas simulados — é isso que tira o teste
#' da faixa da dependência do otimizador.
#' @param gammas matriz (direção × classe) por (simulação).
#' @param chaves data.frame com `direcao` e `u`, na ordem das linhas de `gammas`.
#' @noRd
.tr_spatial_faixa <- function(gammas, chaves) {
  data.frame(direcao = chaves$direcao, u = chaves$u,
             inferior = apply(gammas, 1, min, na.rm = TRUE),
             superior = apply(gammas, 1, max, na.rm = TRUE))
}

#' Simula campos sob ISOTROPIA, nas MESMAS posições dos pontos.
#'
#' As posições não mudam, então as classes de distância e a contagem de pares
#' das simulações são idênticas às do observado: só gamma varia. É o que
#' permite casar as duas tabelas linha a linha.
#'
#' Custo medido em 2026-10-09: 19 campos mais 19 variogramas direcionais em
#' 1,6 s com 389 pontos, 0,34 s com 68.
#' @noRd
.tr_spatial_simular_iso <- function(pontos, modelo, n_sim) {
  co <- as.data.frame(pontos$coords)
  names(co) <- c(".sx", ".sy")
  nd <- sf::st_as_sf(co, coords = c(".sx", ".sy"))
  s <- gstat::krige(stats::as.formula(".z ~ 1"), locations = NULL, newdata = nd,
                    model = .tr_spatial_vgm_model(modelo),
                    nsim = as.integer(n_sim), dummy = TRUE, beta = 0,
                    debug.level = 0)
  as.matrix(sf::st_drop_geometry(s))
}

#' Variogramas direcionais, para diagnosticar anisotropia.
#'
#' Anisotropia é a dependência espacial ter alcance diferente conforme a
#' direção: o variograma omnidirecional a esconde, porque mistura todas. Este
#' bloco separa, e a leitura é **visual** — ele não faz teste de hipótese.
#'
#' @param pontos um objeto espacial (`spatial/points`).
#' @param direcoes graus, horário a partir do Norte, separados por vírgula.
#' @param estimador `classico` ou `robusto`.
#' @param dist_max corte das distâncias; `NA` usa o padrão da coleção.
#' @param n_classes número de classes de distância.
#' @param tolerancia meia-abertura angular, em graus.
#' @param tendencia tendência removida antes, como no `spatial/variogram`.
#' @param pares_min mínimo de pares para a classe entrar.
#' @return um objeto de anisotropia (`spatial/anisotropy`).
#' @export
tr_spatial_anisotropy <- function(pontos, direcoes = "0,45,90,135",
                                  estimador = "classico", dist_max = NA,
                                  n_classes = 10L, tolerancia = 22.5,
                                  tendencia = "constante", envelope = FALSE,
                                  n_sim = 19L, semente = NA, pares_min = 30L,
                                  .simular = NULL) {
  .tr_spatial_pontos_conferir(pontos)
  dirs <- .tr_spatial_direcoes(direcoes)
  if (!estimador %in% .TR_SPATIAL_ESTIMADORES) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Estimador: escolha um de %s.", paste(.TR_SPATIAL_ESTIMADORES, collapse = ", ")))
  }
  n_classes <- suppressWarnings(as.integer(n_classes))
  if (length(n_classes) != 1L || is.na(n_classes) || n_classes < 3L) {
    .tr_spatial_abort("tr_spatial_error_bad_option", "Classes: use ao menos 3.")
  }
  pares_min <- suppressWarnings(as.integer(pares_min))
  if (length(pares_min) != 1L || is.na(pares_min) || pares_min < 1L) {
    .tr_spatial_abort("tr_spatial_error_bad_option",
      "Mínimo de pares: use um inteiro a partir de 1.")
  }
  if (!is.numeric(tolerancia) || length(tolerancia) != 1L ||
      !is.finite(tolerancia) || tolerancia <= 0 || tolerancia > 90) {
    .tr_spatial_abort("tr_spatial_error_bad_option",
      "Tolerância: use um número maior que 0 e até 90 graus.")
  }
  corte <- if (is.na(dist_max)) .tr_spatial_corte_padrao(pontos$coords) else
    as.numeric(dist_max)
  if (!is.finite(corte) || corte <= 0) {
    .tr_spatial_abort("tr_spatial_error_bad_option",
      "Distância máxima: use um número positivo.")
  }
  lim <- seq(0, corte, length.out = n_classes + 1L)
  dt <- .tr_spatial_dados_tendencia(pontos, tendencia)
  v <- gstat::variogram(dt$formula, locations = dt$locations, data = dt$dados,
                        alpha = dirs, tol.hor = as.numeric(tolerancia),
                        boundaries = lim[-1],
                        cressie = identical(estimador, "robusto"))
  tab <- tibble::tibble(direcao = as.numeric(v$dir.hor), u = as.numeric(v$dist),
                        gamma = as.numeric(v$gamma), np = as.integer(v$np))
  tab <- tab[order(tab$direcao, tab$u), , drop = FALSE]
  tab <- tab[tab$np >= pares_min, , drop = FALSE]
  if (!nrow(tab)) {
    .tr_spatial_abort("tr_spatial_error_empty_variogram", sprintf(paste(
      "Nenhuma classe chegou a %d pares. Com %d direções cada classe recebe uma",
      "fração dos pares: aumente a tolerância angular, use menos classes, ou",
      "menos direções."), pares_min, length(dirs)))
  }
  faltam <- setdiff(dirs, unique(tab$direcao))
  notas <- c(sprintf("%d direções, tolerância de %g graus, %d classes até %s.",
                     length(dirs), tolerancia, n_classes,
                     .tr_spatial_num(corte)),
             if (length(faltam)) sprintf(
               "Sem pares bastantes nas direções %s, que ficaram de fora.",
               paste(faltam, collapse = ", ")),
             if (nzchar(pontos$nota)) pontos$nota)
  faixa <- NULL
  if (isTRUE(envelope)) {
    n_sim <- suppressWarnings(as.integer(n_sim))
    if (length(n_sim) != 1L || is.na(n_sim) || n_sim < 5L || n_sim > 999L) {
      .tr_spatial_abort("tr_spatial_error_bad_option", paste(
        "Simulações: use um inteiro de 5 a 999. Abaixo de 5 a faixa não é faixa;",
        "acima de algumas centenas o card fica lento sem ganhar informação."))
    }
    if (!is.na(semente)) set.seed(as.integer(semente))
    # O modelo da nula é ISOTRÓPICO, ajustado ao variograma OMNIDIRECIONAL com a
    # mesma tendência e o mesmo corte — mas com o número de classes PADRÃO do
    # `spatial/variogram`, não com o deste bloco. O bloco reduz as classes
    # porque os pares se dividem entre as direções; o omnidirecional tem todos
    # os pares, e com 10 classes o ajuste do menor conjunto (`milho_se`) não
    # converge, enquanto com 15 converge (medido em 2026-10-09).
    mod_iso <- try(tr_spatial_variogram_fit(tr_spatial_variogram(
      pontos, estimador = estimador, dist_max = corte,
      tendencia = tendencia, pares_min = 1L)), silent = TRUE)
    if (inherits(mod_iso, "try-error")) {
      .tr_spatial_abort("tr_spatial_error_no_convergence", paste(
        "A faixa de referência precisa de um modelo isotrópico ajustado ao",
        "variograma omnidirecional, e esse ajuste não convergiu:",
        sub("
.*", "", conditionMessage(attr(mod_iso, "condition"))),
        "As curvas direcionais em si não dependem dele — desligue a faixa, ou",
        "mude a distância máxima e as classes até o ajuste fechar."))
    }
    simular <- .simular %||% .tr_spatial_simular_iso
    campos <- simular(pontos, mod_iso, n_sim)
    if (!is.matrix(campos) || nrow(campos) != nrow(pontos$dados)) {
      .tr_spatial_abort("tr_spatial_error_bad_option", paste(
        "O simulador devolveu", nrow(campos), "linhas para",
        nrow(pontos$dados), "pontos."))
    }
    chave <- paste(tab$direcao, round(tab$u, 6))
    gam <- vapply(seq_len(ncol(campos)), function(j) {
      ds <- dt$dados
      ds$.zsim <- campos[, j]
      # A MESMA tendência do observado, com `.zsim` no lugar da variável: o
      # observado passa pela remoção por OLS dentro do `gstat`, e simular sem
      # remover deixaria a faixa alta (medido pela revisão: ~4% no geral e ~7%
      # nas classes longas, que são as que separam anisotropia). A nula da ajuda
      # é "isotrópico com esta estrutura", não "sem remoção de tendência".
      f_sim <- stats::as.formula(paste(".zsim ~",
                                       paste(deparse(dt$formula[[3]]), collapse = " ")))
      vs <- gstat::variogram(f_sim,
                             locations = dt$locations, data = ds,
                             alpha = dirs, tol.hor = as.numeric(tolerancia),
                             boundaries = lim[-1],
                             cressie = identical(estimador, "robusto"))
      g <- as.numeric(vs$gamma)[match(chave, paste(as.numeric(vs$dir.hor),
                                                   round(as.numeric(vs$dist), 6)))]
      g
    }, numeric(nrow(tab)))
    if (!is.matrix(gam)) gam <- matrix(gam, nrow = nrow(tab))
    faixa <- .tr_spatial_faixa(gam, tab[, c("direcao", "u")])
    notas <- c(notas, sprintf(paste(
      "Faixa de referência de %d simulações sob isotropia (modelo ajustado ao",
      "variograma omnidirecional). É referência visual, não teste de hipótese."),
      n_sim))
  }
  structure(list(
    tabela = tab, direcoes = dirs, estimador = estimador, dist_max = corte,
    n_classes = n_classes, tolerancia = as.numeric(tolerancia),
    tendencia = tendencia, envelope = faixa,
    n_sim = if (isTRUE(envelope)) as.integer(n_sim) else NA_integer_,
    semente = if (is.na(semente)) NA_real_ else as.numeric(semente),
    variavel = pontos$variavel, unidade = pontos$unidade,
    pontos = pontos, nota = paste(notas, collapse = " ")),
    class = "tr_spatial_anisotropy")
}

.tr_spatial_aniso_conferir <- function(x) {
  .tr_spatial_guard(x, "tr_spatial_anisotropy", .TR_SPATIAL_CAMPOS_ANISO,
                    "tr_spatial_error_not_anisotropy", "um objeto de anisotropia")
}

#' Anisotropia -> tabela: uma linha por direção e classe; com envelope, as
#' colunas da faixa entram ao lado.
#' @noRd
.tr_spatial_aniso_tabela <- function(x) {
  .tr_spatial_aniso_conferir(x)
  t <- as.data.frame(x$tabela)
  if (!is.null(x$envelope)) {
    t <- merge(t, as.data.frame(x$envelope), by = c("direcao", "u"), all.x = TRUE)
    t <- t[order(t$direcao, t$u), , drop = FALSE]
    rownames(t) <- NULL
  }
  t
}

#' O que o card da anisotropia mostra: uma curva por direção.
#' @noRd
.tr_spatial_aniso_preview <- function(x) {
  faixa <- if (is.null(x$envelope)) NULL else as.data.frame(x$envelope)
  curvas <- lapply(x$direcoes, function(d) {
    cl <- x$tabela[x$tabela$direcao == d, , drop = FALSE]
    if (!nrow(cl)) return(NULL)
    fx <- if (is.null(faixa)) NULL else faixa[faixa$direcao == d, , drop = FALSE]
    list(direcao = d,
         classes = lapply(seq_len(nrow(cl)), function(i) {
           list(u = cl$u[[i]], gamma = cl$gamma[[i]], np = cl$np[[i]])
         }),
         faixa = if (is.null(fx) || !nrow(fx)) NULL else
           lapply(seq_len(nrow(fx)), function(i) {
             list(u = fx$u[[i]], inferior = fx$inferior[[i]],
                  superior = fx$superior[[i]])
           }))
  })
  list(variavel = x$variavel, unidade = .tr_spatial_nulo(x$unidade),
       estimador = x$estimador, tendencia = x$tendencia,
       tolerancia = x$tolerancia, n_sim = .tr_spatial_nulo(x$n_sim),
       curvas = Filter(Negate(is.null), curvas),
       nota = .tr_spatial_nulo(x$nota))
}

spatial_anisotropy_type <- function() {
  .tr_spatial_rds_type("spatial/anisotropy", "Anisotropia", "#7c3aed",
                       .tr_spatial_aniso_conferir,
                       function(x, ctx) trama::tr_preview(
                         "spatial/anisotropy", data = .tr_spatial_aniso_preview(x)),
                       report = tr_spatial_report_anisotropy)
}
