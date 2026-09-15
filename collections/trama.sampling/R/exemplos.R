# Os dados de exemplo: POPULAÇÕES simuladas, com a verdade à mão.
#
# Amostragem só se ensina com a população conhecida: a estimativa de 3.412 t
# não diz nada até se saber que a verdade é 3.390. Por isso `fazendas` e
# `escolas` são cadastros inteiros — o `sampling/simulate` confere contra eles —
# e as estruturas foram PLANTADAS para cada desenho mostrar a sua cara:
#
# - `fazendas`: as regiões diferem muito em escala (o Norte tem poucas fazendas
#   enormes), então estratificar por região ganha, e o Neyman manda mais
#   amostra para o Norte. A produção cresce com a área, então PPS pela área
#   ganha. Os municípios têm efeito próprio, então conglomerar por município
#   perde.
# - `escolas`: alunos de uma escola se parecem (ICC ≈ 0,25), que é o caso em que
#   o deff de conglomerados dói.
#
# Semente e gerador próprios, devolvendo o estado do RNG como estava — a
# `fazendas` de hoje é a de amanhã, em qualquer computador.

.TR_SAMPLING_EXEMPLOS <- c("fazendas", "estratos_fazendas", "escolas", "escolas_resumo", "perfil_escolas",
                           "perguntas_exemplo", "domicilios")

#' 2400 fazendas em 4 regiões e 120 municípios.
#' @noRd
.tr_sampling_fazendas <- function() {
  .tr_sampling_com_semente(1977L, {
    reg <- data.frame(
      regiao = c("Norte", "Sul", "Leste", "Oeste"),
      municipios = c(15L, 45L, 30L, 30L),
      area_mediana = c(400, 60, 120, 200),
      produtividade = c(2.5, 4, 3, 3.5),
      p_irrigada = c(0.10, 0.45, 0.30, 0.20),
      stringsAsFactors = FALSE)
    por_mun <- 20L
    linhas <- lapply(seq_len(nrow(reg)), function(i) {
      r <- reg[i, ]
      mun <- rep(sprintf("%s-%02d", substr(r$regiao, 1, 1), seq_len(r$municipios)), each = por_mun)
      n <- length(mun)
      ef_mun <- stats::rnorm(r$municipios, 0, 0.25)[match(mun, unique(mun))]
      area <- round(stats::rlnorm(n, log(r$area_mediana), 0.7), 1)
      prod <- round(area * r$produtividade * exp(ef_mun + stats::rnorm(n, 0, 0.2)), 1)
      irr <- stats::runif(n) < stats::plogis(stats::qlogis(r$p_irrigada) + 2 * ef_mun)
      data.frame(regiao = r$regiao, municipio = mun, area_ha = area, producao_t = prod,
                 irrigada = ifelse(irr, "sim", "não"),
                 trabalhadores = 1L + stats::rpois(n, area / 40), stringsAsFactors = FALSE)
    })
    d <- do.call(rbind, linhas)
    d <- cbind(fazenda = sprintf("F%04d", seq_len(nrow(d))), d)
    tibble::as_tibble(d)
  })
}

#' A tabela de estratos de `fazendas`: N, desvio da produção e custo por
#' entrevista. É o que um `data/group_summarise` do cadastro daria, mais o custo.
#' @noRd
.tr_sampling_estratos_fazendas <- function() {
  f <- .tr_sampling_fazendas()
  regioes <- unique(f$regiao)
  tibble::tibble(
    regiao = regioes,
    N = as.integer(table(f$regiao)[regioes]),
    desvio_producao = round(as.numeric(tapply(f$producao_t, f$regiao, stats::sd)[regioes]), 1),
    custo = c(Norte = 60, Sul = 20, Leste = 30, Oeste = 40)[regioes])
}

#' Alunos em 200 escolas: 160 públicas e 40 privadas, ICC da nota ≈ 0,25.
#' @noRd
.tr_sampling_escolas <- function() {
  .tr_sampling_com_semente(2014L, {
    k <- 200L
    rede <- rep(c("pública", "privada"), c(160L, 40L))
    alunos <- sample(20:60, k, replace = TRUE)
    ef_escola <- stats::rnorm(k, 0, 5)
    escola <- rep(sprintf("E%03d", seq_len(k)), alunos)
    i <- rep(seq_len(k), alunos)
    nota <- 60 + ifelse(rede[i] == "privada", 8, 0) + ef_escola[i] + stats::rnorm(length(i), 0, 9)
    nota <- pmin(100, pmax(0, round(nota, 1)))
    reprovado <- stats::runif(length(i)) < stats::plogis(-1.5 - 0.08 * (nota - 60))
    tibble::tibble(escola = escola, rede = rede[i], aluno = sequence(alunos), nota = nota,
                   reprovado = ifelse(reprovado, "sim", "não"))
  })
}

#' Uma linha por escola: a tabela de UNIDADES para as margens por nível.
#' @noRd
.tr_sampling_escolas_resumo <- function() {
  e <- .tr_sampling_escolas()
  ids <- unique(e$escola)
  tibble::tibble(escola = ids, rede = e$rede[match(ids, e$escola)],
                 alunos = as.integer(table(e$escola)[ids]))
}

#' O perfil da população de alunos, em formato longo: a composição para o
#' tamanho por grupos e os totais para o raking.
#' @noRd
.tr_sampling_perfil_escolas <- function() {
  e <- .tr_sampling_escolas()
  do.call(rbind, lapply(c("rede", "reprovado"), function(v) {
    t <- table(e[[v]])
    tibble::tibble(variavel = v, categoria = names(t), total = as.integer(t),
                   participacao = as.numeric(t) / nrow(e))
  }))
}

#' Um questionário mínimo, um tipo de pergunta por linha: o formato que
#' `sampling/question_margins` lê.
#' @noRd
.tr_sampling_perguntas_exemplo <- function() {
  tibble::tibble(
    pergunta = c("Você repetiu de ano?", "Qual sua rede de ensino?", "Quais transportes usa? (marque todos)",
                 "A escola é segura (1 a 5)", "Se repetiu, quantas vezes?", "O que mudaria na escola?"),
    tipo = c("binária", "única", "múltipla", "escala", "única", "aberta"),
    opcoes = c(2, 3, 6, 5, 3, NA),
    base = c(1, 1, 1, 1, 0.2, 1))
}

#' Uma amostra JÁ COLETADA: domicílios em setores censitários, estratificada
#' por situação (urbano/rural), 30 e 12 setores sorteados por AAS, 10
#' domicílios por setor. Traz o que a base de uma pesquisa oficial traz: o
#' estrato, o setor, o peso e o número de setores do estrato.
#' @noRd
.tr_sampling_domicilios <- function() {
  .tr_sampling_com_semente(2010L, {
    estr <- data.frame(estrato = c("urbano", "rural"), setores = c(400L, 150L), sorteados = c(30L, 12L),
                       renda = c(3200, 1500), p_internet = c(0.85, 0.45), stringsAsFactors = FALSE)
    linhas <- lapply(seq_len(nrow(estr)), function(h) {
      e <- estr[h, ]
      setores <- sort(sample.int(e$setores, e$sorteados))
      do.call(rbind, lapply(setores, function(s) {
        dom_setor <- sample(120:320, 1L)
        ef <- stats::rnorm(1L, 0, 0.3)
        data.frame(estrato = e$estrato, setor = sprintf("%s-%03d", substr(e$estrato, 1, 1), s),
                   setores_estrato = e$setores, domicilios_setor = dom_setor,
                   peso = round((e$setores / e$sorteados) * (dom_setor / 10), 3),
                   moradores = 1L + stats::rpois(10L, 2),
                   renda = round(stats::rlnorm(10L, log(e$renda) + ef, 0.6)),
                   internet = ifelse(stats::runif(10L) < stats::plogis(stats::qlogis(e$p_internet) + 2 * ef),
                                     "sim", "não"),
                   stringsAsFactors = FALSE)
      }))
    })
    tibble::as_tibble(do.call(rbind, linhas))
  })
}

#' Carrega um conjunto de exemplo.
#' @param dataset nome do conjunto (ver a ajuda do nó).
#' @return tibble.
#' @export
tr_sampling_example <- function(dataset = "fazendas") {
  dataset <- .tr_sampling_enum(dataset, .TR_SAMPLING_EXEMPLOS, "dataset")
  switch(dataset,
    fazendas = .tr_sampling_fazendas(),
    estratos_fazendas = .tr_sampling_estratos_fazendas(),
    escolas = .tr_sampling_escolas(),
    escolas_resumo = .tr_sampling_escolas_resumo(),
    perfil_escolas = .tr_sampling_perfil_escolas(),
    perguntas_exemplo = .tr_sampling_perguntas_exemplo(),
    domicilios = .tr_sampling_domicilios())
}
