# Achados da revisão independente de 2026-10-09 que não couberam nos arquivos
# dos blocos: o mapa de grade irregular, a cobertura do KED pelo bloco, a nota
# da validação sobre a tendência, a tendência ignorada fora da universal, o
# padrão declarado no nó contra o da função, e o `version` dos nós que ganharam
# porta.

ked_fx <- function(n = 4L) {
  p <- tr_spatial_example("milho_pr")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p, tendencia = "covariavel"),
                                familia = "esferico")
  g <- expand.grid(
    x = seq(min(p$coords[, 1]), max(p$coords[, 1]), length.out = n),
    y = seq(min(p$coords[, 2]), max(p$coords[, 2]), length.out = n))
  names(g) <- p$coord_cols
  g[[p$covariaveis[[1]]]] <- seq_len(nrow(g)) * 10
  list(p = p, m = m, g = g)
}

# ---- I9: o KED pelo BLOCO, com sucesso -----------------------------------------
#
# Até a revisão, o único KED bem-sucedido dos testes passava por
# `tr_spatial_kriging_em`, o helper interno: as cinco chamadas ao bloco eram
# quatro `expect_error` e uma krigagem ordinária. O objeto de superfície do KED,
# a nota, o `tipo`, o adaptador e o mapa não tinham cobertura nenhuma — na
# feature-título do PR.

test_that("o KED roda pelo bloco e entrega superfície utilizável", {
  x <- ked_fx(4L)
  s <- tr_spatial_kriging(x$p, x$m, grade = x$g, tipo = "universal",
                          tendencia = "covariavel")
  expect_s3_class(s, "tr_spatial_surface")
  expect_equal(s$tipo, "universal")
  expect_equal(nrow(s$grade), nrow(x$g))
  expect_true(all(c("predito", "variancia", "erro_padrao") %in% names(s$grade)))
  expect_true(any(is.finite(s$grade$predito)))
  expect_match(s$nota, "covariavel|covariável")
  t <- .tr_spatial_superficie_tabela(s)
  expect_equal(nrow(t), nrow(x$g))
  g <- tr_spatial_map(s)
  expect_s3_class(g, "ggplot")
  expect_silent(invisible(ggplot2::ggplot_build(g)))
})

# ---- I8: grade irregular -------------------------------------------------------
#
# A ajuda da porta Grade anuncia "predizer em pontos escolhidos em vez de numa
# grade regular", e o mapa usava `geom_raster`, que exige retícula regular: o
# gráfico saía com as células DESLOCADAS e um aviso do ggplot que não chega ao
# card.

test_that("mapa de grade irregular sai sem aviso de pixel desigual", {
  p <- tr_spatial_example("milho_se")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "esferico")
  set.seed(11)
  g <- data.frame(
    x = runif(40, min(p$coords[, 1]), max(p$coords[, 1])),
    y = runif(40, min(p$coords[, 2]), max(p$coords[, 2])))
  names(g) <- p$coord_cols
  s <- tr_spatial_kriging(p, m, grade = g)
  g1 <- tr_spatial_map(s)
  expect_silent(invisible(ggplot2::ggplot_build(g1)))
})

test_that("grade regular segue usando raster, e irregular usa tile", {
  p <- tr_spatial_example("milho_se")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "esferico")
  reg <- tr_spatial_kriging(p, m, resolucao = 12L)
  geom <- function(g) class(g$layers[[1]]$geom)[[1]]
  expect_equal(geom(tr_spatial_map(reg)), "GeomRaster")
  set.seed(12)
  g <- data.frame(x = runif(30, min(p$coords[, 1]), max(p$coords[, 1])),
                  y = runif(30, min(p$coords[, 2]), max(p$coords[, 2])))
  names(g) <- p$coord_cols
  expect_equal(geom(tr_spatial_map(tr_spatial_kriging(p, m, grade = g))), "GeomTile")
})

test_that("a nota diz que a grade ligada não é recortada na borda", {
  p <- tr_spatial_example("milho_se")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "esferico")
  g <- expand.grid(x = seq(min(p$coords[, 1]), max(p$coords[, 1]), length.out = 3),
                   y = seq(min(p$coords[, 2]), max(p$coords[, 2]), length.out = 3))
  names(g) <- p$coord_cols
  s <- tr_spatial_kriging(p, m, grade = g)
  expect_match(s$nota, "não é recortada|sem recorte")
})

# ---- I10: a validação ignora a tendência do modelo ------------------------------

test_that("a validação avisa quando o modelo tem tendência removida", {
  p <- tr_spatial_example("milho_pr")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p, tendencia = "1a ordem"),
                                familia = "esferico")
  # k dobras e não leave-one-out: a nota não depende do método, e LOO em 389
  # pontos custa ~30 s sem acrescentar nada ao que o teste afirma.
  v <- tr_spatial_validation(p, m, metodo = "k dobras", dobras = 5L, semente = 1)
  expect_match(v$nota, "tendência")
  expect_match(v$nota, "ORDIN", fixed = TRUE)
})

test_that("sem tendência no modelo, a validação não inventa aviso", {
  p <- tr_spatial_example("milho_se")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "esferico")
  v <- tr_spatial_validation(p, m)
  expect_false(grepl("tendência", v$nota))
})

# ---- I11: tendência fora da universal ------------------------------------------

test_that("tendência passada fora da universal é ignorada, mas com nota", {
  p <- tr_spatial_example("milho_se")
  m <- tr_spatial_variogram_fit(tr_spatial_variogram(p), familia = "esferico")
  s <- tr_spatial_kriging(p, m, tipo = "ordinaria", tendencia = "2a ordem",
                          resolucao = 12L)
  expect_equal(s$tipo, "ordinaria")
  expect_match(s$nota, "IGNORADA", fixed = TRUE)
  # e o resultado é o mesmo da ordinária sem tendência nenhuma
  s0 <- tr_spatial_kriging(p, m, tipo = "ordinaria", resolucao = 12L)
  expect_equal(s$grade$predito, s0$grade$predito)
})

# ---- I12: padrão do nó contra padrão da função ---------------------------------

test_that("o padrão declarado em cada nó é o da função, param por param", {
  co <- trama_collection()
  # Divergência CONHECIDA e intencional: a função `tr_spatial_kriging` precisa
  # de `tendencia = "constante"` como padrão, porque o padrão dela é a
  # ordinária; o nó declara "1a ordem" porque o param só aparece quando o Tipo
  # é universal, e aí "constante" não é opção válida. Declarada aqui para não
  # ficar implícita — era o passo do plano que faltava.
  excecoes <- list("spatial/kriging" = "tendencia")
  conferidos <- 0L
  for (no in co$nodes) {
    fn <- no$fn
    if (!is.function(fn)) next
    # `deparse` sobre a lista inteira dos formais: ler `fo[[nome]]` de um
    # argumento SEM padrão cria vínculo a um argumento ausente, e qualquer uso
    # depois levanta missingArg.
    dps <- vapply(as.list(formals(fn)),
                  function(x) paste(deparse(x), collapse = ""), "")
    nomes_pm <- names(no$params)
    for (k in seq_along(no$params)) {
      nome <- nomes_pm[[k]] %||% ""
      if (!nzchar(nome) || !nome %in% names(dps)) next
      if (nome %in% (excecoes[[no$id]] %||% character())) next
      if (!nzchar(dps[[nome]])) next            # argumento sem padrão
      pm <- no$params[[k]]
      no_val <- pm$default %||% pm$value
      if (is.null(no_val) || !is.atomic(no_val) || length(no_val) != 1L) next
      expect_equal(paste(deparse(no_val), collapse = ""), dps[[nome]],
                   info = paste(no$id, nome))
      conferidos <- conferidos + 1L
    }
  }
  # o teste só vale se tiver conferido um número plausível de params
  expect_gt(conferidos, 15L)
})

# ---- I13: `version` dos nós que ganharam porta ----------------------------------

test_that("nó que ganhou porta na 0.2.0 subiu de version", {
  # `collections/AGENTS.md`: version sobe quando muda resultado, padrão de param
  # OU PORTAS. `spatial/kriging` ganhou a porta `grade` e `spatial/coordinates`
  # a porta `borda`.
  co <- trama_collection()
  v <- function(id) {
    n <- Filter(function(x) identical(x$id, id), co$nodes)[[1]]
    n$version
  }
  expect_gte(v("spatial/kriging"), 2L)
  expect_gte(v("spatial/coordinates"), 2L)
})
