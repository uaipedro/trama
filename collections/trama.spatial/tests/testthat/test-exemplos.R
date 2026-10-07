test_that("os três conjuntos abrem como objeto espacial", {
  esperado <- list(milho_pr = 389L, cafe_mg = 496L, milho_se = 68L)
  for (nome in names(esperado)) {
    p <- tr_spatial_example(nome)
    expect_s3_class(p, "tr_spatial_points")
    expect_equal(nrow(p$dados), esperado[[nome]], info = nome)
    expect_equal(ncol(p$coords), 2L, info = nome)
    expect_true(is.numeric(p$dados[[p$variavel]]), info = nome)
    expect_false(anyNA(p$coords), info = nome)
  }
})

test_that("os três trazem borda, e a borda é um anel fechado", {
  for (nome in c("milho_pr", "cafe_mg", "milho_se")) {
    b <- tr_spatial_example(nome)$borda
    expect_false(is.null(b), info = nome)
    expect_equal(ncol(b), 2L, info = nome)
    expect_equal(b[1, ], b[nrow(b), ], info = nome)
  }
})

test_that("as coordenadas estão projetadas em metros, não em graus", {
  for (nome in c("milho_pr", "cafe_mg", "milho_se")) {
    p <- tr_spatial_example(nome)
    # UTM em metros: easting na casa das centenas de milhar, northing dos milhões.
    expect_true(max(abs(p$coords)) > 1e5, info = nome)
    expect_false(sf::st_is_longlat(p$crs), info = nome)
  }
})

test_that("milho_pr traz a soja como covariável, e ela não é constante", {
  p <- tr_spatial_example("milho_pr")
  expect_true("soja_kg_ha" %in% p$covariaveis)
  expect_gt(stats::sd(p$dados$soja_kg_ha), 0)
})

test_that("os pontos caem dentro da borda do estado, com folga de 2 km", {
  # A borda foi simplificada (dTolerance = 1 km): sede costeira pode ficar um
  # pouco fora do anel. 2 km absorve isso e ainda pega CRS, estado ou unidade errados.
  for (nome in c("milho_pr", "cafe_mg", "milho_se")) {
    p <- tr_spatial_example(nome)
    pol <- sf::st_sfc(sf::st_polygon(list(p$borda)))
    pts <- sf::st_as_sf(as.data.frame(p$coords), coords = c(1, 2))
    fora <- lengths(sf::st_intersects(pts, sf::st_buffer(pol, 2000))) == 0L
    expect_equal(sum(fora), 0L, info = nome)
  }
})

test_that("o n do catálogo é o número de linhas dos dados", {
  reais <- c(milho_pr = nrow(spatial_milho_pr$dados),
             cafe_mg = nrow(spatial_cafe_mg$dados),
             milho_se = nrow(spatial_milho_se$dados))
  for (d in trama_collection()$datasets) {
    expect_equal(d$n, unname(reais[[d$nome]]), info = d$id)
  }
})

test_that("nome desconhecido é erro de opção, não de coluna", {
  expect_error(tr_spatial_example("marte"), class = "tr_spatial_error_bad_option")
})

test_that("cada conjunto declara proveniência e licença no catálogo de bases", {
  ds <- trama_collection()$datasets
  expect_equal(length(ds), 3L)
  for (d in ds) {
    expect_true(nzchar(d$licenca), info = d$id)
    expect_true(nzchar(d$fonte), info = d$id)
    expect_match(d$fonte, "IBGE", info = d$id)
    expect_false(grepl("CONFERID|TODO|TBD|<", d$licenca), info = d$id)
  }
})

# Guarda de regressão: o nome nu de um dado de LazyData só resolve com o pacote
# ANEXADO (library); o editor carrega a coleção por namespace, sem anexar, e o
# bloco falhava com "objeto 'spatial_milho_pr' não encontrado". Por isso o
# código chama `trama.spatial::spatial_*`. Sob load_all o pacote está anexado e
# o erro real não aparece, então a guarda lê o corpo da função.
test_that("os dados embutidos são chamados por pacote::nome, não pelo nome nu", {
  corpo <- paste(deparse(body(tr_spatial_example)), collapse = " ")
  nus <- regmatches(corpo, gregexpr("(?<![[:alnum:]_.:])spatial_(milho_pr|cafe_mg|milho_se)",
                                    corpo, perl = TRUE))[[1]]
  expect_length(nus, 0L)
})
