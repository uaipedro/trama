# Gera os conjuntos de exemplo da coleção a partir do IBGE.
#
# Rodado à mão, com rede; o resultado vai versionado em `data/`. O pacote NÃO
# depende de `sidrar` nem de `geobr` em tempo de uso: eles são só o cliente que
# baixa, e o que se distribui são os números do IBGE.
#
# Fonte: IBGE/SIDRA, tabela 5457 (Produção Agrícola Municipal), variável 112
# (rendimento médio da produção, kg/ha), ano 2023. Coordenadas: sedes
# municipais do IBGE (geobr::read_municipal_seat, ano 2010). Bordas:
# fronteiras estaduais do IBGE (geobr::read_state, ano 2020).
# Licença: dados abertos do IBGE — reprodução, redistribuição e reuso
# permitidos mediante CITAÇÃO DA FONTE.
#
# Rode a partir de `collections/trama.spatial/` (o `usethis::use_data` grava em
# `data/` do projeto ativo).
#
# DADOS VERSIONADOS: o bloco abaixo descreve os `.rda` hoje commitados em
# `data/`. Se uma regeneração imprimir valores diferentes, é discrepância.
#   data da execução: 2026-10-06
#   sidrar 0.2.9 | geobr 1.9.1 | sf 1.0.22
#   vértices da borda (PR / MG / SE): 497 / 1005 / 148
#   linhas (PR / MG / SE): 389 / 496 / 68
#
# Procedência desta execução (três fontes vivas, de safras diferentes: SIDRA
# 2023, sedes de 2010, malha estadual de 2020). Registrada ao final do run:
# data e versões de sidrar, geobr e sf.
#
# Risco conhecido de divergência silenciosa: município criado depois de 2010 não
# tem sede em 2010 e some no `merge` sem aviso; só a mudança na contagem de
# linhas (o `stopifnot` abaixo) revela.

library(sidrar); library(geobr); library(sf)

PRODUTO <- c(milho = 40122, cafe_total = 40139)

rendimento <- function(uf, produto) {
  r <- get_sidra(x = 5457, variable = 112, period = "2023", geo = "City",
                 geo.filter = list("State" = uf),
                 classific = "c782", category = list(c782 = produto))
  data.frame(code_muni = as.numeric(r$`Município (Código)`),
             valor = suppressWarnings(as.numeric(r$Valor)))
}

sedes <- function(uf, epsg) {
  s <- read_municipal_seat(year = 2010, showProgress = FALSE)
  s <- s[substr(as.character(s$code_muni), 1, 2) == as.character(uf), ]
  s <- st_transform(s, epsg)
  xy <- st_coordinates(s)
  data.frame(code_muni = as.numeric(s$code_muni), municipio = s$name_muni,
             leste = xy[, 1], norte = xy[, 2])
}

borda_uf <- function(uf, epsg) {
  g <- st_transform(read_state(code_state = uf, year = 2020, showProgress = FALSE), epsg)
  # Anel exterior do maior polígono: a borda do tipo é uma matriz n x 2.
  # A malha do IBGE vem com dezenas de milhares de vértices (a costa pesa);
  # 1 km de tolerância dá centenas, muda a área em menos de 0,05% e mantém o
  # polígono válido. Simplifica o estado inteiro, depois escolhe o maior.
  g <- sf::st_simplify(g, dTolerance = 1000, preserveTopology = TRUE)
  p <- st_cast(st_geometry(g), "POLYGON")
  # Guardar só o maior polígono descarta ilhas (sem importância para PR, MG e SE).
  p <- p[[which.max(vapply(p, function(x) as.numeric(st_area(st_sfc(x))), 0))]]
  stopifnot(sf::st_is_valid(sf::st_sfc(sf::st_polygon(list(p[[1]])))))
  m <- unname(p[[1]])
  if (!identical(m[1, ], m[nrow(m), ])) m <- rbind(m, m[1, , drop = FALSE])
  m
}

monta <- function(uf, epsg, produto, nome_valor, extras = NULL) {
  d <- sedes(uf, epsg)
  v <- rendimento(uf, produto); names(v)[2] <- nome_valor
  d <- merge(d, v, by = "code_muni")
  for (nm in names(extras)) {
    e <- rendimento(uf, extras[[nm]]); names(e)[2] <- nm
    d <- merge(d, e, by = "code_muni")
  }
  d <- d[stats::complete.cases(d), ]
  d <- d[order(d$code_muni), ]
  rownames(d) <- NULL
  list(dados = d, borda = borda_uf(uf, epsg))
}

spatial_milho_pr <- monta(41, 31982, PRODUTO[["milho"]], "milho_kg_ha",
                          extras = list(soja_kg_ha = 40124))
spatial_cafe_mg  <- monta(31, 31983, PRODUTO[["cafe_total"]], "cafe_kg_ha")
spatial_milho_se <- monta(28, 31984, PRODUTO[["milho"]], "milho_kg_ha")

stopifnot(nrow(spatial_milho_pr$dados) == 389,
          nrow(spatial_cafe_mg$dados)  == 496,
          nrow(spatial_milho_se$dados) == 68)

cat("Execução em", format(Sys.Date()), "| sidrar", format(packageVersion("sidrar")),
    "| geobr", format(packageVersion("geobr")), "| sf", format(packageVersion("sf")), "
")
cat("Vértices da borda:", nrow(spatial_milho_pr$borda), nrow(spatial_cafe_mg$borda),
    nrow(spatial_milho_se$borda), "
")

usethis::use_data(spatial_milho_pr, spatial_cafe_mg, spatial_milho_se,
                  overwrite = TRUE, compress = "xz")
