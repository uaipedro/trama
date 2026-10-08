# Nós da aba Fonte e Preparar: de onde vem o objeto espacial.

# O nó não expõe `borda`: o registro exige que todo argumento da função tenha
# param ou porta, e a borda de uma tabela qualquer não tem de onde vir. A
# função pública continua aceitando `borda` (é como os exemplos a trazem).
.tr_spatial_coordinates_no <- function(dados, x, y, variavel, covariaveis = "",
                                       crs = "", unidade = "", nome = "") {
  tr_spatial_coordinates(dados, x = x, y = y, variavel = variavel,
                         covariaveis = covariaveis, crs = crs,
                         unidade = unidade, nome = nome)
}

.tr_spatial_nos_fonte <- function() {
  list(
    trama::tr_node("spatial/example", fn = tr_spatial_example, label = "Exemplo espacial",
      category = "espacial_fonte", icon = trama::tr_icon("database"),
      description = "Carrega um conjunto de exemplo do IBGE já como objeto espacial: coordenadas, projeção e borda do estado.",
      outputs = list(out = "spatial/points"),
      params = list(dataset = trama::tr_param_enum("milho_pr", .TR_SPATIAL_EXEMPLOS, label = "Conjunto")),
      pressupostos = .tr_spatial_press_coords(),
      referencias = list(.tr_spatial_refs()$isaaks, .tr_spatial_refs()$cressie),
      help = .tr_spatial_ajuda(r"---[
Carrega um conjunto real, do IBGE, pensado para ensinar geoestatística. Os três
vêm da mesma fonte: o rendimento médio da produção (kg/ha) da Produção Agrícola
Municipal de 2023 (SIDRA, tabela 5457), medido nas sedes municipais. As
coordenadas são UTM em **metros** (SIRGAS 2000), e a borda é a do estado.

- **milho_pr** — milho no Paraná, 389 municípios, com o rendimento da soja
  (`soja_kg_ha`) como covariável. Dependência espacial **forte** e tendência de
  larga escala: o variograma não estabiliza nos primeiros 300 km. É o caso que
  pede a remoção de tendência.
- **cafe_mg** — café em Minas Gerais, 496 municípios. Dependência espacial
  **moderada**, com alcance ajustado de várias centenas de quilômetros.
- **milho_se** — milho em Sergipe, 68 municípios. Dependência forte, alcance
  perto de 110 km. Conjunto pequeno, para exemplo rápido.

Fonte: IBGE. Os dados são abertos, e o uso exige citar a fonte.
]---", r"---[
- **Conjunto** — um dos três acima.
]---", r"---[
Um objeto espacial (`spatial/points`), com coordenadas, projeção, unidade e borda.
]---", r"---[
tr_flow(reg) |>
  tr_add("pontos", "spatial/example", dataset = "milho_pr")
]---", r"---[
`spatial/coordinates` para declarar o mesmo tipo de objeto a partir de uma tabela
sua.
]---")),

    trama::tr_node("spatial/coordinates", fn = .tr_spatial_coordinates_no, label = "Declarar coordenadas",
      category = "espacial_preparar", icon = trama::tr_icon("map-pin"),
      description = "Transforma uma tabela em objeto espacial: diz quais colunas são as coordenadas, a variável, o CRS e a unidade.",
      inputs = list(dados = "data/table"), outputs = list(out = "spatial/points"),
      params = list(
        x = trama::tr_param_col("", label = "Coordenada X", role = "numerica", example = "leste"),
        y = trama::tr_param_col("", label = "Coordenada Y", role = "numerica", example = "norte"),
        variavel = trama::tr_param_col("", label = "Variável", role = "numerica", example = "milho_kg_ha"),
        covariaveis = trama::tr_param_col("", label = "Covariáveis", multi = TRUE, suggest = FALSE,
                                          example = "soja_kg_ha"),
        crs = trama::tr_param_text("", label = "CRS (EPSG)"),
        unidade = trama::tr_param_text("", label = "Unidade da distância"),
        nome = trama::tr_param_text("", label = "Nome")),
      pressupostos = .tr_spatial_press_coords(),
      referencias = list(.tr_spatial_refs()$cressie, .tr_spatial_refs()$diggle,
                         .tr_spatial_refs()$oliver),
      help = .tr_spatial_ajuda(r"---[
É aqui que a tabela vira objeto espacial. O que sai daqui carrega as
coordenadas, a projeção, a unidade e a borda, e nenhum bloco adiante volta a
perguntar onde está o x.

As coordenadas precisam estar **projetadas**. Latitude e longitude em graus
são recusadas: o variograma mede distância em linha reta, e um grau de
longitude não é uma distância fixa — vale cerca de 111 km no equador e menos
conforme a latitude sobe.

Neste bloco o objeto sai **sem borda**: a borda só vem dos conjuntos de exemplo
desta versão (`spatial/example`). A função R `tr_spatial_coordinates()` aceita
uma borda diretamente, no argumento `borda`.
]---", r"---[
- **Coordenada X**, **Coordenada Y** — colunas numéricas, em CRS projetado.
- **Variável** — a variável regionalizada. Linha sem valor nela sai do cálculo,
  e a nota do objeto diz quantas.
- **Covariáveis** — colunas candidatas a tendência externa (opcional).
- **CRS (EPSG)** — código EPSG da projeção (por exemplo, 31983 para o UTM 23S).
  Vazio, trata como plano arbitrário.
- **Unidade da distância** — só rotula eixos e alcance (por exemplo, `m`).
- **Nome** — título do objeto nos cartões e gráficos adiante.
]---", r"---[
Um objeto espacial (`spatial/points`).
]---", r"---[
tr_flow(reg) |>
  tr_add("tab", "data/example") |>
  tr_add("pontos", "spatial/coordinates", x = "leste", y = "norte",
         variavel = "milho_kg_ha", crs = "31982", unidade = "m", from = "tab")
]---", r"---[
`spatial/example` para um conjunto pronto.
]---"))
  )
}
