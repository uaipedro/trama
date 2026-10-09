# Nós da aba Fonte e Preparar: de onde vem o objeto espacial.

# A borda entra por PORTA (`spatial/boundary`, opcional) e, quando não há porta
# ligada, pelo param `borda_modo`, que oferece o casco convexo dos pontos. Na
# 0.1.0 o nó não expunha borda nenhuma, e para dado próprio a grade da krigagem
# era o retângulo da extensão, sem recorte.
.tr_spatial_coordinates_no <- function(dados, x, y, variavel, covariaveis = "",
                                       crs = "", unidade = "", nome = "",
                                       borda = NULL, borda_modo = "nenhuma") {
  tr_spatial_coordinates(dados, x = x, y = y, variavel = variavel,
                         covariaveis = covariaveis, crs = crs,
                         unidade = unidade, nome = nome, borda = borda,
                         borda_modo = borda_modo)
}

.tr_spatial_nos_fonte <- function() {
  E <- trama::tr_param_enum; N <- trama::tr_param_num
  list(
    trama::tr_node("spatial/indicator", fn = tr_spatial_indicator,
      label = "Indicador", category = "espacial_preparar",
      icon = trama::tr_icon("toggle-left"),
      description = "Transforma a variável num indicador 0/1 num valor de corte, para a krigagem estimar probabilidade em vez do valor.",
      inputs = list(pontos = "spatial/points"),
      outputs = list(out = "spatial/points"),
      params = list(
        corte = N(NA, label = "Corte"),
        sentido = E("<=", .TR_SPATIAL_SENTIDOS, label = "Sentido")),
      pressupostos = .tr_spatial_press_coords(),
      referencias = list(.tr_spatial_refs()$isaaks, .tr_spatial_refs()$cressie),
      help = .tr_spatial_ajuda(r"---[
Troca a variável por um **indicador**: 1 onde ela satisfaz a condição, 0 onde
não. Daí o fluxo segue igual — variograma, ajuste, krigagem —, e o que a
krigagem estima passa a ser a **probabilidade** de a condição valer em cada
célula, não o valor da variável.

Serve à pergunta que o mapa do predito não responde: não "quanto", mas "qual a
chance de passar deste limite". Teor acima do crítico, rendimento abaixo do que
paga a lavoura, contaminante acima da norma.

**Por que isto é um bloco, e não uma opção da krigagem.** A krigagem indicadora
é a krigagem ordinária de uma variável 0/1, e o que a torna indicadora é o
variograma ser o **do indicador**. Transformar aqui, antes do variograma,
garante que o modelo ajustado descreva o indicador. Se o corte fosse uma opção
do bloco de krigagem, ele receberia um modelo ajustado à variável contínua e
krigaria o indicador com ele — resultado errado, sem erro nenhum na tela.

O predito é recortado em **[0, 1]**: a krigagem é um interpolador linear e sai
desse intervalo de verdade, sobretudo longe dos pontos. A nota do card diz
quantas células precisaram do recorte, porque muitas delas são sinal de modelo
ruim para a pergunta.
]---", r"---[
- **Corte** — o valor que separa, na unidade da variável original. Precisa cair
  dentro do intervalo observado: fora dele o indicador sai constante, e o bloco
  recusa.
- **Sentido** — `<=` estima a probabilidade de **não exceder** o corte; `>`, a
  de exceder. Os dois são complementares e somam 1.
]---", r"---[
Um objeto espacial (`spatial/points`) cuja variável é o indicador, marcado para
que a krigagem e o mapa adiante o leiam como probabilidade.
]---", r"---[
tr_flow(reg) |>
  tr_add("pontos", "spatial/example", dataset = "milho_pr") |>
  tr_add("ind", "spatial/indicator", corte = 4000, from = "pontos") |>
  tr_add("v", "spatial/variogram", from = "ind") |>
  tr_add("m", "spatial/variogram_fit", from = "v") |>
  tr_add("k", "spatial/kriging", from = c("ind", "m")) |>
  tr_add("mapa", "spatial/map", from = "k")
]---", r"---[
`spatial/variogram`, que daqui mede a dependência do indicador;
`spatial/explore` para ver a distribuição antes de escolher o corte.
]---")),

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
      inputs = list(dados = "data/table",
                    borda = trama::tr_port("spatial/boundary", required = FALSE)),
      outputs = list(out = "spatial/points"),
      params = list(
        x = trama::tr_param_col("", label = "Coordenada X", role = "numerica", example = "leste"),
        y = trama::tr_param_col("", label = "Coordenada Y", role = "numerica", example = "norte"),
        variavel = trama::tr_param_col("", label = "Variável", role = "numerica", example = "milho_kg_ha"),
        covariaveis = trama::tr_param_col("", label = "Covariáveis", multi = TRUE, suggest = FALSE,
                                          example = "soja_kg_ha"),
        crs = trama::tr_param_text("", label = "CRS (EPSG)"),
        unidade = trama::tr_param_text("", label = "Unidade da distância"),
        nome = trama::tr_param_text("", label = "Nome"),
        borda_modo = trama::tr_param_enum("nenhuma", .TR_SPATIAL_BORDA_MODOS,
                                          label = "Borda")),
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

A **borda** da área de estudo entra por aqui, e vale a pena: ela recorta a grade
da krigagem, desenha o contorno no gráfico exploratório e dispara a guarda que
pega borda em escala, projeção ou lugar errado. Sem borda, a grade é o retângulo
da extensão dos pontos, e o mapa prediz fora da área de estudo com cara de
resultado válido.

Dois caminhos: ligar um bloco `spatial/boundary` na porta **Borda**, que lê o
contorno de um arquivo vetorial; ou, sem arquivo nenhum, pôr o param **Borda** em
*casco convexo dos pontos*, que usa o menor polígono convexo que contém a amostra.
Borda ligada na porta vence o param.

Quando as projeções diferem, a borda **é reprojetada** para a dos pontos, e a nota
do objeto diz de qual para qual. Borda em grau com pontos em metro é reprojetada,
não recusada: a malha do IBGE vem em grau, e a recusa de grau vale para os
pontos, cuja distância o variograma mede, não para o recorte. O que o bloco
recusa é borda **sem** projeção declarada quando os pontos têm uma: aí não há
como saber em que plano a borda está.
]---", r"---[
- **Coordenada X**, **Coordenada Y** — colunas numéricas, em CRS projetado.
- **Variável** — a variável regionalizada. Linha sem valor nela sai do cálculo,
  e a nota do objeto diz quantas.
- **Covariáveis** — colunas candidatas a tendência externa (opcional).
- **CRS (EPSG)** — código EPSG da projeção (por exemplo, 31983 para o UTM 23S).
  Vazio, trata como plano arbitrário.
- **Unidade da distância** — só rotula eixos e alcance (por exemplo, `m`).
- **Nome** — título do objeto nos cartões e gráficos adiante.
- **Borda** — o que fazer quando nenhuma borda é ligada na porta: `nenhuma`, ou
  `casco convexo dos pontos`. Pontos colineares não formam casco, e o bloco diz.
]---", r"---[
Um objeto espacial (`spatial/points`).
]---", r"---[
tr_flow(reg) |>
  tr_add("tab", "data/example") |>
  tr_add("pontos", "spatial/coordinates", x = "leste", y = "norte",
         variavel = "milho_kg_ha", crs = "31982", unidade = "m", from = "tab")
]---", r"---[
`spatial/example` para um conjunto pronto; `spatial/read_points` para ler os
pontos de um arquivo vetorial; `spatial/boundary` para a borda.
]---"))
  )
}
