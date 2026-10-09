# As declarações dos blocos de entrada de arquivo vetorial.
#
# Dois verbos, dois blocos, uma saída cada: ler PONTOS devolve `data/table` e
# segue pelo `spatial/coordinates`; ler POLÍGONOS devolve a borda. Um bloco só,
# com param de conteúdo e duas portas de saída, deixaria uma porta vazia
# conforme o param — pior de ler no canvas e nos adaptadores.

.tr_spatial_nos_entrada <- function() {
  T_ <- trama::tr_param_text
  list(
    trama::tr_node("spatial/read_points", fn = tr_spatial_read_points,
      label = "Ler pontos de arquivo", category = "espacial_fonte",
      icon = trama::tr_icon("file-down"),
      description = "Lê um arquivo vetorial de pontos (shapefile zipado, GeoJSON, GeoPackage, KML) como tabela, com as coordenadas em duas colunas.",
      outputs = list(out = "data/table"),
      params = list(
        caminho = T_("", label = "Arquivo"),
        camada = T_("", label = "Camada"),
        crs_saida = T_("", label = "Reprojetar para (EPSG)"),
        nomes_coords = T_("x,y", label = "Nomes das coordenadas")),
      pressupostos = .tr_spatial_press_coords(),
      referencias = list(.tr_spatial_refs()$cressie),
      help = .tr_spatial_ajuda(r"---[
Lê um arquivo vetorial de **pontos** e devolve uma tabela: os atributos do
arquivo mais duas colunas com as coordenadas. Daqui o caminho segue pelo
`spatial/coordinates`, que é onde se diz qual coluna é a variável.

Um shapefile chega quase sempre como **zip** (`.shp` + `.shx` + `.dbf` +
`.prj`), e o bloco abre o zip direto, sem descompactar, lendo a projeção do
`.prj`. Também lê GeoJSON, GeoPackage e KML. Se o zip tiver o shapefile numa
subpasta, não há nada a fazer: o bloco acha. Se tiver mais de uma camada, ele
pede que você escolha e lista as que encontrou.

**GeoJSON é latitude e longitude por especificação**, e o `spatial/coordinates`
recusa grau, porque um grau de longitude não é uma distância fixa — vale cerca
de 111 km no equador e menos conforme a latitude sobe. Por isso existe
**Reprojetar para**: preencha com o EPSG projetado da sua região (por exemplo
31982, UTM 22S) e o arquivo entra já em metro.
]---", r"---[
- **Arquivo** — caminho do `.zip`, `.shp`, `.geojson`, `.gpkg` ou `.kml`.
- **Camada** — só é preciso quando o arquivo tem mais de uma; o erro lista as
  que há.
- **Reprojetar para (EPSG)** — vazio lê como está.
- **Nomes das coordenadas** — dois nomes separados por vírgula. Se o arquivo já
  tiver coluna com esse nome, o bloco recusa em vez de sobrescrever o atributo.
]---", r"---[
Uma tabela (`data/table`) com os atributos do arquivo e as duas colunas de
coordenada.
]---", r"---[
tr_flow(reg) |>
  tr_add("ler", "spatial/read_points", caminho = "sedes.zip", crs_saida = "31982") |>
  tr_add("pontos", "spatial/coordinates", x = "x", y = "y",
         variavel = "milho_kg_ha", crs = "31982", from = "ler")
]---", r"---[
`spatial/boundary` para o contorno da área; `data/read` para tabela sem
geometria.
]---")),

    trama::tr_node("spatial/boundary", fn = tr_spatial_boundary,
      label = "Ler borda de arquivo", category = "espacial_preparar",
      icon = trama::tr_icon("shapes"),
      description = "Lê um arquivo vetorial de polígonos como a borda da área de estudo, dissolvida e reprojetada.",
      outputs = list(out = "spatial/boundary"),
      params = list(
        caminho = T_("", label = "Arquivo"),
        camada = T_("", label = "Camada"),
        crs_saida = T_("", label = "Reprojetar para (EPSG)")),
      referencias = list(.tr_spatial_refs()$cressie),
      help = .tr_spatial_ajuda(r"---[
Lê um arquivo vetorial de **polígonos** e devolve a borda da área de estudo, que
entra na porta **Borda** do `spatial/coordinates`. A borda faz três coisas
adiante: recorta a grade da krigagem, desenha o contorno no gráfico exploratório
e dispara a guarda que pega borda em escala, projeção ou lugar errado.

Feições múltiplas são **dissolvidas**, e fica o contorno de maior área — uma
malha estadual costuma vir como vários polígonos, por causa das ilhas. Anel
interno (buraco, enclave) é descartado, e a nota do card diz quantos: o tipo
guarda um contorno só.

A borda **é reprojetada** para a projeção dos pontos quando as duas diferem, e a
nota diz de qual para qual. Borda em grau com pontos em metro é reprojetada, não
recusada: a malha do IBGE vem em grau, e a recusa de grau vale para os pontos,
cuja distância o variograma mede, não para o recorte.
]---", r"---[
- **Arquivo** — caminho do `.zip`, `.shp`, `.geojson`, `.gpkg` ou `.kml`.
- **Camada** — só é preciso quando o arquivo tem mais de uma.
- **Reprojetar para (EPSG)** — vazio lê como está; a projeção dos pontos ainda
  será aplicada quando a borda for ligada a eles.
]---", r"---[
Uma borda (`spatial/boundary`), com a área, o número de vértices e a fonte.
]---", r"---[
tr_flow(reg) |>
  tr_add("borda", "spatial/boundary", caminho = "pr_uf.zip") |>
  tr_add("ler", "spatial/read_points", caminho = "sedes.zip") |>
  tr_add("pontos", "spatial/coordinates", x = "x", y = "y",
         variavel = "milho_kg_ha", crs = "31982", from = c("ler", "borda"))
]---", r"---[
`spatial/read_points` para os pontos; o param **Borda** do
`spatial/coordinates` para o casco convexo, que não precisa de arquivo.
]---"))
  )
}
