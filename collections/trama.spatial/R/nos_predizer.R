# As declarações da aba Predizer.

.tr_spatial_nos_predizer <- function() {
  E <- trama::tr_param_enum; N <- trama::tr_param_num; I <- trama::tr_param_int
  list(
    trama::tr_node("spatial/kriging", fn = tr_spatial_kriging, label = "Krigagem",
      category = "espacial_predizer", icon = trama::tr_icon("grid-3x3"),
      description = "Interpola a variável numa grade recortada na borda, com o erro-padrão de cada célula.",
      inputs = list(pontos = trama::tr_port("spatial/points", required = FALSE),
                    modelo = "spatial/model"),
      outputs = list(out = "spatial/surface"),
      params = list(
        tipo = E("ordinaria", .TR_SPATIAL_TIPOS_KRIG, label = "Tipo"),
        media = trama::tr_when(N(NA, label = "Média conhecida"), tipo = "simples"),
        resolucao = I(60L, min = 5, max = 500, label = "Resolução"),
        vizinhos_max = N(NA, min = 1, label = "Vizinhos"),
        dist_max = N(NA, min = 0, label = "Raio")),
      pressupostos = .tr_spatial_press_krigagem(),
      referencias = list(.tr_spatial_refs()$isaaks, .tr_spatial_refs()$cressie,
                         .tr_spatial_refs()$pebesma),
      help = .tr_spatial_ajuda(r"---[
A krigagem interpola a variável onde ela não foi medida, usando o modelo de
dependência espacial ajustado: cada ponto vizinho pesa conforme a distância e
conforme o que o variograma diz sobre essa distância. É interpolador exato —
numa coordenada amostral devolve o valor observado — e, diferente de qualquer
outro interpolador, **devolve o erro-padrão de cada célula**. O mapa do
erro-padrão é o par honesto do mapa do predito: mostra onde a predição vale
pouco. Use `spatial/map` para ver os dois.

A entrada **pontos** é opcional: o modelo já carrega os pontos com que foi
ajustado, e a krigagem usa esses. Conecte-a só se quiser ver o encadeamento
explícito; se os pontos conectados não forem os mesmos dados do modelo, a
krigagem para com erro em vez de interpolar outro conjunto.
]---", r"---[
- **Tipo** — ordinária estima a média a partir dos dados e é o padrão.
  Simples supõe a média da população conhecida e pede que você a informe; é a
  escolha certa só quando a média vem de fora dos dados.
- **Média conhecida** — só na simples. Usar a média amostral aqui não é
  krigagem simples: é fingir que se conhece o que se estimou.
- **Resolução** — pontos no lado maior da grade. A grade cobre os pontos e a
  borda, e é recortada na borda, quando há. O número de células cresce com o
  **quadrado** da resolução e o custo cresce com ele: algumas centenas de
  pontos em resolução 500 levam dezenas de segundos com vizinhança global.
  O remédio é **Vizinhos** (por exemplo 30), que reduz esse tempo várias vezes.
  A nota da superfície diz quantas células saíram.
- **Vizinhos / Raio** — vazios krigam com todos os pontos. Limitar acelera e
  deixa o resultado mais local, mas vizinhança pequena demais produz emenda
  visível entre regiões. O raio está na unidade das coordenadas (metros, nos
  exemplos); célula sem nenhum ponto dentro do raio fica sem predição, e a nota
  diz quantas.
]---", r"---[
Uma superfície predita (`spatial/surface`). O adaptador para `data/table` dá uma
linha por célula, com as coordenadas, `predito`, `variancia` e `erro_padrao`.
]---", r"---[
tr_flow(reg) |>
  tr_add("p", "spatial/example", dataset = "milho_pr") |>
  tr_add("v", "spatial/variogram", dist_max = 250000, from = "p") |>
  tr_add("m", "spatial/variogram_fit", familia = "esferico", from = "v") |>
  tr_add("k", "spatial/kriging", resolucao = 40L, from = "m")
]---", r"---[
`spatial/variogram_fit` para o modelo que esta krigagem consome; `spatial/map`
para ver o predito e o erro-padrão.
]---")),

    trama::tr_node("spatial/map", fn = tr_spatial_map, label = "Mapa da superfície",
      category = "espacial_predizer", icon = trama::tr_icon("map"),
      description = "Desenha a superfície krigada, ou o erro-padrão dela, sobre a borda e os pontos.",
      inputs = list(superficie = "spatial/surface"), outputs = list(out = "view/plot"),
      params = .tr_spatial_props(
        mostrar = E("predito", .TR_SPATIAL_MOSTRAR, label = "Mostrar"),
        isolinhas = trama::tr_param_bool(FALSE, label = "Isolinhas"),
        pontos = trama::tr_param_bool(TRUE, label = "Pontos amostrais")),
      help = paste0(.tr_spatial_ajuda(r"---[
Desenha a superfície da krigagem. **O mapa do erro-padrão é o par honesto do
mapa do predito**: o predito é liso e convincente em qualquer lugar, e só o
erro-padrão mostra onde ele vale pouco, longe dos pontos e perto das bordas.
Por isso os dois saem do mesmo bloco, escolhidos por **Mostrar**: o mapa honesto
nunca é mais difícil de pedir que o bonito. Leia um com o outro, e não publique
o predito sozinho.

Os eixos têm sempre a mesma escala (`coord_equal`). Célula sem predição (nenhum
ponto dentro do **Raio** da krigagem) sai em cinza, e não em branco, para que não
se confunda com a borda.
]---", r"---[
- **Mostrar** — `predito` (padrão) ou `erro-padrao`.
- **Isolinhas** — curvas de nível sobre a superfície.
- **Pontos amostrais** — as localizações medidas, por cima; mostram de onde
  vem a informação.
]---", r"---[
Um gráfico (`view/plot`): a grade em `geom_raster`, a borda do domínio e,
quando ligados, as isolinhas e os pontos.
]---", r"---[
tr_flow(reg) |>
  tr_add("p", "spatial/example", dataset = "milho_se") |>
  tr_add("v", "spatial/variogram", from = "p") |>
  tr_add("m", "spatial/variogram_fit", from = "v") |>
  tr_add("k", "spatial/kriging", from = c("p", "m")) |>
  tr_add("mapa", "spatial/map", mostrar = "erro-padrao", from = "k")
]---", r"---[
`spatial/kriging`, que produz a superfície.
]---"), "
", trama.view::tr_view_help_appearance()))
  )
}
