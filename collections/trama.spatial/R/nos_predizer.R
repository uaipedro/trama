# As declarações da aba Predizer.

.tr_spatial_nos_predizer <- function() {
  E <- trama::tr_param_enum; N <- trama::tr_param_num; I <- trama::tr_param_int
  list(
    trama::tr_node("spatial/kriging", fn = tr_spatial_kriging, label = "Krigagem",
      category = "espacial_predizer", icon = trama::tr_icon("grid-3x3"),
      description = "Interpola a variável numa grade recortada na borda, com o erro-padrão de cada célula.",
      inputs = list(pontos = "spatial/points", modelo = "spatial/model"),
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
  tr_add("k", "spatial/kriging", resolucao = 40L, from = c("p", "m"))
]---", r"---[
`spatial/variogram_fit` para o modelo que esta krigagem consome; `spatial/map`
para ver o predito e o erro-padrão.
]---"))
  )
}
