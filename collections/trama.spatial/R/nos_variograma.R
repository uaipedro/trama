# As declarações da aba Variograma.

.tr_spatial_nos_variograma <- function() {
  E <- trama::tr_param_enum; N <- trama::tr_param_num; I <- trama::tr_param_int
  PT <- "spatial/points"; VG <- "spatial/variogram"
  list(
    trama::tr_node("spatial/variogram", fn = tr_spatial_variogram, label = "Variograma",
      category = "espacial_variograma", icon = trama::tr_icon("chart-scatter"),
      description = "Variograma empírico, clássico ou robusto, omnidirecional ou numa direção.",
      inputs = list(pontos = PT), outputs = list(out = VG),
      params = list(
        estimador = E("classico", c("classico", "robusto"), label = "Estimador"),
        n_classes = I(15L, min = 3, max = 100, label = "Classes"),
        dist_max = N(NA, min = 0, label = "Distância máxima"),
        tendencia = E("constante", c("constante", "1a ordem", "2a ordem", "covariavel"),
                      label = "Tendência removida"),
        direcao = N(NA, min = 0, max = 360, label = "Direção (graus do Norte)"),
        tolerancia = N(22.5, min = 1, max = 90, label = "Tolerância angular"),
        pares_min = I(30L, min = 1, label = "Mínimo de pares")),
      pressupostos = .tr_spatial_press_variograma(),
      referencias = list(.tr_spatial_refs()$matheron, .tr_spatial_refs()$cressie_hawkins,
                         .tr_spatial_refs()$oliver, .tr_spatial_refs()$pebesma),
      help = .tr_spatial_ajuda(r"---[
A semivariância média entre pares de pontos, por classe de distância. É a
medida de dependência espacial: se o variograma sobe com a distância e
estabiliza, há estrutura espacial, e o alcance diz até onde ela vai.

Um variograma plano desde a primeira classe não quer dizer "sem estrutura":
pode ser escala errada, poucos pares, ou tendência de larga escala não removida.
As distâncias estão na unidade das coordenadas (metros, nos exemplos).
]---", r"---[
- **Estimador** — clássico é o de Matheron (1963), a média das diferenças ao
  quadrado. Robusto é o de Cressie-Hawkins (1980), que resiste a discrepante:
  use quando a nuvem do variograma mostrar valores altos isolados.
- **Classes** — em quantas faixas de distância, de larguras iguais, os pares são
  agrupados.
- **Distância máxima** — vazio usa a diagonal da área dividida por três. Além da
  metade da maior distância, sobram pares de menos para a média significar algo.
- **Tendência removida** — o variograma é calculado nos resíduos. Tendência de
  larga escala não removida faz o variograma subir sem parar e imitar ausência
  de patamar.
- **Direção** — em graus no sentido horário a partir do **Norte** (0 = Norte,
  90 = Leste), a convenção da bússola. Vazio calcula omnidirecional. Comparar
  duas direções é como se enxerga anisotropia.
- **Tolerância angular** — meia-abertura do setor, em graus.
- **Mínimo de pares** — classe com menos pares que isto sai, e a nota diz
  quantas saíram.
]---", r"---[
Um variograma empírico (`spatial/variogram`). O adaptador para `data/table` dá
uma linha por classe, com `u` (distância média), `gamma` (semivariância) e `np`
(pares).
]---", r"---[
tr_flow(reg) |>
  tr_add("p", "spatial/example", dataset = "milho_pr") |>
  tr_add("v", "spatial/variogram", dist_max = 250000, n_classes = 12, from = "p")
]---", r"---[
`spatial/variogram_fit` para ajustar o modelo teórico; `spatial/explore` para
ver a tendência antes.
]---"))
  )
}
