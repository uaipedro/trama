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
]---")),

    trama::tr_node("spatial/variogram_fit", fn = tr_spatial_variogram_fit,
      label = "Ajustar modelo", category = "espacial_variograma",
      icon = trama::tr_icon("spline"),
      description = "Ajusta um modelo teórico ao variograma empírico e reporta pepita, contribuição e alcance.",
      inputs = list(variograma = VG), outputs = list(out = "spatial/model"),
      params = list(
        familia = E("esferico", c("esferico", "exponencial", "gaussiano", "matern"),
                    label = "Família"),
        metodo = E("WLS-Cressie", c("WLS-Cressie", "WLS-np", "OLS"), label = "Método"),
        pepita_fixa = trama::tr_param_bool(FALSE, label = "Pepita fixa"),
        pepita_inicial = N(NA, min = 0, label = "Pepita inicial"),
        contribuicao_inicial = N(NA, min = 0, label = "Contribuição inicial"),
        alcance_inicial = N(NA, min = 0, label = "Alcance inicial"),
        kappa = N(0.5, min = 0.1, max = 10, label = "Kappa (Matérn)")),
      pressupostos = .tr_spatial_press_ajuste(),
      referencias = list(.tr_spatial_refs()$cressie, .tr_spatial_refs()$oliver,
                         .tr_spatial_refs()$pebesma),
      help = .tr_spatial_ajuda(r"---[
Ajusta uma curva teórica aos pontos do variograma empírico. O que sai daqui é o
que a krigagem usa: sem modelo ajustado não há interpolação.

Três números resumem a estrutura espacial: a **pepita** (variância a distância
zero: erro de medida e variação em escala menor que a malha), a **contribuição**
(quanto a estrutura espacial acrescenta) e o **alcance** (até onde ela vai).

O **alcance prático** é o que se lê no gráfico, e não é o parâmetro do modelo.
No esférico são iguais, porque o modelo atinge o patamar exatamente nesse ponto.
No exponencial o prático é cerca de três vezes o parâmetro; no gaussiano, cerca
de 1,73 vez. O bloco reporta os dois.

O bloco **não escolhe a família por você**: comparar modelos é trabalho de
validação cruzada.
]---", r"---[
- **Família** — esférico atinge o patamar; exponencial e gaussiano só se
  aproximam. O gaussiano supõe um fenômeno muito suave perto da origem e costuma
  precisar de pepita.
- **Método** — WLS-Cressie pesa cada classe por `N/γ²`, dando peso às classes
  curtas, que são as que a krigagem usa; é o padrão da literatura e desta
  coleção. WLS-np pesa só pelo número de pares. OLS não pesa.
  O `sqr` que o bloco reporta é a soma de quadrados ponderada com os pesos do
  método escolhido: só se compara entre ajustes do mesmo método e do mesmo
  variograma empírico.
- **Pepita fixa** — mantém a pepita no valor inicial em vez de estimá-la.
- **Valores iniciais** — o ajuste é sensível ao chute, então o bloco **tenta
  várias partidas**, sempre as mesmas (mesma entrada, mesmo modelo): pepita = γ
  da primeira classe ou zero; contribuição = γ máximo menos a pepita; alcance =
  a distância máxima dividida por 3, 6 e 2. Fica o ajuste válido de menor `sqr`.
  Ajuste que sai singular, não converge, tem sinal errado ou alcance prático fora da
  escala dos dados é descartado; se nenhuma partida serve, o bloco dá erro em vez
  de devolver um modelo ruim. Valor que você informa vale como está e não entra
  na grade. Se o único defeito for pepita negativa, use **Pepita fixa** com
  pepita inicial 0.
- **Kappa** — só para Matérn: 0,5 reproduz o exponencial, valores altos
  aproximam o gaussiano.
]---", r"---[
Um modelo ajustado (`spatial/model`). O adaptador para `data/table` dá uma linha
por parâmetro numérico, pronta para comparar duas famílias com `data/bind_rows`.
]---", r"---[
tr_flow(reg) |>
  tr_add("p", "spatial/example", dataset = "milho_pr") |>
  tr_add("v", "spatial/variogram", dist_max = 250000, from = "p") |>
  tr_add("m", "spatial/variogram_fit", familia = "esferico", from = "v")
]---", r"---[
`spatial/kriging` para interpolar com este modelo.
]---"))
  )
}
