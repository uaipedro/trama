# As declarações da aba Variograma.

# O nó não expõe `.simular`: o registro exige que todo argumento da função tenha
# param ou porta, e a injeção do simulador existe só para o teste do envelope
# não depender do otimizador. Mesmo padrão de `.tr_spatial_coordinates_no`.
.tr_spatial_anisotropy_no <- function(pontos, direcoes = "0,45,90,135",
                                      estimador = "classico", dist_max = NA,
                                      n_classes = 10L, tolerancia = 22.5,
                                      tendencia = "constante", envelope = FALSE,
                                      n_sim = 19L, semente = NA,
                                      pares_min = 30L) {
  tr_spatial_anisotropy(pontos, direcoes = direcoes, estimador = estimador,
                        dist_max = dist_max, n_classes = n_classes,
                        tolerancia = tolerancia, tendencia = tendencia,
                        envelope = envelope, n_sim = n_sim, semente = semente,
                        pares_min = pares_min)
}

.tr_spatial_nos_variograma <- function() {
  E <- trama::tr_param_enum; N <- trama::tr_param_num; I <- trama::tr_param_int
  PT <- "spatial/points"; VG <- "spatial/variogram"
  list(
    trama::tr_node("spatial/explore", fn = tr_spatial_explore, label = "Explorar",
      category = "espacial_explorar", icon = trama::tr_icon("layout-grid"),
      description = "Quatro vistas da variável: postplot por quartil na borda, contra cada coordenada e a distribuição.",
      inputs = list(pontos = PT), outputs = list(out = "view/plot"),
      params = .tr_spatial_props(
        vista = E("completo", .TR_SPATIAL_VISTAS, label = "Vista")),
      help = paste0(.tr_spatial_ajuda(r"---[
O que olhar antes de medir dependência espacial. O painel completo reúne quatro
vistas: o **mapa dos pontos** pintados por quartil da variável, dentro da borda;
a **variável contra a coordenada X** e **contra a Y**; e a **distribuição**.

Os gráficos contra as coordenadas são onde tendência de larga escala aparece:
uma nuvem que sobe ou desce de um lado a outro do domínio é tendência, e o
variograma não a distingue de dependência espacial. Se ela está ali, remova-a
no bloco `spatial/variogram` (param **Tendência removida**) antes de ajustar.

No mapa o quartil vai em **tamanho e cor ao mesmo tempo**, para que o gráfico
leia em preto e branco e para quem não distingue as cores. Os eixos do mapa
têm sempre a mesma escala (`coord_equal`): proporção diferente entre X e Y
distorceria a geometria, que é o que o mapa existe para mostrar. Uma variável
sem variação sai com uma só classe, e um empate de quartis funde classes.
]---", r"---[
- **Vista** — `completo` (padrão), ou só um: `mapa`, `x`, `y`, `histograma`.
  Os cosméticos abaixo valem para o painel inteiro.
]---", r"---[
Um gráfico (`view/plot`). No painel completo é uma composição de quatro
gráficos (um `patchwork`, que continua sendo um ggplot).
]---", r"---[
tr_flow(reg) |>
  tr_add("p", "spatial/example", dataset = "milho_pr") |>
  tr_add("e", "spatial/explore", vista = "completo", from = "p")
]---", r"---[
`spatial/variogram` para medir a dependência, com a tendência removida se o
painel a mostrou.
]---"), "
", trama.view::tr_view_help_appearance())),

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
        kappa = N(0.5, min = 0.1, max = 10, label = "Kappa (Matérn)"),
        razao = N(1, min = 1, label = "Razão de anisotropia"),
        angulo = N(0, min = 0, max = 179.999, label = "Ângulo do eixo maior")),
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
- **Razão de anisotropia** — maior eixo dividido pelo menor. 1 é isotrópico, e
  não muda nada em relação ao comportamento anterior. Razão menor que 1 é
  recusada: para pôr o eixo maior na outra direção, gire o ângulo 90 graus.
- **Ângulo do eixo maior** — graus, horário a partir do Norte, de 0 a 180.
  Aponta a direção de maior continuidade, a de alcance mais longo.

Os dois números de anisotropia **não são estimados aqui**: você os lê no card do
bloco `spatial/anisotropy` e os digita. O motor de ajuste não usa a direção dos
pares — ele preserva a anisotropia que recebe em vez de ajustá-la —, e as três
maneiras de estimá-la que testamos devolvem razão perto de 3 até para campos
isotrópicos.
]---", r"---[
Um modelo ajustado (`spatial/model`). O adaptador para `data/table` dá uma linha
por parâmetro numérico, pronta para comparar duas famílias com `data/bind_rows`.

A linha `grau_dependencia` é a **dependência relativa**, pepita dividida pelo
patamar (Cambardella et al., 1994): **quanto menor, mais forte a dependência
espacial**. Perto de 0 quase toda a variância é estrutura espacial; perto de 1
é quase tudo pepita, e o variograma é praticamente plano. O nome do campo não
muda, mas o sentido é este, e é o inverso do que o nome sugere.
]---", r"---[
tr_flow(reg) |>
  tr_add("p", "spatial/example", dataset = "milho_pr") |>
  tr_add("v", "spatial/variogram", dist_max = 250000, from = "p") |>
  tr_add("m", "spatial/variogram_fit", familia = "esferico", from = "v")
]---", r"---[
`spatial/kriging` para interpolar com este modelo.
]---")),

    trama::tr_node("spatial/anisotropy", fn = .tr_spatial_anisotropy_no,
      label = "Anisotropia", category = "espacial_variograma",
      icon = trama::tr_icon("compass"),
      description = "Variograma em várias direções de uma vez, para ver se a dependência espacial tem alcance diferente conforme a direção.",
      inputs = list(pontos = "spatial/points"),
      outputs = list(out = "spatial/anisotropy"),
      params = list(
        direcoes = trama::tr_param_text("0,45,90,135", label = "Direções"),
        estimador = E("classico", .TR_SPATIAL_ESTIMADORES, label = "Estimador"),
        dist_max = N(NA, min = 0, label = "Distância máxima"),
        n_classes = I(10L, min = 3, max = 50, label = "Classes"),
        tolerancia = N(22.5, min = 0.1, max = 90, label = "Tolerância angular"),
        tendencia = E("constante", .TR_SPATIAL_TENDENCIAS, label = "Tendência"),
        pares_min = I(30L, min = 1, label = "Mínimo de pares"),
        envelope = trama::tr_param_bool(FALSE, label = "Faixa de referência"),
        n_sim = trama::tr_when(I(19L, min = 5, max = 999, label = "Simulações"),
                               envelope = TRUE),
        semente = trama::tr_when(N(NA, label = "Semente"), envelope = TRUE)),
      pressupostos = .tr_spatial_press_variograma(),
      referencias = list(.tr_spatial_refs()$isaaks, .tr_spatial_refs()$oliver),
      help = .tr_spatial_ajuda(r"---[
Anisotropia é a dependência espacial ter **alcance diferente conforme a
direção**: a variável se parece consigo mesma por mais longe num rumo que no
outro. O variograma omnidirecional esconde isso, porque mistura todas as
direções numa curva só. Este bloco separa.

A leitura é **visual**: curvas que sobem junto e estabilizam no mesmo lugar
indicam isotropia; uma curva que estabiliza muito mais longe que as outras
indica o eixo de maior continuidade. O bloco **não faz teste de hipótese**, e
não devolve "razão de anisotropia".

Isso é decisão medida, não omissão. Três maneiras de estimar razão e ângulo
automaticamente foram testadas, e as três reprovaram; a menos ruim devolve
razão perto de 3 para campos **isotrópicos**, indistinguível do que devolve
para campos de razão 3 de verdade. Um número que não separa o caso do seu
contrário não ajuda ninguém. Olhe as curvas, decida, e digite a razão e o
ângulo no bloco `spatial/variogram_fit`.

Cada direção recebe só uma fração dos pares, então classes de menos ou
tolerância estreita deixam direções sem pares bastantes; a nota do card diz
quais ficaram de fora.

A **faixa de referência** ajuda a calibrar o olho: ela mostra onde as curvas
cairiam se a dependência fosse isotrópica, simulando campos isotrópicos nestes
mesmos pontos. A hipótese que ela representa é "isotrópico com esta estrutura",
e não "sem dependência nenhuma". **Não é teste**: medimos que a fração do
observado dentro da faixa dá cerca de 0,95 sob isotropia e 0,84 sob anisotropia
de razão 3, valores que se sobrepõem. O que separa é o padrão — curvas do eixo
maior e do menor escapando de forma sistemática —, não a contagem.
]---", r"---[
- **Direções** — graus separados por vírgula, de 0 a 180, no sentido horário a
  partir do Norte (a convenção da bússola). O variograma não distingue uma
  direção da oposta, então 0 e 180 seriam a mesma curva.
- **Estimador** — `classico` ou `robusto`, como no `spatial/variogram`.
- **Distância máxima**, **Classes**, **Mínimo de pares** — o mesmo do
  `spatial/variogram`, mas lembre que aqui os pares se dividem entre as
  direções.
- **Tolerância angular** — meia-abertura da janela de cada direção, em graus. A
  90 graus a janela cobre tudo e cada curva vira o omnidirecional.
- **Tendência** — removida antes, como no `spatial/variogram`.
- **Faixa de referência** — desenha, por direção, a faixa que curvas
  **isotrópicas** ocupariam nestes mesmos pontos. Curva que escapa da própria
  faixa, de forma sistemática, é o sinal de anisotropia.
- **Simulações** — quantos campos isotrópicos simular (padrão 19). Mais
  simulações dão faixa mais estável e card mais lento.
- **Semente** — fixa a simulação, para o card não mudar a cada execução.
]---", r"---[
Um objeto de anisotropia (`spatial/anisotropy`), com uma curva por direção.
]---", r"---[
tr_flow(reg) |>
  tr_add("pontos", "spatial/example", dataset = "milho_pr") |>
  tr_add("aniso", "spatial/anisotropy", direcoes = "0,45,90,135", from = "pontos")
]---", r"---[
`spatial/variogram` para uma direção só, ou nenhuma;
`spatial/variogram_fit`, que é onde a razão e o ângulo são informados.
]---"))
  )
}
