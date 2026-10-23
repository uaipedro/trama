# As declarações da aba Predizer.

.tr_spatial_nos_predizer <- function() {
  E <- trama::tr_param_enum; N <- trama::tr_param_num; I <- trama::tr_param_int
  list(
    trama::tr_node("spatial/kriging", fn = tr_spatial_kriging, label = "Krigagem",
      # version 2 na 0.2.0: o conjunto de PORTAS mudou (entrou `grade`), e
      # `collections/AGENTS.md` lista portas como gatilho de bump, junto de
      # resultado e de padrão de param. Nenhuma migração é devida: porta e param
      # novos são opcionais e documento antigo dá o mesmo resultado.
      version = 2L,
      category = "espacial_predizer", icon = trama::tr_icon("grid-3x3"),
      description = "Interpola a variável numa grade recortada na borda, com o erro-padrão de cada célula.",
      inputs = list(pontos = trama::tr_port("spatial/points", required = FALSE),
                    modelo = "spatial/model",
                    grade = trama::tr_port("data/table", required = FALSE)),
      outputs = list(out = "spatial/surface"),
      params = list(
        tipo = E("ordinaria", .TR_SPATIAL_TIPOS_KRIG, label = "Tipo"),
        media = trama::tr_when(N(NA, label = "Média conhecida"), tipo = "simples"),
        tendencia = trama::tr_when(
          E("1a ordem", .TR_SPATIAL_TENDENCIAS_KRIG, label = "Tendência"),
          tipo = "universal"),
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
A **krigagem universal** entra pelo param Tipo. Ela não supõe média constante:
estima, junto com a predição, uma tendência de larga escala — um plano (1ª
ordem), uma superfície quadrática (2ª ordem) ou uma covariável conhecida em toda
célula (deriva externa). Use-a quando o variograma só estabiliza depois de
remover tendência: a tendência que você removeu ali e a que escolhe aqui são **a
mesma hipótese**, e as duas devem combinar. Variograma com tendência de 1ª ordem
e krigagem ordinária é incoerente — o modelo descreve o resíduo e a krigagem
prediz o total.

A tendência polinomial é ajustada em coordenada **centrada e padronizada**. Isso
não muda a conta (a krigagem com tendência é invariante a reparametrização
linear da base) e evita o mal condicionamento que coordenada UTM crua produz no
termo quadrático — medimos 3,7e-4 de diferença na 2ª ordem, e o `geoR` chega a
ficar singular.

A **deriva externa** (tendência por covariável) tem uma exigência que não dá
para contornar: a covariável precisa ser conhecida em **toda célula** onde se
prediz, e não só nos pontos amostrais. Por isso ela só roda com uma tabela
ligada na porta **Grade**, trazendo as coordenadas e a covariável. O bloco
recusa quando a covariável falta ou tem célula vazia, em vez de preencher por
conta própria: interpolar a covariável por dentro deixaria o erro-padrão do mapa
subestimado sem avisar, porque o erro dessa interpolação não entra na variância
de krigagem.
]---", r"---[
- **Tipo** — ordinária estima a média a partir dos dados e é o padrão.
  Simples supõe a média da população conhecida e pede que você a informe; é a
  escolha certa só quando a média vem de fora dos dados.
- **Tendência** — só na universal: `1a ordem` (plano), `2a ordem` (superfície
  quadrática) ou `covariavel` (deriva externa, que exige a grade com a
  covariável, pela porta Grade). Tendência constante não aparece aqui porque
  universal com tendência constante é a própria ordinária.
- **Grade** (porta) — uma tabela com as colunas de coordenada, que passa a ser
  a grade de predição. Ligada, ela **vence a Resolução**. É o único caminho da
  deriva externa, e serve também a quem quer predizer em pontos escolhidos em
  vez de numa grade regular. As colunas de coordenada precisam ter os mesmos
  nomes declarados no bloco Coordenadas.
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
  diz quantas. Se **nenhuma** célula for predita — raio fora da escala dos
  dados, como 1 em coordenadas UTM — o bloco para com erro em vez de devolver
  um mapa vazio, e a mensagem diz a que distância está o vizinho mais próximo.
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

    trama::tr_node("spatial/map", fn = tr_spatial_map, label = "Mapa da superfície", role = "leitura",
      category = "espacial_predizer", icon = trama::tr_icon("map"),
      description = "Desenha a superfície krigada, ou o erro-padrão dela, sobre a borda e os pontos.",
      inputs = list(superficie = "spatial/surface"), outputs = list(out = "view/plot"),
      params = .tr_spatial_props(
        mostrar = E("predito", .TR_SPATIAL_MOSTRAR, label = "Mostrar"),
        isolinhas = trama::tr_param_bool(FALSE, label = "Isolinhas"),
        pontos = trama::tr_param_bool(TRUE, label = "Pontos amostrais")),
      help = paste0(.tr_spatial_ajuda(r"---[
Quando a superfície vem de um indicador (`spatial/indicator`), o predito **é
probabilidade**, e o mapa diz isso: a legenda traz "Probabilidade de ..." com o
corte, em vez do nome da variável. Sem esse cuidado o mapa apresentaria
probabilidade com a cara de rendimento ou de teor.

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
", trama.view::tr_view_help_appearance())),

    trama::tr_node("spatial/validation", fn = tr_spatial_validation,
      label = "Validação cruzada", category = "espacial_predizer",
      icon = trama::tr_icon("check-check"),
      description = "Mede o modelo predizendo cada ponto sem ele mesmo: erro médio, RMSE, MSDR e correlação entre observado e predito.",
      inputs = list(pontos = "spatial/points", modelo = "spatial/model"),
      outputs = list(out = "spatial/validation"),
      params = list(
        metodo = E("leave-one-out", .TR_SPATIAL_METODOS_VALID, label = "Método"),
        dobras = trama::tr_when(I(10L, min = 2, label = "Dobras"),
                                metodo = "k dobras"),
        semente = trama::tr_when(N(NA, label = "Semente"), metodo = "k dobras"),
        vizinhos_max = N(NA, min = 1, label = "Vizinhos"),
        dist_max = N(NA, min = 0, label = "Raio")),
      pressupostos = .tr_spatial_press_ajuste(),
      referencias = list(.tr_spatial_refs()$isaaks, .tr_spatial_refs()$cressie),
      help = .tr_spatial_ajuda(r"---[
Um modelo de variograma que descreve bem o variograma empírico ainda pode
krigar mal. A validação cruzada mede isso: tira um ponto de cada vez, prediz o
lugar dele com os outros, e compara com o valor observado.

As quatro medidas, e o que cada uma pega:

- **Erro médio (ME)** — perto de zero é o que se espera; longe de zero indica
  viés sistemático, o mapa inteiro deslocado para cima ou para baixo.
- **RMSE** — o tamanho típico do erro, na unidade da variável. Serve para
  comparar modelos no MESMO conjunto de pontos.
- **MSDR** — a média de (resíduo dividido pelo erro-padrão) ao quadrado. É a
  única que olha o **mapa de erro-padrão**: perto de 1 ele está calibrado; muito
  acima de 1 o mapa é otimista, promete precisão que não tem; muito abaixo é
  pessimista. Como a krigagem entrega sempre dois mapas, essa é a medida que
  diz se o segundo presta.
- **Correlação** entre observado e predito — quanto da variação o modelo
  acompanha.

**Leave-one-out** deixa um ponto de fora por vez: usa o máximo da informação e
é determinístico. **K dobras** deixa um grupo de fora por vez, treinando com
menos pontos; o erro sai maior, e mais perto do que se espera de uma predição
em lugar de verdade não amostrado. A partição em dobras é sorteada, então fixe
a **semente** para o card não mudar a cada execução.
]---", r"---[
- **Método** — `leave-one-out` ou `k dobras`.
- **Dobras** — quantos grupos, no método de dobras. De 2 até o número de
  pontos; acima disso o bloco recusa e diz o máximo.
- **Semente** — fixa o sorteio das dobras.
- **Vizinhos**, **Raio** — a mesma vizinhança da krigagem. Validar com a
  vizinhança que você vai usar no mapa é o que torna a medida comparável.
]---", r"---[
Uma validação (`spatial/validation`): uma linha por ponto, com observado,
predito, variância, resíduo e o resíduo padronizado, mais as quatro métricas.
]---", r"---[
tr_flow(reg) |>
  tr_add("pontos", "spatial/example", dataset = "milho_pr") |>
  tr_add("v", "spatial/variogram", from = "pontos") |>
  tr_add("m", "spatial/variogram_fit", from = "v") |>
  tr_add("valid", "spatial/validation", from = c("pontos", "m"))
]---", r"---[
`spatial/kriging` para o mapa; `spatial/variogram_fit` para o modelo que esta
validação julga.
]---"))
  )
}
