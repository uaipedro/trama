# As declarações das abas Selecionar e Desenho.

.tr_sampling_nos_selecionar <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; N <- trama::tr_param_num; I <- trama::tr_param_int
  B <- trama::tr_param_bool
  T <- "data/table"; S <- "sampling/sample"
  PLANO <- function() trama::tr_port("sampling/plan", required = FALSE)
  ajuda_valor <- r"---[
Uma amostra (`sampling/sample`): as linhas sorteadas com o desenho junto. Ligada
num bloco da `data` ou da `view`, vira a tabela das linhas com `peso_amostral`
(quantas unidades da população cada linha representa) e `prob_inclusao`.
Ligada em `sampling/mean`, `sampling/total`, `sampling/proportion` ou
`sampling/ratio`, leva o desenho para a estimativa sem nada a redigitar.

O card mostra o n, a população, a fração, as unidades primárias por estrato e
a faixa dos pesos; a vista `tabela` mostra as linhas.
]---"
  exemplo <- function(no, params, dataset = "fazendas") sprintf(r"---[
tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "%s") |>
  tr_add("amostra", "%s", %s, from = "pop") |>
  tr_add("media", "sampling/mean", variavel = "%s", from = "amostra")
]---", dataset, no, params, if (dataset == "escolas") "nota" else "producao_t")
  list(
    trama::tr_node("sampling/srs", fn = tr_sampling_srs, label = "Aleatória simples",
      category = "amostra_selecionar", icon = trama::tr_icon("shuffle"), stochastic = TRUE,
      description = "Sorteia n unidades do cadastro, todas com a mesma chance (AAS).",
      inputs = list(populacao = T, plano = PLANO()), outputs = list(out = S),
      params = list(
        n = I(0L, min = 0L, label = "n"),
        fracao = N(0, min = 0, max = 1, step = 0.01, label = "Fração (se n = 0)"),
        reposicao = B(FALSE, label = "Com reposição")),
      help = .tr_sampling_ajuda(r"---[
A amostra aleatória simples (AAS): n unidades sorteadas do cadastro, todas com
a mesma probabilidade n/N. É a referência de todo o resto — o deff das outras
estimativas é medido contra ela.

O n vem de **n**, da **fração** (quando n é 0) ou do **plano** ligado na porta
`plano` (de `sampling/size_mean` ou `sampling/size_proportion`), que vence os
campos.

**Com reposição** a mesma unidade pode sair duas vezes, e a variância perde a
correção de população finita. Serve para ensinar e para bootstrap; em pesquisa
de verdade, sem reposição.

Todo peso é N/n.
]---", r"---[
- **n** — quantas unidades sortear.
- **Fração** — a fração da população, usada quando n é 0.
- **Com reposição** — sortear com reposição.
]---", ajuda_valor, exemplo("sampling/srs", "n = 200L"), r"---[
`sampling/systematic`; `sampling/stratified` quando há estratos; `sampling/size_mean`
para o n; `sampling/simulate` para ver o desenho em ação.
]---", semente = TRUE)),

    trama::tr_node("sampling/systematic", fn = tr_sampling_systematic, label = "Sistemática",
      category = "amostra_selecionar", icon = trama::tr_icon("list-ordered"), stochastic = TRUE,
      description = "Sorteia um começo e toma uma unidade a cada N/n, ao longo do cadastro.",
      inputs = list(populacao = T, plano = PLANO()), outputs = list(out = S),
      params = list(
        n = I(0L, min = 0L, label = "n"),
        fracao = N(0, min = 0, max = 1, step = 0.01, label = "Fração (se n = 0)"),
        ordenar = P("cols", "", label = "Ordenar por", example = "area_ha")),
      help = .tr_sampling_ajuda(r"---[
A amostra sistemática: com intervalo k = N/n, sorteia um começo entre 0 e k e
toma as unidades nas posições começo, começo + k, começo + 2k… É a amostra da
lista telefônica, da linha de produção, das árvores ao longo do talhão.

O intervalo é FRACIONÁRIO: com N/n não inteiro, sai exatamente n unidades.

**Ordenar por** uma coluna antes é estratificação implícita de graça: com as
fazendas em ordem de área, a amostra cobre pequenas, médias e grandes na
proporção certa, e a variância real cai. O estimador de variância, porém, é o
da AAS — a sistemática não tem estimador próprio sem suposição —, então o
erro padrão do card tende a ser CONSERVADOR quando a ordenação ajuda. Cuidado
com cadastro periódico (a mesma posição da semana a cada 7 linhas): aí a
sistemática engana, e a variância real SOBE.
]---", r"---[
- **n**, **Fração** — como em `sampling/srs`.
- **Ordenar por** — coluna pela qual ordenar o cadastro antes (em branco: a
  ordem em que ele veio).
]---", ajuda_valor, exemplo("sampling/systematic", "n = 200L, ordenar = \"area_ha\""), r"---[
`sampling/srs`; `sampling/stratified`; `sampling/simulate` para ver o ganho da
ordenação.
]---", semente = TRUE)),

    trama::tr_node("sampling/stratified", fn = tr_sampling_stratified, label = "Estratificada",
      category = "amostra_selecionar", icon = trama::tr_icon("layers-2"), stochastic = TRUE,
      description = "Divide o cadastro em estratos e sorteia uma AAS dentro de cada um.",
      inputs = list(populacao = T, plano = PLANO()), outputs = list(out = S),
      params = list(
        estrato = P("cols", "", label = "Estrato", example = "regiao"),
        n = I(0L, min = 0L, label = "n total"),
        alocacao = E("proporcional", c("proporcional", "igual", "neyman"), label = "Alocação"),
        variavel_auxiliar = P("cols", "", label = "Variável do Neyman", example = "producao_t")),
      help = .tr_sampling_ajuda(r"---[
A amostra estratificada: o cadastro é dividido em estratos (regiões, redes,
faixas de tamanho) e uma AAS independente é sorteada em cada um. Ganha da AAS
quando os estratos diferem entre si — a variação ENTRE estratos sai do erro —,
e garante que todo estrato apareça na amostra.

O n de cada estrato vem do **plano** ligado (de `sampling/size_stratified`,
casando os estratos pelo nome) ou da **alocação** do n total:

- **proporcional** — n_h ∝ N_h;
- **igual** — o mesmo n em cada estrato;
- **neyman** — n_h ∝ N_h · S_h, com S_h o desvio da **variável do Neyman** no
  cadastro (a produção do censo anterior, a área).

Nenhum estrato recebe menos que 2 nem mais que a sua população. O peso de cada
unidade é N_h/n_h.
]---", r"---[
- **Estrato** — coluna do estrato no cadastro.
- **n total** — o n a alocar (sem plano).
- **Alocação** — `proporcional`, `igual` ou `neyman` (sem plano).
- **Variável do Neyman** — coluna numérica do cadastro; só no Neyman.
]---", ajuda_valor, r"---[
tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("estratos", "sampling/example", dataset = "estratos_fazendas") |>
  tr_add("plano", "sampling/size_stratified", estrato = "regiao", tamanho = "N",
         desvio = "desvio_producao", erro = 60, from = "estratos") |>
  tr_add("amostra", "sampling/stratified", estrato = "regiao", from = "pop") |>
  tr_link("plano", "amostra:plano") |>
  tr_add("media", "sampling/mean", variavel = "producao_t", from = "amostra")
]---", r"---[
`sampling/size_stratified` para o plano; `sampling/poststratify` quando o
estrato só é conhecido depois; `sampling/simulate` para ver o ganho.
]---", semente = TRUE)),

    trama::tr_node("sampling/pps", fn = tr_sampling_pps, label = "PPS",
      category = "amostra_selecionar", icon = trama::tr_icon("scale"), stochastic = TRUE,
      description = "Sorteia com probabilidade proporcional a uma medida de tamanho (PPS sistemática).",
      inputs = list(populacao = T, plano = PLANO()), outputs = list(out = S),
      params = list(
        tamanho = P("cols", "", label = "Medida de tamanho", example = "area_ha"),
        n = I(0L, min = 0L, label = "n")),
      help = .tr_sampling_ajuda(r"---[
A amostra com probabilidade proporcional ao tamanho (PPS): a unidade com o
dobro da área tem o dobro da chance. Quando a variável de interesse é
proporcional ao tamanho (a produção de uma fazenda cresce com a área), o
estimador do total de Horvitz-Thompson quase não varia: cada unidade sorteada
"representa" um pedaço da população do tamanho dela. Nas `fazendas`, o deff do
total fica perto de 0,1.

O sorteio é o sistemático de Madow na ordem do cadastro: as probabilidades
π_i = n · x_i / X são acumuladas, e um começo aleatório com passo 1 escolhe as
unidades. Unidade com π_i ≥ 1 entra com certeza, e a conta é refeita com as
outras. Ordenar o cadastro antes (um `data/arrange`) soma estratificação
implícita.

O peso é 1/π_i. A variância é a com reposição (Hansen-Hurwitz), levemente
conservadora; as unidades de certeza não contribuem.
]---", r"---[
- **Medida de tamanho** — coluna numérica positiva, conhecida para TODO o
  cadastro (área, número de empregados, população do município).
- **n** — quantas unidades sortear (ou o **plano** ligado).
]---", ajuda_valor, exemplo("sampling/pps", "tamanho = \"area_ha\", n = 100L"), r"---[
`sampling/total`; `sampling/cluster` e `sampling/two_stage` com probabilidade
proporcional ao tamanho; `sampling/srs` para comparar.
]---", semente = TRUE)),

    trama::tr_node("sampling/cluster", fn = tr_sampling_cluster, label = "Conglomerados",
      category = "amostra_selecionar", icon = trama::tr_icon("group"), stochastic = TRUE,
      description = "Sorteia conglomerados inteiros (municípios, escolas) e toma todas as suas unidades.",
      inputs = list(populacao = T, plano = PLANO()), outputs = list(out = S),
      params = list(
        conglomerado = P("cols", "", label = "Conglomerado", example = "municipio"),
        conglomerados = I(0L, min = 0L, label = "Conglomerados a sortear"),
        probabilidade = E("iguais", c("iguais", "proporcional ao tamanho"), label = "Probabilidade")),
      help = .tr_sampling_ajuda(r"---[
A amostra de conglomerados em UM estágio: sorteia conglomerados (municípios,
escolas, quarteirões) e entrevista todas as unidades de cada um. Não precisa de
cadastro das unidades — só da lista de conglomerados —, e junta as entrevistas
no espaço, que é onde está o custo de campo.

O preço: unidades do mesmo conglomerado se parecem, e o deff passa de 1 (veja
o card de `sampling/mean`). Nas `fazendas` por município, perto de 3,5.

- **iguais** — todo conglomerado com chance m/M; peso M/m.
- **proporcional ao tamanho** — chance proporcional ao número de unidades
  (PPS sistemática).

A variância é a do conglomerado último: a variação ENTRE os totais dos
conglomerados sorteados.
]---", r"---[
- **Conglomerado** — coluna que identifica o conglomerado.
- **Conglomerados a sortear** — m (ou o **plano** de `sampling/size_cluster`).
- **Probabilidade** — `iguais` ou `proporcional ao tamanho`.
]---", ajuda_valor, exemplo("sampling/cluster", "conglomerado = \"municipio\", conglomerados = 12L"), r"---[
`sampling/two_stage` para sortear também dentro; `sampling/size_cluster` para o
plano; `sampling/simulate` para ver o preço.
]---", semente = TRUE)),

    trama::tr_node("sampling/two_stage", fn = tr_sampling_two_stage, label = "Dois estágios",
      category = "amostra_selecionar", icon = trama::tr_icon("git-fork"), stochastic = TRUE,
      description = "Sorteia conglomerados e, dentro de cada um, um número fixo de unidades.",
      inputs = list(populacao = T, plano = PLANO()), outputs = list(out = S),
      params = list(
        conglomerado = P("cols", "", label = "Conglomerado", example = "escola"),
        conglomerados = I(0L, min = 0L, label = "Conglomerados a sortear"),
        por_conglomerado = I(0L, min = 0L, label = "Unidades por conglomerado"),
        primeiro_estagio = E("proporcional ao tamanho", c("iguais", "proporcional ao tamanho"),
                             label = "Primeiro estágio")),
      help = .tr_sampling_ajuda(r"---[
A amostra em DOIS estágios: sorteia m conglomerados (primeiro estágio) e, dentro
de cada um, uma AAS de unidades (segundo estágio). É o desenho das pesquisas
domiciliares — setores, depois domicílios — e das avaliações escolares —
escolas, depois alunos.

Com o primeiro estágio **proporcional ao tamanho** e o mesmo número de unidades
em todo conglomerado, a amostra é AUTOPONDERADA: toda unidade da população tem
a mesma chance, e o peso é o mesmo para todas. É o padrão, e o que se usa quando
os conglomerados têm tamanhos muito diferentes.

Com **iguais**, o peso de cada unidade é (M/m) · (N_c/n_c).

Conglomerado menor que o número pedido entra inteiro. A variância é a do
conglomerado último — a variação entre os totais estimados dos conglomerados, que
já inclui a do segundo estágio.
]---", r"---[
- **Conglomerado** — coluna que identifica o conglomerado.
- **Conglomerados a sortear** — m.
- **Unidades por conglomerado** — n_c.
- **Primeiro estágio** — `iguais` ou `proporcional ao tamanho`.

Com o **plano** de `sampling/size_cluster` ligado, m e n_c vêm dele.
]---", ajuda_valor,
      exemplo("sampling/two_stage", "conglomerado = \"escola\", conglomerados = 30L, por_conglomerado = 10L", "escolas"), r"---[
`sampling/cluster`; `sampling/size_cluster` para o plano; `sampling/design`
para declarar uma amostra em dois estágios já coletada.
]---", semente = TRUE))
  )
}

.tr_sampling_nos_desenho <- function() {
  P <- trama::tr_param
  T <- "data/table"; S <- "sampling/sample"
  list(
    trama::tr_node("sampling/design", fn = tr_sampling_design, label = "Declarar desenho",
      category = "amostra_desenho", icon = trama::tr_icon("table-properties"),
      description = "Diz quais colunas de uma amostra já coletada são o peso, o estrato e o conglomerado.",
      inputs = list(dados = T), outputs = list(out = S),
      params = list(
        pesos = P("cols", "", label = "Peso", example = "peso"),
        estrato = P("cols", "", label = "Estrato", example = "estrato"),
        conglomerado = P("cols", "", label = "Conglomerado (UPA)", example = "setor"),
        populacao = P("cols", "", label = "UPAs do estrato na população", example = "setores_estrato")),
      help = .tr_sampling_ajuda(r"---[
Transforma a base de uma pesquisa JÁ COLETADA em amostra com desenho. É a porta
de entrada de microdado de pesquisa oficial, que vem com as colunas do
desenho: o peso, o estrato e a unidade primária de amostragem (UPA — o setor, a
escola, o município).

Sem declarar o desenho, uma média com `data/group_summarise` sai com o peso
errado, e qualquer erro padrão sai com a variância de uma AAS que a pesquisa
nunca foi — em conglomerados, subestimado pela metade ou mais.

- **Peso** — obrigatório com conglomerado. Sem conglomerado, pode faltar se a
  **população** do estrato for dada: o peso vira N_h/n_h (AAS estratificada).
- **Estrato** — em branco, sem estratos.
- **Conglomerado** — em branco, cada linha é a sua própria UPA.
- **UPAs do estrato na população** — o número de UPAs (ou de unidades, sem
  conglomerado) no estrato, constante dentro dele. Dá a correção de população
  finita no primeiro estágio; em branco, a variância é a com reposição, um
  pouco conservadora — e é o que as pesquisas oficiais costumam recomendar.

A amostra declarada não guarda a população, então não se simula
(`sampling/simulate` recusa).
]---", r"---[
- **Peso**, **Estrato**, **Conglomerado (UPA)**, **UPAs do estrato na
  população** — colunas da tabela.
]---", r"---[
Uma amostra (`sampling/sample`), pronta para `sampling/mean` e as outras
estimativas.
]---", r"---[
tr_flow(reg) |>
  tr_add("pesquisa", "sampling/example", dataset = "domicilios") |>
  tr_add("desenho", "sampling/design", pesos = "peso", estrato = "estrato", conglomerado = "setor",
         populacao = "setores_estrato", from = "pesquisa") |>
  tr_add("renda", "sampling/mean", variavel = "renda", por = "estrato", from = "desenho")
]---", r"---[
`sampling/poststratify` para calibrar os pesos; `sampling/proportion`;
`sampling/ratio` (renda per capita).
]---")),

    trama::tr_node("sampling/poststratify", fn = tr_sampling_poststratify, label = "Pós-estratificar",
      category = "amostra_desenho", icon = trama::tr_icon("weight"),
      description = "Ajusta os pesos para que cada grupo some o total conhecido da população.",
      inputs = list(amostra = S, totais = T), outputs = list(out = S),
      params = list(
        pos_estrato = P("cols", "", label = "Pós-estrato", example = "regiao"),
        coluna_total = P("cols", "N", label = "Coluna do total", example = "N")),
      help = .tr_sampling_ajuda(r"---[
A pós-estratificação: quando o total de cada grupo na população é conhecido (o
censo diz quantas fazendas há por região, quantas pessoas por sexo e idade),
os pesos da amostra são esticados ou encolhidos para que cada grupo some
exatamente o seu total:

    w*_i = w_i · N_g / N̂_g

Corrige o azar do sorteio (a AAS que veio com Sul demais) e parte da não
resposta (o grupo que respondeu menos pesa mais). A variância cai junto: o
erro padrão passa a usar os resíduos dentro dos pós-estratos, e a parte do erro
que era "quantos de cada grupo vieram" deixa de existir.

A tabela de **totais** tem uma linha por pós-estrato, com a coluna do
pós-estrato com o MESMO nome da coluna da amostra. Todo pós-estrato precisa de
total e de pelo menos uma unidade na amostra; grupo vazio se junta a um vizinho
antes, com um `data/mutate`.

Na simulação (`sampling/simulate`), a pós-estratificação é refeita em cada
amostra.
]---", r"---[
- **Pós-estrato** — coluna do grupo, com o mesmo nome nas duas tabelas.
- **Coluna do total** — coluna da tabela de totais com o N do grupo.
]---", r"---[
A amostra (`sampling/sample`) com os pesos ajustados.
]---", r"---[
tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("totais", "sampling/example", dataset = "estratos_fazendas") |>
  tr_add("aas", "sampling/srs", n = 200L, from = "pop") |>
  tr_add("pos", "sampling/poststratify", pos_estrato = "regiao", coluna_total = "N", from = "aas") |>
  tr_link("totais", "pos:totais") |>
  tr_add("media", "sampling/mean", variavel = "producao_t", from = "pos")
]---", r"---[
`sampling/stratified` quando o estrato é conhecido ANTES do sorteio;
`sampling/design`; `sampling/simulate` para ver o ganho.
]---"))
  )
}
