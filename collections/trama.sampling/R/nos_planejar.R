# As declarações das abas Fonte e Planejar.

.tr_sampling_nos_fonte <- function() {
  list(
    trama::tr_node("sampling/example", fn = tr_sampling_example, label = "Exemplo de amostragem",
      category = "amostra_fonte", icon = trama::tr_icon("database"),
      description = "Carrega uma população simulada com a verdade conhecida, ou uma amostra já coletada.",
      outputs = list(out = "data/table"),
      params = list(dataset = trama::tr_param_enum("fazendas", .TR_SAMPLING_EXEMPLOS, label = "Conjunto")),
      help = .tr_sampling_ajuda(r"---[
Carrega um conjunto pensado para ensinar amostragem. Os dois primeiros são
POPULAÇÕES inteiras — o cadastro de onde se sorteia —, simuladas com estruturas
plantadas, para que a estimativa possa ser conferida contra a verdade.

- **fazendas** — SIMULADO. 2.400 fazendas (`fazenda`) em 4 regiões (`regiao`)
  e 120 municípios (`municipio`, 20 fazendas cada), com `area_ha`,
  `producao_t`, `irrigada` (sim/não) e `trabalhadores`. As regiões foram
  plantadas diferentes em ESCALA: o Norte tem 300 fazendas enormes e muito
  variáveis, o Sul 900 pequenas. Estratificar por região ganha muito, e o
  Neyman manda amostra para o Norte. A produção cresce com a área (correlação
  ≈ 0,9): PPS pela área ganha ainda mais. Os municípios têm efeito próprio:
  sortear municípios inteiros perde.
- **estratos_fazendas** — a tabela de estratos das `fazendas`: `regiao`, `N`
  (fazendas), `desvio_producao` (o desvio da produção na região) e `custo` por
  entrevista (o Norte é longe: 60; o Sul, 20). É a entrada do
  `sampling/size_stratified` e os totais do `sampling/poststratify`.
- **escolas** — SIMULADO. 7.860 alunos (`aluno`) em 200 escolas (`escola`, de
  20 a 60 alunos), 160 públicas e 40 privadas (`rede`), com `nota` e
  `reprovado` (sim/não). Alunos da mesma escola se parecem (ICC da nota ≈ 0,25):
  o caso em que o efeito do desenho de conglomerados dói.
- **escolas_resumo** — uma linha por escola das `escolas`: `escola`, `rede` e
  `alunos` (a população dela). É a tabela de UNIDADES de
  `sampling/margin_levels` e `sampling/referral`.
- **perfil_escolas** — o perfil dos alunos das `escolas` em formato longo:
  `variavel` (`rede`, `reprovado`), `categoria`, `total` e `participacao`. É a
  composição de `sampling/size_domains` e os totais de `sampling/rake`.
- **perguntas_exemplo** — um questionário mínimo, uma pergunta de cada tipo
  (`pergunta`, `tipo`, `opcoes`, `base`): o formato de
  `sampling/question_margins`.
- **domicilios** — SIMULADO, e já é uma AMOSTRA: 420 domicílios de uma
  pesquisa estratificada por situação (`estrato`: urbano/rural), com 30 e 12
  setores sorteados (`setor`) e 10 domicílios por setor. Traz `peso`, o número
  de setores do estrato (`setores_estrato`), `moradores`, `renda` e `internet`.
  É a entrada do `sampling/design`.
]---", r"---[
- **Conjunto** — qual conjunto carregar.
]---", r"---[
Uma tabela (`data/table`).
]---", r"---[
tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("aas", "sampling/srs", n = 200L, from = "pop")
]---", r"---[
`sampling/srs` e os outros blocos de seleção; `sampling/design` para a amostra
coletada; `data/group_summarise` para montar uma tabela de estratos.
]---"))
  )
}

.tr_sampling_nos_planejar <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; N <- trama::tr_param_num; I <- trama::tr_param_int
  T <- "data/table"; PL <- "sampling/plan"
  CONF <- function() .tr_sampling_param_conf()
  POP <- function() N(0, min = 0, label = "População (0 = infinita)")
  DEFF <- function() N(1, min = 0.01, step = 0.1, label = "Efeito do desenho (deff)")
  RESP <- function() N(1, min = 0.01, max = 1, step = 0.05, label = "Taxa de resposta")
  ajuda_comuns <- r"---[
- **Confiança** — número entre 0,5 e 0,999 (0,95 = 95%).
- **Distribuição** — `t` (padrão): o quantil é o t com os graus de liberdade
  que a amostra vai ter (n − 1 unidades), o mesmo que o card de
  `sampling/mean` vai usar; como o t depende do n, o bloco procura o menor n
  cuja margem, com os gl dele, cabe na pedida. `z`: a fórmula clássica com a
  normal (1,96 a 95%), que dá n um pouco menor e subestima a margem com n
  pequeno.
- **População** — o N. Em 0, população infinita (sem correção finita); a
  correção só pesa quando a amostra passa de uns 5% da população.
- **Efeito do desenho (deff)** — quanto o desenho infla a variância em relação
  à AAS: 1 na AAS, abaixo de 1 numa boa estratificação, 1,5 a 3 em
  conglomerados. Vem de uma pesquisa anterior, ou do `sampling/size_cluster`.
- **Taxa de resposta** — a fração que se espera conseguir entrevistar (0,8 =
  80%). O n é dividido por ela: o número de contatos cresce, a informação não.
]---"
  ajuda_escada <- r"---[
### O card

O número grande é o n final; embaixo, a ESCADA de como se chegou nele — o n₀
da fórmula, cada ajuste e o arredondamento —, com uma barra por degrau. É ela
que diz qual suposição vale a pena discutir: se o n dobrou pela não resposta, a
conversa é sobre a taxa de resposta, e não sobre a margem.
]---"
  list(
    trama::tr_node("sampling/size_mean", fn = tr_sampling_size_mean, label = "Tamanho para média", version = 2L,
      category = "amostra_planejar", icon = trama::tr_icon("calculator"),
      description = "Quantas unidades sortear para estimar uma média com a margem de erro desejada?",
      inputs = list(piloto = trama::tr_port(T, required = FALSE)), outputs = list(out = PL),
      params = list(
        variavel = P("cols", "", label = "Variável do piloto", example = "producao_t"),
        desvio_padrao = N(10, min = 0, label = "Desvio padrão"),
        media = N(0, label = "Média esperada (erro relativo)"),
        erro = N(1, min = 0, label = "Margem de erro"),
        tipo_erro = E("absoluto", c("absoluto", "relativo"), label = "Tipo de erro"),
        confianca = CONF(), populacao = POP(), deff = DEFF(), taxa_resposta = RESP(), distribuicao = E("t", .TR_SAMPLING_DISTRIBUICOES, label = "Distribuição")),
      pressupostos = list(trama::tr_pressuposto("A amostra será uma **AAS sem reposição** (ou o deff informado traduz o desenho real para ela); a correção finita é n/(1 + n/N).", verificar = "sampling/simulate", se_falhar = "Para estratos, use `sampling/size_stratified`; para conglomerados, `sampling/size_cluster`; confira o n escolhido com `sampling/simulate`."),
        trama::tr_pressuposto("O **desvio padrão** S (do piloto, de pesquisa anterior ou da regra amplitude ÷ 4) está perto do da população, e a média amostral é aproximadamente normal no n calculado.", se_falhar = "Com piloto pequeno o S é incerto: use um S maior (limite superior) ou refaça o plano com o S da primeira onda."),
        trama::tr_pressuposto("A **não resposta** é ignorável: quem responde se parece com quem não responde, e dividir por ela só repõe o tamanho.", se_falhar = "Se a não resposta depende do tema, aumentar o n não corrige o viés; calibre depois com `sampling/rake` ou `sampling/poststratify`.")),
      referencias = list(.tr_sampling_refs()$cochran, .tr_sampling_refs()$bolfarine,
        .tr_sampling_impl("tr_sampling_size_mean", "n₀ = (q·S/E)², q = t com gl = n − 1 (menor n cuja margem com os gl dele cabe na pedida) ou z se declarado; depois × deff, n/(1 + n/N) e ÷ taxa de resposta, nessa ordem; erro relativo usa E = e·média.")),
      help = .tr_sampling_ajuda(paste(r"---[
O tamanho de uma amostra aleatória simples para estimar uma MÉDIA com margem
de erro E (metade do intervalo de confiança):

    n₀ = (q · S / E)²

em que S é o desvio padrão da variável e q o quantil da confiança: o t com os
gl que a amostra terá (n − 1), por padrão, ou z (1,96 a 95%) se declarado. Depois
vêm os ajustes — deff, população finita e não resposta —, nessa ordem
(Cochran 1977).

O desvio vem de um campo ou de um **piloto**: ligue uma tabela na porta
`piloto` e escolha a variável, e o desvio e a média saem dela. Sem piloto, o
desvio pode vir de uma pesquisa anterior, ou da regra de bolso amplitude ÷ 4.

**Erro relativo** é a margem em % da média ("quero errar no máximo 10%"), que
é como a agronomia e a economia costumam pedir; precisa da média (do campo ou
do piloto).
]---", ajuda_escada), paste(r"---[
- **Variável do piloto** — a coluna do piloto; só com a porta `piloto` ligada.
- **Desvio padrão** — S, sem piloto.
- **Média esperada** — só para erro relativo, sem piloto.
- **Margem de erro** — na unidade da variável (absoluto) ou em % da média
  (relativo).
- **Tipo de erro** — `absoluto` ou `relativo`.
]---", ajuda_comuns), r"---[
Um plano (`sampling/plan`). Ligue-o na porta `plano` de `sampling/srs`,
`sampling/systematic` ou `sampling/pps`.
]---", r"---[
tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("piloto", "sampling/srs", n = 30L, from = "pop") |>
  tr_add("plano", "sampling/size_mean", variavel = "producao_t", erro = 10, tipo_erro = "relativo",
         populacao = 2400) |>
  tr_link("piloto", "plano:piloto")
]---", r"---[
`sampling/size_proportion`; `sampling/size_cluster` para levar o plano a
conglomerados; `sampling/size_curve` para ver o n contra a margem;
`sampling/srs` para sortear.
]---")),

    trama::tr_node("sampling/size_proportion", fn = tr_sampling_size_proportion, label = "Tamanho para proporção", version = 2L,
      category = "amostra_planejar", icon = trama::tr_icon("percent"),
      description = "Quantas unidades sortear para estimar uma proporção com a margem de erro desejada?",
      outputs = list(out = PL),
      params = list(
        proporcao = N(0.5, min = 0, max = 1, step = 0.05, label = "Proporção esperada"),
        erro = N(0.05, min = 0, max = 1, step = 0.01, label = "Margem de erro"),
        confianca = CONF(), populacao = POP(), deff = DEFF(), taxa_resposta = RESP(), distribuicao = E("t", .TR_SAMPLING_DISTRIBUICOES, label = "Distribuição")),
      pressupostos = list(trama::tr_pressuposto("A amostra será uma **AAS sem reposição** (ou o deff informado traduz o desenho real para ela); a correção finita é n/(1 + n/N).", verificar = "sampling/simulate", se_falhar = "Para estratos, use `sampling/size_stratified`; para conglomerados, `sampling/size_cluster`; confira o n escolhido com `sampling/simulate`."),
        trama::tr_pressuposto("**Aproximação normal** da proporção: p esperada não muito perto de 0 ou 1 para o n calculado (n·p e n·(1 − p) acima de uns 10).", se_falhar = "Com p esperada extrema, use p = 0,5 (pior caso) ou confira com `sampling/simulate`."),
        trama::tr_pressuposto("A **não resposta** é ignorável: quem responde se parece com quem não responde, e dividir por ela só repõe o tamanho.", se_falhar = "Se a não resposta depende do tema, aumentar o n não corrige o viés; calibre depois com `sampling/rake` ou `sampling/poststratify`.")),
      referencias = list(.tr_sampling_refs()$cochran, .tr_sampling_refs()$bolfarine,
        .tr_sampling_impl("tr_sampling_size_proportion", "n₀ = q²·p(1 − p)/E², q = t com gl = n − 1 (menor n cuja margem com os gl dele cabe na pedida) ou z se declarado; depois × deff, n/(1 + n/N) e ÷ taxa de resposta.")),
      help = .tr_sampling_ajuda(paste(r"---[
O tamanho de uma amostra aleatória simples para estimar uma PROPORÇÃO p com
margem de erro E:

    n₀ = q² · p(1 − p) / E²

com q o t de n − 1 gl (padrão) ou z. Sem ideia de p, use **0,5**: é o pior
caso, e o n que sai serve para qualquer proporção — é por isso que pesquisa de
opinião com margem de 3 pontos e 95% tem sempre perto de 1.068 entrevistas (com
z; com t, 1.070). Com p = 0,1 o n cai para um terço.

A margem é em PONTOS de proporção: 0,05 são 5 pontos (40% ± 5%), e não 5% de
40%.
]---", ajuda_escada), paste(r"---[
- **Proporção esperada** — p, entre 0 e 1.
- **Margem de erro** — em pontos de proporção (0,03 = 3 pontos).
]---", ajuda_comuns), r"---[
Um plano (`sampling/plan`). Ligue-o na porta `plano` de `sampling/srs`, ou em
`sampling/size_cluster`.
]---", r"---[
tr_flow(reg) |>
  tr_add("plano", "sampling/size_proportion", erro = 0.03, taxa_resposta = 0.7)
]---", r"---[
`sampling/size_mean`; `sampling/size_cluster`; `sampling/size_curve`;
`sampling/proportion` para estimar depois.
]---")),

    trama::tr_node("sampling/size_stratified", fn = tr_sampling_size_stratified, label = "Tamanho estratificado", version = 2L,
      category = "amostra_planejar", icon = trama::tr_icon("layers"),
      description = "Quantas unidades sortear, e quantas em cada estrato, para a margem de erro desejada?",
      inputs = list(estratos = T), outputs = list(out = PL),
      params = list(
        estrato = P("cols", "", label = "Estrato", example = "regiao"),
        tamanho = P("cols", "", label = "Tamanho (N_h)", example = "N"),
        desvio = P("cols", "", label = "Desvio (S_h)", example = "desvio_producao"),
        custo = P("cols", "", label = "Custo (alocação ótima)", example = "custo"),
        alocacao = E("neyman", c("proporcional", "neyman", "ótima", "igual"), label = "Alocação"),
        erro = N(0, min = 0, label = "Margem de erro da média"),
        n_total = I(0L, min = 0L, label = "n total (vence a margem)"),
        confianca = CONF(), taxa_resposta = RESP(), distribuicao = E("t", .TR_SAMPLING_DISTRIBUICOES, label = "Distribuição")),
      pressupostos = list(
        trama::tr_pressuposto("Os **N_h e S_h** da tabela são os da população (ou boas aproximações), e o sorteio dentro de cada estrato é AAS independente.", verificar = "sampling/simulate", se_falhar = "Com S_h incertos, a alocação proporcional é a mais robusta; confira o plano com `sampling/stratified` e `sampling/simulate`."),
        trama::tr_pressuposto("A alocação **ótima** supõe custo linear por entrevista (c_h constante dentro do estrato).", se_falhar = "Custo fixo por estrato não muda as proporções da alocação ótima; desconte-o do orçamento antes de fixar o n total."),
        trama::tr_pressuposto("A **não resposta** é ignorável: quem responde se parece com quem não responde, e dividir por ela só repõe o tamanho.", se_falhar = "Se a não resposta depende do tema, aumentar o n não corrige o viés; calibre depois com `sampling/rake` ou `sampling/poststratify`.")),
      referencias = list(.tr_sampling_refs()$neyman, .tr_sampling_refs()$cochran, .tr_sampling_refs()$bolfarine,
        .tr_sampling_impl("tr_sampling_size_stratified", "n = Σ W_h²S_h²/a_h / ((E/z)² + Σ W_h S_h²/N), com a_h da alocação (proporcional, Neyman, ótima, igual); inteiros, mínimo 2 e máximo N_h por estrato; q = t com gl = n − H (menor n que se sustenta) ou z se declarado.")),
      help = .tr_sampling_ajuda(paste(r"---[
Calcula o n de uma amostra ESTRATIFICADA e o reparte entre os estratos. A
entrada é a tabela de estratos: uma linha por estrato, com o tamanho na
população (N_h) e o desvio da variável dentro dele (S_h) — é o que um
`data/group_summarise` do cadastro dá (`n()` e `sd(x)`), ou o que uma pesquisa
anterior publicou.

As alocações:

- **proporcional** — n_h ∝ N_h. A amostra é um retrato da população, e a
  autoponderada: todo mundo tem o mesmo peso.
- **neyman** — n_h ∝ N_h · S_h. Mais amostra onde há mais população E mais
  variação. É a de menor variância para um n fixo, e o ganho é grande quando os
  estratos diferem em variabilidade (o Norte das `fazendas`).
- **ótima** — n_h ∝ N_h · S_h / √c_h. Neyman com custo: estrato caro recebe
  menos. É a de menor variância para um ORÇAMENTO fixo.
- **igual** — o mesmo n em todo estrato. Serve para comparar estratos entre si,
  e não para a média geral.

Com **Margem de erro**, o n sai da variância da média estratificada igualada a
(E/z)²; com **n total**, o n é dado e o card mostra a margem que ele alcança.
Nenhum estrato recebe menos que 2 (sem isso a variância dele não existe) nem
mais que N_h (o excedente vira censo e é redistribuído).

Para uma proporção, ponha em S_h a raiz de p_h(1 − p_h).
]---", ajuda_escada), r"---[
- **Estrato**, **Tamanho (N_h)**, **Desvio (S_h)** — colunas da tabela de
  estratos.
- **Custo** — coluna do custo por unidade; só na alocação ótima.
- **Alocação** — `proporcional`, `neyman`, `ótima` ou `igual`.
- **Margem de erro da média** — E, na unidade da variável.
- **n total** — n fixo; quando maior que 0, vence a margem.
- **Confiança**, **Taxa de resposta** — como em `sampling/size_mean`; a taxa
  infla cada estrato (até N_h).
]---", r"---[
Um plano (`sampling/plan`) com a alocação; o adaptador para `data/table`
devolve uma linha por estrato. Ligue-o na porta `plano` de
`sampling/stratified`.
]---", r"---[
tr_flow(reg) |>
  tr_add("estratos", "sampling/example", dataset = "estratos_fazendas") |>
  tr_add("plano", "sampling/size_stratified", estrato = "regiao", tamanho = "N",
         desvio = "desvio_producao", alocacao = "neyman", erro = 60, from = "estratos")
]---", r"---[
`sampling/stratified` para sortear com a alocação; `sampling/size_mean` para
a AAS de comparação; `data/group_summarise` para montar a tabela de estratos.
]---")),

    trama::tr_node("sampling/size_cluster", fn = tr_sampling_size_cluster, label = "Tamanho por conglomerados", version = 2L,
      category = "amostra_planejar", icon = trama::tr_icon("boxes"),
      description = "Quantos conglomerados sortear, dado quanto as unidades de um mesmo conglomerado se parecem (ICC)?",
      inputs = list(plano = PL), outputs = list(out = PL),
      params = list(
        tamanho_conglomerado = N(20, min = 1, label = "Unidades por conglomerado (m̄)"),
        icc = N(0.05, min = 0, max = 1, step = 0.01, label = "ICC"),
        conglomerados = N(0, min = 0, label = "Conglomerados na população (0 = infinitos)"),
        cv_tamanho = N(0, min = 0, max = 10, step = 0.05, label = "CV do tamanho dos conglomerados"),
        distribuicao = E("t", .TR_SAMPLING_DISTRIBUICOES, label = "Distribuição")),
      pressupostos = list(
        trama::tr_pressuposto("O deff segue o modelo de **ICC comum**, o mesmo ρ em todos os conglomerados: deff = 1 + ((CV² + 1)·m̄ − 1)·ρ, com CV o coeficiente de variação do tamanho dos conglomerados (Eldridge, Ashby & Kerry 2006); com CV = 0 (tamanhos iguais) é 1 + (m̄ − 1)·ρ. Supõe a média estimada pela razão Σy/Σm (análise por indivíduo).", verificar = "sampling/simulate", se_falhar = "Sem ideia do CV, meça-o no cadastro dos conglomerados (desvio ÷ média do tamanho) ou trabalhe com cenários, e teste o plano com `sampling/two_stage` e `sampling/simulate`."),
        trama::tr_pressuposto("Com **t**, os graus de liberdade são os conglomerados − 1: com poucos conglomerados o quantil cresce bastante, e o plano sobe para o menor número que se sustenta.", se_falhar = "Declare `z` só se o desenho final tiver muitos conglomerados ou outra fonte de gl."),
        trama::tr_pressuposto("Os conglomerados serão sorteados por **AAS** (ou com probabilidade que o deff já absorva), e o ICC informado vem de pesquisa parecida.", se_falhar = "Trabalhe com cenários de ICC e escolha o n do pior plausível.")),
      referencias = list(.tr_sampling_refs()$kish, .tr_sampling_refs()$eldridge, .tr_sampling_refs()$cochran,
        .tr_sampling_impl("tr_sampling_size_cluster", "n₀ do plano refeito com q = t (gl = conglomerados − 1, menor número que se sustenta) ou z, × deff = 1 + ((CV² + 1)·m̄ − 1)·ρ, ÷ m̄ para número de conglomerados, correção finita com M e ÷ taxa de resposta; o deff e a correção finita do plano ligado são trocados.")),
      help = .tr_sampling_ajuda(paste(r"---[
Leva um plano de AAS (de `sampling/size_mean` ou `sampling/size_proportion`) a
um plano de CONGLOMERADOS: escolas em vez de alunos, municípios em vez de
fazendas, setores em vez de domicílios.

Unidades do mesmo conglomerado se parecem, e cada uma a mais no mesmo
conglomerado traz menos informação nova. O quanto se parecem é a correlação
intraclasse (ICC, ρ), e o custo em variância é o efeito do desenho:

    deff = 1 + (m̄ − 1) · ρ

(com conglomerados de tamanhos desiguais, 1 + ((CV² + 1) · m̄ − 1) · ρ, em que CV
é o coeficiente de variação do tamanho — Eldridge, Ashby & Kerry 2006; com CV
de 0,6, o deff de 20 alunos e ρ = 0,05 vai de 1,95 a 2,31)

Com 20 alunos por escola e ρ = 0,05, deff = 1,95: a amostra de conglomerados
precisa de quase o dobro de alunos da AAS para a mesma margem. Com ρ = 0,25
(as `escolas`), deff = 5,75. É por isso que vale mais sortear MAIS escolas com
MENOS alunos em cada.

O n₀ do plano ligado é multiplicado pelo deff, dividido por m̄ (vira número de
conglomerados), corrigido pela população de conglomerados (M) e pela não
resposta do plano. O deff e a correção finita do plano ligado são trocados
pelos daqui, e a nota diz.

ICC de referência: 0,01 a 0,05 em características de domicílio por setor;
0,1 a 0,3 em desempenho escolar por escola.
]---", ajuda_escada), r"---[
- **Unidades por conglomerado (m̄)** — o tamanho médio que se vai entrevistar
  em cada conglomerado.
- **ICC** — a correlação intraclasse esperada.
- **Conglomerados na população** — M; 0 para infinitos.
- **CV do tamanho dos conglomerados** — desvio padrão ÷ média do número de
  unidades por conglomerado; 0 supõe todos iguais.
- **Distribuição** — `t` (padrão, gl = conglomerados − 1) ou `z`.
]---", r"---[
Um plano (`sampling/plan`) de conglomerados. Ligue-o na porta `plano` de
`sampling/cluster` ou `sampling/two_stage`.
]---", r"---[
tr_flow(reg) |>
  tr_add("aas", "sampling/size_mean", desvio_padrao = 12, erro = 1.5) |>
  tr_add("cong", "sampling/size_cluster", tamanho_conglomerado = 15, icc = 0.25,
         conglomerados = 200, from = "aas")
]---", r"---[
`sampling/two_stage` e `sampling/cluster` para sortear; `sampling/mean`, cujo
card mostra o deff que o desenho de fato teve.
]---")),

    trama::tr_node("sampling/size_curve", fn = tr_sampling_size_curve, label = "Curva do tamanho",
      category = "amostra_planejar", icon = trama::tr_icon("chart-spline"),
      description = "Como o tamanho da amostra cresce quando a margem de erro aperta, em cada confiança?",
      inputs = list(plano = PL), outputs = list(out = "view/plot"),
      params = .tr_sampling_props(),
      help = .tr_sampling_ajuda(r"---[
Desenha o n contra a margem de erro, com os mesmos ajustes do plano ligado
(deff, população, não resposta), uma curva para cada confiança (90%, 95%, 99%
e a do plano, quando é outra), e marca o ponto do plano sobre a sua curva.

A curva é a conversa com quem paga a pesquisa: o n cresce com o QUADRADO da
precisão, e metade da margem custa quatro vezes a amostra. É ela que mostra
onde o próximo ponto de precisão fica caro demais.
]---", r"---[
Só os de aparência.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("plano", "sampling/size_proportion", erro = 0.04, populacao = 5000) |>
  tr_add("curva", "sampling/size_curve", titulo = "Quanto custa cada ponto de margem", from = "plano")
]---", r"---[
`sampling/size_mean`; `sampling/size_proportion`.
]---", grafico = TRUE))
  )
}
