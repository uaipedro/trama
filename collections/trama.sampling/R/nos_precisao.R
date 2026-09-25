# As declarações da aba Precisão, e dos dois blocos novos que moram em Planejar
# (tamanho por grupos) e em Desenho (raking).

.tr_sampling_nos_precisao <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; N <- trama::tr_param_num
  T <- "data/table"; PL <- "sampling/plan"
  CONF <- function() E("95%", .TR_SAMPLING_CONFIANCAS, label = "Confiança")
  PROP <- function() N(0.5, min = 0.01, max = 0.99, step = 0.05, label = "Proporção esperada")
  unidades_params <- function() list(
    unidade = P("cols", "", label = "Unidade", example = "escola"),
    tamanho = P("cols", "", label = "População da unidade", example = "alunos"),
    n = P("cols", "", label = "Entrevistas na unidade", example = "n"),
    grupos = P("cols", "", label = "Níveis intermediários", example = "rede"))
  ajuda_unidades <- r"---[
- **Unidade** — coluna com o nome da unidade (capital, escola, município).
- **População da unidade** — coluna com o tamanho da população-alvo nela.
- **Entrevistas na unidade** — coluna com o n previsto (um `data/mutate` com
  `25` põe o mesmo n em todas).
- **Níveis intermediários** — colunas de agrupamento, separadas por vírgula
  (a região, a rede). Em branco, só o total e as unidades.
- **Proporção esperada** — p; 0,5 é o pior caso e vale para qualquer pergunta.
- **Confiança** — 90%, 95% ou 99%.
]---"
  ajuda_tabela_margens <- r"---[
Uma tabela (`data/table`) com uma linha por nível: `nivel_tipo` (`total`, o
nome de cada nível intermediário, `unidade`), `nivel`, `unidades`,
`populacao`, `n`, `deff_ponderacao` (Kish), `deff`, `n_efetivo`, `margem` e
`margem_pp` (em pontos percentuais). Vai direto para
`sampling/plot_margins`, `data/filter` ou `data/write_csv`.
]---"
  exemplo_unidades <- function(no, extra) sprintf(r"---[
tr_flow(reg) |>
  tr_add("escolas", "sampling/example", dataset = "escolas_resumo") |>
  tr_add("n", "data/mutate", name = "n", expr = "10", from = "escolas") |>
  tr_add("margens", "%s", unidade = "escola", tamanho = "alunos", n = "n",
         grupos = "rede"%s, from = "n")
]---", no, extra)
  list(
    trama::tr_node("sampling/margin", fn = tr_sampling_margin, label = "Margem para n dado",
      category = "amostra_precisao", icon = trama::tr_icon("ruler"),
      description = "Com n entrevistas, qual a margem de erro de uma proporção?",
      outputs = list(out = PL),
      params = list(
        n = N(400, min = 2, label = "Entrevistas"), proporcao = PROP(), confianca = CONF(),
        populacao = N(0, min = 0, label = "População (0 = infinita)"),
        deff = N(1, min = 0.01, step = 0.1, label = "Efeito do desenho (deff)"),
        taxa_resposta = N(1, min = 0.01, max = 1, step = 0.05, label = "Taxa de resposta")),
      pressupostos = c(list(trama::tr_pressuposto(
        "Com **População** informada, a correção finita é a da AAS sem reposição, (1 − n/N); a amostra sorteada tem de ser uma fração da mesma população.",
        se_falhar = "Deixe a População em 0 (infinita) quando não souber o N; o erro sai um pouco conservador.")),
        .tr_sampling_press_margem("As entrevistas se comportam como uma **amostra aleatória simples** da população")),
      referencias = list(.tr_sampling_refs()$cochran, .tr_sampling_refs()$bolfarine,
        .tr_sampling_impl("tr_sampling_margin", "E = z·√(deff·p(1 − p)/n·(1 − n/N)), com z normal (não t) e n = entrevistas × taxa de resposta; a correção finita só com N > 0.")),
      help = .tr_sampling_ajuda(paste(r"---[
O caminho contrário do `sampling/size_proportion`: o n já está dado (o campo
contratado, o orçamento fechado) e a pergunta é quanto se erra:

    E = z · √(deff · p(1 − p) / n · (1 − n/N))

O card mostra a margem em pontos percentuais e a escada: entrevistas previstas,
as que viram resposta, o n efetivo depois do deff. É a conta de uma célula
só; para vários níveis de uma vez, `sampling/margin_levels`.
]---"), r"---[
- **Entrevistas** — n previsto.
- **Proporção esperada** — p; 0,5 é o pior caso.
- **Confiança**, **População**, **Efeito do desenho**, **Taxa de resposta** —
  como em `sampling/size_proportion`.
]---", r"---[
Um plano (`sampling/plan`) do tipo margem.
]---", r"---[
tr_flow(reg) |>
  tr_add("capital", "sampling/margin", n = 25, deff = 1.2)
]---", r"---[
`sampling/margin_levels`; `sampling/size_proportion` para o caminho de ida;
`sampling/size_curve`.
]---")),

    trama::tr_node("sampling/margin_levels", fn = tr_sampling_margin_levels, label = "Margem por nível",
      category = "amostra_precisao", icon = trama::tr_icon("layers-plus"),
      description = "Com n entrevistas em cada unidade, qual a margem no total, em cada nível intermediário e em cada unidade?",
      inputs = list(unidades = T), outputs = list(out = T),
      params = c(unidades_params(), list(
        deff = N(1, min = 0.01, step = 0.1, label = "Deff de agrupamento"),
        proporcao = PROP(), confianca = CONF())),
      pressupostos = c(list(trama::tr_pressuposto(
        "Nos agregados, cada unidade é um **estrato** amostrado de forma independente, e a estimativa pondera pela população (W_h = N_h/N).",
        se_falhar = "Se as unidades foram sorteadas (e não todas incluídas), o agregado ganha uma variância entre unidades que a tabela não tem: use `sampling/two_stage` e `sampling/simulate`.")),
        .tr_sampling_press_margem()),
      referencias = list(.tr_sampling_refs()$kish, .tr_sampling_refs()$cochran, .tr_sampling_refs()$bolfarine,
        .tr_sampling_impl("tr_sampling_margin_levels", "Variância estratificada Σ W_h²(1 − n_h/N_h)·p(1 − p)/n_h vezes o deff de agrupamento; deff de ponderação de Kish n·Σ W_h²/n_h; z normal.")),
      help = .tr_sampling_ajuda(paste(r"---[
A margem de erro de uma proporção em todos os níveis de agregação de uma vez: o
total (o país), cada nível intermediário (as macrorregiões) e cada unidade (as
capitais), a partir de uma tabela com a população e o n previsto de cada
unidade.

Numa unidade é a margem de uma AAS de n. Num agregado, as unidades são estratos:
a estimativa pondera cada uma pela sua população, e a margem sai da variância
estratificada, Σ W_h²(1 − f_h)·p(1 − p)/n_h.

**O preço de n igual em unidades desiguais.** Com 25 entrevistas em São Paulo e
25 em Palmas, a média nacional precisa dar a São Paulo o peso da sua população,
e a amostra fica desbalanceada em relação a esses pesos. O custo é o deff de
Kish, n · Σ W_h²/n_h, na coluna `deff_ponderacao`: 1 com alocação proporcional
à população, e tanto maior quanto mais desigual. É por isso que 675
entrevistas em 27 capitais valem, no total, bem menos que 675 aleatórias.

**Deff de agrupamento** multiplica tudo, para quando as entrevistas vêm em
grupos parecidos entre si (escolas, redes de indicação).
]---"), paste(ajuda_unidades, r"---[
- **Deff de agrupamento** — efeito de conglomerado além da ponderação (1:
  nenhum).
]---"), ajuda_tabela_margens, exemplo_unidades("sampling/margin_levels", ""), r"---[
`sampling/referral` para os cenários de indicação; `sampling/plot_margins`;
`sampling/margin` para uma célula.
]---")),

    trama::tr_node("sampling/referral", fn = tr_sampling_referral, label = "Margem com indicação",
      category = "amostra_precisao", icon = trama::tr_icon("network"),
      description = "Se cada participante indicar k pessoas, quanto a margem melhora em cada nível?",
      inputs = list(unidades = T), outputs = list(out = T),
      params = c(unidades_params(), list(
        convidados = P("text", "0, 1, 2, 3, 5", label = "Convidados por participante", example = "0, 1, 2, 3, 5"),
        adesao = N(1, min = 0.01, max = 1, step = 0.05, label = "Adesão dos convidados"),
        icc = P("text", "0.05, 0.1, 0.2", label = "ICC (cenários)", example = "0.05, 0.1, 0.2"),
        proporcao = PROP(), confianca = CONF())),
      pressupostos = c(list(trama::tr_pressuposto(
        "O agrupamento pelas redes segue o modelo de **correlação intraclasse comum**: deff = 1 + (m − 1)·ICC, com redes do mesmo tamanho m e o mesmo ICC em todas.",
        se_falhar = "Trabalhe com vários cenários de ICC e, depois da coleta, meça o ICC real das redes."),
        trama::tr_pressuposto("As **sementes** (a coleta de base) são uma amostra aleatória da unidade; a indicação só multiplica o n, não corrige quem entrou.",
          se_falhar = "Leia a margem como nominal e calibre com `sampling/rake`.")),
        .tr_sampling_press_margem()),
      referencias = list(.tr_sampling_refs()$kish, .tr_sampling_refs()$cochran,
        .tr_sampling_impl("tr_sampling_referral", "Para cada cenário (k, ICC), refaz as margens de tr_sampling_margin_levels com n × (1 + k·adesão) e o deff de agrupamento 1 + (m − 1)·ICC multiplicado.")),
      help = .tr_sampling_ajuda(paste(r"---[
A expansão por indicação: cada participante da coleta de base convida k
pessoas. Para cada combinação de **convidados** e **ICC**, a tabela refaz as
margens de `sampling/margin_levels` com o n multiplicado.

Cada participante vira uma REDE de m = 1 + k · adesão respostas. Quem indica
indica parecido — amigos, colegas, vizinhos —, e respostas da mesma rede trazem
menos informação nova do que o mesmo número de pessoas independentes. O custo é
o deff de conglomerado, 1 + (m − 1) · ICC, que multiplica o deff da
ponderação:

- com ICC = 0, k convidados multiplicam o n efetivo por 1 + k;
- com ICC = 0,2 e 5 convidados, o n cresce 6 vezes mas o efetivo só 3.

O ICC real de uma rede de indicação só se mede depois da coleta (é a
correlação das respostas dentro das redes). Antes, trabalhe com cenários: 0,05
para perguntas pouco ligadas ao círculo social, 0,2 ou mais para as muito
ligadas (hábitos compartilhados, território).

Os cenários com 0 convidados são a coleta de base, na mesma tabela, para a
comparação direta.
]---"), paste(ajuda_unidades, r"---[
- **Convidados por participante** — lista de k, separados por vírgula.
- **Adesão dos convidados** — a fração dos convidados que de fato responde.
- **ICC (cenários)** — lista de correlações intraclasse, com ponto decimal.
]---"), r"---[
Uma tabela (`data/table`) com as colunas de `sampling/margin_levels` precedidas
de `convidados`, `adesao`, `icc`, `tamanho_rede` e `deff_agrupamento`: uma linha
por cenário e nível.
]---", exemplo_unidades("sampling/referral", ",\n         convidados = \"0, 2, 4\", icc = \"0.1, 0.25\""), r"---[
`sampling/margin_levels`; `sampling/plot_margins`; `sampling/size_cluster`,
a mesma fórmula do deff no caminho de ida.
]---")),

    trama::tr_node("sampling/detectable_difference", fn = tr_sampling_detectable_difference,
      label = "Diferença detectável", category = "amostra_precisao", icon = trama::tr_icon("git-compare"),
      description = "Com os grupos do tamanho previsto, qual a menor diferença entre eles que a pesquisa detecta?",
      inputs = list(grupos = T), outputs = list(out = T),
      params = list(
        variavel = P("cols", "", label = "Variável", example = "variavel"),
        grupo = P("cols", "", label = "Grupo", example = "categoria"),
        tamanho = P("cols", "", label = "Tamanho do grupo", example = "participacao"),
        n_total = N(0, min = 0, label = "n total (se tamanho é participação)"),
        proporcao = PROP(), confianca = CONF(),
        poder = E("80%", c("80%", "90%"), label = "Poder"),
        deff = N(1, min = 0.01, step = 0.1, label = "Efeito do desenho (deff)"),
        diferenca_relevante = N(0.10, min = 0.01, max = 1, step = 0.01, label = "Diferença que importa")),
      pressupostos = c(list(trama::tr_pressuposto(
        "Os dois grupos são **independentes** e a comparação é de UM par escolhido antes de olhar os dados, com teste bilateral e a mesma p nos dois.",
        se_falhar = "Para muitas comparações, divida a significância pelo número de pares (Bonferroni) e recalcule; grupos que se sobrepõem pedem a variância da diferença pelo desenho."),
        trama::tr_pressuposto("Sem **correção finita**: supõe grupos pequenos diante da população.",
          se_falhar = "Com grupo recenseado quase inteiro, a DMD sai conservadora.")),
        .tr_sampling_press_margem()),
      referencias = list(.tr_sampling_refs()$fleiss, .tr_sampling_refs()$cochran,
        .tr_sampling_impl("tr_sampling_detectable_difference", "DMD = (z_{1−α/2} + z_{poder})·√(deff·p(1 − p)·(1/n_a + 1/n_b)), aproximação normal com variância não agrupada e p comum; sem correção finita.")),
      help = .tr_sampling_ajuda(paste(r"---[
Responde se dá para COMPARAR grupos: para cada par de categorias de uma mesma
variável (mulheres × homens, pretos × brancos), a menor diferença entre as duas
proporções que a pesquisa detectaria com o nível de confiança e o poder
escolhidos:

    DMD = (z_{1−α/2} + z_{poder}) · √(deff · p(1 − p) · (1/n_a + 1/n_b))

A coluna `sustentavel` diz se a **diferença que importa** (10 pontos, por
padrão) é detectável. Quando não é, um resultado "sem diferença" entre os dois
grupos não quer dizer nada: é falta de gente no menor deles.

Sem cotas, o tamanho de cada grupo é a participação dele vezes o n total. Por
isso a tabela aceita a participação (com **n total**) ou a contagem direta. A
comparação é governada pelo MENOR grupo do par: dobrar o grupo grande quase não
muda a DMD.

Duas margens de erro que não se sobrepõem não são o teste da diferença, e duas
que se sobrepõem ainda podem esconder uma diferença real. É esta a conta certa
para a pergunta "dá para comparar?".
]---"), r"---[
- **Variável**, **Grupo** — colunas da tabela longa (uma linha por categoria).
- **Tamanho do grupo** — coluna com a contagem, ou a participação (0 a 1) se o
  **n total** for maior que 0.
- **Proporção esperada** — p; 0,5 é o pior caso.
- **Confiança**, **Poder** — o nível do teste e a chance de detectar a diferença
  quando ela existe.
- **Efeito do desenho** — deff da pesquisa (ponderação e agrupamento).
- **Diferença que importa** — em proporção (0,10 = 10 pontos).
]---", r"---[
Uma tabela (`data/table`): `variavel`, `grupo_a`, `grupo_b`, `n_a`, `n_b`,
`diferenca_detectavel_pp` e `sustentavel`.
]---", r"---[
tr_flow(reg) |>
  tr_add("perfil", "sampling/example", dataset = "perfil_escolas") |>
  tr_add("dmd", "sampling/detectable_difference", variavel = "variavel", grupo = "categoria",
         tamanho = "participacao", n_total = 675, deff = 1.5, from = "perfil")
]---", r"---[
`sampling/size_domains` para o n que torna cada grupo legível;
`sampling/margin_levels`.
]---")),

    trama::tr_node("sampling/plot_margins", fn = tr_sampling_plot_margins, label = "Gráfico das margens",
      category = "amostra_precisao", icon = trama::tr_icon("chart-no-axes-column"),
      description = "Margem de erro por nível e cenário, com a linha da meta.",
      inputs = list(margens = T), outputs = list(out = "view/plot"),
      params = .tr_sampling_props(meta = N(5, min = 0, max = 100, label = "Meta (pontos)")),
      help = .tr_sampling_ajuda(r"---[
Desenha a tabela de `sampling/margin_levels` ou de `sampling/referral`: uma
linha de painéis por tipo de nível (total, cada nível intermediário, unidades),
a margem de cada nível em pontos percentuais no eixo horizontal, e a meta como
linha tracejada. Os níveis vêm ordenados da menor margem (em cima) para a maior.

Com a tabela de `sampling/referral`, a cor é o número de convidados e há uma
coluna de painéis por ICC: o que está à esquerda da linha tracejada atinge a
meta. Para muitas unidades, escolha a proporção `3:4` ou `1:1`.
]---", r"---[
- **Meta** — a margem máxima aceitável, em pontos percentuais.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("escolas", "sampling/example", dataset = "escolas_resumo") |>
  tr_add("n", "data/mutate", name = "n", expr = "10", from = "escolas") |>
  tr_add("margens", "sampling/referral", unidade = "escola", tamanho = "alunos", n = "n",
         grupos = "rede", convidados = "0, 2, 4", icc = "0.1", from = "n") |>
  tr_add("graf", "sampling/plot_margins", meta = 5, from = "margens")
]---", r"---[
`sampling/margin_levels`; `sampling/referral`.
]---", grafico = TRUE))
  )
}

.tr_sampling_nos_perguntas <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; N <- trama::tr_param_num
  list(
    trama::tr_node("sampling/question_margins", fn = tr_sampling_question_margins, label = "Margem por pergunta",
      category = "amostra_precisao", icon = trama::tr_icon("list-checks"),
      description = "A margem de erro de pior caso de cada pergunta do questionário, em cada nível e cenário.",
      inputs = list(perguntas = "data/table", margens = "data/table"), outputs = list(out = "data/table"),
      params = list(
        pergunta = P("cols", "pergunta", label = "Coluna da pergunta", example = "pergunta"),
        tipo = P("cols", "tipo", label = "Coluna do tipo", example = "tipo"),
        opcoes = P("cols", "opcoes", label = "Coluna do nº de opções", example = "opcoes"),
        base = P("cols", "base", label = "Coluna da base", example = "base"),
        nao_resposta = N(0, min = 0, max = 0.9, step = 0.01, label = "Não resposta esperada"),
        confianca = E("95%", .TR_SAMPLING_CONFIANCAS, label = "Confiança")),
      pressupostos = c(list(trama::tr_pressuposto(
        "A **não resposta** e a base condicional só reduzem o n: quem pula ou não responde se parece com quem responde.",
        se_falhar = "Se a não resposta depende do tema, a margem subestima o erro; calibre com `sampling/rake`."),
        trama::tr_pressuposto("As margens de diferença e de todos os pares valem para pares **definidos antes** de olhar os dados; a de todos os pares usa Bonferroni, conservadora.",
          se_falhar = "Para escolher o par depois de ver os dados, use `margem_todos_pares_pp`.")),
        .tr_sampling_press_margem(), list(trama::tr_pressuposto("Os níveis vêm de uma tabela de margens da mesma pesquisa (n e deff por nível).",
          verificar = "sampling/margin_levels", se_falhar = "Gere os níveis com `sampling/margin_levels` ou `sampling/referral`."))),
      referencias = list(.tr_sampling_refs()$thompson, .tr_sampling_refs()$cochran,
        .tr_sampling_impl("tr_sampling_question_margins", "Pior caso por tipo com z normal: p = 0,5; simultânea de Thompson (max sobre m de z_{α/(2m)}·√((1/m)(1 − 1/m)/n)); diferença √(deff/n); todos os pares com Bonferroni sobre k(k − 1)/2; escala com desvio (k − 1)/2.")),
      help = .tr_sampling_ajuda(r"---[
Cruza o QUESTIONÁRIO com os níveis de uma tabela de margens (de
`sampling/margin_levels` ou `sampling/referral`) e dá, para cada pergunta em
cada nível, a margem de erro no pior caso do TIPO dela. Os cenários de
indicação, se vierem, são mantidos.

A tabela de perguntas tem uma linha por pergunta (ou por item de uma bateria):

| coluna | o que é |
|---|---|
| `pergunta` | o texto ou o código da pergunta |
| `tipo` | `binária`, `única`, `múltipla`, `escala`, `numérica` ou `aberta` |
| `opcoes` | quantas alternativas (ou pontos da escala) |
| `base` | fração da amostra que responde: 1 para todos, menos nas condicionais |

O pior caso de cada tipo, do mais preciso para o menos — uma alternativa, todas
ao mesmo tempo, uma contra outra, todas as comparações:

- **binária** (sim/não) e **múltipla** (marque todas: cada opção é um sim/não
  próprio) — p = 0,5: `margem_pp`.
- **única** (uma entre k) — cada alternativa sozinha tem a mesma `margem_pp`;
  mas para que TODAS as k proporções da distribuição fiquem dentro da margem ao
  mesmo tempo, vale a `margem_simultanea_pp` (Thompson 1987), maior. É ela que
  sustenta frases como "a ordem das alternativas é esta".
- **uma alternativa contra outra** (única, escala e múltipla) — a
  `margem_diferenca_pp`, o DOBRO da margem de uma proporção. As duas
  alternativas vêm das mesmas pessoas e disputam a mesma resposta: quando o
  acaso põe gente a mais em A, tira de B, e numa diferença esse movimento
  contrário se soma. O pior caso (as duas com 50%) dá variância 1/n. É a
  margem de frases como "preço preocupa mais que agrotóxico", para UM par
  escolhido antes de olhar os dados.
- **todas as comparações de uma vez** — a `margem_todos_pares_pp`, com
  Bonferroni sobre os k(k − 1)/2 pares. É a de "a ordem das alternativas é
  esta" ou de procurar, depois de ver os dados, qual par difere.
- **escala** (Likert de k pontos) — as margens da única, e a
  `margem_media_escala`, em pontos da escala, com o pior desvio possível
  (k − 1)/2.
- **numérica** — tratada como sim/não por faixa; categorize antes.
- **aberta** — sem margem.

**A base é o que mais pesa.** Uma pergunta que só quem trabalha responde, numa
população em que metade trabalha, tem a margem de uma amostra com metade do
tamanho: √2 vezes pior. Uma que só 20% respondem, √5 vezes pior. A **não
resposta esperada** ("prefiro não responder") desconta de todas as fechadas.

`confiavel` marca as linhas com pelo menos 30 respondentes: abaixo disso a
aproximação normal da margem já não vale, e o número não deve ir para o
relatório.
]---", r"---[
- **Coluna da pergunta**, **do tipo**, **do nº de opções**, **da base** —
  colunas da tabela de perguntas (as duas últimas podem ficar em branco).
- **Não resposta esperada** — fração de "prefiro não responder" nas fechadas.
- **Confiança** — 90%, 95% ou 99%.
]---", r"---[
Uma tabela (`data/table`): `pergunta`, `tipo`, `opcoes`, `base`, `nivel_tipo`,
`nivel`, `n_respondentes`, `deff`, `margem_pp`, `margem_simultanea_pp`,
`margem_diferenca_pp`, `margem_todos_pares_pp`, `margem_media_escala` e
`confiavel`, precedidas das colunas de cenário.
]---", r"---[
tr_flow(reg) |>
  tr_add("escolas", "sampling/example", dataset = "escolas_resumo") |>
  tr_add("n", "data/mutate", name = "n", expr = "10", from = "escolas") |>
  tr_add("niveis", "sampling/margin_levels", unidade = "escola", tamanho = "alunos", n = "n",
         grupos = "rede", from = "n") |>
  tr_add("perguntas", "sampling/example", dataset = "perguntas_exemplo") |>
  tr_add("margens", "sampling/question_margins", nao_resposta = 0.05, from = "perguntas") |>
  tr_link("niveis", "margens:margens")
]---", r"---[
`sampling/margin_levels`; `sampling/referral`; `sampling/margin` para uma
célula.
]---"))
  )
}

.tr_sampling_nos_domains <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; N <- trama::tr_param_num
  list(
    trama::tr_node("sampling/size_domains", fn = tr_sampling_size_domains, label = "Tamanho por grupos",
      category = "amostra_planejar", icon = trama::tr_icon("users"),
      description = "Quantas entrevistas no total para que CADA grupo de perfil tenha a margem desejada, sem cotas?",
      inputs = list(composicao = "data/table"), outputs = list(out = "sampling/plan"),
      params = list(
        variavel = P("cols", "", label = "Variável", example = "variavel"),
        grupo = P("cols", "", label = "Grupo", example = "categoria"),
        participacao = P("cols", "", label = "Participação", example = "participacao"),
        erro = N(0.05, min = 0.005, max = 0.5, step = 0.01, label = "Margem em cada grupo"),
        proporcao = N(0.5, min = 0.01, max = 0.99, step = 0.05, label = "Proporção esperada"),
        confianca = E("95%", .TR_SAMPLING_CONFIANCAS, label = "Confiança"),
        deff = N(1, min = 0.01, step = 0.1, label = "Efeito do desenho (deff)"),
        taxa_resposta = N(1, min = 0.01, max = 1, step = 0.05, label = "Taxa de resposta"),
        participacao_minima = N(0, min = 0, max = 1, step = 0.01, label = "Ignorar grupos abaixo de")),
      pressupostos = list(
        trama::tr_pressuposto("Sem cotas, a coleta traz cada grupo **na proporção da população** informada.", se_falhar = "Use a composição de uma coleta anterior ou do piloto; coleta aberta costuma atrair uns grupos mais que outros."),
        trama::tr_pressuposto("Dentro de cada grupo, as respostas se comportam como **amostra aleatória** do grupo, com aproximação normal da proporção.", se_falhar = "Leia a margem como nominal e calibre com `sampling/rake`.")),
      referencias = list(.tr_sampling_refs()$cochran, .tr_sampling_refs()$lohr,
        .tr_sampling_impl("tr_sampling_size_domains", "n por grupo = z²·p(1 − p)/E² × deff; total = maior n_g/participação_g entre os grupos considerados; ÷ taxa de resposta; sem correção finita.")),
      help = .tr_sampling_ajuda(r"---[
O tamanho da amostra quando a margem tem de valer DENTRO de cada grupo de
perfil (cada sexo, cada raça/cor, cada faixa de renda) e não há cotas.

Para ±5 pontos, cada grupo precisa de uns 385 respondentes. Sem cota, o grupo
aparece na coleta na proporção em que existe na população, e o total tem de ser
385 ÷ participação. Quem manda no tamanho da pesquisa é o MENOR grupo que se
quer ler: com um grupo de 2%, o total passa de 19 mil. A escada do card mostra
o grupo limitante; a vista `alocação` e a tabela, o que cada grupo exigiria e a
margem que ele teria no total escolhido.

Um alerta que a conta não resolve:

- **Grupos muito pequenos** (indígenas e amarelos, em muitos recortes) exigem
  totais inviáveis. **Ignorar grupos abaixo de** os tira da conta, e a nota diz
  quais ficaram fora: a pesquisa não terá margem para eles, e isso deve ir para
  o relatório.
]---", r"---[
- **Variável**, **Grupo**, **Participação** — colunas da tabela longa de
  composição (a participação soma 1 dentro de cada variável).
- **Margem em cada grupo** — em proporção (0,05 = 5 pontos).
- **Proporção esperada**, **Confiança**, **Efeito do desenho**, **Taxa de
  resposta** — como em `sampling/size_proportion`.
- **Ignorar grupos abaixo de** — participação mínima para entrar na conta.
]---", r"---[
Um plano (`sampling/plan`) cujo n é o total necessário. O adaptador para
`data/table` dá uma linha por grupo: `participacao`, `n` (o necessário no grupo),
`n_total_necessario` (o total que ele exigiria), `n_final` (quantos ele teria no
total do plano), `margem_esperada_pp`, `considerado` e `limitante`.
]---", r"---[
tr_flow(reg) |>
  tr_add("perfil", "sampling/example", dataset = "perfil_escolas") |>
  tr_add("plano", "sampling/size_domains", variavel = "variavel", grupo = "categoria",
         participacao = "participacao", erro = 0.05, from = "perfil")
]---", r"---[
`sampling/detectable_difference` para comparar os grupos; `sampling/size_proportion`
para a margem só no total; `sampling/rake` para calibrar depois.
]---"))
  )
}

.tr_sampling_nos_rake <- function() {
  P <- trama::tr_param; I <- trama::tr_param_int
  list(
    trama::tr_node("sampling/rake", fn = tr_sampling_rake, label = "Calibrar (raking)",
      category = "amostra_desenho", icon = trama::tr_icon("sliders-horizontal"),
      description = "Ajusta os pesos para que a amostra bata os totais conhecidos de várias variáveis ao mesmo tempo.",
      inputs = list(amostra = "sampling/sample", totais = "data/table"), outputs = list(out = "sampling/sample"),
      params = list(
        variavel = P("cols", "variavel", label = "Coluna da variável", example = "variavel"),
        categoria = P("cols", "categoria", label = "Coluna da categoria", example = "categoria"),
        total = P("cols", "total", label = "Coluna do total", example = "total"),
        iteracoes = I(50L, min = 1L, max = 1000L, label = "Rodadas")),
      pressupostos = list(
        trama::tr_pressuposto("Os totais das **margens** são exatos e consistentes (todas as variáveis somam a mesma população), e toda categoria tem gente na amostra.", se_falhar = "Junte categorias vazias antes; totais de fontes diferentes precisam ser reconciliados."),
        trama::tr_pressuposto("A não resposta (ou a seleção) é **ignorável dadas as variáveis calibradas** — o modelo é o de efeitos principais, sem interação entre elas.", se_falhar = "Inclua a variável que explica a participação, ou um cruzamento dela, entre as calibradas."),
        trama::tr_pressuposto("A variância linearizada supõe amostra **probabilística**; em coleta não probabilística ela é nominal.", verificar = "sampling/simulate", se_falhar = "Leia o erro como nominal e reporte a amplitude dos pesos da nota.")),
      referencias = list(.tr_sampling_refs()$deming, .tr_sampling_refs()$deville, .tr_sampling_refs()$lohr,
        .tr_sampling_impl("tr_sampling_rake", "Ajuste proporcional iterativo (uma variável por rodada, até as margens baterem ou o limite de rodadas); variância pelo resíduo da regressão ponderada nas indicadoras de todas as variáveis calibradas.")),
      help = .tr_sampling_ajuda(r"---[
A calibração por várias variáveis: os pesos são ajustados para que a amostra
tenha, ao mesmo tempo, o total conhecido de cada categoria de cada variável —
sexo, raça/cor, faixa etária, capital. É o ajuste proporcional iterativo
(raking): uma variável por vez, em rodadas, até todas baterem.

Diferente de `sampling/poststratify`, não precisa do total de cada CRUZAMENTO
(mulher preta de 18 a 24 anos em Belém), que o Censo raramente publica e a
amostra raramente preenche: bastam as margens de cada variável.

A tabela de **totais** é longa, uma linha por categoria: a coluna da variável
tem o NOME da coluna da amostra (`sexo`), a da categoria o valor (`mulher`), e a
do total a população. Todas as variáveis têm de somar a mesma população (tolerância
de 1%), e toda categoria precisa de gente na amostra: junte as vazias antes.

Numa coleta não probabilística (indicação, adesão aberta), calibrar é o mínimo:
não torna a amostra aleatória, mas faz o perfil dela ser o da população. A
variância usa os resíduos da regressão nas variáveis calibradas, e a nota da
amostra traz a amplitude dos pesos — pesos muito desiguais (razão acima de uns
10) inflam a variância e pedem categorias mais largas.
]---", r"---[
- **Coluna da variável**, **Coluna da categoria**, **Coluna do total** — colunas
  da tabela de totais.
- **Rodadas** — máximo de rodadas do ajuste.
]---", r"---[
A amostra (`sampling/sample`) com os pesos calibrados.
]---", r"---[
tr_flow(reg) |>
  tr_add("escolas", "sampling/example", dataset = "escolas") |>
  tr_add("perfil", "sampling/example", dataset = "perfil_escolas") |>
  tr_add("aas", "sampling/srs", n = 300L, from = "escolas") |>
  tr_add("cal", "sampling/rake", from = "aas") |>
  tr_link("perfil", "cal:totais") |>
  tr_add("nota", "sampling/mean", variavel = "nota", from = "cal")
]---", r"---[
`sampling/poststratify` para uma variável; `sampling/design` para a base
coletada; `sampling/simulate`, que refaz a calibração em cada amostra.
]---"))
  )
}
