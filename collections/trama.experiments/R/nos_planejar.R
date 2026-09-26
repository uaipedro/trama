# As declarações das abas Planejar e Ver: o delineamento e as suas vistas.

.tr_experiments_nos_planejar <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; I <- trama::tr_param_int
  PL <- "experiments/plan"
  list(
    trama::tr_node("experiments/design", version = 4L, fn = tr_experiments_design, label = "Delineamento",
      category = "exp_planejar", icon = trama::tr_icon("grid-3x3"), stochastic = TRUE,
      description = "Declara o delineamento (fatores, blocos, unidades) e sorteia a alocação.",
      pressupostos = .tr_exp_doc("experiments/design")$pressupostos,
      referencias = .tr_exp_doc("experiments/design")$referencias,
      inputs = list(), outputs = list(out = PL),
      params = list(
        estrutura = E("dbc", .TR_EXP_ESTRUTURAS, label = "Estrutura"),
        fatores = P("text", "tratamento: A, B, C, D", label = "Fatores",
                    example = "irrigacao: baixa, alta; variedade: A, B, C"),
        repeticoes = I(0L, min = 0L, max = 1000L, label = "Repetições (0 = padrão)"),
        delineamento_base = E("dbc", c("dbc", "dic"), label = "Base (fatorial, subdividida, grupos)"),
        confundir = P("text", "", label = "Confundir com blocos", example = "ABC"),
        geradores = P("text", "", label = "Geradores do fracionado", example = "D = ABC"),
        distancia_axial = P("text", "rotacional", label = "Distância axial α (composto central)",
                            example = "rotacional, face ou 1.5"),
        pontos_centrais = I(4L, min = 0L, max = 50L, label = "Pontos centrais"),
        tamanho_bloco = I(3L, min = 2L, max = 100L, label = "Parcelas por bloco (BIB)"),
        tempos = P("text", "", label = "Tempos (medidas repetidas)", example = "0, 30, 60, 90"),
        locais = I(3L, min = 1L, max = 100L, label = "Locais (grupos)"),
        colunas_grade = I(0L, min = 0L, max = 1000L, label = "Colunas da grade (0 = automática)"),
        covariaveis = P("text", "", label = "Covariáveis observadas", example = "peso_inicial")),
      help = .tr_exp_ajuda(r"---[
Declara um experimento e sorteia a alocação dos tratamentos às unidades. O
delineamento é uma descrição só, preenchida aqui; o que se aprende está em
VER o resultado em `experiments/view`: quem é unidade de quê e em que nível
cada fator foi sorteado.

Para cada fator o plano registra o **papel** (tratamento, bloco, tempo,
covariável observada), a **unidade de atribuição**, o **escopo do sorteio**
(entre quais unidades) e o **mecanismo** (livre, restrito, em estágios ou sem
sorteio, com a justificativa). Bloco, linha, coluna, local e tempo não são
sorteados: são fontes de variação controladas ou observadas.

As estruturas, e como cada uma sorteia:

- **dic** — inteiramente casualizado: os t tratamentos, r vezes cada, sorteados
  entre todas as parcelas.
- **dbc** — blocos casualizados: cada bloco recebe todos os tratamentos uma
  vez, em ordem sorteada dentro do bloco, independente dos outros blocos.
- **dql** — quadrado latino t × t, pelo procedimento de Fisher & Yates:
  sorteia um quadrado padrão (1ª linha e 1ª coluna em ordem) entre todos os
  da ordem t — 1, 1, 4, 56 e 9408 para t = 2 a 6 —, depois permuta as linhas,
  as colunas e a associação letra → tratamento. Até t = 6 o quadrado sai
  uniforme entre TODOS os quadrados latinos t × t. De t = 7 em diante os
  padrões são milhões (16.942.080 com t = 7) e o quadrado vem de t³ passos da
  cadeia de Jacobson & Matthews (1996), cuja distribuição limite é a uniforme:
  o sorteio é APROXIMADAMENTE uniforme, e o plano avisa. Cada tratamento fica
  uma vez em cada linha e em cada coluna.
- **fatorial** — as combinações de 2 ou mais fatores como tratamentos, em DIC
  ou DBC (**Base**).
- **confundimento** — 2^k em blocos incompletos: os efeitos em **Confundir com
  blocos** (e as suas interações generalizadas) ficam confundidos com blocos.
  Cada combinação vai ao bloco dado pela paridade das letras do efeito; a
  ordem dos blocos em cada repetição e das parcelas em cada bloco é sorteada.
- **fracionado** — 2^(k−p): os fatores básicos em ordem padrão e os demais
  pelos **Geradores**. O plano traz a relação de definição, a resolução e os
  aliases dos efeitos principais e das interações duplas. A ordem das corridas
  é sorteada; a coluna `padrao` guarda a ordem padrão. As colunas dos fatores
  saem SEMPRE codificadas, −1 e +1, mesmo com níveis nomeados: −1 é o 1º nível
  declarado e +1 o 2º (`A: baixo, alto` dá −1 = baixo). Os nomes ficam no
  metadado (`fatores$niveis`, e a tabela `extras$codificacao`).
- **composto_central** — porção fatorial, 2k pontos axiais em ±α e os
  **Pontos centrais**. A porção fatorial é o 2^k completo ou, com
  **Geradores**, a fração 2^(k−p) que eles definem, que precisa ter
  resolução V ou mais (com 5 fatores, `E = ABCD` dá 16 pontos em vez de 32).
  α rotacional = n_F^(1/4), com n_F os pontos da porção fatorial (2 com
  n_F = 16), 1 (face) ou o número dado em **Distância axial**. Escala
  codificada; a ordem das corridas é sorteada.
- **parcela_subdividida** — o 1º fator na parcela, sorteado no bloco (ou entre
  todas as parcelas, com Base `dic`); o 2º na subparcela, sorteado DENTRO de
  cada parcela: dois estágios, dois erros.
- **faixas** — o 1º fator em faixas horizontais e o 2º em faixas verticais,
  cada um sorteado no bloco, independentemente.
- **bib** — blocos incompletos balanceados de k = **Parcelas por bloco**. Do
  menor λ admissível (r = λ(t−1)/(k−1) e b = rt/k inteiros, b ≥ t) para
  cima, procura: BIB cíclico (conjunto de diferenças, b = t); família de 2
  ou 3 blocos iniciais desenvolvidos mod t ou mod t − 1 com um ponto fixo;
  uma pequena tabela de BIBs conferidos ((10, 4), b = 15); busca exaustiva
  com limite de passos (b ≤ 60); e, com k > t/2, o complementar do BIB de
  t − k. Ex.: t = 6, k = 3 dá 10 blocos (r = 5, λ = 2); 9 e 3 dão 12; 8 e 4
  dão 14; 10 e 4 dão 15. Sem nada menor, usa todos os C(t, k) blocos, com
  aviso, ou recusa se passam de 300. Nem sempre o b achado é o menor que
  existe: as construções são finitas. O plano traz b, r, λ, a construção e o
  fator de eficiência λt/(rk). Sorteia os rótulos, a ordem dos blocos e a
  posição no bloco.
- **medidas_repetidas** — os tratamentos sorteados aos indivíduos (DIC entre
  indivíduos) e cada indivíduo medido em todos os **Tempos**, na ordem do tempo.
  A análise sugerida, `(1 | individuo)`, supõe a mesma correlação entre
  quaisquer dois tempos do indivíduo (simetria composta, que implica a
  esfericidade). Quando a correlação cai com a distância no tempo — o AR(1)
  que `experiments/error` simula —, esse modelo não a representa, e os testes
  do tempo e da interação ficam liberais; aí vale um modelo com estrutura de
  covariância no tempo.
- **crossover** — sequências de Williams: com t par, um quadrado; com t ímpar,
  o quadrado e o espelho (2t sequências). Cada tratamento é precedido por cada
  outro o mesmo número de vezes, o balanço para o efeito residual. As
  sequências são sorteadas aos indivíduos. A coluna `residual` é o
  tratamento do período anterior; no 1º período, onde não há residual, ela
  leva o primeiro tratamento (a referência). Como os efeitos residuais só se
  estimam como diferenças e o efeito do período absorve a constante do 1º
  período, isso equivale a λ = 0 no 1º período (Jones & Kenward, 2014) e
  deixa a matriz com posto completo. A análise sugerida é
  `~ periodo + trat + residual + (1 | individuo)`: o residual tem t − 1
  graus de liberdade, e cada coeficiente é a diferença para o residual do
  primeiro tratamento. Ao simular um residual com `experiments/effect`, dê
  efeito 0 ao primeiro tratamento.
- **grupos** — o mesmo delineamento (DIC, DBC ou fatorial) em cada um dos
  **Locais**, com sorteio independente por local. Na análise sugerida cada
  termo de tratamento — efeitos principais e todas as interações — ganha a
  sua interação aleatória com o local (`(1 | local:A)`, `(1 | local:A:B)`...),
  para que nenhum seja testado contra o resíduo dentro do local.

Recusas (card vermelho): fator com nome de coluna estrutural, número errado de
fatores, estrutura sem grau de liberdade para o resíduo (quadrado latino 2 × 2,
DBC de um bloco), gerador malformado, efeito principal confundido com blocos.
Avisos (no plano, sem bloquear): interação dupla confundida, resolução III,
modelo sem resíduo no fracionado e no confundimento, BIB não reduzido, quadrado latino com
t ≥ 7 (aproximadamente uniforme).
]---", r"---[
- **Estrutura** — uma das treze acima.
- **Fatores** — `nome: nível, nível; nome: nível, ...`. `dose: 4` cria os
  níveis 1 a 4. No fracionado e no composto central basta o nome (escala
  −1/+1); as letras A, B, C… dos geradores seguem a ordem declarada.
- **Repetições** — repetições do DIC, blocos do DBC, da parcela subdividida e
  das faixas, réplicas da matriz no confundimento, no fracionado e no composto
  central, cópias do BIB, indivíduos por tratamento (medidas repetidas) ou por
  sequência (crossover), blocos por local (grupos). 0 usa o padrão da
  estrutura; no quadrado latino é ignorado.
- **Base** — `dbc` ou `dic`: fatorial, parcela subdividida e grupos.
- **Confundir com blocos** — efeitos em letras, `ABC` ou `ABC; ABD`.
- **Geradores do fracionado** — `D = ABC; E = -ABD`. No composto central,
  definem a fração da porção fatorial (resolução V ou mais).
- **Distância axial α (composto central)** — `rotacional` (n_F^(1/4)), `face`
  (1) ou um número positivo (`1.5`).
- **Pontos centrais**.
- **Parcelas por bloco (BIB)** — k.
- **Tempos (medidas repetidas)** — `0, 30, 60`.
- **Locais (grupos)**.
- **Colunas da grade** — largura da grade no DIC, no fracionado e no composto
  central (0 = quase quadrada).
- **Covariáveis observadas** — nomes de colunas a medir (saem vazias, `NA`).
]---", r"---[
Um plano (`experiments/plan`): a tabela de unidades, uma linha por unidade de
observação, já na ordem de execução, com as colunas estruturais (`bloco`,
`parcela`, `subparcela`, `linha`, `coluna`, `individuo`, `tempo`, `periodo`,
`sequencia`, `local`...) e uma coluna por fator; mais o metadado do plano
(fatores, hierarquia, geometria, semente, a análise sugerida). Ligado num bloco
da `data`, vira essa tabela — e, com uma coluna de resposta, vai direto a
`models/anova_dic`, `models/anova_dbc`, `models/anova_dql`,
`models/anova_factorial`, `models/anova_split_plot`, `models/lm` ou
`models/lmer`. O card mostra o croqui (o mapa).
]---", r"---[
tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "parcela_subdividida",
         fatores = "irrigacao: baixa, alta; variedade: A, B, C", repeticoes = 4L) |>
  tr_add("mapa", "experiments/view", aba = "hierarquia", from = "plano")
]---", r"---[
`experiments/view` para o mapa, a hierarquia, as combinações e a ordem;
`models/anova_split_plot` e as demais ANOVAs para analisar.
]---", semente = TRUE)),

    trama::tr_node("experiments/view", fn = tr_experiments_view, label = "Ver o plano",
      category = "exp_planejar", role = "leitura", icon = trama::tr_icon("layout-grid"),
      description = "Mapa, hierarquia, combinações ou ordem de execução de um plano.",
      pressupostos = .tr_exp_doc("experiments/view")$pressupostos,
      referencias = .tr_exp_doc("experiments/view")$referencias,
      inputs = list(plano = PL), outputs = list(out = "view/plot"),
      params = c(list(aba = E("mapa", .TR_EXP_ABAS, label = "Aba"),
                      por = E("tratamento", .TR_EXP_POR, label = "Componentes por")), .tr_exp_props()),
      help = .tr_exp_ajuda(r"---[
Desenha um plano de `experiments/design` (ou o que sai de `experiments/effect`
e `experiments/error`) por um dos cinco lados:

- **mapa** — a grade 2D (linhas × colunas) com o tratamento de cada unidade e
  o contorno dos blocos. No fracionado e no composto central a grade é a
  sequência das corridas; nas medidas repetidas e no crossover, indivíduo ×
  tempo (período).
- **hierarquia** — os níveis de unidade, do mais alto ao mais fino, com cada
  fator ao lado do nível em que é aplicado e o mecanismo e o escopo do sorteio.
  Fator sem sorteio (bloco, tempo, covariável) sai em cinza.
- **combinacoes** — a grade completa dos níveis dos tratamentos com o número de
  réplicas de cada combinação; célula sem réplica aparece como "vazia" (no
  fracionado, é a fração que ficou de fora).
- **ordem** — o tratamento de cada unidade na ordem de execução.
- **componentes** — a resposta simulada decomposta: uma barra empilhada com a
  contribuição de cada termo (`.ef_*`), e o resíduo quando o erro é normal. O
  intercepto sai da pilha e vai ao subtítulo; o ponto é a resposta menos o
  intercepto. Por **tratamento**, cada barra é a média de cada termo na
  combinação de tratamentos; por **unidade**, uma barra por unidade, na ordem
  de execução. Fora da normal, os termos estão na escala do preditor linear.
  Pede um plano com termos.
]---", r"---[
- **Aba** — `mapa`, `hierarquia`, `combinacoes`, `ordem` ou `componentes`.
- **Componentes por** — `tratamento` ou `unidade` (só na aba `componentes`).
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "fracionado", fatores = "A; B; C; D",
         geradores = "D = ABC") |>
  tr_add("comb", "experiments/view", aba = "combinacoes", from = "plano")
]---", r"---[
`experiments/design`, `experiments/effect`, `experiments/error`.
]---", grafico = TRUE))
  )
}
