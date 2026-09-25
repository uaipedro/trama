# As declarações das abas Planejar e Ver: o delineamento e as suas vistas.

.tr_experiments_nos_planejar <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; I <- trama::tr_param_int
  PL <- "experiments/plan"
  list(
    trama::tr_node("experiments/design", fn = tr_experiments_design, label = "Delineamento",
      category = "exp_planejar", icon = trama::tr_icon("grid-3x3"), stochastic = TRUE,
      description = "Declara o delineamento (fatores, blocos, unidades) e sorteia a alocação.",
      inputs = list(), outputs = list(out = PL),
      params = list(
        estrutura = E("dbc", .TR_EXP_ESTRUTURAS, label = "Estrutura"),
        fatores = P("text", "tratamento: A, B, C, D", label = "Fatores",
                    example = "irrigacao: baixa, alta; variedade: A, B, C"),
        repeticoes = I(0L, min = 0L, max = 1000L, label = "Repetições (0 = padrão)"),
        delineamento_base = E("dbc", c("dbc", "dic"), label = "Base (fatorial, subdividida, grupos)"),
        confundir = P("text", "", label = "Confundir com blocos", example = "ABC"),
        geradores = P("text", "", label = "Geradores do fracionado", example = "D = ABC"),
        alfa = E("rotacional", c("rotacional", "face"), label = "α do composto central"),
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
- **dql** — quadrado latino t × t: parte do quadrado cíclico e sorteia a ordem
  das linhas, das colunas e a associação letra → tratamento. Cada tratamento
  fica uma vez em cada linha e em cada coluna. (Não sorteia entre TODOS os
  quadrados latinos possíveis, só entre as permutações do cíclico.)
- **fatorial** — as combinações de 2 ou mais fatores como tratamentos, em DIC
  ou DBC (**Base**).
- **confundimento** — 2^k em blocos incompletos: os efeitos em **Confundir com
  blocos** (e as suas interações generalizadas) ficam confundidos com blocos.
  Cada combinação vai ao bloco dado pela paridade das letras do efeito; a
  ordem dos blocos em cada repetição e das parcelas em cada bloco é sorteada.
- **fracionado** — 2^(k−p): os fatores básicos em ordem padrão e os demais
  pelos **Geradores**. O plano traz a relação de definição, a resolução e os
  aliases dos efeitos principais e das interações duplas. A ordem das corridas
  é sorteada; a coluna `padrao` guarda a ordem padrão.
- **composto_central** — cubo 2^k completo, 2k pontos axiais em ±α e os
  **Pontos centrais**; α rotacional = (2^k)^(1/4), ou 1 (face). Escala
  codificada; a ordem das corridas é sorteada.
- **parcela_subdividida** — o 1º fator na parcela, sorteado no bloco (ou entre
  todas as parcelas, com Base `dic`); o 2º na subparcela, sorteado DENTRO de
  cada parcela: dois estágios, dois erros.
- **faixas** — o 1º fator em faixas horizontais e o 2º em faixas verticais,
  cada um sorteado no bloco, independentemente.
- **bib** — blocos incompletos balanceados de k = **Parcelas por bloco**: um
  BIB cíclico (b = t) quando existe conjunto de diferenças; senão todos os
  C(t, k) blocos, com aviso. O plano traz b, r, λ e o fator de eficiência
  λt/(rk). Sorteia os rótulos, a ordem dos blocos e a posição no bloco.
- **medidas_repetidas** — os tratamentos sorteados aos indivíduos (DIC entre
  indivíduos) e cada indivíduo medido em todos os **Tempos**, na ordem do tempo.
- **crossover** — sequências de Williams: com t par, um quadrado; com t ímpar,
  o quadrado e o espelho (2t sequências). Cada tratamento é precedido por cada
  outro o mesmo número de vezes, o balanço para o efeito residual. As
  sequências são sorteadas aos indivíduos.
- **grupos** — o mesmo delineamento (DIC, DBC ou fatorial) em cada um dos
  **Locais**, com sorteio independente por local.

Recusas (card vermelho): fator com nome de coluna estrutural, número errado de
fatores, estrutura sem grau de liberdade para o resíduo (quadrado latino 2 × 2,
DBC de um bloco), gerador malformado, efeito principal confundido com blocos.
Avisos (no plano, sem bloquear): interação dupla confundida, resolução III,
modelo sem resíduo no fracionado, BIB não reduzido.
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
- **Geradores do fracionado** — `D = ABC; E = -ABD`.
- **α do composto central**, **Pontos centrais**.
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
]---", referencias = r"---[
- Banzatto, D. A.; Kronka, S. N. *Experimentação agrícola*. 4. ed. Jaboticabal:
  FUNEP, 2006.
- Montgomery, D. C. *Design and Analysis of Experiments*. 9. ed. Hoboken:
  Wiley, 2017.
- Cochran, W. G.; Cox, G. M. *Experimental Designs*. 2. ed. New York: Wiley,
  1957.
- Box, G. E. P.; Hunter, J. S.; Hunter, W. G. *Statistics for Experimenters*.
  2. ed. Hoboken: Wiley, 2005.
- Box, G. E. P.; Wilson, K. B. On the experimental attainment of optimum
  conditions. *Journal of the Royal Statistical Society B*, 13(1), 1–45, 1951.
- Williams, E. J. Experimental designs balanced for the estimation of residual
  effects of treatments. *Australian Journal of Scientific Research A*, 2(2),
  149–168, 1949.
- Bailey, R. A. *Design of Comparative Experiments*. Cambridge: Cambridge
  University Press, 2008.
]---", semente = TRUE)),

    trama::tr_node("experiments/view", fn = tr_experiments_view, label = "Ver o plano",
      category = "exp_planejar", role = "leitura", icon = trama::tr_icon("layout-grid"),
      description = "Mapa, hierarquia, combinações ou ordem de execução de um plano.",
      inputs = list(plano = PL), outputs = list(out = "view/plot"),
      params = c(list(aba = E("mapa", .TR_EXP_ABAS, label = "Aba")), .tr_exp_props()),
      help = .tr_exp_ajuda(r"---[
Desenha um plano de `experiments/design` por um dos quatro lados:

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
]---", r"---[
- **Aba** — `mapa`, `hierarquia`, `combinacoes` ou `ordem`.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "fracionado", fatores = "A; B; C; D",
         geradores = "D = ABC") |>
  tr_add("comb", "experiments/view", aba = "combinacoes", from = "plano")
]---", r"---[
`experiments/design`.
]---", grafico = TRUE))
  )
}
