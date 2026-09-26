# As declarações dos nós de avaliação: `experiments/power` (antes do campo,
# sobre a resposta simulada) e `experiments/randomization_test` (depois, sobre
# a resposta observada). Os dois repetem a análise de models muitas vezes.

.tr_experiments_nos_avaliar <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; I <- trama::tr_param_int; N <- trama::tr_param_num
  PL <- "experiments/plan"; T <- "data/table"
  alvo <- list(
    analise = P("text", "", label = "Análise (nó de models)", example = "models/anova_factorial"),
    parametros = P("text", "", label = "Params da análise", example = "fatores = irrigacao, variedade; bloco = bloco"),
    termo = P("text", "", label = "Termo testado", example = "irrigacao"),
    conjunto = E("nenhum", .TR_EXP_AV_CONJUNTOS, label = "Contraste: conjunto"),
    contraste = P("text", "", label = "Contraste: linha", example = "linear"),
    contrastes = P("expr", "", label = "Contrastes (digitados)", example = "A vs B: A - B"),
    controle = P("text", "", label = "Controle", example = "testemunha"),
    doses = P("text", "", label = "Doses (valores dos níveis)", example = "0 50 100 150"))
  list(
    trama::tr_node("experiments/power", version = 2L, fn = tr_experiments_power, label = "Poder",
      category = "exp_avaliar", icon = trama::tr_icon("gauge"), stochastic = TRUE,
      description = "Repete a cadeia effect → error e a análise N vezes e conta as rejeições: poder (ou erro tipo I) com IC, e a curva por nº de repetições.",
      inputs = list(plano = PL), outputs = list(out = "view/plot", tabela = T),
      params = c(alvo, list(
        replicas = I(200L, min = 10L, max = 100000L, label = "Réplicas"),
        significancia = N(0.05, min = 0.001, max = 0.2, step = 0.01, label = "Significância (α)"),
        confianca = N(0.95, min = 0.5, max = 0.999, step = 0.01, label = "Confiança do IC"),
        repeticoes = P("text", "", label = "Grade de repetições", example = "3, 4, 6, 8")),
        .tr_exp_an_props(.aspecto = "4:3")),
      help = .tr_exp_ajuda_power()),

    trama::tr_node("experiments/randomization_test", version = 2L, fn = tr_experiments_randomization_test,
      label = "Teste de aleatorização", category = "exp_avaliar", icon = trama::tr_icon("shuffle"), stochastic = TRUE,
      description = "Re-sorteia a alocação pela receita do delineamento, mantendo a resposta de cada unidade, e situa o F observado na distribuição do sorteio.",
      inputs = list(plano = PL, dados = trama::tr_port(T, required = FALSE)),
      outputs = list(out = "view/plot", tabela = T, distribuicao = T),
      params = c(list(resposta = P("text", "", label = "Resposta", example = "y")), alvo, list(
        replicas = I(999L, min = 19L, max = 100000L, label = "Re-sorteios"),
        metodo = E("automático", .TR_EXP_AV_METODOS, label = "Método")),
        .tr_exp_an_props(.aspecto = "4:3")),
      help = .tr_exp_ajuda_randomization())
  )
}

.tr_exp_ajuda_alvo <- function() r"---[
- **Análise (nó de models)** — em branco, a do plano (`plano$analise`, a que o
  `experiments/error` sugeriu). Outro id (`models/anova_factorial`) troca a
  análise; os params dela vêm de **Params da análise**.
- **Params da análise** — `nome = valor; nome = valor`, sobrescrevendo os do
  plano. A resposta é posta sozinha quando o nó tem `resposta`.
- **Termo testado** — a linha do quadro de ANOVA (`models/anova_table`, SQ
  tipo I) cujo p conta: `irrigacao`, `irrigacao:variedade`. Em branco, o
  primeiro fator de tratamento do plano.
- **Contraste: conjunto** — `nenhum` testa o F do termo; um conjunto do
  `experiments/contrasts` testa o F (1 gl) de uma linha dele, com o **Termo**
  como fator contrastado.
- **Contraste: linha** — o nome da linha (`linear`, `quadratico`).
- **Contrastes (digitados)**, **Controle**, **Doses** — como no
  `experiments/contrasts`.
]---"

.tr_exp_ajuda_power <- function() paste0(.tr_exp_an_ajuda(r"---[
Poder por **simulação de Monte Carlo**, sem fórmula fechada por delineamento:
o bloco refaz a cadeia que o plano guarda — o mesmo `experiments/design`, cada
`experiments/effect` com os seus argumentos e o `experiments/error` —,
**Réplicas** vezes com sementes derivadas, roda a análise em cada resposta
simulada e conta em quantas o p do teste ficou abaixo de **Significância**.
A taxa sai com o IC exato de **Clopper-Pearson** no nível **Confiança**.

O que a taxa é depende do modelo declarado, e a tabela diz qual é:

- **H0 falsa** (o termo tem efeito na parte fixa declarada): a taxa é o
  **poder**.
- **H0 verdadeira** (efeito declarado zero): a taxa é o **erro tipo I**, que
  deve cair perto de α. Se não cai, a análise usa o erro errado: na parcela
  subdividida com erro de parcela, a análise ingênua (`models/anova_factorial`,
  que testa a parcela contra o resíduo das subparcelas) rejeita muito mais que
  5%, e a de parcela subdividida fica em 5%.

A conferência de H0 lê a parte fixa da resposta (intercepto, fixo, interação,
quantitativo): médias por nível iguais no efeito principal, médias de célula
aditivas na interação, Σ cᵢ mᵢ = 0 no contraste. Termo aleatório e covariável
não entram nela.

### Curva de poder

**Grade de repetições** (`3, 4, 6, 8`) refaz o delineamento com cada valor no
param `repeticoes` do `experiments/design` (repetições no DIC, blocos no DBC)
e desenha a taxa contra ele. As sementes das réplicas são as mesmas em todo
ponto da grade (números aleatórios comuns): a curva fica mais lisa, e cada
ponto continua estimando o poder daquele tamanho.

### Custo

Um ajuste por réplica e ponto da grade: 200 réplicas de uma ANOVA levam
poucos segundos. O erro de Monte Carlo da taxa é √(p(1 − p)/R): com R = 200 e
p = 0,5, 0,035; o IC já o mostra.
]---", r"---[
Os do modelo declarado na cadeia: é ele que gera cada resposta. O resultado
vale para esse modelo — poder com σ declarado maior ou menor que o real é
poder de outro experimento. O plano precisa guardar os argumentos dos termos
(desde a versão 0.2.0 da coleção).
]---", paste0(.tr_exp_ajuda_alvo(), r"---[
- **Réplicas** — respostas simuladas por ponto da grade.
- **Significância (α)** — rejeita quando p < α.
- **Confiança do IC** — nível do intervalo de Clopper-Pearson da taxa.
- **Grade de repetições** — valores do param `repeticoes` do design; em
  branco, só o tamanho do plano.
]---"), r"---[
`out`: o gráfico da taxa de rejeição (ponto e IC; linha na grade), com α
tracejado. `tabela`: uma linha por ponto da grade — `repeticoes`, `unidades`,
`replicas` (as que ajustaram), `falhas`, `rejeicoes`, `taxa`, `li`, `ls`,
`confianca`, `significancia`, `p_binomial`, `hipotese` (H0 verdadeira ou
falsa no modelo declarado), `analise` e `teste`. Com H0 verdadeira,
`p_binomial` é o p do teste binomial exato de "taxa = α" (`binom.test(rejeicoes,
replicas, p = significancia)`): p pequeno diz que o teste não mantém o tipo I
nominal, e não ruído de Monte Carlo (o critério de Oliveira & Ferreira, 2010);
com H0 falsa, NA.
]---", r"---[
tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "dic", fatores = "t: A, B, C, D", repeticoes = 4L) |>
  tr_add("mu", "experiments/effect", tipo = "intercepto", valor = 10, from = "plano") |>
  tr_add("t", "experiments/effect", tipo = "fixo", fator = "t", efeitos = "A = 0, B = 0, C = 1, D = 2",
         from = "mu") |>
  tr_add("y", "experiments/error", sd = 1, from = "t") |>
  tr_add("poder", "experiments/power", replicas = 20L, repeticoes = "3, 5", from = "y")
]---", r"---[
- Oliveira, I. R. C.; Ferreira, D. F. Multivariate extension of chi-squared
  univariate normality test. *Journal of Statistical Computation and
  Simulation*, v. 80, n. 5, p. 513–526, 2010. DOI: 10.1080/00949650902731377.
- Clopper, C. J.; Pearson, E. S. The use of confidence or fiducial limits
  illustrated in the case of the binomial. *Biometrika*, v. 26, n. 4,
  p. 404–413, 1934. DOI: 10.1093/biomet/26.4.404.
- Montgomery, D. C. *Design and Analysis of Experiments*. 9. ed. Hoboken:
  Wiley, 2017. (Poder pelo F não central, cap. 3 — o oráculo dos testes.)
- Gelman, A.; Hill, J. *Data Analysis Using Regression and
  Multilevel/Hierarchical Models*. Cambridge: Cambridge University Press, 2007.
  (Poder por simulação de dados falsos, cap. 20.)
]---", r"---[
`experiments/effect`, `experiments/error`, `experiments/contrasts`,
`experiments/randomization_test`, `models/anova_table`,
`models/anova_split_plot`.
]---", grafico = TRUE), .tr_exp_ajuda_semente_sim())

.tr_exp_ajuda_randomization <- function() paste0(.tr_exp_an_ajuda(r"---[
O **teste de aleatorização** de Fisher: a justificativa do teste vem do
sorteio que foi de fato feito, e não da normalidade. Sob a hipótese nula
**exata** — o tratamento não muda a resposta de nenhuma unidade —, cada
unidade teria dado a mesma resposta com qualquer outra alocação que o sorteio
poderia ter produzido. O bloco então re-sorteia a alocação **pela mesma
receita** do `experiments/design` (`tr_experiments_randomize`), mantém a
resposta de cada unidade, recalcula a estatística e situa a observada nessa
distribuição.

O conjunto de re-sorteios é o das alocações **admissíveis** do delineamento,
não o de todas as permutações: no DBC o tratamento só troca dentro do bloco;
na parcela subdividida a parcela só troca de lugar com parcela do mesmo bloco
e a subparcela dentro da parcela; no quadrado latino sai outro quadrado. A
tabela repete o escopo de cada fator.

### Estatística

O F do **Termo** no quadro de ANOVA da análise (a do plano, ou outra), ou o F
(1 gl, bilateral) de uma linha de contraste do `experiments/contrasts`.

### p-valor

- **exato** — enumera todas as alocações admissíveis (DIC, DBC e fatorial
  neles, até 100 000) e dá a proporção com estatística ≥ a observada; a
  observada está entre elas.
- **Monte Carlo** — com R re-sorteios e b deles ≥ a observada,
  p = (b + 1)/(R + 1), que nunca é zero e tem nível exato (Phipson e Smyth,
  2010).
- **automático** — exato quando a estrutura é enumerável e há até
  **Re-sorteios** alocações; Monte Carlo fora disso.

Empates contam como "≥" (tolerância relativa de 10⁻⁸). A tabela traz também
o p do quadro (o do F de Snedecor): em dados normais os dois ficam perto
(Pitman, 1938).
]---", r"---[
Só a aleatorização: que a alocação observada saiu do sorteio descrito no plano
e que, sob H0, a resposta de cada unidade não depende do tratamento que ela
recebeu. Não pede normalidade nem variâncias iguais. A hipótese testada é a
nula exata (efeito zero em toda unidade), mais forte que a igualdade de
médias.
]---", paste0(r"---[
- **Resposta** — em branco, a do plano. Com **dados** ligado, a tabela precisa
  de `unidade` (a do plano) e da resposta; se trouxer as colunas de
  tratamento, elas têm de ser a alocação do plano.
]---", .tr_exp_ajuda_alvo(), r"---[
- **Re-sorteios** — R do Monte Carlo (999 é o usual).
- **Método** — `automático`, `monte carlo` ou `exato`.
]---"), r"---[
`out`: o histograma da estatística sob re-sorteio, com a observada marcada.
`tabela`: `teste`, `estatistica`, `observado`, `p_valor`, `metodo`,
`alocacoes` (avaliadas), `admissiveis` (quantas há, quando enumerável),
`maiores_ou_iguais`, `falhas`, `p_parametrico` (o do quadro), `analise` e
`escopo`. `distribuicao`: a estatística de cada alocação.
]---", r"---[
tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "dbc", fatores = "t: A, B", repeticoes = 5L) |>
  tr_add("mu", "experiments/effect", tipo = "intercepto", valor = 10, from = "plano") |>
  tr_add("t", "experiments/effect", tipo = "fixo", fator = "t", efeitos = "A = 0, B = 1", from = "mu") |>
  tr_add("y", "experiments/error", sd = 1, from = "t") |>
  tr_add("ta", "experiments/randomization_test", from = "y")
]---", r"---[
- Fisher, R. A. *The Design of Experiments*. Edinburgh: Oliver and Boyd,
  1935. (Cap. III: os dados de Darwin em *Zea mays*.)
- Pitman, E. J. G. Significance tests which may be applied to samples from
  any populations. *Supplement to the Journal of the Royal Statistical
  Society*, v. 4, n. 1, p. 119–130, 1937. DOI: 10.2307/2984124.
- Pitman, E. J. G. Significance tests which may be applied to samples from
  any populations. III. The analysis of variance test. *Biometrika*, v. 29,
  n. 3/4, p. 322–335, 1938. DOI: 10.2307/2332008.
- Edgington, E. S.; Onghena, P. *Randomization Tests*. 4. ed. Boca Raton:
  Chapman & Hall/CRC, 2007.
- Hinkelmann, K.; Kempthorne, O. *Design and Analysis of Experiments*, v. 1.
  2. ed. Hoboken: Wiley, 2008. (Aleatorização e análise pela
  aleatorização.)
- Phipson, B.; Smyth, G. K. Permutation p-values should never be zero.
  *Statistical Applications in Genetics and Molecular Biology*, v. 9, n. 1,
  2010. DOI: 10.2202/1544-6115.1585.
- Hothorn, T.; Hornik, K.; van de Wiel, M. A.; Zeileis, A. Implementing a
  class of permutation tests: the coin package. *Journal of Statistical
  Software*, v. 28, n. 8, 2008. DOI: 10.18637/jss.v028.i08. (O oráculo dos
  testes.)
]---", r"---[
`experiments/design` (a receita do sorteio), `experiments/power`,
`experiments/contrasts`, `models/anova_table`.
]---", grafico = TRUE), .tr_exp_ajuda_semente_rt())

.tr_exp_ajuda_semente_rt <- function() "

### O sorteio

O bloco é **estocástico** no Monte Carlo: a semente é do card, e não da
sessão; a mesma semente dá os mesmos re-sorteios e o mesmo p. O exato não
sorteia. A semente do console (`set.seed()`) não é tocada.
"
