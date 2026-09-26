# As declarações dos nós de simulação: `experiments/effect` e
# `experiments/error`. Moram na aba Planejar: a resposta simulada é o
# experimento imaginado antes de ir a campo.

.tr_experiments_nos_simular <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; I <- trama::tr_param_int; N <- trama::tr_param_num
  PL <- "experiments/plan"
  list(
    trama::tr_node("experiments/effect", version = 2L, fn = tr_experiments_effect, label = "Efeito",
      category = "exp_planejar", icon = trama::tr_icon("plus"), stochastic = TRUE,
      description = "Soma um termo à resposta simulada: intercepto, fixo (por nível ou por contraste), aleatório, interação, quantitativo ou covariável.",
      inputs = list(plano = PL), outputs = list(out = PL),
      params = list(
        tipo = E("intercepto", .TR_EXP_EF_TIPOS, label = "Tipo"),
        fator = P("text", "", label = "Fator(es)", example = "bloco:parcela"),
        valor = N(0, label = "Valor (intercepto)"),
        efeitos = P("text", "", label = "Efeitos por nível ou célula", example = "baixa = 0, alta = 2"),
        conjunto = E("por nível", .TR_EXP_EF_CONJUNTOS, label = "Declarar por"),
        magnitudes = P("text", "", label = "Magnitudes dos contrastes", example = "linear = 4, quadratico = 1, cubico = 0"),
        doses = P("text", "", label = "Doses (valores dos níveis)", example = "0, 50, 100, 150"),
        controle = P("text", "", label = "Controle", example = "testemunha"),
        contrastes = P("expr", "", label = "Contrastes (digitados)", example = "A vs B: A - B"),
        sd = N(1, min = 0, label = "Desvio-padrão (aleatório; covariável gerada)"),
        coeficientes = P("text", "", label = "Coeficientes (quantitativo)", example = "0.5, -0.02"),
        inclinacao = N(0, label = "Inclinação (covariável)"),
        media = N(0, label = "Média (covariável)"),
        nome = P("text", "", label = "Nome do termo", example = "erro_parcela")),
      help = .tr_exp_ajuda_effect()),

    trama::tr_node("experiments/error", fn = tr_experiments_error, label = "Erro",
      category = "exp_planejar", icon = trama::tr_icon("sigma"), stochastic = TRUE,
      description = "Soma os termos, sorteia o resíduo (normal, Poisson, binomial ou gama) e fecha a coluna de resposta.",
      inputs = list(plano = PL), outputs = list(out = PL),
      params = list(
        resposta = P("text", "y", label = "Resposta"),
        distribuicao = E("normal", .TR_EXP_ER_DIST, label = "Distribuição"),
        sd = N(1, min = 0, label = "Desvio-padrão (normal)"),
        sd_por = P("text", "", label = "sd por nível de", example = "irrigacao"),
        sds = P("text", "", label = "sd de cada nível", example = "baixa = 1, alta = 3"),
        ensaios = I(10L, min = 1L, max = 1000000L, label = "Ensaios (binomial)"),
        forma = N(2, min = 0, label = "Forma (gama)"),
        correlacao = E("independente", .TR_EXP_ER_COR, label = "Correlação no indivíduo"),
        rho = N(0.5, min = -1, max = 1, step = 0.05, label = "ρ"),
        caudas_gl = N(0, min = 0, label = "Caudas pesadas: gl da t (0 = não)"),
        assimetria = N(0, min = -10, max = 10, step = 0.1, label = "Assimetria (0 = não)"),
        perdidas = N(0, min = 0, max = 0.9, step = 0.05, label = "Proporção perdida")),
      help = .tr_exp_ajuda_error())
  )
}

.tr_exp_ajuda_effect <- function() paste0(.tr_exp_an_ajuda(r"---[
Soma **um** termo à resposta que o experimento vai ter, antes de ela existir.
A contribuição de cada unidade fica numa coluna própria, `.ef_<nome>`, e o
termo fica no plano com o seu **valor verdadeiro** — é contra ele que a
análise se confere depois. Encadeie um `experiments/effect` por termo e feche
com `experiments/error`. O plano que sai é o mesmo tipo que entra
(`experiments/plan`).

O **Tipo** diz quais params são lidos:

| Tipo | Lê | Contribuição |
|---|---|---|
| intercepto | Valor | μ em toda unidade |
| fixo | Fator, Efeitos **ou** Declarar por + Magnitudes | τᵢ do nível |
| aleatorio | Fator (`bloco` ou `bloco:parcela`), sd | um N(0, sd²) sorteado por nível (ou combinação) |
| interacao | Fator (`a:b`), Efeitos por célula **ou** Declarar por + Magnitudes | (τβ)ᵢⱼ da célula |
| quantitativo | Fator (numérico, codificado ou com Doses), Coeficientes | b₁x + b₂x² + … ; com `x1:x2`, b·x₁x₂ |
| covariavel | Fator (a coluna), Inclinação, Média, sd | β(x − média) |

### Por contraste

Com **Declarar por** = `polinomiais`, `helmert`, `controle` ou `digitados`, o
efeito fixo vem de um conjunto de contrastes e da **magnitude** de cada um. Os
conjuntos são os mesmos do `experiments/contrasts`, com os mesmos coeficientes
inteiros (para quatro doses igualmente espaçadas: linear `-3 -1 1 3`,
quadrático `1 -1 -1 1`, cúbico `-1 3 -3 1`).

**Escala:** a magnitude é o **valor do contraste** nesses coeficientes,
Σ cᵢ τᵢ — a mesma estimativa que o `experiments/contrasts` devolve com o
mesmo conjunto. "Linear = 4" com `-3 -1 1 3` dá τ = 4·c/20 =
(−0,6; −0,2; 0,2; 0,6). A conversão é τ = Lᵀ(LLᵀ)⁻¹m, que para contrastes
ortogonais é Σⱼ mⱼcⱼ/Σcⱼ²; os efeitos somam zero. Contraste não citado em
**Magnitudes** vale 0. A tabela da conversão (contraste, coeficientes,
magnitude, Σc², conferência Σcτ) fica em `plano$termos` e sai no `print` do
plano.

Na **interação**, `polinomiais` ou `helmert` são aplicados a cada fator e as
magnitudes são dos **produtos** (`Linear:alta vs baixa = 2`); os efeitos de
célula saem com margens nulas. Para ler a magnitude de volta, desdobre no
`experiments/contrasts` (Fator = o primeiro, Dentro de = o segundo): a
magnitude do produto é Σⱼ dⱼ · (estimativa do contraste no nível j), com dⱼ
os coeficientes do contraste do segundo fator — em `Linear:Linear` com três
níveis, a estimativa no terceiro menos a do primeiro.

### Validação contra o plano

O fator tem de ser coluna do plano (a combinação `bloco:parcela` só existe
onde há parcela); o efeito por nível cobre todos os níveis, e a tabela da
interação, todas as células presentes no plano; o nome do termo não se repete;
nenhum termo depois de `experiments/error`. Efeito aleatório com um nível por
unidade avisa que se confunde com o resíduo.
]---", r"---[
Nenhum sobre dados: é simulação. O que se declara é o modelo verdadeiro; o
efeito aleatório é normal de média zero; o fixo é "soma zero" quando declarado
por contraste (e o que se digitar, quando por nível).
]---", r"---[
- **Tipo** — ver a tabela.
- **Fator(es)** — coluna(s) do plano: `irrigacao`, `bloco:parcela`, `dose:irrigacao`.
- **Valor (intercepto)**.
- **Efeitos por nível ou célula** — `baixa = 0, alta = 2`; na interação
  `baixa:A = 1, baixa:B = 0, ...`, com os níveis na ordem do Fator.
- **Declarar por** — `por nível` ou um conjunto de contrastes.
- **Magnitudes dos contrastes** — `linear = 4, quadratico = 1, cubico = 0`
  (nome sem caixa e sem acento) ou só os números, na ordem dos contrastes.
- **Doses** — valores numéricos dos níveis, na ordem (polinomiais e
  quantitativo). Com dois fatores: `dose: 0, 50, 100; irrigacao: 0, 1`.
- **Controle** — o nível controle (conjunto `controle`: as magnitudes são as
  diferenças de cada tratamento para ele, `B vs A`, `C vs A`…).
- **Contrastes (digitados)** — sintaxe do `models/linear_hypothesis`.
- **Desvio-padrão** — do efeito aleatório; ou da covariável, quando ela é
  gerada.
- **Coeficientes (quantitativo)** — `b1` ou `b1, b2` (linear, quadrática…), na
  escala de x (a codificada, no composto central).
- **Inclinação**, **Média (covariável)** — se a coluna da covariável está vazia
  (`NA`), ela é gerada normal com essa média e o sd; a contribuição é
  inclinação · (x − média).
- **Nome do termo** — em branco, vem do fator (`bloco_parcela`).
]---", r"---[
O plano (`experiments/plan`) com a coluna `.ef_<nome>` nas unidades e o termo
em `plano$termos` (tipo, parâmetros, valor verdadeiro, conversão). A aba
`componentes` do `experiments/view` o desenha.
]---", r"---[
tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "dic", fatores = "dose: 0, 50, 100, 150") |>
  tr_add("mu", "experiments/effect", tipo = "intercepto", valor = 20, from = "plano") |>
  tr_add("dose", "experiments/effect", tipo = "fixo", fator = "dose", conjunto = "polinomiais",
         magnitudes = "linear = 4, quadratico = 1, cubico = 0", from = "mu")
]---", r"---[
- Montgomery, D. C. *Design and Analysis of Experiments*. 9. ed. Hoboken:
  Wiley, 2017. (Contrastes e contrastes ortogonais, cap. 3.)
- Gelman, A.; Hill, J. *Data Analysis Using Regression and
  Multilevel/Hierarchical Models*. Cambridge: Cambridge University Press, 2007.
  (Simulação de dados falsos para conferir a análise, cap. 8.)
]---", r"---[
`experiments/error` para fechar a resposta; `experiments/view` (aba
`componentes`); `experiments/contrasts` para recuperar o contraste declarado.
]---"), .tr_exp_ajuda_semente_sim())

.tr_exp_ajuda_error <- function() paste0(.tr_exp_an_ajuda(r"---[
Fecha a resposta: soma os termos dos `experiments/effect`, sorteia o resíduo e
escreve a coluna **Resposta**. Depois dele o plano vai direto aos nós de
`trama.models` (pelo adaptador para tabela), e `plano$analise` já traz a
análise sugerida com a resposta.

### Distribuição

- **normal** — y = Σ termos + ε, ε com desvio-padrão **sd** (ou um sd por
  nível de **sd por nível de**). A coluna `.ef_residuo` guarda ε.
- **poisson** — Σ termos é o preditor linear η; y ~ Poisson(e^η).
- **binomial** — y ~ Binomial(**ensaios**, logit⁻¹(η)); com mais de um ensaio
  sai também `<resposta>_fracassos`.
- **gama** — média e^η e **forma** k (variância média²/k).

### Só na normal

- **Correlação no indivíduo** — `simetria_composta` (corr. ρ entre quaisquer
  duas medidas) ou `ar1` (ρ^|i−j|, na ordem do `tempo`, ou do `periodo`) entre
  as medidas do mesmo `individuo`. Pede um plano com `individuo` (medidas
  repetidas, crossover).
- **Caudas pesadas** — ε = sd · t_gl · √((gl−2)/gl): a mesma variância, caudas
  de t (gl > 2).
- **Assimetria** — ε = sd · gama padronizada de forma 4/γ², que tem média 0,
  variância 1 e assimetria γ.

Com correlação e caudas (ou assimetria) juntas, a correlação é exata e a
distribuição marginal é aproximada.

### Parcelas perdidas

**Proporção perdida** apaga a resposta de round(p·N) unidades sorteadas ao
acaso (MCAR), em qualquer distribuição.

### A análise sugerida

Na normal sem covariável, o nó do plano com a resposta (param `resposta`, ou
a fórmula prefixada). Com covariável, `models/lm` (ou `models/lmer`, se a
análise do delineamento tem termo aleatório, `(1 | ...)`) com a covariável na
fórmula. Fora da normal, `models/glm` ou `models/glmer` com a família, pela
mesma regra: é a fórmula do delineamento que decide, e não os
`experiments/effect` — o bloco do DBC entra fixo mesmo quando o efeito dele
foi simulado aleatório. Como o `models/glmer` não tem gama, a gama com termo
aleatório na fórmula (o erro de parcela da subdividida, por exemplo) sugere o
GLM só dos fixos, com aviso.
]---", r"---[
Nenhum sobre dados: é simulação. O resíduo é independente entre unidades,
salvo a correlação declarada dentro do indivíduo. A perda é completamente ao
acaso (MCAR), que é o caso em que a análise dos dados restantes não tem viés.
]---", r"---[
- **Resposta** — nome da coluna (não pode existir no plano).
- **Distribuição** — `normal`, `poisson`, `binomial`, `gama`.
- **Desvio-padrão (normal)**; **sd por nível de** + **sd de cada nível**
  (`baixa = 1, alta = 3`) para heterocedasticidade proposital.
- **Ensaios (binomial)**, **Forma (gama)**.
- **Correlação no indivíduo**, **ρ**.
- **Caudas pesadas: gl da t** (0 = não), **Assimetria** (0 = não) — uma ou outra.
- **Proporção perdida** — 0 a 0,9.
]---", r"---[
O plano (`experiments/plan`) com a coluna da resposta e `plano$resposta`
(distribuição, parâmetros, unidades perdidas, semente). Ligado num nó de
`trama.models`, vira a tabela de unidades.
]---", r"---[
tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "parcela_subdividida",
         fatores = "irrigacao: baixa, alta; variedade: A, B, C", repeticoes = 4L) |>
  tr_add("mu", "experiments/effect", tipo = "intercepto", valor = 50, from = "plano") |>
  tr_add("bl", "experiments/effect", tipo = "aleatorio", fator = "bloco", sd = 3, from = "mu") |>
  tr_add("var", "experiments/effect", tipo = "fixo", fator = "variedade",
         efeitos = "A = 0, B = 1, C = 3", from = "bl") |>
  tr_add("ep", "experiments/effect", tipo = "aleatorio", fator = "bloco:parcela", sd = 4, from = "var") |>
  tr_add("y", "experiments/error", resposta = "producao", sd = 1, from = "ep") |>
  tr_add("sp", "models/anova_split_plot", resposta = "producao", parcela = "irrigacao",
         subparcela = "variedade", bloco = "bloco", from = "y")
]---", r"---[
- McCullagh, P.; Nelder, J. A. *Generalized Linear Models*. 2. ed. London:
  Chapman & Hall, 1989. (Famílias e funções de ligação.)
- Littell, R. C.; Milliken, G. A.; Stroup, W. W.; Wolfinger, R. D.;
  Schabenberger, O. *SAS for Mixed Models*. 2. ed. Cary: SAS Institute, 2006.
  (Simetria composta e AR(1) em medidas repetidas.)
- Johnson, N. L.; Kotz, S.; Balakrishnan, N. *Continuous Univariate
  Distributions*, v. 1. 2. ed. New York: Wiley, 1994. (Assimetria da gama,
  2/√k.)
]---", r"---[
`experiments/effect`; `experiments/view` (aba `componentes`);
`models/anova_split_plot`, `models/glm`, `models/glmer`, `models/lmer`.
]---"), .tr_exp_ajuda_semente_sim())

.tr_exp_ajuda_semente_sim <- function() "

### O sorteio

O bloco é **estocástico**: a semente é do card, e não da sessão. O mesmo
documento sorteia sempre os mesmos valores; trocar a semente sorteia outros.
A semente fica no plano (no termo ou em `plano$resposta`). A semente do console
(`set.seed()`) não é tocada. Para repetir a cadeia inteira com uma semente só,
use `tr_experiments_simulate()` no console.
"
