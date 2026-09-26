# As declarações dos nós que medem a previsão: confusão, ROC, métricas e a
# importância das preditoras.
#
# Confusão, ROC e métricas têm as DUAS entradas opcionais (ver `R/avaliar.R`):
# a ajuda de cada uma repete o parágrafo dos três modos, porque é a primeira
# coisa que quem abre o card precisa saber — o que ligar.

.tr_models_ajuda_modos <- function() {
  "
### O que ligar

As duas entradas são opcionais, mas uma tem de estar ligada:

- **só `modelo`** — avalia no TREINO, pela **validação**: `cruzada` (padrão;
  cada linha prevista por um modelo ajustado sem ela) ou `resubstituição` (o
  modelo completo prevê as linhas que o ajustaram — otimista).
- **`modelo` e `dados`** — prevê `dados` aqui e compara com a resposta do
  modelo, que a tabela precisa ter (o teste de uma divisão treino/teste, por exemplo).
  A validação é ignorada.
- **só `dados`** — modo tabela: a tabela já traz a resposta real e a prevista
  (de um `models/predict`, ou de fora), e os params `resposta`/`predito`
  dizem quais colunas são.

Com `dados` vindo da divisão treino/teste da coleção de aprendizado de máquina (a tabela leva a marca de treino/teste),
previsões das linhas de TREINO são recusadas (`tr_ml_error_train_eval`): a
medida no treino é otimista. **Permitir avaliar o treino** avalia assim mesmo,
avisa e põe a nota de otimismo na coluna `nota` (ou na legenda do gráfico).
Tabela sem a marca é avaliada como chega.
"
}

.tr_models_nos_avaliar <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum
  Fm <- "models/fit"; T <- "data/table"
  opc <- function(tipo) trama::tr_port(tipo, required = FALSE)
  validacao <- E(.TR_MODELS_VALIDACAO_PADRAO, .TR_MODELS_VALIDACOES, label = "Validação (só modelo)")
  resposta <- trama::tr_param_col("", label = "Resposta (modo tabela)", role = "qualquer", from = "dados", example = "am")
  predito <- trama::tr_param_col("previsto", label = "Previsto (modo tabela)", role = "qualquer", from = "dados", example = "previsto")
  positiva <- P("text", "", label = "Classe positiva", example = "1")
  permitir_treino <- trama::tr_param_bool(FALSE, label = "Permitir avaliar o treino")
  list(
    # Versão 2 (9.1b): `tabela = "métricas"` (da `multi/confusion` da main) e
    # `permitir_treino`, a proveniência da `ml/split`.
    trama::tr_node("models/confusion", version = 2L,
      pressupostos = .tr_models_doc("models/confusion")$pressupostos,
      referencias = .tr_models_doc("models/confusion")$referencias,
      fn = tr_models_confusion, label = "Matriz de confusão",
      category = "modelo_avaliar", icon = trama::tr_icon("table"),
      description = "Classe real × classe prevista, com o acerto de cada classe e o geral.",
      inputs = list(modelo = opc(Fm), dados = opc(T)), outputs = list(out = T),
      params = list(validacao = validacao, resposta = resposta, predito = predito,
                    tabela = E("matriz", .TR_MODELS_TABELAS_CONFUSAO, label = "Tabela"),
                    permitir_treino = permitir_treino),
      help = .tr_models_ajuda(paste0(r"---[
Conta, para cada classe REAL, em qual classe o modelo pôs cada caso. A
diagonal são os acertos; fora dela, as confusões. Ao lado, o total da classe,
os acertos e a taxa de acerto; a última linha (`real = total`) é o geral.
]---", .tr_models_ajuda_modos(), r"---[
### Por que cruzada

A taxa de acerto por resubstituição é otimista: cada caso ajudou a desenhar a
regra que o classifica. A cruzada é a estimativa honesta de quanto o modelo
acerta num caso novo, sem precisar separar teste. Leia a taxa de cada classe,
e não só a geral: com classes desbalanceadas, prever sempre a maior já acerta
muito.

Serve a todo classificador que viaja em `models/fit`: o GLM binomial daqui, a
discriminante e a logística da `multi`, as árvores da `ml`. Modelo de
regressão recusa — use `models/evaluate`.

### Métricas

Com **tabela = métricas**, a mesma matriz vira medidas que não se deixam
enganar por classes desbalanceadas: **acurácia** (acertos / total);
**acurácia balanceada**, a média das revocações das classes (Brodersen et al.
2010); **kappa de Cohen**, (pₒ − pₑ)/(1 − pₑ), o acerto além do esperado pelo
acaso com os mesmos totais (Cohen 1960); e **precisão**, **revocação** e
**F1** por classe. Classe nunca prevista tem precisão e F1 `NA` (0/0), e não
zero; o bloco não tira média macro de precisão ou F1.
]---"), r"---[
- **Validação (só modelo)** — `cruzada` (padrão) ou `resubstituição`.
- **Resposta**, **Previsto** — só no modo tabela: as colunas da classe real e
  da prevista (padrão `previsto`, a do `models/predict`).
- **Tabela** — `matriz` (padrão) ou `métricas`.
- **Permitir avaliar o treino** (`permitir_treino`) — com `dados` marcados pelo
  divisão da ml: desligado (padrão) recusa o treino; ligado, avalia com a nota.
]---", r"---[
Uma tabela (`data/table`) no formato largo: `real`, uma coluna por classe
prevista, `total`, `acertos`, `taxa_acerto`, e a linha `total`. Para o formato
longo (real, previsto, n), um `data/pivot_longer` nas colunas das classes.
Com `métricas`: `medida`, `grupo` (só nas medidas por classe) e `valor`.
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("logit", "models/glm", formula = "am ~ wt", familia = "binomial", from = "carros") |>
  tr_add("cv", "models/confusion", validacao = "cruzada", from = "logit")
]---", r"---[
`models/roc` para todos os cortes; `models/evaluate` para acurácia, kappa e
F1; `models/predict` para ver os casos.
]---")),

    # Versão 2: no modo tabela, com a coluna de probabilidade informada e a
    # positiva vazia, a classe sai do nome `prob_<classe>` (antes, o 2º nível),
    # como a `ml/roc` v2 da main. Versão 3 (9.1b): IC de DeLong da AUC
    # (`confianca`), corte de Youden e AUC multiclasse de Hand & Till, portados
    # da `ml/roc` e da `multi/roc` da main.
    trama::tr_node("models/roc", version = 3L,
      pressupostos = .tr_models_doc("models/roc")$pressupostos,
      referencias = .tr_models_doc("models/roc")$referencias,
      fn = tr_models_roc, label = "Curva ROC",
      category = "modelo_avaliar", icon = trama::tr_icon("chart-line"),
      description = "Sensibilidade × especificidade em todos os cortes, com a AUC.",
      inputs = list(modelo = opc(Fm), dados = opc(T)), outputs = list(out = "view/plot"),
      params = .tr_models_props(
        validacao = validacao, positiva = positiva, resposta = resposta,
        probabilidade = P("cols", "", label = "Probabilidade (modo tabela)", example = "prob_1"),
        confianca = trama::tr_param_num(0.95, min = 0.5, max = 0.999, step = 0.01, label = "Confiança do IC da AUC"),
        permitir_treino = permitir_treino,
        .aspecto = "1:1"),
      help = .tr_models_ajuda(paste0(r"---[
Para cada corte de probabilidade possível, a fração dos positivos que a regra
pega (**sensibilidade**) contra a fração dos negativos que ela alarma à toa
(**1 − especificidade**). Um classificador inútil anda na diagonal tracejada;
um perfeito sobe reto até o canto superior esquerdo.

A **AUC** (área sob a curva) é a probabilidade de um positivo sorteado ter
probabilidade prevista maior que a de um negativo sorteado: 0,5 é moeda, 1 é
perfeito. Não depende do corte, e compara modelos melhor que a taxa de acerto
quando as classes são desbalanceadas.

- **Duas classes** — uma curva; a positiva é a **Classe positiva**, ou o
  SEGUNDO nível se vazia. O ponto rosa é o corte do modelo (o `corte` da
  logística da `multi`; 0,5 nos demais).
- **Três ou mais** — uma curva por classe, ela contra todas as outras, com a
  AUC de cada na legenda. Com **Classe positiva** preenchida, só a curva dela.
  Sem ela, o subtítulo traz a AUC multiclasse M de Hand & Till (2001): a média,
  sobre os pares de classes, da AUC do par — não depende das proporções das
  classes, mas pondera todos os pares igualmente e não tem intervalo.

### Intervalo da AUC e corte de Youden

Cada AUC sai com o intervalo de DeLong, DeLong & Clarke-Pearson (1988), no
nível da **Confiança**: assintótico (normal), cortado em [0, 1]. Com menos de
duas linhas numa classe, ou AUC 0 ou 1 (variância zero), o intervalo sai
indisponível, com a nota na legenda do gráfico. Com validação cruzada, as
probabilidades vêm de n ajustes e o intervalo as trata como um escore fixo.

Na curva de uma classe, o círculo vazio marca o corte de Youden (1950), o que
maximiza J = sensibilidade + especificidade − 1 (em empate, o de maior
limiar). É um corte que equilibra os dois erros com peso igual — não o melhor
para todo problema.
]---", .tr_models_ajuda_modos(), r"---[
No modo tabela a curva lê a coluna **Probabilidade**; vazia, a
`prob_<positiva>` que o `models/predict` escreve (e, com três ou mais classes
sem positiva, todas as `prob_<nível>`).
]---"), r"---[
- **Validação (só modelo)** — `cruzada` (padrão) ou `resubstituição`.
- **Classe positiva** — vazia = o segundo nível.
- **Resposta**, **Probabilidade** — só no modo tabela.
- **Permitir avaliar o treino** (`permitir_treino`) — com `dados` marcados pelo
  divisão da ml: desligado (padrão) recusa o treino; ligado, avalia com a nota.
- **Confiança do IC da AUC** (`confianca`) — 0,95 por padrão.
]---", r"---[
Um gráfico (`view/plot`), com a AUC e o IC de DeLong no subtítulo (ou na
legenda, uma por classe). Os dados do gráfico trazem, por corte, `limiar`,
`fpr`, `tpr` e `classe`, e repetidos `auc`, `auc_ep`, `auc_inf`, `auc_sup`,
`auc_nota`, `youden_limiar`, `youden_j` e a marca `youden`.
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("logit", "models/glm", formula = "am ~ wt", familia = "binomial", from = "carros") |>
  tr_add("roc", "models/roc", validacao = "cruzada", from = "logit")
]---", r"---[
`models/confusion` para o acerto num corte; `models/evaluate` para as métricas.
]---", grafico = TRUE)),

    # Veio da `ml/pr_curve` (main) na 9.2, com os modos da `models/roc`. As
    # contas e o oráculo são os de lá; a versão começa em 1 no id novo. Versão
    # 2 (9.1b): `permitir_treino`, a proveniência da `ml/split`, e uma curva por
    # classe com três ou mais e `positiva` vazia (da `multi/pr_curve` da main).
    trama::tr_node("models/pr_curve", version = 2L,
      pressupostos = .tr_models_doc("models/pr_curve")$pressupostos,
      referencias = .tr_models_doc("models/pr_curve")$referencias,
      fn = tr_models_pr_curve, label = "Curva precisão-revocação",
      category = "modelo_avaliar", icon = trama::tr_icon("chart-line"),
      description = "Precisão × revocação em todos os cortes, com a precisão média (AP).",
      inputs = list(modelo = opc(Fm), dados = opc(T)), outputs = list(out = "view/plot"),
      params = .tr_models_props(
        validacao = validacao, positiva = positiva, resposta = resposta,
        probabilidade = P("cols", "", label = "Probabilidade (modo tabela)", example = "prob_sim"),
        permitir_treino = permitir_treino,
        .aspecto = "16:9"),
      help = .tr_models_ajuda(paste0(r"---[
Ordena as linhas pela probabilidade da classe positiva e mostra, em cada corte,
a **precisão** (dos que a regra chama de positivos, quantos são) contra a
**revocação** (dos positivos, quantos ela pega). Prefira à ROC quando a classe
de interesse é rara: a ROC pode parecer boa enquanto a precisão na classe rara
é baixa.

A **AP** (precisão média) soma, nos cortes, o ganho de revocação vezes a
precisão, sem interpolação — é a do `yardstick` e do scikit-learn. A **área**
do subtítulo usa a interpolação de Davis & Goadrich, a do `PRROC`; ligar os
pontos por reta superestimaria a área. A linha tracejada é a **prevalência**
da positiva, a precisão de um classificador ao acaso.

A classe positiva é a **Classe positiva**; vazia, o segundo nível (ou, no modo
tabela, a classe do nome `prob_<classe>` da coluna). Com três ou mais classes
ela é obrigatória, e a curva é ela contra as outras.
]---", .tr_models_ajuda_modos(), r"---[
No modo tabela a curva lê a coluna **Probabilidade**; vazia, a
`prob_<positiva>` que o `models/predict` escreve.
]---"), r"---[
- **Validação (só modelo)** — `cruzada` (padrão) ou `resubstituição`.
- **Classe positiva** — vazia = o segundo nível (ou a do nome da coluna); com três
  ou mais classes e vazia, uma curva por classe contra as outras.
- **Resposta**, **Probabilidade** — só no modo tabela.
- **Permitir avaliar o treino** (`permitir_treino`) — com `dados` marcados pelo
  divisão da ml: desligado (padrão) recusa o treino; ligado, avalia com a nota.
]---", r"---[
Um gráfico (`view/plot`), com a AP, a área e a prevalência. Os dados do
gráfico trazem `limiar`, `recall`, `precision`, `ap`, `area` e `prevalencia`.
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("logit", "models/glm", formula = "am ~ wt", familia = "binomial", from = "carros") |>
  tr_add("pr", "models/pr_curve", validacao = "cruzada", from = "logit")
]---", r"---[
`models/roc`; `models/confusion` para o acerto num corte; `models/evaluate`
para a acurácia balanceada e o F1.
]---", grafico = TRUE)),

    # Versão 2: a tabela ganhou a coluna `classe` e as métricas ponderadas e por
    # classe (porte da `ml/evaluate` da main).
    trama::tr_node("models/evaluate", version = 3L,
      pressupostos = .tr_models_doc("models/evaluate")$pressupostos,
      referencias = .tr_models_doc("models/evaluate")$referencias,
      fn = tr_models_evaluate, label = "Avaliar previsões",
      category = "modelo_avaliar", icon = trama::tr_icon("gauge"),
      description = "Erro de previsão (regressão) ou acerto, kappa e F1 (classificação), numa tabela de métricas.",
      inputs = list(modelo = opc(Fm), dados = opc(T)), outputs = list(out = T),
      params = list(validacao = validacao, positiva = positiva, resposta = resposta, predito = predito,
                    permitir_treino = permitir_treino),
      help = .tr_models_ajuda(paste0(r"---[
Mede quanto a previsão erra, numa linha por métrica:

- **Regressão** — `mae` (erro absoluto médio), `rmse` (raiz do erro
  quadrático médio, na unidade da resposta) e `r2` de PREVISÃO (1 − SQ do erro
  / SQ total nas linhas avaliadas). Na cruzada ou em dados novos o `r2` pode
  ser negativo: o modelo prevê pior que a média.
- **Classificação** — `accuracy` (acerto), `balanced_accuracy` (média do
  acerto por classe observada), `macro_f1`, `kappa` de Cohen (o acerto além do
  que o acaso daria), precisão e revocação macro, precisão, revocação e F1
  ponderados pelo suporte e, com duas classes, `sensitivity` e `specificity`
  da **Classe positiva** (vazia = o segundo nível). Por classe, `precision`,
  `recall` e `f1` de cada uma (um contra todos), com o suporte em `n`. Médias
  macro usam as classes observadas; precisão de classe nunca prevista é
  indefinida (NA) e fica fora das médias macro e ponderada.

Os nomes e as contas das três primeiras de cada tarefa são os que a busca de
hiperparâmetros da coleção `ml` compara, para que os números batam.
Diferente de `models/fit_stats`, que descreve o AJUSTE no treino, este bloco
mede a PREVISÃO.
]---", .tr_models_ajuda_modos(), r"---[
No modo tabela, resposta ou previsto categóricos (texto, fator, lógico) fazem
classificação; dois números, regressão. Linhas sem real ou sem previsto ficam
fora, e `n` diz quantas contaram.
]---"), r"---[
- **Validação (só modelo)** — `cruzada` (padrão) ou `resubstituição`.
- **Classe positiva** — a das `sensitivity`/`specificity`.
- **Resposta**, **Previsto** — só no modo tabela.
- **Permitir avaliar o treino** (`permitir_treino`) — com `dados` marcados pelo
  divisão da ml: desligado (padrão) recusa o treino; ligado, avalia com a nota.
]---", r"---[
Uma tabela (`data/table`): `metrica`, `valor`, `n` e, na classificação,
`classe` (vazia nas métricas globais).
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("reg", "models/lm", formula = "mpg ~ wt + hp", from = "carros") |>
  tr_add("cv", "models/evaluate", validacao = "cruzada", from = "reg")
]---", r"---[
`models/fit_stats` para o ajuste no treino; `models/confusion` para as
confusões entre classes.
]---")),

    trama::tr_node("models/importance", fn = tr_models_importance_table, label = "Importância",
      category = "modelo_resumir", icon = trama::tr_icon("chart-bar"),
      description = "Quanto cada preditora pesa no modelo, da maior para a menor.",
      inputs = list(modelo = Fm), outputs = list(out = T),
      help = .tr_models_ajuda(r"---[
Uma linha por preditora (ou coeficiente), da mais para a menos importante. A
coluna `medida` diz qual régua foi usada, e ela muda com o modelo:

- `lm` e GLM — **|t|** (ou |z|) de cada coeficiente: não depende da escala da
  preditora, e o intercepto fica de fora. Um fator vira uma linha por
  contraste.
- Árvores da `ml` — a importância interna do motor: redução de impureza
  (ganho no XGBoost). A referência linear da `ml` usa o |t| (|z|), como o
  `lm`; a SVM não tem importância interna e recusa.

Não compare importâncias de medidas diferentes, e não leia sinal nem causa:
preditoras correlacionadas repartem a importância entre si. O misto e a
parcela subdividida recusam.
]---", r"---[
Sem params.
]---", r"---[
Uma tabela (`data/table`): `termo`, `importancia`, `medida`.
]---", r"---[
tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("reg", "models/lm", formula = "mpg ~ wt + hp + qsec", from = "carros") |>
  tr_add("imp", "models/importance", from = "reg")
]---", r"---[
`models/coefficients` com escala `desvio padrão` para o efeito comparável de
cada preditora.
]---"))
  )
}
