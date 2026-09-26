# As páginas de ajuda dos nós de análise (contrastes, Box-Cox, superfície).
#
# Montadas por função, como nas coleções irmãs. Pressupostos e referências não
# entram na prosa: vêm estruturados de `docs_blocos.R` (`.tr_exp_doc`), e a
# página os monta a partir do nó.

#' @noRd
.tr_exp_an_ajuda <- function(descricao, parametros, valor, exemplos, veja,
                             grafico = FALSE, teste = FALSE) {
  paste0("## Descrição\n\n", trimws(descricao),
         "\n\n## Parâmetros\n\n", trimws(parametros),
         "\n\n## Valor\n\n", trimws(valor),
         "\n\n## Exemplos\n\n```r\n", trimws(exemplos), "\n```",
         "\n\n## Veja também\n\n", trimws(veja),
         if (grafico) paste0("\n", trama.view::tr_view_help_appearance()) else "",
         if (teste) paste0("\n", trama::tr_help_test_card()) else "")
}

.tr_exp_an_ajuda_contrasts <- function() .tr_exp_an_ajuda(r"---[
**Tudo é contraste.** Com k tratamentos, o SQ do tratamento é a soma de k − 1
contrastes ortogonais; a tendência linear de doses, cada tratamento contra o controle, o
efeito principal e a interação de um fatorial são todos contrastes. Este bloco
lê um modelo já ajustado e abre a ANOVA: **uma linha por contraste**, com
estimativa Σ cᵢ ȳᵢ, SQ, F e p.

### Conjuntos

- **polinomiais** — linear, quadrático, cúbico… até o grau k − 1. Vale para
  níveis **desigualmente espaçados** e **réplicas desiguais**: os coeficientes
  saem de `poly()` sobre as observações (cᵢ = rᵢ pⱼ(xᵢ)), e não da tabela de
  níveis igualmente espaçados. No caso igualmente espaçado e balanceado, são os
  inteiros das tabelas (`-3 -1 1 3`, `1 -1 -1 1`, `-1 3 -3 1`).
- **helmert** — cada nível contra a média dos anteriores.
- **controle** — cada tratamento contra o controle (`B vs A`, `C vs A`…, as
  comparações de Dunnett), k − 1 contrastes. Não são ortogonais (todos usam a
  média do controle), então os SQ não somam o do tratamento; o p de cada linha
  é o de uma comparação, sem o ajuste de Dunnett para as k − 1 juntas.
- **fatorial 2^k** — os efeitos principais e as interações de 2 a 5 fatores de
  dois níveis, como contrastes de sinais ±1 nas médias das células (o SEGUNDO
  nível de cada fator é o alto, +1). A coluna
  `efeito` é a estimativa dividida por 2^(k−1): a diferença entre as médias do
  nível alto e do baixo.
- **digitados** — na mesma sintaxe do `models/linear_hypothesis` (números, um
  por nível, ou expressão nos nomes dos níveis, com rótulo opcional antes de
  `:`).

### O que se confere

- **Soma dos SQ**: no rodapé, Σ SQ dos contrastes ao lado do SQ do tratamento
  lido do quadro do próprio modelo, e se confere. Só se espera que some quando o
  conjunto tem k − 1 contrastes e é ortogonal.
- **Ortogonalidade**: a porta `ortogonalidade` traz a matriz Σ cᵢdᵢ/rᵢ, que é
  a covariância entre dois contrastes de médias a menos de σ². Fora da diagonal,
  zero é ortogonal; os pares que não são aparecem na nota — e é por isso que os
  SQ deixam de somar (eles se sobrepõem). Com réplicas desiguais, contrastes
  ortogonais no papel (Σ cᵢdᵢ = 0, como o Helmert) deixam de ser.
- **Regressão**: no conjunto polinomial sobre um `lm`, a coluna `sq_regressao`
  traz o SQ sequencial de cada grau de `poly(dose)` no mesmo modelo, e ele é
  igual ao SQ do contraste: o polinômio ortogonal É a regressão.

### Desdobramento da interação

Com **Dentro** preenchido, os contrastes do fator são estimados em cada nível do
outro fator (linear de N dentro de cada variedade). Com um conjunto completo e
ortogonal, a soma de todos os SQ desdobrados é SQ(fator) + SQ(fator × dentro).
Os coeficientes de cada grupo usam as réplicas da CÉLULA daquele grupo: com
células de tamanhos diferentes, os polinômios de cada nível de **Dentro** saem
diferentes e ortogonais dentro da célula, e a porta `ortogonalidade` traz uma
matriz por grupo.

### O termo de erro

O F de cada contraste é o t² do `emmeans` sobre o modelo, e é o modelo que
escolhe o erro: no delineamento e no `lm`, o resíduo; na **parcela
subdividida**, o misto do `models/fit` com gl de Satterthwaite — no balanceado,
o erro a para contrastes entre níveis da parcela e o erro b para os da
subparcela. O `qm_erro` de cada linha é SQ / F, o erro que aquela comparação de
fato usou. O SQ de cada linha é o SQ extra do teste de 1 gl, `sq` = F ×
`qm_erro` (o mesmo do `car::linearHypothesis`). No `lm` e nos delineamentos,
`qm_erro` é o QM do resíduo, igual em todas as linhas; no misto e na parcela
subdividida, é o erro efetivo (combinado) que aquele contraste usou. No
balanceado, `sq` é o SQ do livro, est² / Σ(cᵢ²/rᵢ), com rᵢ as observações de
cada média; no desbalanceado os dois diferem, e o do livro sai à parte, na
coluna `sq_livro`.
]---", r"---[
- **Fator** — o fator cujas médias se contrastam. No conjunto `fatorial 2^k`,
  de 2 a 5 fatores de dois níveis, separados por vírgula.
- **Conjunto** — `polinomiais`, `helmert`, `controle`, `fatorial 2^k` ou
  `digitados`.
- **Contrastes** — só no conjunto `digitados`: um por linha ou separados por
  `;`, cada um com rótulo opcional (`linear: -3 -1 1 3; B - A`).
- **Controle** — o nível controle no conjunto `controle`; em branco, o primeiro.
- **Doses** — os valores numéricos dos níveis, na ordem, para os polinomiais
  (`0 30 60 120`); em branco, os nomes dos níveis lidos como número.
- **Dentro** — um fator em cujos níveis os contrastes são desdobrados.
]---", r"---[
Duas portas:

- `out` — um quadro de efeitos (`models/effects`) com uma linha por contraste:
  `termo`, `coeficientes`, `estimativa`, `erro_padrao`, `gl`, `sq`, `F`,
  `gl_erro`, `qm_erro`, `p_valor` (e `efeito` no 2^k, `sq_regressao` nos
  polinomiais sobre `lm`, `sq_livro` quando o modelo é desbalanceado). O rodapé traz a
  conferência da soma dos SQ.
- `ortogonalidade` — uma tabela (`data/table`) com a matriz Σ cᵢdᵢ/rᵢ entre os
  contrastes (uma por nível de **Dentro**, com a coluna do grupo).
]---", r"---[
tr_flow(reg) |>
  tr_add("aveia", "models/example", dataset = "aveia") |>
  tr_add("split", "models/anova_split_plot", resposta = "producao", parcela = "variedade",
         subparcela = "nitrogenio", bloco = "bloco", from = "aveia") |>
  tr_add("pol", "experiments/contrasts", fator = "nitrogenio", conjunto = "polinomiais",
         doses = "0 0.2 0.4 0.6", dentro = "variedade", from = "split")
]---", r"---[
`models/linear_hypothesis` para o F conjunto dos mesmos contrastes;
`models/polinomial` para a curva ajustada às doses; `models/emmeans` para as
médias que os contrastes combinam; `experiments/boxcox` quando os resíduos pedem
transformação.
]---", teste = TRUE)

.tr_exp_an_ajuda_boxcox <- function() .tr_exp_an_ajuda(r"---[
Procura a potência λ da resposta que torna o erro do modelo o mais próximo de
normal com variância constante: y^(λ) = (y^λ − 1)/λ, e log(y) em λ = 0. Mantém o
MESMO modelo (o delineamento do `models/fit`) e varre λ, calculando a
log-verossimilhança perfilada em cada um, com a resposta dividida pela média
geométrica para que os valores sejam comparáveis entre λ — a conta do
`MASS::boxcox`, que o bloco reproduz ponto a ponto.

Devolve:

- **λ ótimo** — o máximo exato do perfil (não só o da grade);
- **intervalo de confiança** — os λ cuja log-verossimilhança fica a menos de
  χ²₁(confiança)/2 do máximo (razão de verossimilhança);
- **λ sugerido** — dentro do intervalo, a potência interpretável mais próxima do
  ótimo, entre −2, −1, −0,5, 0 (log), 0,5, 1 (nenhuma) e 2. Se nenhuma cair no
  intervalo, o bloco diz isso e não sugere;
- se λ = 1 (não transformar) está no intervalo;
- se λ̂ caiu na **borda da grade** (`na_borda`): aí o perfil ainda sobe além
  do limite, λ̂ não é o ótimo, e o bloco não sugere transformação — amplie a
  grade.

Na parcela subdividida, o perfil é o do modelo de efeitos fixos com bloco ×
parcela como fator (o que dá os resíduos do erro b).
]---", r"---[
- **λ mínimo**, **λ máximo**, **Passo** — a grade do perfil; o padrão, −2 a 2
  de 0,1 em 0,1, é o do `MASS::boxcox`.
- **Confiança** — o nível do intervalo para λ (0,95).
]---", r"---[
Três portas: `out`, o gráfico do perfil (`view/plot`) com o ótimo, o intervalo
e o λ sugerido; `resumo`, uma tabela de uma linha (`lambda_otimo`, `li`, `ls`,
`confianca`, `lambda_sugerido`, `transformacao`, `um_no_intervalo`,
`na_borda`, `nota`); e
`perfil`, a tabela (λ, log-verossimilhança) da grade.
]---", r"---[
tr_flow(reg) |>
  tr_add("fios", "models/example", dataset = "warpbreaks") |>
  tr_add("fat", "models/anova_factorial", resposta = "breaks", fatores = "wool, tension",
         from = "fios") |>
  tr_add("bc", "experiments/boxcox", from = "fat")
]---", r"---[
`models/shapiro_residuals` e `models/levene` para conferir os resíduos antes e
depois; `data/mutate` para aplicar a transformação; `experiments/contrasts`.
]---", grafico = TRUE)

.tr_exp_an_ajuda_superficie <- function() .tr_exp_an_ajuda(r"---[
Ajusta a superfície de resposta aos fatores **codificados** (−1, 0, +1, ±α) —
de 1ª ordem (plano: y = b0 + Σ bᵢxᵢ) ou de 2ª ordem (quadrática completa:
mais os produtos xᵢxⱼ e os quadrados xᵢ²) — e faz a leitura dela:

- **Quadro** — os SQ sequenciais agrupados na ordem dos livros: bloco, primeira
  ordem, interações, quadráticos, resíduo; e, havendo pontos repetidos (os
  centrais), o resíduo partido em **falta de ajuste** e **erro puro**, com o F
  da falta de ajuste. Falta de ajuste significativa diz que o modelo daquela
  ordem não basta.
- **Análise canônica** (2ª ordem) — escrevendo ŷ = b0 + x'b + x'Bx, o ponto
  estacionário é xₛ = −B⁻¹b/2 e ŷₛ = b0 + xₛ'b/2. Os autovalores de B dão a
  natureza: todos negativos, máximo; todos positivos, mínimo; sinais mistos,
  ponto de sela. Autovalor perto de zero (menos de 5% do maior em módulo) é
  sinalizado como cumeeira — o critério de 5% é uma convenção deste bloco. Se
  o ponto cai fora da região experimentada, a nota avisa: é extrapolação.
- **1ª ordem** — no lugar da canônica, a direção de maior subida (o vetor b
  normalizado), o passo do método de Box & Wilson.
- **Contorno** — ŷ nos dois primeiros fatores, com os demais no centro (0), os
  pontos do delineamento e o ponto estacionário marcado.

O modelo sai como `models/fit` comum (o `lm` do `models/lm`): resíduos,
pressupostos, coeficientes e previsão funcionam nele. Com bloco, o bloco entra
aditivo, e o b0 da canônica e do contorno é a média dos blocos.
]---", r"---[
- **Resposta** — coluna numérica.
- **Fatores** — de 1 a 6 colunas numéricas codificadas, separadas por vírgula.
- **Ordem** — `1` (plano) ou `2` (quadrática completa).
- **Bloco** — coluna do bloco (opcional).
]---", r"---[
Quatro portas: `modelo` (`models/fit`); `quadro` (`models/effects`, a ANOVA da
superfície com falta de ajuste e erro puro); `canonica` (`data/table`: ponto
estacionário, ŷ nele, autovalores com os autovetores na nota, natureza e
distância ao centro; na 1ª ordem, a direção de maior subida); `grafico`
(`view/plot`, o contorno).
]---", r"---[
tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "composto_central", fatores = "x1; x2") |>
  tr_add("ccd", "data/mutate", name = "rendimento",
         expr = "80 - (x1 - 0.4)^2 - 2 * x2^2 + sin(unidade) / 5", from = "plano") |>
  tr_add("rsm", "experiments/response_surface", resposta = "rendimento",
         fatores = "x1, x2", ordem = "2", from = "ccd")
]---", r"---[
`models/residuals` e `models/predict` sobre o `modelo`; `experiments/boxcox`.
]---", grafico = TRUE)
