# As páginas de ajuda dos nós de análise (contrastes, Box-Cox, superfície).
#
# Montadas por função, como nas coleções irmãs, e com duas seções a mais que o
# padrão do models: **Pressupostos** (o que tem de valer para o número ser lido
# como o livro lê) e **Referências** (onde conferir a conta). Referência entra
# só conferida; o que não foi conferido na fonte diz "a conferir".

#' @noRd
.tr_exp_an_ajuda <- function(descricao, pressupostos, parametros, valor, exemplos, referencias, veja,
                             grafico = FALSE, teste = FALSE) {
  paste0("## Descrição\n\n", trimws(descricao),
         "\n\n## Pressupostos\n\n", trimws(pressupostos),
         "\n\n## Parâmetros\n\n", trimws(parametros),
         "\n\n## Valor\n\n", trimws(valor),
         "\n\n## Exemplos\n\n```r\n", trimws(exemplos), "\n```",
         "\n\n## Referências\n\n", trimws(referencias),
         "\n\n## Veja também\n\n", trimws(veja),
         if (grafico) paste0("\n", trama.view::tr_view_help_appearance()) else "",
         if (teste) paste0("\n", trama::tr_help_test_card()) else "")
}

.tr_exp_an_ajuda_contrasts <- function() .tr_exp_an_ajuda(r"---[
**Tudo é contraste.** Com k tratamentos, o SQ do tratamento é a soma de k − 1
contrastes ortogonais; a tendência linear de doses, o controle contra o resto, o
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
- **controle** — o controle contra a média dos demais, e os demais entre si por
  Helmert, completando k − 1 contrastes.
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

### O termo de erro

O F de cada contraste é o t² do `emmeans` sobre o modelo, e é o modelo que
escolhe o erro: no delineamento e no `lm`, o resíduo; na **parcela
subdividida**, o misto do `models/fit` com gl de Satterthwaite — no balanceado,
o erro a para contrastes entre níveis da parcela e o erro b para os da
subparcela. O `qm_erro` de cada linha é SQ / F, o erro que aquela comparação de
fato usou. O SQ é o do livro, est² / Σ(cᵢ²/rᵢ), com rᵢ as observações de cada
média.
]---", r"---[
- Modelo linear com erro normal, independente e de variância constante (ANOVA,
  `lm`, misto ou parcela subdividida); GLM é recusado — lá o contraste vive na
  escala da ligação e não tem SQ que some (use `models/linear_hypothesis`).
- Contrastes **planejados** antes de ver os dados: o p de cada linha não é
  corrigido para multiplicidade. Contrastes escolhidos depois de olhar as médias
  pedem Scheffé ou outra correção.
- No desbalanceado, as médias são as ajustadas do `emmeans`, e o SQ de livro
  (est² / Σ cᵢ²/rᵢ) é uma aproximação: o F e o p, que vêm da covariância do
  modelo, continuam exatos, e o rodapé diz que a soma não fecha.
- Polinômios exigem fator quantitativo: os níveis têm de ser números, ou os
  valores vão em **Doses**.
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
  polinomiais sobre `lm`). O rodapé traz a conferência da soma dos SQ.
- `ortogonalidade` — uma tabela (`data/table`) com a matriz Σ cᵢdᵢ/rᵢ entre os
  contrastes.
]---", r"---[
tr_flow(reg) |>
  tr_add("aveia", "models/example", dataset = "aveia") |>
  tr_add("split", "models/anova_split_plot", resposta = "producao", parcela = "variedade",
         subparcela = "nitrogenio", bloco = "bloco", from = "aveia") |>
  tr_add("pol", "experiments/contrasts", fator = "nitrogenio", conjunto = "polinomiais",
         doses = "0 0.2 0.4 0.6", dentro = "variedade", from = "split")
]---", r"---[
- Montgomery, D. C. (2017). *Design and Analysis of Experiments*, 9th ed.
  Wiley. Cap. 3 (contrastes e contrastes ortogonais; o exemplo da taxa de
  gravação, reproduzido nos testes) e cap. 6 (o fatorial 2^k como contrastes).
- Pimentel-Gomes, F. (2009). *Curso de Estatística Experimental*, 15ª ed. FEALQ.
  (Contrastes ortogonais e polinômios ortogonais — capítulo a conferir.)
- Searle, S. R. (1971). *Linear Models*. Wiley.
- Lenth, R. V. `emmeans`: Estimated Marginal Means (pacote R), usado para as
  estimativas e os erros padrão.
]---", r"---[
`models/linear_hypothesis` para o F conjunto dos mesmos contrastes;
`models/dose_response` para a curva ajustada às doses; `models/emmeans` para as
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
- se λ = 1 (não transformar) está no intervalo.

Na parcela subdividida, o perfil é o do modelo de efeitos fixos com bloco ×
parcela como fator (o que dá os resíduos do erro b).
]---", r"---[
- Resposta estritamente positiva: y^λ não é definido para y ≤ 0. Se houver
  zeros, some uma constante antes (`data/mutate`) e registre isso.
- Existe uma potência que normaliza e estabiliza a variância ao mesmo tempo — o
  método escolhe o λ pela verossimilhança normal; confira os resíduos do modelo
  transformado (`models/shapiro_residuals`, `models/levene`) depois.
- A transformação muda a escala da interpretação: as médias transformadas de
  volta são medianas, não médias, na escala original.
]---", r"---[
- **λ mínimo**, **λ máximo**, **Passo** — a grade do perfil; o padrão, −2 a 2
  de 0,1 em 0,1, é o do `MASS::boxcox`.
- **Confiança** — o nível do intervalo para λ (0,95).
]---", r"---[
Três portas: `out`, o gráfico do perfil (`view/plot`) com o ótimo, o intervalo
e o λ sugerido; `resumo`, uma tabela de uma linha (`lambda_otimo`, `li`, `ls`,
`confianca`, `lambda_sugerido`, `transformacao`, `um_no_intervalo`, `nota`); e
`perfil`, a tabela (λ, log-verossimilhança) da grade.
]---", r"---[
tr_flow(reg) |>
  tr_add("fios", "models/example", dataset = "warpbreaks") |>
  tr_add("fat", "models/anova_factorial", resposta = "breaks", fatores = "wool, tension",
         from = "fios") |>
  tr_add("bc", "experiments/boxcox", from = "fat")
]---", r"---[
- Box, G. E. P. & Cox, D. R. (1964). An analysis of transformations. *Journal of
  the Royal Statistical Society, Series B*, 26, 211–252. (Os dados de venenos e
  tratamentos, `boot::poisons`, são deste artigo e estão nos testes.)
- Venables, W. N. & Ripley, B. D. (2002). *Modern Applied Statistics with S*,
  4th ed. Springer. (`MASS::boxcox`.)
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
- Fatores já codificados; a canônica só tem leitura nessa escala.
- Erro normal, independente e de variância constante, como em todo `lm`.
- O modelo de 2ª ordem é uma aproximação local: vale dentro da região
  experimentada, e o ponto estacionário fora dela não é recomendação.
- Falta de ajuste só se testa com pontos repetidos; o erro puro vem das
  repetições no mesmo ponto (e no mesmo bloco).
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
  tr_add("ccd", "data/read_csv", path = "ccd_codificado.csv") |>
  tr_add("rsm", "experiments/response_surface", resposta = "rendimento",
         fatores = "x1, x2", ordem = "2", bloco = "bloco", from = "ccd")
]---", r"---[
- Box, G. E. P. & Wilson, K. B. (1951). On the experimental attainment of
  optimum conditions. *Journal of the Royal Statistical Society, Series B*, 13,
  1–45.
- Myers, R. H., Montgomery, D. C. & Anderson-Cook, C. M. (2016). *Response
  Surface Methodology*, 4th ed. Wiley. (Os dados do processo químico em dois
  blocos, `rsm::ChemReact`, são da tabela 7.6 do livro segundo a documentação
  do `rsm` — edição a conferir — e estão nos testes.)
- Lenth, R. V. (2009). Response-Surface Methods in R, Using rsm. *Journal of
  Statistical Software*, 32(7).
]---", r"---[
`models/residuals` e `models/predict` sobre o `modelo`; `experiments/boxcox`.
]---", grafico = TRUE)
