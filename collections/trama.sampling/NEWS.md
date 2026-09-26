# trama.sampling 0.3.1

* Parâmetros que só valem para certa escolha de outro parâmetro agora declaram `trama::tr_when()` e somem do card quando não se aplicam. Exige `trama (>= 0.2.0)`.

# trama.sampling 0.3.0

Versão sobe para 0.3.0: a 0.2.0 era a das mudanças de método da main
(planejamento com t, Clopper-Pearson de Korn & Graubard); a coesão das
coleções muda de novo (confiança numérica no glossário, a curva do plano
inclui a confiança), e sobe mais um minor.

## 0.2.0 (mudanças de método da main)

## Mudanças de método (revisão metodológica, 2026-09-25)

- Planejamento com t: margem muito folgada não dá mais "t com 0 gl" (n = 1).
  A busca começa no menor n com gl ≥ 1 (2 na AAS, H + 1 na estratificada, 2
  conglomerados); em `size_stratified` o n também não fica abaixo do piso da
  alocação (2 por estrato), que antes dava erro. Com `z` nada muda.

- `sampling/proportion` (versão 3): com p̂ = 0 ou 1 o intervalo não degenera
  mais no ponto. `logit` e `wilson` passam, nesses casos, ao Clopper-Pearson de
  Korn & Graubard (1998, Survey Methodology 24(2), 193-201): n efetivo
  p̂(1 − p̂)/v(p̂) ajustado pelos gl, n_ef·(t_{n−1}/t_gl)², e limites beta. Como a
  variância é zero nos extremos, o n efetivo é substituído pelo n nominal do
  domínio (ainda ajustado pelos gl): sem efeito de desenho é o Clopper-Pearson
  exato. Nova opção `intervalo = "clopper_pearson"` para usá-lo sempre. Wald
  continua literal. Validado contra `survey::svyciprop(method = "beta")` em AAS,
  estratificada e conglomerados (≤ 1e-6) e, nos extremos, contra
  `stats::binom.test` (≤ 1e-10). Estimativa NA não quebra mais o intervalo.

- `sampling/proportion` (versão 2): o intervalo padrão passa a ser **logit**
  (Wald na escala log-odds, EP pelo método delta, t nos gl do desenho), o padrão
  de `survey::svyciprop` e o recomendado para amostras complexas (Korn &
  Graubard 1999, doi:10.1002/9781118032619): não sai de (0, 1) e é assimétrico
  perto dos extremos. Novo param `intervalo`: `logit`, `wilson` (escore de
  Wilson 1927 com n efetivo e t) ou `wald` (o intervalo da versão 1). A
  `margem` passa a ser a maior das duas metades. Validado contra
  `survey::svyciprop(method = "logit")` em AAS, estratificada, conglomerados e
  domínio (estimativa ≤ 1e-8, limites ≤ 1e-6).
- Planejamento com **t nos gl do desenho** (versão 2 de `size_mean`,
  `size_proportion`, `size_stratified`, `size_cluster`, `size_domains`,
  `margin`, `margin_levels`, `referral`, `detectable_difference` e
  `question_margins`): o quantil passa a ser o t com os gl que a amostra vai ter
  (UPAs − estratos: n − 1, n − H, conglomerados − 1, redes − unidades), o
  mesmo que o card de `sampling/mean` usa; quando o gl depende do próprio n, o
  bloco devolve o menor n cuja margem, com os gl dele, cabe na pedida. Novo
  param `distribuicao` (`t` | `z`); `z` reproduz a versão 1. Com muitos gl a
  diferença é de uma ou duas unidades (±5 pontos: 385 → 387); com poucos
  conglomerados é grande. Validado por busca exaustiva do menor n (oráculo da
  definição).
- `sampling/detectable_difference` (versão 2): fórmula de duas proporções de
  Fleiss, Levin & Paik (2003, cap. 4), com p̄ agrupada sob H0 e variâncias
  próprias sob H1, n desiguais, proporção de referência por grupo (coluna
  `proporcao_grupo`) e correção finita opcional (coluna `populacao`); a DMD é o
  pior caso entre as duas referências e os dois sentidos. Com n iguais, `z` e
  sem correção finita reproduz `stats::power.prop.test` (≤ 1e-8). Novas colunas
  `p_a`, `p_b`, `gl`.
- `sampling/size_cluster` (`cv_tamanho`) e `sampling/referral` (`cv_rede`):
  coeficiente de variação do tamanho dos conglomerados/redes, com
  deff = 1 + ((CV² + 1)·m̄ − 1)·ρ (Eldridge, Ashby & Kerry 2006,
  doi:10.1093/ije/dyl129). CV = 0 (padrão) reproduz o deff anterior. Conferido
  contra a variância exata da média sob ICC comum.
