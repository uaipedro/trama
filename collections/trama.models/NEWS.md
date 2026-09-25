# trama.models (desenvolvimento)

## Mudanças de método

- `models/levene` e `models/bartlett` (versão 3): recusam a parcela subdividida
  com `tr_models_error_block_design`. Os resíduos do erro (b) vêm de um
  delineamento em que o fator da parcela está confundido com a parcela, e a
  correção de O'Neill & Mathews (2002) supõe um estrato de erro só; não há
  correção publicada para os dois estratos. Em simulação sob H0 no desenho da
  aveia (4000 réplicas), o Levene comum nesses resíduos rejeitava 10,5% a 5%
  com centro na média e 2,5% na mediana. A mensagem aponta o painel
  escala-locação do `models/plot_diagnostics` e o `models/lmer`.
- `models/levene` (versão 2): nos delineamentos com bloco (DBC, fatorial em
  DBC, DQL) passa a ser o teste de O'Neill & Mathews (2002, *Biometrics*
  58:216-224, doi:10.1111/j.0006-341x.2002.00216.x): ANOVA dos |resíduos| de
  mínimos quadrados em tratamento + bloco (+ linha e coluna) com o F
  multiplicado pelo fator do delineamento, a razão dos quadrados médios
  esperados de |e| sob H0 calculada pelas correlações dos resíduos. O Levene
  comum nos resíduos correlacionados sai liberal (no quadrado latino 7 × 7,
  8,3% de rejeição a 5% contra 5,0% com a correção, em simulação). Ali o centro
  é sempre o ajuste do modelo (a média) e o param `centro` não muda o
  resultado; delineamento desbalanceado é recusado. DIC e modelos de fórmula
  não mudam. Validado contra `oneilldbc()` do ExpDes.pt 1.2.2 (arquivado no
  CRAN em 2026-06, por isso fora do Suggests; os p ficam fixos no teste):
  warpbreaks em 9 blocos, F = 3,1517, p = 0,017183 (e o fatorial em blocos dá
  o mesmo), e o exemplo `ex4` do ExpDes.pt (carbono), p = 0,30708, iguais a
  1e-14; o fator fechado do DBC do artigo é reproduzido a 1e-10. No DQL o
  fator foi conferido por Monte Carlo (OrchardSprays, 20000 réplicas, 1%).
- `models/bartlett` (versão 2): recusa delineamento com bloco (DBC, fatorial
  em DBC, DQL) com `tr_models_error_block_design`, apontando o
  `models/levene`: não há correção publicada do Bartlett para a correlação dos
  resíduos. DIC e modelos de fórmula não mudam. O exemplo `experimentos`
  troca o Bartlett do DBC pelo Levene.

- `models/pairwise` (versão 2): o ajuste `dunnett` passa a ser o Dunnett
  EXATO (`emmeans`, `adjust = "mvt"`: integração da t multivariada de Genz &
  Bretz 2009), no lugar da aproximação de Hsu (`"dunnettx"`). Os p-valores e
  intervalos mudam pouco (PlantGrowth: 0,3296 → 0,3227). A integração usa a
  semente do nó, então o resultado é reprodutível. Validado contra
  `multcomp::glht(mcp(... = "Dunnett"))`: exato com 2 contrastes (tol. 1e-6),
  diferença máxima de 5e-5 no p com 5 contrastes (tol. 2e-3).
- `models/chisq` (versão 2): `correcao` passa a ser `FALSE` por padrão (X² de
  Pearson sem a correção de Yates, que deixa o teste conservador; Agresti
  2002, *Categorical Data Analysis*, doi:10.1002/0471249688). A opção
  continua. Fluxos antigos com 2 × 2 e sem `correcao` explícita mudam de
  resultado. Validado no Physicians' Health Study (aspirina × infarto,
  Agresti): X² = 25,01 sem e 24,43 com Yates, iguais à forma fechada e a
  `stats::chisq.test` (tol. 1e-10).
