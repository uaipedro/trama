# trama.models (desenvolvimento)

## Blocos novos

- `models/friedman`: teste de Friedman (1937, doi:10.1080/01621459.1937.10503522)
  para o DBC de um fator, uma observação por bloco e tratamento (casela
  repetida recusa; bloco incompleto sai inteiro, contado na nota), estatística
  corrigida para empates e W de Kendall como efeito. É a saída não paramétrica
  que os pressupostos do `models/anova_dbc` apontam. Validado contra
  `stats::friedman.test` (1e-12) e o exemplo de Hollander & Wolfe (1973, p.
  140; 22 jogadores × 3 métodos), S = 11,14 com correção para empates.
- `models/scott_knott`: agrupamento de Scott & Knott (1974, *Biometrics*
  30:507-512, doi:10.2307/2529204) sobre o QM e os gl do erro do quadro, uma
  letra por média. Implementação própria das fórmulas do artigo. Recusa dados
  desbalanceados e termos não ortogonais ao tratamento (bloco incompleto,
  covariável), em que as médias da tabela deixam de ter variância comum. Reproduz
  o exemplo publicado do sorgo em Jelihovschi, Faria & Allaman (2014, TEMA
  15(1), Fig. 1; doi:10.5540/tema.2014.015.01.0003): dois grupos, de cima
  14 8 5 7 9 3 1 4 2 (esse exemplo é um látice, que o bloco recusa; a
  reprodução é da função interna com as médias da tabela). Mesma partição que
  `ScottKnott::SK` 1.4.0 no `milho_dbc` e no PlantGrowth, e que a conta à mão.

## Mudanças de método

- `models/levene` (com bloco): o equilíbrio passa a exigir o mesmo número de
  parcelas em cada casela tratamento × bloco (e × linha, × coluna no DQL),
  não só alavancas iguais. O tamanho do teste de O'Neill & Mathews foi medido
  por simulação sob H0 (20000 réplicas, semente fixa, nível 5%): DBC 5 × 6
  4,6% (o Levene comum nos resíduos: 8,5%), DBC 5 × 10 4,6%, DQL 8 × 8 4,7%
  (comum: 7,9%); em desenho pequeno o multiplicador, que acerta a média do F e
  não a cauda, deixa o teste conservador: DBC 4 × 3 2,9% (comum: 12,0%), DQL
  5 × 5 2,9% (comum: 5,1%), DQL 4 × 4 2,3%. A ajuda e os pressupostos dizem
  isso; um teste fixa DBC 5 × 6 e DQL 8 × 8 em 5% ± 1 ponto.

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
  resultado. Validado nas contagens do Physicians' Health Study (aspirina ×
  infarto, 189/10845 e 104/10933, como em Agresti): X² = 25,01 sem e 24,43
  com Yates, calculados por `stats::chisq.test` e pela forma fechada do 2 × 2,
  iguais a 1e-10 (os valores não foram conferidos no texto do livro).
