# trama.multi (desenvolvimento)

## Blocos novos

* `multi/mardia`: teste de normalidade multivariada de Mardia (1970,
  doi:10.1093/biomet/57.3.519) — assimetria b1,p (χ², e com a correção de
  amostra pequena de Mardia 1974) e curtose b2,p (z), na tabela toda ou dentro
  de cada grupo. Covariância de divisor n, como no artigo e em
  `MVN::mardia(use_population = TRUE)`. Validação: `psych::mardia` reescalado
  (divisor n − 1) a 1e-10 em quatro tabelas; estatísticas e p iguais ao código
  do `MVN::mardia` 6.3 a 1e-13. Os pressupostos de normalidade da
  discriminante, do M de Box, da fatorial e do KMO/Bartlett passam a apontá-lo.

## Logística de Firth

* `multi/logistic` ganha `metodo = c("ml", "firth")` (padrão `ml`, sem mudança
  de resultado). `firth` (só binária) maximiza a verossimilhança penalizada de
  Firth (1993, doi:10.1093/biomet/80.1.27): coeficientes finitos mesmo com
  separação. `multi/logistic_coefficients` (versão 2) devolve a coluna nova
  `intervalo` (`Wald` ou `perfilado`); no Firth, IC da verossimilhança
  penalizada perfilada e p da razão de verossimilhanças penalizadas (Heinze &
  Schemper 2002, doi:10.1002/sim.1047). EP pela inversa da informação de
  Fisher em β̂ (o `logistf` usa (X'W(1 + h)X)⁻¹ e dá EP menores). Validação:
  `logistf` 1.26.1 no `sex2` — coeficientes a 1e-6, limites e p a 1e-4
  (também com confiança 0,9 e num exemplo com separação completa); nos
  `vinhos` B × C quase separados, onde o `logistf` não converge em quatro dos
  oito limites, os limites conferem pela definição (χ²₁ do perfil a 1e-6) e
  batem com os quatro em que ele converge.

## Intervalo da AUC

* `multi/roc` (versão 2): a AUC sai com o intervalo de DeLong, DeLong &
  Clarke-Pearson (1988, doi:10.2307/2531595), no subtítulo (dois grupos) ou
  na legenda (cada grupo contra os outros), com o param novo `confianca`
  (0,95). Validação: `pROC::ci.auc(method = "delong")` 1.18 a 1e-8 no `aSAH`
  (três escores, com empates; também a 90%) e nas probabilidades de
  deixa-um-fora da logística do `pima` (AUC 0,849, IC 0,816–0,882).

## Métricas da matriz de confusão

* `multi/confusion` ganha `tabela = c("matriz", "métricas")` (padrão `matriz`,
  sem mudança). `métricas` devolve acurácia, acurácia balanceada (Brodersen et
  al. 2010, doi:10.1109/ICPR.2010.764), kappa de Cohen (1960,
  doi:10.1177/001316446002000104) e precisão, revocação e F1 por grupo
  (Sokolova & Lapalme 2009); precisão de grupo nunca previsto é NA. Validação:
  contas à mão; exemplo 20/5/10/15 (κ = 0,4, o exemplo da página "Cohen's
  kappa" da Wikipédia — não é fonte primária); `irr::kappa2` e
  `psych::cohen.kappa` a 1e-12 nas previsões cruzadas da LDA do `iris`.
