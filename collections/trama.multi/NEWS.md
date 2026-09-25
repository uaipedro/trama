# trama.multi (desenvolvimento)

## Blocos novos

* `multi/pr_curve`: curva precisão-revocação dos classificadores (LDA/QDA e
  logística), por deixa-um-fora como a `multi/roc`, com a precisão média (AP
  = Σ ΔR·P) e a área de Davis & Goadrich (2006, doi:10.1145/1143844.1143874)
  integrada em forma fechada (Keilwagen, Grosse & Grau 2014,
  doi:10.1371/journal.pone.0092209), e a prevalência como linha do acaso
  (Saito & Rehmsmeier 2015, doi:10.1371/journal.pone.0118432). Três ou mais
  grupos: cada um contra os outros. A mesma conta da `ml/pr_curve`, copiada
  (a coleção não depende da `trama.ml`). Validação: exemplo à mão da ml;
  `yardstick::average_precision` 1.4.0 a 1e-10 e `PRROC::pr.curve` 1.4
  (`auc.integral`) a 1e-8 no `pima` por deixa-um-fora (AP 0,721, área 0,719,
  acaso 0,333) e nos três cultivares dos `vinhos` com empates.

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

## `confianca` na `multi/logistic_coefficients`

* `multi/logistic_coefficients` (versão 3): o param do nível do intervalo passa
  de `nivel` a `confianca`, a convenção da coleção (`multi/roc`) e das irmãs.
  O núcleo não tem migração de params: um fluxo salvo com `nivel` acusa
  `unknown_param` (e `version_drift`) ao ser validado e precisa do param
  renomeado. Sem alias: o nó exige que o `fn` seja a função exportada e que
  cada argumento dela seja um param declarado, então `nivel =` também deixa de
  ser aceito na chamada R. Resultado igual.

## Intervalo perfilado na logística ML

* `multi/logistic_coefficients` (versão 4) ganha `intervalo = c("perfilado",
  "Wald")`, com **perfilado como padrão**: na logística ML binária, o IC da
  verossimilhança perfilada (Venables & Ripley 2002, sec. 7.2,
  doi:10.1007/978-0-387-21706-2) e o p da razão de verossimilhanças. O de
  Wald supõe a log-verossimilhança quadrática; Hosmer, Lemeshow & Sturdivant
  (2013, sec. 1.4, doi:10.1002/9781118548387) preferem o perfilado em amostra
  pequena, e com n grande os dois coincidem. **Muda o resultado padrão** (os
  limites e o p da ML binária; coeficientes e EP iguais); `intervalo = "Wald"`
  reproduz o anterior. Na multinomial fica Wald (o `nnet::multinom` não tem
  perfil e não há implementação de referência para conferir um próprio),
  dito na coluna `intervalo`. No Firth, `Wald` passa a ser opção também.
  Validação: `confint` do `glm` (o perfil do MASS, no `stats` desde o R 4.4)
  com grade fina a 1e-4 (2e-6 observado) no `pima` inteiro e numa
  subamostra de 64, a 95% e 90%; a grade padrão erra ~6e-4 na subamostra
  pela spline, e os limites do trama conferem pela definição (desvio
  perfilado = χ²₁ a 1e-6); p contra `drop1(test = "LRT")` a 1e-10. No `pima`
  (glicose, imc, pedigree), a razão de chances do pedigree 3,71 tem IC
  1,88–7,45 perfilado e 1,86–7,39 de Wald.

## AUC multiclasse de Hand & Till

* `multi/roc` (versão 3): com três ou mais grupos, o subtítulo traz a AUC
  multiclasse M de Hand & Till (2001, *Machine Learning* 45(2):171–186,
  doi:10.1023/A:1010920819831) — média, sobre os pares de grupos, de
  [A(i|j) + A(j|i)]/2, cada A só com os casos do par. Não depende das
  proporções dos grupos. Sem intervalo. Validação: `pROC::multiclass.roc`
  1.19.1 a 1e-10 (LDA da `iris` por deixa-um-fora, M = 0,998133, diferença
  1e-16; logística dos `vinhos`; 4 grupos com empates) e conta à mão.
