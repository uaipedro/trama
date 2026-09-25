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
