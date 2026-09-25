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
