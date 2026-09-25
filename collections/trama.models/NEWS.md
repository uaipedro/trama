# trama.models (desenvolvimento)

## Mudanças de método

- `models/pairwise` (versão 2): o ajuste `dunnett` passa a ser o Dunnett
  EXATO (`emmeans`, `adjust = "mvt"`: integração da t multivariada de Genz &
  Bretz 2009), no lugar da aproximação de Hsu (`"dunnettx"`). Os p-valores e
  intervalos mudam pouco (PlantGrowth: 0,3296 → 0,3227). A integração usa a
  semente do nó, então o resultado é reprodutível. Validado contra
  `multcomp::glht(mcp(... = "Dunnett"))`: exato com 2 contrastes (tol. 1e-6),
  diferença máxima de 5e-5 no p com 5 contrastes (tol. 2e-3).
