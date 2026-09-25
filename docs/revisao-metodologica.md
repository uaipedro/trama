# Revisão metodológica

Divergências entre implementação e teoria encontradas ao documentar pressupostos e referências. Nada aqui foi corrigido: cada item espera decisão.

## trama.sampling

- **`sampling/proportion`** — intervalo de Wald com t (estimativa ± t·EP). Pode sair de [0, 1] e cobre menos que o nominal com proporção extrema e domínio pequeno; a literatura de amostras complexas recomenda intervalos alternativos (logit, Korn-Graubard/Clopper-Pearson com n efetivo) nesses casos (Lohr 2021, *Sampling: Design and Analysis*, 3. ed., doi:10.1201/9780429298899). A ajuda avisa; o comportamento não mudou.
- **Planejar/Precisão × Estimar** — `size_*`, `margin`, `margin_levels`, `referral`, `detectable_difference` e `question_margins` usam z normal, enquanto as estimativas usam t com gl = UPAs − estratos. Com poucos conglomerados, a margem planejada sai menor que a que o card de `sampling/mean` vai mostrar (Cochran 1977, cap. 2 e 9).
- **`sampling/detectable_difference`** — usa a mesma p nos dois grupos e não aplica correção finita. É a aproximação de pior caso; a fórmula de duas proporções (Fleiss, Levin & Paik 2003, cap. 4, doi:10.1002/0471445428) usa p_a e p_b próprios.
- **`sampling/size_cluster` e `sampling/referral`** — deff = 1 + (m̄ − 1)·ρ supõe conglomerados (ou redes) do mesmo tamanho; com tamanhos desiguais o deff real é maior (Kish 1965, *Survey Sampling*, sec. 5.4).
