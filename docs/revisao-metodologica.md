# Revisão metodológica

Divergências entre implementação e teoria encontradas ao documentar pressupostos e referências. Nada aqui foi corrigido: cada item espera decisão.

## trama.sampling

- **`sampling/proportion`** — intervalo de Wald com t (estimativa ± t·EP). Pode sair de [0, 1] e cobre menos que o nominal com proporção extrema e domínio pequeno; a literatura de amostras complexas recomenda intervalos alternativos (logit, Korn-Graubard/Clopper-Pearson com n efetivo) nesses casos (Lohr 2021, *Sampling: Design and Analysis*, 3. ed., doi:10.1201/9780429298899). A ajuda avisa; o comportamento não mudou.
- **Planejar/Precisão × Estimar** — `size_*`, `margin`, `margin_levels`, `referral`, `detectable_difference` e `question_margins` usam z normal, enquanto as estimativas usam t com gl = UPAs − estratos. Com poucos conglomerados, a margem planejada sai menor que a que o card de `sampling/mean` vai mostrar (Cochran 1977, cap. 2 e 9).
- **`sampling/detectable_difference`** — usa a mesma p nos dois grupos e não aplica correção finita. É a aproximação de pior caso; a fórmula de duas proporções (Fleiss, Levin & Paik 2003, cap. 4, doi:10.1002/0471445428) usa p_a e p_b próprios.
- **`sampling/size_cluster` e `sampling/referral`** — deff = 1 + (m̄ − 1)·ρ supõe conglomerados (ou redes) do mesmo tamanho; com tamanhos desiguais o deff real é maior (Kish 1965, *Survey Sampling*, sec. 5.4).

## trama.models

- **`models/pairwise`** — o ajuste `dunnett` chama `emmeans::contrast(adjust = "dunnettx")`, a aproximação de Hsu para a distribuição de Dunnett, e não o Dunnett exato (`adjust = "mvt"`, integração da t multivariada). A diferença é pequena, mas o card diz "Dunnett" (Dunnett 1955, doi:10.1080/01621459.1955.10501294).
- **`models/chisq`** — `correcao = TRUE` por padrão: a correção de Yates (1934, doi:10.2307/2983604) em toda tabela 2 × 2, que torna o teste conservador. É o padrão de `stats::chisq.test`, mas é escolha discutível como padrão de um bloco didático.
- **`models/levene` e `models/bartlett`** — aplicados aos RESÍDUOS do modelo agrupados pelos tratamentos (também no DBC, DQL e fatorial com bloco), e não às observações de cada grupo como na formulação original (Levene 1960; Bartlett 1937, doi:10.1098/rspa.1937.0109). Os gl do teste não descontam os parâmetros do modelo (bloco), o que pode deixá-lo levemente liberal com poucos gl de resíduo. No DIC as duas formas coincidem (resíduo = desvio da média do grupo).
