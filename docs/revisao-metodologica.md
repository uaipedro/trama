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
- **Friedman ausente** — a alternativa não paramétrica ao DBC (Friedman 1937, doi:10.1080/01621459.1937.10503522) não tem bloco; os pressupostos de DBC, DQL e fatorial apontam para ela como lacuna. Só o DIC tem saída por postos (`models/kruskal`).
- **Superdispersão na binomial agregada** — o `models/glm` oferece `quasipoisson`, mas não `quasibinomial`, e não há GLM misto (`glmer`) para um efeito aleatório por observação. Contagens de sucessos em n tentativas superdispersas ficam sem saída no trama (McCullagh & Nelder 1989, *Generalized Linear Models*, 2. ed., cap. 4).
- **Parcela subdividida no tempo** — com a subparcela em medidas repetidas, a análise de dois erros supõe esfericidade. O `models/lmer` só especifica efeitos aleatórios, não estrutura de correlação no erro (AR(1), não estruturada, como em `nlme::gls`/`lme`), então não há como relaxar esse pressuposto no trama.

## trama.ml

- **Validação só por divisão aleatória** — o `ml/split` sorteia linhas (estratificando a classe) e o `ml/tune` faz folds aleatórios. Não há divisão temporal nem por grupo (indivíduo, lote, área), nem validação cruzada bloqueada; com dados dependentes o erro estimado fica otimista (Roberts et al. 2017, *Ecography*, 40(8), 913-929, doi:10.1111/ecog.02881). Os pressupostos apontam o `data/filter` para um corte temporal manual; o resto é lacuna.
- **Isolamento do teste não é imposto** — nenhum bloco sabe se recebeu treino ou teste: o `ml/tune` e os ajustes aceitam a tabela inteira, e o `ml/evaluate` mede o que chegar. É escolha de desenho (o fluxo é explícito), mas o isolamento depende do usuário.
- **`ml/cart` sem poda** — `rpart` com `cp = 0`: a árvore só é contida por `max_depth` e `minbucket`, sem a poda por custo-complexidade escolhida por validação cruzada do CART original (Breiman et al. 1984; James et al. 2021, sec. 8.1.1). O `ml/tune` supre em parte, escolhendo profundidade e `min_n`.
- **Importância por impureza** — `ml/forest` usa `importance = "impurity"` e `ml/cart` a de `rpart` (com cortes substitutos); ambas favorecem preditores contínuos ou com muitos valores distintos (Strobl et al. 2007, doi:10.1186/1471-2105-8-25). O `ranger` oferece `"permutation"` e `"impurity_corrected"`, não expostos.
- **`ml/linear` com corte fixo em 0,5** — a logística classifica por probabilidade ≥ 0,5, sem opção de corte; com classes desequilibradas isso favorece a maioritária.
- **`ml/svm`: `.pred` × `.prob_*`** — a classe vem da margem e as probabilidades da calibração de Platt (validação cruzada interna do LIBSVM); as duas podem discordar em linhas perto da fronteira (Chang & Lin 2011, doi:10.1145/1961189.1961199).
- **`ml/roc`: classe positiva padrão** — com `positiva` vazia, usa a segunda classe na ordem de aparição das linhas (`unique`), sem relação com a coluna `.prob_<classe>` escolhida; se não baterem, a curva sai espelhada e a AUC vira 1 − AUC (Fawcett 2006, doi:10.1016/j.patrec.2005.10.010). A ajuda descreve o comportamento; seria mais seguro deduzir a classe do nome da coluna.
- **Sem curva precisão-revocação nem escolha de corte** — com classe rara, a ROC/AUC pode parecer boa enquanto a classe de interesse é mal prevista (Saito & Rehmsmeier 2015, doi:10.1371/journal.pone.0118432); não há bloco para a curva PR nem para escolher o corte de probabilidade.
- **`ml/tune` sem estimativa honesta própria** — a média dos folds do vencedor é otimista (Varma & Simon 2006, doi:10.1186/1471-2105-7-91); não há validação cruzada aninhada. O card exibe essa média; o teste separado é a estimativa a reportar.
