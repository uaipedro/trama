# Revisão metodológica

Divergências entre implementação e teoria encontradas ao documentar pressupostos e referências, e o que se decidiu sobre cada uma. Item marcado **Resolvido** traz o commit, a referência conferida, o oráculo e a tolerância; os demais esperam decisão.

## trama.sampling

### Resolvido

- **`sampling/proportion`** — intervalo de Wald com t (estimativa ± t·EP). Pode sair de [0, 1] e cobre menos que o nominal com proporção extrema e domínio pequeno; a literatura de amostras complexas recomenda intervalos alternativos (logit, Korn-Graubard/Clopper-Pearson com n efetivo) nesses casos (Lohr 2021, *Sampling: Design and Analysis*, 3. ed., doi:10.1201/9780429298899). A ajuda avisa; o comportamento não mudou. **Resolvido** em 48d932a: padrão `intervalo = "logit"` (Wald na escala log-odds, EP pelo método delta, t nos gl do desenho; Korn & Graubard 1999, *Analysis of Health Surveys*, doi:10.1002/9781118032619; padrão de `survey::svyciprop`), com `wilson` (escore de Wilson 1927 com n efetivo de Kish e t) e `wald` como opções; `sampling/proportion` versão 2. Validação: `survey::svyciprop(method = "logit", df = degf)` 4.x em AAS com fpc, estratificada, conglomerado em um estágio e domínio (fazendas): estimativa ≤ 1e-8 e limites ≤ 1e-6 (AAS n = 200: [0,289124890889522; 0,416188372013590], iguais a 1e-9); Wilson conferido contra `stats::prop.test(correct = FALSE)` (1e-10). **Extremos resolvidos** em 7de869e: com p̂ = 0 ou 1 o logit e o wilson degeneravam no ponto; agora usam o Clopper-Pearson com n efetivo de Korn & Graubard (1998, *Survey Methodology* 24(2):193-201, Statistics Canada 12-001-X199800204356; sem DOI no Crossref, conferido no catálogo da StatCan — PDF escaneado, fórmula conferida no `svyciprop` do survey e em Ward 2019, *Stata Journal* 19(3):510-522, doi:10.1177/1536867x19874221, só metadados/resumo), n_ef = p̂(1 − p̂)/v ajustado por (t_{n−1}/t_gl)²; nos extremos (v = 0) n_ef = n nominal do domínio, ainda ajustado pelos gl (convenção documentada; o survey dá NaN aí). Nova opção `intervalo = "clopper_pearson"`; guarda para estimativa NA; `sampling/proportion` versão 3. Validação: `survey::svyciprop(method = "beta")` em AAS (n = 200, semente 11: [0,286693285495; 0,417466839166]), estratificada e conglomerados (12 municípios, semente 3: [0,225426037938; 0,518145294072]), ≤ 1e-6; extremos numa AAS sem fpc (n = 150) iguais a `stats::binom.test` (1e-10), e 0/n dá [0, 1 − 0,025^(1/n)]. Limite: com conglomerados, o n nominal nos extremos não desconta o deff e pode cobrir menos que o nominal (documentado).
- **Planejar/Precisão × Estimar** — `size_*`, `margin`, `margin_levels`, `referral`, `detectable_difference` e `question_margins` usam z normal, enquanto as estimativas usam t com gl = UPAs − estratos. Com poucos conglomerados, a margem planejada sai menor que a que o card de `sampling/mean` vai mostrar (Cochran 1977, cap. 2 e 9). **Resolvido** em b6f9c4a: param `distribuicao` (`t` padrão, `z` declarado) em todos esses blocos, t com gl = UPAs − estratos (n − 1, n − H, conglomerados − 1, redes − unidades; nos níveis, entrevistas − unidades), e o n planejado é o menor n cuja margem, com os gl dele, cabe na pedida; versões 2. Validação: busca exaustiva do menor n (margens 0,02 a 0,3; média a 99%; população finita; conglomerados com m̄ 5 e 20, ρ 0,02 e 0,2), igual exatamente; `z` reproduz os números clássicos (385, 1.068) e a versão 1. **Borda resolvida** em 8d9b143: margem muito folgada dava n = 1 e "t com 0 gl"; a busca começa no menor n com gl ≥ 1 (2, H + 1, 2 conglomerados) e `size_stratified` respeita o piso de 2 por estrato da alocação (antes dava erro). Validação: menor n por força bruta com t_{n−1} (σ = 10, E = 1000 ⇒ n = 2). Não validado contra exemplo resolvido de Cochran (1977) ou Bolfarine & Bussab (2005) com t: sem acesso ao texto para conferir página.
- **`sampling/detectable_difference`** — usa a mesma p nos dois grupos e não aplica correção finita. É a aproximação de pior caso; a fórmula de duas proporções (Fleiss, Levin & Paik 2003, cap. 4, doi:10.1002/0471445428) usa p_a e p_b próprios. **Resolvido** em b6f9c4a: equação de duas proporções de Fleiss, Levin & Paik (2003, cap. 4, sem correção de continuidade) com n desiguais, p de referência por grupo (`proporcao_grupo`), correção finita opcional (`populacao`) e t com n_a + n_b − 2 gl; a DMD é o pior caso entre as duas referências e os dois sentidos; versão 2. Validação: com n iguais, z e sem fpc, igual a `stats::power.prop.test(n = 500, p1 = 0,5, power = 0,8, tol = 1e-12)` (8,8145 pontos, tolerância 1e-8); forma de Fleiss com razão r = n_b/n_a satisfeita a 1e-6; equação com fpc e t conferida no δ que sai (1e-9). Ajuda e site passam a dizer "sem correção de continuidade" e trazem a fórmula da versão 2 (4bdc41c).
- **`sampling/size_cluster` e `sampling/referral`** — deff = 1 + (m̄ − 1)·ρ supõe conglomerados (ou redes) do mesmo tamanho; com tamanhos desiguais o deff real é maior (Kish 1965, *Survey Sampling*, sec. 5.4). **Resolvido** em b6f9c4a: `cv_tamanho` (size_cluster) e `cv_rede` (referral), deff = 1 + ((CV² + 1)·m̄ − 1)·ρ (Eldridge, Ashby & Kerry 2006, doi:10.1093/ije/dyl129); CV = 0 (padrão) reproduz o deff anterior exatamente (teste de regressão). Validação: identidade com a variância exata da média Σy/Σm sob ICC comum, Var = σ²·Σ m_i(1 + (m_i − 1)ρ)/(Σm_i)², num vetor de 8 tamanhos (1e-12). Fórmula conferida na fonte (2026-09-25): Crossref confirma Eldridge, Ashby & Kerry, *Int. J. Epidemiol.* 35(5):1292-1300, 2006 (sem resumo no registro); o resumo no PubMed (PMID 16943232) descreve a "fórmula simples" com o CV do tamanho, sem escrevê-la; o texto integral não estava acessível. A forma DE = 1 + {(CV² + 1)·m̄ − 1}·ρ foi conferida em fonte secundária que a cita: Kristunas et al. 2017, *Trials* 18:109, doi:10.1186/s13063-017-1832-8 (texto integral no Europe PMC, PMC5341460), que a atribui a Eldridge et al. (2006) e Manatunga et al. (2001), com CV = desvio padrão/média do tamanho — igual à implementada.

## trama.models

### Resolvido

- **Superdispersão na binomial agregada** — o `models/glm` oferece `quasipoisson`, mas não `quasibinomial`, e não há GLM misto (`glmer`) para um efeito aleatório por observação. Contagens de sucessos em n tentativas superdispersas ficam sem saída no trama (McCullagh & Nelder 1989, *Generalized Linear Models*, 2. ed., cap. 4). **Resolvido** em 7f79a5f e c81d60c: `models/glm` ganha `quasibinomial` (Wedderburn 1974, doi:10.1093/biomet/61.3.439; igual a `stats::glm` no `cbpp`, coeficientes e EP a 1e-10, dispersão 2,19) e o novo `models/glmer` (binomial/Poisson, `nivel_obs` = efeito por observação, Harrison 2014, doi:10.7717/peerj.616) reproduz o `gm1` de `?lme4::glmer` no `cbpp` (fixos −1,3983, −0,9919, −1,1282, −1,5797; variância do rebanho 0,4123; AIC 194,1 — saída impressa do lme4, artigo de Bates et al. 2015 não conferido página a página).
- **Parcela subdividida no tempo** — com a subparcela em medidas repetidas, a análise de dois erros supõe esfericidade. O `models/lmer` só especifica efeitos aleatórios, não estrutura de correlação no erro (AR(1), não estruturada, como em `nlme::gls`/`lme`), então não há como relaxar esse pressuposto no trama. **Resolvido** em 5bba7a5: `models/gls` (`nlme::gls`; AR(1), simetria composta, não estruturada, `varIdent`; Pinheiro & Bates 2000, doi:10.1007/b98882). Validação contra o `nlme` direto: `Ovary` AR(1) phi 0,7532 e logLik −780,7273; simetria composta = `lme` de intercepto aleatório; `Orthodont` `corSymm` + `varIdent`; valores de saída do nlme, não conferidos no livro.
- **Friedman ausente** — a alternativa não paramétrica ao DBC (Friedman 1937, doi:10.1080/01621459.1937.10503522) não tem bloco; os pressupostos de DBC, DQL e fatorial apontam para ela como lacuna. Só o DIC tem saída por postos (`models/kruskal`). **Resolvido** em 81401a2: `models/friedman` (DBC de um fator, uma observação por casela, bloco incompleto sai inteiro, correção para empates, W de Kendall como efeito); os pressupostos do DBC apontam para ele; DQL e fatorial dizem que não há teste por postos no trama. Validação: `stats::friedman.test` (1e-12) nos dados do exemplo de `?friedman.test` (atribuídos a Hollander & Wolfe 1973; S = 11,14, p = 0,0038 calculados por nós, página do livro não conferida). Bloco incompleto **resolvido** no `models/friedman` versão 2 (`metodo = auto/friedman/durbin/skillings_mack`; `auto` escolhe pelo desenho): Durbin (1951, doi:10.1111/j.2044-8317.1951.tb00310.x; T1 de Conover 1999 com empates) só em BIB, recusa classificada apontando o Skillings–Mack; Skillings & Mack (1981, doi:10.1080/00401706.1981.10486261) para faltantes quaisquer. Validação: `agricolae::durbin.test` (1e-12; sorvete, T1 = 12, p = 0,0620, calculado, página de Conover não conferida; e com empates); `Skillings.Mack::Ski.Mack` (1e-8; exemplo do pacote SM = 15,493, 3 gl, e faltantes ao acaso com empates); igualdade ao `friedman.test` em dados completos sem empate. `PMCMRplus` não instala (Rmpfr pede MPFR do sistema). Commit 731a544.
- **`models/levene` e `models/bartlett`** — aplicados aos RESÍDUOS do modelo agrupados pelos tratamentos (também no DBC, DQL e fatorial com bloco), e não às observações de cada grupo como na formulação original (Levene 1960; Bartlett 1937, doi:10.1098/rspa.1937.0109). Os gl do teste não descontam os parâmetros do modelo (bloco), o que pode deixá-lo levemente liberal com poucos gl de resíduo. No DIC as duas formas coincidem (resíduo = desvio da média do grupo). **Resolvido** em 31837a1: no DBC, fatorial em DBC e DQL o `models/levene` (versão 2) passa a ser o teste de O'Neill & Mathews (2002, *Biometrics* 58:216-224, doi:10.1111/j.0006-341x.2002.00216.x): ANOVA dos |resíduos| de mínimos quadrados em tratamento + bloco (+ linha e coluna) com o F multiplicado pelo fator do delineamento (razão dos QM esperados de |e| sob H0, pelas correlações dos resíduos); desbalanceado é recusado. O `models/bartlett` (versão 2) recusa delineamento com bloco (`tr_models_error_block_design`), sem correção publicada, e aponta o Levene. DIC sem mudança. Validação: `ExpDes.pt::oneilldbc` 1.2.2 (arquivado no CRAN; p fixos no teste) em warpbreaks com 9 blocos (F = 3,1517, p = 0,0171832) e no `ex4` do ExpDes.pt (p = 0,3070816), iguais a 1e-14 (tolerância 1e-8); fator fechado do DBC reproduzido a 1e-10; no DQL o fator foi conferido por Monte Carlo (OrchardSprays, 20000 réplicas, 1%; no 7 × 7 do `ex3`, rejeição a 5% de 8,3% sem e 5,0% com a correção). Parcela subdividida **resolvida** em 6e32a7b: `models/levene` e `models/bartlett` (versão 3) recusam com `tr_models_error_block_design` — a correção de O'Neill & Mathews supõe um estrato de erro, e no erro (b) o fator da parcela está confundido com a parcela (o "bloco" desse estrato), então não há correção validada; em simulação sob H0 no desenho da aveia (4000 réplicas) o Levene comum nos resíduos (b) rejeita 10,5% a 5% (centro na média) e 2,5% (mediana). A mensagem aponta o painel escala-locação e o `models/lmer`. Tamanho medido em fecb410 (20000 réplicas sob H0, 5%): DBC 5 × 6 4,6%, 5 × 10 4,6%, DQL 8 × 8 4,7% (Levene comum: 8,5% e 7,9%); conservador em desenho pequeno — DBC 4 × 3 2,9%, DQL 5 × 5 2,9%, 4 × 4 2,3% —, documentado; equilíbrio passa a exigir casela tratamento × bloco (linha, coluna) com a mesma contagem. O teste do DQL anterior conferia só a aritmética do multiplicador.
- **`models/pairwise`** — o ajuste `dunnett` chama `emmeans::contrast(adjust = "dunnettx")`, a aproximação de Hsu para a distribuição de Dunnett, e não o Dunnett exato (`adjust = "mvt"`, integração da t multivariada). A diferença é pequena, mas o card diz "Dunnett" (Dunnett 1955, doi:10.1080/01621459.1955.10501294). **Resolvido** em 598f09c: `adjust = "mvt"` (Dunnett exato) com a semente do nó; `models/pairwise` versão 2. Validação: `multcomp::glht(mcp(... = "Dunnett"))` em PlantGrowth (2 contrastes, igual a 1e-6: p = 0,3227 e 0,1535) e InsectSprays (5 contrastes, diferença máxima de 5e-5 no p, tolerância 2e-3; intervalos iguais).
- **`models/chisq`** — `correcao = TRUE` por padrão: a correção de Yates (1934, doi:10.2307/2983604) em toda tabela 2 × 2, que torna o teste conservador. É o padrão de `stats::chisq.test`, mas é escolha discutível como padrão de um bloco didático. **Resolvido** em 15a93e2: padrão `correcao = FALSE` (Agresti 2002, *Categorical Data Analysis*, doi:10.1002/0471249688), opção mantida; `models/chisq` versão 2. Validação: nas contagens do Physicians' Health Study (aspirina × infarto, como em Agresti), X² = 25,01 sem e 24,43 com Yates calculados por `stats::chisq.test` e pela forma fechada do 2 × 2, iguais a 1e-10 (valores não conferidos no texto do livro).

- **`models/scott_knott` com repetições desiguais** — a main recusava todo desbalanceado, com o argumento de que as médias não têm a variância comum QM / r; a branch tinha outro partidor, que já tratava o caso como o pacote `ScottKnott` (Jelihovschi, Faria & Allaman 2014, doi:10.5540/tema.2014.015.01.0003). **Resolvido** em 1aa8efa: partidor único com s² = média de QM / rᵢ no grupo que se parte, recalculada a cada nível (o `ScottKnott:::MaxValue`, linha `s2c`); o DIC desbalanceado passa a ser aceito; continua recusado o que tem razão estatística — termo não ortogonal ao tratamento (bloco incompleto, DBC com parcela perdida, covariável) e parcela subdividida desbalanceada. `models/scott_knott` versão 2 (no balanceado os grupos da versão 1 não mudam: a média de QM / r constante é QM / r). Validação contra `ScottKnott::SK` 1.4-0, partição idêntica (grupos exatos, sem tolerância): DIC 6 tratamentos com r = 3, 5, 4, 5, 5, 5 (F a, E b, C e D c, B d, A e), CRD1 e RCBD do `?SK`, `milho_dbc` (c c a c b), InsectSprays (a a b b b a) e CRD2 (45 tratamentos, grupos de 6, 33, 3 e 3); o sorgo publicado (Jelihovschi et al. 2014, Fig. 1) pela função interna.
- **Regressão nos tratamentos quantitativos em dois blocos** — `models/polinomial` (main: só o quadro, até o grau 5, DIC/DBC) e `models/dose_response` (branch: quadro + curva, até o cúbico, com DQL, MET e falta de ajuste do grau escolhido) faziam a mesma decomposição (Pimentel-Gomes 2009; Banzatto & Kronka 2006). **Resolvido** em a86e479: um bloco, id `models/polinomial` versão 2, com as duas saídas (`quadro` e `modelo`, a curva ponderada com o erro da ANOVA nos coeficientes); nenhum grau significativo no automático dá a curva de grau 0 (média geral) em vez de erro. Migração `models/dose_response` → `models/polinomial`, `grau` numérico da main → `grau_max`, porta `out` → `quadro`. Validação: SQ do exemplo do algodão de Montgomery (tab. 3.1; 33,62, 343,21, 64,98 e 33,95, a duas casas) e ANOVA sequencial de `lm(y ~ poly(x, 4))` (1e-10); `contr.poly(scores = doses reais)` com doses 0, 50, 100, 200, 400 (1e-10); DBC com duas parcelas perdidas contra `anova(lm(y ~ bloco + x + x² + x³ + factor(dose)))`, SQ, p e gl (1e-10). Não validado contra exemplo publicado de Banzatto & Kronka ou Pimentel-Gomes (página não conferida nesta integração); a página exata de Montgomery com essas SQ também não foi conferida — os valores são os fixados pela main.

## trama.ml


### Resolvido

- **Isolamento do teste não é imposto** — nenhum bloco sabe se recebeu treino ou teste: o `ml/tune` e os ajustes aceitam a tabela inteira, e o `ml/evaluate` mede o que chegar. É escolha de desenho (o fluxo é explícito), mas o isolamento depende do usuário. **Resolvido** em da77947: o `ml/split` marca as saídas (atributo `tr_ml_origem`: papel treino/teste e id da divisão; atributo e não coluna porque o `data/table` guarda em RDS, que o preserva, e filtros/colunas novas também). Ajustar no teste (modelos, `ml/tune`, `ml/nested_cv`) é recusado com `tr_ml_error_test_leak` (vazamento de Kaufman et al. 2012, doi:10.1145/2382577.2382579); `ml/predict` propaga a marca e recusa o teste de outra divisão (`tr_ml_error_split_mismatch`); `ml/evaluate`/`confusion`/`roc`/`pr_curve` recusam previsões do treino (`tr_ml_error_train_eval`) salvo `permitir_treino = TRUE`, que devolve o mesmo número com aviso e nota de otimismo (erro por padrão porque o aviso se perde no card). Sem marca (divisão por fora, treino+teste juntados), comportamento anterior, dito nos pressupostos. Validação: testes de cada caminho, inclusive no fluxo real com store e segunda execução lida do cache e pelo `store`/`restore` do `data/table`; números com `permitir_treino` iguais aos da tabela sem marca.
- **Revisão do isolamento (da77947 contornável)** — o revisor reproduziu desvios: ajustar na tabela inteira e prever o teste (B1), juntar treino e teste com a marca de treino (B2), mistura de previsões de treino e teste avaliada como teste (B4), além das operações que perdem o atributo (B3). **Resolvido** em 63ec621 (impressões digitais por linha: xxHash64 do conteúdo nas colunas da divisão, multiconjunto com cópias no teste e no treino, modelo guarda as do seu treino; 100 mil linhas ≈ 1 s e ≈ 7 MB) e 56ece37 (B3 documentado: junção à direita, pivot, recriação à mão, colunas reescritas/removidas, divisão por fora). Validação: um teste por desvio dos scripts do revisor e ausência de falso positivo com linhas duplicadas entre os lados (5 sementes) e nos três fluxos de `exemplos/machine-learning`. Junto, na 0.4.0: IC de DeLong NA com nota para AUC 0/1 (688b87e) e para menos de 2 por classe (168b489); aviso de fold de validação com uma classe e `grupo_estratificado` (cf750a0); precisão indefinida NA fora das médias, como `zero_division = np.nan` (2b3d355); importância com `medida` visível e permutação = aumento do Brier do `ranger` (8845ef2).
- **Validação só por divisão aleatória** — o `ml/split` sorteia linhas (estratificando a classe) e o `ml/tune` faz folds aleatórios. Não há divisão temporal nem por grupo (indivíduo, lote, área), nem validação cruzada bloqueada; com dados dependentes o erro estimado fica otimista (Roberts et al. 2017, *Ecography*, 40(8), 913-929, doi:10.1111/ecog.02881). Os pressupostos apontam o `data/filter` para um corte temporal manual; o resto é lacuna. **Resolvido** em 8605879: `estrategia` = `aleatoria`/`temporal`/`grupo` no `ml/split` e no `ml/tune` (padrão inalterado); CV temporal por origem móvel com janela crescente (Tashman 2000, doi:10.1016/S0169-2070(00)00065-0; FPP3 sec. 5.10; Bergmeir, Hyndman & Koo 2018, doi:10.1016/j.csda.2017.11.003). Validação: contabilidade exata dos folds (20 dias em 5 blocos; cada linha valida uma vez por grupo) e ausência de vazamento (nenhum grupo dos dois lados, todo teste posterior ao treino).
- **Importância por impureza** — `ml/forest` usa `importance = "impurity"` e `ml/cart` a de `rpart` (com cortes substitutos); ambas favorecem preditores contínuos ou com muitos valores distintos (Strobl et al. 2007, doi:10.1186/1471-2105-8-25). O `ranger` oferece `"permutation"` e `"impurity_corrected"`, não expostos. **Resolvido** em fb874ad: `ml/forest` ganha `importancia` = `impureza` (padrão) / `permutacao` / `impureza_corrigida` (Nembrini, König & Wright 2018, doi:10.1093/bioinformatics/bty373). Validação: `ranger` chamado direto com a mesma semente, igual a 1e-12. CART segue com a importância do `rpart` (documentada).
- **Sem curva precisão-revocação nem escolha de corte** — com classe rara, a ROC/AUC pode parecer boa enquanto a classe de interesse é mal prevista (Saito & Rehmsmeier 2015, doi:10.1371/journal.pone.0118432); não há bloco para a curva PR nem para escolher o corte de probabilidade. **Resolvido** em a0dd5cc (`ml/pr_curve`: AP e área de Davis & Goadrich 2006, doi:10.1145/1143844.1143874; oráculos `yardstick::average_precision` 1e-10 e `PRROC::pr.curve` 1e-8; exemplo à mão AP 0,7556 / área 0,7161) e 144c859 (`ml/roc` versão 3: corte de Youden e IC de DeLong 1988, doi:10.2307/2531595, iguais ao `pROC` a 1e-8).
- **`ml/tune` sem estimativa honesta própria** — a média dos folds do vencedor é otimista (Varma & Simon 2006, doi:10.1186/1471-2105-7-91); não há validação cruzada aninhada. O card exibe essa média; o teste separado é a estimativa a reportar. **Resolvido** em 629eafb: `ml/nested_cv`. Validação: fold externo refeito à mão e, em ruído puro (12 réplicas semeadas, n = 40, SVM), aninhada 0,519 de acurácia (acaso 0,5; tolerância 0,06) contra 0,604 da não aninhada.
- **`ml/roc`: classe positiva padrão** — com `positiva` vazia, usa a segunda classe na ordem de aparição das linhas (`unique`), sem relação com a coluna `.prob_<classe>` escolhida; se não baterem, a curva sai espelhada e a AUC vira 1 − AUC (Fawcett 2006, doi:10.1016/j.patrec.2005.10.010). A ajuda descreve o comportamento; seria mais seguro deduzir a classe do nome da coluna. **Resolvido** em eacfb9c: `positiva` vazia ⇒ classe do nome da coluna `.prob_<classe>`; se não houver, segundo nível do fator (ordem alfabética para texto); `ml/roc` versão 2. Validação: teste de regressão com `y = sim, nao, sim, nao`, `.prob_sim = .8, .1, .7, .4` (AUC 0 → 1) e AUC igual a U/(n1·n0) de Mann-Whitney via `stats::wilcox.test` (Hanley & McNeil 1982), 0,82 num exemplo com empate (tolerância 1e-12). `ml/evaluate` e `ml/confusion` não usam classe positiva (métricas macro e tabela completa), então não há padrão a alinhar. Após a revisão da fase 1 (8984dcd): sem `.prob_<classe>` reconhecível, o bloco não adivinha mais o segundo nível — recusa com `tr_ml_error_positive_required`.
- **`ml/cart` sem poda** — `rpart` com `cp = 0`: a árvore só é contida por `max_depth` e `minbucket`, sem a poda por custo-complexidade escolhida por validação cruzada do CART original (Breiman et al. 1984; James et al. 2021, sec. 8.1.1). O `ml/tune` supre em parte, escolhendo profundidade e `min_n`. **Resolvido** em df0edad: poda por custo-complexidade com a regra 1-EP sobre o `xval` de 10 folds do `rpart` (Breiman et al. 1984, sec. 3.4.3), `cp` e `poda` (`1ep`/`minimo`/`nenhuma`) expostos; `ml/cart` e `ml/tune` versão 2. Validação: `printcp`/`prune` do `rpart` reproduzidos em `airquality` (casos completos, semente 42): `cptable` idêntico (1e-12), árvore cheia com 30 divisões, 1-EP com 3, previsões iguais às de `prune(cp)`. Após a revisão (35a3869): `xval = min(10, n)` (deixa-um-fora com n ≤ 10), conferido contra `rpart` com `xval = 1:n` (1e-12, n = 3, 5, 8); custo da poda interna no `ml/tune` documentado (5f4750f).
- **`ml/linear` com corte fixo em 0,5** — a logística classifica por probabilidade ≥ 0,5, sem opção de corte; com classes desequilibradas isso favorece a maioritária. **Resolvido** em a9ef59a: parâmetro `corte` (padrão 0,5, resultado anterior mantido, versão mantida). Validação: `.prob_*` iguais a `fitted(glm)` (1e-10) e `.pred` igual à regra P ≥ corte para 0,3/0,5/0,8.
- **`ml/svm`: `.pred` × `.prob_*`** — a classe vem da margem e as probabilidades da calibração de Platt (validação cruzada interna do LIBSVM); as duas podem discordar em linhas perto da fronteira (Chang & Lin 2011, doi:10.1145/1961189.1961199). **Resolvido (a premissa estava errada)** em ca59d3e: com `probability = TRUE`, o `svm_predict_probability` do LIBSVM já rotula pela maior probabilidade de Platt, não pela margem; o bloco agora calcula `.pred` = argmax das `.prob_*` explicitamente (empates na ordem dos níveis) e o pressuposto foi corrigido. Resultado idêntico fora de empates exatos, versão mantida. Validação: em iris binária (Sepal) a margem discorda de 2 linhas e `.pred` ainda coincide com o argmax; idem em iris com 3 classes.

- **Sem curva precisão-revocação** — com classe rara, a ROC/AUC pode parecer boa enquanto a classe de interesse é mal prevista (Saito & Rehmsmeier 2015, doi:10.1371/journal.pone.0118432). **Resolvido** em a0dd5cc (`ml/pr_curve`, main) e movido em 3bb4c5c para `models/pr_curve`, com os três modos da `models/roc` (modelo com validação, modelo + dados, tabela). AP = Σ ΔR·P e área com a interpolação de Davis & Goadrich (2006, doi:10.1145/1143844.1143874) integrada em forma fechada (Keilwagen, Grosse & Grau 2014), contas sem mudança. Validação: exemplo à mão (AP = (1 + 2/3 + 3/5)/3, área = (3 − ln 1,5 − 2 ln 1,25)/3, 1e-12); `yardstick::average_precision` (1e-10) e `PRROC::pr.curve` `auc.integral` (1e-8) em 200 linhas com empates; no modo modelo, a AP da resubstituição é igual à da tabela do `models/predict` (1e-12).

## trama.multi

Sem pendências abertas (25/09/2026).

### Resolvido

- **Jackknife** — só deixa-uma-linha-fora; dados agrupados pediriam jackknife por grupo. O intervalo usa t(n − 1) sobre a estimativa corrigida pelo viés, uma escolha usual mas não a única (Efron & Tibshirani 1993, cap. 11). **Resolvido** em 51d6d28: os quatro blocos ganham `grupo` (apagar-um-grupo; Shao & Tu 1995, doi:10.1007/978-1-4612-0795-5; Kott 2001, JOS 17(4):521-526, sem DOI), EP com G réplicas e t(G − 1). Validação: `survey` 4.5 com réplicas JK1 (`as.svrepdesign(type = "JK1")`) a 1e-10 na média e na razão (9 grupos desiguais; diferenças 6e-17 e 1e-17), exemplo de 3 grupos à mão. O intervalo t sobre a corrigida segue documentado como escolha.
- **`multi/roc` — AUC multiclasse** (pendente da entrada abaixo). **Resolvido** em 7fd83da: `multi/roc` versão 3 traz o M de Hand & Till (2001, doi:10.1023/A:1010920819831) no subtítulo com 3+ grupos. Validação: `pROC::multiclass.roc` 1.19.1 a 1e-10 (iris LDA deixa-um-fora M = 0,998133, diferença 1e-16; vinhos; 4 grupos com empates) e à mão.
- **Curva precisão-revocação na multi** (pendente da entrada de grupos desbalanceados). **Resolvido** em b68484f: bloco `multi/pr_curve` (AP; área de Davis & Goadrich 2006, doi:10.1145/1143844.1143874), mesma conta da `ml/pr_curve`. Validação: `yardstick::average_precision` a 1e-10 e `PRROC::pr.curve` a 1e-8 (pima deixa-um-fora: AP 0,721, área 0,719; vinhos com empates).
- **Intervalo perfilado na logística ML** (pendente da entrada de Firth). **Resolvido** em 495ea07: `multi/logistic_coefficients` versão 4 com `intervalo = "perfilado"` por padrão (Venables & Ripley 2002 sec. 7.2; Hosmer, Lemeshow & Sturdivant 2013 sec. 1.4) e p da razão de verossimilhanças na binária; Wald como opção; multinomial segue Wald (sem implementação de referência do perfil para o `multinom`, documentado). Validação: `confint` do glm (perfil do MASS) com grade fina a 1e-4 (2e-6 observado), limites pela definição a 1e-6, `drop1(test = "LRT")` a 1e-10. Param `nivel` → `confianca` em d7c4c25 (versão 3; fluxos salvos com `nivel` migram sozinhos pela migração de parâmetros do núcleo, cccf0f7 + b97222c).
- **`multi/roc`** — AUC sem intervalo de confiança (Hanley & McNeil 1982, doi:10.1148/radiology.143.1.7063747). Com 3+ grupos, só curvas um-contra-os-outros, sem AUC multiclasse resumida. **Resolvido** em 3740464: `multi/roc` versão 2 com IC de DeLong, DeLong & Clarke-Pearson (1988, doi:10.2307/2531595), param `confianca`, limites cortados em [0, 1] como no pROC. Validação: `pROC::ci.auc(method = "delong")` a 1e-8 (aSAH, três escores com empates, 95% e 90%; pima por deixa-um-fora: AUC 0,849, IC 0,816–0,882). Pendente: AUC multiclasse resumida (Hand & Till 2001) segue sem bloco.
- **Avaliação com grupos desbalanceados** — `multi/confusion` dá taxa por grupo e geral, mas não acurácia balanceada nem kappa, e não há curva precisão-revocação (Saito & Rehmsmeier 2015, doi:10.1371/journal.pone.0118432). **Resolvido** em 2b88e85: `multi/confusion` com `tabela = "métricas"` — acurácia, acurácia balanceada (Brodersen et al. 2010, doi:10.1109/ICPR.2010.764), kappa de Cohen (1960, doi:10.1177/001316446002000104) e precisão/revocação/F1 por grupo; padrão inalterado. Validação: contas à mão, exemplo 20/5/10/15 com κ = 0,4 (Wikipédia, não fonte primária), `irr::kappa2` e `psych::cohen.kappa` a 1e-12. Pendente: curva precisão-revocação na multi (existe o plano de `ml/pr_curve`).
- **`multi/logistic` com separação** — o bloco detecta e recusa os coeficientes, mas não há logística penalizada (Firth 1993, doi:10.1093/biomet/80.1.27) como saída. Os intervalos de `multi/logistic_coefficients` são de Wald; com amostra pequena, Hosmer, Lemeshow & Sturdivant (2013, cap. 1) preferem o de verossimilhança perfilada. **Resolvido** em 531cd34: `multi/logistic` com `metodo = "firth"` (binária; Firth 1993; Heinze & Schemper 2002, doi:10.1002/sim.1047) e `multi/logistic_coefficients` versão 2 com IC da verossimilhança penalizada perfilada e p da razão de verossimilhanças penalizadas (coluna `intervalo`). Validação: `logistf` 1.26.1 no `sex2` (coeficientes 1e-6; limites e p 1e-4; também confiança 0,9 e separação completa); no caso quase separado dos `vinhos` B × C, onde o `logistf` não converge em metade dos limites, cada limite confere pela definição (perfil = χ²₁ a 1e-6). Pendente: na ML o intervalo continua de Wald (perfilado para ML não implementado).
- **Normalidade multivariada sem teste** — a LDA/QDA, o M de Box, o teste χ² da fatorial por ML e a esfericidade de Bartlett supõem normalidade multivariada, mas o trama só confere cada variável isoladamente (`models/shapiro`, `view/qq`). Falta um teste multivariado (Mardia; Johnson & Wichern 2007, sec. 4.6). **Resolvido** em 6ebe41c: bloco `multi/mardia` (Mardia 1970, doi:10.1093/biomet/57.3.519) — assimetria b1,p com χ² (e com o fator k de amostra pequena de Mardia 1974, *Sankhyā B* 36(2):115-128, conferido só em fontes secundárias e no código do MVN/psych) e curtose b2,p com z; covariância de divisor n; por grupo opcional. Validação: `psych::mardia` reescalado ((n − 1)/n)^3 e ^2 a 1e-10 (iris setosa, iris, USArrests, mtcars); código do `MVN::mardia` 6.3 a 1e-13 nas estatísticas e p (o pacote não instala aqui: depende do gsl do sistema). Os pressupostos da discriminante, do M de Box, da fatorial e do KMO/Bartlett passam a apontá-lo.

## trama.series

- **Pendências** — raiz unitária com duas quebras (Lee & Strazicich 2003, doi:10.1162/003465303772815961; Narayan & Popp 2010): **ainda sem bloco** — nenhum pacote do CRAN implementa nenhum dos dois (busca no índice do CRAN de 2026-09-25 por "Strazicich", "Narayan", "two breaks": só `unitrootests`, que traz Narayan & Liu 2015 com GARCH, outro teste), e sem implementação de referência ou valores publicados conferidos na fonte não há oráculo; não implementado.

### Resolvido

- **`series/zivot_andrews`** — o número de defasagens é fixo (0 = trunc((n − 1)^(1/3))), sem escolha pelos dados. Zivot & Andrews (1992, doi:10.1080/07350015.1992.10509904) escolhem k pelo procedimento do geral para o específico (significância da última defasagem), e o `series/adf` do próprio trama escolhe por AIC. A tabela de críticos supõe k bem escolhido. **Resolvido** em 645cf20: padrão `selecao = "t_sig"`, do geral para o específico (Perron 1989, doi:10.2307/1913712; Zivot & Andrews 1992): do teto (`defasagens`; 0 = Schwert 1989, doi:10.1080/07350015.1989.10509723) para baixo, fica o primeiro k com |t| da última diferença defasada ≥ 1,645, lido na regressão do corte escolhido com aquele k; `fixa` mantém a versão 1; `series/zivot_andrews` versão 2. Validação: regressão do corte = `urca::ur.za` (t de y_{t-1}, 1e-10, três modelos); propriedades da seleção conferidas (última significativa, todas as de cima descartadas); com k = 8 o bloco reproduz Zivot & Andrews (1992), modelo A, PNB real (−5,58, 1929) e nominal (−5,82, 1929) de Nelson-Plosser (`urca::nporg`, log, 1909-1970). Os valores publicados vêm de memória do artigo, não de conferência no PDF (acesso bloqueado). **Revisão (f1acce8, versão 3):** a regra implementada em 645cf20 escolhia o corte primeiro e o k só nele; agora k é escolhido do geral para o específico EM CADA corte e o teste é o mínimo dos t (regra do artigo, seção 4). Validação: força bruta com a regressão do `ur.za` (1e-10) em quatro séries, uma em que as regras divergem (−4,335 → −4,738); `fixa` com `defasagens = 0` passa a ser zero defasagens. Nelson-Plosser, modelo A, k = 8: −5,576386 (real) e −5,823666 (nominal), 1929, recalculados com `urca::ur.za` sobre `urca::nporg` (a tabela do artigo não foi conferida no PDF). Medido sob passeio aleatório (nível, 300 réplicas): a regra do artigo rejeita a 5% em 31% (n = 30), 27% (50) e 13% (100), contra 10%, 6% e 5% com k fixo — documentado na ajuda e na `nota` (aviso abaixo de 100). **Revisão (e92cf0d, versão 4):** padrão passa a `fixa` com a regra l4 de Schwert (1989, eq. 13a do NBER TWP 73, conferida no texto), trunc(4·(n/100)^(1/4)); `t_sig` fica opção (teto l12) com o aviso abaixo de 100. Medido sob passeio aleatório, nível, 1000 réplicas, rejeição a 5% (n = 30/50/100): t_sig 29,3/19,8/14,6%; fixa l12 12,0/5,3/5,6%; fixa l4 8,0/6,9/6,2%. Oráculo: `urca::ur.za(lag = l4)` a 1e-12.
- **`series/phillips_perron`** — `stats::PP.test` roda sempre com constante e tendência; não há a versão só com constante (Phillips & Perron 1988, doi:10.1093/biomet/75.2.335), que tem mais poder em série sem tendência. **Resolvido** em 7b4449f: `deterministico` = `tendência` (padrão, `stats::PP.test`, idêntico à versão 1) ou `constante` (Z(t) pela forma geral, Hamilton 1994, eq. 17.6.8, convenções do `PP.test`; p pela tabela τ_μ de Fuller 1976); `series/phillips_perron` versão 2 (a `nota` do caso com tendência mudou). Validação: `stats::PP.test` e `tseries::pp.test(type = "Z(t_alpha)")` iguais a 1e-12 (tendência); `aTSA::pp.test` tipo 2 igual a 1e-10 no Z(t) com constante (n = 60, 150, 400), `urca::ur.pp` a < 0,5% (normalização de MacKinnon); p interpolado na mesma tabela do aTSA a ≤ 0,022 (o aTSA tem a coluna da mediana), mesma decisão; quantis da tabela devolvem p exato. **Revisão (8542966, versão 3):** o p do caso só constante interpolava a tabela τ_μ de Fuller (colunas de 10%/2,5% e cauda superior não conferidas) e prendia n < 25 na linha n = 25; passa a ser a superfície de resposta de MacKinnon (1996, doi:10.1002/(SICI)1099-1255(199611)11:6<601::AID-JAE417>3.0.CO;2-T), `urca::punitroot`: devolve 1/5/10% nos críticos assintóticos de MacKinnon (2010) e fica a < 0,002 das colunas de 1% e 5% de Fuller. Série com menos de 25 observações ganha aviso na `nota` (medido, 4000 réplicas: n = 12 rejeita 7,0% constante / 10,2% tendência).
- **`series/fisher`** — o periodograma é calculado com `detrend = TRUE` (tira uma reta antes), e o p-valor usa só o primeiro termo da série exata de Fisher (1929, doi:10.1098/rspa.1929.0151), conservador; a ordenada de Nyquist (série de tamanho par) entra na soma de g. A ajuda documenta os três; a formulação original supõe ruído branco gaussiano sem remoção de tendência. **Resolvido** em 498d4a6: a divergência era real em dois pontos e foi corrigida — a ordenada de Nyquist sai (χ² com 1 gl contra 2 das outras; m = ⌊(N − 1)/2⌋) e o p-valor é a série exata inteira; o crítico publicado é o quantil exato a 5%. A remoção da reta vira parâmetro `remover` (`reta`, padrão da dissertação; `media`, formulação original); `series/fisher` versão 2. Validação: `GeneCycle::fisher.g.test` (Wichert, Fokianos & Strimmer 2004, doi:10.1093/bioinformatics/btg364) igual a 1e-10 em seis séries do `datasets` com `media`, e nos resíduos da reta com `reta`; crítico com um termo igual à fórmula fechada (m = 5: 0,6838). Decisões das cinco séries da página inalteradas (p do `lh`: 0,052 → 0,062).
- **Erro autocorrelacionado nas regressões** — `series/regression` e os três F (`series/f_global`, `series/f_sazonal`, `series/f_tendencia`) usam mínimos quadrados ordinários, e o erro de série temporal costuma ser autocorrelacionado: p-valores otimistas. Regressão com erro ARMA / mínimos quadrados generalizados ainda sem bloco no trama (Morettin & Toloi 2006, *Análise de séries temporais*, 2. ed.). A entrada `regressor` do `series/regression` está declarada e recusa uso. **Resolvido** em 9996d62: `series/regression` ganha `erro = "arma"` (`nlme::gls` + `corARMA(p = ar, q = ma)`, ML; Pinheiro & Bates 2000, doi:10.1007/b98882), padrão MQO sem mudança de resultado (versões mantidas); com GLS os três F viram F de Wald com a covariância do GLS. Validação: `stats::arima(xreg = , method = "ML")` (coeficientes 1e-3, log-verossimilhança 1e-4), MQO de Prais-Winsten com o phi estimado (1e-8), F de Wald refeito à mão (1e-8). Tamanho medido sem tendência, AR(1), n = 120: MQO 37% / GLS 8% (phi 0,6); 69% / 17% (phi 0,9) — perto da raiz unitária o GLS ainda passa do nominal (documentado). Não validado contra exemplo resolvido de livro (Cochrane-Orcutt): sem texto acessível para conferir. **Revisão (5be3a70):** GLS singular (série reproduzida sem resíduo) recusado com `tr_series_error_singular_fit` em vez de "não convergiu"; AR do erro com raiz inversa ≥ 0,9 emite `tr_series_warn_near_unit_root` e vai para a `nota` dos três F.
- **Testes de tendência com dependência serial** — `series/mann_kendall`, `series/cox_stuart` e `series/pettitt` supõem observações independentes; não há Mann-Kendall modificado nem pré-branqueamento (Hamed & Rao 1998, doi:10.1016/S0022-1694(97)00125-X). Lacuna: ainda sem bloco no trama. **Resolvido em parte** em 87dbce3: `series/mann_kendall` ganha `correcao` = `hamed_rao` (Hamed & Rao 1998) ou `pre_branqueamento` (livre de tendência; Yue et al. 2002, doi:10.1002/hyp.1095). Validação: `modifiedmk::mmkh` e `::tfpwmk` (1.6) a 1e-8 (diferença medida < 1e-14) em seis séries, com empates. Medido sem tendência, AR(1), 2000 réplicas, rejeição a 5% (nenhuma / Hamed-Rao / pré-branqueamento): phi 0,6, n = 60: 30,7% / 21,1% / 39,4%; ruído branco: 5,1% / 8,5% / 4,7% — nenhuma correção devolve o nível e o pré-branqueamento piora (Hamed 2009, doi:10.1016/j.jhydrol.2009.01.040); documentado. **Revisão:** `correcao = "bootstrap_blocos"` (blocos móveis de round(√n), 1999 reamostras, semente do nó; Kundzewicz & Robson 2004, doi:10.1623/hysj.49.1.7.53993) no `series/mann_kendall` (8dd2edb) e no `series/cox_stuart` e `series/pettitt` (cfb1f3e). Oráculo da mecânica: bootstrap à mão com a mesma semente, igual exatamente. Nível medido (AR(1), sem tendência, 1000 réplicas, 5%; n = 60/120): MK phi 0,3 7,2/5,5%, phi 0,6 9,0/7,7% (sem correção 31%); Cox-Stuart 4,4/3,8% e 5,5/6,6%; Pettitt 3,5/4,6% e 8,7/7,8% — nominal com autocorrelação moderada, acima dele com phi 0,6 (documentado; opção, não padrão). Pré-branqueamento conferido na fonte (61fdd23): Yue et al. (2002, p. 1822-1823) removem o AR(1) sempre, sem teste de significância (o teste B.1 só seleciona estações); o r1 da eq. 14a é n/(n − 1) vezes o do `acf` usado pelo `modifiedmk` — documentado.
- **Lacunas de modelagem** — sem intervalos de previsão por bootstrap, sem modelo de intervenção (ARIMA com regressor de degrau/pulso) e sem teste de raiz unitária com duas quebras; os pressupostos de `series/arima`, `series/ets`, `series/forecast` e `series/zivot_andrews` apontam para essas lacunas. **Resolvido em parte:** intervalos por bootstrap em `series/forecast` (57fc01e; `intervalo = "bootstrap"`, oráculo `forecast::forecast(bootstrap = TRUE, npaths = 5000)` com a mesma semente, 1e-12) e bloco `series/intervencao` (e5235ac; Box & Tiao 1975, doi:10.1080/01621459.1975.10480264, forma de ordem zero com degrau/pulso/rampa; oráculo `forecast::Arima(xreg = )` a 1e-8; Seatbelts com degrau = coluna `law`: ω = −0,2397, EP 0,0433 — valores de Harvey & Durbin 1986 não conferidos). Resposta gradual **resolvida** em 201df6a (o bloco foi refeito na 0.7.0: ver "Intervenções declaradas antes do modelo"): `resposta = "gradual"`, ω/(1 − δB) para degrau e pulso, δ perfilado, EP pela hessiana completa; oráculo `TSA::arimax(transfer = list(c(1, 0)))` 1.3.1 (Cryer & Chan 2008, cap. 11, doi:10.1007/978-0-387-75959-3) no airmiles 2001-09, a 1e-3 (pulso: ω = −0,3459, δ = 0,6947). **Pendente:** raiz unitária com duas quebras (ver Pendências).

## Integração 9.1b (coesão das coleções × main, 25/09/2026)

Os itens resolvidos acima continuam valendo, com os commits citados; na
branch `coesao-colecoes` os blocos da `ml` e da `multi` que avaliam ou leem
coeficientes foram fundidos nos da `trama.models`, e as correções foram
portadas para lá com os mesmos oráculos (os testes da main mudaram de casa,
não de conta):

- IC de DeLong e corte de Youden (`ml/roc`, 144c859/688b87e/168b489; `multi/roc`,
  3740464/16d930b) e AUC multiclasse de Hand & Till (`multi/roc`, 7fd83da) →
  `models/roc` versão 3 (`confianca`). Oráculos: `pROC::ci.auc(method =
  "delong")` a 1e-8, `pROC::coords(best.method = "youden")`,
  `pROC::multiclass.roc` a 1e-10 (collections/trama.models/tests/testthat/test-roc-ic.R
  e, com os classificadores da multi, test-roc-delong.R / test-roc-hand-till.R).
- Curva PR da multi (b68484f) → `models/pr_curve` versão 2 (uma curva por
  classe com 3+ classes); oráculos `yardstick`/`PRROC`.
- Métricas da confusão (2b88e85, 03bfaf9) → `models/confusion` versão 2,
  `tabela = "métricas"`; kappa contra `irr`/`psych`.
- Precisão de classe nunca prevista NA e fora das médias (2b3d355) →
  `models/evaluate` versão 3.
- IC perfilado (495ea07) e Firth (531cd34) → `multi/logistic` (`metodo =
  "firth"`) e o método `tr_models_coefs()` da logística, lido pelo
  `models/coefficients` versão 2 (`intervalo`); oráculos `confint` do perfil,
  `drop1(test = "LRT")`, `logistf`.
- Importância por permutação e impureza corrigida (fb874ad, 8845ef2) →
  `ml/forest` (`importancia`) lida pelo `models/importance`; oráculo `ranger`.
- Isolamento do teste pela proveniência (da77947, 63ec621) → a marca
  `tr_ml_origem` viaja pelo `models/predict`; o modelo da ml recusa prever o
  teste que viu (`tr_models_predict_raw`), e `models/evaluate`,
  `models/confusion`, `models/roc` e `models/pr_curve` ganharam
  `permitir_treino`.
- Jackknife por grupo (51d6d28) fica na `multi`, com `confianca`.

## Influência e colinearidade em modelos lineares (27/09/2026)

`models/coefficients` versão 3 acrescenta VIF/GVIF somente a `lm` com dois ou
mais termos preditores. Os valores seguem `car::vif`; para termos com múltiplos
graus de liberdade também se apresenta GVIF^(1/(2·gl)), conforme Fox & Monette
(1992, doi:10.1080/01621459.1992.10475190). `models/influence` aplica
`stats::influence.measures` a `lm`; alavanca, resíduo estudentizado, Cook,
DFFITS, DFBETAS e a marca influente são confrontados com essa função a 1e-10.
A linha 4/n no gráfico de Cook é triagem visual, não regra automática de
exclusão. Testes: `collections/trama.models/tests/testthat/test-resumir.R`.

## Permutação e bootstrap sobre `lm` (27/09/2026)

`models/permutation` permuta a resposta (dentro de **grupo**, quando dado) com
a matriz do modelo fixa e recalcula o F sequencial (tipo I) do termo pelos
efeitos Q'y da QR do ajuste, o mesmo cálculo do `anova.lm`; o bloco confere
que reproduz o F observado antes de permutar. p = (b + 1)/(B + 1) (Phipson e
Smyth, 2010), empates por tolerância 1e-8. Oráculos: permutação escrita à mão
com `anova(lm())` na mesma semente (1e-12) e `coin::oneway_test` Monte Carlo no
PlantGrowth (dentro de 4·√2 erros-padrão binomiais). Distinto do
`experiments/randomization_test`, que re-sorteia pela receita do plano.

`models/bootstrap` reamostra casos (`boot::boot`, estratificado pelo fator nas
médias e diferenças) e reajusta por `lm.fit`; toda quantidade é `L β`, com L
fixa na grade de referência original (médias = `emmeans` do ajuste original).
IC percentil e BCa do `boot::boot.ci`; conferidos contra `boot`/`boot.ci`
aplicados à estatística escrita à mão (`coef(lm())`, `tapply` das médias) na
mesma semente, a 1e-10. Decisão: valores a menos de 1e-9 (relativo) de t0
viram t0 antes do BCa — com resposta discreta, reamostras com média exatamente
igual à observada são comuns, e o ruído de ponto flutuante decidia se entravam
em `t < t0` (medido: BCa diferia na 4ª casa entre duas estatísticas
matematicamente iguais). Reamostras de posto incompleto são descartadas e
contadas; nesse caso a influência do BCa vem do jackknife, e o BCa fica NA
quando o jackknife não estima. Testes: `test-permutation.R`, `test-bootstrap.R`.

## Seleção multimodelo e ponte tidy (30/09/2026)

`models/select` ranqueia modelos por AICc = AIC + 2k(k+1)/(n−k−1) (Hurvich e
Tsai, 1989, doi:10.1093/biomet/76.2.297) e pesos de Akaike exp(−Δ/2)
normalizados (Burnham e Anderson, 2002, cap. 2, doi:10.1007/b97636). k conta os
parâmetros de variância (`attr(logLik, "df")`), como `stats::AIC`. Misto e GLS
são reajustados por ML: REML não compara efeitos fixos diferentes. Com
n−k−1 ≤ 0 o AICc não existe e o modelo sai sem peso. Recusa respostas
diferentes (`log(y)` × `y`) e números de linhas diferentes. Oráculo: AIC/BIC
contra `stats::AIC/BIC`; AICc e pesos contra a fórmula fechada escrita à mão, a
1e-10 e, com `MuMIn` 1.48.19, contra `MuMIn::AICc` e `MuMIn::Weights` (lm; o
teste pula onde o pacote não existe). Referências conferidas na fonte em 30/09/2026.
Hurvich e Tsai (1989), Biometrika 76(2): 297–307: título, autores, volume e
páginas confirmados no Crossref. Burnham e Anderson, *Model Selection and
Multimodel Inference: A Practical Information-Theoretic Approach*, 2ª ed.,
Springer, 2002, ISBN 0-387-95364-7: título, editora, DOI, edição e ano
confirmados (catálogo da Springer e de bibliotecas); o Crossref dá 2004 para o
DOI porque é a data da publicação online, e a citação usual é 2002. Faixas do
delta: a de Δ < 2 (suporte substancial), a de 4 a 7 (bem menos) e a de
Δ > 10 (essencialmente nenhum) aparecem, atribuídas a esse livro, em textos
que o citam; **a página exata não foi conferida** (nenhuma fonte acessível a
cita), então o help não cita página e diz que a transição entre as faixas é
gradual. Fica aberto abrir o livro para fixar seção e página. Também: mistos, GLS e nls não têm oráculo `MuMIn` próprio (o AIC
deles vem de `logLik`).
`tidy()`, `glance()` e `augment()` só renomeiam colunas dos leitores do
contrato; oráculo `broom` (lm, glm) a 1e-8. Testes:
`collections/trama.models/tests/testthat/test-select.R`, `test-tidy.R`.

## Amplitude–média em séries (30/09/2026)

`series/range_mean` agrupa observações consecutivas em blocos completos (padrão
igual à frequência/ciclo declarado), calcula média e amplitude (máximo menos
mínimo) e ajusta OLS amplitude ~ média. O p-valor bilateral testa inclinação
zero com t e n_blocos − 2 graus de liberdade. Inclinação positiva significativa
orienta a avaliar log/Box-Cox; ausência de significância não prova
homocedasticidade. O trecho incompleto final é descartado. O gráfico mostra os
pontos e a reta.

Fonte metodológica consultada: Zucoloto, Giarola e Rocha (2018), “Modelagem da
exportação brasileira de automóveis”, *Revista Eletrônica Matemática e
Estatística em Foco*, 6(1), 12–23,
[PDF](https://seer.ufu.br/index.php/matematicaeestatisticaemfoco/article/download/39080/22266/179091),
p. 15. A fonte especifica grupos de 12, amplitude versus média, teste t da
inclinação e H0: inclinação zero; relata p = 0,168045 para a série Bovespa.
Também consultada, p. 15: a fonte descreve transformação log quando a amplitude
cresce proporcionalmente à média. `tr_ref` e o texto de pressuposto refletem
essas afirmações verificadas.

Oráculo numérico: o `rmplot` do gretl 2023c (pacote de referência, outro
código) no AirPassengers dá 12 sub-amostras de 12, inclinação 0,560685 e
p = 4,78409e-10; o bloco bate nos 6 algarismos impressos, e na tabela de
amplitude e média por bloco. Divergência declarada: com bloco final incompleto
o gretl o usa (AirPassengers até 1958:06 dá 0,43901) e o bloco o descarta, porque
a amplitude de um bloco menor é menor por construção. Não foi possível conferir
no livro de Morettin & Toloi. Testes: `test-testar.R`, `test-catalogo.R`.

## Séries: regressão por fórmula, componentes com inferência (30/09/2026)

`series/regression` v2 ajusta uma fórmula sobre `valor`, `t`, `periodo`, `ano`
e `regressor` e sai como `models/fit` (`trama.models::tr_models_as_fit`, 0.6.0).
Migração da v1: grau 1 → `t`, grau g ≥ 2 → `poly(t, g, raw = TRUE)` (um termo,
para o F do bloco ser uma linha do quadro), sazonalidade → `periodo`,
`alfa` → `confianca = 1 − alfa`. Oráculos:

- coeficientes da fórmula padrão e da migrada de grau 2 contra `lm` direto com
  o fator do mês em `contr.sum` (1e-8 a 1e-10);
- F de cada bloco (`models/anova_table`, tipo III) contra o F parcial de
  `anova(reduzido, completo)` (1e-8); com erro AR(1), contra o F de Wald
  b'V⁻¹b/q refeito à mão (1e-6); o tamanho com erro AR(1) (37% → 8%) segue;
- previsão contra `forecast::tslm(y ~ trend + season)` (média e limites de 80 e
  95%, 1e-8); com erro ARMA, contra `forecast::Arima(xreg = )` do mesmo desenho
  (1e-6), cujos coeficientes batem com os do GLS a 1e-3 (mesma verossimilhança
  ML); com regressor, contra `predict.lm` com o regressor futuro.

Os três F (`series/f_global`, `f_seasonal`, `f_trend`) saíram: migram para
`models/fit_stats` e `models/anova_table` com `tipo_sq = "III"`. `series/detrend`
v2 ganha a saída `ajuste` (`valor ~ t` ou `poly(t, g, raw = TRUE)`, conferido
contra `lm`; loess e diferença dão a reta como referência, dito na nota).
`series/deseasonalize` (novo) ajusta `valor ~ t + periodo` por padrão
(controlando a tendência) ou `valor ~ periodo`, conferido contra `lm` e, sem
tendência, contra as médias de período centradas (1e-10). Com ciclos
incompletos, os dois avisam e recomendam a regressão conjunta. Testes:
`test-decompor.R`, `test-operar.R`, `test-testar.R`, `test-motor.R`.

Em 01/10/2026 (`trama.series` 0.6.0), `series/detrend` v3 e
`series/deseasonalize` v2 trocam a saída `ajuste` pelo componente removido
(`tendencia`/`sazonal`, série), e os coeficientes vão para o card da série sem o
componente. As contas não mudam e seguem conferidas contra `lm` (1e-10).
Loess e diferença deixam de mostrar a reta de referência: o card delas é o
gráfico, sem coeficiente que não corresponde ao que foi removido.

## ARIMA em `models/compare` e `models/select` (01/10/2026)

`trama.series` 0.6.0 / `trama.models` 0.6.1. O ARIMA chega aos blocos de modelo
por adaptador (`series/model` → `models/fit`); nenhum bloco novo. Razão de
verossimilhança `2(ℓ₁ − ℓ₀)` ~ χ² com gl = diferença de parâmetros (Wilks,
1938), aceita só entre ajustes da MESMA série com os mesmos d, D e período e
coeficientes do menor contidos no maior; com d diferente as verossimilhanças
são de séries diferentes. Seleção com `k` = coeficientes livres + variância e
`n` = observações efetivas (`nobs`), o que reproduz o AICc e o BIC do
`forecast` (Hyndman e Athanasopoulos, 2021, sec. 9.8). Coeficientes com
erro-padrão da matriz de informação e teste z. Validação (1e-8, série `lh`):
estatística, gl e p contra `lmtest::lrtest`; AICc e BIC contra `forecast::Arima`;
estimativa, erro-padrão e p contra `lmtest::coeftest`. Teste:
`collections/trama.series/tests/testthat/test-arima-fit.R`.

## Tamanhos de efeito nos testes não paramétricos e no qui-quadrado (02/10/2026)

`trama.models` 0.6.2. `models/kruskal` (v2) passa a dar no efeito o épsilon²
dos postos, H / (n − 1) (Tomczak e Tomczak, 2014), sem IC. `models/wilcoxon`
(v2) mantém o deslocamento de Hodges-Lehmann como efeito e acrescenta a
bisserial de postos r = 2W/(n₁n₂) − 1 (Cureton, 1956; Kerby, 2014) na nota e em
`extra_r_bisserial_postos`. `models/chisq` (v3) dá o V de Cramér (1946),
√(X² / (n (min(l, c) − 1))), sem correção de viés, com nota de que superestima
em amostra pequena (Bergsma, 2013), e os resíduos padronizados ajustados
(`chisq.test()$stdres`; Agresti, 2002, sec. 3.3.1) com |r| > 2 na nota e o
maior em `extra_maior_residuo_padronizado`. Validação (1e-9): pacote de
referência `effectsize` 1.0.3 (`rank_epsilon_squared`, `rank_biserial`,
`cramers_v(adjust = FALSE)`) em `InsectSprays`, `ToothGrowth` e `mtcars`
(cyl × gear); valores anotados no teste e conferidos ao vivo quando o pacote
está instalado. A fórmula de Tomczak e Tomczak foi conferida por meio do
`effectsize`, que a cita, e não no artigo. Cramér (1946), *Mathematical
Methods of Statistics* (Princeton University Press), conferido no catálogo da
editora (press.princeton.edu) e da De Gruyter (reimpressão,
doi:10.1515/9781400883868); a página da fórmula não foi conferida. Shapiro-Wilk com n > 5000 passa a
recusar com `tr_models_error_too_many_rows` (antes, a classe de "de menos").
Teste: `collections/trama.models/tests/testthat/test-testes.R`.

## Aviso do tipo I com desenho desbalanceado (02/10/2026)

`models/anova_table` num `lm` com `tipo_sq = "I"` compara a SQ sequencial de
cada termo com a do tipo II (`car::Anova`); se alguma difere (relativo 1e-8),
a nota diz que aquela SQ depende da ordem dos termos e aponta o tipo II
(Langsrud, 2003). O padrão continua "I" (decisão de quem analisa) e os números
não mudam, por isso a versão do nó fica. Teste em `ToothGrowth` (balanceado,
sem aviso) e sem as 5 primeiras linhas (aviso só em `supp`, cuja SQ muda mais
de 1 unidade ao trocar a ordem): `collections/trama.models/tests/testthat/test-resumir.R`.

## Binomial negativa no `models/glm` (02/10/2026)

`trama.models` 0.6.2, `models/glm` versão 3. Família `binomial negativa` por
`MASS::glm.nb`: NB2, V(μ) = μ + μ²/θ, ligação log, θ por máxima verossimilhança
alternada com o IRLS (Lawless, 1987, doi:10.2307/3314912; Venables e Ripley,
2002, sec. 7.4, doi:10.1007/978-0-387-21706-2 — seção conferida no script
`ch07.R` do MASS). O `models/compare` usa o `anova.negbin` (razão de
verossimilhança com θ reestimado em cada modelo); o quadro tipo I fixa o θ do
modelo completo (aviso do próprio MASS), o tipo II/III é o do `car`; a validação
cruzada reestima θ sem a linha. Validação: coeficientes, θ, EP(θ), EP dos
coeficientes e log-verossimilhança contra `MASS::glm.nb` no exemplo do livro
(`quine`, `Days ~ .^4`), 1e-8; θ e coeficientes contra máxima verossimilhança
direta por `optim` sobre `dnbinom` (`Days ~ Eth + Sex + Age + Lrn`), 1e-4
(tolerância do otimizador), log-verossimilhança 1e-8; razão de verossimilhança
contra `anova.negbin`, 1e-8; tipo II contra `car::Anova`, 1e-8. Os valores
impressos do livro não foram conferidos página a página (a referência numérica é o
pacote do mesmo autor). Teste:
`collections/trama.models/tests/testthat/test-binomial-negativa.R`.

## Intervenções declaradas antes do modelo; detecção de Chen & Liu (05/10/2026)

`trama.series` 0.7.0. `series/intervencao` (v2) deixou de ajustar: só anota a
série (carimbo com valores e calendário), e o `series/arima` estima as
intervenções junto com o ARMA. Tipos: pulso (AO), degrau (LS), rampa e
inovacional (IO) (Fox, 1972, doi:10.1111/j.2517-6161.1972.tb00912.x; Chen e
Liu, 1993, doi:10.1080/01621459.1993.10594321); dinâmica gradual ω/(1 − δB)
de Box e Tiao (1975). Mudou o resultado do bloco (tabela → série), por isso
v2 com migração dos params (`resposta` → `dinamica`, ordens saem).

- **Imediatas**: `forecast::Arima(xreg = )`, uma coluna por intervenção.
  Oráculo: a mesma chamada a 1e-8 (Seatbelts, degrau = coluna `law`, ω =
  −0,2397; Nile com rampa e pulso juntos, coeficientes e EP).
- **Gradual**: δ perfilado, EP pela hessiana da verossimilhança completa,
  agora com δ contado no AIC/AICc/BIC e no `k` do `models/select` (antes o
  bloco não reportava AIC). Oráculo inalterado: `TSA::arimax` 1.3.1 no
  airmiles a 1e-3. Previsão: o regressor continua o filtro (pulso decai como
  δ^h, conferido a 1e-12).
- **Inovacional**: regressor = pesos ψ do modelo inteiro (AR com (1 − B)^d
  (1 − B^s)^D, MA) a partir da data, recalculados até o ponto fixo (|Δcoef| <
  1e-8; quando o `optim` oscila no 3º–4º algarismo, como no SARIMA(1,1,1)(1,1,1)
  do log(AirPassengers), aceita-se variação relativa < 1e-3, e o reajuste com o
  regressor final confere a 1e-3). Oráculo: `tsoutliers::outliers.effects(pars = coefs2poly(ajuste))`
  0.6-10 a 1e-8 (Nile, ARIMA(1,1,1) e AR(2); Seatbelts sazonal com d = D = 1)
  e reajuste `Arima(xreg = )` com esse regressor a 1e-6. O `tso` usa os
  parâmetros da etapa anterior; aqui são os do próprio ajuste. O EP de ω trata
  ψ como conhecido (limitação declarada na ajuda).
- **Automático com intervenções**: `auto.arima(xreg = )` com os regressores na
  forma imediata; com IO ou gradual, a ordem escolhida é reajustada inteira (a
  ordem é escolhida num modelo aproximado — limitação declarada).
- **Recusas**: previsão `bootstrap` com intervenções (o `simulate.Arima` do
  forecast 9.0.2 não recebe o `xreg` futuro nem a deriva); ETS, Holt-Winters e
  `series/regression` recusam série com intervenções em vez de ignorá-las.
- **`series/detect_interventions`**: `tsoutliers::tso()` (Chen e Liu, 1993),
  tipos AO/LS/TC/IO, valor crítico do pacote (3 a 4 pelo n) ou informado,
  modelo ligado (`tsmethod = "arima"` com a ordem dele) ou automático.
  Oráculo: igualdade com a mesma chamada; no Nilo, degrau em 1899 (ω =
  −242,2, t = −9,05), a data da barragem de Assuã (Cobb, 1978,
  doi:10.1093/biomet/65.2.243). O pulso de 1913 que o `tso` também acha não foi
  conferido contra a literatura.

Teste: `collections/trama.series/tests/testthat/test-intervencao.R`.

## Distribuição no `data/summary` (06/10/2026)

O `data/summary` (versão 2) ganhou `distribuicao`. Numa coluna numérica: 10
classes de mesma largura entre mínimo e máximo, fechadas à esquerda, com o
máximo na última. Oráculo: `graphics::hist(breaks = seq(min, max, length.out
= 11), right = FALSE, include.lowest = TRUE)$counts`, com igualdade exata em
normal, exponencial, inteiros com empate na borda e escala 1e6. A altura do
bloco é proporcional à classe mais cheia; classe ocupada sobe pelo menos um
degrau, para não se confundir com vazia. Níveis: fração entre os não-faltantes,
já que `faltantes` é coluna própria. O perfil que ficava no card de toda
tabela saiu.

## Várias intervenções num bloco (06/10/2026)

`series/intervencao` (versão 3) aceita várias datas e uma tabela de datas. O
modelo não mudou: cada data vira o mesmo termo que viraria num bloco
encadeado. Oráculo: equivalência com o encadeamento (carimbo idêntico e
coeficientes do `series/arima` a 1e-10), cujo ajuste já confere com
`forecast::Arima(xreg = )` a 1e-8. Na tabela da detecção, a temporária (TC,
δ = 0,7 fixo no `tsoutliers`) entra como pulso gradual com δ estimado — a
mesma forma ω/(1 − δB) —, e o limite de uma gradual por modelo continua.
