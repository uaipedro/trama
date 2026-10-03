# Referências de Estatística para o Trama

## Finalidade e escopo

Este documento liga a matriz de Estatística da UFLA às áreas estatísticas presentes no Trama. Serve como índice de fontes para futuras revisões dos métodos implementados e para documentar a fundamentação teórica nos catálogos das coleções.

A matriz consultada é a **G058 — Estatística, 2026/01**, campus Lavras. As referências assinaladas como “básicas” foram selecionadas das bibliografias básicas dos planos de ensino oficiais. A seleção abaixo prioriza obras que fundamentam diretamente cada área; não é a transcrição integral das bibliografias básicas e complementares. Para preservar rastreabilidade, cada conjunto aponta aos PDFs oficiais da disciplina no SIG/UFLA. Os metadados bibliográficos foram mantidos como publicados nesses documentos e devem ser conferidos antes de reutilização formal em artigo ou citação acadêmica.

Este mapa **não é uma validação metodológica** do código. Ele organiza a base para comparar, em uma etapa posterior, as definições, pressupostos, estimadores, diagnósticos e interpretações documentados no Trama com fontes teóricas e com o comportamento dos pacotes R chamados pelo código.

Para propostas de blocos e referências iniciais por operação, consulte [Propostas de blocos estatísticos para o Trama](propostas-blocos-estatistica-trama.md). Para a declaração de base curricular e o registro dos docentes listados no PPC, consulte [Atribuição curricular e créditos — UFLA](atribuicao-curricular-ufla.md).

Para referências complementares encontradas em ementas da USP e da ESALQ/USP, consulte [Referências complementares USP/ESALQ](referencias-complementares-usp-esalq.md). Elas ampliam o levantamento UFLA; não substituem a bibliografia inicial da matriz do curso.

## Áreas da matriz e referências principais

### 1. Dados, estatística descritiva, visualização e programação

**Matriz:** Organização e Apresentação de Dados (GES139); Fundamentos de Programação (GES135); Estrutura de Dados (GES134); Ciência de Dados e Big Data (GES130); Consultoria em Estatística I e II (GES141, GES127).

**Referências básicas selecionadas:**

- Bussab, W. O., & Morettin, P. A. *Estatística Básica* (8ª ed., 2013; também indicada em edições posteriores em outros planos). Base de estatística descritiva, associação e inferência introdutória.
- Bruce, P., & Bruce, A. *Estatística prática para cientistas de dados: 50 conceitos essenciais* (2019). Ponte entre conceitos estatísticos, exploração e prática de ciência de dados.
- Wickham, H., & Grolemund, G. *R para Data Science: importe, arrume, transforme, visualize e modele dados* (2019). Referência de fluxo de dados e análise em R.
- Cormen, T. H., Leiserson, C. E., Rivest, R. L., & Stein, C. *Algoritmos: teoria e prática* (3ª ed., 2012; 4ª ed. listada em outro plano). Fundamentos de algoritmos e estruturas.
- McKinney, W. *Python for Data Analysis* (2012). Manipulação e análise computacional de dados.

**Relação com o Trama:** `trama.data` cobre importação, inspeção, limpeza, transformação e agregação; `trama.view` cobre visualização. São camadas de preparação e comunicação, e não substituem a justificação estatística de um estimador ou teste.

**Fontes curriculares:** [GES139](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10253), [GES134](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10195), [GES135](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10198), [GES130](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9359), [GES141](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10255), [GES127](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9356).

### 2. Fundamentos matemáticos e probabilidade

**Matriz:** Matemática para Estatística I–III (GES110, GES137, GES116); Probabilidade I–II (GES113, GES117); disciplinas eletivas de cálculo, álgebra, análise e equações diferenciais.

**Referências básicas selecionadas:**

- Ferreira, D. F. *Fundamentos de probabilidade* (UFLA, 2020). Referência central do curso de Probabilidade I–II.
- James, B. R. *Probabilidade: um curso de nível intermediário* (2010). Desenvolvimento matemático da teoria de probabilidade.
- Magalhães, M. N. *Probabilidade e variáveis aleatórias* (IME-USP, 2004). Variáveis aleatórias e distribuições.
- Stewart, J. *Cálculo*, volume 1 (2014), e Flemming, D. M., & Gonçalves, M. B. *Cálculo A* (6ª ed., 2007). Base de cálculo diferencial e integral.

**Relação com o Trama:** são pré-requisitos teóricos para inferência, modelos, amostragem, séries temporais e métodos multivariados. O núcleo de execução não precisa implementar cálculo simbólico para que as coleções estatísticas dependam desses fundamentos.

**Fontes curriculares:** [GES110](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9339), [GES113](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9342), [GES117](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9346), [GES137](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10200), [GES116](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9345).

### 3. Inferência estatística e testes de hipóteses

**Matriz:** Inferência Estatística I–II (GES118, GES124); parte de Probabilidade I–II (GES113, GES117).

**Referências básicas selecionadas:**

- Casella, G., & Berger, R. L. *Inferência Estatística* (edição indicada pela UFLA, 2010). Referência principal para estimação, suficiência, testes e teoria de decisão.
- Bolfarine, H., & Sandoval, M. C. *Introdução à Inferência Estatística* (2ª ed., SBM, 2010). Inferência clássica em nível de graduação avançada.
- DasGupta, A. *Asymptotic Theory of Statistics and Probability* (2008). Fundamentos assintóticos.
- Siegel, S., & Castellan, N. J. *Estatística não-paramétrica para ciências do comportamento* (2ª ed., 2006). Referência para métodos não paramétricos.

**Relação com o Trama:** `trama.models` inclui testes t, Wilcoxon, Kruskal–Wallis, qui-quadrado, Fisher, correlação e testes de pressupostos. Na revisão, conferir condições de aplicação, hipóteses nula e alternativa, definição do estimador/estatística, tratamento de empates e dados ausentes, ajustes por multiplicidade e interpretação de intervalos e valores-p.

**Fontes curriculares:** [GES118](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9347), [GES124](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9353).

### 4. Regressão, modelos lineares, modelos generalizados e mistos

**Matriz:** Modelos Lineares I–II (GES119, GES122); Modelos Lineares Generalizados (GES128); tópicos de modelos em Inferência e Planejamento Experimental.

**Referências básicas selecionadas:**

- Charnet, R., Freire, C. A. L., Charnet, E. M. R., & Bonvino, H. *Análise de modelos de regressão linear com aplicações* (2ª ed., 2008).
- Rencher, A. C., & Schaalje, G. B. *Linear Models in Statistics* (2ª ed., 2008). Modelos lineares em notação matricial e inferência.
- Searle, S. R. *Linear Models* (1997). Formulação e estrutura de modelos lineares.
- Dobson, A. J., & Barnett, A. *An Introduction to Generalized Linear Models* (2008). Modelos lineares generalizados.
- Paula, G. A. *Modelos de regressão com apoio computacional* (2013, apostila indicada pela UFLA).

**Relação com o Trama:** `trama.models` implementa `lm`, `glm`, modelos mistos via `lmer`, hipóteses lineares, ANOVA, diagnósticos e comparações. A futura revisão deve associar cada bloco à família e função de ligação efetivamente usada e revisar independência, estrutura da média/variância, resíduos, codificação de fatores, contrastes, soma de quadrados, REML/ML e limites da inferência após seleção.

**Fontes curriculares:** [GES119](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9348), [GES122](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9351), [GES128](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9357).

### 5. Planejamento experimental e comparação de tratamentos

**Matriz:** Introdução aos Planos Experimentais (GES136); Planejamento e Análise de Experimentos (GES123); Modelos Lineares II (GES122). O currículo também oferece tópicos de experimentação agrícola.

**Referências básicas selecionadas:**

- Montgomery, D. C. *Design and Analysis of Experiments* (9ª ed., 2017). Delineamentos, análise de variância e planejamento experimental.
- Hinkelmann, K., & Kempthorne, O. *Design and Analysis of Experiments* (2ª ed., 2008). Teoria e análise de delineamentos.
- Banzatto, D. A., & Kronka, S. N. *Experimentação agrícola* (4ª ed., 2006).
- Pimentel Gomes, F. *Curso de Estatística Experimental* (15ª ed., 2009).
- Barros Neto, B., Scarminio, I. S., & Bruns, R. E. *Como fazer experimentos: pesquisa e desenvolvimento na ciência e na indústria* (4ª ed., 2010).

**Relação com o Trama:** `trama.models` contém ANOVA para DIC, blocos casualizados, quadrado latino, esquemas fatoriais e parcelas subdivididas, além de comparações de médias e blocos de pressupostos. A revisão deve conferir a unidade experimental, randomização, estrutura do erro, graus de liberdade, contrastes, multiplicidade e adequação de cada teste pós-hoc ao desenho.

**Fontes curriculares:** [GES136](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10199), [GES123](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9352).

### 6. Amostragem e inferência para pesquisas amostrais

**Matriz:** Amostragem (GES142); amostras e delineamentos também aparecem em Probabilidade, Inferência e Estágio.

**Referências básicas selecionadas:**

- Bolfarine, H., & Bussab, W. O. *Elementos de Amostragem* (2005).
- Chaudhuri, A., & Stenger, H. *Survey Sampling: Theory and Methods* (2005).
- Silva, P. L. N., Bianchini, Z. M., & Dias, A. J. R. *Amostragem: teoria e prática usando R* (ENCE, 2020, volumes 1–2).

**Relação com o Trama:** `trama.sampling` cobre amostragem aleatória simples, sistemática, estratificada, PPS, conglomerados e dois estágios; desenho, pesos, pós-estratificação/raking, estimação de médias, totais, proporções e razões; e simulação. A revisão deve seguir a inferência baseada no desenho e examinar probabilidades de inclusão, pesos, correção para população finita, alocação, variância sob conglomerados/estratos e compatibilidade entre o plano declarado e o estimador.

**Fonte curricular:** [GES142](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10256).

### 7. Séries temporais e previsão

**Matriz:** Séries Temporais (GES125); fundamentos também se apoiam em probabilidade, inferência e regressão.

**Referências básicas selecionadas:**

- Morettin, P. A., & Toloi, C. M. *Análise de séries temporais: modelos lineares univariados* (3ª ed., 2018).
- Morettin, P. A., & Toloi, C. M. *Análise de séries temporais: modelos multivariados e não lineares* (2020).
- Shumway, R. H., & Stoffer, D. S. *Time Series Analysis and Its Applications: With R Examples* (listada no plano como 2000).
- Referência complementar atual para aplicação e avaliação de previsões: Hyndman, R. J., & Athanasopoulos, G. *Forecasting: Principles and Practice* (3ª ed., 2021; texto online). [Livro e bibliografia por capítulo](https://otexts.com/fpp3/).

**Relação com o Trama:** `trama.series` inclui operações e gráficos de série, decomposição clássica/STL, testes de estacionariedade e tendência, ARIMA, ETS, Holt–Winters, previsão, baselines e medidas de acurácia. Na revisão: causalidade temporal da validação, estabilidade/estacionariedade, sazonalidade, escolha de ordem, diagnóstico de resíduos, intervalos de previsão, horizonte e comparação contra baselines.

**Fontes curriculares:** [GES125](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9354). A bibliografia de previsão atual está disponível no [texto aberto FPP3](https://otexts.com/fpp3/).

### 8. Análise multivariada, classificação discriminante e dados espaciais

**Matriz:** Análise Multivariada (GES132); Técnicas Multivariadas (GES138); Geoestatística (GES133); disciplinas eletivas de análise sensorial.

**Referências básicas selecionadas:**

- Ferreira, D. F. *Estatística Multivariada* (3ª ed., UFLA, 2018).
- Hair, J. F. Jr. et al. *Análise Multivariada de Dados* (8ª ed., 2019).
- Anderson, T. W. *An Introduction to Multivariate Statistical Analysis* (1984).
- Schabenberger, O., & Gotway, C. A. *Statistical Methods for Spatial Data Analysis* (2005).
- Cressie, N. *Statistics for Spatial Data* (1993), listada como complementar na disciplina de Geoestatística.

**Relação com o Trama:** `trama.multi` inclui PCA, análise fatorial, matrizes de correlação, análise discriminante, regressão logística multivariada e ferramentas de classificação/avaliação. `trama.ml` oferece árvores, FIGS, random forest, SVM, XGBoost e modelos lineares de referência. A coleção dedicada à geoestatística ainda não aparece no catálogo de coleções do Trama; essa área é uma oportunidade de expansão, não uma capacidade atual presumida.

Na revisão: distinguir PCA de análise fatorial; revisar centralização/padronização, escolhas de retenção/rotação, condições para discriminante linear/quadrática, validação fora da amostra, desbalanceamento, métricas e interpretação de importância. Para ML, consultar a referência específica de cada algoritmo e pacote em [`trama.ml/REFERENCES.md`](../collections/trama.ml/REFERENCES.md).

**Fontes curriculares:** [GES132](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10177), [GES138](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10257), [GES133](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10178).

### 9. Computação estatística, Monte Carlo e inferência Bayesiana

**Matriz:** Estatística Computacional (GES126); Inferência Bayesiana (GES129); fundamentos computacionais de Ciência de Dados e Aprendizagem de Máquinas (GES130, GES131).

**Referências básicas selecionadas:**

- Ferreira, D. F. *Estatística computacional em Java* (UFLA, 2013).
- Gentle, J. E. *Random Number Generation and Monte Carlo Methods* (2ª ed., 2003).
- Gamerman, D., & Lopes, H. F. *Markov Chain Monte Carlo: Stochastic Simulation for Bayesian Inference* (2ª ed., 2006).
- Paulino, C. D., Turkman, M. A. A., Murteira, B., & Silva, G. L. *Estatística Bayesiana* (2ª ed. rev. e ampl., 2018).
- James, G., Witten, D., Hastie, T., & Tibshirani, R. *An Introduction to Statistical Learning with Applications in R* (2ª ed., 2021), disponível no [site dos autores](https://www.statlearning.com/).
- Hastie, T., Tibshirani, R., & Friedman, J. *The Elements of Statistical Learning* (2ª ed., 2009).

**Relação com o Trama:** estes tópicos sustentam a reprodutibilidade computacional, simulação, ajuste Bayesiano e os modelos de aprendizado de `trama.ml`. A coleção `trama.ml` atualmente oferece CART, FIGS, random forest, SVM, XGBoost e referências lineares/logísticas; não inferir que a matriz curricular por si só valide defaults, estratégia de seleção de hiperparâmetros ou propriedades de cada implementação.

**Fontes curriculares:** [GES126](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9355), [GES129](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9358), [GES131](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9360).

## Extensões eletivas e de domínio

Estas áreas aparecem entre as eletivas da matriz. Elas ampliam os possíveis usos do Trama, mas não são cobertas necessariamente por coleções atuais:

| Área | Disciplinas da matriz | Referências centrais indicadas nos planos |
|---|---|---|
| Computação aplicada | Programação Aplicada com Suporte de IA (GAC126) | Behrman, *Fundamentos de Python para ciência de dados*; Sweigart, *Automate the Boring Stuff with Python*. |
| Pesquisa operacional e otimização | Pesquisa Operacional (GAE140) | Hillier & Lieberman, *Introdução à Pesquisa Operacional*; Arenales et al., *Pesquisa Operacional*. |
| Economia, finanças e derivativos | Mercado de Capitais (GAE309); Fundamentos de Macroeconomia (GAE338); Diagnóstico e Análise de Problemas Econômicos (GAE346); Estratégias em Mercados de Derivativos Agropecuários (GGA114) | Hull, *Opções, futuros e outros derivativos*; bibliografias de economia dos planos correspondentes. |
| Geociências e variabilidade espacial | Geoestatística (GES133) | Schabenberger & Gotway; Cressie; Isaaks & Srivastava, *An Introduction to Applied Geostatistics*. |
| Ciência de alimentos e experimentação sensorial | Análise Sensorial (GCA119) | Meilgaard, Civille & Thomas, *Sensory Evaluation Techniques*; Minim, *Análise sensorial: estudos com consumidores*. |
| Matemática avançada | Teoria dos Conjuntos (GMM105); Álgebra Linear (GMM109); Teoria dos Números (GMM112); Álgebra (GMM113); EDO (GMM114, GMM156); Análise Matemática (GMM115); Espaços Métricos (GMM120); Variáveis Complexas (GMM122); Trigonometria e Números Complexos (GMM128); Cálculo Avançado (GMM129); Complementos de Álgebra Linear (GMM136); Matemática Finita (GMM141) | Referências específicas de cada plano. A matriz não transforma esses tópicos em métodos disponíveis no Trama. |
| Física | Física A (GFI125) | Nussenzveig, *Curso de Física Básica*; Young & Freedman, *Sears & Zemansky Física I*. |
| Formação profissional, legislação, inclusão e linguagem | GES140; LIBRAS (GDE124); História e Culturas Afro-Brasileiras e Indígenas (GDE165); Direito e Legislação (GDI189); disciplinas de inglês | Manter as referências associadas aos respectivos planos e áreas; não são referências estatísticas para algoritmos. |

Para verificar todas as eletivas e seus planos, consulte a [matriz 2026/01 no SIG/UFLA](https://sig.ufla.br/modulos/publico/matrizes_curriculares/index.php?cod_matriz_curricular=381&op=abrir) e os PDFs de ementa ligados no [catálogo organizado de ementas](../ementas-estatistica-ufla-2026-01.md).

## Índice para a futura revisão metodológica

| Coleção | Núcleo de métodos a revisar | Perguntas metodológicas prioritárias |
|---|---|---|
| `trama.models` | Testes, regressão linear/generalizada/mista, ANOVA, pressupostos, comparações de médias | O contrato e a saída expõem as hipóteses e pressupostos certos? Os contrastes, graus de liberdade, ajuste por multiplicidade e diagnósticos correspondem ao modelo/desenho? |
| `trama.sampling` | Seleção, desenho, ponderação, estimadores, dimensionamento e simulação | As probabilidades de inclusão e pesos são preservados? Os estimadores e variâncias respeitam estratos/conglomerados/estágios? A simulação avalia o estimador sob o desenho que declara? |
| `trama.series` | Decomposição, testes de estacionariedade/tendência, ARIMA/ETS/Holt–Winters e previsão | A validação respeita a ordem temporal? Há baseline? Testes, resíduos e intervalos são interpretados corretamente? |
| `trama.multi` | PCA, análise fatorial, discriminante, logística, correlação e classificação | As matrizes/escala, critérios de retenção, suposições e validação são explícitos? Os rótulos distinguem associação, redução de dimensão e classificação? |
| `trama.ml` | CART, FIGS, random forest, SVM, XGBoost, avaliação e importância | Os dados de teste permanecem isolados? A validação e a busca de hiperparâmetros evitam vazamento? Limitações de tarefas e interpretabilidade estão documentadas? |
| `trama.data` e `trama.view` | Preparação, transformações e visualização | Transformações que afetam população/amostra e escolhas gráficas ficam rastreáveis? A visualização não é apresentada como evidência inferencial por si só? |

Para cada método revisado, registrar separadamente: (1) a fonte teórica; (2) a API e versão do pacote R usado; (3) o contrato e parâmetros do nó; (4) pressupostos e condições de validade; (5) testes de referência/simulações; (6) limites conhecidos; e (7) a fonte visível na ajuda do bloco. Esse registro permite rastrear a afirmação do Trama até a teoria e à implementação efetiva.

## Fontes

- UFLA/SIG. [Matriz curricular G058 — Estatística, 2026/01](https://sig.ufla.br/modulos/publico/matrizes_curriculares/index.php?cod_matriz_curricular=381&op=abrir).
- UFLA/SIG. [Ementas e conteúdos programáticos oficiais](https://sig.ufla.br/modulos/publico/matrizes_curriculares/index.php). Os links por disciplina acima abrem os PDFs específicos de onde foram extraídas as referências curriculares.
- Trama. [Referências dos métodos de aprendizado de máquina](../collections/trama.ml/REFERENCES.md), com fontes de algoritmos e pacotes para CART, FIGS, random forest, SVM, XGBoost e modelos lineares.
- Hyndman, R. J., & Athanasopoulos, G. [*Forecasting: Principles and Practice*, 3ª ed.](https://otexts.com/fpp3/), fonte aberta atual para previsão.
- James, G., Witten, D., Hastie, T., & Tibshirani, R. [*An Introduction to Statistical Learning*](https://www.statlearning.com/), site dos autores com edições e recursos do livro.
