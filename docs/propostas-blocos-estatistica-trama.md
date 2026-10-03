# Propostas de blocos estatísticos para o Trama

## Como ler este documento

Esta é uma proposta de produto, ainda não uma especificação de implementação. Os blocos estão agrupados por trabalho analítico e cada grupo aponta para referências listadas diretamente nos planos ou no PPC do Bacharelado em Estatística da UFLA. “Básica” e “complementar” seguem a classificação dos documentos oficiais.

As referências curriculares dão um ponto de partida, mas não substituem a seleção de uma fonte metodológica específica para cada algoritmo, a verificação do pacote R ou a revisão das decisões estatísticas antes de implementar um bloco.

## Prioridade A — áreas explícitas na matriz, sem coleção correspondente

### Inferência Bayesiana — futura `trama.bayes`

| Bloco candidato | O que entregaria | Referências curriculares iniciais |
|---|---|---|
| `bayes/model` | Ajuste de um modelo declarado com verossimilhança, prioris, algoritmo, cadeias, iterações e semente explícitos. | **Básicas GES129:** Paulino et al., *Estatística Bayesiana* (2018); Gamerman & Lopes, *Markov Chain Monte Carlo* (2ª ed., 2006); Lee, *Bayesian Statistics: An Introduction* (4ª ed., 2012); Box & Tiao, *Bayesian Inference in Statistical Analysis* (1992). [Plano GES129](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9358) |
| `bayes/prior_predictive` | Simulação preditiva a priori para inspecionar escala, suporte e implicações das prioris antes do ajuste. | Paulino et al.; Gamerman & Lopes; Box & Tiao. [Plano GES129](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9358) |
| `bayes/diagnostics` | Diagnósticos de convergência e amostragem, com tabelas e gráficos por cadeia; separar falha computacional de evidência substantiva. | **Complementares GES129:** Gelman et al., *Bayesian Data Analysis* (3ª ed.); Albert, *Bayesian Computation with R* (2009); Gamerman & Lopes. [Plano GES129](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9358) |
| `bayes/posterior` | Resumos posteriores, intervalos, probabilidades de eventos e comparações de parâmetros. | Paulino et al.; Lee; Box & Tiao. [Plano GES129](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9358) |
| `bayes/predictive_check` | Distribuição preditiva posterior e comparação visual entre dados observados e dados replicados. | **Complementares GES129:** Gelman et al., *Bayesian Data Analysis*; Albert, *Bayesian Computation with R*. [Plano GES129](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9358) |

**Dados de fluxo sugeridos:** um tipo `bayes/fit` que preserve ajuste, dados usados, fórmula, prioris, versão do motor e configuração de amostragem; saídas distintas para diagnóstico, resumo posterior e checagem preditiva. Evitar esconder decisões de prioris e convergência em texto livre.

### Geoestatística — futura `trama.spatial`

| Bloco candidato | O que entregaria | Referências curriculares iniciais |
|---|---|---|
| `spatial/coordinates` | Declarar colunas de coordenadas, sistema/unidade espacial e suporte amostral. | Schabenberger & Gotway, *Statistical Methods for Spatial Data Analysis* (2005); Clark, *Practical Geostatistics* (1979). **Básicas GES133.** [Plano GES133](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10178) |
| `spatial/autocorrelation` | Explorar dependência espacial para os tipos de dados cobertos pela coleção, distinguindo dados de área, pontos e variáveis regionalizadas. | Schabenberger & Gotway (2005); Vieira et al. (1983), sobre teoria geoestatística e variabilidade de propriedades agronômicas. **Básicas GES133.** [Plano GES133](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10178) |
| `spatial/variogram` | Estimar e visualizar variograma/covariograma, com modelo ajustado e parâmetros rastreáveis. | Clark (1979); Schabenberger & Gotway (2005); Cressie, *Statistics for Spatial Data* (1993). Cressie aparece como **complementar GES133**. [Plano GES133](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10178) |
| `spatial/kriging` | Gerar predição espacial e erro/variância de krigagem; explicitar modelo, vizinhança e suporte. | Schabenberger & Gotway (2005); Isaaks & Srivastava, *An Introduction to Applied Geostatistics* (1989); Journel & Huijbregts, *Mining Geostatistics* (1981). **Complementares GES133.** [Plano GES133](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10178) |
| `spatial/cokriging` | Analisar duas ou mais variáveis espacialmente relacionadas e estimar por cokrigagem. | Schabenberger & Gotway (2005); Wackernagel, *Multivariate Geostatistics* (2002). **Básica/complementar GES133.** [Plano GES133](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10178) |
| `spatial/validation` | Validação cruzada espacial, diagnóstico e comparação de erros de predição por vizinhança. | Isaaks & Srivastava (1989); Diggle & Ribeiro Jr., *Model-based Geostatistics* (2007); Schabenberger & Gotway (2005). **Complementares GES133.** [Plano GES133](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10178) |

**Dados de fluxo sugeridos:** tipo espacial que mantenha coordenadas, sistema de referência e geometria separados da tabela comum; saídas próprias para ajuste, superfície predita e incerteza. Não tratar coordenadas como colunas intercambiáveis sem registrar o sistema e a unidade.

## Prioridade B — expandir coleções existentes

### Planejamento experimental — nova coleção `trama.experiments`

Todo o planejamento experimental sai de `trama.models` e vai para uma coleção própria, `trama.experiments`, descrita em [visão de design](visao-trama-experiments.md). `trama.models` fica como está; experiments lê e produz `models/fit` sem criar tipo de modelo novo.

| Bloco | O que entrega | Referências iniciais |
|---|---|---|
| `experiments/design` | Declarar e sortear o delineamento (DIC, DBC, DQL, fatorial, confundimento, fracionado, composto central, parcelas subdivididas, faixas, BIB, medidas repetidas, crossover, grupos de experimentos), com semente. | Montgomery (2017); Hinkelmann & Kempthorne (2008); Banzatto & Kronka (2006); Cochran & Cox (1957) |
| `experiments/effect` | Somar um termo à resposta simulada: intercepto, fixo (por nível ou por contraste), aleatório, interação, quantitativo, covariável. | Montgomery (2017); Mead (1994) |
| `experiments/error` | Fechar a resposta: normal, Poisson, binomial, gama; variância por nível; correlação em medidas repetidas; parcelas perdidas. | Montgomery (2017); Milliken & Johnson (1997) |
| `experiments/power` | Poder e número de repetições por simulação da cadeia design → effect → error → análise. | Montgomery (2017); Barros Neto, Scarminio & Bruns (2010) |
| `experiments/randomization_test` | Teste de aleatorização re-sorteando pelo próprio delineamento. | Bailey (2008); Hinkelmann & Kempthorne (2008) |
| `experiments/contrasts` | Contrastes ortogonais prontos (polinomiais, Helmert, controle, fatoriais), desdobramento do SQ e verificação de ortogonalidade. | Montgomery (2017); Banzatto & Kronka (2006) |
| `experiments/boxcox` | Perfil de λ e transformação sugerida. | Box & Cox (1964) |
| `experiments/response_surface` | Ajuste de segunda ordem, análise canônica e ponto estacionário. | Myers & Montgomery (2008); Cirillo (2015) |

### Análise multivariada em `trama.multi`

| Bloco candidato | O que entregaria | Referências curriculares iniciais |
|---|---|---|
| `multi/cluster` | Agrupar observações com método e distância explícitos; retornar atribuição, centros/dendrograma e medidas de adequação. | Ferreira, *Estatística Multivariada* (3ª ed., UFLA, 2018); Hair et al., *Análise Multivariada de Dados* (8ª ed., 2019); Anderson, *An Introduction to Multivariate Statistical Analysis* (1984). **Básicas GES138/GES132.** [GES138](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10257), [GES132](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10177) |
| `multi/cluster_diagnostics` | Ajudar a avaliar número de grupos e estabilidade, sem apresentar um índice isolado como prova de grupos naturais. | Hair et al. (2019); Tabachnick & Fidell, *Using Multivariate Statistics* (7ª ed., 2019). **Básicas GES138.** [Plano GES138](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=10257) |
| `multi/correspondence` | Representar associação entre categorias em tabelas de contingência. | Ferreira (2018); Hair et al. (2019). **Básicas GES138/GES132**, referências gerais de análise multivariada; selecionar fonte específica antes da implementação. |

### Validação temporal em `trama.series`

| Bloco candidato | O que entregaria | Referências curriculares iniciais |
|---|---|---|
| `series/rolling_origin` | Repetir previsões em origens temporais sucessivas, sem embaralhar observações futuras para o treino. | Morettin & Toloi, *Análise de séries temporais: modelos lineares univariados* (3ª ed., 2018); Shumway & Stoffer, *Time Series Analysis and Its Applications*. **Básicas GES125.** [Plano GES125](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9354) |
| `series/compare_forecasts` | Comparar modelos e baselines em horizontes e origens equivalentes; apresentar erro e incerteza da comparação. | Morettin & Toloi (2018, 2020); Shumway & Stoffer. **Básicas GES125.** [Plano GES125](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9354) |
| `series/xreg` | Ajustar modelos com covariáveis externas e exigir alinhamento temporal e disponibilidade da covariável no horizonte previsto. | Morettin & Toloi, volume 2 (2020); Shumway & Stoffer. **Básicas GES125.** [Plano GES125](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9354) |

### Validação preditiva em `trama.ml`

| Bloco candidato | O que entregaria | Referências curriculares iniciais |
|---|---|---|
| `ml/group_split` | Separar treino e teste por indivíduo, unidade, grupo ou período, para impedir que observações relacionadas atravessem os conjuntos. | James et al., *An Introduction to Statistical Learning with Applications in R* (2ª ed., 2021); Hastie, Tibshirani & Friedman, *The Elements of Statistical Learning* (2ª ed., 2009). **Básicas GES131.** [Plano GES131](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9360) |
| `ml/calibration` | Comparar probabilidades previstas e frequências observadas; separar discriminação, calibração e escolha de limiar. | James et al. (2021); Hastie et al. (2009). **Básicas GES131.** [Plano GES131](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9360) |
| `ml/compare_models` | Comparar modelos sob os mesmos folds, métrica, orçamento e conjunto final de teste. | James et al. (2021); Hastie et al. (2009). **Básicas GES131.** [Plano GES131](https://sig.ufla.br/modulos/publico/matrizes_curriculares/gerar_ementa.php?cod_disciplina=9360) |

## Áreas em que falta bibliografia metodológica específica

### Inferência causal — possível futura `trama.causal`

A matriz oferece referências para regressão e planejamento experimental, mas a busca nos PDFs da matriz 2026/01 não encontrou bibliografia básica explicitamente dedicada à identificação causal em estudos observacionais. Ementas da USP preenchem parte dessa lacuna: MAE0042 lista referências para resultados potenciais, DAGs e desenhos quase experimentais. Por isso, ficam como **conceitos candidatos**, e não como especificação pronta: `causal/estimand`, `causal/assumptions`, `causal/estimate` e `causal/sensitivity`. Consulte [as referências USP/ESALQ para inferência causal](referencias-complementares-usp-esalq.md#inferencia-causal--futura-tramacausal) e verifique as condições de identificação antes de implementar estimadores.

### Sobrevivência — possível futura `trama.survival`

Não foi encontrada uma bibliografia básica dedicada à análise de sobrevivência nos planos consultados da matriz 2026/01 da UFLA. Ementa detalhada da USP lista referências próprias para censura, Kaplan–Meier, Nelson–Aalen, log-rank e Cox, embora o registro detalhado disponível seja da disciplina desativada MAE0514; é preciso conferir a bibliografia da oferta atual MAE0354 antes de fechar a seleção. O PPC 2023 cita, na bibliografia de Modelos Lineares, Resende, Silva & Azevedo (2014), cuja descrição inclui sobrevivência, mas essa é uma referência abrangente e não basta para fundamentar uma coleção. Blocos como `survival/kaplan_meier`, `survival/cox` e `survival/check_ph` ficam condicionados à revisão das fontes específicas. Consulte [o levantamento USP/ESALQ](referencias-complementares-usp-esalq.md#analise-de-sobrevivencia--futura-tramasurvival).

## Rastreabilidade para a implementação

Antes de implementar cada bloco proposto, registrar no catálogo:

1. disciplina e matriz que inspiraram o problema;
2. referência curricular e seção básica/complementar;
3. fonte metodológica específica e páginas/capítulos pertinentes;
4. pacote R e versão usados como implementação;
5. entradas, parâmetros, pressupostos, saídas e limites;
6. dados ou simulação de referência para conferir o comportamento.

O mapa da matriz e as informações de atribuição/creditação estão em [Referências de Estatística para o Trama](referencias-estatistica-trama.md), [Atribuição curricular UFLA](atribuicao-curricular-ufla.md) e [Referências complementares USP/ESALQ](referencias-complementares-usp-esalq.md).
