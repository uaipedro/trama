# Cobertura de oráculos

Levantamento de 08/10/2026: para cada bloco estatístico (os que têm referências
em `site/src/data/node-docs.json`), se os testes da coleção comparam o resultado
com um oráculo externo — a saída de um pacote R de referência ou números de um
exemplo publicado, com a fonte citada no teste.

Não conta como oráculo: conferir só classe, nomes ou forma da saída; recalcular
a fórmula dentro do próprio teste; comparar o bloco com outro bloco que reusa a
mesma conta. "Parcial" quer dizer que o oráculo cobre só parte da saída.

O levantamento foi feito por leitura automática dos testes e conferido por
amostragem. Ao corrigir uma linha ou acrescentar um oráculo, atualize esta tabela.

Resumo: 64 com oráculo, 19 parciais, 44 sem oráculo (127 blocos).

Atualizado em 09/10/2026 com a `trama.spatial`, que faltava inteira: os seis
blocos estatísticos dela entraram. Os de fonte e preparação
(`spatial/example`, `spatial/coordinates`, `spatial/read_points`,
`spatial/boundary`) e os de leitura (`spatial/explore`, `spatial/map`) ficam
fora, como os de `data/*` e `view/*`.

| Bloco | Situação | Evidência (caminhos relativos a `collections/`) |
|---|---|---|
| `experiments/effect` | sem oráculo | trama.experiments/tests/testthat/test-simular.R:8 so identidades internas e recuperacao de magnitude declarada |
| `experiments/error` | sem oráculo | trama.experiments/tests/testthat/test-simular.R:27 so checagens internas do motor de simulacao |
| `experiments/view` | sem oráculo | trama.experiments/tests/testthat/test-simular.R:244 so fluxo do motor; sem oraculo externo |
| `ml/figs` | sem oráculo | trama.ml/tests/testthat/test-models.R:109 so forma da previsao; figsr sem referencia numerica |
| `ml/nested_cv` | sem oráculo | trama.ml/tests/testthat/test-nested-cv.R:3 folds conferidos a mao pelo proprio teste, sem pacote |
| `ml/split` | sem oráculo | trama.ml/tests/testthat/test-proveniencia.R:26 so proveniencia e reprodutibilidade |
| `ml/svm` | sem oráculo | trama.ml/tests/testthat/test-models.R:337 coerencia interna com prob_*; e1071 nao comparado numericamente |
| `ml/tune` | sem oráculo | trama.ml/tests/testthat/test-tuning.R:19 so status e formato do historico |
| `ml/xgboost` | sem oráculo | trama.ml/tests/testthat/test-models.R:411 so rotulo e soma = 1; metricas de exemplo em test-exemplos.R sem fonte |
| `models/anova_dql` | sem oráculo | trama.models/tests/testthat/test-testes.R:224 o proprio teste admite: sem oraculo de pacote |
| `models/random_test` | sem oráculo | trama.models/tests/testthat/test-resumir.R:94 so gl e forma do quadro; sem oraculo numerico |
| `models/tukey_additivity` | sem oráculo | trama.models/tests/testthat/test-testes.R:10 so classe e gl; sem valor de referencia |
| `multi/box_m` | sem oráculo | trama.multi/tests/testthat/test-discriminante.R:96 literais (140,94; 146,66) sem fonte citada |
| `multi/jackknife_discriminant` | sem oráculo | trama.multi/tests/testthat/test-jackknife.R:103 so recusa e forma; sem oraculo numerico |
| `multi/jackknife_fa` | sem oráculo | trama.multi/tests/testthat/test-jackknife.R:91 so alinhamento interno |
| `multi/jackknife_logistic` | sem oráculo | trama.multi/tests/testthat/test-jackknife.R:112 so relacoes internas (exp, EP de Wald); sem glm de referencia |
| `sampling/margin` | sem oráculo | trama.sampling/tests/testthat/test-precisao.R:1 formula recomputada no teste (qt), sem pacote |
| `sampling/margin_levels` | sem oráculo | trama.sampling/tests/testthat/test-precisao.R:15 formulas recomputadas (Kish, qnorm), sem pacote |
| `sampling/mean` | sem oráculo | trama.sampling/tests/testthat/test-estimar.R:1 formulas do livro recomputadas no teste; sem survey |
| `sampling/poststratify` | sem oráculo | trama.sampling/tests/testthat/test-estimar.R:80 propriedade qualitativa; sem survey |
| `sampling/question_margins` | sem oráculo | trama.sampling/tests/testthat/test-precisao.R:110 formula recomputada (qnorm com Bonferroni), sem pacote |
| `sampling/rake` | sem oráculo | trama.sampling/tests/testthat/test-precisao.R:87 propriedade das margens e equivalencia interna com pos-estratificacao |
| `sampling/ratio` | sem oráculo | trama.sampling/tests/testthat/test-estimar.R:64 so checagens de forma e faltantes |
| `sampling/referral` | sem oráculo | trama.sampling/tests/testthat/test-planejar-t.R:95 formula recomputada (qt, deff), sem pacote |
| `sampling/simulate` | sem oráculo | trama.sampling/tests/testthat/test-avaliar.R:1 propriedades estatisticas por simulacao; sem oraculo externo |
| `sampling/size_cluster` | sem oráculo | trama.sampling/tests/testthat/test-planejar-t.R:32 menor n por formula com qt (busca propria), sem pacote |
| `sampling/size_domains` | sem oráculo | trama.sampling/tests/testthat/test-precisao.R:53 desigualdades com qt, sem pacote |
| `sampling/size_mean` | sem oráculo | trama.sampling/tests/testthat/test-planejar-t.R:15 menor n por formula com qt (busca propria), sem pacote |
| `sampling/size_proportion` | sem oráculo | trama.sampling/tests/testthat/test-planejar-t.R:15 menor n por formula com qt (busca propria), sem pacote |
| `sampling/size_stratified` | sem oráculo | trama.sampling/tests/testthat/test-planejar.R:57 formula com qt recomputada, sem pacote |
| `sampling/total` | sem oráculo | trama.sampling/tests/testthat/test-estimar.R:1 formulas do livro recomputadas no teste; sem survey |
| `series/accuracy` | sem oráculo | trama.series/tests/testthat/test-modelar.R:86 so consistencia entre treino, teste e cobertura |
| `series/adf` | sem oráculo | trama.series/tests/testthat/test-testar.R:110 estatistica -1,565276 literal sem fonte citada; ur.df so em comentario |
| `series/baseline` | sem oráculo | trama.series/tests/testthat/test-modelar.R:58 so checagem de metodos e erro |
| `series/box_pierce` | sem oráculo | trama.series/tests/testthat/test-testar.R:476 so diferenca entre BP e LB; Box.test nao chamado |
| `series/cox_stuart` | sem oráculo | trama.series/tests/testthat/test-testar.R:747 so contagem de pares e comportamento com ruido |
| `series/decompose` | sem oráculo | trama.series/tests/testthat/test-decompor.R:1 soma e forma internas; sem stats::decompose como referencia |
| `series/ets` | sem oráculo | trama.series/tests/testthat/test-modelar.R:25 so ajuste e forma; forecast::ets nao usado como referencia |
| `series/holt_winters` | sem oráculo | trama.series/tests/testthat/test-modelar.R:33 so classe do objeto |
| `series/ljung_box` | sem oráculo | trama.series/tests/testthat/test-testar.R:457 so decisao e graus; Box.test nao usado como referencia |
| `series/periodicity_fisher` | sem oráculo | trama.series/tests/testthat/test-testar.R:1312 valores medidos pelo proprio bloco; fonte externa nao citada |
| `series/phillips_perron` | sem oráculo | trama.series/tests/testthat/test-testar.R:201 stats::PP.test so citado em comentario; p nao comparado |
| `series/seasonality_kw` | sem oráculo | trama.series/tests/testthat/test-testar.R:1207 so decisoes e casos de erro; kruskal.test nao usado como referencia do bloco |
| `series/stl` | sem oráculo | trama.series/tests/testthat/test-decompor.R:40 soma e forma internas; sem stats::stl como referencia |
| `experiments/design` | parcial | trama.experiments/tests/testthat/test-design.R:19 DBC vs agricolae::design.rcbd; composto central vs rsm::ccd; DIC/DQL/fatorial so propriedades combinatorias |
| `models/anova_dbc` | parcial | trama.experiments/tests/testthat/test-contrastes.R:133 car::linearHypothesis e SQ do livro sobre contrastes do DBC; quadro do DBC nao comparado direto |
| `models/anova_dic` | parcial | trama.experiments/tests/testthat/test-contrastes.R:6 SQ de contrastes do livro (Montgomery 3.1) sobre o DIC; quadro nao comparado direto |
| `models/breusch_pagan` | parcial | trama.models/tests/testthat/test-testes.R:15 estatistica recalculada no teste (lm auxiliar + pchisq); sem pacote de referencia |
| `models/cohen_d` | parcial | trama.models/tests/testthat/test-efeito.R:91 g vs J exato de Hedges (1981); d e IC so formula |
| `models/confusion` | parcial | trama.models/tests/testthat/test-avaliar.R:67 reuso do calculo da multi/logistic; sem pacote externo |
| `models/cor_test` | parcial | trama.models/tests/testthat/test-testes.R:45 so o coeficiente r vs stats::cor; p-valor e IC nao conferidos |
| `models/effect_size` | parcial | trama.models/tests/testthat/test-efeito.R:14 formulas sobre o quadro; unico numero externo e quadro impresso sem fonte |
| `models/evaluate` | parcial | trama.models/tests/testthat/test-avaliar.R:175 reuso das metricas da ml; sem pacote externo |
| `models/lmer` | parcial | trama.models/tests/testthat/test-gls.R:95 F tipo III vs lmerTest; coeficientes e variancias so comparados consigo |
| `models/pairwise` | parcial | trama.models/tests/testthat/test-medias.R:63 so Dunnett vs multcomp::glht; Tukey so consistencia interna das letras |
| `models/wilcoxon` | parcial | trama.models/tests/testthat/test-testes.R:354 so a bisserial de postos vs effectsize; p-valor nao conferido por oraculo |
| `sampling/detectable_difference` | parcial | trama.sampling/tests/testthat/test-precisao.R:81 so n iguais (Fleiss) vs power.prop.test; n desiguais e t por formula |
| `series/f_global` | parcial | trama.series/tests/testthat/test-catalogo.R:343 alias de models/fit_stats; sem teste proprio do id |
| `series/f_seasonal` | parcial | trama.series/tests/testthat/test-catalogo.R:341 alias de models/anova_table (SQ tipo III vs car); sem teste proprio do id |
| `series/f_trend` | parcial | trama.series/tests/testthat/test-catalogo.R:342 alias de models/anova_table (SQ tipo III vs car); sem teste proprio do id |
| `series/kpss` | parcial | trama.series/tests/testthat/test-testar.R:170 criticos 10/5/1 vs tabela do ur.kpss (urca); estatistica nao conferida |
| `series/regression` | parcial | trama.series/tests/testthat/test-decompor.R:344 oraculo cobre a previsao (tslm), nao os coeficientes |
| `experiments/boxcox` | com oráculo | trama.experiments/tests/testthat/test-boxcox.R:11 MASS::boxcox na mesma grade (perfil e IC) |
| `experiments/contrasts` | com oráculo | trama.experiments/tests/testthat/test-contrastes.R:6 SQ do livro (Montgomery 3.1, 6.2) e car::linearHypothesis, emmeans, poly |
| `experiments/power` | com oráculo | trama.experiments/tests/testthat/test-avaliar.R:26 binom.test (Clopper-Pearson) e pf com ncp analitico |
| `experiments/randomization_test` | com oráculo | trama.experiments/tests/testthat/test-avaliar.R:146 coin::oneway_test exato; Monte Carlo vs coin |
| `experiments/response_surface` | com oráculo | trama.experiments/tests/testthat/test-superficie.R:15 rsm::rsm e rsm::canonical sobre ChemReact |
| `ml/cart` | com oráculo | trama.ml/tests/testthat/test-models.R:278 poda cp 1-EP vs rpart com mesma semente e folds |
| `ml/forest` | com oráculo | trama.ml/tests/testthat/test-models.R:382 importancia por permutacao e impureza vs ranger direto |
| `ml/linear` | com oráculo | trama.ml/tests/testthat/test-contrato.R:72 coeficientes vs stats::lm |
| `models/anova_factorial` | com oráculo | trama.models/tests/testthat/test-ajustar.R:64 F do quadro vs stats::aov (ToothGrowth) |
| `models/anova_split_plot` | com oráculo | trama.models/tests/testthat/test-ajustar.R:100 F tipo III vs modelo misto; gl conferidos com livro |
| `models/anova_table` | com oráculo | trama.models/tests/testthat/test-resumir.R:1 SQ tipo II/III vs car::Anova; quadro do aov em test-ajustar.R |
| `models/bartlett` | com oráculo | trama.models/tests/testthat/test-testes.R:7 p-valor vs stats::bartlett.test |
| `models/chisq` | com oráculo | trama.models/tests/testthat/test-testes.R:164 p-valor sem e com Yates vs stats::chisq.test |
| `models/compare` | com oráculo | trama.models/tests/testthat/test-binomial-negativa.R:46 razao de verossimilhanca vs stats::anova de glm.nb |
| `models/duncan` | com oráculo | trama.models/tests/testthat/test-lagarta-agricolae.R:46 grupos vs agricolae::duncan.test |
| `models/dunn` | com oráculo | trama.models/tests/testthat/test-testes.R:116 z e p fixados de dunn.test 1.3.6 (comentario em test-testes.R:113-114) |
| `models/emmeans` | com oráculo | trama.models/tests/testthat/test-glmer.R:43 medias e contrastes vs emmeans |
| `models/fisher_exact` | com oráculo | trama.models/tests/testthat/test-testes.R:46 p-valor vs stats::fisher.test |
| `models/friedman` | com oráculo | trama.models/tests/testthat/test-friedman.R:24 estatistica e p vs stats::friedman.test (exemplo RoundingTimes) |
| `models/glm` | com oráculo | trama.models/tests/testthat/test-ajustar.R:41 coeficientes vs stats::glm |
| `models/glmer` | com oráculo | trama.models/tests/testthat/test-glmer.R:10 fixos e variancia vs lme4::glmer (cbpp) |
| `models/gls` | com oráculo | trama.models/tests/testthat/test-gls.R:10 coeficientes e logLik vs nlme::gls (Ovary, Orthodont) |
| `models/kruskal` | com oráculo | trama.models/tests/testthat/test-testes.R:43 p-valor vs stats::kruskal.test |
| `models/levene` | com oráculo | trama.models/tests/testthat/test-testes.R:173 p-valor literal de O'Neill & Mathews (oneilldbc) e ExpDes.pt |
| `models/linear_hypothesis` | com oráculo | trama.models/tests/testthat/test-hipotese.R:30 F vs car::linearHypothesis e emmeans::contrast |
| `models/lm` | com oráculo | trama.models/tests/testthat/test-ajustar.R:5 coeficientes vs stats::lm |
| `models/nls` | com oráculo | trama.models/tests/testthat/test-naolinear.R:67 coeficientes vs stats::nls (Puromycin) e NIST StRD |
| `models/one_sample_t` | com oráculo | trama.models/tests/testthat/test-testes.R:49 p-valor vs stats::t.test |
| `models/paired_t` | com oráculo | trama.models/tests/testthat/test-testes.R:54 p-valor vs stats::t.test pareado |
| `models/polinomial` | com oráculo | trama.models/tests/testthat/test-polinomial.R:19 SQ do livro (Montgomery) e lm com poly() |
| `models/pr_curve` | com oráculo | trama.models/tests/testthat/test-pr-curve.R:20 AP vs yardstick e area vs PRROC |
| `models/predict` | com oráculo | trama.models/tests/testthat/test-prever.R:6 previsao e intervalos vs predict.lm e formula do livro |
| `models/rls` | com oráculo | trama.models/tests/testthat/test-rls.R:4 coeficientes vs lm() |
| `models/roc` | com oráculo | trama.models/tests/testthat/test-roc-ic.R:9 AUC, IC DeLong e Youden vs pROC |
| `models/scott_knott` | com oráculo | trama.models/tests/testthat/test-lagarta-agricolae.R:75 grupos vs pacote ScottKnott; exemplo do sorgo em test-scott-knott.R:33 |
| `models/shapiro` | com oráculo | trama.models/tests/testthat/test-testes.R:51 p-valor vs stats::shapiro.test |
| `models/shapiro_residuals` | com oráculo | trama.models/tests/testthat/test-testes.R:4 p-valor sobre residuos vs stats::shapiro.test |
| `models/t_test` | com oráculo | trama.models/tests/testthat/test-testes.R:38 p-valor e efeito vs stats::t.test (ToothGrowth) |
| `models/waller_duncan` | com oráculo | trama.models/tests/testthat/test-lagarta-agricolae.R:50 grupos vs agricolae::waller.test |
| `multi/cluster` | com oráculo | trama.multi/tests/testthat/test-agrupamento.R:44 hclust, cutree, cofenetica e kmeans vs stats |
| `multi/discriminant` | com oráculo | trama.multi/tests/testthat/test-discriminante.R:8 escalas e predicao vs MASS::lda; LOO vs MASS CV=TRUE |
| `multi/distance` | com oráculo | trama.multi/tests/testthat/test-agrupamento.R:11 dist, mahalanobis e cluster::daisy |
| `multi/factor_analysis` | com oráculo | trama.multi/tests/testthat/test-fatorial.R:16 ML e rotacoes vs factanal; PAF vs psych::fa |
| `multi/jackknife_pca` | com oráculo | trama.multi/tests/testthat/test-jackknife.R:71 replicas via stats::prcomp |
| `multi/kmo_bartlett` | com oráculo | trama.multi/tests/testthat/test-diagnostico.R:7 KMO, MSA e Bartlett vs psych |
| `multi/logistic` | com oráculo | trama.multi/tests/testthat/test-logistica-perfilada.R:24 IC de perfil vs confint.glm; LRT vs drop1 |
| `multi/manova` | com oráculo | trama.multi/tests/testthat/test-manova.R:6 estatisticas vs summary.manova e car::Manova |
| `multi/mardia` | com oráculo | trama.multi/tests/testthat/test-mardia.R:10 b1,p e b2,p vs psych::mardia |
| `multi/pca` | com oráculo | trama.multi/tests/testthat/test-pca.R:7 sdev e rotacao vs stats::prcomp |
| `multi/tocher` | com oráculo | trama.multi/tests/testthat/test-agrupamento.R:116 grupos vs biotools::tocher (garlicdist) |
| `sampling/proportion` | com oráculo | trama.sampling/tests/testthat/test-proporcao-ic.R:25 IC logit e beta vs survey::svyciprop em AAS, estratificada, conglomerados |
| `series/arima` | com oráculo | trama.series/tests/testthat/test-arima-fit.R:12 LR vs lmtest::lrtest; coeftest; AICc vs forecast::Arima |
| `series/detect_interventions` | com oráculo | trama.series/tests/testthat/test-intervencao.R:258 deteccao no Nilo vs tsoutliers::tso; modelo ligado vs forecast::Arima |
| `series/forecast` | com oráculo | trama.series/tests/testthat/test-decompor.R:344 previsao, IC vs forecast::forecast(tslm) e forecast::Arima(xreg) |
| `series/intervencao` | com oráculo | trama.series/tests/testthat/test-intervencao.R:34 ajuste vs forecast::Arima(xreg); regressor vs tsoutliers::outliers.effects |
| `series/mann_kendall` | com oráculo | trama.series/tests/testthat/test-testar.R:664 S, Z e p vs trend::mk.test; hamed_rao vs modifiedmk::mmkh |
| `series/pettitt` | com oráculo | trama.series/tests/testthat/test-testar.R:1049 K e p vs trend::pettitt.test |
| `series/runs` | com oráculo | trama.series/tests/testthat/test-testar.R:930 estatistica e p vs randtests::runs.test |
| `series/zivot_andrews` | com oráculo | trama.series/tests/testthat/test-testar.R:374 estatistica e indice da quebra vs urca::ur.za |
| `spatial/anisotropy` | com oráculo | trama.spatial/tests/testthat/test-anisotropia-geor.R:33 gamma e pares vs geoR::variog4 por rotulo de direcao, 1e-8 (medido 5,6e-16); robusto e tendencia tambem. A FAIXA do envelope nao tem oraculo externo: testada por quantil a mao com simulador injetado |
| `spatial/indicator` | com oráculo | trama.spatial/tests/testthat/test-indicador.R:10 transformacao vs as.numeric(z <= corte) nos dois sentidos e no empate; a krigagem do indicador e a ordinaria, cujo oraculo e test-krigagem-oraculo.R |
| `spatial/kriging` | com oráculo | trama.spatial/tests/testthat/test-krigagem-oraculo.R:241 sistema resolvido a mao (Isaaks cap. 12, 1e-8) e geoR::krige.conv (1e-6); universal 1a/2a ordem e deriva externa vs geoR::krige.conv com trend.d/trend.l em test-universal-geor.R:52 e test-ked-geor.R:25 (1e-6) |
| `spatial/validation` | com oráculo | trama.spatial/tests/testthat/test-validacao-geor.R:41 predito, variancia e as quatro metricas vs geoR::xvalid com o modelo forcado, 1e-6 (medido 1,07e-14) |
| `spatial/variogram` | com oráculo | trama.spatial/tests/testthat/test-variograma-geor.R:30 gamma e pares vs geoR::variog nas mesmas classes, 1e-8 (medido ~1e-14): classico, robusto, direcional a 30 e 60 graus, tendencia de 1a e 2a ordem e por covariavel (esta em test-ked-geor.R:112) |
| `spatial/variogram_fit` | parcial | trama.spatial/tests/testthat/test-ajuste-oraculo.R alcance pratico vs raiz numerica de gama = 0,95 do patamar (2e-3) e Matern vs gstat::variogramLine (1e-6); contra geoR::variofit a concordancia e so de ORDEM DE GRANDEZA (0,35), porque os criterios minimizados sao proximos e nao identicos. A anisotropia geometrica tem oraculo determinístico: variogramLine com dir, razao 3,00 exata (test-ajuste-aniso.R:30) |
