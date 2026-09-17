# Fontes e referências de `trama.ml`

Documento de referência da coleção inicial de modelos supervisionados. As referências de software apontam para a documentação primária no CRAN; as de método apontam para o artigo ou livro que introduziu o algoritmo. Consultado em 17 de setembro de 2026.

## Escopo e leitura da interpretabilidade

Esta coleção começa com modelos clássicos de regressão e classificação. A interpretabilidade é uma propriedade do modelo ajustado e de sua apresentação, não uma garantia de que as relações estimadas sejam causais. Uma árvore CART é fácil de seguir como regras; FIGS preserva essa leitura somando poucas árvores; florestas aleatórias e XGBoost são conjuntos de árvores e, portanto, exigem resumos ou visualizações para interpretação global. SVM pode ser linear e relativamente direto, mas kernels não lineares tornam a função menos transparente. Todos os modelos continuam sujeitos a extrapolação, viés de amostragem, vazamento de informação e instabilidade quando os dados mudam.

## Modelos e APIs R

### FIGS (`figsr`)

`figs(formula, data, max_splits = 10, max_trees = NULL, min_n = 5, mode = "regression", ...)` ajusta Fast Interpretable Greedy-Tree Sums para regressão ou classificação binária. O algoritmo escolhe gananciosamente, a cada passo, entre abrir uma árvore e aprofundar uma árvore existente, sempre reduzindo a impureza residual. `summary()` imprime as regras, `plot()` mostra as árvores, `predict()` produz previsões e `figsr_importance()` agrega a redução de erro por variável. `bagging_figs()` faz bootstrap de vários FIGS e melhora estabilidade, ao custo de um conjunto de regras menos simples. A integração com `parsnip` usa `figs_tree(...) |> set_engine("figsr")`.

Limitações documentadas: classificação apenas binária; sem pesos de caso; preditores fatoriais com mais de dez níveis em um nó são ignorados; valores ausentes são removidos no ajuste e provocam erro na predição; não há surrogate splits. O limite `max_splits` deve ser exposto na coleção como controle explícito da troca entre complexidade e ajuste.

Fonte do pacote: [CRAN `figsr`](https://CRAN.R-project.org/package=figsr), [manual de referência](https://cran.r-universe.dev/figsr/doc/manual.html) e [vignette introdutória](https://cran.mirror.garr.it/mirrors/CRAN/web/packages/figsr/vignettes/figsr-intro.html). O pacote é de João Paulo Assis Bonifácio, Geraldo Magela da Cruz Pereira, Pedro Mambelli Fernandes e João Vitor Andrade Alves de Souza (versão 0.1.1; licença MIT; DOI do pacote: `10.32614/CRAN.package.figsr`).

Fonte do método: TAN, Yan Shuo; SINGH, Chandan; NASSERI, Keyan; AGARWAL, Abhineet; DUNCAN, James; RONEN, Omer; EPLAND, Matthew; KORNBLITH, Aaron; YU, Bin. **Fast Interpretable Greedy-Tree Sums**. *Proceedings of the National Academy of Sciences*, 122(7), e2310151122, publicado em 2025. DOI: [10.1073/pnas.2310151122](https://doi.org/10.1073/pnas.2310151122). A ficha editorial do [PubMed](https://pubmed.ncbi.nlm.nih.gov/39951504/) confirma a publicação em 18 de fevereiro de 2025, volume, número e artigo; o [texto integral no PMC](https://pmc.ncbi.nlm.nih.gov/articles/PMC11848335/) registra recebimento em 2023 e aceite em 2024. Portanto, 2023 é o ano de submissão (e o identificador `e2310151122` é o número do artigo), não o ano correto para citar a publicação. O preprint anterior pode ser citado separadamente como [arXiv:2201.11931](https://arxiv.org/abs/2201.11931). O artigo apresenta CART como caso de árvore única, FIGS como soma adaptativa, G-FIGS para grupos e Bagging-FIGS para redução de variância. Não atribuir o artigo aos autores do pacote: são referências distintas.

### CART / árvore de decisão (`rpart`)

`rpart(formula, data, method = "class"|"anova", control = rpart.control(...))` ajusta árvores para classificação, regressão e sobrevivência. `predict()`, `plot()`/`text()`, `printcp()` e `prune()` permitem inspecionar e podar o modelo; `cp` é o parâmetro de complexidade usado na poda. CART é a opção mais diretamente legível da coleção, mas uma árvore muito profunda fica difícil de auditar e uma árvore única pode ter viés contra estruturas aditivas.

Fonte do software: [CRAN `rpart`](https://CRAN.R-project.org/package=rpart), Therneau, Terry; Atkinson, Beth. **rpart: Recursive Partitioning and Regression Trees**. R package version 4.1.27 (2026). A implementação cobre a maior parte da funcionalidade descrita no livro de CART.

Fonte do algoritmo: BREIMAN, Leo; FRIEDMAN, Jerome H.; OLSHEN, Richard A.; STONE, Charles J. **Classification and Regression Trees**. Wadsworth, 1984. ISBN 978-0412048418. `rpart` é uma implementação R relacionada a esse método; não afirmar que reproduz necessariamente todas as escolhas do livro.

### Random forest (`ranger`)

`ranger(dependent.variable.name = NULL, data, num.trees = 500, mtry = NULL, importance = "none", probability = FALSE, ...)` ajusta florestas rápidas para classificação, regressão e sobrevivência; `predict()` produz classes, valores, probabilidades ou riscos conforme o tipo. `variable.importance` é disponibilizado quando solicitado. O modelo reduz variância pela média de árvores construídas em amostras bootstrap e subconjuntos de variáveis, mas a floresta inteira não é uma árvore legível; importância por permutação pode ser enviesada quando há variáveis correlacionadas.

Fonte do software: [CRAN `ranger`](https://CRAN.R-project.org/package=ranger) e [manual da API](https://cran.r-universe.dev/ranger/ranger.pdf). Cite WRIGHT, Marvin N.; ZIEGLER, Andreas. **ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R**. *Journal of Statistical Software*, 77(1), 1–17, 2017. DOI: [10.18637/jss.v077.i01](https://doi.org/10.18637/jss.v077.i01).

Fonte do algoritmo: BREIMAN, Leo. **Random forests**. *Machine Learning*, 45, 5–32, 2001. DOI: [10.1023/A:1010933404324](https://doi.org/10.1023/A:1010933404324).

### Support vector machine (`e1071`)

`e1071::svm(formula, data, type = NULL, kernel = "radial", cost = 1, gamma = if (is.vector(x)) 1 else 1/ncol(x), probability = FALSE, ...)` cobre classificação, regressão (`eps-regression`/`nu-regression`) e one-class SVM. `predict()` faz previsões; `tune()` ou `tune.svm()` pode selecionar hiperparâmetros como `cost`, `gamma` e `epsilon`. Escalonamento dos preditores é normalmente necessário para kernels e deve ser parte explícita do fluxo de treino, sem calcular parâmetros usando o conjunto de teste.

Fonte do software: [CRAN `e1071`](https://CRAN.R-project.org/package=e1071) e a vignette [Support Vector Machines—the Interface to libsvm](https://CRAN.R-project.org/package=e1071). O pacote é de Meyer, Dimitriadou, Hornik, Weingessel e Leisch e incorpora `libsvm` de Chang e Lin; cite o pacote e, quando pertinente, a implementação subjacente.

Fonte do método: CORTES, Corinna; VAPNIK, Vladimir. **Support-vector networks**. *Machine Learning*, 20, 273–297, 1995. DOI: [10.1007/BF00994018](https://doi.org/10.1007/BF00994018). Para a implementação libsvm: CHANG, Chih-Chung; LIN, Chih-Jen. **LIBSVM: A library for support vector machines**. *ACM TIST*, 2(3), 2011. DOI: [10.1145/1961189.1961199](https://doi.org/10.1145/1961189.1961199).

### XGBoost (`xgboost`)

`xgb.train(params, data, nrounds, watchlist = list(), ...)` é a interface estável recomendada pela documentação para código de pacote; `xgboost()` é a interface de alto nível para os casos comuns e `xgb.DMatrix()` representa os dados. `predict()`, `xgb.importance()` e `xgb.cv()` apoiam previsão, inspeção e escolha de rodadas. É gradient boosting de árvores (ou solver linear), com excelente desempenho e custo de interpretação maior que CART/FIGS; profundidade, regularização e número de rodadas controlam a complexidade e o sobreajuste.

Fonte do software: [CRAN `xgboost`](https://CRAN.R-project.org/package=xgboost), [documentação da API R](https://xgboost.readthedocs.io/en/stable/r_docs/R-package/docs/index.html) e a referência primária de [`xgb.train`](https://xgboost.readthedocs.io/en/latest/r_docs/R-package/docs/reference/xgb.train.html). `xgb.train()` recebe um `xgb.DMatrix`, `params` e `nrounds`; a documentação recomenda essa interface mais estável para código de pacote e reserva `xgboost()` para uso interativo. Fonte do método: CHEN, Tianqi; GUESTRIN, Carlos. **XGBoost: A scalable tree boosting system**. *Proceedings of the 22nd ACM SIGKDD*, 2016, 785–794. DOI: [10.1145/2939672.2939785](https://doi.org/10.1145/2939672.2939785).

### Modelos lineares base (`stats`)

Para a base comparável da coleção, `stats::lm(formula, data, ...)` ajusta
regressão linear por mínimos quadrados e `stats::glm(formula, family, data,
...)` generaliza o preditor linear a famílias como `binomial()` (logística).
`predict()` e `summary()` são as interfaces de previsão e inspeção. As fontes
primárias da API são a [documentação oficial de `lm`](https://search.r-project.org/R/refmans/stats/html/lm.html)
e a [documentação oficial do pacote `stats`](https://search.r-project.org/R/refmans/stats/html/00Index.html).
Para a fundamentação histórica, cite Chambers (1992), “Linear Models”, em
*Statistical Models in S*, e Nelder & Wedderburn (1972), **Generalized Linear
Models**, *Journal of the Royal Statistical Society B*, 135–153, DOI
[10.1111/j.2517-6161.1972.tb00832.x](https://doi.org/10.1111/j.2517-6161.1972.tb00832.x).

## Convenção de citação

Em exemplos e documentação, cite sempre o pacote efetivamente usado (nome, versão e DOI do CRAN quando disponível) e a fonte primária do algoritmo. Para FIGS, registre separadamente `figsr` e Tan et al. (2025); se mencionar o preprint, indique explicitamente arXiv (2022). Para SVM, registre `e1071`, Cortes & Vapnik (1995) e, se o resultado depender da biblioteca, Chang & Lin (2011). Registre também a versão de R e os hiperparâmetros, pois defaults e APIs podem mudar.
