# Comparar modelos no mesmo teste

Instale os motores opcionais antes de executar todos os ramos:

```r
install.packages(c("figsr", "rpart", "ranger", "e1071", "xgboost"))
```

Da raiz do repositório, execute `Rscript exemplos/machine-learning/app.R`.
O fluxo `main` compara os seis métodos em iris binária (versicolor/virginica).
O fluxo `regressao` prevê mpg em mtcars usando wt, hp e disp. Cada fluxo usa
uma única divisão de treino/teste para todos os modelos. Os dados são pequenos
e didáticos: diferenças de métricas não estabelecem um vencedor universal.

O fluxo `cart-vs-figs` isola os dois métodos interpretáveis (uma árvore vs.
soma de poucas árvores) e acrescenta **Ler regras das árvores** a cada um —
o ponto ali não é a métrica, é comparar a FORMA da explicação: uma árvore só
contra a soma das contribuições de árvores pequenas.

O card de CART/FIGS mostra as regras. Adicione **Ler regras das árvores** para
exportá-las ou **Importância de variáveis** para inspecionar os motores de árvore.
Os exemplos são regeneráveis por `Rscript exemplos/machine-learning/gerar.R`.
