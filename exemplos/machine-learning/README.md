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
exportá-las ou **Importância** (`models/importance`) para inspecionar os motores
de árvore.

Os modelos da ml são `models/fit`: prever, avaliar, a matriz de confusão e a ROC
são os blocos da coleção **Modelos** (`trama.models`), que por isso vem carregada
junto. A tabela prevista traz `previsto` e, na classificação, `prob_<classe>`
(antes `.pred` e `.prob_<classe>`). Os fluxos gravados ainda têm os blocos antigos
da ml; ao abrir, a migração os troca pelos da models. Um bloco a jusante que lia
`.pred` ou `variavel` pelo nome precisa do nome novo (`previsto`, `termo`).

`Rscript exemplos/machine-learning/verificar.R` abre, roda e confere os três
fluxos. `gerar.R` regenera `regressao` e `cart-vs-figs` (o `main` foi editado à
mão depois e é sobrescrito por ele).
