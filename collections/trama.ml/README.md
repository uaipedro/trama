# trama.ml

Aprendizado supervisionado no Trama, começando por modelos simples e árvores
interpretáveis. Redes neurais ficam para outra coleção.

| Modelo | Motor | Regressão | Classificação | Leitura |
|---|---|---|---|---|
| Linear / logística | stats | Sim | Binária | Coeficientes |
| CART | rpart | Sim | Binária e multiclasse | Caminho até uma folha |
| FIGS | figsr | Sim | Binária | Soma de poucas árvores |
| Random forest | ranger | Sim | Binária e multiclasse | Importância das variáveis |
| SVM | e1071 | Sim | Binária e multiclasse | Margem e kernel |
| XGBoost | xgboost | Sim | Binária e multiclasse | Importância por ganho |

## Instalar e abrir

```r
pak::pak("uaipedro/trama/collections/trama.ml")
# Instale apenas os motores que vai usar:
install.packages(c("rpart", "figsr", "ranger", "e1071", "xgboost"))
library(trama)
tr_app(tr_project("meu-ml", collections = c("trama.data", "trama.view", "trama.models", "trama.ml")))
```

Na paleta: **Dados para aprender → Separar treino / teste**. Ligue treino ao
modelo escolhido. Os ajustes saem como `models/fit`, o modelo da coleção
`trama.models`, e prever e avaliar é com os blocos de lá: em **Prever**
(`models/predict`), conecte o modelo e a tabela de teste; a saída ganha
`previsto` e, na classificação, `prob_<classe>`. **Avaliar previsões**,
**Matriz de confusão**, **Curva ROC** e **Importância** também são da models, e
aceitam o modelo direto (com o teste em `dados`; sem ele, medem por validação
cruzada de 5 folds no treino). CART e FIGS mostram regras no card; **Ler
regras das árvores** as disponibiliza como tabela.

## FIGS no console

```r
library(trama.ml)
d <- tr_ml_split(tr_ml_example("iris_binaria"), resposta = "Species", seed = 42)
m <- tr_ml_figs(d$treino, resposta = "Species", max_splits = 6)
p <- trama.models::tr_models_predict(m, d$teste)
trama.models::tr_models_evaluate(m, d$teste)
trama.models::tr_models_confusion(m, d$teste)
tr_ml_rules(m)
trama.models::tr_models_importance(m)
```

No CART, siga as condições até uma folha e leia sua previsão. No FIGS,
encontre uma folha **em cada árvore** e some as contribuições. Na classificação,
a soma é limitada a [0, 1] e corresponde à segunda classe em `m$niveis`.
Um orçamento pequeno de divisões facilita a leitura. O ajuste original fica
em `m$ajuste`; motor, versão e hiperparâmetros ficam em `m$extras`.

## Escopo e avaliação

São 13 blocos: dados, divisão, seis modelos, tuning, regras e três
visualizadores (árvore, histórico do tuning, resíduos); prever, avaliar, a
confusão, a ROC e a importância são os blocos da `trama.models`. **Ajustar
hiperparâmetros** usa validação cruzada somente nas linhas recebidas, devolve o
modelo vencedor reajustado e o histórico completo; preserve o teste para a
avaliação final. **Visualizar árvores** desenha CART ou uma árvore do FIGS;
há também histórico do tuning e resíduos. A primeira versão aceita
preditores numéricos. Para
classes codificadas com números, converta a fator ou escolha
`tarefa = "classificacao"`. FIGS e logística suportam duas classes.

Regressão produz MAE, RMSE e R²; classificação produz acurácia, acurácia
balanceada e macro F1, com médias macro sobre as classes observadas.
R² é indefinido quando a resposta é constante. Faltantes não são descartados
em silêncio. A importância de variáveis depende do motor; não compare suas
magnitudes entre modelos, nem a interprete como causalidade.

A divisão aleatória pressupõe linhas independentes. Para tempo, grupos ou
medidas repetidas, separe os dados por tempo/grupo antes do ajuste. A SVM
aprende escalas somente no treino. Imputação, codificação e seleção de
variáveis também devem ser aprendidas no treino e reaplicadas ao teste.
Esta versão recusa faltantes, infinitos e preditores não numéricos; não faz
imputação, validação cruzada ou busca automática de hiperparâmetros.

**Avaliar** mede as linhas recebidas: não deduz se você conectou treino ou
teste. Reserve o teste para o resultado final; selecionar hiperparâmetros
usando repetidamente o teste produz uma estimativa otimista.

## Créditos

O pacote **figsr** é de João Paulo Assis Bonifácio, Geraldo Magela da Cruz
Pereira, Pedro Mambelli Fernandes e João Vitor Andrade Alves de Souza.
O método FIGS é de Tan et al., *Fast Interpretable Greedy-Tree Sums*,
[DOI 10.1073/pnas.2310151122](https://doi.org/10.1073/pnas.2310151122).
O pacote e o método recebem créditos separados.

Veja [REFERENCES.md](REFERENCES.md), a ajuda de cada bloco e
`citation("figsr")` (ou o nome do outro motor) para as referências de software
e dos algoritmos. A coleção chama os pacotes; não copia suas implementações.
