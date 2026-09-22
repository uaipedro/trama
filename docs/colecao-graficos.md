# Coleção Gráficos

A coleção `trama.view` produz gráficos `ggplot2` a partir de tabelas. A
escolha do nó depende da estrutura da pergunta e das colunas disponíveis.
Totais, médias e outras medidas calculadas são obtidos antes com
`data/group_summarise`.

## Escolha do gráfico

| Pergunta | Nó | Estrutura de dados |
|---|---|---|
| Duas medidas variam juntas? | `view/points` | Uma linha por observação |
| Há muitos pontos sobrepostos? | `view/bin2d` | Uma linha por observação |
| Como uma medida evolui? | `view/line` | Eixo X ordenado |
| Como o total se compõe? | `view/area` | Uma linha por período e grupo |
| Como uma medida se distribui? | `view/histogram` ou `view/density` | Coluna numérica |
| Como grupos diferem? | `view/boxplot` ou `view/violin` | Categoria e medida |
| Quais categorias têm maior valor? | `view/bars` ou `view/dotplot` | Categoria e medida |
| Médias diferem com incerteza? | `view/means` | Categoria e observações |
| Há mudança na mesma unidade? | `view/paired` | Condição, medida e identificador |
| Quais causas acumulam maior parcela? | `view/pareto` | Categoria e valor |

## Relação e evolução

`view/points` representa uma linha da tabela por marca. Ele é usado para
examinar associação, dispersão e observações extremas entre duas medidas.
`Cor por` separa grupos sem alterar a quantidade de linhas.

```r
tr_flow(reg) |>
  tr_add("nuvem", "view/points", x = "idade", y = "renda",
         cor = "regiao", tendencia = "linear", from = "dados")
```

`view/bin2d` divide o plano em células e colore cada uma pela contagem. Ele é
adotado quando a sobreposição de pontos oculta a densidade. `view/labels`
identifica pontos por texto e convém para conjuntos pequenos ou filtrados.

`view/line` requer um eixo X com ordem, como data, tempo, dose ou tamanho.
Uma linha por grupo é obtida com `Cor por`. `view/area` empilha grupos e
mostra a composição do total; séries acima da base não compartilham uma linha
de referência comum, mas o total e a série inferior permanecem comparáveis.

```r
tr_flow(reg) |>
  tr_add("mensal", "data/group_summarise", by = "mes, regiao",
         name = "receita", expr = "sum(valor, na.rm = TRUE)", from = "dados") |>
  tr_add("evolucao", "view/line", x = "mes", y = "receita", cor = "regiao",
         from = "mensal")
```

`view/heatmap` usa cor para representar uma medida em combinações de duas
variáveis. Ele é adequado a matrizes de período por grupo ou categoria por
categoria.

## Distribuição

`view/histogram` agrupa valores em faixas e mostra contagens. Poucas classes
suavizam irregularidades, mas muitas classes produzem contagens instáveis quando
a amostra é pequena. `view/density` estima uma curva suavizada e depende do
parâmetro de suavidade.

`view/boxplot` mostra mediana, quartis e valores extremos. `view/violin`
mostra também a forma aproximada da distribuição. `view/strip` preserva
observações individuais e reduz sobreposição por deslocamento. `view/ecdf`
mostra a proporção de observações até cada valor. `view/qq` compara quantis
observados aos de uma normal e é usado na inspeção visual de resíduos.

## Comparação

`view/bars` usa a altura para contagem, quando o eixo Y está em branco, ou
para um valor fornecido. Média não deve ser obtida pela altura de barras sobre
dados brutos, uma vez que a agregação padrão é soma; a média é calculada antes
com `data/group_summarise` ou estimada com `view/means`.

```r
tr_flow(reg) |>
  tr_add("medias", "data/group_summarise", by = "regiao",
         name = "valor_medio", expr = "mean(valor, na.rm = TRUE)", from = "dados") |>
  tr_add("barras", "view/bars", x = "regiao", y = "valor_medio",
         from = "medias")
```

`view/means` calcula média e barra de incerteza por grupo. `view/dotplot`
ordena valores numa escala comum. `view/dumbbell` liga dois valores resumidos
por categoria. `view/paired` liga observações da mesma unidade entre
condições e requer um identificador. `view/pareto` ordena categorias e mostra
o percentual acumulado.

## Preparação da tabela

Dados mensais com várias linhas por mês e região são resumidos antes de
`view/line`, `view/area`, `view/bars` ou `view/heatmap` quando o desenho
representa uma medida agregada. Em contraste, `view/points`, `view/strip` e
`view/paired` preservam observações individuais.

Códigos numéricos de grupo devem ser convertidos para texto ou fator antes de
serem usados em `Cor por`, pois uma escala contínua sugere uma ordem
quantitativa inexistente. `data/convert` realiza a conversão. Faltantes são
inspecionados com `data/summary`, pois a remoção de pontos altera a base
exibida.

## Aparência comum

Todos os gráficos usam **Proporção**, **Tema**, **Título**, **Rótulo do X**,
**Rótulo do Y** e **Legenda**. A proporção altera a área disponível para eixos,
rótulos e legenda. O tema é definido pelo projeto. Títulos e rótulos em branco
adotam os nomes das colunas quando eles descrevem adequadamente a medida.
