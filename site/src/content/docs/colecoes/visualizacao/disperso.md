---
title: Disperso
description: Mostre a relação entre duas medidas com uma marca por linha da tabela.
section: colecoes
collection: visualizacao
node: view/points
category: relacao
order: 2
related: [view/line, view/bin2d, view/labels, data/group_summarise]
---

## O que o bloco faz

`view/points` representa cada linha da tabela como um ponto nas coordenadas de duas colunas. Cor por distingue grupos sem alterar a quantidade de pontos. O nó não agrega as observações.

## Quando usar

Use para examinar associação, concentração e observações afastadas entre duas medidas. Cada ponto representa a unidade descrita por uma linha; identifique essa unidade antes de interpretar a nuvem.

## Configuração

**Eixo X** e **Eixo Y** são obrigatórios. **Cor por** é opcional: texto ou fator produz grupos discretos, enquanto número produz uma escala contínua. **Tendência** aceita `nenhuma`, `linear` ou `suave`; ambas as linhas de tendência incluem uma faixa de confiança de 95%, não uma faixa que contenha as observações. **Eixo em log** aceita `nenhum`, `X`, `Y` ou `ambos` e exige valores positivos. **Painéis por** divide os grupos em painéis com a mesma escala.

## Exemplo

```r
library(trama.view)
dados <- data.frame(qtd = c(2, 4, 5, 7, 9, 10), valor = c(8, 12, 11, 18, 21, 24),
                    regiao = c("Norte", "Sul", "Norte", "Sul", "Norte", "Sul"))
tr_points(dados, x = "qtd", y = "valor", cor = "regiao", tendencia = "linear")
```

## Como interpretar

A posição mostra os valores das duas medidas para cada linha. Sobreposição intensa impede distinguir quantas observações ocupam uma região; `view/bin2d` conta essas observações por célula. `linear` ajusta uma reta e `suave` uma curva local; são resumos visuais, não testes de hipótese. Com dezenas de milhares de linhas, agregue ou use a grade antes de interpretar densidade.

## Veja também

`view/line` conecta observações quando X tem ordem; `view/labels` identifica pontos; `view/bin2d` mostra densidade em nuvens grandes; `data/group_summarise` agrega antes do gráfico quando cada marca deve representar um grupo.
