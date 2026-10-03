---
title: Disperso com rótulos
description: Posiciona um nome junto de cada observação num gráfico de duas medidas.
section: colecoes
collection: visualizacao
node: view/labels
category: relacao
related: [view/points, view/dotplot, data/filter]
---

## O que o bloco faz

`view/labels` posiciona um nome junto de cada observação num gráfico de duas medidas.

## Quando usar

Use para identificar poucas observações relevantes em uma nuvem.

## Configuração

Eixo X, Eixo Y e Rótulo são obrigatórios. Cor por é opcional. Omitir rótulos sobrepostos (padrão TRUE) conserva o ponto e omite o texto que colidir; a ordem das linhas determina prioridade. Eixo em log e Painéis por seguem as regras do Disperso.

## Exemplo

```r
library(trama.view)
dados <- data.frame(renda = c(2, 4, 6, 8), populacao = c(7, 5, 9, 4),
                    municipio = c("Alfa", "Beta", "Gama", "Delta"))
tr_labels(dados, x = "renda", y = "populacao", rotulo = "municipio")
```

## Como interpretar

A posição de cada ponto codifica as duas medidas e o texto identifica a observação. Se dois rótulos se sobrepõem e a omissão está ligada, o ponto permanece, mas o nome pode não aparecer; a primeira linha da tabela tem prioridade. Use poucos pontos para que nomes e coordenadas continuem legíveis.
