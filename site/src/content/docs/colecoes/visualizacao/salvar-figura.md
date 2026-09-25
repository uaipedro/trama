---
title: Salvar figura
description: Grava o gráfico ou o painel em PDF, PNG ou TIFF, no tamanho e na resolução do periódico.
section: colecoes
collection: visualizacao
node: view/save
category: figura
related: [view/combine, data/write_csv]
---

## O que o bloco faz

`view/save` grava o gráfico que recebe num arquivo e o repassa adiante, sem mudança. O tamanho é dado em milímetros e a resolução em dpi, como nas instruções aos autores.

## Quando usar

Use no fim do fluxo que produz uma figura de artigo, relatório ou tese. Como o nó grava a cada execução, a figura no disco acompanha o dado e a análise.

## Configuração

Arquivo é o caminho de saída; relativo, parte da pasta do projeto, e a pasta precisa existir. Em branco, o nó não grava nada. Sem extensão, o arquivo recebe a do formato. Formato aceita `pdf` (vetorial, preferido para pontos e linhas), `png` e `tiff` (com compressão LZW). Largura (mm) vem em 170, a largura de página comum; uma coluna tem por volta de 85 mm. Altura (mm) 0 segue a proporção escolhida no gráfico. Resolução (dpi) vale para PNG e TIFF: 300 para figura colorida, 600 ou mais para desenho de linhas.

## Exemplo

```r
library(trama.view)
p <- tr_boxplot(mtcars, x = "cyl", y = "mpg", tema = "clássico", aspecto = "4:3")
tr_save(p, "figura1.tiff", formato = "tiff", largura_mm = 85, dpi = 600)
```

## Como interpretar

Um PNG ou TIFF tem largura em pixels igual a largura em mm × dpi / 25,4, arredondada: 170 mm a 300 dpi dão 2008 px. O texto mantém o tamanho em pontos do tema, então a mesma figura gravada em 85 mm tem letras proporcionalmente maiores em relação ao desenho, que é o ajuste certo para uma coluna.
