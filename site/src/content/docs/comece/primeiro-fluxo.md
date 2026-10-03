---
title: Primeiro fluxo
description: Crie uma análise pequena com uma tabela de exemplo, uma inspeção e um gráfico.
section: comece
order: 1
related: [data/read, data/summary, view/points]
---

Ao final desta página você terá um fluxo de três blocos: uma tabela de
exemplo (`mtcars`), um resumo das colunas e um gráfico de dispersão do
consumo (`mpg`) contra o peso (`wt`), com os pontos coloridos pelo número de
cilindros (`cyl`).

```r
tr_flow(reg) |>
  tr_add("carros", "data/example", dataset = "mtcars") |>
  tr_add("resumo", "data/summary", from = "carros") |>
  tr_add("grafico", "view/points", x = "wt", y = "mpg", cor = "cyl", from = "carros")
```

O canvas acima é o mesmo fluxo que você vai montar no editor; a aba ao lado
mostra o código R equivalente.

**Já instalou o trama?** Abra o editor e siga a partir de
[Finalidade](#finalidade).
**Ainda não?** Comece pela [Instalação](/trama/por-dentro/instalacao/) e volte
aqui com o editor aberto.

## Finalidade

Um fluxo organiza operações em uma sequência explícita. Cada bloco recebe um
resultado, produz outro e deixa esse resultado disponível para inspeção.

> **Antes de continuar**
>
> Um bloco não é uma versão “mais simples” de uma função R. Ele representa uma
> função configurada dentro de um fluxo: entradas, parâmetros e saída ficam
> visíveis no mesmo lugar.

## Um primeiro caminho

No editor, acrescente os blocos **Dados de exemplo**,
**Resumo** e **Disperso**. Ligue a saída de **Dados de exemplo** à entrada
de **Resumo** e à entrada de **Disperso**.

Escolha `mtcars` em **Dados de exemplo**. Em **Disperso**, use `wt` no eixo X,
`mpg` no eixo Y e `cyl` em **Cor por**. O gráfico mostra uma marca por linha da
tabela.

## Como conferir o caminho

O card de cada bloco mostra o resultado correspondente. O **Resumo** descreve
os tipos e faltantes das colunas; o gráfico usa a tabela que chega pela conexão.
Mudar um parâmetro recalcula apenas os blocos afetados adiante no fluxo.

> **Antes de continuar**
>
> Começar com uma tabela pequena serve para separar duas perguntas: se o fluxo
> está montado como esperado e se os dados reais estão preparados para a mesma
> análise. A primeira pode ser respondida antes de escolher um arquivo.
