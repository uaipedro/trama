---
title: Primeiro fluxo
description: Crie uma análise pequena com uma tabela de exemplo, uma inspeção e um gráfico.
section: comece
order: 1
related: [data/read_csv, data/summary, view/points]
---

## Instalação

### Antes de instalar

O caminho mais simples usa o `trama-cli`. Ele precisa do **Node.js 20 ou
superior**; confira no terminal com `node --version`. Se o comando não existir
ou mostrar uma versão anterior, instale uma versão atual pelo
[site do Node.js](https://nodejs.org/).

Você não precisa instalar R separadamente: o `trama-cli` baixa uma versão
portátil e prepara o núcleo do Trama junto com as coleções Dados e
Visualização.

### Instale e crie um projeto

```bash
npm install -g @uaipedro/trama-cli
trama create meu-primeiro-fluxo
```

O segundo comando cria a pasta do projeto, pergunta se você quer acrescentar
outras coleções e abre o editor no navegador. Para este guia, siga sem marcar
coleções extras: Dados e Visualização já estão disponíveis.

## Finalidade

Um fluxo organiza operações em uma sequência explícita. Cada bloco recebe um
resultado, produz outro e deixa esse resultado disponível para inspeção.

> **Antes de continuar**
>
> Um bloco não é uma versão “mais simples” de uma função R. Ele representa uma
> função configurada dentro de um fluxo: entradas, parâmetros e saída ficam
> visíveis no mesmo lugar.

## Um primeiro caminho

No editor que acabou de abrir, acrescente os blocos **Dados de exemplo**,
**Resumo** e **Disperso**. Ligue a saída de um bloco à entrada do próximo.

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
