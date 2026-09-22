---
title: Tamanho por grupos
description: Quantas entrevistas no total para que CADA grupo de perfil tenha a margem desejada, sem cotas?
section: colecoes
collection: amostragem
node: sampling/size_domains
related: [sampling/detectable_difference, sampling/size_proportion, sampling/rake]
---

## O que o bloco faz

O tamanho da amostra quando a margem tem de valer DENTRO de cada grupo de
perfil (cada sexo, cada raça/cor, cada faixa de renda) e não há cotas.

Para ±5 pontos, cada grupo precisa de uns 385 respondentes. Sem cota, o grupo
aparece na coleta na proporção em que existe na população, e o total tem de ser
385 ÷ participação. Quem manda no tamanho da pesquisa é o MENOR grupo que se
quer ler: com um grupo de 2%, o total passa de 19 mil. A escada do card mostra
o grupo limitante; a vista `alocação` e a tabela, o que cada grupo exigiria e a
margem que ele teria no total escolhido.

Dois alertas que a conta não resolve:

- **Grupos muito pequenos** (indígenas e amarelos, em muitos recortes) exigem
  totais inviáveis. **Ignorar grupos abaixo de** os tira da conta, e a nota diz
  quais ficaram fora: a pesquisa não terá margem para eles, e isso deve ir para
  o relatório.
- A conta supõe que a coleta traga cada grupo na proporção da população. Coleta
  aberta costuma atrair uns grupos mais que outros; a composição de uma coleta
  anterior (ou do piloto) é melhor que a do Censo, quando existe.

## Quando usar

Use quando a margem precisa ser alcançada dentro de cada grupo de perfil, sem impor cotas na coleta.

## Configuração

- **Variável**, **Grupo**, **Participação** — colunas da tabela longa de
  composição (a participação soma 1 dentro de cada variável).
- **Margem em cada grupo** — em proporção (0,05 = 5 pontos).
- **Proporção esperada**, **Confiança**, **Efeito do desenho**, **Taxa de
  resposta** — como em `sampling/size_proportion`.
- **Ignorar grupos abaixo de** — participação mínima para entrar na conta.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("perfil", "sampling/example", dataset = "perfil_escolas") |>
  tr_add("plano", "sampling/size_domains", variavel = "variavel", grupo = "categoria",
         participacao = "participacao", erro = 0.05, from = "perfil")
```

## Como interpretar

A alocação identifica o grupo limitante e o n total necessário para atingir a margem dentro dos grupos considerados. Grupos abaixo do corte configurado são excluídos e aparecem como não considerados. A saída é Plano `sampling/plan` com n total e alocação diagnóstica por grupo.

## Veja também

`sampling/detectable_difference`, `sampling/size_proportion`, `sampling/rake`.
