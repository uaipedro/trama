---
title: Declarar desenho
description: Diz quais colunas de uma amostra já coletada são o peso, o estrato e o conglomerado.
section: colecoes
collection: amostragem
node: sampling/design
related: [sampling/poststratify, sampling/rake, sampling/proportion, sampling/ratio]
---

## O que o bloco faz

Transforma a base de uma pesquisa JÁ COLETADA em amostra com desenho. É a porta
de entrada de microdado de pesquisa oficial, que vem com as colunas do
desenho: o peso, o estrato e a unidade primária de amostragem (UPA — o setor, a
escola, o município).

Sem declarar o desenho, uma média com `data/group_summarise` sai com o peso
errado, e qualquer erro padrão sai com a variância de uma AAS que a pesquisa
nunca foi — em conglomerados, subestimado pela metade ou mais.

- **Peso** — obrigatório com conglomerado. Sem conglomerado, pode faltar se a
  **população** do estrato for dada: o peso vira N_h/n_h (AAS estratificada).
- **Estrato** — em branco, sem estratos.
- **Conglomerado** — em branco, cada linha é a sua própria UPA.
- **UPAs do estrato na população** — o número de UPAs (ou de unidades, sem
  conglomerado) no estrato, constante dentro dele. Dá a correção de população
  finita no primeiro estágio; em branco, a variância é a com reposição, um
  pouco conservadora — e é o que as pesquisas oficiais costumam recomendar.

A amostra declarada não guarda a população, então não se simula
(`sampling/simulate` recusa).

## Quando usar

Use para analisar microdados já coletados quando a documentação da pesquisa informa pesos, estratos e unidades primárias.

## Configuração

- **Peso**, **Estrato**, **Conglomerado (UPA)**, **UPAs do estrato na
  população** — colunas da tabela.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pesquisa", "sampling/example", dataset = "domicilios") |>
  tr_add("desenho", "sampling/design", pesos = "peso", estrato = "estrato",
         conglomerado = "setor", populacao = "setores_estrato", from = "pesquisa") |>
  tr_add("estimativa", "sampling/mean", variavel = "renda", por = "estrato", from = "desenho")
```

## Como interpretar

A saída associa cada linha a peso, estrato e UPA conforme as colunas informadas. Sem coluna de UPA, cada linha representa sua própria unidade primária; sem cadastro populacional, o desenho declarado não pode ser simulado. A saída é Amostra `sampling/sample` com desenho declarado.

## Veja também

`sampling/poststratify`, `sampling/rake`, `sampling/proportion`, `sampling/ratio`.
