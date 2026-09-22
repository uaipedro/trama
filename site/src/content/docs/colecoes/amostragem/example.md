---
title: Exemplo de amostragem
description: Carrega uma população simulada com a verdade conhecida, ou uma amostra já coletada.
section: colecoes
collection: amostragem
node: sampling/example
related: [sampling/srs, sampling/design]
---

## O que o bloco faz

Carrega um conjunto pensado para ensinar amostragem. Os dois primeiros são
POPULAÇÕES inteiras — o cadastro de onde se sorteia —, simuladas com estruturas
plantadas, para que a estimativa possa ser conferida contra a verdade.

- **fazendas** — SIMULADO. 2.400 fazendas (`fazenda`) em 4 regiões (`regiao`)
  e 120 municípios (`municipio`, 20 fazendas cada), com `area_ha`,
  `producao_t`, `irrigada` (sim/não) e `trabalhadores`. As regiões foram
  plantadas diferentes em ESCALA: o Norte tem 300 fazendas enormes e muito
  variáveis, o Sul 900 pequenas. Estratificar por região ganha muito, e o
  Neyman manda amostra para o Norte. A produção cresce com a área (correlação
  ≈ 0,9): PPS pela área ganha ainda mais. Os municípios têm efeito próprio:
  sortear municípios inteiros perde.
- **estratos_fazendas** — a tabela de estratos das `fazendas`: `regiao`, `N`
  (fazendas), `desvio_producao` (o desvio da produção na região) e `custo` por
  entrevista (o Norte é longe: 60; o Sul, 20). É a entrada do
  `sampling/size_stratified` e os totais do `sampling/poststratify`.
- **escolas** — SIMULADO. 7.860 alunos (`aluno`) em 200 escolas (`escola`, de
  20 a 60 alunos), 160 públicas e 40 privadas (`rede`), com `nota` e
  `reprovado` (sim/não). Alunos da mesma escola se parecem (ICC da nota ≈ 0,25):
  o caso em que o efeito do desenho de conglomerados dói.
- **escolas_resumo** — uma linha por escola das `escolas`: `escola`, `rede` e
  `alunos` (a população dela). É a tabela de UNIDADES de
  `sampling/margin_levels` e `sampling/referral`.
- **perfil_escolas** — o perfil dos alunos das `escolas` em formato longo:
  `variavel` (`rede`, `reprovado`), `categoria`, `total` e `participacao`. É a
  composição de `sampling/size_domains` e os totais de `sampling/rake`.
- **perguntas_exemplo** — um questionário mínimo, uma pergunta de cada tipo
  (`pergunta`, `tipo`, `opcoes`, `base`): o formato de
  `sampling/question_margins`.
- **domicilios** — SIMULADO, e já é uma AMOSTRA: 420 domicílios de uma
  pesquisa estratificada por situação (`estrato`: urbano/rural), com 30 e 12
  setores sorteados (`setor`) e 10 domicílios por setor. Traz `peso`, o número
  de setores do estrato (`setores_estrato`), `moradores`, `renda` e `internet`.
  É a entrada do `sampling/design`.

## Quando usar

Use para explorar o comportamento dos métodos com população e verdade conhecidas, ou para obter a amostra didática `domicilios` e praticar a declaração de desenho.

## Configuração

- **Conjunto** — qual conjunto carregar.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("fazendas", "sampling/example", dataset = "fazendas")
```

## Como interpretar

Cada tabela descreve o conjunto nomeado pelo parâmetro `Conjunto`; `fazendas` e `escolas` são populações inteiras com relações conhecidas, enquanto `domicilios` já é uma amostra com colunas de desenho. Use a verdade das populações para conferir estimativas simuladas. A saída é Tabela `data/table` com os dados do conjunto escolhido.

## Veja também

`sampling/srs`, `sampling/design`.
