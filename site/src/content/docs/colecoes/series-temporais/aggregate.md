---
title: Mudar frequência
description: "Agrega para uma frequência menor: mensal em trimestral ou anual."
section: colecoes
collection: series-temporais
node: series/aggregate
category: Operar
order: 2
related: [series/interpolate, series/moving_average]
---

## O que o bloco faz

Junta os períodos em blocos maiores: 12 meses viram um ano (**Nova
frequência** = 1), 3 meses viram um trimestre (4). A nova frequência tem de
dividir a antiga; a mensagem de erro lista as que servem.

### Os blocos seguem o calendário

Numa série que começa em março, o primeiro "ano" NÃO é de março a fevereiro:
os meses antes do primeiro janeiro são descartados, e o primeiro ano completo é
o primeiro bloco. O mesmo vale para o fim — um último ano com seis meses não
entra, porque a soma de meio ano pareceria uma queda. (O `aggregate()` do R
agrupa a partir da primeira observação e rotula com o ano dela: anos que não
existem, sem erro. Foi medido, e é por isso que este nó não o usa.)

### Qual função

- **soma** — para fluxos: vendas, chuva, passageiros. O total do ano.
- **media** — para níveis: temperatura, taxa, preço. O típico do ano.
- **ultimo** — para estoques: saldo, população. O valor no fim do ano.

Faltante dentro de um bloco deixa o bloco inteiro em NA na soma e na média.
Interpole antes (`series/interpolate`) se for o caso.

## Quando usar

Converta observações para um calendário mais amplo, como meses em trimestres ou anos. Escolha a função de agregação de acordo com o significado da variável.

## Configuração

- **Nova frequência** — observações por ciclo depois de agregar. Tem de
  dividir a frequência da série.
- **Função** — `soma`, `media` ou `ultimo`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("anual", "series/aggregate", frequencia = 1L, funcao = "soma", from = "pax")
```

## Como interpretar

Uma série (`series/ts`) mais curta, na nova frequência.

## Veja também

`data/group_summarise` para agregações que não são por calendário;
`series/interpolate` para os faltantes antes de somar.
