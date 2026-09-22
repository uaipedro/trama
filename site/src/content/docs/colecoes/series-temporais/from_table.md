---
title: Tabela para série
description: "Monta uma série a partir de uma coluna de valores e, opcionalmente, uma de tempo."
section: colecoes
collection: series-temporais
node: series/from_table
category: Fonte
order: 2
related: [series/interpolate, series/example]
---

## O que o bloco faz

Transforma uma coluna de uma tabela numa série temporal. É a porta de entrada
de todo dado seu: lido por `data/read_csv`, limpo e filtrado na coleção
`data`, e só então virado série aqui.

### A frequência é declarada aqui, uma vez

**Frequência** é quantas observações formam um ciclo: 12 para mensal, 4 para
trimestral, 1 para anual, 7 para diária com ciclo semanal, 24 para horária
com ciclo diário. Ela viaja com a série, e é por ela que a diferença sazonal
sabe usar defasagem 12, a decomposição corta ciclos de 12 e o correlograma
marca as defasagens 12, 24, 36. Errar aqui erra todo o fluxo — por isso é o
único lugar em que se digita.

### Tempo: ordena e confere a grade

Com **Tempo** preenchido, as linhas são ORDENADAS por ele (a tabela pode vir
embaralhada) e duas coisas viram card vermelho:

- **instante repetido** — dois valores para o mesmo mês. Agregue antes num
  `data/group_summarise`.
- **período faltando** — a linha de maio sumiu. Isto é o que mais importa: um
  `ts` é só um vetor com início e frequência, e sem a checagem junho passaria a
  ser chamado de maio, deslocando o calendário inteiro dali em diante, com o
  card verde. A mensagem diz entre quais datas está o buraco. Se não há dado
  naquele mês, a linha tem de existir com o valor em branco (NA) — e depois um
  `series/interpolate` decide o que pôr lá.

A grade só é conferida onde há calendário: coluna de DATA com frequência 12, 4
ou 1, e coluna de ANO inteiro com frequência 1. Nesses casos o início sai da
própria coluna. Para as outras frequências (7, 24, 52…) a coluna só ordena e
confere repetição; o início vem do campo **Início**.

Sem **Tempo**, a série segue a ordem das linhas da tabela, e começa em
**Início** (ou no período 1).

### Série simulada

Não há gerador aqui, e é de propósito: sortear é trabalho da coleção `data`.
Uma coluna criada num `data/mutate` com `as.numeric(arima.sim(list(ar = 0.7),
n()))` dá um AR(1) do tamanho da tabela; ligue a tabela aqui, com **Valor**
apontando para essa coluna.

## Quando usar

Converta uma tabela preparada na coleção Dados em série temporal. Informe a coluna numérica, a frequência e, quando disponível, a coluna que define a ordem e o calendário.

## Configuração

- **Valor** — coluna numérica com os valores da série. Obrigatório. Coluna de
  texto é recusada: converta antes num `data/convert`.
- **Tempo** — coluna que ordena as linhas e, quando é data ou ano, dá o início
  e tem a grade conferida. Vazio usa a ordem da tabela.
- **Frequência** — observações por ciclo (12 mensal, 4 trimestral, 1 anual).
- **Início** — `ano` ou `ano, período` (`2019, 7` é julho de 2019). Só vale
  sem **Tempo** ou quando o tempo não é data/ano; preencher os dois é recusado,
  porque seriam duas fontes para o mesmo fato.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)
tr_use("trama.series", registry = reg)

arquivo <- tempfile(fileext = ".csv")
dados <- data.frame(
  mes = seq(as.Date("2022-01-01"), by = "month", length.out = 24),
  vendas = round(100 + 8 * sin(2 * pi * (1:24) / 12) + 0.5 * (1:24))
)
write.csv(dados, arquivo, row.names = FALSE)

tr_flow(reg) |>
  tr_add("ler", "data/read_csv", path = arquivo) |>
  tr_add("serie", "series/from_table", valor = "vendas", tempo = "mes",
         frequencia = 12L, from = "ler")
```

## Como interpretar

Uma série (`series/ts`), que o card mostra como gráfico.

## Veja também

`data/read_csv` e `data/read_excel` para ler; `data/group_summarise` para
agregar dias em meses antes; `data/mutate` para simular uma coluna;
`series/example` para as séries que vêm com o R.
