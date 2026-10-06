---
title: Intervenções
description: "Declara eventos em uma ou mais datas (pulso, degrau, rampa ou inovacional) para o modelo seguinte estimar."
section: colecoes
collection: series-temporais
node: series/intervencao
category: Modelar
order: 2
related: [series/arima, models/coefficients, series/forecast, series/detect_interventions, series/pettitt, series/zivot_andrews]
---

## O que o bloco faz

Marca na série EVENTOS em datas conhecidas — uma lei, uma greve, um dado
errado — para o modelo seguinte (`series/arima`) estimar o efeito de cada um
junto com a dinâmica da série. O bloco não ajusta nada: devolve a mesma
série, com as intervenções anotadas.

Há três jeitos de declarar várias, e eles se somam:

- várias datas no mesmo bloco, separadas por `;` (`1975, 1; 1983, 2`), todas
  com o mesmo tipo;
- uma tabela na entrada **datas**, uma intervenção por linha — a saída do
  `series/detect_interventions` liga direto, cada linha com o tipo que a
  busca achou;
- blocos encadeados, um depois do outro, cada um com o seu tipo. A série que
  já tem intervenções recebe mais.

É a análise de intervenção de Box e Tiao (1975), com os quatro tipos que a
literatura de outliers usa (Fox, 1972; Chen e Liu, 1993; Morettin e Toloi,
2006):

- **pulso** — o outlier ADITIVO (AO): 1 só na data. Um período fora do
  lugar, e a série volta como se nada tivesse acontecido.
- **degrau** — a mudança de NÍVEL (LS): 0 antes, 1 da data em diante.
- **rampa** — a mudança de INCLINAÇÃO: 0 antes, 1, 2, 3, ... a partir da data.
- **inovacional** — o outlier INOVACIONAL (IO): um choque no ruído da data,
  que se propaga pela dinâmica do próprio modelo, ψ(B) = θ(B)/φ(B). Num
  modelo com raiz unitária ele vira um degrau; num AR estável, um eco que se
  apaga. Por depender do modelo, só existe dentro do ajuste — e é por isso
  que a intervenção é declarada aqui e estimada lá.

### Dinâmica gradual

Com **Dinâmica** = `gradual` (pulso ou degrau), o efeito entra pela função
de transferência ω/(1 − δB): no primeiro período vale ω, e cada período soma
δ vezes o anterior. O pulso gradual se desfaz à razão δ por período (é a
mudança temporária, TC, que a detecção usa com δ = 0,7 fixo; aqui δ é
estimado); o degrau gradual cresce até o nível de longo prazo ω/(1 − δ). Uma
intervenção gradual por modelo.

### Tabela de datas

A coluna das datas é a **Coluna de data**; em branco, a coluna `data`. Ela
pode trazer texto (`1983, 2`, `1983 fev`, `1983 T1`, `1913`), datas de
calendário (série anual, trimestral ou mensal) ou o tempo decimal da série.
Se a tabela tiver uma coluna `tipo`, ela manda sobre o param **Tipo**, linha
a linha; a `temporaria` da detecção entra como pulso gradual. Linha com data
em branco é ignorada.

### A data vem de FORA

A data é o que se sabia antes de olhar o gráfico. Escolhê-la pelo maior salto
da própria série e depois testá-la é usar o dado que sugeriu a hipótese, e o
p-valor sai otimista. Para PROCURAR datas, `series/detect_interventions`
(e trate o que ela achar como hipótese a explicar). Ligar a tabela da busca
direto aqui é cômodo, mas os p-valores do modelo seguinte continuam
otimistas pelo mesmo motivo.

### Logo antes do modelo

A intervenção vale para a série exata em que foi declarada. Um
`series/transform`, `series/window` ou `series/diff` DEPOIS dela muda o
sentido do efeito, e o `series/arima` para em vermelho pedindo para trazer os
blocos de intervenção para depois da transformação.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes.

## Parâmetros

- **datas** (entrada, opcional) — tabela com uma data por linha (e, se
  quiser, o `tipo` de cada uma).
- **Datas** — o período, como `1983, 2` (fevereiro de 1983) ou só o ano numa
  série anual; várias separadas por `;`. Precisa haver ao menos uma
  observação antes de cada uma. Pode ficar em branco se a tabela vier ligada.
- **Coluna de data** — a coluna da tabela com as datas; em branco, `data`.
- **Tipo** — `pulso` (AO), `degrau` (LS, padrão), `rampa` ou `inovacional` (IO).
- **Dinâmica** — `imediata` (padrão) ou `gradual` (ω/(1 − δB)); só pulso e degrau.

## Valor

A mesma série (`series/ts`), com as intervenções declaradas. Cada coeficiente
sai no `series/arima` com o nome do tipo e da data (`degrau_1983_fev`).

## Exemplos

```r
tr_flow(reg) |>
  tr_add("sb", "series/example", dataset = "Seatbelts$drivers") |>
  tr_add("log", "series/transform", from = "sb") |>
  tr_add("lei", "series/intervencao", data = "1983, 2", tipo = "degrau", from = "log") |>
  tr_add("arima", "series/arima", automatico = FALSE, p = 1L, d = 0L, q = 0L,
         P = 1L, D = 1L, Q = 1L, constante = FALSE, from = "lei") |>
  tr_add("coef", "models/coefficients", from = "arima")
```

## Veja também

`series/arima` para estimar; `models/coefficients` para o teste de cada
efeito; `series/forecast` para prever com o efeito; `series/detect_interventions`
para procurar datas candidatas; `series/pettitt` e `series/zivot_andrews` para
testar uma quebra desconhecida.

