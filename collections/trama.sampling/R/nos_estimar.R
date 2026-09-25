# As declarações das abas Estimar e Avaliar.

.tr_sampling_nos_estimar <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum
  S <- "sampling/sample"; ES <- "sampling/estimate"
  CONF <- function() .tr_sampling_param_conf()
  POR <- function(ex = "regiao") P("cols", "", label = "Por (domínios)", example = ex)
  ajuda_por <- r"---[
- **Por** — coluna dos domínios: uma estimativa por grupo (região, sexo). O
  domínio é estimado SEM cortar a amostra — as unidades fora dele continuam no
  cálculo da variância, com valor zero. Filtrar antes com `data/filter` e
  estimar depois subestima o erro, porque finge que o número de unidades do
  domínio na amostra era fixo.
- **Confiança** — número entre 0,5 e 0,999 (0,95 = 95%). O intervalo é t com os graus de liberdade
  do desenho (unidades primárias − estratos).
]---"
  ajuda_valor <- r"---[
Uma estimativa (`sampling/estimate`). O adaptador para `data/table` dá uma linha
por domínio com `quantidade`, `variavel`, `desenho`, `estimativa`,
`erro_padrao`, `li`, `ls`, `margem`, `cv_pct`, `deff`, `n` e `gl` — pronta para
um `data/bind_rows` juntar várias num relatório.
]---"
  ajuda_desenho <- r"---[
O desenho vem da amostra ligada — estratos, conglomerados, pesos e correção
finita —, e não se digita aqui. A variância é a do conglomerado último, com
linearização de Taylor; na AAS, na estratificada e no conglomerado em um
estágio ela é exatamente a fórmula dos livros.

Faltante na variável fica fora, tratado como domínio (sem cortar o desenho), e
a nota conta quantas linhas.
]---"
  exemplo <- function(no, params) sprintf(r"---[
tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("amostra", "sampling/stratified", estrato = "regiao", n = 240L, from = "pop") |>
  tr_add("est", "%s", %s, from = "amostra")
]---", no, params)
  list(
    trama::tr_node("sampling/mean", fn = tr_sampling_mean, label = "Média",
      category = "amostra_estimar", icon = trama::tr_icon("sigma"),
      description = "Estima a média da população, com o erro padrão e o intervalo do desenho.",
      inputs = list(amostra = S), outputs = list(out = ES),
      params = list(variavel = P("cols", "", label = "Variável", example = "producao_t"), por = POR(),
                    confianca = CONF()),
      help = .tr_sampling_ajuda(paste(r"---[
A média da população, estimada pela média PONDERADA da amostra (Σ w·y / Σ w).
Numa AAS todo peso é igual e ela é a média simples; numa estratificada
desproporcional ou numa PPS, a média simples estaria errada.
]---", ajuda_desenho), paste(r"---[
- **Variável** — coluna numérica.
]---", ajuda_por), ajuda_valor, exemplo("sampling/mean", "variavel = \"producao_t\", por = \"irrigada\""), r"---[
`sampling/total`; `sampling/ratio` quando o denominador também é aleatório;
`sampling/plot_estimates`; `sampling/simulate`.
]---", cv = TRUE)),

    trama::tr_node("sampling/total", fn = tr_sampling_total, label = "Total",
      category = "amostra_estimar", icon = trama::tr_icon("square-sigma"),
      description = "Estima o total da população (Horvitz-Thompson), com o erro do desenho.",
      inputs = list(amostra = S), outputs = list(out = ES),
      params = list(variavel = P("cols", "", label = "Variável", example = "producao_t"), por = POR(),
                    confianca = CONF()),
      help = .tr_sampling_ajuda(paste(r"---[
O total da população pelo estimador de Horvitz-Thompson: Σ w·y, em que cada
unidade da amostra conta pelas w unidades da população que representa. É o que
responde "quanto se produziu na região", "quantos domicílios têm internet" (o
total de um indicador 0/1).

O total depende do peso inteiro, e não só das proporções entre os pesos: uma
amostra sem peso (ou declarada sem ele) daria o total da AMOSTRA.
]---", ajuda_desenho), paste(r"---[
- **Variável** — coluna numérica.
]---", ajuda_por), ajuda_valor, exemplo("sampling/total", "variavel = \"producao_t\", por = \"regiao\""), r"---[
`sampling/mean`; `sampling/pps`, o desenho que torna o total preciso;
`sampling/poststratify`.
]---", cv = TRUE)),

    trama::tr_node("sampling/proportion", fn = tr_sampling_proportion, label = "Proporção",
      category = "amostra_estimar", icon = trama::tr_icon("chart-pie"),
      description = "Estima a proporção da população em cada categoria de uma variável, com o erro do desenho.",
      inputs = list(amostra = S), outputs = list(out = ES),
      params = list(variavel = P("cols", "", label = "Variável", example = "irrigada"),
                    nivel = P("text", "", label = "Categoria (em branco: todas)", example = "sim"),
                    por = POR(), confianca = CONF()),
      help = .tr_sampling_ajuda(paste(r"---[
A proporção da população em cada categoria: a média ponderada do indicador
(1 se a unidade é da categoria, 0 se não). Com **Categoria** em branco, uma
linha por categoria; com uma categoria, só ela.

O card mostra em %; a tabela, em proporção (0 a 1). O intervalo é o de Wald com
t, que pode passar de 0 ou 1 em proporções extremas com amostra pequena — leia
junto do n.
]---", ajuda_desenho), paste(r"---[
- **Variável** — coluna categórica (texto, fator ou lógica).
- **Categoria** — o valor cuja proporção se quer; em branco, todas.
]---", ajuda_por), ajuda_valor, exemplo("sampling/proportion", "variavel = \"irrigada\", nivel = \"sim\", por = \"regiao\""), r"---[
`sampling/size_proportion` para o n; `sampling/total` do indicador para o
número de unidades; `sampling/plot_estimates`.
]---", cv = TRUE)),

    trama::tr_node("sampling/ratio", fn = tr_sampling_ratio, label = "Razão",
      category = "amostra_estimar", icon = trama::tr_icon("divide"),
      description = "Estima a razão entre dois totais (produtividade, renda per capita), com o erro do desenho.",
      inputs = list(amostra = S), outputs = list(out = ES),
      params = list(numerador = P("cols", "", label = "Numerador", example = "producao_t"),
                    denominador = P("cols", "", label = "Denominador", example = "area_ha"),
                    por = POR(), confianca = CONF()),
      help = .tr_sampling_ajuda(paste(r"---[
A razão entre dois totais, R = Σ w·y / Σ w·x: toneladas por hectare, renda por
morador, trabalhadores por fazenda. Não é a média das razões de cada unidade
(que daria à fazenda de 1 ha o mesmo peso que à de 1.000 ha): é a produção
total sobre a área total.

O denominador também é aleatório — outra amostra traria outra área —, e a
variância leva isso em conta pela linearização. A média de uma variável é o
caso particular com denominador 1 em toda unidade.

O deff não é calculado para a razão: a AAS de comparação dependeria da
variância conjunta das duas colunas, e um número a mais que se lê errado é pior
que nenhum.
]---", ajuda_desenho), paste(r"---[
- **Numerador**, **Denominador** — colunas numéricas.
]---", ajuda_por), ajuda_valor, exemplo("sampling/ratio", "numerador = \"producao_t\", denominador = \"area_ha\", por = \"regiao\""), r"---[
`sampling/mean`; `sampling/total`; `sampling/design` com a renda per capita dos
`domicilios`.
]---", cv = TRUE))
  )
}

.tr_sampling_nos_avaliar <- function() {
  P <- trama::tr_param; E <- trama::tr_param_enum; I <- trama::tr_param_int
  S <- "sampling/sample"; SIM <- "sampling/simulation"
  list(
    trama::tr_node("sampling/simulate", fn = tr_sampling_simulate, label = "Simular desenho",
      category = "amostra_avaliar", icon = trama::tr_icon("dices"), stochastic = TRUE,
      description = "Re-sorteia a amostra centenas de vezes e mede viés, erro padrão e cobertura contra a verdade.",
      inputs = list(amostra = S), outputs = list(out = SIM),
      params = list(variavel = P("cols", "", label = "Variável", example = "producao_t"),
                    estimador = E("média", c("média", "total"), label = "Estimador"),
                    repeticoes = I(500L, min = 20L, max = 10000L, label = "Repetições"),
                    confianca = .tr_sampling_param_conf()),
      help = .tr_sampling_ajuda(r"---[
Avalia o DESENHO, e não a amostra: pega a receita guardada na amostra ligada
(o bloco de seleção, o n, os estratos, a pós-estratificação), sorteia de novo da
mesma população **Repetições** vezes, estima em cada uma, e compara com a
verdade — que existe, porque a população inteira está no cadastro.

O card é o histograma das estimativas, com a verdade (linha cheia) e a média
das estimativas (tracejada). Embaixo:

- **EP** — o erro padrão EMPÍRICO, o desvio das estimativas entre amostras. É o
  erro que o desenho de fato tem. Entre parênteses, o erro padrão que o
  estimador CALCULA, em média: se os dois batem, o card de uma amostra só é
  confiável.
- **viés** — quanto a média das estimativas se afasta da verdade, em %. Média
  e total pelo desenho são (quase) sem viés; um estimador que ignora o peso não
  seria.
- **cobre** — em quantas amostras o intervalo de confiança contém a verdade.
  Deve ficar perto da confiança (95%); abaixo, o intervalo promete mais do que
  entrega.

Duas simulações lado a lado, com o mesmo n e desenhos diferentes, são a
comparação de eficiência: o desenho de EP menor precisa de menos amostra para a
mesma precisão. Para desenhá-las juntas, `sampling/plot_simulation`; para uma
tabela, ligue as duas num `data/bind_rows` (o adaptador dá uma linha de resumo
por simulação).

Só funciona com amostra de bloco de seleção: a de `sampling/design` não guarda
a população.
]---", r"---[
- **Variável** — coluna numérica da população.
- **Estimador** — `média` ou `total`.
- **Repetições** — quantas amostras sortear (20 a 10.000). 500 dão a cobertura
  com erro de ±2 pontos.
- **Confiança** — a do intervalo cuja cobertura se mede.
]---", r"---[
Uma simulação (`sampling/simulation`); o adaptador para `data/table` dá uma
linha com o desenho, o n, a verdade, a média das estimativas, o viés relativo,
o EP empírico e o estimado, a raiz do erro quadrático médio e a cobertura.
]---", r"---[
tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("aas", "sampling/srs", n = 200L, from = "pop") |>
  tr_add("sim", "sampling/simulate", variavel = "producao_t", repeticoes = 200L, from = "aas")
]---", r"---[
`sampling/plot_simulation` para comparar desenhos; `sampling/srs`,
`sampling/stratified`, `sampling/cluster`.
]---", semente = TRUE)),

    trama::tr_node("sampling/plot_simulation", fn = tr_sampling_plot_simulation, label = "Comparar simulações",
      category = "amostra_avaliar", icon = trama::tr_icon("chart-candlestick"),
      description = "Desenha a distribuição das estimativas de vários desenhos, lado a lado, contra a verdade.",
      inputs = list(simulacoes = trama::tr_port(SIM, multiple = TRUE)), outputs = list(out = "view/plot"),
      params = .tr_sampling_props(),
      help = .tr_sampling_ajuda(r"---[
Uma faixa por simulação ligada: o violino das estimativas, a caixa com a
mediana e os quartis, e a verdade como linha vertical. Ao lado de cada desenho,
o EP empírico (e o estimado), o viés e a cobertura.

A leitura é a da eficiência: com o mesmo n, a faixa mais ESTREITA é o desenho
mais preciso. Nas `fazendas` com n = 200, a estratificada por região fica mais
estreita que a AAS, e a de conglomerados por município, bem mais larga.

A porta aceita quantos cabos forem ligados.
]---", r"---[
Só os de aparência.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("aas", "sampling/srs", n = 200L, from = "pop") |>
  tr_add("estr", "sampling/stratified", estrato = "regiao", n = 200L, from = "pop") |>
  tr_add("sim_aas", "sampling/simulate", variavel = "producao_t", repeticoes = 100L, from = "aas") |>
  tr_add("sim_estr", "sampling/simulate", variavel = "producao_t", repeticoes = 100L, from = "estr") |>
  tr_add("comparar", "sampling/plot_simulation", from = "sim_aas") |>
  tr_link("sim_estr", "comparar:simulacoes")
]---", r"---[
`sampling/simulate`.
]---", grafico = TRUE)),

    trama::tr_node("sampling/plot_estimates", fn = tr_sampling_plot_estimates, label = "Gráfico de estimativas",
      category = "amostra_avaliar", icon = trama::tr_icon("crosshair"),
      description = "Estimativas por domínio com o intervalo de confiança, coloridas pela precisão (CV).",
      inputs = list(estimativa = "sampling/estimate"), outputs = list(out = "view/plot"),
      params = .tr_sampling_props(),
      help = .tr_sampling_ajuda(r"---[
O gráfico de uma estimativa: um ponto e o intervalo de confiança por domínio (e
por categoria, na proporção), coloridos pela faixa de precisão do CV — ótima,
boa, regular, imprecisa. É a figura do relatório: o domínio cujo intervalo é
largo demais para publicar aparece em outra cor sem legenda a decifrar.

Proporções saem em %.
]---", r"---[
Só os de aparência.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("amostra", "sampling/stratified", estrato = "regiao", n = 240L, from = "pop") |>
  tr_add("irr", "sampling/proportion", variavel = "irrigada", nivel = "sim", por = "regiao", from = "amostra") |>
  tr_add("graf", "sampling/plot_estimates", titulo = "Fazendas irrigadas por região", from = "irr")
]---", r"---[
`sampling/mean`; `sampling/proportion`; `sampling/total`; `sampling/ratio`.
]---", grafico = TRUE))
  )
}
