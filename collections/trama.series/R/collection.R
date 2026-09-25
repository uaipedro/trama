# A coleção `series`: a declaração dos nós e as páginas de ajuda.
#
# As páginas são montadas por `.tr_series_ajuda()`, que fixa as seções na
# ordem de `?funcao` (Descrição / Parâmetros / Valor / Exemplos / Veja também):
# com trinta nós, a seção esquecida em um deles é certa se cada página for
# escrita à mão. O texto de cada seção vem em raw string (`r"---[ ]---"`)
# porque as páginas citam código com aspas e barras, e escapar cada uma delas
# é o jeito mais seguro de publicar um exemplo que não roda.

.tr_series_ajuda <- function(descricao, parametros, valor, exemplos, veja, grafico = FALSE,
                             teste = FALSE) {
  paste0("## Descrição\n\n", trimws(descricao),
         "\n\n## Parâmetros\n\n", trimws(parametros),
         "\n\n## Valor\n\n", trimws(valor),
         "\n\n## Exemplos\n\n```r\n", trimws(exemplos), "\n```",
         "\n\n## Veja também\n\n", trimws(veja),
         if (grafico) paste0("\n", trama.view::tr_view_help_appearance()) else "",
         if (teste) paste0("\n", .tr_series_ajuda_pontinhos()) else "")
}

#' A seção que explica o card de teste.
#'
#' O card é o do núcleo (`trama/test`), e a explicação também:
#' `trama::tr_help_test_card()` cobre a régua dos testes com p-valor e os
#' pontinhos dos testes de tabela crítica, que esta coleção tem dos dois. O nome
#' ficou, porque é por ele que a varredura de `test-catalogo.R` confere que todo
#' bloco de teste traz a seção.
#' @noRd
.tr_series_ajuda_pontinhos <- function() trama::tr_help_test_card()


#' Os cosméticos da `view`, com a proporção padrão própria do gráfico.
#'
#' A decomposição empilha quatro painéis, e em 16:9 cada um vira uma fita.
#' Mudar só o DEFAULT do spec mantém o param igual ao da `view` em tudo o mais
#' — nome, escolhas, posição —, e o `fn` declara o mesmo default, para que o
#' card e o console desenhem o mesmo gráfico.
#' @noRd
.tr_series_props <- function(..., .aspecto = "16:9") {
  ps <- trama.view::tr_view_props(...)
  ps$aspecto$default <- .aspecto
  ps
}

#' A coleção `series`.
#'
#' Carrega DEPOIS de `trama.data` e `trama.view`: as portas usam `data/table`
#' e `view/plot`, e o registro recusa porta com tipo desconhecido. É a mesma
#' dependência de ordem que a `view` tem com a `data`, travada por teste em
#' `test-catalogo.R`.
#'
#' Os ids de categoria têm prefixo (`serie_*`) porque o registro de
#' categorias é GLOBAL (`registry$categories[[id]]`): uma categoria `fonte`
#' aqui sobrescreveria em silêncio a de outra coleção que usasse o mesmo id.
#' @export
trama_collection <- function() {
  S <- "series/ts"; D <- "series/decomposition"; M <- "series/model"; F <- "series/forecast"
  R <- "series/regression"; TE <- "series/test"
  T <- "data/table"; G <- "view/plot"
  P <- trama::tr_param
  I <- trama::tr_param_int
  E <- trama::tr_param_enum
  B <- trama::tr_param_bool
  icone <- function(n) trama::tr_icon(n)

  trama::tr_collection(
    id = "series", version = "0.1.0", label = "Séries temporais",
    js = "trama/index.js",
    types = list(series_ts_type(), series_decomposition_type(), series_model_type(),
                 series_forecast_type(), series_regression_type(), series_test_type()),
    adapters = .tr_series_adapters(),
    # Glossário (docs/glossario-parametros.md): id em inglês que diga a
    # PERGUNTA, como os outros da coleção. `kruskal_wallis` e `fisher` diziam o
    # autor do teste, não o que ele testa. Fluxos salvos abrem com o id novo.
    migrations = list(nodes = list(
      "series/f_sazonal" = "series/f_seasonal",
      "series/f_tendencia" = "series/f_trend",
      "series/kruskal_wallis" = "series/seasonality_kw",
      "series/fisher" = "series/periodicity_fisher")),
    # Os testes ocupam cinco categorias, e não uma "Testar": são quinze blocos,
    # um por hipótese nula, e quinze itens numa lista só não se varrem com o
    # olho. O corte é pela PERGUNTA que o teste responde — a série tem raiz
    # unitária, sobrou autocorrelação, há tendência, há sazonalidade —, não pelo
    # pacote que o implementa: quem chega à aba sabe o que quer perguntar, não
    # de onde a função vem.
    categories = list(
      trama::tr_category("serie_fonte",     "Fonte", role = "origem"),
      trama::tr_category("serie_operar",    "Operar", role = "preparacao"),
      trama::tr_category("serie_decompor",  "Decompor", role = "ajuste"),
      trama::tr_category("serie_modelar",   "Modelar", role = "ajuste"),
      trama::tr_category("serie_raiz",      "Raiz unitária", role = "avaliacao"),
      trama::tr_category("serie_autocorr",  "Autocorrelação", role = "avaliacao"),
      trama::tr_category("serie_tendencia", "Tendência", role = "avaliacao"),
      trama::tr_category("serie_sazonal",   "Sazonalidade", role = "avaliacao"),
      trama::tr_category("serie_regressao", "Regressão", role = "avaliacao"),
      trama::tr_category("serie_ver",       "Ver", role = "inspecao")
    ),
    nodes = list(

# ---- Fonte ------------------------------------------------------------------

      trama::tr_node("series/example", fn = tr_series_example, label = "Série de exemplo",
        category = "serie_fonte", icon = icone("activity"),
        description = "Carrega uma das séries temporais que vêm com o R.",
        outputs = list(out = S),
        params = list(dataset = E("AirPassengers", .tr_series_exemplos(), label = "Série")),
        help = .tr_series_ajuda(r"---[
Carrega uma das séries temporais do pacote `datasets` do R. São exatamente os
conjuntos que o `data/example` recusa — lá eles não são tabela; aqui são o
assunto.

Algumas que valem conhecer:

- **AirPassengers** — passageiros aéreos mensais, 1949–1960. O caso de livro:
  tendência, sazonalidade, e sazonalidade que CRESCE com o nível (pede log).
- **co2** — CO₂ em Mauna Loa, mensal: tendência e sazonalidade estáveis.
- **Nile** — vazão anual do Nilo, com uma quebra em 1898. Sem sazonalidade.
- **UKgas** — consumo trimestral de gás no Reino Unido.
- **presidents** — aprovação trimestral dos presidentes dos EUA, com faltantes.
- **lynx** — capturas anuais de linces: ciclo de ~10 anos que não é sazonal.

A série chega com a frequência de origem — 12 para mensal, 4 para trimestral,
1 para anual — e é ela que os nós adiante usam para saber o que é "sazonal".

Séries múltiplas (`EuStockMarkets`, `Seatbelts`) aparecem uma vez por coluna,
como `EuStockMarkets$DAX`: o tipo `series/ts` guarda uma série só.
]---", r"---[
- **Série** — qual série carregar. A lista é derivada do R que está rodando.
]---", r"---[
Uma série (`series/ts`). O card mostra o gráfico dela, e o resumo diz início,
fim, frequência e quantos faltantes. Ligada a qualquer nó da coleção `data`, a
série vira tabela com as colunas `tempo` e `valor` sozinha, sem nó no meio.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example", dataset = "AirPassengers") |>
  tr_add("log", "series/transform", metodo = "log", from = "pax")
]---", r"---[
`series/from_table` para montar a série a partir de uma tabela sua;
`data/example` para os conjuntos que são tabela; `series/plot` para desenhá-la
com título e proporção escolhidos.
]---")),

      trama::tr_node("series/from_table", fn = tr_series_from_table, label = "Tabela para série",
        category = "serie_fonte", icon = icone("table-rows-split"),
        description = "Monta uma série a partir de uma coluna de valores e, opcionalmente, uma de tempo.",
        inputs = list(dados = T), outputs = list(out = S),
        params = list(
          valor = P("cols", "", label = "Valor", example = "vendas"),
          tempo = P("cols", "", label = "Tempo", example = "mes"),
          frequencia = I(12L, min = 1L, max = 366L, label = "Frequência"),
          inicio = P("text", "", label = "Início", example = "2019, 7")),
        help = .tr_series_ajuda(r"---[
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
]---", r"---[
- **Valor** — coluna numérica com os valores da série. Obrigatório. Coluna de
  texto é recusada: converta antes num `data/convert`.
- **Tempo** — coluna que ordena as linhas e, quando é data ou ano, dá o início
  e tem a grade conferida. Vazio usa a ordem da tabela.
- **Frequência** — observações por ciclo (12 mensal, 4 trimestral, 1 anual).
- **Início** — `ano` ou `ano, período` (`2019, 7` é julho de 2019). Só vale
  sem **Tempo** ou quando o tempo não é data/ano; preencher os dois é recusado,
  porque seriam duas fontes para o mesmo fato.
]---", r"---[
Uma série (`series/ts`), que o card mostra como gráfico.
]---", r"---[
tr_flow(reg) |>
  tr_add("ler", "data/read_csv", path = "vendas_mensais.csv") |>
  tr_add("serie", "series/from_table", valor = "vendas", tempo = "mes",
         frequencia = 12L, from = "ler")
]---", r"---[
`data/read_csv` e `data/read_excel` para ler; `data/group_summarise` para
agregar dias em meses antes; `data/mutate` para simular uma coluna;
`series/example` para as séries que vêm com o R.
]---")),

# ---- Operar -----------------------------------------------------------------

      trama::tr_node("series/diff", fn = tr_series_diff, label = "Diferença",
        category = "serie_operar", icon = icone("diff"),
        description = "Diferença simples (x[t] - x[t-1]) ou sazonal (x[t] - x[t-ciclo]).",
        inputs = list(serie = S), outputs = list(out = S),
        params = list(tipo = E("simples", c("simples", "sazonal"), label = "Tipo"),
                      ordem = I(1L, min = 1L, max = 3L, label = "Ordem")),
        help = .tr_series_ajuda(r"---[
Troca cada valor pela variação em relação a um valor anterior. É a operação
que tira tendência (diferença simples) e sazonalidade (diferença sazonal) de
uma série, e o **I** do ARIMA.

- **simples**: `x[t] - x[t-1]`, a variação de um período para o seguinte.
  Tira tendência linear.
- **sazonal**: `x[t] - x[t-ciclo]`, a variação em relação ao mesmo período do
  ciclo anterior — janeiro contra janeiro. Tira sazonalidade estável. O ciclo é
  a frequência da série; numa série anual (frequência 1) não há ciclo, e o nó
  para em vermelho em vez de fazer uma diferença simples com outro nome.

**Ordem** repete a operação: ordem 2 é a diferença da diferença. Raramente
passa de 1; `series/ndiffs` diz quantas a série pede.

Cada diferença come observações do começo: uma simples come 1, uma sazonal
mensal come 12. É por isso que a série sai mais curta.

Para série com sazonalidade E tendência, o comum é encadear dois nós: sazonal
e depois simples (a ordem não altera o resultado).
]---", r"---[
- **Tipo** — `simples` (defasagem 1) ou `sazonal` (defasagem igual à
  frequência).
- **Ordem** — quantas vezes aplicar, de 1 a 3.
]---", r"---[
Uma série mais curta (`series/ts`), começando depois das observações
consumidas.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("log", "series/transform", from = "pax") |>
  tr_add("d12", "series/diff", tipo = "sazonal", from = "log") |>
  tr_add("d1", "series/diff", from = "d12") |>
  tr_add("adf", "series/adf", from = "d1")
]---", r"---[
`series/ndiffs` para saber quantas diferenças usar; `series/adf` e
`series/kpss` para conferir o resultado; `series/acf` para ver o que sobrou de
estrutura;
`series/arima`, que faz a diferença por dentro quando `d` ou `D` são maiores
que zero.
]---")),

      trama::tr_node("series/ndiffs", fn = tr_series_ndiffs, label = "Diferenças necessárias",
        category = "serie_operar", icon = icone("hash"),
        description = "Quantas diferenças simples e sazonais a série pede para ficar estacionária.",
        inputs = list(serie = S), outputs = list(out = T),
        params = list(teste = E("kpss", c("kpss", "adf", "pp"), label = "Teste")),
        help = .tr_series_ajuda(r"---[
Responde, antes de montar o `series/diff` ou de fixar o `d` e o `D` de um
ARIMA à mão, quantas diferenças a série pede.

- **simples**: aplica o teste escolhido em sequência — a série, a diferença, a
  diferença da diferença — até a série passar (`forecast::ndiffs`).
- **sazonal**: pelo teste de força sazonal (`forecast::nsdiffs`). Só existe
  para série com ciclo e dois ciclos de dado; na anual sai em branco, com o
  motivo na nota — e não zero, que afirmaria "testei e não precisa".

A ordem prática: faça as sazonais primeiro, e rode este nó de novo depois delas
para as simples.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.
]---", r"---[
- **Teste** — o teste de raiz unitária das diferenças simples: `kpss`
  (padrão), `adf` ou `pp`.
]---", r"---[
Uma tabela (`data/table`) com as linhas `simples` e `sazonal`.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("quantas", "series/ndiffs", from = "pax")
]---", r"---[
`series/diff` para aplicar; `series/adf` e `series/kpss` para conferir depois.
]---")),

      trama::tr_node("series/lag", fn = tr_series_lag, label = "Defasar",
        category = "serie_operar", icon = icone("move-right"),
        description = "Desloca a série k períodos no tempo, sem mudar os valores.",
        inputs = list(serie = S), outputs = list(out = S),
        params = list(k = I(1L, min = -1000L, max = 1000L, label = "Defasagem (k)")),
        help = .tr_series_ajuda(r"---[
Desloca a série k períodos para frente no tempo: o valor que era de janeiro
passa a ser de fevereiro (com k = 1). Os VALORES não mudam, só as datas — é
isso que uma defasagem é.

Serve para pôr uma série ao lado da própria versão atrasada: ligue a original e
a defasada em `data/join` por `tempo`, e cada linha terá `x[t]` e `x[t-k]` —
a base de um modelo com defasagem, ou de um disperso da série contra o
passado dela (para esse gráfico pronto, `series/lag_plot`).

k negativo adianta.
]---", r"---[
- **Defasagem (k)** — quantos períodos deslocar. Positivo atrasa, negativo
  adianta; zero devolve a série como está.
]---", r"---[
A mesma série (`series/ts`), com início e fim deslocados k períodos.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("atrasada", "series/lag", k = 12L, from = "pax")
]---", r"---[
`series/lag_plot` para ver a série contra ela mesma defasada; `data/join` para
alinhar original e defasada numa tabela; `series/diff`, que é a série menos a
defasada.
]---")),

      trama::tr_node("series/transform", fn = tr_series_transform, label = "Transformar",
        category = "serie_operar", icon = icone("square-function"),
        description = "Log, raiz ou Box-Cox: estabiliza a variância que cresce com o nível.",
        inputs = list(serie = S), outputs = list(out = S),
        params = list(metodo = E("log", c("log", "raiz", "boxcox"), label = "Método"),
                      lambda = P("text", "", label = "λ (Box-Cox)", example = "0.5")),
        help = .tr_series_ajuda(r"---[
Aplica uma transformação que ACHATA a variação quando ela cresce com o nível
da série. Em `AirPassengers` a oscilação de cada ano é maior que a do anterior
porque a sazonalidade é multiplicativa; depois do log, as oscilações ficam do
mesmo tamanho, e decomposição aditiva, ARIMA e ETS aditivo passam a servir.

- **log** — a mais comum, e a que se interpreta: diferença de log é variação
  percentual (aproximada).
- **raiz** — mais branda que o log; aceita zero.
- **boxcox** — uma família que vai da identidade (λ = 1) ao log (λ = 0). Com
  **λ** em branco, o valor é escolhido pelo método de Guerrero, e fica
  guardado na série.

Log e Box-Cox exigem valores positivos, e raiz exige não negativos: zero ou
negativo para o nó em vermelho, dizendo quantos há. O conserto não é um param
aqui — é somar uma constante antes, num `data/mutate`, escolhendo às claras
quanto somar.

A previsão de uma série transformada sai na escala transformada. Para voltar,
leve a tabela da previsão a um `data/mutate` com `exp()`.
]---", r"---[
- **Método** — `log`, `raiz` ou `boxcox`.
- **λ (Box-Cox)** — só vale para `boxcox`. Vazio escolhe automaticamente;
  um número (`0.5`, ou `0,5`) fixa. Ignorado nos outros métodos.
]---", r"---[
Uma série (`series/ts`) do mesmo tamanho, na escala transformada.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("bc", "series/transform", metodo = "boxcox", lambda = "", from = "pax")
]---", r"---[
`series/decompose` e `series/ets`, que têm versão multiplicativa quando não se
quer transformar; `data/mutate` para somar uma constante antes, ou para voltar
da escala com `exp()`.
]---")),

      trama::tr_node("series/window", fn = tr_series_window, label = "Recortar período",
        category = "serie_operar", icon = icone("calendar-range"),
        description = "Mantém só os períodos entre um início e um fim.",
        inputs = list(serie = S), outputs = list(out = S),
        params = list(inicio = P("text", "", label = "Início", example = "1955, 1"),
                      fim = P("text", "", label = "Fim", example = "1958")),
        help = .tr_series_ajuda(r"---[
Recorta a série no tempo. O uso mais importante é separar TREINO de TESTE:
um recorte até 1958 alimenta o modelo, e a série inteira vai ao
`series/accuracy` para medir o erro nos anos que o modelo não viu.

Os períodos se escrevem como `ano` ou `ano, período`: `1955, 3` é março de
1955 numa série mensal, o terceiro trimestre numa trimestral.

**Fim só com o ano vai até o último período daquele ano**: `fim = 1958` numa
série mensal para em dezembro de 1958. (O `window()` do R pararia em janeiro,
que não é o que ninguém quer dizer.) **Início** só com o ano começa no primeiro
período.

Período fora da série para o nó em vermelho, dizendo de quando a quando ela
vai — em vez de recortar em silêncio até a borda, o que faria um `1995`
digitado no lugar de `1959` devolver a série inteira.

Os dois em branco desligam o nó: a série passa inteira.
]---", r"---[
- **Início** — primeiro período mantido. Vazio é o começo da série.
- **Fim** — último período mantido. Vazio é o fim da série.
]---", r"---[
Uma série (`series/ts`) mais curta.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("treino", "series/window", fim = "1958", from = "pax") |>
  tr_add("modelo", "series/ets", from = "treino") |>
  tr_add("prev", "series/forecast", horizonte = 24L, from = "modelo") |>
  tr_add("erro", "series/accuracy", from = "prev") |>
  tr_link("pax", "erro:real")
]---", r"---[
`series/accuracy`, que usa o recorte para medir erro fora da amostra;
`data/filter`, para recortar a tabela por data quando a série já virou tabela.
]---")),

      trama::tr_node("series/moving_average", fn = tr_series_moving_average, label = "Média móvel",
        category = "serie_operar", icon = icone("spline"),
        description = "Suaviza a série com a média de k períodos vizinhos.",
        inputs = list(serie = S), outputs = list(out = S),
        params = list(ordem = I(12L, min = 2L, max = 1000L, label = "Ordem (k)"),
                      centrada = B(TRUE, label = "Centrada")),
        help = .tr_series_ajuda(r"---[
Troca cada valor pela média dos k vizinhos. É a forma mais simples de ver a
TENDÊNCIA: com k igual à frequência (12 numa série mensal), cada média cobre
um ano inteiro, e a sazonalidade some por construção.

Centrada com ordem par é a média 2×k — a média de duas médias de k defasadas
de um período —, que é o jeito de pôr o resultado no MEIO da janela quando a
janela não tem meio. É a mesma que a decomposição clássica usa por dentro.

Não centrada, a média de cada ponto usa só o passado (os k anteriores): é a
que se pode calcular em tempo real, mas ela ATRASA meia janela em relação à
série.

As pontas saem em branco (NA): meia janela no começo e no fim não tem vizinhos
para a média. É o honesto, e o gráfico mostra a linha começando depois.
]---", r"---[
- **Ordem (k)** — quantos períodos entram em cada média.
- **Centrada** — liga a média no meio da janela (2×k para k par); desligada,
  a média usa os k períodos anteriores.
]---", r"---[
Uma série (`series/ts`) do mesmo tamanho, com NA nas pontas.
]---", r"---[
tr_flow(reg) |>
  tr_add("co2", "series/example", dataset = "co2") |>
  tr_add("tend", "series/moving_average", ordem = 12L, from = "co2")
]---", r"---[
`series/decompose` e `series/stl`, que separam a tendência junto com os outros
componentes; `series/plot` para ver o resultado.
]---")),

      trama::tr_node("series/aggregate", fn = tr_series_aggregate, label = "Mudar frequência",
        category = "serie_operar", icon = icone("calendar-days"),
        description = "Agrega para uma frequência menor: mensal em trimestral ou anual.",
        inputs = list(serie = S), outputs = list(out = S),
        params = list(frequencia = I(1L, min = 1L, max = 365L, label = "Nova frequência"),
                      funcao = E("soma", c("soma", "media", "ultimo"), label = "Função")),
        help = .tr_series_ajuda(r"---[
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
]---", r"---[
- **Nova frequência** — observações por ciclo depois de agregar. Tem de
  dividir a frequência da série.
- **Função** — `soma`, `media` ou `ultimo`.
]---", r"---[
Uma série (`series/ts`) mais curta, na nova frequência.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("anual", "series/aggregate", frequencia = 1L, funcao = "soma", from = "pax")
]---", r"---[
`data/group_summarise` para agregações que não são por calendário;
`series/interpolate` para os faltantes antes de somar.
]---")),

      trama::tr_node("series/interpolate", fn = tr_series_interpolate, label = "Interpolar faltantes",
        category = "serie_operar", icon = icone("pipette"),
        description = "Preenche os faltantes pela tendência e pela sazonalidade da série.",
        inputs = list(serie = S), outputs = list(out = S),
        help = .tr_series_ajuda(r"---[
Estima os valores faltantes a partir da própria série: numa série sem
sazonalidade, por interpolação linear entre os vizinhos; numa sazonal, pela
decomposição STL — o julho faltante sai com cara de julho, a partir da
tendência daquele ano e do padrão dos outros julhos (`forecast::na.interp`).

Existe ao lado do `data/replace_na` porque faz outra coisa. Aquele põe uma
CONSTANTE, e numa série um zero no lugar do julho derruba a sazonalidade
inteira que os nós seguintes vão estimar. Este nó estima.

Decomposição, testes de raiz unitária e Holt-Winters recusam série com
faltante, apontando para cá.

Série sem faltante passa intacta.
]---", r"---[
Nenhum.
]---", r"---[
Uma série (`series/ts`) do mesmo tamanho, sem faltantes.
]---", r"---[
tr_flow(reg) |>
  tr_add("pres", "series/example", dataset = "presidents") |>
  tr_add("cheia", "series/interpolate", from = "pres") |>
  tr_add("stl", "series/stl", from = "cheia")
]---", r"---[
`data/replace_na` para preencher com constante; `series/window` para cortar um
trecho com buraco em vez de inventá-lo.
]---")),

      trama::tr_node("series/detrend", fn = tr_series_detrend, label = "Tirar tendência",
        category = "serie_operar", icon = icone("trending-down"),
        description = "Estima a tendência e a subtrai da série, mantendo a sazonalidade.",
        inputs = list(serie = S), outputs = list(out = S),
        params = list(metodo = E("linear", c("linear", "polinomial", "loess", "diferenca"),
                                 label = "Método"),
                      grau = I(2L, min = 2L, max = 5L, label = "Grau (polinomial)"),
                      suavidade = trama::tr_param_num(0.75, min = 0.05, max = 1, step = 0.05,
                                                      label = "Suavidade (loess)")),
        help = .tr_series_ajuda(r"---[
Estima a tendência da série e a TIRA, deixando o resto — sazonalidade
incluída. É o "ajusto uma reta e subtraio" feito num nó só, com a tendência
escolhida às claras:

- **linear** — a reta de mínimos quadrados em t. A saída tem média zero e
  inclinação zero por construção.
- **polinomial** — um polinômio de grau 2 a 5 em t, para tendência que curva.
  Graus altos acompanham demais as pontas; comece pelo 2.
- **loess** — regressão local, sem forma imposta. A **Suavidade** é a fração
  da série que entra em cada ajuste local: perto de 1, uma curva larga; perto
  de 0,1, uma que segue até a sazonalidade (e aí a tira junto).
- **diferenca** — `x[t] − x[t−1]`, sem modelo nenhum. A série perde a 1ª
  observação e começa um período depois. É a mesma conta de `series/diff`
  simples; está aqui para comparar com os outros métodos no mesmo card.

A tendência estimada vai junto com a série, como atributo `tendencia` —
no console, `attr(saida, "tendencia")` —, e `saída + tendência` devolve a
série original.

A sazonalidade FICA na saída. Para tirar as duas, use `series/component` com
`resto`; para tirar só a sazonalidade, `dessazonalizada`. A reta daqui é
estimada sem olhar a sazonalidade; com anos completos, é a mesma de
`series/regression` de grau 1, e com anos incompletos difere um pouco — lá a
reta e as dummies sazonais são estimadas juntas.

Faltantes não impedem o ajuste (exceto na diferença, onde propagam): a
tendência é estimada com os valores observados, e a saída tem NA onde a série
tinha.
]---", r"---[
- **Método** — `linear`, `polinomial`, `loess` ou `diferenca`.
- **Grau (polinomial)** — de 2 a 5. Só vale para `polinomial`.
- **Suavidade (loess)** — maior que 0 e até 1. Só vale para `loess`.
]---", r"---[
Uma série (`series/ts`) sem a tendência, no mesmo tempo da original (um
período mais curta na diferença), com a tendência estimada no atributo
`tendencia`.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("sem", "series/detrend", metodo = "linear", from = "pax") |>
  tr_add("g", "series/plot", from = "sem")
]---", r"---[
`series/component` com `sem_tendencia`, para a mesma conta a partir de uma
decomposição; `series/diff` para diferenças de ordem maior ou sazonais;
`series/moving_average` para ver a tendência sem tirá-la.
]---")),

# ---- Decompor ---------------------------------------------------------------

      trama::tr_node("series/decompose", fn = tr_series_decompose, label = "Decomposição clássica",
        category = "serie_decompor", icon = icone("layers-2"),
        description = "Separa tendência, sazonalidade e resto por médias móveis.",
        inputs = list(serie = S), outputs = list(out = D),
        params = list(tipo = E("aditiva", c("aditiva", "multiplicativa"), label = "Tipo")),
        help = .tr_series_ajuda(r"---[
Separa a série em três componentes, pelo método que se ensina primeiro:

1. a TENDÊNCIA é a média móvel centrada de ordem igual ao ciclo;
2. o SAZONAL é a média, estação por estação, do que sobra da série sem a
   tendência — um valor para cada mês, repetido todos os anos;
3. o RESTO é o que sobra.

Na **aditiva**, `série = tendência + sazonal + resto`: o efeito de dezembro é
"+40 passageiros". Na **multiplicativa**, `série = tendência × sazonal ×
resto`: o efeito de dezembro é "×1,2" — e é a certa quando a oscilação cresce
com o nível, como em `AirPassengers`.

A clássica tem dois limites que a STL não tem: o sazonal é IDÊNTICO em todos os
anos, e as pontas da tendência e do resto saem em branco (meio ciclo em cada
lado). Por isso a STL é a de uso; esta é a de entender.

Pede série sazonal (frequência maior que 1), com pelo menos dois ciclos.
O bloco não aceita faltantes, e a multiplicativa pede valores positivos.

O resumo do card traz a FORÇA da tendência e da sazonalidade, de 0 a 1
(Hyndman & Athanasopoulos): acima de ~0,6, o componente é real; perto de 0, é
ruído.
]---", r"---[
- **Tipo** — `aditiva` ou `multiplicativa`.
]---", r"---[
Uma decomposição (`series/decomposition`): o card mostra os quatro painéis.
`series/component` tira um componente como série; ligada a um nó da `data`,
vira tabela com `tempo`, `observado`, `tendencia`, `sazonal` e `resto`.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("dec", "series/decompose", tipo = "multiplicativa", from = "pax") |>
  tr_add("dessaz", "series/component", componente = "dessazonalizada", from = "dec")
]---", r"---[
`series/stl`, a decomposição que se usa na prática; `series/component` para
extrair um componente; `series/plot_decomposition` para escolher proporção e
título do gráfico; `series/regression`, que estima os mesmos componentes e, por
estimá-los, dá coeficiente e p-valor a cada um.
]---")),

      trama::tr_node("series/stl", fn = tr_series_stl, label = "Decomposição STL",
        category = "serie_decompor", icon = icone("layers"),
        description = "Decomposição por suavização local: sazonalidade que muda devagar, robusta a outlier.",
        inputs = list(serie = S), outputs = list(out = D),
        params = list(janela_sazonal = I(0L, min = 0L, max = 999L, label = "Janela sazonal"),
                      robusta = B(FALSE, label = "Robusta")),
        help = .tr_series_ajuda(r"---[
Separa tendência, sazonalidade e resto por regressão local (loess) — o
método STL de Cleveland et al. (1990). Contra a clássica, ele:

- deixa a sazonalidade MUDAR com os anos, devagar;
- não perde as pontas: tendência e resto existem na série inteira;
- com **Robusta**, não deixa um mês estranho (uma greve, um apagão) puxar a
  sazonalidade de todos os outros anos — o mês vai parar no resto, que é onde
  se procura por ele.

### Janela sazonal

É quantos anos entram na suavização de cada estação. **0 é "periódica"**: o
mesmo padrão em todos os anos, como na clássica. Um número ímpar maior ou
igual a 7 deixa o padrão variar — quanto menor, mais depressa. 7 é o mínimo
que o método aceita; 13 ou 21 são escolhas comuns para séries longas.

A STL é só aditiva. Para sazonalidade multiplicativa, transforme com log antes
(`series/transform`) e a decomposição do log é aditiva.

Pede série sazonal, com pelo menos dois ciclos, e não aceita faltantes.
]---", r"---[
- **Janela sazonal** — 0 para periódica; senão, ímpar maior ou igual a 7.
- **Robusta** — pesos que ignoram outlier na estimação.
]---", r"---[
Uma decomposição (`series/decomposition`), com a força de tendência e de
sazonalidade no resumo do card.
]---", r"---[
tr_flow(reg) |>
  tr_add("co2", "series/example", dataset = "co2") |>
  tr_add("stl", "series/stl", janela_sazonal = 13L, robusta = TRUE, from = "co2") |>
  tr_add("resto", "series/component", componente = "resto", from = "stl") |>
  tr_add("acf", "series/acf", from = "resto")
]---", r"---[
`series/decompose` para a clássica; `series/regression` para a paramétrica, a
única que dá coeficiente e p-valor por componente; `series/transform` para
decompor o log; `series/component` para extrair um componente;
`series/interpolate` quando a série tem faltantes.
]---")),

      trama::tr_node("series/component", role = "leitura", fn = tr_series_component, label = "Componente",
        category = "serie_decompor", icon = icone("square-split-horizontal"),
        description = "Tira um componente da decomposição como série.",
        inputs = list(decomposicao = D), outputs = list(out = S),
        params = list(componente = E("dessazonalizada",
                                     c("tendencia", "sazonal", "resto", "dessazonalizada",
                                       "sem_tendencia"),
                                     label = "Componente")),
        help = .tr_series_ajuda(r"---[
Devolve um dos componentes de uma decomposição como série, para seguir
adiante: modelar a dessazonalizada, testar se o resto é ruído branco, desenhar
só a tendência.

**dessazonalizada** é a série sem o efeito sazonal — o número que se publica
quando se diz "descontado o efeito do mês". Na decomposição multiplicativa ela
é a série DIVIDIDA pelo sazonal (subtrair um fator de 1,2 de um valor na casa
das centenas não tiraria sazonalidade nenhuma); na aditiva, a série menos o
sazonal. O nó sabe qual das duas pela decomposição que recebeu.

**sem_tendencia** é o espelho: a série sem a tendência, com a sazonalidade
dentro — `série − tendência` na aditiva, `série / tendência` na multiplicativa.
É o "estimo a tendência e subtraio" às claras: com `series/regression` de grau 1
na frente, é a série menos a reta ajustada por mínimos quadrados.

Da clássica, tendência e resto (e, por isso, `sem_tendencia`) chegam com NA
nas pontas.
]---", r"---[
- **Componente** — `tendencia`, `sazonal`, `resto`, `dessazonalizada` ou
  `sem_tendencia`.
]---", r"---[
Uma série (`series/ts`) do mesmo tamanho da original.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("stl", "series/stl", from = "pax") |>
  tr_add("resto", "series/component", componente = "resto", from = "stl") |>
  tr_add("rb", "series/ljung_box", from = "resto")
]---", r"---[
`series/stl` e `series/decompose`, que produzem a decomposição;
`series/regression`, que também produz uma — estimada, com coeficiente e
p-valor por componente; `series/ljung_box` para testar o resto.
]---")),

      trama::tr_node("series/regression", fn = tr_series_regression, label = "Regressão dos componentes",
        category = "serie_decompor", icon = icone("trending-up"),
        description = "Estima tendência e sazonalidade por regressão, com coeficientes e p-valores.",
        inputs = list(serie = S, regressor = trama::tr_port(S, required = FALSE)),
        outputs = list(out = R),
        params = list(grau = I(1L, min = 0L, max = 3L, label = "Grau da tendência"),
                      sazonalidade = B(TRUE, label = "Sazonalidade"),
                      contraste = E("soma_zero", c("soma_zero", "categoria_base"),
                                    label = "Contraste")),
        help = .tr_series_ajuda(r"---[
Ajusta um modelo EXPLÍCITO para os componentes da série:

`valor = tendência(t) + sazonal(estação) + erro`

A tendência é um polinômio no tempo (`t`, `t²`, `t³`), e a sazonalidade, uma
variável indicadora por período — onze dummies numa série mensal. É a
decomposição que se faz com regressão, e a diferença para `series/decompose` e
`series/stl` é que aqui cada componente tem COEFICIENTE, erro-padrão e
p-valor: dá para testar se ele existe, em vez de olhar o gráfico e achar.

### O que sai

O card mostra o ajuste como um estatístico o lê: coeficientes, significância,
R² e o F global. Ligado num nó da `data`, vira a tabela de coeficientes
(`termo`, `estimativa`, `erro_padrao`, `estatistica_t`, `p_valor`). Ligado em
`series/component` ou `series/plot_decomposition`, vira decomposição — os
componentes estimados, que somam a série de volta.

### Contraste

Muda como os coeficientes são lidos, e **não** a decomposição:

- **soma_zero** — cada coeficiente sazonal é o desvio daquele período em
  relação à média do ano, e os desvios somam zero. É a convenção da
  decomposição clássica, e o que torna o sazonal daqui comparável ao de lá.
- **categoria_base** — cada coeficiente é a diferença para o primeiro período
  (janeiro, numa mensal). É o `summary()` do R, e o que se vê no livro-texto.

Em ambos, o componente sazonal devolvido é centrado em zero e a tendência
absorve a média — senão a mesma série daria duas decomposições diferentes por
causa de uma escolha de leitura.

### Limites

O bloco não aceita faltantes, e sazonalidade pede frequência maior que 1. Grau 0
com sazonalidade desligada não tem o que estimar, e para o nó em vermelho. A
entrada **regressor** está declarada mas ainda não é usada.

O modelo supõe erro sem autocorrelação, e série temporal quase nunca obedece:
confira o resto com `series/ljung_box` antes de levar os p-valores a sério.

As potências do tempo são cruas (`t`, `t²`, `t³`) e fortemente
correlacionadas entre si: em `series/example` com grau 3, a matriz de desenho
tem número de condição de 7,3 milhões, e `t³` sai com p = 0,46 enquanto o F do
bloco de tendência é esmagador. Do **grau 2 em diante, os p-valores individuais
dos termos de tendência não se leem um a um** — quem quer saber se há tendência
lê o F do bloco, em `series/f_trend`. Os coeficientes sazonais não
sofrem disso.

O nó não prevê, e é de propósito: tendência polinomial fora da amostra é das
extrapolações mais perigosas que existem — um grau 3 dispara para o infinito
logo depois do último ponto. Para prever, `series/arima` ou `series/ets`.
]---", r"---[
- **Grau da tendência** — 0 (sem tendência) a 3.
- **Sazonalidade** — inclui as dummies de período.
- **Contraste** — `soma_zero` ou `categoria_base`.
]---", r"---[
Uma regressão (`series/regression`): o card traz o resumo do ajuste.
`series/f_global`, `series/f_seasonal` e `series/f_trend` testam os blocos;
`series/component` extrai um componente como série; ligada à `data`, vira a
tabela de coeficientes.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("reg", "series/regression", grau = 2L, from = "pax") |>
  tr_add("f", "series/f_global", from = "reg") |>
  tr_add("resto", "series/component", componente = "resto", from = "reg") |>
  tr_add("ruido", "series/ljung_box", from = "resto")
]---", r"---[
`series/f_global`, `series/f_seasonal` e `series/f_trend` para a
significância dos blocos; `series/decompose` e `series/stl` para as
decomposições não paramétricas; `series/transform` para ajustar em log quando a
oscilação cresce com o nível.
]---")),

# ---- Modelar ----------------------------------------------------------------

      trama::tr_node("series/arima", fn = tr_series_arima, label = "ARIMA",
        category = "serie_modelar", icon = icone("sigma"),
        description = "Ajusta um ARIMA sazonal, automático ou com a ordem escolhida.",
        inputs = list(serie = S), outputs = list(out = M),
        params = list(
          automatico = B(TRUE, label = "Automático"),
          sazonal = B(TRUE, label = "Parte sazonal (automático)"),
          constante = B(TRUE, label = "Constante / deriva"),
          p = I(1L, min = 0L, max = 5L, label = "p (AR)"),
          d = I(1L, min = 0L, max = 2L, label = "d (diferenças)"),
          q = I(1L, min = 0L, max = 5L, label = "q (MA)"),
          P = I(0L, min = 0L, max = 2L, label = "P (AR sazonal)"),
          D = I(0L, min = 0L, max = 1L, label = "D (diferença sazonal)"),
          Q = I(0L, min = 0L, max = 2L, label = "Q (MA sazonal)")),
        help = .tr_series_ajuda(r"---[
Ajusta um modelo ARIMA(p,d,q)(P,D,Q)[ciclo]: a série depois de `d` diferenças
simples e `D` sazonais é explicada pelos seus `p` valores anteriores (AR), pelos
`q` erros anteriores (MA), e pelas versões sazonais disso (`P`, `Q`), uma
defasagem de ciclo por vez.

### Automático

Com **Automático** ligado, a ordem é escolhida pelo algoritmo de Hyndman e
Khandakar (`forecast::auto.arima`): testes de raiz unitária decidem `d` e `D`, e
uma busca pelo menor AICc decide o resto. **As seis ordens do card são
ignoradas** — o card não sabe esconder campo, então a regra fica escrita aqui.
O resumo do card mostra a ordem escolhida (`ARIMA(0,1,1)(0,1,1)[12]`), e o
caminho natural é rodar automático, ler, e só então fixar à mão se quiser.

**Parte sazonal** desligada restringe a busca a modelos sem P, D, Q. Só vale no
automático.

### Manual

Com **Automático** desligado, o modelo é exatamente o das seis ordens. P, D ou
Q maiores que zero numa série sem ciclo param o nó em vermelho.

**Constante / deriva** permite a média (sem diferença) ou a deriva (com uma
diferença) — uma tendência linear na previsão. Com duas diferenças ela não é
estimada, porque não é identificável.

Transformação (log, Box-Cox) não é param daqui: é o `series/transform` antes,
visível no fio.

Falha de ajuste ("non-stationary AR part") para o nó com a mensagem do
`forecast` logo abaixo da nossa.
]---", r"---[
- **Automático** — escolhe a ordem sozinho, ignorando as seis ordens.
- **Parte sazonal (automático)** — permite P, D, Q na busca automática.
- **Constante / deriva** — permite média ou deriva no modelo.
- **p**, **d**, **q** — ordens da parte não sazonal (só no manual).
- **P**, **D**, **Q** — ordens da parte sazonal (só no manual).
]---", r"---[
Um modelo (`series/model`). O card mostra os coeficientes; o resumo, o nome do
modelo, o AIC e o desvio dos resíduos.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("log", "series/transform", from = "pax") |>
  tr_add("arima", "series/arima", from = "log") |>
  tr_add("prev", "series/forecast", horizonte = 24L, from = "arima")
]---", r"---[
`series/forecast` para prever; `series/residuals` e `series/ljung_box` para
diagnosticar; `series/ndiffs`, `series/acf` e `series/pacf` para escolher a
ordem à mão; `series/ets` para a alternativa por suavização exponencial.
]---")),

      trama::tr_node("series/ets", fn = tr_series_ets, label = "ETS",
        category = "serie_modelar", icon = icone("waves-horizontal"),
        description = "Suavização exponencial em espaço de estados (erro, tendência, sazonalidade).",
        inputs = list(serie = S), outputs = list(out = M),
        params = list(modelo = P("text", "ZZZ", label = "Modelo", example = "MAM"),
                      amortecida = E("auto", c("auto", "sim", "não"), label = "Tendência amortecida")),
        help = .tr_series_ajuda(r"---[
Ajusta um modelo de suavização exponencial — a família que inclui Holt e
Holt-Winters — na formulação em espaço de estados de Hyndman et al.
(`forecast::ets`), com escolha por AICc e intervalos de previsão de verdade.

### O código do modelo

Três letras, na ordem da literatura:

1. **erro** — `A` aditivo, `M` multiplicativo;
2. **tendência** — `N` nenhuma, `A` aditiva, `M` multiplicativa;
3. **sazonalidade** — `N` nenhuma, `A` aditiva, `M` multiplicativa.

`Z` em qualquer posição deixa o ajuste escolher. `ZZZ` (o padrão) escolhe
tudo; `MAM` fixa erro e sazonalidade multiplicativos com tendência aditiva,
que costuma servir para `AirPassengers`. O resumo do card mostra o modelo
escolhido no mesmo formato (`ETS(M,Ad,M)`, onde `Ad` é a tendência
amortecida), e é esse código que se copia para fixar.

Sazonalidade `A` ou `M` pede série sazonal com dois ciclos, e frequência até
24 — acima disso o ETS não modela sazonalidade (use `Z` ou `N`, ou decomponha
antes). Com `Z`, numa série de frequência maior que 24, a sazonalidade é
simplesmente deixada de fora.

### Tendência amortecida

Uma tendência que perde força ao longo do horizonte, em vez de seguir reta
para sempre. Costuma prever melhor a longo prazo. `auto` deixa o ajuste
decidir; `sim` com tendência `N` é combinação proibida, e o nó para.
]---", r"---[
- **Modelo** — o código de três letras. Obrigatório: em branco, o nó para.
- **Tendência amortecida** — `auto`, `sim` ou `não`.
]---", r"---[
Um modelo (`series/model`), com o nome `ETS(…)` no resumo.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("ets", "series/ets", modelo = "MAM", amortecida = "auto", from = "pax") |>
  tr_add("prev", "series/forecast", from = "ets")
]---", r"---[
`series/holt_winters` para a versão clássica; `series/arima` para a outra
grande família; `series/baseline` para a referência que o modelo tem de bater.
]---")),

      trama::tr_node("series/holt_winters", fn = tr_series_holt_winters, label = "Holt-Winters",
        category = "serie_modelar", icon = icone("trending-up-down"),
        description = "Suavização exponencial clássica com nível, tendência e sazonalidade.",
        inputs = list(serie = S), outputs = list(out = M),
        params = list(tendencia = B(TRUE, label = "Tendência"),
                      sazonalidade = B(TRUE, label = "Sazonalidade"),
                      tipo = E("aditiva", c("aditiva", "multiplicativa"), label = "Sazonalidade do tipo")),
        help = .tr_series_ajuda(r"---[
O método de Holt-Winters como o `stats` o implementa: três equações de
suavização exponencial — nível, tendência e sazonalidade —, com as constantes
α, β e γ escolhidas por mínimos quadrados dos erros de um passo.

É o método que se ensina e se cita, e fica ao lado do `series/ets`, que o
generaliza. Desligar **Tendência** dá a suavização de Holt sem tendência;
desligar também **Sazonalidade** dá a suavização exponencial simples. O efeito
de cada chave aparece na previsão do card seguinte.

Com sazonalidade, pede série sazonal com dois ciclos, e não aceita faltantes.
Não tem AIC (não é ajustado por verossimilhança), então o resumo o mostra em
branco.
]---", r"---[
- **Tendência** — estima a inclinação (β).
- **Sazonalidade** — estima o padrão sazonal (γ).
- **Sazonalidade do tipo** — `aditiva` ou `multiplicativa`.
]---", r"---[
Um modelo (`series/model`).
]---", r"---[
tr_flow(reg) |>
  tr_add("co2", "series/example", dataset = "co2") |>
  tr_add("hw", "series/holt_winters", from = "co2") |>
  tr_add("prev", "series/forecast", horizonte = 36L, from = "hw")
]---", r"---[
`series/ets`, a versão em espaço de estados; `series/forecast` para prever.
]---")),

      trama::tr_node("series/forecast", role = "leitura", fn = tr_series_forecast, label = "Prever",
        category = "serie_modelar", icon = icone("trending-up"),
        description = "Prevê h períodos à frente com um modelo ajustado, com intervalos de 80 e 95%.",
        inputs = list(modelo = M), outputs = list(out = F),
        params = list(horizonte = I(12L, min = 1L, max = 1000L, label = "Horizonte")),
        help = .tr_series_ajuda(r"---[
Projeta o modelo **Horizonte** períodos à frente, com a previsão pontual e os
intervalos de 80% e 95%.

É um nó separado do ajuste por causa do custo: ajustar é o caro (um ARIMA
automático testa dezenas de modelos), prever é aritmética. Trocar o horizonte
de 12 para 24 recomputa só este card.

Os intervalos são o ponto. Uma previsão sem leque diz "vai dar 450"; com o
leque, diz "entre 390 e 520 com 95% de chance" — e o leque ABRE com o
horizonte, que é a informação mais honesta que um modelo dá sobre o próprio
limite. Eles supõem resíduos sem autocorrelação e com variância constante:
confira com `series/residuals` → `series/ljung_box` antes de confiar.

Os níveis são sempre 80 e 95, porque são os que o gráfico e a tabela nomeiam
(`li_80`, `ls_95`); um fluxo que filtra por `ls_95` não quebra.

Série transformada é prevista na escala transformada.
]---", r"---[
- **Horizonte** — quantos períodos prever.
]---", r"---[
Uma previsão (`series/forecast`): o card mostra histórico e leque. Ligada a um
nó da `data`, vira tabela com `tempo`, `previsto`, `li_80`, `ls_80`, `li_95` e
`ls_95` — é por aí que se grava num CSV.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("ets", "series/ets", from = "pax") |>
  tr_add("prev", "series/forecast", horizonte = 24L, from = "ets") |>
  tr_add("csv", "data/write_csv", path = "previsao.csv", from = "prev")
]---", r"---[
`series/arima`, `series/ets` e `series/holt_winters` para o modelo;
`series/baseline` para a referência; `series/accuracy` para o erro;
`series/plot_forecast` para escolher o histórico mostrado; `data/write_csv`
para exportar.
]---")),

      trama::tr_node("series/baseline", fn = tr_series_baseline, label = "Previsão de referência",
        category = "serie_modelar", icon = icone("repeat"),
        description = "Média, ingênuo, ingênuo sazonal ou deriva: o que qualquer modelo tem de bater.",
        inputs = list(serie = S), outputs = list(out = F),
        params = list(metodo = E("ingênuo sazonal", c("média", "ingênuo", "ingênuo sazonal", "deriva"),
                                 label = "Método"),
                      horizonte = I(12L, min = 1L, max = 1000L, label = "Horizonte")),
        help = .tr_series_ajuda(r"---[
Previsões que não estimam nada, e é por isso que servem: são a régua. Um
ARIMA que erra mais que "o mesmo mês do ano passado" não merece o card — e sem
a referência ao lado ninguém fica sabendo.

- **média** — todo futuro é a média da série inteira.
- **ingênuo** — todo futuro é o último valor observado. Imbatível em passeio
  aleatório (cotações, câmbio).
- **ingênuo sazonal** — cada mês futuro é o mesmo mês do último ano. A
  referência certa para série sazonal; pede ciclo.
- **deriva** — o último valor mais a inclinação média da série: uma reta do
  primeiro ao último ponto, prolongada.

Os intervalos saem como os de um modelo, e o `series/accuracy` os compara nos
mesmos termos.
]---", r"---[
- **Método** — `média`, `ingênuo`, `ingênuo sazonal` ou `deriva`.
- **Horizonte** — quantos períodos prever.
]---", r"---[
Uma previsão (`series/forecast`), igual à de um modelo.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("treino", "series/window", fim = "1958", from = "pax") |>
  tr_add("ref", "series/baseline", metodo = "ingênuo sazonal", horizonte = 24L,
         from = "treino") |>
  tr_add("erro", "series/accuracy", from = "ref") |>
  tr_link("pax", "erro:real")
]---", r"---[
`series/accuracy` para comparar com o modelo; `series/forecast` para a previsão
de um modelo ajustado; `data/bind_rows` para juntar as tabelas de erro de
vários métodos numa só.
]---")),

      trama::tr_node("series/residuals", role = "leitura", fn = tr_series_residuals, label = "Resíduos",
        category = "serie_modelar", icon = icone("scan-line"),
        description = "Os resíduos do modelo, como série, para diagnóstico.",
        inputs = list(modelo = M), outputs = list(out = S),
        help = .tr_series_ajuda(r"---[
O que o modelo NÃO explicou: a série menos o valor ajustado de um passo à
frente. Sai como série para que o diagnóstico seja feito com os mesmos nós de
qualquer série:

- `series/ljung_box` — sobrou autocorrelação? Se sim, o modelo deixou
  estrutura para trás, e os intervalos de previsão estão estreitos demais;
- `series/acf` — em que defasagem ela sobrou (uma barra na 12 é sazonalidade
  mal modelada);
- `view/histogram` (a série vira tabela sozinha no fio, com a coluna `valor`) —
  os resíduos parecem normais? Os intervalos supõem que sim.

Um modelo bom deixa resíduos sem padrão: média zero, variância constante, sem
autocorrelação.
]---", r"---[
Nenhum.
]---", r"---[
Uma série (`series/ts`) dos resíduos. No Holt-Winters ela começa depois do
primeiro ciclo, que o método usa para iniciar.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("arima", "series/arima", from = "pax") |>
  tr_add("res", "series/residuals", from = "arima") |>
  tr_add("lb", "series/ljung_box", graus = 2L, from = "res") |>
  tr_add("acf", "series/acf", from = "res")
]---", r"---[
`series/ljung_box` e `series/acf` para o diagnóstico; `view/histogram` para
a forma da distribuição.
]---")),

      trama::tr_node("series/accuracy", role = "avaliacao", fn = tr_series_accuracy, label = "Acurácia",
        category = "serie_modelar", icon = icone("square-sigma"),
        description = "Medidas de erro da previsão: no treino e, com a série real, no teste.",
        inputs = list(previsao = F, real = trama::tr_port(S, required = FALSE)),
        outputs = list(out = T),
        help = .tr_series_ajuda(r"---[
Calcula as medidas de erro de uma previsão:

- **rmse** — raiz do erro quadrático médio, na unidade da série. Pune erro
  grande.
- **mae** — erro absoluto médio, na unidade da série.
- **mape** — erro percentual absoluto médio. Explode perto de zero.
- **mase** — erro absoluto escalado pelo erro do ingênuo (sazonal) no treino.
  Abaixo de 1, o modelo bate a referência; acima, perde. É a medida que
  compara séries diferentes.
- **me**, **mpe** — erro médio e percentual médio: o VIÉS (sistematicamente
  acima ou abaixo).
- **acf1** — autocorrelação dos erros na defasagem 1.

### Treino e teste

Sem nada na entrada **real**, sai só a linha do TREINO: erro de um passo à
frente dentro da amostra, que é otimista por construção — o modelo viu esses
dados.

Com a série inteira ligada em **real** (e o modelo ajustado num recorte dela,
por `series/window`), sai também a linha do TESTE: o erro nos períodos que o
modelo não viu. É ela que diz se a previsão presta.

Série real que não cobre nenhum período da previsão para o nó em vermelho: é
quase sempre o fio ligado na série de treino por engano, e uma tabela só com o
treino pareceria resposta.

A tabela sai com o nome do método em cada linha: várias acurácias num
`data/bind_rows` viram a comparação de modelos.
]---", r"---[
Nenhum. Duas entradas: **previsao** (obrigatória) e **real** (opcional, a
série com os períodos previstos).
]---", r"---[
Uma tabela (`data/table`) com uma linha por conjunto (`treino`, `teste`) e as
colunas `metodo`, `conjunto`, `me`, `rmse`, `mae`, `mpe`, `mape`, `mase`,
`acf1` (e `theil_s_u` no teste).
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("treino", "series/window", fim = "1958", from = "pax") |>
  tr_add("ets", "series/ets", from = "treino") |>
  tr_add("prev", "series/forecast", horizonte = 24L, from = "ets") |>
  tr_add("erro", "series/accuracy", from = "prev") |>
  tr_link("pax", "erro:real")
]---", r"---[
`series/window` para separar treino e teste; `series/baseline` para a
referência; `data/bind_rows` para comparar vários modelos numa tabela.
]---")),

# ---- Testar: raiz unitária --------------------------------------------------

      trama::tr_node("series/adf", fn = tr_series_adf, label = "ADF",
        category = "serie_raiz", icon = icone("test-tube"),
        description = "Dickey-Fuller Aumentado: a série tem raiz unitária?",
        inputs = list(serie = S), outputs = list(out = TE),
        params = list(
          deterministico = E("constante", c("constante", "tendência"),
                             label = "Termos determinísticos"),
          defasagens = I(0L, min = 0L, max = 50L, label = "Defasagens")),
        help = .tr_series_ajuda(r"---[
Testa se a série tem RAIZ UNITÁRIA — a não estacionariedade que faz o nível
passear sem voltar, e que se resolve diferenciando.

### A leitura é invertida

A hipótese nula aqui é a RAIZ UNITÁRIA. Quem está acostumado a "p-valor
pequeno quer dizer que achei alguma coisa" lê este teste ao contrário:
rejeitar H0 é concluir ESTACIONÁRIA.

Não rejeitar não prova raiz unitária — com amostra pequena o teste deixa de
rejeitar por falta de poder. Por isso o ADF anda em par com um teste de
hipótese nula oposta (H0 = estacionária): é quando os dois lados concordam que
a conclusão tem chão. Se discordam, a série está no limite, e diferenciar é o
lado seguro.

### O que sai

A estatística t, a tabela de valores críticos a 10%, 5% e 1% e a conclusão em
palavras. O `urca` só publica a tabela: o `p_valor` sai em branco, e não
interpolado a partir dela — seria inventar precisão que o pacote não dá. A
decisão a 5% vem do valor crítico.

**Termos determinísticos**: `constante` para série que oscila em torno de um
nível; `tendência` para série que pode ser estacionária em torno de uma reta.

**Defasagens**: o teto do termo aumentado. `0` usa a regra de sempre (a raiz
cúbica de n - 1); dentro do teto, quem escolhe é o AIC.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.
]---", r"---[
- **Termos determinísticos** — `constante` ou `tendência`.
- **Defasagens** — teto de defasagens; `0` para a regra automática.
]---", r"---[
Um teste (`series/test`). Ligado numa entrada de tabela, ele vira UMA linha de
relatório: um `data/bind_rows` junta vários testes num só.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("log", "series/transform", from = "pax") |>
  tr_add("d", "series/diff", from = "log") |>
  tr_add("adf", "series/adf", from = "d")
]---", r"---[
`series/kpss`, o teste de hipótese nula oposta, que se lê ao lado deste;
`series/phillips_perron`, a mesma hipótese nula por outro caminho;
`series/zivot_andrews` quando a série pode ter uma QUEBRA: este bloco aqui
tende a não rejeitar nesse caso, e lá a quebra é estimada e o veredito muda;
`series/ndiffs` para saber quantas diferenças a série pede; `series/diff` para
fazê-las.
]---", teste = TRUE)),

      trama::tr_node("series/kpss", fn = tr_series_kpss, label = "KPSS",
        category = "serie_raiz", icon = icone("test-tubes"),
        description = "KPSS: a série é estacionária? (H0 é a estacionariedade)",
        inputs = list(serie = S), outputs = list(out = TE),
        params = list(
          deterministico = E("constante", c("constante", "tendência"),
                             label = "Termos determinísticos")),
        help = .tr_series_ajuda(r"---[
Testa se a série é ESTACIONÁRIA em torno de um nível (ou de uma reta, com
tendência). É o espelho do `series/adf`, e o único teste da coleção cuja
hipótese nula é a estacionariedade.

### A leitura, e por que ela é o contrário do ADF

| teste | H0 | rejeitar H0 quer dizer |
|---|---|---|
| `series/adf` | raiz unitária | estacionária |
| `series/phillips_perron` | raiz unitária | estacionária |
| KPSS | estacionária | não estacionária |

Rejeitar aqui é concluir NÃO estacionária. A rejeição vem da cauda de cima: a
estatística grande é a que derruba H0.

Com amostra pequena, qualquer um desses testes "não rejeita" por falta de
poder — o ADF por falta de evidência contra a raiz unitária, o KPSS por falta
de evidência contra a estacionariedade. Por isso eles andam em par: é a
CONCORDÂNCIA dos dois (o ADF rejeita, o KPSS não) que dá chão à conclusão. Se
discordam, a série está no limite, e diferenciar é o lado seguro.

### O que sai

A estatística `eta`, a tabela de valores críticos a 10%, 5% e 1% e a conclusão
em palavras. O `urca` só publica a tabela: o `p_valor` sai em branco, e não
interpolado a partir dela. A decisão a 5% vem do valor crítico.

**Termos determinísticos**: `constante` para série que oscila em torno de um
nível; `tendência` para série que pode ser estacionária em torno de uma reta —
a tabela de críticos é outra em cada caso.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.
]---", r"---[
- **Termos determinísticos** — `constante` ou `tendência`.
]---", r"---[
Um teste (`series/test`). Ligado numa entrada de tabela, ele vira UMA linha de
relatório: um `data/bind_rows` junta o KPSS e o ADF no mesmo quadro.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("log", "series/transform", from = "pax") |>
  tr_add("d", "series/diff", from = "log") |>
  tr_add("kpss", "series/kpss", from = "d") |>
  tr_add("adf", "series/adf", from = "d")
]---", r"---[
`series/adf` e `series/phillips_perron`, os testes de hipótese nula oposta —
é com um deles ao lado que este se lê; `series/ndiffs` para quantas diferenças
a série pede; `series/diff` para fazê-las.
]---", teste = TRUE)),

      trama::tr_node("series/phillips_perron", fn = tr_series_phillips_perron,
        label = "Phillips-Perron",
        category = "serie_raiz", icon = icone("flask-conical"),
        description = "Phillips-Perron: a série tem raiz unitária?",
        inputs = list(serie = S), outputs = list(out = TE),
        params = list(),
        help = .tr_series_ajuda(r"---[
Testa se a série tem RAIZ UNITÁRIA, a mesma hipótese nula do `series/adf`.
Muda o caminho: em vez de acrescentar defasagens à regressão para limpar a
autocorrelação dos resíduos, o Phillips-Perron corrige a própria estatística.
Por isso não tem o parâmetro de defasagens do ADF.

### A leitura é invertida

Rejeitar H0 é concluir ESTACIONÁRIA. E, como no ADF, não rejeitar não prova
raiz unitária: com amostra pequena o teste deixa de rejeitar por falta de
poder. O par natural é o `series/kpss`, de hipótese nula oposta (H0 =
estacionária) — é quando os dois concordam que a conclusão tem chão.

### O p-valor preso na borda

O p-valor sai de uma tabela interpolada que vai de 0,01 a 0,99. Fora dela, o
valor é PRESO na borda, sem aviso nenhum: um `0,01` quer dizer "0,01 ou
menos", e um `0,99`, "0,99 ou mais". Quando isso acontece, a `nota` do teste
diz. A decisão a 5% não muda com isso — a ressalva está lá para quem for
reportar o número.

Os termos determinísticos não são parâmetro: o teste do `stats` roda sempre
com constante e tendência.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.
]---", r"---[
Nenhum.
]---", r"---[
Um teste (`series/test`). Ligado numa entrada de tabela, ele vira UMA linha de
relatório: um `data/bind_rows` junta vários testes num só quadro.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("log", "series/transform", from = "pax") |>
  tr_add("d", "series/diff", from = "log") |>
  tr_add("pp", "series/phillips_perron", from = "d") |>
  tr_add("kpss", "series/kpss", from = "d")
]---", r"---[
`series/adf`, o teste da mesma hipótese nula por outro caminho; `series/kpss`,
o de hipótese nula oposta; `series/ndiffs` para quantas diferenças a série
pede.
]---", teste = TRUE)),

      trama::tr_node("series/zivot_andrews", fn = tr_series_zivot_andrews,
        label = "Zivot-Andrews",
        category = "serie_raiz", icon = icone("split"),
        description = "Zivot-Andrews: raiz unitária, com a quebra achada pelo próprio teste?",
        inputs = list(serie = S), outputs = list(out = TE),
        params = list(
          mudanca = E("ambas", c("nível", "inclinação", "ambas"),
                      label = "O que quebra"),
          defasagens = I(0L, min = 0L, max = 50L, label = "Defasagens")),
        help = .tr_series_ajuda(r"---[
Testa se a série tem RAIZ UNITÁRIA admitindo que ela possa ter sofrido uma
QUEBRA ESTRUTURAL — e achando a data da quebra sozinho.

### Por que este bloco existe

É a resposta a um viés dos vizinhos, e é o motivo de a dissertação trazer o
teste. O `series/adf` e o `series/phillips_perron` tendem a NÃO REJEITAR quando
a série tem quebra estrutural (MARGARIDO, 2001): uma série que é estacionária
em torno de uma média que DESLOCOU no meio do caminho se parece muito com um
passeio aleatório para os dois, e eles não rejeitam a raiz unitária.

Então a leitura prática é esta: quando o `series/adf` não rejeita
numa série em que você desconfia de uma ruptura, este é o bloco que confere se
a quebra era a explicação.

Medido, numa série de 120 observações feita de 60 pontos em torno de zero e 60
em torno de seis — estacionária dos dois lados, com um degrau no meio:

```
bloco                     estatística   decisão a 5%   conclusão
series/adf                     -1.57    não rejeita    não há evidência contra a raiz unitária
series/zivot_andrews          -11.00    rejeita        estacionária, quebra na obs. 60
```

O ADF erra a pergunta inteira; este acha o degrau na observação exata em que
ele foi posto.

### A quebra é ESTIMADA, não informada

Não existe parâmetro de data, e é de propósito: o teste percorre os cortes
entre 15% e 85% da série, ajusta a regressão em cada um e fica com aquele que
MAIS favorece a estacionariedade. A data sai como resultado, não como entrada — é
o que separa este teste de um ADF com dummy escrita à mão.

A janela segue o teste publicado por Zivot e Andrews, de onde vem a tabela de
valores críticos, e não o `urca`, que varre todos os cortes. Sem ela a quebra
apontada às vezes cai na ponta da série, com um trecho de uma observação só, e
essa data não se lê como quebra de nada. Aparar não muda de forma sensível o
quanto o teste rejeita: ela conserta a data, não o excesso de rejeição da série
curta descrito mais abaixo.

A contrapartida é que, escolhendo o corte mais favorável entre muitos, ele
precisa de valores críticos bem mais severos que os do ADF. São os da tabela
abaixo, e é por isso que um t de -4.5 que rejeitaria no ADF não rejeita aqui.

### A leitura é invertida

Como no ADF, H0 é a RAIZ UNITÁRIA, e sem quebra — a quebra só entra na
alternativa (eq. 3.35 a 3.37 da dissertação): rejeitar é concluir ESTACIONÁRIA
(em torno de um nível que quebrou). A cauda é a de BAIXO — o t bem negativo é o que
derruba H0.

E não rejeitar aqui diz MAIS do que não rejeitar no ADF: a explicação
alternativa mais comum, a quebra, já foi dada de graça à série e mesmo assim
não bastou. Por isso a conclusão sai escrita "mesmo admitindo uma quebra".

### Cada modelo tem a SUA tabela

O que você declara em **O que quebra** não muda só a regressão: muda os valores
críticos junto. São três tabelas diferentes, e o bloco lê a do modelo que rodou.

```
o que quebra        1%      5%     10%     equação
nível            -5.34   -4.80   -4.58     3.35
inclinação       -4.93   -4.42   -4.11     3.36
nível e inclinação -5.57  -5.08   -4.82     3.37
```

A Tabela 3.2 da dissertação publica só a última linha, que é a do modelo
completo — o default daqui. Escolha `nível` para um degrau (a série pula e
segue no mesmo ritmo), `inclinação` para uma virada de tendência (a série muda
de inclinação sem pular), `ambas` quando não souber ou quando as duas coisas
mudarem. No exemplo do degrau acima, `inclinação` sai com t de -2.68 e NÃO
rejeita: o modelo errado não enxerga a quebra certa.

### O que sai

A estatística t, a tabela de valores críticos a 10%, 5% e 1% e a conclusão em
palavras; a posição da quebra e o rótulo do período vão nas colunas extras. O
`urca` publica só a tabela, então o `p_valor` sai em branco — interpolar um
p-valor a partir dela seria inventar precisão que o pacote não dá. A decisão a
5% vem do valor crítico.

**Defasagens**: quantas diferenças defasadas entram na regressão. `0` usa a
regra de sempre (a raiz cúbica de n - 1). Diferente do `series/adf`, aqui não há
escolha por AIC: o número vale como foi dado. Defasagem demais numa série curta
deixa a regressão da quebra sem graus de liberdade, e nesse caso o bloco recusa
dizendo qual é o máximo — sem isso o `urca` morreria com um erro cru do R.

### Precisa de série, e de série que chegue

O bloco pede pelo menos 20 observações, e o piso é mais alto que os 12 dos
irmãos de categoria porque este teste gasta mais: a dummy da quebra (duas, no
modelo completo) sai dos mesmos graus de liberdade, e ainda se escolhe o melhor
entre n - 1 cortes.

O número foi medido. Sob passeio aleatório — onde H0 é VERDADEIRA e toda
rejeição é erro — o teste rejeita a 5% em 68% das amostras com n = 11, 24% com
n = 14, 18% com n = 18, contra 14% com n = 20. Abaixo de vinte o bloco
devolveria "estacionária com quebra" para série com raiz unitária de verdade na
maioria das vezes.

O excesso não acaba em vinte, só deixa de ser catastrófico: a taxa fica em torno
de 10% a 14% até n = 30, e mesmo depois não chega aos 5% nominais: medido no
modelo completo, fica em torno de 7% a 10% com n = 40 e perto de 7% com n = 100.
Entre vinte e quarenta, onde o excesso é maior, a `nota` do teste avisa, e um "rejeita H0" apertado aí pede
confirmação — de preferência olhando a série no `series/plot`, para ver se a
quebra que ele apontou existe.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.
]---", r"---[
- **O que quebra** — `nível`, `inclinação` ou `ambas`; muda a regressão e a
  tabela de valores críticos junto.
- **Defasagens** — quantas diferenças defasadas entram; `0` para a regra
  automática.
]---", r"---[
Um teste (`series/test`), com a posição da quebra e o rótulo do período em
colunas extras. Ligado numa entrada de tabela, ele vira UMA linha de relatório:
um `data/bind_rows` põe este e o `series/adf` lado a lado, que é como a
diferença entre os dois se lê.
]---", r"---[
tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("za", "series/zivot_andrews", from = "nilo") |>
  tr_add("adf", "series/adf", from = "nilo")
]---", r"---[
`series/adf`, o teste que este corrige — é a discordância entre os dois que
diz que havia uma quebra; `series/kpss`, de hipótese nula oposta;
`series/pettitt`, que localiza um ponto de mudança sem supor modelo nenhum — os
dois respondem perguntas vizinhas, e a dissertação usa localização de quebra e
teste de tendência juntos; `series/plot` para OLHAR a quebra que o teste
apontou antes de acreditar nela.
]---", teste = TRUE)),

# ---- Testar: autocorrelação -------------------------------------------------

      trama::tr_node("series/ljung_box", fn = tr_series_ljung_box, label = "Ljung-Box",
        category = "serie_autocorr", icon = icone("audio-waveform"),
        description = "Ljung-Box: a série é ruído branco, ou sobrou autocorrelação?",
        inputs = list(serie = S), outputs = list(out = TE),
        params = list(defasagens = I(0L, min = 0L, max = 500L, label = "Defasagens"),
                      graus = I(0L, min = 0L, max = 20L, label = "Graus do modelo")),
        help = .tr_series_ajuda(r"---[
Testa se as primeiras autocorrelações da série são, EM CONJUNTO, zero — isto
é, se a série é ruído branco até aquela defasagem. H0 é "as autocorrelações
até a defasagem usada são nulas": p-valor pequeno quer dizer que HÁ
autocorrelação. Não rejeitar não prova ruído branco: a conclusão sai "não há
evidência de autocorrelação", e dependência além da última defasagem o teste
não olha.

O uso típico é nos resíduos de um modelo (`series/residuals`): se sobrou
autocorrelação, o modelo deixou estrutura para trás e ainda há o que ajustar.

### Este ou o Box-Pierce?

Este. Os dois testam a mesma hipótese nula, com a mesma estatística; o
Ljung-Box é a versão CORRIGIDA para amostra pequena, e é a que se reporta. O
`series/box_pierce` é a fórmula original de 1970, e está na coleção para
comparar com trabalho antigo que a usou — não para escolher no cara ou coroa.

### Os graus descontados

Nos resíduos de um modelo, `graus` é o número de parâmetros estimados —
p + q + P + Q de um ARIMA (o `print` no card do modelo mostra os coeficientes;
conte-os). Cada parâmetro ajustado consome um grau de liberdade, e sem
descontá-los o teste fica generoso demais: aceita ruído branco onde há
estrutura. Numa série qualquer, que não é resíduo de nada, deixe 0.

**Defasagens**: quantas autocorrelações entram. `0` é a regra de Hyndman — 10
para série não sazonal, duas vezes o ciclo para sazonal, e nunca mais que um
quinto da série. As defasagens usadas e os graus descontados saem na `nota`.

### Faltantes

Este bloco ACEITA série com faltante, ao contrário dos três blocos de raiz
unitária (`series/adf`, `series/kpss`, `series/phillips_perron`) e do
`series/ndiffs`, que os recusam. Aqui eles são ignorados na conta: o tamanho
usado é o de observações VÁLIDAS, e o bloco pede pelo menos doze delas — com
menos, o nó sai em vermelho, em vez de um veredito calculado sobre uma
defasagem só. É deliberado: o uso típico é diagnosticar resíduo, e resíduo vem
com buraco sempre que o modelo perdeu observação — exigir série cheia recusaria
justamente o caso mais comum.
]---", r"---[
- **Defasagens** — 0 para automático.
- **Graus do modelo** — parâmetros ajustados, a descontar. Tem de ser menor
  que as defasagens.
]---", r"---[
Um teste (`series/test`). Ligado numa entrada de tabela, ele vira UMA linha de
relatório: um `data/bind_rows` junta vários testes num só quadro.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("arima", "series/arima", automatico = FALSE, p = 0L, d = 1L, q = 1L,
         P = 0L, D = 1L, Q = 1L, from = "pax") |>
  tr_add("res", "series/residuals", from = "arima") |>
  tr_add("lb", "series/ljung_box", graus = 2L, from = "res")
]---", r"---[
`series/residuals` para testar um modelo; `series/acf` para ver em que
defasagem está a autocorrelação; `series/box_pierce`, o mesmo teste sem a
correção de amostra pequena.
]---", teste = TRUE)),

      trama::tr_node("series/box_pierce", fn = tr_series_box_pierce, label = "Box-Pierce",
        category = "serie_autocorr", icon = icone("activity"),
        description = "Box-Pierce: a série é ruído branco? (a fórmula original, sem correção)",
        inputs = list(serie = S), outputs = list(out = TE),
        params = list(defasagens = I(0L, min = 0L, max = 500L, label = "Defasagens"),
                      graus = I(0L, min = 0L, max = 20L, label = "Graus do modelo")),
        help = .tr_series_ajuda(r"---[
Testa se as primeiras autocorrelações da série são, EM CONJUNTO, zero — se a
série é ruído branco até aquela defasagem. H0 é "as autocorrelações até a
defasagem usada são nulas": p-valor pequeno quer dizer que HÁ autocorrelação,
e não rejeitar diz só que não há evidência dela.

### Provavelmente você quer o `series/ljung_box`

Os dois testam a mesma hipótese nula, com a mesma estatística. Este é a
fórmula ORIGINAL, de 1970; o `series/ljung_box` é a correção de 1978 para
amostra pequena, onde o Box-Pierce erra o nível do teste — e é o que se
reporta hoje. Este bloco existe para reproduzir ou comparar com trabalho
antigo que citou o Box-Pierce. Em amostra grande os dois praticamente
coincidem.

### Os graus descontados

Nos resíduos de um modelo, `graus` é o número de parâmetros estimados —
p + q + P + Q de um ARIMA. Cada um consome um grau de liberdade, e sem
descontá-los o teste fica generoso demais: aceita ruído branco onde há
estrutura. Numa série qualquer, deixe 0.

**Defasagens**: quantas autocorrelações entram. `0` é a regra de Hyndman — 10
para série não sazonal, duas vezes o ciclo para sazonal, e nunca mais que um
quinto da série. As defasagens usadas e os graus descontados saem na `nota`.

### Faltantes

Este bloco ACEITA série com faltante, ao contrário dos três blocos de raiz
unitária (`series/adf`, `series/kpss`, `series/phillips_perron`) e do
`series/ndiffs`, que os recusam. Aqui eles são ignorados na conta: o tamanho
usado é o de observações VÁLIDAS, e o bloco pede pelo menos doze delas — com
menos, o nó sai em vermelho, em vez de um veredito calculado sobre uma
defasagem só. É deliberado: o uso típico é diagnosticar resíduo, e resíduo vem
com buraco sempre que o modelo perdeu observação — exigir série cheia recusaria
justamente o caso mais comum.
]---", r"---[
- **Defasagens** — 0 para automático.
- **Graus do modelo** — parâmetros ajustados, a descontar. Tem de ser menor
  que as defasagens.
]---", r"---[
Um teste (`series/test`). Ligado numa entrada de tabela, ele vira UMA linha de
relatório: um `data/bind_rows` põe o Box-Pierce e o Ljung-Box lado a lado.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("arima", "series/arima", automatico = FALSE, p = 0L, d = 1L, q = 1L,
         P = 0L, D = 1L, Q = 1L, from = "pax") |>
  tr_add("res", "series/residuals", from = "arima") |>
  tr_add("bp", "series/box_pierce", graus = 2L, from = "res") |>
  tr_add("lb", "series/ljung_box", graus = 2L, from = "res")
]---", r"---[
`series/ljung_box`, o mesmo teste corrigido — é ele que se reporta;
`series/residuals` para testar um modelo; `series/acf` para ver em que
defasagem está a autocorrelação.
]---", teste = TRUE)),

# ---- Testar: os F da regressão ----------------------------------------------

      trama::tr_node("series/f_global", fn = tr_series_f_global, label = "F global",
        category = "serie_regressao", icon = icone("sigma"),
        description = "Teste F do modelo inteiro: a regressão explica alguma coisa?",
        inputs = list(ajuste = R), outputs = list(out = TE), params = list(),
        help = .tr_series_ajuda(r"---[
Testa o ajuste de `series/regression` INTEIRO. H0 é "todos os coeficientes,
fora o intercepto, são nulos" — nenhum termo explica a série: p-valor pequeno quer dizer que o modelo — tendência e sazonalidade
juntas — captura parte do movimento.

Diz que há sinal, não de onde ele vem. Para separar, `series/f_seasonal` e
`series/f_trend`, que testam cada bloco por si.

### Por que em BLOCO

É a razão de os três blocos de F existirem. Com onze dummies mensais, olhar
onze p-valores é onze chances de encontrar um "significativo" por acaso; a
pergunta honesta é se o CONJUNTO delas melhora o ajuste. Os coeficientes um a
um continuam disponíveis: ligue a regressão num nó da `data`.

### Com um bloco só, este teste se repete

Numa regressão sem sazonalidade (ou de grau 0), o modelo tem um bloco de
termos apenas — e aí o F global e o F parcial daquele bloco são o MESMO teste.
Os dois cards mostram números idênticos. É esperado, não é defeito.

### Antes de citar o p-valor

O teste supõe erro sem autocorrelação. Série temporal quase nunca obedece, e o
efeito é conhecido: o p-valor sai otimista demais. Extraia o resto com
`series/component` e passe pelo `series/ljung_box`; se houver autocorrelação,
os p-valores daqui são indicativos, não conclusivos.
]---", r"---[
Nenhum. Uma entrada: **ajuste**, vindo de `series/regression`.
]---", r"---[
Um teste (`series/test`). Ligado numa entrada de tabela, ele vira UMA linha de
relatório: um `data/bind_rows` junta os três F num só quadro.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("reg", "series/regression", from = "pax") |>
  tr_add("f", "series/f_global", from = "reg")
]---", r"---[
`series/f_seasonal` e `series/f_trend`, o mesmo F bloco a bloco;
`series/regression`, que produz o ajuste; `series/ljung_box` para conferir a
autocorrelação do resto.
]---", teste = TRUE)),

      trama::tr_node("series/f_seasonal", fn = tr_series_f_sazonal, label = "F do bloco sazonal",
        category = "serie_regressao", icon = icone("calendar-range"),
        description = "Teste F do bloco sazonal da regressão: há sazonalidade?",
        inputs = list(ajuste = R), outputs = list(out = TE), params = list(),
        help = .tr_series_ajuda(r"---[
Testa os coeficientes sazonais de um ajuste de `series/regression` EM BLOCO.
H0 é "os coeficientes sazonais são todos nulos": rejeitar é concluir que há
sazonalidade.

O F parcial compara o ajuste com e sem o bloco — reajusta a regressão sem as
dummies e mede o quanto o encaixe piorou.

### Por que em BLOCO

Com onze dummies mensais, olhar onze p-valores é onze chances de encontrar um
"significativo" por acaso. A pergunta honesta é se o CONJUNTO delas melhora o
ajuste, e é o que este teste mede. Os coeficientes um a um continuam
disponíveis: ligue a regressão num nó da `data`.

### A regressão precisa ter o bloco

Regressão ligada sem sazonalidade é cartão vermelho, e não um card verde
dizendo que não há: o bloco não foi testado, ele nunca existiu. Ligue a
sazonalidade no `series/regression`.
]---", r"---[
Nenhum. Uma entrada: **ajuste**, vindo de `series/regression` com sazonalidade
ligada.
]---", r"---[
Um teste (`series/test`). Ligado numa entrada de tabela, vira uma linha de
relatório.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("reg", "series/regression", from = "pax") |>
  tr_add("f", "series/f_seasonal", from = "reg")
]---", r"---[
`series/f_trend`, o mesmo teste no outro bloco; `series/f_global`, o
modelo inteiro; `series/seasonal_plot` para ver a sazonalidade que o teste
mede.
]---", teste = TRUE)),

      trama::tr_node("series/f_trend", fn = tr_series_f_tendencia,
        label = "F do bloco de tendência",
        category = "serie_regressao", icon = icone("trending-up-down"),
        description = "Teste F do bloco de tendência da regressão: há tendência?",
        inputs = list(ajuste = R), outputs = list(out = TE), params = list(),
        help = .tr_series_ajuda(r"---[
Testa os termos do polinômio de tendência de um ajuste de `series/regression`
EM BLOCO. H0 é "os coeficientes do polinômio são todos nulos": rejeitar é
concluir que há tendência.

O F parcial compara o ajuste com e sem o bloco — reajusta a regressão sem os
termos de tendência e mede o quanto o encaixe piorou.

### Por que em BLOCO

Num polinômio de grau 2 ou 3, o termo linear e o quadrático dividem o mesmo
sinal, e nenhum dos dois aparece sozinho: olhar os p-valores um a um faria
concluir que não há tendência nenhuma quando há. A pergunta honesta é se o
CONJUNTO dos termos melhora o ajuste. Os coeficientes um a um continuam
disponíveis: ligue a regressão num nó da `data`.

### A regressão precisa ter o bloco

Regressão de grau 0 é cartão vermelho, e não um card verde dizendo que não há
tendência: o bloco não foi testado, ele nunca existiu. Suba o grau no
`series/regression`.
]---", r"---[
Nenhum. Uma entrada: **ajuste**, vindo de `series/regression` com grau 1 ou
mais.
]---", r"---[
Um teste (`series/test`). Ligado numa entrada de tabela, vira uma linha de
relatório.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("reg", "series/regression", grau = 2L, from = "pax") |>
  tr_add("f", "series/f_trend", from = "reg")
]---", r"---[
`series/f_seasonal`, o mesmo teste no outro bloco; `series/f_global`, o modelo
inteiro; `series/regression`, que produz o ajuste.
]---", teste = TRUE)),

# ---- Testar: tendência ------------------------------------------------------

      trama::tr_node("series/mann_kendall", fn = tr_series_mann_kendall,
        label = "Mann-Kendall",
        category = "serie_tendencia", icon = icone("trending-up"),
        description = "Mann-Kendall: a série tem tendência?",
        inputs = list(serie = S), outputs = list(out = TE), params = list(),
        help = .tr_series_ajuda(r"---[
Testa se a série tem TENDÊNCIA. É o teste de tendência mais usado em
climatologia — chuva, vazão, temperatura —, e o que se espera encontrar num
trabalho da área.

Ele conta, par a par, quantas vezes um valor posterior supera um anterior. Essa
contagem é o S: positivo quando o futuro costuma estar acima, negativo quando
costuma estar abaixo. A estatística Z é o S padronizado.

### Não paramétrico

Não supõe distribuição nenhuma para a série. É por isso que ele é o padrão onde
o dado não é normal, que é o caso da maior parte das variáveis ambientais. O
`series/f_trend` responde à mesma pergunta, mas cobra normalidade do erro em
troca; quando os dois concordam, a conclusão tem chão.

### A tendência é MONOTÔNICA

É o limite que mais surpreende: o teste procura movimento em um sentido só. Uma
série que sobe durante metade do período e desce na outra metade tem pares se
cancelando e pode sair SEM tendência nenhuma — o que não quer dizer que nada
aconteceu. Olhe o `series/plot` antes de acreditar num "não há evidência".

### A direção vem no sinal de Z

Rejeitar H0 não é só "há tendência": Z positivo é tendência de AUMENTO, Z
negativo é tendência de QUEDA, e é assim que a conclusão sai escrita.

### Empates

Valor repetido não aponta direção nenhuma, e a variância é corrigida por isso.
Sem a correção, uma série com muitos valores iguais — medição arredondada, uma
corrida de zeros na chuva — sairia com um p-valor otimista demais. A `nota` diz
quantos grupos de empate entraram na conta, junto do S.

### O que sai

A estatística Z, o p-valor bilateral e a conclusão em palavras; o S vai na
coluna extra do relatório. A decisão é a 5%. O p-valor vem da aproximação
normal, que pede pelo menos dez observações — abaixo disso o bloco recusa em vez
de devolver um número que a amostra não sustenta. Dez bastam porque a
aproximação aqui corre sobre TODAS as n(n-1)/2 comparações par a par, e não
sobre uma contagem de símbolos: dez observações já rendem quarenta e cinco
comparações, enquanto o `series/runs`, que depende do corte pela mediana, só
alcança a normal dele com quarenta observações.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue um
`series/interpolate` antes, ou recorte a parte cheia com `series/window`.
]---", r"---[
Nenhum. Uma entrada: **serie**.
]---", r"---[
Um teste (`series/test`), com o S numa coluna extra. Ligado numa entrada de
tabela, ele vira UMA linha de relatório: um `data/bind_rows` junta vários testes
num só quadro.
]---", r"---[
tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("mk", "series/mann_kendall", from = "nilo")
]---", r"---[
`series/f_trend`, a mesma pergunta pela regressão; `series/adf` e
`series/kpss`, que perguntam por estacionariedade e não por tendência;
`series/plot` para ver se o movimento é mesmo de um sentido só;
`series/example` para uma série com tendência à mão.
]---", teste = TRUE)),

      trama::tr_node("series/cox_stuart", fn = tr_series_cox_stuart,
        label = "Cox-Stuart",
        category = "serie_tendencia", icon = icone("arrow-up-down"),
        description = "Cox-Stuart: a série tem tendência?",
        inputs = list(serie = S), outputs = list(out = TE),
        params = list(
          pareamento = E("terços", c("terços", "metades"), label = "Pareamento")),
        help = .tr_series_ajuda(r"---[
Testa se a série tem TENDÊNCIA por um teste de SINAL: pareia observações
distantes no tempo e conta quantas vezes a segunda é maior que a primeira. Se
não houvesse tendência, subir e descer seriam igualmente prováveis, e essa
contagem — o M — se comportaria como cara-ou-coroa. É o mesmo veredito do
`series/mann_kendall` por um caminho mais barato, e a dissertação que esta
coleção segue compara os dois diretamente.

### As duas formas de parear

O param **pareamento** decide QUAIS observações formam cada par, e a escolha não
é cosmética: os dois caminhos dão estatísticas diferentes na mesma série.

**Terços** é o padrão, e é o teste original de Cox & Stuart: compara o primeiro
terço da série com o último e JOGA FORA o miolo. Descartar o meio é o que dá
poder ao teste contra tendência monotônica — são os extremos que acumularam o
movimento, e parear observações vizinhas dilui o contraste.

Não espere daqui o mesmo número do pacote `trend`, que responde à mesma
pergunta por outra conta: ele padroniza pelo tamanho da SÉRIE (n/6 e n/12) em
vez de pelos pares que de fato sobraram, corrige continuidade em série curta,
nunca usa a binomial exata, e reporta a estatística sem sinal. Os dois só
coincidem num conjunto estreito de séries, e esse limite está fixado em teste
em vez de prometido aqui — três tentativas de escrevê-lo como regra saíram
erradas, porque ele tem mais condições do que cabe numa frase. O sinal, aqui,
fica de propósito: é ele que deixa a conclusão dizer "queda" em vez de só
"tendência".

**Metades** é a formulação da dissertação: parte a série ao meio e pareia cada
observação com a que está meia série adiante. Escolha esta quando o trabalho
tiver de reproduzir a dissertação. Entram mais pares, cada um com contraste
menor, e o resultado muda de verdade: na mesma reta com ruído, o Z em metades
sai maior e o p-valor duas ordens de grandeza menor que o de terços. Mesma
série, mesma hipótese nula, dois números.

### Empates

Par com os dois valores iguais não aponta direção nenhuma e é DESCARTADO, como
manda o método — contá-lo como "não subiu" seria pôr no prato da queda um par
que não disse nada. A `nota` diz quantos pares sobraram, que é o número que
explica um M pequeno numa série grande.

### Exata ou aproximada, conforme o número de pares

Com poucos pares o p-valor vem da binomial EXATA; de vinte pares em diante, da
aproximação normal, e aí a estatística é o Z. No ramo exato não há Z nenhum
calculado, e o que o card mostra é o próprio M, com esse rótulo: reportar um Z
que ninguém computou seria mentir sobre a conta. A `nota` diz sempre qual dos
dois caminhos entrou, porque a escolha é automática.

### A direção vem da contagem

Rejeitar H0 não é só "há tendência": mais pares subindo que descendo é tendência
de AUMENTO, o contrário é de QUEDA, e é assim que a conclusão sai escrita.

### O que sai

A estatística (Z ou M), o p-valor bilateral e a conclusão em palavras; o M e o
número de pares vão nas colunas extras do relatório. A decisão é a 5%. O bloco
pede pelo menos dezesseis observações: abaixo disso o terço da ponta fica com
menos de seis pares, e com menos de seis pares nem o resultado mais extremo
possível alcança o corte de 5% — o teste não teria como rejeitar nunca.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue um
`series/interpolate` antes, ou recorte a parte cheia com `series/window`.
]---", r"---[
**pareamento** — quais observações formam cada par. *Terços* (padrão) compara o
primeiro terço com o último, descartando o miolo, como no artigo original;
*metades* pareia cada observação com a que está meia série adiante, como na
dissertação. Uma entrada: **serie**.
]---", r"---[
Um teste (`series/test`), com o M e o número de pares em colunas extras. Ligado
numa entrada de tabela, ele vira UMA linha de relatório: um `data/bind_rows`
junta vários testes num só quadro.
]---", r"---[
tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("cs", "series/cox_stuart", from = "nilo")
]---", r"---[
`series/mann_kendall`, a mesma pergunta contando todos os pares — a dissertação
compara os dois, e vale rodar os dois; `series/f_trend`, a mesma pergunta
pela regressão; `series/plot` para ver se o movimento é mesmo de um sentido só;
`series/example` para uma série com tendência à mão.
]---", teste = TRUE)),

      trama::tr_node("series/runs", fn = tr_series_runs,
        label = "Run",
        category = "serie_tendencia", icon = icone("shuffle"),
        description = "Run (Wald-Wolfowitz): a série é aleatória?",
        inputs = list(serie = S), outputs = list(out = TE), params = list(),
        help = .tr_series_ajuda(r"---[
Testa se a série foi gerada ao ACASO. Também chamado teste de sequências de
Wald-Wolfowitz: marca cada valor conforme esteja acima ou abaixo da mediana e
conta as SEQUÊNCIAS — trechos seguidos do mesmo lado. Uma série aleatória troca
de lado com uma frequência previsível; sequências de menos dizem que os valores
se agrupam, sequências demais, que alternam.

### A hipótese nula é ALEATORIEDADE, e não ausência de tendência

É a diferença que mais importa nesta página, e o que separa este bloco dos dois
vizinhos de categoria. Rejeitar aqui NÃO é concluir que há tendência: é concluir
que a série não parece ter saído do acaso. Tendência é só uma das causas que
produzem sequências de menos — uma mudança de nível no meio da série, um ciclo e
qualquer agrupamento produzem o mesmo efeito, e o teste não distingue os três. A
conclusão sai escrita assim, sem afirmar tendência, justamente para não dar por
respondida uma pergunta que este bloco não fez.

### Não use este teste sozinho

A literatura já documentou o erro. Back (2001), revisado pela dissertação que
esta coleção segue, aplicou Run, Pettitt e Mann-Kendall às mesmas séries
climáticas: o Run foi o ÚNICO que deixou de detectar a tendência que os outros
dois encontraram. Morettin & Toloi (2006) colocam Run e Cox-Stuart na mesma
posição — testes para rodar ao lado de outros, não no lugar deles. Quem procura
tendência deve ligar também um `series/mann_kendall`; um "não rejeita H0" daqui,
sozinho, é evidência fraca de que não há nada acontecendo.

### Empates com a mediana

Valor exatamente IGUAL à mediana não fica nem acima nem abaixo, e sai da conta,
como manda o método. Empurrá-lo para um dos lados inventaria um símbolo que o
dado não deu e ainda emendaria duas sequências numa só. O descarte muda o número
de observações que entram no teste e não aparece em lugar nenhum do card, então a
`nota` diz quantas saíram e quantas sobraram de cada lado.

### O que sai

A estatística Z, o p-valor bilateral e a conclusão em palavras; o número de
sequências vai na coluna extra do relatório. A decisão é a 5%. O p-valor vem da
aproximação normal, e ela pede pelo menos 20 observações de cada lado da mediana
— por isso o bloco exige 40 observações, que é o tamanho em que o corte pela
mediana entrega esses 20 de cada lado. Abaixo disso valeria a distribuição exata
do número de sequências, que este bloco não calcula, e devolver mesmo assim o Z
seria publicar uma precisão que a amostra não sustenta. Uma série que passe no
tamanho mas chegue ao teste com um platô na mediana também é recusada, pelo mesmo
motivo.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue um
`series/interpolate` antes, ou recorte a parte cheia com `series/window`.
]---", r"---[
Nenhum. Uma entrada: **serie**.
]---", r"---[
Um teste (`series/test`), com o número de sequências numa coluna extra. Ligado
numa entrada de tabela, ele vira UMA linha de relatório: um `data/bind_rows`
junta vários testes num só quadro.
]---", r"---[
tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("run", "series/runs", from = "nilo")
]---", r"---[
`series/mann_kendall` e `series/cox_stuart`, que perguntam por tendência de
verdade — a dissertação compara os três, e este é o que menos detecta;
`series/pettitt`, o terceiro teste que Back (2001) aplicou às mesmas séries, e
que localiza a mudança em vez de afirmar tendência; `series/plot` para ver de que
jeito a aleatoriedade falhou; `series/ljung_box`, que também pergunta se a série
é ruído, mas pela autocorrelação.
]---", teste = TRUE)),

      trama::tr_node("series/pettitt", fn = tr_series_pettitt,
        label = "Pettitt",
        category = "serie_tendencia", icon = icone("milestone"),
        description = "Pettitt: a série tem um ponto de mudança?",
        inputs = list(serie = S), outputs = list(out = TE), params = list(),
        help = .tr_series_ajuda(r"---[
Testa se a série tem um PONTO DE MUDANÇA: um instante a partir do qual ela passou
a se comportar como outra série. A hipótese nula é a HOMOGENEIDADE — que o trecho
antes e o trecho depois de qualquer ponto venham da mesma população. O teste
percorre todos os cortes possíveis, mede em cada um o quanto as duas partes se
separam, e fica com o corte onde a separação é maior.

### Quebra não é tendência

É o que mais importa nesta página, e o que separa este bloco dos três vizinhos de
categoria. Rejeitar aqui NÃO é concluir que a série sobe ou que a série desce: é
concluir que houve uma RUPTURA, e dizer onde. A dissertação que esta coleção
segue é enfática no ponto, citando Niel et al. (1998) — o teste localiza a
mudança, não afirma direção nenhuma. A conclusão sai escrita assim: nomeia a
observação em que a série mudou e cala sobre direção, para não dar por respondida
uma pergunta que este bloco não fez. Uma série pode mudar de nível para baixo no
meio e ainda assim subir do começo ao fim.

### Use junto com o Mann-Kendall

É o par que a dissertação recomenda, e Penereiro & Orlando (2013), citados lá,
usam os dois exatamente assim: o `series/mann_kendall` estabelece que existe
tendência, o Pettitt localiza QUANDO a série mudou. Um responde "o quê", o outro
"quando", e nenhum dos dois responde pelo outro. Ligar só este bloco e escrever
"há tendência" no relatório é o erro que esta página inteira existe para evitar.

### Onde foi a mudança

Rejeitando H0, o ponto de mudança sai em dois formatos, e os dois vão para o
relatório: a POSIÇÃO na série, que é o número que outros pacotes reportam e que
se confere, e o RÓTULO do período — "1953 jun" numa mensal, "1960 T3" numa
trimestral, o ano numa anual. O rótulo vem do calendário da própria série; numa
série sem calendário ele seria o próprio índice, e aí o bloco não o repete.

Não rejeitando, a posição continua sendo calculada e publicada: é onde o corte
foi MAIS favorável, e nem assim deu. Ler esse número como uma quebra fraca é lê-lo
errado.

### O que sai

A estatística K, o p-valor bilateral e a conclusão em palavras; a posição do
ponto de mudança e o rótulo do período vão nas colunas extras do relatório. A
decisão é a 5%. O p-valor é uma APROXIMAÇÃO, e ela passa de 1 em série curta e
homogênea — nesse caso o valor é preso em 1 e a `nota` avisa, porque um "p = 1"
no card se lê como certeza de homogeneidade quando é só a fórmula estourando. O
bloco pede pelo menos onze observações: com dez, o maior K que existe — o da
série perfeitamente monotônica — ainda fica acima do corte de 5%, e o teste não
teria como rejeitar nunca, qualquer que fosse o dado. É o mesmo tipo de piso
medido do `series/mann_kendall` e do `series/cox_stuart`, e por isso os números
diferem entre os quatro blocos: cada um sai da fórmula do seu próprio teste, e
não de uma convenção comum.

### O ponto pode empatar

Mais de um corte pode alcançar exatamente o mesmo K máximo, e o reportado é o
PRIMEIRO deles. Quando isso acontece a `nota` diz quantos empataram: são cortes
igualmente bons, e ler o número publicado como o único ponto possível seria ler
mais do que o teste disse.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue um
`series/interpolate` antes, ou recorte a parte cheia com `series/window`.
]---", r"---[
Nenhum. Uma entrada: **serie**.
]---", r"---[
Um teste (`series/test`), com a posição do ponto de mudança e o rótulo do período
em colunas extras. Ligado numa entrada de tabela, ele vira UMA linha de
relatório: um `data/bind_rows` junta vários testes num só quadro.
]---", r"---[
tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("pet", "series/pettitt", from = "nilo")
]---", r"---[
`series/mann_kendall`, o par recomendado — ele diz se há tendência, este diz
quando a série mudou; `series/cox_stuart` e `series/runs`, os outros dois não
paramétricos da categoria; `series/plot` para ver a quebra que o teste apontou;
`series/window` para analisar separadamente os dois trechos que ele separou.
]---", teste = TRUE)),

# ---- Sazonalidade -----------------------------------------------------------

      trama::tr_node("series/seasonality_kw", fn = tr_series_kruskal_wallis,
        label = "Sazonalidade (Kruskal-Wallis)",
        category = "serie_sazonal", icon = icone("calendar-days"),
        description = "Kruskal-Wallis: a série tem sazonalidade?",
        inputs = list(serie = S), outputs = list(out = TE), params = list(),
        help = .tr_series_ajuda(r"---[
Testa se a série tem SAZONALIDADE sem supor distribuição nenhuma. Põe todas as
observações em POSTOS — o menor valor da série inteira vira posto 1, o maior
vira o posto N — e compara a soma dos postos de cada estação. Se janeiro é
sempre alto e julho é sempre baixo, as somas se afastam e o H cresce. H0 é "as
estações têm a mesma distribuição" — sem sazonalidade —, e rejeitar é concluir
que há.

É o irmão não paramétrico do `series/f_seasonal`, que responde à mesma pergunta
pedindo erro normal em troca.

### Sazonalidade determinística

O que este teste enxerga é o padrão que se repete IGUAL todo ciclo: janeiro
sempre alto, julho sempre baixo. A sazonalidade ESTOCÁSTICA — a que vai mudando
de ano para ano — não tem um "nível de janeiro" fixo para o posto encontrar, e
passa por aqui sem ser vista. A dissertação separa as duas na seção 3.4: para a
estocástica o caminho é a diferença sazonal do `series/diff`.

### Tendência atrapalha, e o log não salva

É o ponto que mais engana desta página, e ele se mede no caso de livro. O
`AirPassengers` é A série sazonal dos manuais, e ela NÃO rejeita aqui:

```
Kruskal-Wallis chi-squared = 11.148, df = 11, p-value = 0.4309
```

Não é defeito do bloco: é a TENDÊNCIA. A série quase triplica ao longo dos doze
anos, então os postos altos são todos dos anos finais e se espalham por todas as
estações — cada mês recebe um valor baixo do começo da série e um alto do fim, e
as somas de postos acabam empatando. O padrão sazonal existe; o posto o perde
por baixo do crescimento.

Passar um log ANTES não muda NADA, e isso costuma surpreender: o log torna a
sazonalidade multiplicativa em aditiva, sim, mas este teste é de POSTOS e o log
é monotônico — ele não troca a ordem de valor nenhum, então o H e o p-valor saem
idênticos aos de cima, dígito por dígito. O `series/transform` serve para
estabilizar a variância; para este teste ele é inócuo.

O que resolve é tirar a TENDÊNCIA. Uma diferença simples do `series/diff` antes
do bloco, e o mesmo `AirPassengers` rejeita com folga:

```
Kruskal-Wallis chi-squared = 119.2, df = 11, p-value = 2.623e-20
```

Ou seja: um "não rejeita H0" numa série que você SABE que é sazonal é quase
sempre a tendência falando. Olhe a série no `series/seasonal_plot` ou no
`series/subseries` antes de concluir que não há estação.

### A série precisa ter estação

Série com frequência 1 é cartão vermelho, e não um card verde dizendo que não há
sazonalidade: sem ciclo declarado não existem estações para comparar, e o teste
seria mandado comparar um grupo só. Declare a frequência no nó que cria a série.
Menos de TRÊS ciclos completos também é recusado, e não dois como nos outros
blocos sazonais. Com duas observações por estação, o maior H possível — as
estações perfeitamente separadas — não chega ao valor crítico, então o teste não
teria como rejeitar e o veredito estaria decidido antes de ler o dado:

```
frequência   ciclos   maior H possível   corte a 5%
4            2          6.667              7.815
4            3         10.385              7.815
2            2          2.400              3.841
2            3          3.857              3.841
12           2         22.880             19.675
```

A mensal até rejeitaria com dois ciclos, mas o piso é o mesmo para todas as
frequências: uma regra só, ao custo de um ano de dado.

### O que sai

A estatística H, o p-valor e a conclusão em palavras; os graus de liberdade — o
número de estações menos um — vão numa coluna extra, porque é com eles que se lê
o H contra a tabela do qui-quadrado. A decisão é a 5%. A cauda é a SUPERIOR: é o
H grande que derruba H0, ao contrário dos testes de tendência da coleção. A
`nota` diz quantas estações entraram e quantas observações cada uma tem — ciclo
incompleto na ponta deixa umas com uma observação a mais, e é o que explica um H
diferente do da mesma série fechada no último ciclo.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue um
`series/interpolate` antes, ou recorte a parte cheia com `series/window`.
]---", r"---[
Nenhum. Uma entrada: **serie**, que precisa ter frequência maior que 1.
]---", r"---[
Um teste (`series/test`), com os graus de liberdade numa coluna extra. Ligado
numa entrada de tabela, ele vira UMA linha de relatório: um `data/bind_rows`
junta vários testes num só quadro.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("kw", "series/seasonality_kw", from = "pax")

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("d", "series/diff", from = "pax") |>
  tr_add("kw", "series/seasonality_kw", from = "d")
]---", r"---[
`series/f_seasonal`, a mesma pergunta pedindo erro normal em troca;
`series/diff` para tirar a tendência antes do teste, ou para a diferença sazonal
quando a sazonalidade é estocástica; `series/transform`, o log que estabiliza a
variância e que NÃO muda este teste; `series/seasonal_plot` e `series/subseries`
para ver a sazonalidade que o teste mede.
]---", teste = TRUE)),

      trama::tr_node("series/periodicity_fisher", fn = tr_series_fisher,
        label = "Periodicidade (Fisher)",
        category = "serie_sazonal", icon = icone("signal"),
        description = "Fisher: existe uma periodicidade escondida?",
        inputs = list(serie = S), outputs = list(out = TE), params = list(),
        help = .tr_series_ajuda(r"---[
Procura uma PERIODICIDADE ESCONDIDA. Decompõe a série nas ondas de todos os
períodos que ela comporta — o periodograma — e pergunta se o MAIOR pico é maior
do que o acaso produziria. A estatística é o **g**, a fração da potência total
que esse pico sozinho carrega (eq. 3.41 da dissertação). H0 é "a série não tem
periodicidade", e rejeitar é concluir que há um ciclo.

É o irmão do `series/seasonality_kw`, e os dois fazem a pergunta em sentidos
opostos. O Kruskal-Wallis COMPARA ESTAÇÕES QUE VOCÊ JÁ DECLAROU: ele lê a
frequência da série para saber o que é janeiro, e responde se aquelas estações
diferem. Este aqui CAÇA UM PERÍODO DESCONHECIDO: não pergunta a frequência a
ninguém, varre a grade inteira e diz onde está o pico. Quem já sabe qual é o
ciclo e quer saber se ele é real usa o Kruskal-Wallis; quem desconfia de um ciclo
e não sabe qual, usa este.

### O pico pode não ser uma estação

É o ponto que mais engana desta página: o teste acha o maior pico ONDE QUER QUE
ELE ESTEJA, e nem todo pico é estação. Cinco séries do R, medidas:

```
série            n      g       zα (5%)   p            período do pico
AirPassengers   72   0.5017    0.0974    2.392e-20     12 observações
UKgas           54   0.5531    0.1235    1.566e-17      4 observações
nottem         120   0.9130    0.0633    7.636e-125    12 observações
lh              24   0.2344    0.2354    5.158e-02      8 observações
Nile            50   0.1781    0.1315    3.356e-03    100 observações
```

O `Nile` REJEITA — e não tem sazonalidade nenhuma. Olhe o período: 100
observações, que é a série inteira. O ciclo não chega a se repetir uma segunda
vez dentro dos dados, e isso não é estação; é a tendência e a estrutura de baixa
frequência que sobraram depois de tirar a reta. Um pico sazonal se REPETE dentro
da série: poucas observações por ciclo e muitos ciclos, como os 12 do
`AirPassengers`. O primeiro período da grade, N, é justamente o contrário — um
ciclo só, que é tendência e não estação —, e é nele que o `Nile` caiu.

Em série de tamanho par a grade inclui ainda o último período, de duas
observações. Ele entra na soma do g como os outros; tirá-lo não muda a decisão
no `lh`, o caso mais apertado da tabela (o p vai de 0.052 para 0.062, e segue
sem rejeitar).

Por isso o bloco publica o PERÍODO junto com o veredito, e a `nota` avisa em voz
alta quando o pico não se repete ao menos duas vezes. Um "há periodicidade" lido
sem olhar o período é o erro mais fácil de cometer aqui.

### O período sai em observações

O periodograma mede frequência em ciclos por unidade de tempo, e o inverso disso
viria na unidade de tempo da série — num `AirPassengers` mensal, a estação de
doze meses apareceria como o número 1, que se lê como "um mês" e inverte o
sentido da frase. O bloco converte para OBSERVAÇÕES: o mesmo pico sai como 12,
que é o que se conta no gráfico. Em série de frequência 1 os dois números
coincidem. Junto vai quantas vezes o ciclo cabe na série — 12 no
`AirPassengers`, 1 no `Nile` —, que é a leitura já feita do período.

### O corte da dissertação e o p-valor

A dissertação decide comparando o **g** com o valor crítico zα = 1 - (α/n)^(1/(n-1))
(eq. 3.42), rejeitando quando o g passa de zα. Este bloco emite um p-valor de
verdade — o primeiro termo da série exata de Fisher — para que os três pontos do
card funcionem como em todo outro teste da coleção. As duas regras são A MESMA,
escrita de dois jeitos: nas cinco séries acima elas concordam, inclusive no caso
apertado do `lh`, onde o g fica logo ABAIXO de zα e o p-valor fica logo ACIMA de
5%, e as duas não rejeitam. Para que a comparação da dissertação possa ser
conferida direto no card, o zα sai publicado como o valor crítico a 5%, ao lado
do p-valor.

### Não precisa de estação declarada, mas precisa de tamanho

Ao contrário do `series/seasonality_kw`, este bloco ACEITA série de frequência 1:
ele não agrupa por estação, e o periodograma existe para qualquer série. Recusar
frequência 1 bloquearia justamente o uso para o qual ele serve — caçar um período
que ninguém declarou —, e foi assim que a linha do `Nile` da tabela acima foi
medida.

O que ele pede são oito observações. O limite vem da GRADE: com N observações o
periodograma só enxerga os períodos N, N/2, N/3 e assim por diante, e o menor
ciclo que esta coleção sabe declarar é o trimestral. Para que um período 4 exista
nessa grade e ainda se repita duas vezes dentro da série são precisas oito
observações — em N igual a 8 a grade tem o 4, em N igual a 7 ela não tem. Abaixo
disso o "período do pico" não é uma escolha entre alternativas: é o único lugar
onde ele poderia cair.

### O que sai

O g, o p-valor e a conclusão em palavras; o período do pico e o número de ciclos
vão em colunas extras, e o zα na coluna do valor crítico a 5%. A decisão é a 5%.
A cauda é a SUPERIOR: é o g grande — o pico que concentra a potência — que
derruba H0.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue um
`series/interpolate` antes, ou recorte a parte cheia com `series/window`.
]---", r"---[
Nenhum. Uma entrada: **serie**.
]---", r"---[
Um teste (`series/test`), com o período do pico e o número de ciclos em colunas
extras, e o zα da dissertação na coluna do valor crítico a 5%. Ligado numa
entrada de tabela, ele vira UMA linha de relatório: um `data/bind_rows` junta
vários testes num só quadro.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("f", "series/periodicity_fisher", from = "pax")

tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("f", "series/periodicity_fisher", from = "nilo")
]---", r"---[
`series/seasonality_kw`, a outra pergunta da categoria — lá você declara as
estações e o teste as compara, aqui o teste procura o período sozinho;
`series/acf` e `series/seasonal_plot` para OLHAR o ciclo que o pico apontou antes
de acreditar nele; `series/subseries` quando o período achado bate com a
frequência da série; `series/diff` para tirar a tendência que produz o pico
enganoso do caso `Nile`.
]---", teste = TRUE)),

# ---- Ver --------------------------------------------------------------------

      trama::tr_node("series/plot", fn = tr_series_plot, label = "Série no tempo",
        category = "serie_ver", icon = icone("chart-line"),
        description = "Desenha a série ao longo do tempo.",
        inputs = list(serie = S), outputs = list(out = G),
        params = .tr_series_props(pontos = B(FALSE, label = "Marcar pontos")),
        help = .tr_series_ajuda(r"---[
A série como linha no tempo, que é o primeiro gráfico de qualquer análise:
tendência, sazonalidade, quebras, outliers e mudança de variância se veem
aqui antes de qualquer teste.

O card de toda série já mostra este gráfico. O nó existe para o que o card não
escolhe: proporção, tema claro para o relatório, título — e para sair como
`view/plot`, igual aos gráficos da coleção `view`.

Faltante interrompe a linha: o buraco aparece como buraco.
]---", r"---[
- **Marcar pontos** — desenha cada observação sobre a linha. Útil em série
  curta, e para ver onde estão os faltantes.
]---", r"---[
Um gráfico (`view/plot`). No console, um ggplot comum, somável.
]---", r"---[
tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("g", "series/plot", pontos = TRUE, titulo = "Vazão anual do Nilo",
         tema = "claro", from = "nilo")
]---", r"---[
`view/line` para várias séries numa tabela; `series/seasonal_plot` para o
padrão sazonal; `series/plot_decomposition` para os componentes.
]---", grafico = TRUE)),

      trama::tr_node("series/acf", fn = tr_series_acf, label = "Correlograma (ACF)",
        category = "serie_ver", icon = icone("chart-column"),
        description = "A autocorrelação da série em cada defasagem, com a banda do ruído branco.",
        inputs = list(serie = S), outputs = list(out = G),
        params = .tr_series_props(defasagens = I(0L, min = 0L, max = 500L, label = "Defasagens")),
        help = .tr_series_ajuda(r"---[
O correlograma: para cada defasagem k, a correlação da série com ela mesma k
períodos antes. É o retrato da memória da série.

### Como ler

- barras que caem DEVAGAR, ainda altas depois de dezenas de defasagens: série
  não estacionária (tendência). Diferencie antes de ler o resto.
- picos nos múltiplos do ciclo (12, 24, 36 numa mensal): sazonalidade. As
  marcas do eixo caem justamente nesses pontos.
- cortar abruptamente depois da defasagem q: assinatura de um MA(q).
- decair aos poucos (exponencial ou senoidal): assinatura de um AR — confira no
  `series/pacf`.

A linha tracejada é a banda de ±1,96/√n: onde fica uma autocorrelação que é
zero, 95% das vezes. As barras FORA dela saem coloridas. Com 36 defasagens,
espere uma ou duas fora por puro acaso (5% de 36 é 1,8) — uma barra isolada e
pequena fora da banda, numa defasagem sem significado, não é estrutura.

O eixo é em DEFASAGENS (1, 2, … 36), e não na unidade de tempo que o `acf()`
do R usa para série sazonal (0,083, 0,167, … 3).
]---", r"---[
- **Defasagens** — até onde ir. 0 é automático: 10·log10(n), mas nunca menos
  de três ciclos numa série sazonal.
]---", r"---[
Um gráfico (`view/plot`). No console, um ggplot com a tabela `defasagem`, `r`,
`fora` em `$data`.
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("log", "series/transform", from = "pax") |>
  tr_add("d", "series/diff", tipo = "sazonal", from = "log") |>
  tr_add("acf", "series/acf", defasagens = 36L, from = "d") |>
  tr_add("pacf", "series/pacf", defasagens = 36L, from = "d")
]---", r"---[
`series/pacf`, que se lê junto; `series/ljung_box` para o teste formal;
`series/lag_plot` para ver a nuvem por trás de cada barra.
]---", grafico = TRUE)),

      trama::tr_node("series/pacf", fn = tr_series_pacf, label = "Autocorrelação parcial (PACF)",
        category = "serie_ver", icon = icone("chart-bar"),
        description = "A correlação com a defasagem k, descontadas as defasagens intermediárias.",
        inputs = list(serie = S), outputs = list(out = G),
        params = .tr_series_props(defasagens = I(0L, min = 0L, max = 500L, label = "Defasagens")),
        help = .tr_series_ajuda(r"---[
A autocorrelação PARCIAL: a correlação da série com ela mesma k períodos
antes, depois de descontar o que as defasagens 1 a k−1 já explicam. Se hoje
depende só de ontem, a ACF da defasagem 2 é alta (ontem dependia de anteontem),
mas a PACF da 2 é zero.

Lê-se junto com o `series/acf`:

| | ACF | PACF |
|---|---|---|
| AR(p) | decai aos poucos | corta depois de p |
| MA(q) | corta depois de q | decai aos poucos |
| ARMA | decai | decai |

Picos nos múltiplos do ciclo sugerem termos sazonais (P no ARIMA).

Banda, cores e eixo como no correlograma.
]---", r"---[
- **Defasagens** — até onde ir. 0 é automático, como no `series/acf`.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("lynx", "series/example", dataset = "lynx") |>
  tr_add("log", "series/transform", from = "lynx") |>
  tr_add("pacf", "series/pacf", from = "log")
]---", r"---[
`series/acf`, que se lê junto; `series/arima` para ajustar a ordem sugerida.
]---", grafico = TRUE)),

      trama::tr_node("series/lag_plot", fn = tr_series_lag_plot, label = "Gráfico de defasagens",
        category = "serie_ver", icon = icone("chart-scatter"),
        description = "A série contra ela mesma k períodos antes, um painel por defasagem.",
        inputs = list(serie = S), outputs = list(out = G),
        params = .tr_series_props(defasagens = I(0L, min = 0L, max = 16L, label = "Defasagens")),
        help = .tr_series_ajuda(r"---[
Um disperso por defasagem: no painel k, cada ponto é o valor de um período
contra o valor de k períodos antes. É o correlograma sem o resumo — cada painel
é a nuvem cuja correlação é uma barra do `series/acf`.

Mostra o que o número esconde: relação curva, um ponto sozinho puxando a
correlação, dois regimes. Pontos colados na diagonal tracejada são
autocorrelação forte.

Numa série sazonal os pontos saem coloridos pela estação, e o painel do ciclo
(12 na mensal) é o que fica mais colado na diagonal — janeiro perto de janeiro.
]---", r"---[
- **Defasagens** — quantos painéis, até 16. 0 é o ciclo (até 12) numa série
  sazonal, e 4 numa sem ciclo.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("lags", "series/lag_plot", from = "pax")
]---", r"---[
`series/acf` para o resumo numérico; `series/lag` para montar a série
defasada numa tabela.
]---", grafico = TRUE)),

      trama::tr_node("series/seasonal_plot", fn = tr_series_seasonal_plot, label = "Gráfico sazonal",
        category = "serie_ver", icon = icone("chart-spline"),
        description = "Um traço por ano, percorrendo o ciclo: o formato da sazonalidade.",
        inputs = list(serie = S), outputs = list(out = G),
        params = .tr_series_props(),
        help = .tr_series_ajuda(r"---[
Cada ano vira uma linha, e o eixo horizontal é o ciclo — jan a dez numa série
mensal, T1 a T4 numa trimestral. Responde duas perguntas de uma vez:

- qual é o FORMATO da sazonalidade — o pico é em julho? há dois picos?
- ele MUDA com o tempo — os anos recentes (cor mais clara) repetem a forma dos
  antigos, ou o pico migrou?

As linhas empilhadas de baixo para cima são a tendência; o formato de cada uma
é a sazonalidade. Se as oscilações crescem com o nível, a sazonalidade é
multiplicativa.

Pede série sazonal (frequência maior que 1).
]---", r"---[
Só os de aparência, abaixo.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("saz", "series/seasonal_plot", from = "pax")
]---", r"---[
`series/subseries` para como cada estação evoluiu; `series/stl` para medir a
sazonalidade.
]---", grafico = TRUE)),

      trama::tr_node("series/subseries", fn = tr_series_subseries, label = "Subséries sazonais",
        category = "serie_ver", icon = icone("chart-no-axes-combined"),
        description = "Um painel por estação, com os anos em sequência e a média de cada estação.",
        inputs = list(serie = S), outputs = list(out = G),
        params = .tr_series_props(),
        help = .tr_series_ajuda(r"---[
Um painel por estação (mês, trimestre), com os anos em sequência dentro de
cada um e a média daquela estação tracejada. É o complemento do
`series/seasonal_plot`: aquele mostra o formato do ciclo; este mostra como
CADA estação evoluiu.

As médias tracejadas desenham o padrão sazonal médio (a altura de cada painel).
As linhas mostram se uma estação se afastou das outras: um julho que sobe mais
que os outros meses — sazonalidade que muda — é invisível no sazonal e óbvio
aqui.

Pede série sazonal com pelo menos dois ciclos, e frequência até 24 — acima
disso seriam painéis demais; agregue antes com `series/aggregate`.
]---", r"---[
Só os de aparência, abaixo.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("gas", "series/example", dataset = "UKgas") |>
  tr_add("sub", "series/subseries", from = "gas")
]---", r"---[
`series/seasonal_plot` para o formato do ciclo; `series/aggregate` quando a
frequência é alta demais.
]---", grafico = TRUE)),

      trama::tr_node("series/plot_decomposition", role = "leitura", fn = tr_series_plot_decomposition,
        label = "Gráfico da decomposição",
        category = "serie_ver", icon = icone("file-chart-line"),
        description = "Os quatro componentes da decomposição, empilhados.",
        inputs = list(decomposicao = D), outputs = list(out = G),
        params = .tr_series_props(.aspecto = "4:3"),
        help = .tr_series_ajuda(r"---[
Desenha a série e os três componentes — tendência, sazonal, resto — em
painéis empilhados, com o tempo em comum.

**Cada painel tem a própria escala**, e é o que se tem de ler com cuidado: um
sazonal de ±40 e um resto de ±5 saem com a MESMA altura. O tamanho de cada
componente está nos números do eixo, não na altura do desenho. Um resto com
estrutura visível (ondas, degraus) quer dizer que a decomposição deixou algo
para trás.

O card da decomposição já mostra este gráfico; o nó existe para escolher
proporção, tema e título. A proporção padrão é 4:3 porque quatro painéis em
16:9 viram fitas.
]---", r"---[
Só os de aparência, abaixo.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("co2", "series/example", dataset = "co2") |>
  tr_add("stl", "series/stl", from = "co2") |>
  tr_add("g", "series/plot_decomposition", tema = "claro", from = "stl")
]---", r"---[
`series/stl` e `series/decompose` para a decomposição; `series/component` para
um componente só.
]---", grafico = TRUE)),

      trama::tr_node("series/plot_forecast", role = "leitura", fn = tr_series_plot_forecast, label = "Gráfico da previsão",
        category = "serie_ver", icon = icone("chart-area"),
        description = "O histórico e a previsão, com os leques de 80 e 95%.",
        inputs = list(previsao = F), outputs = list(out = G),
        params = .tr_series_props(historico = I(0L, min = 0L, max = 100000L,
                                                label = "Períodos de histórico")),
        help = .tr_series_ajuda(r"---[
O histórico em cinza, a previsão em cor, e os dois leques: o escuro é o
intervalo de 80%, o claro o de 95%. O leque ABRE com o horizonte — é a
incerteza crescendo, e a parte mais honesta do gráfico.

**Períodos de histórico** corta o passado mostrado: com 40 anos de série e 12
meses previstos, o leque vira um risco na ponta direita. Não muda a previsão,
que já foi feita no nó anterior — muda só o enquadramento.

O subtítulo diz o método (`ETS(M,Ad,M)`, `Seasonal naive method`).
]---", r"---[
- **Períodos de histórico** — quantos períodos do passado mostrar. 0 mostra
  tudo.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("ets", "series/ets", from = "pax") |>
  tr_add("prev", "series/forecast", horizonte = 24L, from = "ets") |>
  tr_add("g", "series/plot_forecast", historico = 48L, from = "prev")
]---", r"---[
`series/forecast` e `series/baseline` para a previsão; `series/accuracy` para
o erro.
]---", grafico = TRUE))
    )
  )
}
