# Os nós que chegaram depois dos cinco primeiros, um bloco por categoria.
#
# Moram fora de `trama_collection()` só pelo tamanho: a página de ajuda é
# metade de cada nó, e treze páginas numa função só tornariam impossível achar
# o nó que se quer editar. As funções recebem `P`, `PAINEL` e o tipo `G` de
# quem as chama para que a declaração continue lendo igual à dos irmãos.

.tr_view_nos_relacao <- function(P, PAINEL, G) {
  T <- "data/table"
  list(
    trama::tr_node("view/area", fn = tr_area, label = "Área",
      category = "relacao", description = "Empilha séries ao longo de um eixo com ordem.",
      icon = trama::tr_icon("chart-area"),
      inputs = list(dados = T), outputs = list(out = G),
      params = .tr_view_props(
        x   = P("cols", "", label = "Eixo X", example = "mes"),
        y   = P("cols", "", label = "Eixo Y", example = "receita"),
        cor = P("cols", "", label = "Empilhar por", example = "regiao"),
        painel = PAINEL),
      help = paste0("## Descrição

A linha preenchida até o zero, com as séries EMPILHADAS uma sobre a outra. O
topo da pilha é o total; cada faixa colorida é a parte de uma série nele. É o
gráfico de COMPOSIÇÃO ao longo de um eixo com ordem: a receita mensal somada das
regiões, e quanto dela veio de cada uma.

Vale para o X o mesmo que vale na **Linha**: precisa ter ORDEM — tempo, dose,
tamanho —, e precisa ser número ou data. Um X de texto não tem entre-pontos
para preencher, e o gráfico falha ao desenhar.

### O que se lê bem e o que não

O total e a série de BAIXO leem bem, porque partem de um chão reto. As de cima
não: cada uma flutua sobre a soma das anteriores, e uma faixa que parece
engordar pode estar só subindo junto com a de baixo. Para comparar a evolução
das séries entre si, a **Linha** com **Uma linha por** responde melhor; para a
parte de cada uma em cada momento, **Barras** com `proporção`.

Quando as séries têm X diferentes — uma região sem dado em março —, o gráfico
interpola cada uma nos X das outras antes de empilhar. É o que evita dentes,
mas quer dizer que o buraco foi preenchido por uma reta: trate os faltantes
antes se isso importar.

**Eixo X** e **Eixo Y** são obrigatórios e param o nó em branco. Coluna citada
que não existe para o nó e lista as colunas disponíveis.

## Parâmetros

- **Eixo X** — coluna com ordem, número ou data. Obrigatória.
- **Eixo Y** — coluna numérica empilhada. Obrigatória. Precisa ser positiva
  para a pilha fazer sentido: valor negativo empilha para baixo do zero.
- **Empilhar por** — coluna que separa as séries. Vazio desenha uma área só.
- **Painéis por** — coluna que divide o gráfico em painéis, na mesma escala.

## Valor

Um gráfico. No console, `tr_area(df, \"mes\", \"receita\", \"regiao\")` devolve um
ggplot comum, somável.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"mensal\", \"data/group_summarise\", by = \"mes, regiao\",
         name = \"receita\", expr = \"sum(valor)\", from = \"ler\") |>
  tr_add(\"pilha\", \"view/area\", x = \"mes\", y = \"receita\", cor = \"regiao\",
         titulo = \"Receita mensal, por região\", from = \"mensal\")
```

## Veja também

`view/line` para comparar as séries em vez de somá-las; `view/bars` com
posição `proporção` para a parte de cada uma; `data/group_summarise` para
chegar a uma linha por período e série antes de empilhar.", .TR_VIEW_AJUDA_APARENCIA)),

    trama::tr_node("view/bin2d", fn = tr_bin2d, label = "Grade de densidade",
      category = "relacao", description = "Conta as linhas em cada casa de uma grade, para nuvens grandes.",
      icon = trama::tr_icon("grid-3x3"),
      inputs = list(dados = T), outputs = list(out = G),
      params = .tr_view_props(
        x = P("cols", "", label = "Eixo X", example = "valor"),
        y = P("cols", "", label = "Eixo Y", example = "qtd"),
        classes = trama::tr_param_int(40L, min = 5L, max = 200L, label = "Classes"),
        log = trama::tr_param_enum("nenhum", .TR_VIEW_LOG, label = "Eixo em log"),
        painel = PAINEL),
      help = paste0("## Descrição

O **Disperso** para quando as linhas são tantas que as marcas se empilham. O
plano é cortado numa grade de casas iguais e cada casa é pintada pelo número de
linhas que caíram nela. Onde o disperso mostraria uma mancha sólida, sem
diferença entre mil pontos e cem mil, a grade mostra onde a nuvem é densa.

Ao contrário do disperso, aqui NÃO se agrega antes: o gráfico conta as linhas,
e a contagem é a informação.

### Classes, de novo

**Classes** é o número de casas em cada eixo, e faz aqui o que faz no
**Histograma**: poucas alisam e escondem estrutura, muitas deixam cada casa com
dois ou três pontos e a imagem vira sal e pimenta. O padrão 40 é pensado para
alguns milhares de linhas; com centenas, desça, ou volte ao disperso.

A escala de cor é a contínua do tema. Uma casa sem nenhuma linha não é pintada:
fica o fundo, que é diferente de uma casa com uma linha só.

**Eixo X** e **Eixo Y** são obrigatórios, numéricos, e param o nó em branco.

## Parâmetros

- **Eixo X** — coluna numérica horizontal. Obrigatória.
- **Eixo Y** — coluna numérica vertical. Obrigatória.
- **Classes** — casas por eixo, de 5 a 200. Padrão 40.
- **Eixo em log** — `nenhum` (padrão), `X`, `Y` ou `ambos`. Para medidas que
  variam em ordens de grandeza, ou crescem em proporção. Coluna com zero ou
  negativo para o nó e diz quantos valores impedem: o log deles não existe.
- **Painéis por** — coluna que divide o gráfico em painéis, na mesma escala.

## Valor

Um gráfico. No console, `tr_bin2d(df, \"valor\", \"qtd\")` devolve um ggplot
comum, somável — `+ ggplot2::scale_fill_continuous(transform = \"log10\")` põe a
contagem em log, quando umas poucas casas muito cheias apagam o resto.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"grade\", \"view/bin2d\", x = \"qtd\", y = \"valor\", classes = 30L,
         from = \"ler\")
```

## Veja também

`view/points` quando as linhas são poucas o bastante para se ver uma a uma;
`view/heatmap` quando os dois eixos são categorias, e não medidas;
`data/filter` para olhar de perto uma região da nuvem.", .TR_VIEW_AJUDA_APARENCIA)),

    trama::tr_node("view/heatmap", fn = tr_heatmap, label = "Mapa de calor",
      category = "relacao", description = "Pinta um número em cada par de duas categorias.",
      icon = trama::tr_icon("layout-grid"),
      inputs = list(dados = T), outputs = list(out = G),
      params = .tr_view_props(
        x = P("cols", "", label = "Colunas", example = "mes"),
        y = P("cols", "", label = "Linhas", example = "regiao"),
        valor = P("cols", "", label = "Valor", example = "receita"),
        rotulos = trama::tr_param_bool(FALSE, label = "Escrever valores")),
      help = paste0("## Descrição

Uma célula por par de valores de duas colunas — região × mês, máquina × turno —,
pintada pelo número daquele par. É a tabela de dupla entrada vista de uma vez:
onde o número se concentra, que linha ou coluna destoa, se há um padrão
diagonal.

### Valor em branco conta; preenchido soma

A regra é a das **Barras**, de propósito, para que os dois gráficos digam a
mesma coisa da mesma tabela. Sem **Valor**, cada célula é a CONTAGEM de linhas
do par. Com **Valor**, é a SOMA daquela coluna nas linhas do par. Média,
mediana ou qualquer outra conta: agregue ANTES, num **Agrupar e resumir** por
duas chaves, e aponte **Valor** para a coluna criada — com uma linha por par, a
soma é a identidade.

A agregação é feita pelo nó, e não pelo desenho: sem ela, duas linhas no mesmo
par seriam duas células no mesmo lugar, e só a última apareceria. Linhas com
**Colunas** ou **Linhas** faltantes ficam de fora. Par sem nenhuma linha fica
sem célula, e aparece como fundo — o que é diferente de zero.

### Cor não é comprimento

Cor é o canal que o olho compara PIOR: dá para ver o padrão, mas não para dizer
se uma célula é o dobro da outra. Para os números de verdade, ligue **Escrever
valores**, que põe o número em cada célula. Com dezenas de linhas e colunas, os
números viram poeira: aí, deixe desligado e leia o padrão.

**Colunas** e **Linhas** são obrigatórias e param o nó em branco. **Valor**
precisa ser numérico: texto para o nó e pede um **Converter tipo**.

## Parâmetros

- **Colunas** — categoria do eixo horizontal. Obrigatória.
- **Linhas** — categoria do eixo vertical. Obrigatória.
- **Valor** — coluna numérica somada por par. Vazio conta linhas.
- **Escrever valores** — escreve o número de cada célula. Desligado por padrão.

## Valor

Um gráfico. O objeto guarda a tabela JÁ agregada em `p$data`: no console,
`tr_heatmap(df, \"mes\", \"regiao\", \"receita\")$data` mostra os números que as
cores representam.

## Exemplos

Quantos pedidos por região e mês:

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"calor\", \"view/heatmap\", x = \"mes\", y = \"regiao\", rotulos = TRUE,
         from = \"ler\")
```

## Veja também

`view/bars` com **Subgrupo** para os mesmos pares lidos como comprimento;
`data/group_summarise` quando o número da célula não é soma nem contagem;
`view/bin2d` quando os dois eixos são medidas contínuas.", .TR_VIEW_AJUDA_APARENCIA)),
    trama::tr_node("view/labels", fn = tr_labels, label = "Disperso com rótulos",
      category = "relacao", description = "O disperso escrevendo o nome de cada ponto.",
      icon = trama::tr_icon("tags"),
      inputs = list(dados = T), outputs = list(out = G),
      params = .tr_view_props(
        x = P("cols", "", label = "Eixo X", example = "renda"),
        y = P("cols", "", label = "Eixo Y", example = "populacao"),
        rotulo = P("cols", "", label = "Rótulo", example = "municipio"),
        cor = P("cols", "", label = "Cor por", example = "regiao"),
        evitar = trama::tr_param_bool(TRUE, label = "Omitir rótulos sobrepostos"),
        log = trama::tr_param_enum("nenhum", .TR_VIEW_LOG, label = "Eixo em log"),
        painel = PAINEL),
      help = paste0("## Descrição

O **Disperso** com o valor de uma coluna escrito acima de cada ponto. É o
gráfico de quando os pontos têm NOME e o nome importa: municípios, variedades,
produtos, países — \"quem é aquele lá em cima, sozinho?\".

Serve para dezenas de pontos, não para milhares. Com muitos pontos, os nomes
viram uma mancha de texto; aí, filtre antes os que merecem nome, ou use o
disperso comum.

### Omitir rótulos sobrepostos

Ligado por padrão. Quando um rótulo cairia em cima de outro já escrito, ele NÃO
é escrito — o ponto continua lá, só sem o nome. A ordem de escrita é a ordem
das linhas da tabela: quem vem primeiro ganha o lugar. Para garantir o nome dos
pontos importantes, ordene a tabela antes num **Ordenar**, com os importantes
no topo.

A coleção não empurra os rótulos para os lados, como fazem os pacotes
especializados (`ggrepel`): isso pediria uma dependência que ela não tem.
Desligado, todos os rótulos são escritos, mesmo encavalados — útil para
conferir que nenhum sumiu.

**Eixo X**, **Eixo Y** e **Rótulo** são obrigatórios e param o nó em branco.

## Parâmetros

- **Eixo X** — coluna da medida horizontal. Obrigatória.
- **Eixo Y** — coluna da medida vertical. Obrigatória.
- **Rótulo** — coluna cujo valor é escrito junto de cada ponto. Obrigatória.
- **Cor por** — coluna que colore pontos e rótulos. Vazio desliga.
- **Omitir rótulos sobrepostos** — não escreve o rótulo que colidiria. Ligado
  por padrão.
- **Eixo em log** — `nenhum` (padrão), `X`, `Y` ou `ambos`. Coluna com zero ou
  negativo para o nó.
- **Painéis por** — coluna que divide o gráfico em painéis, na mesma escala.

## Valor

Um gráfico. No console, `tr_labels(df, \"renda\", \"populacao\", \"municipio\")`
devolve um ggplot comum, somável.

## Exemplos

Só os 20 maiores recebem ponto e nome:

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"municipios.csv\") |>
  tr_add(\"ordem\", \"data/arrange\", cols = \"populacao\", desc = TRUE, from = \"ler\") |>
  tr_add(\"top\", \"data/slice_head\", n = 20L, from = \"ordem\") |>
  tr_add(\"nomes\", \"view/labels\", x = \"renda\", y = \"populacao\",
         rotulo = \"municipio\", from = \"top\")
```

## Veja também

`view/points` para a nuvem sem nomes; `view/dotplot` quando a pergunta é o
ranking de UMA medida, e não a relação entre duas; `data/arrange` para decidir
quem ganha o rótulo; `data/filter` para ficar só com os pontos que merecem nome.", .TR_VIEW_AJUDA_APARENCIA))
  )
}

.tr_view_nos_distribuicao <- function(P, PAINEL, G) {
  T <- "data/table"
  list(
    trama::tr_node("view/density", fn = tr_density, label = "Densidade",
      category = "distribuicao", description = "Desenha a forma de uma medida como curva suave.",
      icon = trama::tr_icon("chart-spline"),
      inputs = list(dados = T), outputs = list(out = G),
      params = .tr_view_props(
        x = P("cols", "", label = "Medida", example = "valor"),
        cor = P("cols", "", label = "Separar por", example = "regiao"),
        suavidade = trama::tr_param_num(1, min = 0.2, max = 5, step = 0.1, label = "Suavidade"),
        log = trama::tr_param_bool(FALSE, label = "Eixo em log"),
        painel = PAINEL),
      help = paste0("## Descrição

O **Histograma** sem o corte em classes: uma curva que alisa as observações e
mostra a forma da distribuição — corcovas, cauda, assimetria. A área debaixo de
cada curva é 1, de modo que grupos de TAMANHOS diferentes saem na mesma escala
e as formas se comparam direto. É a vantagem sobre o histograma com **Separar
por**, e também o seu custo: a curva não diz quantas linhas cada grupo tem.

Com **Separar por**, as curvas se SOBREPÕEM com transparência, cada uma a
partir do zero — não há empilhamento aqui.

### Suavidade: o param que muda a conclusão

É o **Classes** desta forma. O R escolhe uma largura de alisamento pela regra de
Silverman, e **Suavidade** a multiplica: `1` é a escolha automática, `0.5` é
metade da largura (mais detalhe, mais ruído), `2` é o dobro (mais liso, e
corcovas próximas se fundem numa). Suba e desça e veja o que sobrevive — o que
aparece em todas é estrutura.

A curva também se estende um pouco além do menor e do maior valor, e não sabe
de limites naturais: uma medida que não pode ser negativa ganha uma cauda
abaixo de zero. É o desenho, não o dado.

**Medida** é obrigatória, numérica, e para o nó em branco.

## Parâmetros

- **Medida** — coluna numérica. Obrigatória.
- **Separar por** — coluna que dá uma curva por grupo, sobrepostas. Vazio
  desenha a curva da amostra inteira.
- **Suavidade** — multiplicador da largura automática, de 0,2 a 5. Padrão 1.
- **Eixo em log** — põe a medida em escala log. Coluna com zero ou negativo
  para o nó e diz quantos valores impedem: o log deles não existe.
  A curva é estimada na escala log, o que também tira a cauda falsa abaixo
  de zero.
- **Painéis por** — coluna que divide o gráfico em painéis, na mesma escala.

## Valor

Um gráfico. No console, `tr_density(df, \"valor\", suavidade = .5)` devolve um
ggplot comum, somável.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"forma\", \"view/density\", x = \"valor\", cor = \"regiao\",
         from = \"ler\")
```

## Veja também

`view/histogram` quando a contagem importa, e não só a forma; `view/violin`
para as mesmas curvas lado a lado, uma por grupo; `view/ecdf` para comparar
grupos sem escolher suavidade nenhuma.", .TR_VIEW_AJUDA_APARENCIA)),

    trama::tr_node("view/violin", fn = tr_violin, label = "Violino",
      category = "distribuicao", description = "Compara a forma de uma medida entre grupos.",
      icon = trama::tr_icon("audio-waveform"),
      inputs = list(dados = T), outputs = list(out = G),
      params = .tr_view_props(
        y = P("cols", "", label = "Medida", example = "valor"),
        x = P("cols", "", label = "Grupo", example = "regiao"),
        cor = P("cols", "", label = "Preencher por", example = "produto"),
        caixa = trama::tr_param_bool(TRUE, label = "Caixa por dentro"),
        pontos = trama::tr_param_bool(FALSE, label = "Mostrar observações"),
        log = trama::tr_param_bool(FALSE, label = "Eixo em log"),
        painel = PAINEL),
      help = paste0("## Descrição

Uma curva de densidade por grupo, espelhada em volta do eixo, todas na mesma
escala vertical. Onde o violino é largo há muitas observações; onde é fino,
poucas. É o **Boxplot** que não esconde a forma: duas corcovas aparecem como
duas barrigas, e a caixa que as resumiria numa só mentira fica evidente.

### Caixa por dentro

Ligada por padrão, uma caixa estreita dentro de cada violino dá os números que a
curva não dá — mediana (a linha), quartis (a caixa) e bigodes, com a mesma
definição do **Boxplot**. Forma e resumo no mesmo desenho, que é o motivo de o
padrão ser ligado.

### O que o violino exagera

A largura de cada violino é normalizada: um grupo com dez linhas e outro com dez
mil saem do mesmo tamanho, e o de dez linhas desenha uma forma cheia de
confiança sobre quase nada. Ligue **Mostrar observações** para ver quantas há,
ou confira no **Boxplot** com observações. E o violino corta nas pontas o que
não foi observado, mas alisa entre elas: um vale real entre duas corcovas pode
ser preenchido.

**Medida** é obrigatória, numérica, e para o nó em branco. **Grupo** vazio
desenha um violino só, da amostra inteira.

## Parâmetros

- **Medida** — coluna numérica. Obrigatória.
- **Grupo** — categoria do eixo horizontal. Vazio: um violino só.
- **Preencher por** — coluna que subdivide cada grupo em violinos lado a lado.
- **Caixa por dentro** — caixa estreita dentro de cada violino. Ligada por
  padrão.
- **Mostrar observações** — as linhas espalhadas na horizontal por cima.
- **Eixo em log** — põe a medida em escala log. Coluna com zero ou negativo
  para o nó e diz quantos valores impedem: o log deles não existe.
- **Painéis por** — coluna que divide o gráfico em painéis, na mesma escala.

## Valor

Um gráfico. No console, `tr_violin(df, \"valor\", \"regiao\")` devolve um ggplot
comum, somável.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"violinos\", \"view/violin\", y = \"valor\", x = \"regiao\",
         from = \"ler\")
```

## Veja também

`view/boxplot` para o resumo sem a forma, quando os grupos são muitos;
`view/density` para as curvas sobrepostas num eixo só; `view/means` quando a
pergunta é a média de cada grupo.", .TR_VIEW_AJUDA_APARENCIA)),

    trama::tr_node("view/ecdf", fn = tr_ecdf, label = "Acumulada",
      category = "distribuicao",
      description = "Mostra que proporção das linhas fica até cada valor.",
      icon = trama::tr_icon("chart-no-axes-combined"),
      inputs = list(dados = T), outputs = list(out = G),
      params = .tr_view_props(
        x = P("cols", "", label = "Medida", example = "valor"),
        cor = P("cols", "", label = "Separar por", example = "regiao"),
        log = trama::tr_param_bool(FALSE, label = "Eixo em log"),
        painel = PAINEL),
      help = paste0("## Descrição

A distribuição acumulada empírica: para cada valor no eixo X, a curva diz que
proporção das linhas é MENOR OU IGUAL a ele. Sobe em degraus, um por
observação, de 0% a 100%.

É a forma de distribuição sem nenhum param que mude a conclusão: nem
**Classes**, como o histograma, nem **Suavidade**, como a densidade. Cada
observação está no desenho, exatamente onde está. O preço é a leitura, que pede
um minuto de costume:

- a MEDIANA é o X onde a curva cruza 50%; os quartis, 25% e 75%;
- trecho ÍNGREME é onde os valores se concentram; trecho deitado, onde são
  raros;
- entre grupos, a curva mais à DIREITA tem valores maiores — se ela fica à
  direita de ponta a ponta, o grupo é maior em toda a distribuição, e não só na
  média.

A última frase é a razão de o gráfico existir: é a melhor forma de ver se dois
grupos diferem no nível, no espalhamento (curvas com inclinações diferentes que
se cruzam) ou só numa cauda.

**Medida** é obrigatória, numérica, e para o nó em branco.

## Parâmetros

- **Medida** — coluna numérica. Obrigatória.
- **Separar por** — coluna que dá uma curva por grupo. Vazio desenha a curva
  da amostra inteira.
- **Eixo em log** — põe a medida em escala log. Coluna com zero ou negativo
  para o nó e diz quantos valores impedem: o log deles não existe.
- **Painéis por** — coluna que divide o gráfico em painéis, na mesma escala.

## Valor

Um gráfico. No console, `tr_ecdf(df, \"valor\", \"regiao\")` devolve um ggplot
comum, somável.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"acumulada\", \"view/ecdf\", x = \"valor\", cor = \"regiao\",
         from = \"ler\")
```

## Veja também

`view/density` e `view/histogram` para a forma lida como altura, que é mais
imediata; `view/qq` para comparar a distribuição com a normal.", .TR_VIEW_AJUDA_APARENCIA)),

    trama::tr_node("view/qq", fn = tr_qq, label = "Quantil-quantil",
      category = "distribuicao",
      description = "Compara a distribuição de uma medida com a normal.",
      icon = trama::tr_icon("trending-up"),
      inputs = list(dados = T), outputs = list(out = G),
      params = .tr_view_props(
        y = P("cols", "", label = "Medida", example = "valor"),
        cor = P("cols", "", label = "Separar por", example = "regiao"),
        painel = PAINEL),
      help = paste0("## Descrição

Ordena as observações e põe cada uma contra o valor que ocuparia a mesma
posição numa distribuição normal. Se a medida é normal, os pontos caem sobre a
reta; o jeito como fogem dela diz COMO a distribuição difere da normal.

### Como ler

- pontos sobre a reta, com um tremor nas pontas — normal o bastante;
- as duas pontas se afastando para FORA (a de baixo abaixo, a de cima acima) —
  caudas mais pesadas que a normal: valores extremos mais frequentes;
- as duas pontas se dobrando para DENTRO — caudas mais leves;
- uma curva em arco, com as duas pontas do mesmo lado — assimetria: arco
  côncavo para cima é cauda à direita (o caso de renda, tempo, contagem);
- degraus horizontais — valores repetidos, medida arredondada ou discreta.

A reta passa pelos QUARTIS da amostra, e não pela média e desvio. É de
propósito: assim ela não é puxada pelas caudas, que são justamente o que o
gráfico existe para mostrar.

### Não é teste

Com poucas linhas, até dados normais fazem curvas; com milhares, qualquer
desvio minúsculo aparece nítido. O gráfico mostra o tamanho e a forma do
desvio, que é o que importa para decidir; para um valor-p, há o teste de
Shapiro-Wilk na coleção de modelos. E para checar o pressuposto de um modelo, o
que precisa ser normal são os RESÍDUOS, e não a medida crua.

**Medida** é obrigatória, numérica, e para o nó em branco.

## Parâmetros

- **Medida** — coluna numérica. Obrigatória.
- **Separar por** — coluna que dá pontos e reta por grupo, na mesma imagem.
  Grupos com níveis diferentes se sobrepõem mal: prefira **Painéis por**.
- **Painéis por** — coluna que divide o gráfico em painéis, um QQ por grupo.

## Valor

Um gráfico. No console, `tr_qq(df, \"valor\")` devolve um ggplot comum, somável.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"normal\", \"view/qq\", y = \"valor\", painel = \"regiao\",
         from = \"ler\")
```

## Veja também

`view/histogram` e `view/density` para ver a forma que o QQ resume;
`view/ecdf` para comparar grupos entre si, e não com a normal;
`data/mutate` para transformar a medida (log, raiz) e olhar de novo.", .TR_VIEW_AJUDA_APARENCIA)),
    trama::tr_node("view/strip", fn = tr_strip, label = "Faixa de pontos",
      category = "distribuicao", description = "Mostra cada observação, por grupo, sem resumir.",
      icon = trama::tr_icon("grip-vertical"),
      inputs = list(dados = T), outputs = list(out = G),
      params = .tr_view_props(
        y = P("cols", "", label = "Medida", example = "altura"),
        x = P("cols", "", label = "Grupo", example = "tratamento"),
        cor = P("cols", "", label = "Cor por", example = "bloco"),
        estilo = trama::tr_param_enum("colmeia", .TR_VIEW_ESTILOS_FAIXA, label = "Estilo"),
        resumo = trama::tr_param_enum("mediana", .TR_VIEW_RESUMOS, label = "Traço de resumo"),
        log = trama::tr_param_bool(FALSE, label = "Eixo em log"),
        painel = PAINEL),
      help = paste0("## Descrição

Um ponto por LINHA da tabela, arrumado em faixas verticais, uma por grupo. Não
há caixa, curva nem classe: o que se vê são os dados. É o gráfico certo para
grupos PEQUENOS — as quatro repetições de um tratamento, os doze animais de um
lote —, onde **Boxplot** e **Violino** fingem resumir uma forma que cinco
pontos não têm.

### Estilo

- `colmeia` (padrão) — pontos de valores parecidos se afastam para os lados,
  alternando direita e esquerda, e a LARGURA da faixa passa a mostrar onde os
  valores se concentram. Nenhum ponto cobre outro.
- `espalhado` — cada ponto é deslocado na horizontal por uma quantidade que
  parece sorteada. Mais leve com muitos pontos, mas dois pontos iguais podem
  cair perto um do outro.

Nos dois, o deslocamento é só horizontal, e é o MESMO a cada execução: a
altura de cada ponto é exatamente o valor, e a imagem não muda sozinha entre
uma execução e outra.

### Traço de resumo

Um traço horizontal na `mediana` (padrão) ou na `média` de cada grupo, sobre os
pontos. `nenhum` o tira. Com o eixo em log, prefira a mediana: a média é a dos
valores crus, e cai acima do meio da nuvem na escala log.

**Medida** é obrigatória, numérica, e para o nó em branco. **Grupo** vazio
desenha uma faixa só. **Cor por** colore os pontos sem separar as faixas — os
subgrupos ficam misturados na mesma faixa, que é o que se quer para ver um
bloco ou uma repetição dentro do tratamento.

## Parâmetros

- **Medida** — coluna numérica. Obrigatória.
- **Grupo** — categoria do eixo horizontal. Vazio: uma faixa só.
- **Cor por** — coluna que colore os pontos, na mesma faixa.
- **Estilo** — `colmeia` (padrão) ou `espalhado`.
- **Traço de resumo** — `mediana` (padrão), `média` ou `nenhum`.
- **Eixo em log** — põe a medida em escala log. Coluna com zero ou negativo
  para o nó.
- **Painéis por** — coluna que divide o gráfico em painéis, na mesma escala.

## Valor

Um gráfico. O eixo horizontal é numérico por baixo, com os nomes dos grupos nos
marcadores: a tabela do objeto ganha a coluna `posicao_x`, que é onde cada
ponto foi desenhado.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"ensaio.csv\") |>
  tr_add(\"faixas\", \"view/strip\", y = \"altura\", x = \"tratamento\",
         cor = \"bloco\", from = \"ler\")
```

## Veja também

`view/boxplot` e `view/violin` quando os grupos são grandes o bastante para ter
forma; `view/means` para a média com a incerteza dela; `view/paired` quando a
mesma unidade aparece em mais de um grupo.", .TR_VIEW_AJUDA_APARENCIA))
  )
}

.tr_view_nos_comparacao <- function(P, PAINEL, G) {
  T <- "data/table"
  list(
    trama::tr_node("view/means", fn = tr_means, label = "Médias com barras",
      category = "comparacao",
      description = "Compara a média de uma medida entre grupos, com a incerteza.",
      icon = trama::tr_icon("git-commit-vertical"),
      inputs = list(dados = T), outputs = list(out = G),
      params = .tr_view_props(
        x = P("cols", "", label = "Grupo", example = "regiao"),
        y = P("cols", "", label = "Medida", example = "valor"),
        cor = P("cols", "", label = "Separar por", example = "produto"),
        barra = trama::tr_param_enum("IC", .TR_VIEW_BARRAS_ERRO, label = "Barra"),
        confianca = trama::tr_param_num(0.95, min = 0.5, max = 0.999, step = 0.01, label = "Confiança (IC)"),
        painel = PAINEL),
      help = paste0("## Descrição

Um ponto na média de cada grupo e uma barra em volta dele. É o gráfico de
responder \"qual grupo tem a média maior, e com que segurança\" — o que as
**Barras** fazem mal quando a altura é média: a barra cheia gasta tinta num
zero que não interessa, e não mostra incerteza nenhuma.

O nó CALCULA as médias; não agregue antes. Ele recebe a tabela de linhas e
agrupa por **Grupo**, **Separar por** e **Painéis por**.

### A barra: três perguntas diferentes

Merece um parágrafo próprio, porque é a fonte do erro mais comum ao ler este
gráfico — confundir as três:

- `IC` (padrão) — intervalo de confiança da MÉDIA, no nível de **Confiança**,
  pela t de Student com n − 1 graus de liberdade, o mesmo do `t.test()`. Diz onde a média do grupo
  provavelmente está. Encolhe quando o grupo cresce.
- `erro padrão` — o desvio padrão dividido por √n: a incerteza da média numa
  unidade só. É cerca de metade do IC 95% com grupos grandes, e parece mais
  preciso do que é. Diga sempre qual se usou — o rótulo do eixo diz.
- `desvio padrão` — o espalhamento dos DADOS em volta da média. Não encolhe com
  n. É a barra certa para descrever os grupos, e a errada para compará-los.

Barras de IC que NÃO se sobrepõem indicam médias diferentes; barras que se
sobrepõem um pouco NÃO provam que são iguais — dois ICs de 95% podem se
sobrepor com uma diferença significativa. Para o teste de verdade, a ANOVA e
as comparações múltiplas da coleção de modelos.

Grupo com uma linha só não tem desvio: sai o ponto sem barra. Linhas com a
medida faltante ficam fora da conta, e linhas com **Grupo** faltante ficam fora
do gráfico.

**Grupo** e **Medida** são obrigatórios. **Medida** precisa ser numérica:
texto para o nó e pede um **Converter tipo**, em vez de desenhar um gráfico
vazio.

## Parâmetros

- **Grupo** — categoria do eixo horizontal. Obrigatória.
- **Medida** — coluna numérica cuja média se calcula. Obrigatória.
- **Separar por** — coluna que dá pontos lado a lado, por cor, em cada grupo.
- **Barra** — `IC` (padrão), `erro padrão` ou `desvio padrão`.
- **Confiança (IC)** — nível do IC, de 0,5 a 0,999 (padrão 0,95 = 95%). Só vale
  com **Barra** `IC`; o rótulo do eixo mostra o nível usado.
- **Painéis por** — coluna que divide o gráfico em painéis, na mesma escala.

## Valor

Um gráfico. O objeto guarda a tabela calculada em `p$data`, com `n`, `media`,
`inferior` e `superior` por grupo: no console,
`tr_means(df, \"regiao\", \"valor\")$data` mostra os números desenhados.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"medias\", \"view/means\", x = \"regiao\", y = \"valor\",
         barra = \"IC\", confianca = 0.9, from = \"ler\")
```

## Veja também

`view/boxplot` e `view/violin` para a distribuição inteira de cada grupo, e não
só a média; `view/bars` para somas e contagens; `data/group_summarise` para ter
a tabela de médias como tabela.", .TR_VIEW_AJUDA_APARENCIA)),

    trama::tr_node("view/dotplot", fn = tr_dotplot, label = "Pontos ordenados",
      category = "comparacao", description = "Um ponto por categoria, em ranking.",
      icon = trama::tr_icon("lollipop"),
      inputs = list(dados = T), outputs = list(out = G),
      params = .tr_view_props(
        x = P("cols", "", label = "Categoria", example = "municipio"),
        y = P("cols", "", label = "Valor", example = "producao"),
        cor = P("cols", "", label = "Cor por", example = "safra"),
        estilo = trama::tr_param_enum("pontos", .TR_VIEW_ESTILOS_PONTOS, label = "Estilo"),
        ordenar = trama::tr_param_bool(TRUE, label = "Ordenar pelo valor"),
        painel = PAINEL),
      help = paste0("## Descrição

Um ponto por categoria, com as categorias empilhadas no eixo VERTICAL e o valor
no horizontal, do maior (em cima) para o menor. É o ranking legível para
DEZENAS de categorias com nome: as **Barras** com trinta categorias viram uma
parede de tinta, e os nomes não cabem embaixo delas.

O desenho é sempre deitado, e isso tem uma consequência nos cosméticos:
**Rótulo do X** é o do eixo horizontal, que aqui é o do VALOR, e **Rótulo do
Y** é o das categorias.

### Valor: conta ou soma, como nas barras

Sem **Valor**, cada ponto é a contagem de linhas da categoria; com **Valor**, a
soma. Para média ou outra conta, agregue antes num **Agrupar e resumir**.

### Estilo: com ou sem haste

- `pontos` (padrão) — só o ponto. O eixo NÃO precisa começar no zero, e é essa
  a vantagem sobre as barras quando os valores são todos altos e próximos (a
  produtividade de trinta variedades entre 3,1 e 3,6 t/ha): a diferença
  aparece, em vez de trinta barras quase iguais.
- `pirulito` — o ponto com uma haste desde o zero. Volta a ser uma barra, mais
  leve; use quando o zero importa para a leitura.

**Ordenar pelo valor** vem ligado: ranking é a razão do gráfico. Desligue quando
a categoria tem ordem própria (meses, faixas), e aí as categorias seguem os
níveis da coluna, de cima para baixo.

**Cor por** põe um ponto por subgrupo na mesma linha — duas safras por
município, digamos. Para ligar os dois pontos com a distância entre eles, os
**Halteres** respondem melhor.

**Categoria** é obrigatória e para o nó em branco.

## Parâmetros

- **Categoria** — coluna das linhas do ranking. Obrigatória.
- **Valor** — coluna numérica somada por categoria. Vazio conta linhas.
- **Cor por** — coluna que dá um ponto por subgrupo, por cor.
- **Estilo** — `pontos` (padrão) ou `pirulito`.
- **Ordenar pelo valor** — maior em cima. Ligado por padrão.
- **Painéis por** — coluna que divide o gráfico em painéis, na mesma escala.

## Valor

Um gráfico. O objeto guarda a tabela agregada em `p$data`.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"producao.csv\") |>
  tr_add(\"ranking\", \"view/dotplot\", x = \"municipio\", y = \"producao\",
         from = \"ler\")
```

## Veja também

`view/bars` para poucas categorias, ou quando a composição importa;
`view/dumbbell` para comparar duas condições por categoria;
`data/group_summarise` quando o valor não é soma nem contagem.", .TR_VIEW_AJUDA_APARENCIA)),

    trama::tr_node("view/dumbbell", fn = tr_dumbbell, label = "Halteres",
      category = "comparacao", description = "Liga dois valores de cada categoria, como antes e depois.",
      icon = trama::tr_icon("dumbbell"),
      inputs = list(dados = T), outputs = list(out = G),
      params = .tr_view_props(
        x = P("cols", "", label = "Categoria", example = "regiao"),
        cor = P("cols", "", label = "Condição", example = "ano"),
        y = P("cols", "", label = "Valor", example = "receita"),
        ordenar = trama::tr_param_bool(TRUE, label = "Ordenar pela diferença")),
      help = paste0("## Descrição

Para cada categoria, um ponto por condição — 2023 e 2024, antes e depois,
tratado e controle — ligados por um segmento. O comprimento do segmento é a
DIFERENÇA, e ela é o que o gráfico mostra: onde cresceu, onde caiu, onde quase
não mudou. Com barras lado a lado a mesma informação está lá, mas o olho tem de
subtrair duas alturas em cada par.

### A tabela vem longa

Uma linha por categoria por condição, com a condição numa COLUNA — a forma que
sai de um **Agrupar e resumir** por duas chaves, ou de um **Pivotar para
longo** de uma tabela com uma coluna por ano. Várias linhas no mesmo par são
somadas.

**Condição** precisa de pelo menos dois valores distintos; com um só não há o
que ligar, e o nó para. Com três ou mais, todos os pontos aparecem e o segmento
vai do menor ao maior.

### Ordenar pela diferença

Ligado, as categorias saem da maior diferença (em cima) para a menor, sendo a
diferença a ÚLTIMA condição menos a PRIMEIRA, na ordem dos valores da coluna:
texto em ordem alfabética (`2023` antes de `2024`, `antes` antes de `depois`),
fator na ordem dos níveis. Se a ordem alfabética não for a do tempo, converta a
coluna para fator antes. Categoria sem uma das duas condições fica embaixo.

Os três campos são obrigatórios, e **Valor** precisa ser numérico.

## Parâmetros

- **Categoria** — coluna das linhas do gráfico. Obrigatória.
- **Condição** — coluna com as condições comparadas, ao menos dois valores.
  Obrigatória.
- **Valor** — coluna numérica, somada por categoria e condição. Obrigatória.
- **Ordenar pela diferença** — última menos primeira condição, maior em cima.
  Ligado por padrão.

## Valor

Um gráfico, deitado: **Rótulo do X** é o do valor, **Rótulo do Y** o das
categorias. O objeto guarda a tabela agregada em `p$data`.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"anual\", \"data/group_summarise\", by = \"regiao, ano\",
         name = \"receita\", expr = \"sum(valor)\", from = \"ler\") |>
  tr_add(\"halteres\", \"view/dumbbell\", x = \"regiao\", cor = \"ano\",
         y = \"receita\", from = \"anual\")
```

## Veja também

`view/paired` quando cada categoria é uma UNIDADE medida duas vezes e há muitas
delas; `view/dotplot` para uma condição só; `data/pivot_longer` para chegar à
tabela longa.", .TR_VIEW_AJUDA_APARENCIA)),

    trama::tr_node("view/paired", fn = tr_paired, label = "Pareamento",
      category = "comparacao",
      description = "Liga a mesma unidade entre condições, uma linha por unidade.",
      icon = trama::tr_icon("git-compare-arrows"),
      inputs = list(dados = T), outputs = list(out = G),
      params = .tr_view_props(
        x = P("cols", "", label = "Condição", example = "fase"),
        y = P("cols", "", label = "Medida", example = "altura"),
        unidade = P("cols", "", label = "Unidade", example = "planta"),
        cor = P("cols", "", label = "Cor por", example = "tratamento"),
        media = trama::tr_param_bool(TRUE, label = "Linha da média"),
        painel = PAINEL),
      help = paste0("## Descrição

A mesma unidade — planta, paciente, parcela, loja — medida em cada condição,
com uma linha ligando as medidas dela. É o gráfico dos dados PAREADOS, e o que
ele mostra e os outros escondem é a CONSISTÊNCIA: se quase todas as linhas
sobem, o efeito é de todos; se metade sobe e metade desce, a média pode subir
por causa de três unidades.

Um **Boxplot** por condição apagaria o pareamento: as duas caixas podem se
sobrepor inteiras e, ainda assim, cada unidade ter crescido. É o mesmo motivo
de o teste t pareado ser mais sensível que o de amostras independentes.

### Uma medida por unidade por condição

O par **Unidade** × **Condição** não pode repetir. Se repetir, o nó para e diz
quantos pares — e não tira a média calado. Repetição quase sempre é a coluna
de unidade errada (o lote no lugar da planta) ou medidas repetidas que
precisam ser resumidas ANTES, num **Agrupar e resumir**, com uma decisão
explícita sobre o que resumir quer dizer.

A ordem das condições no eixo é a dos valores da coluna: texto em ordem
alfabética, fator na ordem dos níveis. `antes`/`depois` sai certo; `pré`/`pós`
sai invertido, e pede um **Converter tipo** para fator com os níveis na ordem.

### Linha da média

Ligada por padrão: a média de cada condição, em traço grosso sobre as linhas
das unidades. Com **Cor por**, uma linha de média por grupo.

**Condição**, **Medida** e **Unidade** são obrigatórios; **Medida** é numérica,
e **Condição** precisa de ao menos dois valores.

## Parâmetros

- **Condição** — coluna das condições, no eixo horizontal. Obrigatória.
- **Medida** — coluna numérica. Obrigatória.
- **Unidade** — coluna que identifica quem foi medido. Obrigatória.
- **Cor por** — coluna que colore as unidades por grupo, e dá uma média por
  grupo.
- **Linha da média** — a média de cada condição, em destaque. Ligada por
  padrão.
- **Painéis por** — coluna que divide o gráfico em painéis, na mesma escala.

## Valor

Um gráfico. No console, `tr_paired(df, \"fase\", \"altura\", \"planta\")` devolve
um ggplot comum, somável.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"medidas.csv\") |>
  tr_add(\"pares\", \"view/paired\", x = \"fase\", y = \"altura\",
         unidade = \"planta\", cor = \"tratamento\", from = \"ler\")
```

## Veja também

`view/dumbbell` para poucas categorias com nome; `view/strip` quando as
unidades NÃO se repetem entre os grupos; `data/group_summarise` para chegar a
uma medida por unidade e condição.", .TR_VIEW_AJUDA_APARENCIA)),

    trama::tr_node("view/pareto", fn = tr_pareto, label = "Pareto",
      category = "comparacao",
      description = "Ordena as categorias e mostra quanto do total elas acumulam.",
      icon = trama::tr_icon("chart-column-decreasing"),
      inputs = list(dados = T), outputs = list(out = G),
      params = .tr_view_props(
        x = P("cols", "", label = "Categoria", example = "defeito"),
        y = P("cols", "", label = "Valor", example = "custo"),
        referencia = trama::tr_param_num(80, min = 0, max = 100, step = 5,
                                         label = "Referência (%)", unit = "%")),
      help = paste0("## Descrição

Barras da maior categoria para a menor, e sobre elas uma linha com o total
ACUMULADO: o eixo da esquerda é o da barra, o da direita diz que porcentagem do
total as categorias até ali somam. É o gráfico da regra do 80/20 — quais poucos
tipos de defeito, motivos de reclamação ou produtos respondem pela maior parte
do problema.

A linha e o eixo da direita são a MESMA escala das barras, relida em
porcentagem: o 100% está exatamente na altura da soma de todas as barras, e
não em qualquer lugar. A **Referência** tracejada cai onde o acumulado chega a
esse percentual.

### Valor: conta ou soma

Sem **Valor**, cada barra é a contagem de linhas — quantas ocorrências de cada
defeito. Com **Valor**, a soma — o custo de cada defeito, que costuma dar um
Pareto bem diferente do das ocorrências, e é muitas vezes o que se queria.

Valores negativos param o nó: o Pareto acumula PARCELAS de um total, e
parcela negativa faz a linha descer. Categoria faltante fica de fora.

Com muitas categorias pequenas, a cauda vira uma fileira de barras
minúsculas. Agrupe-as antes numa categoria \"outros\", num **Criar/alterar
colunas** — o Pareto clássico põe \"outros\" por último, mesmo que não seja a
menor, e aqui ele ficará onde a soma o puser.

**Categoria** é obrigatória e para o nó em branco.

## Parâmetros

- **Categoria** — coluna das barras. Obrigatória.
- **Valor** — coluna numérica, não negativa, somada por categoria. Vazio conta
  linhas.
- **Referência (%)** — linha tracejada no percentual acumulado, de 0 a 100.
  Padrão 80; `0` a desliga.

## Valor

Um gráfico. O objeto guarda em `p$data` a tabela ordenada com `acumulado` e
`acumulado_pct` — as categorias que somam até 80% são as de `acumulado_pct`
até 0,8.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"inspecao.csv\") |>
  tr_add(\"pareto\", \"view/pareto\", x = \"defeito\", y = \"custo\",
         from = \"ler\")
```

## Veja também

`view/bars` com **Ordenar pela altura** para o ranking sem o acumulado;
`view/dotplot` para dezenas de categorias; `data/mutate` para agrupar a cauda
em \"outros\".", .TR_VIEW_AJUDA_APARENCIA))
  )
}
