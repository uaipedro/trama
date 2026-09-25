# A seção de aparência é IDÊNTICA em todos os gráficos, então mora aqui uma vez
# só. Repeti-la em cada `help` seria repetir o lugar onde uma coleção
# apodrece mais rápido: a documentação que ninguém atualiza em todas as cópias.
# Mesma figura do `.tr_view_props()`, que declara os mesmos seis params uma vez.
.TR_VIEW_AJUDA_APARENCIA <- "
### Aparência (comum a todos os gráficos)

- **Proporção** — a forma da imagem: `16:9` e `2:1` para paisagem, `1:1` para
  quadrado, `4:3` e `3:4` para o que vai numa página. A imagem sai sempre com
  1600 px no lado maior; o card só a escala, e um clique a abre em tela cheia.
  Mudar a proporção RECOMPUTA o gráfico — é o único param de aparência que
  muda mesmo o desenho, porque o ggplot recoloca a legenda e remede os rótulos.
  Arrastar a alça do card, não: aquilo é tamanho, não proporção.
- **Tema** — `padrão` segue o tema padrão do projeto; os temas (fundo, cores,
  fonte, paleta) são do projeto, e não do gráfico. Sem temas próprios valem os
  embutidos, com `escuro` como padrão; `claro` e `clássico` servem bem ao que
  sai no relatório. O tema dos gráficos não segue o claro/escuro do editor, que
  é preferência de cada pessoa: para mudar todos de uma vez, troque o tema
  padrão em ⚙ Configurações. A paleta só entra onde o
  gráfico não escolheu cores por conta própria.
- **Título**, **Rótulo do X**, **Rótulo do Y** — em branco, o gráfico usa o
  nome da coluna, que costuma ser a legenda certa.
- **Legenda** — `nenhuma` quando a cor já está explicada no título.
"

#' A coleção `view`.
#'
#' Não declara `js`: o preview aponta pro renderer `trama/image`, que é do
#' núcleo, e a coleção não traz uma linha de JavaScript. É de propósito, e é o
#' teste mais duro do contrato de renderização — uma coleção inteira que ganha
#' visualização, lightbox e a vista `resumo` sem tocar no front.
#'
#' As portas de entrada são `data/table`, que pertence à coleção `data`. Isso
#' faz `trama.data` uma dependência REAL nos dois níveis: pacote em `Imports` e
#' coleção carregada ANTES no registro (`R/registry.R:63-68` recusa porta com
#' tipo desconhecido). A alternativa — um tipo tabular próprio, ligado por
#' adaptador — só moveria o acoplamento pra ordem de carga, onde ele falha em
#' runtime em vez de no `DESCRIPTION`.
#'
#' A ordem das categorias é a pergunta que se faz antes de escolher o gráfico:
#' o que eu quero MOSTRAR? Relação entre duas medidas, a forma de uma, ou a
#' comparação entre categorias. É a mesma disciplina da paleta da coleção
#' `data`, onde a ordem sugere o próximo passo.
#' @export
trama_collection <- function() {
  T <- "data/table"
  G <- "view/plot"
  P <- trama::tr_param
  PAINEL <- .tr_view_painel_param()
  .tr_view_aplicar_ajuda_curta(trama::tr_collection(
    id = "view", version = "0.1.0", label = "Gráficos",
    types = list(view_plot_type()),
    categories = list(
      trama::tr_category("relacao",     "Relação", role = "inspecao"),
      trama::tr_category("distribuicao", "Distribuição", role = "inspecao"),
      trama::tr_category("comparacao",  "Comparação", role = "inspecao"),
      # Por último, e com papel de saída: montar e gravar a figura é o passo
      # depois de escolher o gráfico, e a paleta lê na ordem do trabalho.
      trama::tr_category("figura",      "Figura", role = "saida")
    ),
    # Os nós que vieram depois moram por categoria em `R/catalogo.R`, e entram
    # logo depois dos irmãos da mesma categoria: é a ordem da paleta.
    nodes = c(list(
      trama::tr_node("view/points", fn = tr_points, label = "Disperso",
        category = "relacao", description = "Desenha uma marca por linha, relacionando duas medidas.",
        icon = trama::tr_icon("chart-scatter"),
        inputs = list(dados = T), outputs = list(out = G),
        params = .tr_view_props(
          x   = P("cols", "", label = "Eixo X", example = "valor"),
          y   = P("cols", "", label = "Eixo Y", example = "qtd"),
          cor = P("cols", "", label = "Cor por", example = "regiao"),
          tendencia = trama::tr_param_enum("nenhuma", .TR_VIEW_TENDENCIAS, label = "Tendência"),
          log = trama::tr_param_enum("nenhum", .TR_VIEW_LOG, label = "Eixo em log"),
          painel = PAINEL),
        help = paste0("## Descrição

Desenha uma marca por LINHA da tabela, na posição dada por duas colunas. É o
gráfico de relação: serve para responder se duas medidas andam juntas, onde
está a nuvem principal e quais pontos fogem dela.

Uma linha da tabela é uma marca, e nada é agregado no caminho. Com poucas
centenas de linhas isso é exatamente o que se quer ver; com dezenas de
milhares, as marcas se empilham e o miolo da nuvem vira uma mancha sólida onde
não se distingue mais densidade nenhuma. Quando for esse o caso, agregue ANTES
— um **Agrupar e resumir** por alguma chave, e o disperso passa a mostrar uma
marca por grupo, que é o desenho legível da mesma pergunta.

**Eixo X** e **Eixo Y** são obrigatórios e param o nó em branco. Não há
desenho degradado para um eixo ausente: um disperso sem coluna no X não é um
disperso pobre, é desenho nenhum. **Cor por** é diferente — em branco é o
mapeamento de cor DESLIGADO, e sai a nuvem inteira de uma cor só, o que é um
gráfico legítimo e o caso mais comum.

Coluna citada que não existe na tabela de entrada para o nó e lista as colunas
disponíveis. Errar `valro` por `valor` não desenha um eixo vazio em silêncio.

**Cor por** aceita tanto coluna de texto quanto de número, e a legenda muda de
acordo: texto (ou fator) dá cores discretas, uma por categoria; número dá uma
escala contínua, em degradê. Se a coluna é um código numérico que na verdade
nomeia grupos — `1`, `2`, `3` para três máquinas — o degradê engana, e o certo
é converter para texto ou fator num **Converter tipo** antes.

### Tendência: a linha que responde, e o que ela não diz

`linear` ajusta uma reta de mínimos quadrados (`lm`); `suave` ajusta uma curva
local (`loess`), que segue a nuvem sem pressupor forma. As duas vêm com a faixa
de 95% da LINHA — a incerteza de onde a média de Y passa em cada X, e não a
faixa onde caem as observações, que é bem mais larga. Com **Cor por**
preenchido, sai uma linha por grupo.

A reta aparece mesmo quando a relação é curva, e aí resume mal: se os pontos
fazem um arco em volta da reta, olhe o `suave`. E o `suave` nas pontas da
nuvem, onde há poucos pontos, se abre e se dobra atrás de dois ou três
valores — é a faixa larga ali que avisa. Nenhuma das duas é teste: para o
coeficiente e o valor-p, a regressão e o teste de correlação da coleção de
modelos.

## Parâmetros

- **Eixo X** — coluna da medida horizontal. Obrigatório: em branco, o nó para e
  pede o preenchimento.
- **Eixo Y** — coluna da medida vertical. Obrigatório, pelo mesmo motivo.
- **Cor por** — coluna que colore as marcas. Vazio desliga a cor, e o gráfico
  sai monocromático. Nome inexistente para o nó e lista as colunas disponíveis.
- **Tendência** — `nenhuma` (padrão), `linear` ou `suave`, com faixa de 95%.
  Com o eixo em log, a linha é ajustada nos valores transformados.
- **Eixo em log** — `nenhum` (padrão), `X`, `Y` ou `ambos`. Para medidas que
  variam em ordens de grandeza, ou crescem em proporção. Coluna com zero ou
  negativo para o nó e diz quantos valores impedem: o log deles não existe.
- **Painéis por** — coluna que divide o gráfico em painéis, um por valor, na
  mesma escala. Vazio desliga. Serve quando os grupos se escondem uns atrás
  dos outros na mesma nuvem.

## Valor

Um gráfico. No fluxo, o card mostra a imagem, e o clique a abre em tela cheia.
No console, `tr_points(df, \"x\", \"y\")` devolve um objeto ggplot de verdade,
somável — `tr_points(df, \"x\", \"y\") + ggplot2::scale_x_log10()` funciona, e é
por aí que se chega ao que a coleção não expõe como param.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"nuvem\", \"view/points\", x = \"qtd\", y = \"valor\", cor = \"regiao\",
         titulo = \"Valor por quantidade\", from = \"ler\")
```

Com muitas linhas, agregue antes:

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"por_cliente\", \"data/group_summarise\", by = \"cliente\",
         name = \"receita\", expr = \"sum(valor)\", from = \"ler\") |>
  tr_add(\"nuvem\", \"view/points\", x = \"cliente\", y = \"receita\",
         from = \"por_cliente\")
```

## Veja também

`view/line` para a mesma relação quando o X tem ordem;
`view/bin2d` quando as linhas são tantas que a nuvem vira mancha;
`data/group_summarise` para agregar antes de plotar, quando as marcas se
sobrepõem; `data/filter` para olhar uma fatia da nuvem de perto;
`data/convert` quando a coluna de **Cor por** é um código numérico que devia
ser categoria.", .TR_VIEW_AJUDA_APARENCIA)),

      trama::tr_node("view/line", fn = tr_line, label = "Linha",
        category = "relacao", description = "Liga os pontos na ordem do eixo X.",
        icon = trama::tr_icon("chart-line"),
        inputs = list(dados = T), outputs = list(out = G),
        params = .tr_view_props(
          x   = P("cols", "", label = "Eixo X", example = "mes"),
          y   = P("cols", "", label = "Eixo Y", example = "receita"),
          cor = P("cols", "", label = "Uma linha por", example = "regiao"),
          marcar = trama::tr_param_bool(FALSE, label = "Marcar pontos"),
          log = trama::tr_param_enum("nenhum", .TR_VIEW_LOG, label = "Eixo em log"),
          painel = PAINEL),
        help = paste0("## Descrição

Liga os pontos por segmentos, percorrendo o eixo X. É o disperso com uma
afirmação a mais: que existe um CAMINHO de um ponto ao seguinte.

Por isso o gráfico só faz sentido quando o X tem ORDEM — tempo, dose, dia,
tamanho, posição. Ligar pontos sobre um X sem ordem (nome de cliente, código de
produto) desenha um caminho que não existe no mundo, e a inclinação de cada
segmento sugere uma variação que é só o efeito da ordem alfabética.

### Uma linha por: o erro silencioso deste gráfico

Vale um parágrafo próprio, porque é o modo de falha que sai bonito e errado.
Quando a tabela tem VÁRIAS séries empilhadas — a receita mensal de três
regiões, digamos, com uma linha por região por mês — o campo **Uma linha por**
é o que separa as séries. Preenchido com `regiao`, saem três linhas, cada uma
percorrendo os seus doze meses. Em branco, o nó não erra e não avisa: desenha
UMA linha só, que vai ao mês 1 da região A, salta para o mês 1 da região B,
volta, e sai ziguezagueando entre as séries. O desenho é plausível à distância
e não quer dizer nada. Se o gráfico saiu com dentes verticais que você não
esperava, é quase sempre isto.

Com uma série só na tabela, o campo em branco é o certo: sai uma linha, e é a
que se queria.

A ORDEM dos pontos é a ordem em que o ggplot enxerga o eixo X — crescente para
número e data, alfabética para texto e a ordem dos NÍVEIS para fator. Um mês
gravado como texto (`\"jan\"`, `\"fev\"`, `\"mar\"`) sai em ordem alfabética, e a
linha percorre abr–ago–dez–fev…: converta a coluna para data ou para fator com
os níveis na ordem certa, num **Converter tipo**, antes de plotar. Ordenar a
tabela num **Ordenar** deixa a leitura mais fácil, mas não muda o desenho — a
linha segue o eixo, não a ordem das linhas da tabela.

**Eixo X** e **Eixo Y** são obrigatórios e param o nó em branco: não há linha
degradada com um eixo ausente. Coluna citada que não existe para o nó e lista
as colunas disponíveis.

Faltante no meio de uma série ABRE a linha — o segmento que atravessaria o
buraco não é desenhado, e o corte é a leitura honesta de \"não há dado aqui\".
Para fechá-la, trate os faltantes antes, num **Remover faltantes** ou num
**Preencher faltantes**, decidindo explicitamente o que o buraco significa.

## Parâmetros

- **Eixo X** — coluna que ordena o percurso. Obrigatório: em branco, o nó para
  e pede o preenchimento. Deve ser uma coluna com ordem que signifique algo.
- **Eixo Y** — coluna da medida vertical. Obrigatório, pelo mesmo motivo.
- **Uma linha por** — coluna que separa as séries, e que também as colore.
  Vazio significa UMA série só na tabela; com várias séries misturadas, o vazio
  produz a linha em ziguezague descrita acima. Nome inexistente para o nó e
  lista as colunas disponíveis.
- **Marcar pontos** — desenha a observação em cada vértice. Ligado, fica
  visível onde há dado e onde a linha só atravessa: dois pontos distantes
  ligados por um segmento longo são um salto, não uma tendência medida.
- **Eixo em log** — `nenhum` (padrão), `X`, `Y` ou `ambos`. Para medidas que
  variam em ordens de grandeza, ou crescem em proporção. Coluna com zero ou
  negativo para o nó e diz quantos valores impedem: o log deles não existe.
  Em log no Y, crescimento a taxa constante vira reta. X de data não tem log.
- **Painéis por** — coluna que divide o gráfico em painéis, na mesma escala.
  Com muitas séries, uma por painel lê melhor que um novelo de linhas.

## Valor

Um gráfico. No card, a imagem na proporção escolhida; no console, um objeto
ggplot comum, somável — `tr_line(df, \"mes\", \"receita\") + ggplot2::scale_y_log10()`
põe o eixo em escala log, que é o que se quer quando a série cresce em
proporção.

## Exemplos

Uma série por região, com o mês já como data:

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"mensal\", \"data/group_summarise\", by = \"mes, regiao\",
         name = \"receita\", expr = \"sum(valor)\", from = \"ler\") |>
  tr_add(\"serie\", \"view/line\", x = \"mes\", y = \"receita\", cor = \"regiao\",
         titulo = \"Receita mensal por região\", from = \"mensal\")
```

## Veja também

`view/points` para a relação sem o caminho, quando o X não tem ordem;
`view/area` quando as séries são partes de um total;
`data/group_summarise` para reduzir a tabela a um ponto por período antes de
ligar; `data/arrange` para deixar a tabela na ordem do X, o que ajuda a
conferir a série na prévia; `data/convert` quando o X é uma data ou um mês
guardado como texto.", .TR_VIEW_AJUDA_APARENCIA))),
    .tr_view_nos_relacao(P, PAINEL, G),
    list(
      trama::tr_node("view/histogram", fn = tr_histogram, label = "Histograma",
        category = "distribuicao",
        description = "Mostra a forma da distribuição de uma medida.",
        icon = trama::tr_icon("chart-column"),
        inputs = list(dados = T), outputs = list(out = G),
        params = .tr_view_props(
          x       = P("cols", "", label = "Medida", example = "valor"),
          cor     = P("cols", "", label = "Separar por", example = "regiao"),
          classes = trama::tr_param_int(30L, min = 2L, max = 200L, label = "Classes"),
          posicao = trama::tr_param_enum("empilhar", .TR_VIEW_POSICOES_HIST, label = "Posição"),
          log     = trama::tr_param_bool(FALSE, label = "Eixo em log"),
          painel  = PAINEL),
        help = paste0("## Descrição

Corta o intervalo de UMA medida em faixas de largura igual e desenha, para cada
faixa, quantas linhas caíram dentro dela. É o gráfico da FORMA: onde está o
grosso dos dados, se há uma corcova ou duas, se a cauda puxa para um lado, se
existem valores muito longe do resto.

Ao contrário do disperso, aqui a tabela é agregada pelo próprio gráfico. Não
agregue antes: um **Agrupar e resumir** entregaria uma linha por grupo, e o
histograma passaria a mostrar a distribuição dos RESUMOS, não a dos dados.

**Medida** é obrigatória e para o nó em branco. Precisa ser NUMÉRICA — não há
histograma de texto, e uma coluna que veio do CSV como texto (`\"1.234,50\"`,
ou um número com espaço sobrando) falha ao desenhar, reclamando de variável
contínua. O conserto é um **Converter tipo** para número antes do gráfico, não
um param aqui.

### Classes: o param que muda a conclusão

Merece um parágrafo próprio, porque é o único param desta coleção que altera o
que o gráfico DIZ, e não como ele parece. A mesma coluna, com 8 classes, sai
como uma corcova única e simétrica; com 40, mostra duas corcovas separadas por
um vale — e as duas imagens são igualmente verdadeiras sobre os mesmos números.
Poucas classes ALISAM: escondem bimodalidade, degraus, o pico duplo de duas
máquinas reguladas diferente. Muitas classes viram ruído: cada faixa pega tão
poucas linhas que a silhueta fica serrilhada e o olho lê estrutura onde só há
sorteio.

O padrão 30 é um chute do ggplot2, herdado tal e qual: é um número redondo
escolhido sem olhar para os seus dados. Trate-o como ponto de partida. (O aviso
que o ggplot2 dá sobre isso no console você NÃO vai ver aqui: o nó sempre passa
**Classes** explicitamente, então o aviso nunca dispara — mais um motivo para o
número não passar despercebido.) O jeito de trabalhar é mexer e olhar —
suba para 60, desça para 12, e veja o que sobrevive às três versões. O que
aparece em todas é estrutura; o que só aparece num valor de **Classes** é
provavelmente artefato do corte.

### Separar por empilha, e isso engana

Com **Separar por** preenchido, as barras de cada grupo ficam EMPILHADAS umas
sobre as outras, que é o padrão do ggplot2. A silhueta que se vê no topo é a da
soma dos grupos, não a de nenhum deles: só a faixa de baixo começa no zero, e
as outras flutuam sobre o que veio antes. Quem lê como se fossem distribuições
independentes lado a lado tira a conclusão errada sobre a forma de cada uma.

Para COMPARAR formas entre grupos há três saídas, e todas estão aqui ou ao lado:
**Posição** `sobrepor` põe cada grupo a partir do zero, com transparência (bom
para dois ou três grupos; com mais, as cores se misturam); **Painéis por** com
a mesma coluna dá um histograma por painel, na mesma escala (o mais limpo); e
o **Boxplot**, o **Violino** e a **Densidade** comparam formas sem classes.
Empilhar serve quando a pergunta é a COMPOSIÇÃO de uma distribuição só —
quanto de cada região há em cada faixa de valor.

Coluna citada que não existe na tabela de entrada para o nó e lista as colunas
disponíveis.

## Parâmetros

- **Medida** — coluna numérica cuja distribuição se quer ver. Obrigatória: em
  branco, o nó para e pede o preenchimento. Coluna de texto falha no desenho;
  converta antes. Nome inexistente para o nó e lista as colunas disponíveis.
- **Separar por** — coluna que colore e EMPILHA as barras. Vazio desliga o
  mapeamento, e sai o histograma da amostra inteira numa cor só, que é o caso
  mais comum.
- **Classes** — quantas faixas cortar o intervalo, de 2 a 200. Padrão 30, que é
  um chute e não uma escolha sobre os seus dados. Veja o parágrafo acima.
- **Posição** — `empilhar` (padrão) ou `sobrepor`. Só muda algo com **Separar
  por** preenchido.
- **Eixo em log** — põe a medida em escala log. Coluna com zero ou negativo
  para o nó e diz quantos valores impedem: o log deles não existe.
  As classes passam a ter largura igual NA ESCALA LOG: cada uma cobre a mesma
  razão (de 10 a 20, de 100 a 200), que é o que abre uma cauda longa.
- **Painéis por** — coluna que divide o gráfico em painéis, na mesma escala.

## Valor

Um gráfico. No card, a imagem na proporção escolhida; no console,
`tr_histogram(df, \"valor\", classes = 50)` devolve um objeto ggplot comum,
somável — `+ ggplot2::scale_x_log10()` abre uma cauda longa à direita, que
de outro modo espreme todo o miolo em duas classes.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"forma\", \"view/histogram\", x = \"valor\", classes = 40L,
         titulo = \"Distribuição do valor por pedido\", from = \"ler\")
```

Medida que veio como texto do CSV, convertida antes. Repare no `tr_set()`: o
param do `data/convert` se chama `type`, que também é argumento do próprio
`tr_add()` — passá-lo direto faz o valor ir para o argumento da DSL, e o fluxo
falha com \"Params de tr_add() precisam ser nomeados\", longe da causa. Param
com nome de argumento de `tr_add()` se preenche depois, e este é o caso.

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"num\", \"data/convert\", cols = \"valor\", from = \"ler\") |>
  tr_set(\"num\", type = \"numero\") |>
  tr_add(\"forma\", \"view/histogram\", x = \"valor\", from = \"num\")
```

## Veja também

`view/boxplot` para comparar a distribuição ENTRE grupos, que é o que o
empilhamento daqui não faz; `view/density` para a mesma forma sem o corte em
classes; `view/ecdf` para comparar grupos sem escolher param nenhum; `view/points` para a relação entre duas medidas;
`data/convert` quando a medida veio como texto; `data/filter` para cortar a
cauda e olhar o miolo de perto.", .TR_VIEW_AJUDA_APARENCIA)),

      trama::tr_node("view/boxplot", fn = tr_boxplot, label = "Boxplot",
        category = "distribuicao",
        description = "Compara a distribuição de uma medida entre grupos.",
        icon = trama::tr_icon("chart-candlestick"),
        inputs = list(dados = T), outputs = list(out = G),
        params = .tr_view_props(
          y   = P("cols", "", label = "Medida", example = "valor"),
          x   = P("cols", "", label = "Grupo", example = "regiao"),
          cor = P("cols", "", label = "Preencher por", example = "produto"),
          pontos = trama::tr_param_bool(FALSE, label = "Mostrar observações"),
          log = trama::tr_param_bool(FALSE, label = "Eixo em log"),
          painel = PAINEL),
        help = paste0("## Descrição

Resume a distribuição de uma medida em cinco números e desenha um por grupo,
todos na mesma escala. É o gráfico de comparar: qual grupo tem o nível mais
alto, qual espalha mais, onde estão os pontos que fogem.

**Medida** vem antes de **Grupo** nos campos porque é a ordem em que se pensa:
primeiro que número se está olhando, depois por que categoria separá-lo.

### O que a caixa desenha

O card mostra o desenho e não a definição, então ela fica aqui:

- a LINHA dentro da caixa é a mediana — metade das linhas do grupo está acima,
  metade abaixo;
- a CAIXA vai do primeiro ao terceiro quartil, e contém o miolo de 50% do
  grupo; a altura dela é o intervalo interquartil (IQR);
- os BIGODES esticam até o valor mais extremo que ainda esteja a até 1,5·IQR da
  borda da caixa — não até o mínimo e o máximo;
- os PONTOS soltos além dos bigodes são o que ficou fora desse alcance. São
  observações, não erros: a regra de 1,5·IQR é uma convenção de desenho, e num
  grupo grande sempre saem alguns. Não é um teste de outlier.

### Grupo em branco: uma caixa só

Com **Grupo** vazio, o mapeamento é DESLIGADO e sai uma caixa única, da amostra
inteira, com o eixo horizontal sem rótulo. É legítimo e é a pergunta de quem
ainda não tem por que separar: onde está a mediana, quanto espalha, quem foge.

### O que o boxplot esconde

Por construção, cinco números não guardam a forma. Duas distribuições muito
diferentes podem dar caixas IDÊNTICAS: uma corcova única no centro e duas
corcovas separadas, uma de cada lado, têm a mesma mediana, os mesmos quartis e
os mesmos bigodes. O boxplot não mostra multimodalidade, e não avisa que está
escondendo uma. É aí que o **Histograma** entra — quando duas caixas parecem
iguais, ou quando uma delas parece boa demais, olhe a forma antes de concluir.

**Mostrar observações** é o antídoto barato: espalha cada linha da tabela na
horizontal por cima da caixa (a altura é o valor, e não se mexe), e três coisas
que a caixa esconde aparecem — quantas observações há em cada grupo, onde elas
se amontoam, e se a caixa de um grupo foi calculada sobre quatro pontos. Os
pontos soltos da caixa somem nesse modo, para nenhum extremo sair duas vezes.
Com milhares de linhas por grupo, vira borrão: aí o **Violino** diz o mesmo.

**Medida** é obrigatória e para o nó em branco; precisa ser numérica. Coluna
citada que não existe na tabela de entrada para o nó e lista as colunas
disponíveis.

**Grupo** com muitos valores distintos — um código de cliente, digamos —
desenha uma caixa por valor e o eixo vira uma parede de rótulos ilegíveis, cada
caixa com poucas linhas dentro. Boxplot compara PUNHADOS de grupos; para
dezenas, filtre antes num **Filtrar** ou agrupe a categoria.

## Parâmetros

- **Medida** — coluna numérica cuja distribuição se resume. Obrigatória: em
  branco, o nó para e pede o preenchimento.
- **Grupo** — coluna categórica que separa as caixas. Vazio desenha UMA caixa
  só, da amostra inteira. Nome inexistente para o nó e lista as colunas
  disponíveis.
- **Preencher por** — coluna que subdivide cada grupo, preenchendo as caixas
  com cores. Com ela, cada posição do eixo ganha várias caixas lado a lado.
  Vazio desliga o preenchimento.
- **Mostrar observações** — desenha cada linha sobre as caixas. Desligado por
  padrão.
- **Eixo em log** — põe a medida em escala log. Coluna com zero ou negativo
  para o nó e diz quantos valores impedem: o log deles não existe.
  Os quartis continuam os dos dados; só a régua muda.
- **Painéis por** — coluna que divide o gráfico em painéis, na mesma escala.

## Valor

Um gráfico. No card, a imagem na proporção escolhida; no console,
`tr_boxplot(df, \"valor\", \"regiao\")` devolve um objeto ggplot comum, somável —
`+ ggplot2::coord_flip()` deita as caixas, que salva rótulo de grupo comprido.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"caixas\", \"view/boxplot\", y = \"valor\", x = \"regiao\",
         titulo = \"Valor do pedido por região\", from = \"ler\")
```

Uma caixa só, da amostra inteira:

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"geral\", \"view/boxplot\", y = \"valor\", from = \"ler\")
```

## Veja também

`view/histogram` para a forma que a caixa esconde, e sempre que duas caixas
saírem parecidas demais; `view/violin` para a forma e a caixa no mesmo desenho;
`view/means` quando a pergunta é a média e a incerteza dela, e não a
distribuição; `view/bars` para comparar UM número por categoria, em
vez da distribuição inteira; `data/filter` para reduzir o número de grupos
antes de plotar; `data/convert` quando a medida veio como texto ou o grupo é um
código numérico que devia ser categoria.", .TR_VIEW_AJUDA_APARENCIA))),
    .tr_view_nos_distribuicao(P, PAINEL, G),
    list(
      trama::tr_node("view/bars", fn = tr_bars, label = "Barras",
        category = "comparacao",
        description = "Compara um valor entre categorias.",
        icon = trama::tr_icon("chart-bar"),
        inputs = list(dados = T), outputs = list(out = G),
        params = .tr_view_props(
          x   = P("cols", "", label = "Categoria", example = "regiao"),
          y   = P("cols", "", label = "Altura", example = "receita_total"),
          cor = P("cols", "", label = "Subgrupo", example = "produto"),
          posicao = trama::tr_param_enum("empilhar", .TR_VIEW_POSICOES_BARRAS, label = "Posição"),
          ordenar = trama::tr_param_bool(FALSE, label = "Ordenar pela altura"),
          deitar  = trama::tr_param_bool(FALSE, label = "Deitar"),
          rotulos = trama::tr_param_bool(FALSE, label = "Escrever valores"),
          painel  = PAINEL),
        help = paste0("## Descrição

Uma barra por categoria, comparadas pela altura. É o gráfico de responder
\"qual é maior\", e o comprimento a partir do zero é o que o olho compara bem —
melhor do que ângulo, área ou cor.

**Categoria** é obrigatória e para o nó em branco. **Altura** não: o vazio dela
tem sentido próprio, e é o parágrafo seguinte.

### Altura em branco conta linhas

Sem **Altura**, cada barra é a CONTAGEM de linhas daquela categoria — quantos
pedidos por região, quantas peças por máquina. É a pergunta natural de quem
acabou de ler o CSV e só tem a categoria em mãos, e não falhar aqui é
deliberado: exigir um **Agrupar e resumir** só para descobrir quantos são seria
cobrar um nó por uma pergunta de um passo. O eixo vertical sai rotulado
`contagem`, para que o gráfico diga o que está mostrando.

### Altura preenchida SOMA

Com **Altura** preenchida, a barra é o valor daquela coluna — e, quando a
categoria aparece em MAIS DE UMA linha, é a SOMA dos valores dessas linhas.
Isso surpreende quem esperava média: com quatro pedidos da região Sul na
tabela, a barra do Sul é a soma dos quatro valores, e nunca o valor típico de
um pedido do Sul.

Quando a soma é o que se quer, deixar o gráfico somar funciona. Quando não é —
média, mediana, contagem de distintos, qualquer outra coisa — o caminho certo é
agregar ANTES, num **Agrupar e resumir**, e apontar **Altura** para a coluna
que ele criou. Aí o número da barra é o número que se pediu, aparece na prévia
da tabela e pode ser conferido, em vez de nascer escondido dentro do desenho. O
segundo exemplo abaixo mostra isso.

Se cada categoria já aparece uma única vez na tabela — o caso de quem acabou de
agregar — a soma é a identidade, e a distinção não importa.

### Subgrupo e Posição

Com **Subgrupo**, cada categoria é dividida pelos valores dessa coluna, e
**Posição** decide a pergunta que o desenho responde:

- `empilhar` (padrão) — as fatias uma sobre a outra; a altura total continua a
  da categoria. Responde COMPOSIÇÃO. Para comparar subgrupos entre si
  atrapalha: só a fatia de baixo começa no zero, e as outras flutuam.
- `lado a lado` — uma barra por subgrupo, todas a partir do zero. Responde
  COMPARAÇÃO entre subgrupos. Também aqui a barra é a SOMA das linhas do par
  categoria × subgrupo, a mesma regra das outras posições.
- `proporção` — as fatias empilhadas e esticadas até 100%. Responde \"que
  PARTE de cada categoria é de cada subgrupo\", e apaga de propósito o tamanho
  da categoria: uma com dez linhas e outra com dez mil saem da mesma altura.

### Ordenar e deitar

**Ordenar pela altura** põe as categorias da maior para a menor (pela soma ou
contagem total, a mesma que a barra desenha), que é o que transforma a
comparação num ranking lido de um golpe. Não ligue quando a categoria tem uma
ordem própria — faixas de idade, meses, notas —: aí a ordem do eixo é a
informação, e reordenar pela altura a destrói.

**Deitar** gira o gráfico: as categorias vão para o eixo vertical, e os
rótulos compridos passam a caber sem inclinar. Deitado e ordenado, a maior
fica em cima.

### Escrever valores

Põe o número de cada barra no gráfico, onde ele se lê: no meio da fatia quando
empilhada, logo além da ponta quando a barra é inteira (sem subgrupo, ou lado a
lado), e em porcentagem da categoria na `proporção` — que é o que o eixo mostra
ali. A precisão acompanha a ordem de grandeza: contagem sai inteira, e somas
abaixo de 100 ganham uma ou duas casas. Com dezenas de barras finas os números
se atropelam; deitar e ordenar costuma resolver.

Coluna citada que não existe na tabela de entrada para o nó e lista as colunas
disponíveis. **Categoria** com muitos valores distintos vira uma parede de
barras finas com rótulos ilegíveis: filtre ou agrupe antes.

## Parâmetros

- **Categoria** — coluna que define as barras. Obrigatória: em branco, o nó
  para e pede o preenchimento. Nome inexistente para o nó e lista as colunas
  disponíveis.
- **Altura** — coluna numérica que dá a altura. VAZIO conta linhas; preenchida,
  soma os valores da categoria quando há mais de uma linha por categoria.
- **Subgrupo** — coluna que divide cada categoria em subgrupos, pintados por
  cor. Vazio desliga, e as barras saem de uma cor só.
- **Posição** — `empilhar` (padrão), `lado a lado` ou `proporção`. Só muda
  algo com **Subgrupo** preenchido.
- **Ordenar pela altura** — da maior para a menor. Desligado por padrão.
- **Deitar** — barras horizontais. Desligado por padrão.
- **Escrever valores** — o número de cada barra (ou fatia). Desligado por
  padrão.
- **Painéis por** — coluna que divide o gráfico em painéis, na mesma escala.

## Valor

Um gráfico. No card, a imagem na proporção escolhida; no console,
`tr_bars(df, \"regiao\")` devolve um objeto ggplot comum, somável —
`+ ggplot2::scale_fill_manual(values = c(sul = \"grey60\", norte = \"tomato\"))`
destaca um subgrupo e apaga o resto.

## Exemplos

Contagem, com **Altura** em branco:

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"quantos\", \"view/bars\", x = \"regiao\",
         titulo = \"Pedidos por região\", from = \"ler\")
```

Média por região — agregada antes, porque a barra somaria:

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"media\", \"data/group_summarise\", by = \"regiao\",
         name = \"valor_medio\", expr = \"mean(valor)\", from = \"ler\") |>
  tr_add(\"barras\", \"view/bars\", x = \"regiao\", y = \"valor_medio\",
         titulo = \"Valor médio do pedido por região\", from = \"media\")
```

## Veja também

`data/group_summarise` sempre que a altura não for soma nem contagem;
`view/means` quando a barra seria uma média e a incerteza dela importa;
`view/heatmap` para duas categorias ao mesmo tempo;
`view/boxplot` para a distribuição inteira de cada categoria, em vez de um
número só; `view/line` quando a categoria do eixo tem ordem e o interesse é a
evolução; `data/filter` para reduzir o número de barras;
`data/convert` quando a altura veio como texto.", .TR_VIEW_AJUDA_APARENCIA))),
    .tr_view_nos_comparacao(P, PAINEL, G),
    .tr_view_nos_figura(P, G), list(.tr_view_no_salvar(P, G))),
    # Glossário de params: a barra das médias tinha o nível no nome ("IC 95%");
    # agora é "IC" + `confianca`. `when` só pega o formato velho, então as
    # outras barras e um fluxo já migrado ficam intactos.
    migrations = list(params = list(
      "view/means" = list(barra = list(
        to = "barra", when = function(v) is.character(v) && grepl("^IC [0-9]+([.,][0-9]+)?%$", v),
        value = function(v) list(barra = "IC",
                                 confianca = as.numeric(sub(",", ".", sub("^IC ([0-9.,]+)%$", "\\1", v))) / 100)))
    ))
  ))
}
