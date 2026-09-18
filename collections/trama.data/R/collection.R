#' A coleção `data`.
#'
#' Registra tipos, categorias e nós. `js` aponta pro módulo ES que o front
#' carrega depois do runtime — é como uma coleção traz widget próprio sem o
#' núcleo saber o que é uma coluna.
#'
#' Todo leitor de arquivo é IMPURO e declara o mesmo `fingerprint`
#' (`.tr_data_file_print`): o hash de conteúdo do trama cobre código e params,
#' mas não o mundo. Sem isso, editar o arquivo no disco não mudaria a chave, e
#' o grafo inteiro serviria dado velho — em silêncio, que é o pior modo de
#' falha possível numa ferramenta de análise.
#' @export
trama_collection <- function() {
  T <- "data/table"
  P <- trama::tr_param
  trama::tr_collection(
    id = "data", version = "0.1.0", label = "Dados", js = "trama/index.js",
    types = list(data_table_type()),
    categories = list(
      trama::tr_category("source",    "Fonte",       "#6366f1"),
      # Conhecer vem logo depois de trazer: a ordem daqui é a ordem dos grupos
      # na paleta, e é ela que sugere o próximo passo a quem está montando.
      trama::tr_category("inspect",   "Conhecer",    "#f59e0b"),
      # Limpar vem depois de conhecer e antes de transformar: só depois de ver
      # o resumo é que se sabe qual coluna veio como texto quando devia ser
      # número.
      trama::tr_category("clean",     "Limpar",      "#14b8a6"),
      trama::tr_category("transform", "Transformar", "#0ea5e9"),
      # Reformatar vem depois de transformar e antes de agregar: mudar a FORMA
      # da tabela é o passo que costuma preceder o resumo — empilha primeiro,
      # agrupa depois.
      trama::tr_category("reshape",   "Reformatar",  "#ec4899"),
      trama::tr_category("aggregate", "Agregar",     "#a855f7"),
      # "Fluxo" é grupo próprio, e não um canto de "Transformar": o par
      # `to_stream`/`from_stream` não muda a tabela — muda o MODO DE EXECUÇÃO
      # do pedaço de grafo entre os dois, que passa a rodar ponto a ponto, numa
      # unidade só. A Decisão 5 pede que isso se veja na tela, e um grupo com
      # nome próprio é o mínimo. Vem depois dos verbos e antes da saída porque é
      # essa a ordem do trabalho: primeiro se sabe montar a transformação,
      # depois se decide assisti-la acontecer.
      trama::tr_category("stream",    "Fluxo",       "#84cc16"),
      trama::tr_category("sink",      "Saída",       "#22c55e")
    ),
    nodes = list(
      trama::tr_node("data/read_csv", fn = tr_read_csv, label = "Ler CSV",
        category = "source", description = "Lê um arquivo delimitado do disco.",
        icon = trama::tr_icon("file-spreadsheet"),
        outputs = list(out = T),
        params = list(path  = P("path", "", label = "Arquivo", example = "vendas.csv"),
                      delim = trama::tr_param_enum(",", c(",", ";", "\t", "|"), label = "Separador"),
                      na    = P("text", "NA", label = "Marcas de faltante",
                                example = "NA, -, sem dado")),
        pure = FALSE, fingerprint = .tr_data_file_print,
        help = "## Descrição

Lê um arquivo de texto delimitado e devolve a tabela correspondente. O tipo de
cada coluna é deduzido do conteúdo do arquivo: coluna com número escrito à
brasileira (`1.234,56`) chega como texto, e só vira número num nó
**Converter tipo**.

Caminho relativo é resolvido a partir da pasta do projeto; caminho absoluto
passa direto.

Célula VAZIA conta sempre como faltante, e nenhum ajuste desliga isso: `a,,b`
tem um buraco no meio em qualquer ferramenta de dados, e um texto vazio que se
faz passar por valor presente atravessa o fluxo inteiro em silêncio — o
`data/drop_na` não descarta a linha, o `data/remove_empty` não a vê e o
`data/summary` reporta zero faltantes numa coluna cheia de buracos. **Marcas de
faltante** ACRESCENTA a esse piso os textos que, naquele arquivo, também
significam ausência.

O nó é impuro, e por isso declara uma impressão digital do arquivo — caminho,
tamanho e data de modificação. Editar o CSV no disco muda essa impressão, o
resultado guardado deixa de valer e o fluxo recomputa daqui para frente. É o
que impede a tela mostrar dado velho depois que o arquivo mudou.

## Parâmetros

- **Arquivo** — caminho do arquivo, relativo à pasta do projeto. Campo
  obrigatório: em branco, o nó para e pede o preenchimento.
- **Separador** — caractere entre os campos. Planilha exportada em português
  costuma sair com `;`.
- **Marcas de faltante** — os textos que, neste arquivo, significam valor
  ausente, separados por vírgula: `NA, -, sem dado`. A célula vazia já conta
  como faltante sempre, então o campo em branco não é erro — quer dizer apenas
  \"nenhuma marca além da célula vazia\". Espaço em volta de cada marca é
  aparado.

## Valor

Uma tabela, uma linha por linha do arquivo. Arquivo inexistente falha com o
caminho procurado na mensagem.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\", delim = \";\",
         na = \"NA, -, sem dado\") |>
  tr_add(\"olhar\", \"data/summary\", from = \"ler\")
```

## Veja também

`data/summary` para conferir o que veio; `data/convert` para as colunas que
chegaram como texto; `data/write_csv` para gravar de volta."),

      trama::tr_node("data/read_json", fn = tr_read_json, label = "Ler JSON",
        category = "source", description = "Lê dados tabulares de um arquivo JSON.",
        icon = trama::tr_icon("braces"),
        outputs = list(out = T),
        params = list(path = P("path", "", label = "Arquivo", example = "vendas.json")),
        pure = FALSE, fingerprint = .tr_data_file_print,
        help = "## Descrição

Lê um arquivo JSON tabular e devolve uma tabela. O formato mais comum é um
array de objetos, em que cada objeto representa uma linha e cada chave vira
uma coluna. Um objeto de vetores com o mesmo comprimento também é aceito.

Valores `null` viram valores faltantes. Objetos ou arrays aninhados podem virar
colunas compostas, conforme a simplificação feita pelo pacote `jsonlite`.

Caminho relativo é resolvido a partir da pasta do projeto; caminho absoluto
passa direto. Como todo leitor, o nó é impuro e declara uma impressão digital
do arquivo — caminho, tamanho e data de modificação —, então editar o JSON faz
o fluxo recomputar.

## Parâmetros

- **Arquivo** — caminho do `.json`, relativo à pasta do projeto. Campo
  obrigatório.

## Valor

Uma tabela no formato `tibble`. JSON que não representa dados tabulares falha
em vez de produzir um card verde com um objeto incompatível.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_json\", path = \"vendas.json\") |>
  tr_add(\"olhar\", \"data/summary\", from = \"ler\")
```

## Veja também

`data/read_csv` para arquivos delimitados; `data/summary` para conferir a
estrutura que foi lida."),

      trama::tr_node("data/read_rds", fn = tr_read_rds, label = "Ler RDS",
        category = "source", description = "Lê um objeto R gravado em .rds.",
        icon = trama::tr_icon("file-box"),
        outputs = list(out = T),
        params = list(path = P("path", "", label = "Arquivo", example = "vendas.rds")),
        pure = FALSE, fingerprint = .tr_data_file_print,
        help = "## Descrição

Lê um objeto gravado no formato `.rds`, o formato binário do próprio R. Ao
contrário do CSV, nada é deduzido: fator continua fator, data continua data, e
as colunas voltam exatamente como estavam quando foram gravadas.

O objeto guardado precisa ser mesmo uma tabela. Um `.rds` com um modelo, uma
lista ou um vetor faz o nó falhar no fluxo, dizendo o que veio no lugar — antes
valia qualquer objeto, e o resultado era um card verde com uma tabela plausível
de zero colunas.

Quem recusa é a porta de SAÍDA, não a leitura: o guard mora no tipo
`data/table`, por onde passa todo valor que qualquer nó entrega ao fluxo. É por
isso que ele vale para a coleção inteira sem cada nó precisar lembrar — e
também por que chamar `tr_read_rds()` direto no console devolve o objeto como
ele está, seja lá o que for. No console, quem lê é quem confere; no fluxo, o
tipo confere por você.

Arquivo inexistente é conferido aqui, e não pelo R: cru, a leitura de `.rds`
falha com \"não é possível abrir a conexão\", sem dizer qual caminho. Esta é
justamente a falha de quem renomeou um arquivo ou moveu o projeto.

Como todo leitor, o nó é impuro e declara a impressão digital do arquivo —
caminho, tamanho e data de modificação —, de modo que regravar o `.rds` faz o
fluxo recomputar.

## Parâmetros

- **Arquivo** — caminho do `.rds`, relativo à pasta do projeto. Campo
  obrigatório.

## Valor

A tabela como foi gravada, com os mesmos tipos.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_rds\", path = \"saida/vendas.rds\") |>
  tr_add(\"olhar\", \"data/summary\", from = \"ler\")
```

## Veja também

`data/write_rds` grava neste mesmo formato; `data/read_csv` para dado que vem
de texto."),

      trama::tr_node("data/read_parquet", fn = tr_read_parquet, label = "Ler Parquet",
        category = "source", description = "Lê uma tabela em formato Parquet.",
        icon = trama::tr_icon("file-digit"),
        outputs = list(out = T),
        params = list(path = P("path", "", label = "Arquivo", example = "vendas.parquet")),
        pure = FALSE, fingerprint = .tr_data_file_print,
        help = "## Descrição

Lê uma tabela em Parquet — formato colunar e comprimido, que guarda o tipo de
cada coluna junto com os dados. Nada é deduzido na leitura, e arquivos grandes
ocupam bem menos disco que o CSV equivalente.

Depende do pacote `arrow`, que é sugerido e não obrigatório: sem ele
instalado, o nó falha logo de saída dizendo o comando de instalação, em vez de
reclamar de função inexistente.

Como todo leitor, é impuro e declara a impressão digital do arquivo — caminho,
tamanho e data de modificação —, então reescrever o arquivo no disco faz o
fluxo recomputar.

## Parâmetros

- **Arquivo** — caminho do `.parquet`, relativo à pasta do projeto. Campo
  obrigatório.

## Valor

Uma tabela, sempre como `tibble`. Arquivo inexistente falha com o caminho na
mensagem.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_parquet\", path = \"vendas.parquet\") |>
  tr_add(\"olhar\", \"data/summary\", from = \"ler\")
```

## Veja também

`data/write_parquet` grava neste mesmo formato; `data/read_csv` para dado que
vem de texto."),

      trama::tr_node("data/read_excel", fn = tr_read_excel, label = "Ler Excel",
        category = "source", description = "Lê uma planilha de um arquivo .xlsx ou .xls.",
        icon = trama::tr_icon("sheet"),
        outputs = list(out = T),
        params = list(path  = P("path", "", label = "Arquivo", example = "vendas.xlsx"),
                      sheet = P("text", "1", label = "Planilha", example = "Plan1")),
        pure = FALSE, fingerprint = .tr_data_file_print,
        help = "## Descrição

Lê uma planilha de um arquivo `.xlsx` ou `.xls` e devolve a tabela.

A planilha é indicada pelo nome ou pela posição, no mesmo campo: `1` é a
primeira planilha do arquivo, e `Plan1` é a planilha com esse nome.

Texto que é um número inteiro vale SEMPRE como posição, e não há como escapar
disso. Uma aba chamada `2026` — que é como se batiza a aba do ano — não é
alcançável pelo nome: `2026` no campo pede a planilha de número 2026, que o
arquivo não tem, e o nó para dizendo que ela não existe. Nesse caso, use a
POSIÇÃO da aba (`1`, `2`, …) ou renomeie a aba para algo que não seja só
dígitos (`ano 2026`).

Depende do pacote `readxl`, que é sugerido e não obrigatório: sem ele
instalado, o nó falha logo de saída dizendo o comando de instalação.

Como todo leitor, é impuro e declara a impressão digital do arquivo — caminho,
tamanho e data de modificação —, então salvar a planilha de novo faz o fluxo
recomputar.

## Parâmetros

- **Arquivo** — caminho do `.xlsx` ou `.xls`, relativo à pasta do projeto.
  Campo obrigatório.
- **Planilha** — nome da planilha ou a sua posição no arquivo. Número inteiro
  é sempre lido como posição, nunca como nome. Também é obrigatório: sem
  planilha não há tabela a produzir.

## Valor

Uma tabela com o conteúdo da planilha. O tipo de cada coluna vem do que o Excel
guardou — célula formatada como texto chega como texto, mesmo parecendo número.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_excel\", path = \"vendas.xlsx\", sheet = \"Base\") |>
  tr_add(\"nomes\", \"data/clean_names\", from = \"ler\") |>
  tr_add(\"olhar\", \"data/summary\", from = \"nomes\")
```

## Veja também

`data/clean_names`, porque cabeçalho de planilha costuma vir com acento e
espaço; `data/convert` para a coluna que veio como texto."),

      trama::tr_node("data/example", fn = tr_example, label = "Dados de exemplo",
        category = "source",
        description = "Carrega um conjunto de dados embutido no R, sem precisar de arquivo.",
        icon = trama::tr_icon("flask-conical"),
        outputs = list(out = T),
        params = list(dataset = trama::tr_param_enum("mtcars", .tr_data_exemplos(),
                        label = "Conjunto")),
        help = "## Descrição

Carrega um dos conjuntos de dados que vêm com o R, sem precisar de arquivo
nenhum no disco. É a fonte para experimentar um fluxo, reproduzir um exemplo ou
começar um projeto em branco antes de ter o dado próprio.

A lista oferecida é todo `data.frame` do pacote `datasets` — quarenta e seis no
R 4.5 — e sai dele na hora, e não de uma lista escrita à mão aqui. O que o
pacote traz e NÃO é tabela (as séries temporais como `AirPassengers`, as
tabelas de contingência como `Titanic`) fica de fora de propósito: uma opção
que não produz tabela só descobriria isso depois de rodar, com o card vermelho.

Ao contrário dos leitores de arquivo, este nó é puro: não toca no mundo, logo o
resultado é sempre o mesmo e fica guardado sem prazo de validade.

Nove dos conjuntos guardam informação fora das colunas, no nome da linha — o
modelo do carro em `mtcars`, o estado em `USArrests`, a província em `swiss`.
Aqui ela vira a primeira coluna, chamada `nome`, para não desaparecer.

## Parâmetros

- **Conjunto** — qual dos conjuntos carregar. `mtcars` (32 carros),
  `iris` (150 flores), `airquality` (qualidade do ar, com faltantes de
  verdade), `ToothGrowth`, `ChickWeight` e `penguins` são os do costume;
  `?datasets::<nome>` no console descreve qualquer um deles.

## Valor

Uma tabela. `airquality` é o exemplo com valores faltantes — útil para provar
os nós de remover e preencher faltantes.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"dados\", \"data/example\", dataset = \"airquality\") |>
  tr_add(\"olhar\", \"data/summary\", from = \"dados\") |>
  tr_add(\"limpo\", \"data/drop_na\", cols = \"Ozone\", from = \"dados\")
```

## Veja também

`data/read_csv` quando o dado é seu e está em arquivo; `data/summary` para o
primeiro olhar em qualquer tabela."),

      trama::tr_node("data/generate", fn = tr_generate, label = "Gerar dados",
        category = "source", stochastic = TRUE,
        description = "Fabrica uma tabela a partir de expressões R, com semente própria.",
        icon = trama::tr_icon("dices"),
        outputs = list(out = T),
        params = list(
          n = trama::tr_param_int(100, min = 1, label = "Tamanho (n)"),
          expr = trama::tr_param("expr", "x = rnorm(n)", label = "Colunas",
                   example = "x = rnorm(n), y = 2 * x + rnorm(n, 0, 0.5)")),
        help = "## Descrição

Fabrica a tabela em vez de carregá-la. É a fonte para demonstrar simulação —
uma amostra de uma distribuição, uma regressão com ruído, um processo gerador
de que se conhece a resposta — sem depender de arquivo no disco nem de um
conjunto embutido que por acaso tenha a forma certa.

É o irmão do `data/example`, que carrega um conjunto que já existe. Aqui você
escreve o que quer.

## Semente

O nó é aleatório e mesmo assim cacheável, porque a semente é parte do card e
não do acaso: ela é gravada no fluxo, viaja com o arquivo e entra na chave de
cache. O mesmo fluxo aberto amanhã, ou na máquina de outra pessoa, produz a
MESMA amostra — e nada recomputa enquanto ela não mudar.

Cada card tem a sua, independente: dois geradores no mesmo fluxo sorteiam
coisas diferentes, e re-sortear um não mexe no outro. No console, é o argumento
`seed` de `tr_add()`:

```r
tr_add(\"amostra\", \"data/generate\", expr = \"x = rnorm(n)\", seed = 42L)
```

## Parâmetros

- **Tamanho (n)** — fica disponível como a variável `n` dentro das expressões,
  para não repetir o tamanho em cada coluna. É uma variável à disposição, não
  uma promessa sobre o número de linhas: quem escrever `1:5` recebe cinco
  linhas, qualquer que seja o `n`.
- **Colunas** — uma ou mais expressões R separadas por vírgula, no formato
  `nome = expressão`. São avaliadas NA ORDEM escrita, então uma coluna pode
  usar a anterior: em `x = rnorm(n), y = 2 * x`, o `y` enxerga o `x`.

  Uma expressão sozinha e sem nome é o caso do vetor: `runif(n, 0, 1)` devolve
  uma tabela de uma coluna chamada `valor`. Uma que devolva matriz vira tabela
  de colunas `V1`, `V2`…; uma que devolva `data.frame` passa como está.

## Valor

Uma tabela. O que a expressão devolver e não for tabela, vetor ou matriz é
recusado pelo tipo `data/table`, com o nome do que veio no lugar.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"sim\", \"data/generate\", n = 500L,
         expr = \"x = rnorm(n), y = 3 + 2 * x + rnorm(n, 0, 0.5)\", seed = 42L) |>
  tr_add(\"olhar\", \"data/summary\", from = \"sim\")
```

Amostragem de grupos, para um fatorial equilibrado:

```r
tr_add(\"ensaio\", \"data/generate\", n = 60L,
       expr = \"trat = rep(c('A','B','C'), each = n / 3), y = rnorm(n, 10, 2)\")
```

## Veja também

`data/example` quando o dado já existe e é só carregar; `data/summary` para o
primeiro olhar; `view/points` para ver a nuvem que você acabou de simular."),

      trama::tr_node("data/summary", fn = tr_summary, label = "Resumo", category = "inspect",
        description = "Uma linha por coluna: tipo, faltantes, distintos, mínimo, máximo e um exemplo.",
        icon = trama::tr_icon("clipboard-list"),
        inputs = list(data = T), outputs = list(out = T),
        help = "## Descrição

Descreve a tabela de entrada: uma linha para cada coluna dela, com `coluna`,
`tipo`, `faltantes`, `distintos`, `minimo`, `maximo` e `exemplo`. É o nó que se
liga logo depois da fonte para saber o que de fato chegou.

O resultado é uma TABELA como qualquer outra do fluxo — ordenável, filtrável, e
com nós ligáveis adiante. Ordenar por `faltantes` acha a coluna furada; ordenar
por `distintos` separa o que é categoria do que é identificador.

`minimo` e `maximo` saem como TEXTO, e não como número: o mínimo de uma coluna
de datas e o de uma coluna numérica moram na mesma coluna do resultado, e uma
só delas não pode ser das duas. Para comparar como número, converta depois.

`faltantes` conta o valor ausente do R — o que inclui a célula vazia de um CSV,
que o `data/read_csv` lê como faltante, e não como texto vazio. Se a contagem
sair zero numa coluna que você sabe furada, o buraco virou algum texto
particular do arquivo (`-`, `n/d`): declare-o em **Marcas de faltante** na
leitura.

Coluna inteiramente vazia sai com `minimo` e `maximo` em branco — não há
extremo a mostrar —, mas `faltantes` diz o tamanho do estrago. Coluna lógica
resume como `FALSE`/`TRUE`, e não como 0/1; fator resume pelo rótulo. `distintos`
não conta o faltante. Texto usa a ordem alfabética do idioma configurado.

Nenhum tipo derruba o resumo: coluna de listas, que é justamente a que alguém
pluga aqui para entender, sai sem mínimo mas com exemplo.

## Valor

Uma tabela com sete colunas e uma linha por coluna da entrada.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"perfil\", \"data/summary\", from = \"ler\") |>
  tr_add(\"furadas\", \"data/arrange\", cols = \"faltantes\", desc = TRUE, from = \"perfil\")
```

## Veja também

`data/get_dupes` para ver as linhas repetidas; `data/convert` para a coluna que
o resumo mostrou como texto e devia ser número."),

      # `get_dupes` MOSTRA as repetidas; `distinct` REMOVE. Fases diferentes do
      # trabalho, e por isso categorias diferentes: ver antes de apagar.
      trama::tr_node("data/get_dupes", fn = tr_get_dupes, label = "Duplicadas",
        category = "inspect",
        description = "Mostra as linhas repetidas nas colunas escolhidas, com a contagem.",
        icon = trama::tr_icon("copy"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(cols = P("cols", "", label = "Colunas", example = "regiao, produto")),
        help = "## Descrição

Mostra apenas as linhas que se repetem nas colunas escolhidas, acrescentando a
coluna `dupe_count` com o tamanho de cada repetição. Linha que aparece uma vez
só não sai no resultado.

Este nó não apaga nada: ele existe para VER a repetição antes de decidir o que
fazer com ela. Duplicata costuma ser sintoma — exportação feita duas vezes,
junção que multiplicou linhas, chave que não era chave. Olhar antes evita
apagar dado bom.

Com **Colunas** em branco, compara a linha inteira, que é o caso de quem acabou
de empilhar duas tabelas. Com colunas preenchidas, procura repetição só nelas —
é assim que se descobre que o mesmo código de cliente aparece com dois nomes.

## Parâmetros

- **Colunas** — colunas que definem a repetição, separadas por vírgula. Vazio
  compara a linha inteira. Nome de coluna inexistente para o nó e lista as
  colunas disponíveis, em vez de ser ignorado em silêncio.

## Valor

As linhas repetidas, com as repetidas juntas e uma coluna `dupe_count` a mais.
Sem repetição nenhuma, a tabela sai vazia — o que já é a resposta.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"repetidas\", \"data/get_dupes\", cols = \"regiao, produto\", from = \"ler\")
```

## Veja também

`data/distinct` é o par deste, na fase de limpar: das linhas que este nó
mostra, ele guarda a PRIMEIRA de cada repetição e descarta as demais — ver
antes de apagar."),

      trama::tr_node("data/clean_names", fn = tr_clean_names, label = "Limpar nomes",
        category = "clean",
        description = "Padroniza os nomes das colunas em snake_case, sem acento e sem repetição.",
        icon = trama::tr_icon("case-sensitive"),
        inputs = list(data = T), outputs = list(out = T),
        help = "## Descrição

Reescreve os nomes das colunas num formato único: tudo minúsculo, sem acento,
sem espaço nas pontas, com espaço e pontuação virando `_`. `Região Norte` vira
`regiao_norte`; `  Valor R$ ` vira `valor_r`.

Vale a pena logo depois de ler planilha ou CSV exportado de sistema: com nome
limpo, as expressões dos nós seguintes deixam de precisar de crase e de acento
para citar uma coluna.

Quando duas colunas diferentes viram o MESMO nome depois da limpeza — o caso de
`Valor R$` e `Valor (R$)`, ambas indo para `valor_r` —, a segunda ganha sufixo
numérico (`valor_r_2`). Nenhuma coluna é perdida, mas vale conferir qual ficou
com qual nome antes de seguir.

O conteúdo das colunas não é tocado; só os nomes mudam.

## Valor

A mesma tabela, com os nomes das colunas padronizados. O número de linhas e de
colunas nunca muda.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_excel\", path = \"vendas.xlsx\", sheet = \"1\") |>
  tr_add(\"nomes\", \"data/clean_names\", from = \"ler\") |>
  tr_add(\"olhar\", \"data/summary\", from = \"nomes\")
```

## Veja também

`data/rename` quando os nomes a corrigir são poucos e escolhidos a dedo."),

      trama::tr_node("data/remove_empty", fn = tr_remove_empty, label = "Remover vazias",
        category = "clean",
        description = "Descarta linhas ou colunas inteiramente vazias.",
        icon = trama::tr_icon("eraser"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(which = trama::tr_param_enum("ambos", c("linhas","colunas","ambos"),
                                                   label = "O quê")),
        help = "## Descrição

Descarta as linhas e as colunas INTEIRAMENTE vazias — aquelas em que todo valor
é faltante. É a faxina de quem leu planilha com linha de rodapé em branco ou
com coluna criada por engano e nunca preenchida.

Vazio aqui significa faltante em todas as posições, e nada mais. Coluna com
texto vazio (`\"\"`), com espaço ou com zero não é vazia e não é removida — esses
valores existem, e apagá-los seria decidir por quem está analisando. Basta um
valor presente para a linha ou a coluna ficar.

Isso NÃO deixa de fora a linha em branco do CSV: célula vazia de arquivo
delimitado chega como faltante (ver `data/read_csv`), não como texto vazio, e
uma linha inteira de vírgulas é removida como se espera.

Aplicar aos dois sentidos não tem ordem: como só sai o que é vazio por
completo, remover linhas antes ou depois das colunas dá o mesmo resultado.

## Parâmetros

- **O quê** — `linhas`, `colunas` ou `ambos`.

## Valor

A tabela sem o que estava inteiramente vazio. Uma tabela sem nada vazio passa
intacta.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_excel\", path = \"vendas.xlsx\", sheet = \"1\") |>
  tr_add(\"faxina\", \"data/remove_empty\", which = \"ambos\", from = \"ler\") |>
  tr_add(\"olhar\", \"data/summary\", from = \"faxina\")
```

## Veja também

`data/drop_na` para as linhas com faltante em ALGUMA coluna, que é bem mais
agressivo; `data/summary` para ver antes quanta coisa falta em cada coluna."),

      # O par de `data/get_dupes`, lá em "conhecer": aquele mostra, este apaga.
      trama::tr_node("data/distinct", fn = tr_distinct, label = "Remover duplicadas",
        category = "clean",
        description = "Mantém uma linha por combinação das colunas escolhidas (ou da linha inteira, se vazio).",
        icon = trama::tr_icon("copy-x"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(cols = P("cols", "", label = "Colunas", example = "regiao, produto")),
        help = "## Descrição

Guarda uma linha por combinação das colunas escolhidas e descarta as demais.
Com **Colunas** em branco, compara a linha inteira — o pedido comum de quem
acabou de empilhar duas tabelas com sobreposição.

Das repetidas, fica a PRIMEIRA na ordem em que a tabela está, com todas as
outras colunas como estavam nela. O nó não resume e não escolhe a melhor: para
decidir qual repetida sobrevive, ordene antes por aquilo que a define — a mais
recente, a de maior valor — e só então remova.

Este é o par de **Duplicadas**, na fase de conhecer: aquele mostra o que se
repete, este apaga. Ver antes de apagar poupa descobrir tarde que a repetição
era um dado legítimo.

## Parâmetros

- **Colunas** — colunas que definem a identidade da linha, separadas por
  vírgula. Vazio compara a linha inteira. Nome inexistente para o nó e lista as
  colunas disponíveis, em vez de ser ignorado em silêncio.

## Valor

A tabela com uma linha por combinação distinta. As colunas continuam as mesmas.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"recentes\", \"data/arrange\", cols = \"data\", desc = TRUE, from = \"ler\") |>
  tr_add(\"unicas\", \"data/distinct\", cols = \"cliente\", from = \"recentes\")
```

## Veja também

`data/get_dupes` mostra o que este nó vai apagar; `data/arrange` decide qual das
repetidas fica."),

      trama::tr_node("data/rename", fn = tr_rename, label = "Renomear",
        category = "clean",
        description = "Renomeia colunas: a primeira de 'De' vira a primeira de 'Para'.",
        icon = trama::tr_icon("pencil-line"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(from = P("cols", "", label = "De", example = "regiao, valor"),
                      to   = P("cols", "", label = "Para", example = "uf, preco")),
        help = "## Descrição

Troca o nome de colunas escolhidas. As duas listas casam pela POSIÇÃO: o
primeiro nome de **De** vira o primeiro de **Para**, o segundo vira o segundo, e
assim por diante. O conteúdo da coluna vai junto com o nome.

Casar por posição é o que permite dois campos simples em vez de um editor de
pares, e é também o jeito de errar calado — por isso as duas listas precisam ter
o mesmo tamanho. Com tamanhos diferentes o nó para e diz quantos nomes há de
cada lado, em vez de renomear a coluna errada.

Nome de destino que já pertence a outra coluna da tabela é recusado, e o mesmo
vale para dois destinos iguais na própria lista: a tabela ficaria com duas
colunas de mesmo nome. Trocar dois nomes entre si é caso legítimo e passa — a
troca é simultânea, e `regiao, produto` para `produto, regiao` funciona.

Com **De** em branco, o nó não faz nada.

## Parâmetros

- **De** — nomes atuais das colunas, separados por vírgula. Nome inexistente
  para o nó e lista as colunas disponíveis.
- **Para** — nomes novos, na mesma ordem e na mesma quantidade.

## Valor

A tabela com as colunas renomeadas, na mesma posição e com o mesmo conteúdo.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"nomes\", \"data/rename\", to = \"uf, preco\") |>
  tr_link(\"ler\", \"nomes\") |>
  tr_set(\"nomes\", from = \"regiao, valor\")
```

O param **De** deste nó se chama `from`, que é também o argumento de ligação de
`tr_add()`. Escrevendo o fluxo em R, os dois se separam: a aresta vai por
`tr_link()` e o param por `tr_set()` — passar `from =` no `tr_add()` é recusado,
justamente para o valor não virar aresta em silêncio.

## Veja também

`data/clean_names` quando o problema é a tabela inteira, e não uma coluna ou
outra."),

      trama::tr_node("data/drop_na", fn = tr_drop_na, label = "Remover faltantes",
        category = "clean",
        description = "Descarta as linhas com valor faltante nas colunas escolhidas (ou em qualquer uma, se vazio).",
        icon = trama::tr_icon("circle-slash"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(cols = P("cols", "", label = "Colunas", example = "valor, qtd")),
        help = "## Descrição

Descarta as linhas que têm valor faltante nas colunas escolhidas. Com
**Colunas** em branco, descarta a linha que tiver faltante em QUALQUER coluna —
o que costuma ser bem mais agressivo do que parece numa tabela larga.

Vale olhar o resumo antes: uma coluna com muitos faltantes que nem entra na
análise leva embora, sozinha, metade das linhas boas. Nomear no campo só as
colunas de que a análise depende é quase sempre o que se quer.

Faltante aqui é o valor ausente do R. Zero é valor presente, e a linha que o
contém fica. Texto vazio TAMBÉM é valor presente — mas repare que célula vazia
de CSV não chega como texto vazio: o `data/read_csv` a lê como faltante, e essa
linha cai aqui.

## Parâmetros

- **Colunas** — colunas em que o faltante derruba a linha, separadas por
  vírgula. Vazio olha todas. Nome inexistente para o nó e lista as colunas
  disponíveis, em vez de ser ignorado em silêncio.

## Valor

A tabela com menos linhas — ou com as mesmas, se não faltava nada. As colunas
continuam as mesmas.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"dados\", \"data/example\", dataset = \"airquality\") |>
  tr_add(\"perfil\", \"data/summary\", from = \"dados\") |>
  tr_add(\"limpo\", \"data/drop_na\", cols = \"Ozone\", from = \"dados\")
```

## Veja também

`data/replace_na` para preencher em vez de descartar; `data/remove_empty` para
o que está inteiramente vazio; `data/summary` para medir o faltante antes de
decidir."),

      trama::tr_node("data/replace_na", fn = tr_replace_na, label = "Preencher faltantes",
        category = "clean",
        description = "Substitui os valores faltantes das colunas escolhidas por um valor fixo.",
        icon = trama::tr_icon("paint-bucket"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(cols  = P("cols", "", label = "Colunas", example = "valor, qtd"),
                      value = P("text", "0", label = "Substituir por", example = "0")),
        help = "## Descrição

Troca os valores faltantes das colunas escolhidas por um valor fixo, digitado
no card.

O valor digitado é sempre texto, e é convertido para o TIPO da coluna antes de
entrar. Coluna numérica continua numérica, coluna de inteiros continua de
inteiros, coluna lógica continua lógica, e fator ganha o nível novo. Isso é o
ponto do nó: preencher sem promover a coluna a texto por causa de um campo de
formulário — coluna com tipo trocado quebra o cálculo lá adiante, longe daqui.

Quando o valor não vira o tipo da coluna, o nó para e diz qual coluna e qual
valor: `abacaxi` numa coluna de datas, `talvez` numa lógica, `0.5` numa coluna
de inteiros (que viraria 0 calado). Preencher com um valor que não é daquele
tipo é sempre engano.

Campo em branco é nó desligado, dos dois lados: sem **Colunas** ou sem
**Substituir por**, a tabela passa intacta. Campo limpo não é ordem de trocar
faltante por texto vazio.

## Parâmetros

- **Colunas** — colunas a preencher, separadas por vírgula. Nome inexistente
  para o nó e lista as colunas disponíveis.
- **Substituir por** — o valor que entra no lugar do faltante. Data se escreve
  como `2026-01-31`; lógico aceita `TRUE`/`FALSE`.

## Valor

A tabela com os faltantes preenchidos nas colunas escolhidas, com os tipos
preservados. O número de linhas nunca muda.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"cheio\", \"data/replace_na\", cols = \"qtd\", value = \"0\", from = \"ler\")
```

## Veja também

`data/drop_na` para descartar em vez de preencher; `data/convert` quando o
problema é o tipo da coluna, e não o buraco nela."),

      trama::tr_node("data/convert", fn = tr_convert, label = "Converter tipo",
        category = "clean",
        description = "Converte colunas para número, inteiro, texto, data, fator ou lógico.",
        icon = trama::tr_icon("arrow-right-left"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(cols    = P("cols", "", label = "Colunas", example = "valor, qtd"),
                      type    = trama::tr_param_enum("numero",
                                  c("numero","inteiro","texto","data","fator","logico"),
                                  label = "Para"),
                      format  = P("text", "", label = "Formato da data", example = "%d/%m/%Y"),
                      decimal = trama::tr_param_enum(".", c(".", ","), label = "Decimal")),
        help = "## Descrição

Converte as colunas escolhidas para número, inteiro, texto, data, fator ou
lógico. É o nó do dia seguinte à leitura: coluna que o CSV entregou como texto
porque tinha vírgula decimal, data que chegou como `31/12/2026`, código que
virou número e devia ser categoria.

A conversão FALHA ALTO quando algum valor presente na entrada viraria faltante.
Em vez de sumir com o dado e seguir verde, o nó para e mostra os valores que
atrapalharam — até três, mais a contagem do resto. Faltante que já era faltante
na entrada continua faltante e não dispara nada.

A leitura de número é estrita de propósito. `R$ 10`, `10abc`, `(50)`, `10%`,
`1/2` e `1.2.3` INTERROMPEM o fluxo em vez de virarem 10, 50 ou 1,2 — que é o
que uma leitura tolerante faria, calada. Um número errado em silêncio é pior que
um erro: limpe a coluna antes, num nó **Criar coluna**, e converta depois.

O separador decimal decide como o texto é lido, junto com o de milhar, que é o
outro dos dois: com decimal `,`, o milhar é `.`, e `1.234,56` vira 1234,56; com
decimal `.`, `1,234.56` vira o mesmo número. O separador de milhar só é aceito
quando está bem formado, em grupos de exatos três dígitos — é o que separa
`1,234.56` de `1,2,3`. E `1.234,56` lido com decimal `.` é recusado, em vez de
virar 1,23456.

Coluna que JÁ é do tipo de destino passa direto, sem virar texto no caminho. Não
é economia: com decimal `,`, o número 1234.56 dava a volta por texto e voltava
como 123456 — mil vezes maior, sem nenhum faltante para denunciar.

Para data, o campo **Formato da data** em branco aceita a data em ordem
ANO-MÊS-DIA, com `-` ou com `/`: `2026-12-31` e `2026/12/31` passam os dois.
Qualquer outra ordem, não: `31/12/2026` sem formato viraria faltante em toda a
coluna — e é aí que o nó para, pedindo o formato.

`inteiro` lê como número e ARREDONDA: `1.234,56` vira 1235.

## Parâmetros

- **Colunas** — colunas a converter, separadas por vírgula. Todas vão para o
  mesmo tipo. Vazio deixa a tabela intacta; nome inexistente para o nó e lista
  as colunas disponíveis.
- **Para** — tipo de destino: `numero`, `inteiro`, `texto`, `data`, `fator` ou
  `logico`.
- **Formato da data** — só é usado quando o destino é `data`. Escreve-se com os
  códigos do R: `%d/%m/%Y` para `31/12/2026`, `%d-%m-%Y`, `%Y%m%d`.
- **Decimal** — qual caractere separa a parte decimal no texto de origem, `.`
  ou `,`. O outro passa a valer como separador de milhar.

## Valor

A tabela com as colunas escolhidas no tipo pedido. O número de linhas e de
colunas não muda; só o tipo, e com ele o que os nós seguintes conseguem fazer —
soma, média e ordenação por grandeza só existem depois daqui.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\", delim = \";\") |>
  tr_add(\"olhar\", \"data/summary\", from = \"ler\") |>
  tr_add(\"valor\", \"data/convert\", cols = \"valor\", decimal = \",\", from = \"ler\") |>
  tr_set(\"valor\", type = \"numero\") |>
  tr_add(\"quando\", \"data/convert\", cols = \"data\", format = \"%d/%m/%Y\",
         from = \"valor\") |>
  tr_set(\"quando\", type = \"data\") |>
  tr_add(\"total\", \"data/group_summarise\", by = \"regiao\", name = \"receita\",
         expr = \"sum(valor)\", from = \"quando\")
```

O param **Para** deste nó se chama `type`, que é também o argumento de tipo de
nó de `tr_add()`; escrevendo o fluxo em R, ele se preenche com `tr_set()`.

## Veja também

`data/summary` mostra o tipo de cada coluna e é o que revela a conversão que
falta; `data/mutate` para limpar o texto (tirar moeda, unidade, parênteses)
antes de converter."),

      trama::tr_node("data/filter", fn = tr_filter, label = "Filtrar", version = 2L,
        category = "transform",
        description = "Mantém as linhas em que a condição é verdadeira. Com 'Por grupo' preenchido, a condição é relativa ao grupo.",
        icon = trama::tr_icon("list-filter"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(expr = P("expr", "", label = "Condição", example = "valor > 100"),
                      by   = P("cols", "", label = "Por grupo", example = "regiao")),
        help = "## Descrição

Mantém as linhas em que a condição é verdadeira e descarta as demais. A
condição é avaliada com as colunas da entrada visíveis como variáveis:
`valor > 100`, `regiao == \"sul\"`, `!is.na(qtd) & qtd > 0`.

Com **Por grupo** preenchido, a condição passa a ser relativa ao GRUPO, e não à
tabela inteira. `valor == max(valor)` sem grupo olha o máximo da tabela toda;
com grupo em `regiao`, olha o máximo de cada região e devolve a campeã de cada
uma. Nos dois casos, EMPATE no máximo devolve todas as linhas empatadas — a
condição é uma comparação, não um \"pegue uma\". Para ficar com uma linha só,
use **Ordenar** e depois **Primeiras N**.

Linha em que a condição sai faltante não é mantida — faltante não é verdadeiro.
Para guardá-la, escreva a condição que a inclua, como
`is.na(qtd) | qtd > 0`.

Condição em branco é nó desligado: a tabela passa intacta.

## Parâmetros

- **Condição** — expressão R que resulta em verdadeiro ou falso por linha.
  Expressão que não é R válido para o nó e ecoa o texto digitado; expressão que
  cita coluna inexistente para na avaliação e lista as colunas disponíveis.
- **Por grupo** — colunas que definem os grupos, separadas por vírgula. Vazio
  significa sem agrupamento. Nome inexistente para o nó e lista as colunas
  disponíveis.

## Valor

A tabela de entrada com menos linhas — ou com as mesmas, se tudo passou. As
colunas continuam as mesmas.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"grandes\", \"data/filter\", expr = \"valor > 100\", from = \"ler\") |>
  tr_add(\"campeas\", \"data/filter\", expr = \"valor == max(valor)\",
         by = \"regiao\", from = \"ler\")
```

## Veja também

`data/slice_head` para cortar por posição em vez de por condição;
`data/mutate` quando o cálculo deve virar coluna em vez de derrubar linha."),

      trama::tr_node("data/mutate", fn = tr_mutate, label = "Criar coluna", version = 2L,
        category = "transform",
        description = "Acrescenta ou substitui uma coluna. Com 'Por grupo' preenchido, vira função de janela (rank, lag, cumsum, participação no total).",
        icon = trama::tr_icon("square-plus"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(name = P("text", "nova", label = "Nome", example = "participacao"),
                      expr = P("expr", "", label = "Expressão",
                               example = "valor / sum(valor)"),
                      by   = P("cols", "", label = "Por grupo", example = "regiao")),
        help = "## Descrição

Acrescenta uma coluna à tabela, ou substitui a coluna de mesmo nome. A
expressão é avaliada com as colunas da entrada visíveis como variáveis.

Com **Por grupo** preenchido, a expressão é avaliada dentro de cada grupo e o
número de linhas é preservado — é assim que se obtém posição, defasagem, soma
acumulada e participação no total.

Com **Expressão** em branco o nó está desligado e a tabela passa inteira: um
card recém-arrastado ainda não calcula nada. Com **Nome** em branco, não: não
existe criar coluna sem nome, então o nó para e pede o preenchimento em vez de
devolver a tabela intacta fingindo que trabalhou.

## Parâmetros

- **Nome** — nome da coluna criada. Campo obrigatório quando há expressão.
- **Expressão** — expressão R avaliada sobre as colunas da entrada.
- **Por grupo** — colunas que definem os grupos, separadas por vírgula. Vazio
  significa sem agrupamento. Nome inexistente para o nó e lista as colunas
  disponíveis.

## Valor

A tabela de entrada com a coluna acrescentada ou substituída. O número de
linhas nunca muda.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"parte\", \"data/mutate\", name = \"participacao\",
         expr = \"valor / sum(valor)\", by = \"regiao\", from = \"ler\") |>
  tr_add(\"pos\", \"data/mutate\", name = \"posicao\", expr = \"rank(-valor)\",
         by = \"regiao\", from = \"parte\")
```

## Veja também

`data/group_summarise` quando o resultado deve ser uma linha por grupo, e não
uma coluna a mais; `data/convert` para acertar o tipo antes de calcular."),

      # Um nó, duas direções: `remove` chegou na versão 2, e documento salvo na
      # versão 1 não traz o param — `.tr_effective_params()` preenche com o
      # default do spec, então o fluxo antigo continua abrindo e mantendo.
      trama::tr_node("data/select", fn = tr_select, label = "Selecionar", version = 2L,
        category = "transform",
        description = "Mantém apenas as colunas escolhidas — ou, com a chave ligada, joga fora só elas.",
        icon = trama::tr_icon("columns-3"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(cols   = P("cols", "", label = "Colunas", example = "regiao, valor"),
                      remove = trama::tr_param_bool(FALSE, label = "Remover em vez de manter")),
        help = "## Descrição

Escolhe com quais colunas a tabela segue adiante. No modo MANTER, as colunas
ficam na ORDEM em que foram escritas no campo, o que faz do nó também o jeito
de reordenar colunas.

A chave **Remover em vez de manter** inverte o sentido da mesma lista: com ela
desligada, ficam só as colunas nomeadas; com ela ligada, saem só as nomeadas e
o resto continua — nesse modo a ordem digitada não vale como ordem de saída,
porque as colunas que ficam são justamente as que NÃO foram nomeadas, e elas
seguem na ordem original da tabela. Para reordenar, use o modo manter.

É um nó só porque a decisão entre manter e remover é a mesma lista vista pelos
dois lados — quem monta o fluxo troca de ideia sem trocar de caixa.

Nome de coluna que não existe na entrada para o nó e lista as colunas
disponíveis. Antes o nome errado era descartado em silêncio: escrever `regaio`
no lugar de `regiao` deixava o fluxo verde e a tabela sem a coluna.

Campo em branco é nó desligado: a tabela passa inteira, com ou sem a chave.

## Parâmetros

- **Colunas** — colunas a manter (ou a remover), separadas por vírgula. No modo
  manter, a ordem digitada é a ordem da saída; no modo remover, a saída fica na
  ordem original da tabela.
- **Remover em vez de manter** — inverte o sentido da lista.

## Valor

A tabela com o subconjunto de colunas escolhido. O número de linhas nunca muda.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"essencial\", \"data/select\", cols = \"regiao, produto, valor\",
         from = \"ler\") |>
  tr_add(\"sem_id\", \"data/select\", cols = \"id_interno\", remove = TRUE,
         from = \"ler\")
```

## Veja também

`data/rename` para trocar o nome das colunas que ficaram; `data/summary` para
decidir quais valem a pena guardar."),

      trama::tr_node("data/arrange", fn = tr_arrange, label = "Ordenar",
        category = "transform", description = "Ordena as linhas pelas colunas escolhidas.",
        icon = trama::tr_icon("arrow-up-down"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(cols = P("cols", "", label = "Colunas", example = "valor"),
                      desc = trama::tr_param_bool(FALSE, label = "Decrescente")),
        help = "## Descrição

Ordena as linhas pelas colunas escolhidas. Com mais de uma coluna, a primeira
manda e as seguintes desempatam, na ordem em que foram escritas.

A chave **Decrescente** vale para TODAS as colunas do campo de uma vez; não há
como inverter só uma delas. Para misturar sentidos, encadeie dois nós — a
ordenação do dplyr é estável, então a segunda passada preserva a ordem da
primeira dentro dos empates.

Ordenar é o que decide qual linha é a \"primeira\": os nós que guardam a
primeira de cada grupo — **Primeiras N** e **Remover duplicadas** — dependem da
ordem em que a tabela chega neles.

Faltantes vão sempre para o fim, em qualquer dos dois sentidos. Texto usa a
ordem alfabética do idioma configurado; fator usa a ordem dos NÍVEIS, e não a
alfabética — o que é justamente o ponto de converter para fator.

Campo em branco é nó desligado: a tabela passa na ordem em que chegou.

## Parâmetros

- **Colunas** — colunas de ordenação, separadas por vírgula, da mais forte para
  a que desempata. Nome inexistente para o nó e lista as colunas disponíveis.
- **Decrescente** — inverte o sentido de todas as colunas do campo.

## Valor

A mesma tabela, com as linhas em outra ordem. Nenhuma linha e nenhuma coluna
entra ou sai.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"maiores\", \"data/arrange\", cols = \"valor\", desc = TRUE,
         from = \"ler\") |>
  tr_add(\"top10\", \"data/slice_head\", n = 10L, from = \"maiores\")
```

## Veja também

`data/slice_head` costuma vir logo depois, para ficar com o começo da ordem;
`data/distinct` usa a ordem para decidir qual repetida sobrevive."),

      trama::tr_node("data/slice_head", fn = tr_slice_head, label = "Primeiras N", version = 2L,
        category = "transform",
        description = "Mantém as N primeiras linhas. Com 'Por grupo' preenchido, N por grupo (top-N).",
        icon = trama::tr_icon("list-start"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(n  = trama::tr_param_int(10, 1, 10000, label = "N"),
                      by = P("cols", "", label = "Por grupo", example = "regiao")),
        help = "## Descrição

Guarda as N primeiras linhas da tabela, na ordem em que ela chega, e descarta o
resto. Corta por POSIÇÃO, e não por condição: quem decide o que é \"primeiro\" é
o nó de ordenar que vem antes.

Com **Por grupo** preenchido, são N linhas POR GRUPO — o top-N por região, os
três produtos mais caros de cada categoria. As linhas escolhidas saem na ordem
em que estavam na tabela de entrada, e não reagrupadas por grupo: agrupar aqui
escolhe QUAIS linhas ficam, não em que ordem elas saem. Para ver os grupos
juntos, ordene por eles depois.

Tabela ou grupo com menos de N linhas sai inteiro, sem erro e sem preenchimento.

**Por grupo** em branco é o agrupamento DESLIGADO, e não o nó desligado: sem
ele, as N primeiras são as N primeiras da tabela inteira. É o mesmo vazio de
**Filtrar**, **Criar coluna** e **Agrupar e resumir**. Ao contrário desses
três, aqui não há campo de texto que desligue o nó: **N** é um número, então
este nó sempre corta.

E como nos outros três, nome de coluna que não existe na entrada para o nó e
lista as colunas disponíveis, antes de qualquer corte. Errar `regaio` por
`regiao` agruparia por nada e o top-N sairia da tabela toda, verde e errado.

## Parâmetros

- **N** — quantas linhas guardar, de 1 a 10000.
- **Por grupo** — colunas que definem os grupos, separadas por vírgula. Vazio
  conta as N primeiras da tabela toda. Nome inexistente para o nó e lista as
  colunas disponíveis.

## Valor

A tabela com no máximo N linhas — ou N por grupo. As colunas continuam as
mesmas.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"maiores\", \"data/arrange\", cols = \"valor\", desc = TRUE,
         from = \"ler\") |>
  tr_add(\"top3\", \"data/slice_head\", n = 3L, by = \"regiao\", from = \"maiores\")
```

## Veja também

`data/arrange` é o nó que dá sentido a este; `data/filter` para cortar por
condição em vez de por posição."),

      trama::tr_node("data/pivot_longer", fn = tr_pivot_longer, label = "Empilhar colunas",
        category = "reshape",
        description = "Transforma várias colunas em duas: uma com o nome, outra com o valor.",
        icon = trama::tr_icon("unfold-vertical"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(cols      = P("cols", "", label = "Colunas", example = "jan, fev, mar"),
                      names_to  = P("text", "nome", label = "Nome vai para", example = "mes"),
                      values_to = P("text", "valor", label = "Valor vai para",
                                    example = "faturamento")),
        help = "## Descrição

Empilha várias colunas em duas: uma com o NOME da coluna de origem, outra com o
VALOR que estava nela. A tabela fica mais alta e mais estreita.

É o caminho da planilha para a tabela de análise. Uma planilha com uma coluna
por mês — `jan`, `fev`, `mar` — guarda o mês no cabeçalho, onde nenhum nó
consegue agrupar, filtrar ou somar por ele. Empilhada, o mês vira dado numa
coluna, e o resto do fluxo passa a alcançá-lo.

As colunas que NÃO foram escolhidas são repetidas em cada linha nova: uma linha
com três colunas empilhadas vira três linhas.

As colunas empilhadas viram uma coluna só, então precisam ser do mesmo tipo, ou
o valor é promovido ao tipo que acomoda todas — empilhar uma coluna de texto
junto com colunas numéricas transforma todos os números em texto.

Os dois campos de destino são obrigatórios e param o nó em branco. Voltar ao
nome padrão calado faria a tela discordar do resultado: o card mostraria o
campo vazio e a tabela sairia com uma coluna chamada `nome`.

Sem colunas escolhidas, a tabela passa intacta.

## Parâmetros

- **Colunas** — colunas a empilhar, separadas por vírgula. Nome inexistente
  para o nó e lista as colunas disponíveis.
- **Nome vai para** — nome da coluna que recebe os nomes das colunas
  empilhadas. Obrigatório.
- **Valor vai para** — nome da coluna que recebe os valores. Obrigatório.

## Valor

Uma tabela mais alta: as colunas não escolhidas, mais as duas novas. O número
de linhas fica multiplicado pela quantidade de colunas empilhadas.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_excel\", path = \"vendas.xlsx\", sheet = \"1\") |>
  tr_add(\"alto\", \"data/pivot_longer\", cols = \"jan, fev, mar\",
         names_to = \"mes\", values_to = \"faturamento\", from = \"ler\") |>
  tr_add(\"total\", \"data/group_summarise\", by = \"mes\", name = \"receita\",
         expr = \"sum(faturamento)\", from = \"alto\")
```

## Veja também

`data/pivot_wider` faz o caminho de volta; `data/group_summarise` é o passo que
o empilhamento costuma destravar."),

      trama::tr_node("data/pivot_wider", fn = tr_pivot_wider, label = "Espalhar colunas",
        category = "reshape",
        description = "Transforma os valores de uma coluna em colunas novas.",
        icon = trama::tr_icon("unfold-horizontal"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(names_from  = P("text", "", label = "Nomes vêm de", example = "mes"),
                      values_from = P("text", "", label = "Valores vêm de",
                                      example = "faturamento"),
                      values_fill = P("text", "", label = "Preencher vazio com",
                                      example = "0")),
        help = "## Descrição

Espalha os valores de uma coluna em colunas novas: cada valor distinto de
**Nomes vêm de** vira uma coluna, preenchida com o que estava em **Valores vêm
de**. A tabela fica mais larga e mais baixa.

É o caminho de volta do empilhar, e o formato de quem vai LER a tabela: uma
linha por cliente, uma coluna por mês. Costuma ser o último passo antes de
gravar ou de mostrar.

A identidade da linha na saída é tudo o que sobra — todas as colunas que não
são **Nomes vêm de** nem **Valores vêm de**. São elas que definem quantas
linhas o resultado terá.

A identidade se repetir é o caso NORMAL, e é justamente o que produz as várias
colunas: o mesmo cliente aparece uma vez por mês, e cada uma dessas linhas vira
uma coluna. O que o nó recusa é a identidade repetida JUNTO COM o mesmo **Nomes
vêm de** — o mesmo cliente com o mesmo mês duas vezes. Aí sim uma célula
receberia dois valores, e a mensagem diz quais combinações se repetiram (até
três, mais a contagem do resto) e manda agregar antes num nó **Agrupar e
resumir**. Sem essa recusa, a célula mostraria `1,2` como se fosse um valor só
— um número plausível, na tela, que não existe no dado.

Combinação que não aparece na entrada vira faltante na saída, a menos que
**Preencher vazio com** esteja preenchido. O que se digita ali é convertido:
número escrito vira número, para não transformar a coluna inteira em texto por
causa de um campo de formulário; qualquer outra coisa entra como texto.

Os dois campos de origem são obrigatórios e param o nó em branco, cada um
culpando o seu próprio campo — são dois campos distintos no card, e uma
mensagem que citasse os dois deixaria quem digitou adivinhando qual errou.

## Parâmetros

- **Nomes vêm de** — UMA coluna, cujos valores viram os nomes das colunas
  novas. Obrigatório; espaço em volta é aparado.
- **Valores vêm de** — UMA coluna, que preenche as células. Obrigatório;
  espaço em volta é aparado.
- **Preencher vazio com** — valor para as combinações que não existem na
  entrada. Em branco, elas ficam faltantes.

## Valor

Uma tabela mais larga: uma linha por combinação das colunas que sobraram, e uma
coluna por valor distinto de **Nomes vêm de**.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"mensal\", \"data/group_summarise\", by = \"cliente, mes\",
         name = \"faturamento\", expr = \"sum(valor)\", from = \"ler\") |>
  tr_add(\"largo\", \"data/pivot_wider\", names_from = \"mes\",
         values_from = \"faturamento\", values_fill = \"0\", from = \"mensal\")
```

O resumo antes do espalhar não é enfeite: é ele que garante uma linha por
combinação de chave, que é o que este nó exige.

## Veja também

`data/pivot_longer` faz o caminho de volta; `data/group_summarise` é o nó que
resolve a chave repetida."),

      trama::tr_node("data/group_summarise", fn = tr_group_summarise, label = "Agrupar e resumir",
        category = "aggregate", description = "Agrupa por colunas e calcula um ou mais resumos.",
        icon = trama::tr_icon("sigma"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(by   = P("cols", "", label = "Agrupar por", example = "regiao"),
                      name = P("text", "n", label = "Nome", example = "receita, pedidos"),
                      expr = P("expr", "dplyr::n()", label = "Resumo",
                               example = "sum(valor), dplyr::n()")),
        help = "## Descrição

Agrupa a tabela pelas colunas escolhidas e calcula uma ou mais métricas por
grupo. O resultado tem uma linha por combinação distinta das colunas de
agrupamento, as próprias colunas de agrupamento e mais uma coluna por resumo.

Para várias métricas, separe por vírgula nos DOIS campos — os nomes casam com
os resumos pela POSIÇÃO, como no nó **Renomear**:

```
Nome:   receita, media, pedidos
Resumo: sum(valor), mean(valor), dplyr::n()
```

Listas de tamanhos diferentes param o nó dizendo os dois números, em vez de
casar o nome de um resumo com a conta de outro em silêncio. Nome repetido — ou
igual a uma coluna de agrupamento — também para: a tabela sairia com o número
de colunas certo e a conta errada.

A vírgula DENTRO de uma chamada não separa resumo nenhum: `sum(valor, na.rm =
TRUE), dplyr::n()` são dois resumos, não três. Quem decide isso é o parser do
R, então parêntese, colchete e aspas valem como você espera.

Com **Agrupar por** em branco, resume a tabela INTEIRA numa linha só — que é
como se obtém o total geral.

Cada expressão precisa produzir um valor por grupo: `sum(valor)`, `mean(valor)`,
`dplyr::n()`, `dplyr::n_distinct(cliente)`, `max(valor) - min(valor)`. Uma
expressão que devolve vários valores por grupo — como `valor * 2` — não é
resumo, e é caso de **Criar coluna**.

Faltante contamina a conta: `sum(valor)` com um faltante no grupo devolve
faltante para o grupo inteiro. Escreva `sum(valor, na.rm = TRUE)` para ignorá-lo
— ou trate os faltantes antes, o que costuma ser mais honesto.

Os campos **Nome** e **Resumo** são obrigatórios e param o nó em branco,
culpando o campo certo. Voltar ao padrão calado faria o card ficar em branco e
a tabela sair com uma coluna chamada `n` contando linhas — tela e resultado
discordando. Nenhum dos dois tem comportamento honesto para o vazio: não existe
resumir sem conta, nem coluna sem nome.

## Parâmetros

- **Agrupar por** — colunas que definem os grupos, separadas por vírgula. Vazio
  resume a tabela inteira. Nome inexistente para o nó e lista as colunas
  disponíveis.
- **Nome** — nome da coluna de resumo, ou vários separados por vírgula.
  Obrigatório.
- **Resumo** — expressão R que produz um valor por grupo, ou várias separadas
  por vírgula, na mesma ordem dos nomes. Obrigatório. Expressão que não é R
  válido para o nó e ecoa o texto; expressão que falha ao avaliar para e lista
  as colunas disponíveis.

## Valor

Uma tabela com uma linha por grupo: as colunas de agrupamento mais uma coluna
por resumo, na ordem em que foram escritas. A saída não fica agrupada — os nós
seguintes recebem uma tabela comum.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"painel\", \"data/group_summarise\", by = \"regiao\",
         name = \"receita, pedidos\",
         expr = \"sum(valor), dplyr::n()\", from = \"ler\")
```

Ainda vale usar um nó por métrica quando cada resultado merece caixa própria na
tela, para conferir separado — as duas formas convivem.

## Veja também

`data/mutate` com **Por grupo** quando o resultado deve voltar para cada linha,
e não virar uma linha por grupo; `data/join` para pôr métricas de tabelas
DIFERENTES lado a lado; `data/pivot_wider` para espalhar o resumo em colunas."),

      trama::tr_node("data/join", fn = tr_join, label = "Juntar",
        category = "aggregate", description = "Junta duas tabelas por colunas em comum.",
        icon = trama::tr_icon("combine"),
        inputs = list(left = T, right = T), outputs = list(out = T),
        params = list(by   = P("cols", "", label = "Por", example = "regiao"),
                      type = trama::tr_param_enum("inner", c("inner","left","right","full","anti"),
                                                  label = "Tipo")),
        help = "## Descrição

Junta duas tabelas LADO A LADO, casando as linhas pelas colunas em comum. A
tabela ligada na primeira porta é a da esquerda; a da segunda porta é a da
direita.

O **Tipo** decide o que fazer com a linha que não encontra par:

- `inner` — só as linhas que existem dos dois lados.
- `left` — todas as da esquerda; as colunas da direita ficam faltantes quando
  não há par.
- `right` — todas as da direita, pelo mesmo critério.
- `full` — todas as linhas das duas, emparelhadas onde dá.
- `anti` — só as linhas da esquerda que NÃO têm par, e apenas com as colunas da
  esquerda. É o tipo para conferência: descobrir qual pedido não achou cliente.

Com **Por** em branco, o casamento é feito por todas as colunas de mesmo nome
nas duas tabelas — o que é cômodo e frágil, porque uma coluna homônima
esquecida (um `id` que significa outra coisa de cada lado) muda o resultado sem
avisar. Nomear as colunas é o caminho seguro.

Chave repetida MULTIPLICA linhas: uma chave que aparece duas vezes à esquerda e
três à direita produz seis linhas. É a causa mais comum de uma tabela crescer
sozinha depois de uma junção, e é o que **Duplicadas** mostra antes.

Coluna de mesmo nome que não está na chave sai duplicada com sufixo `.x` e
`.y`, uma de cada lado.

## Parâmetros

- **Por** — colunas que casam as duas tabelas, separadas por vírgula. Precisam
  existir nas duas: nome inexistente para o nó e lista as colunas disponíveis.
- **Tipo** — `inner`, `left`, `right`, `full` ou `anti`.

## Valor

Uma tabela com as colunas das duas — só as da esquerda, no caso do `anti`. O
número de linhas depende do tipo e da repetição da chave, e pode ser maior que
o das duas entradas somadas.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"pedidos\", \"data/read_csv\", path = \"pedidos.csv\") |>
  tr_add(\"clientes\", \"data/read_csv\", path = \"clientes.csv\") |>
  tr_add(\"junta\", \"data/join\", by = \"cliente_id\",
         from = c(\"pedidos\", \"clientes\")) |>
  tr_set(\"junta\", type = \"left\")
```

O param **Tipo** deste nó se chama `type`, que é também o argumento de tipo de
nó de `tr_add()`; escrevendo o fluxo em R, ele se preenche com `tr_set()`.

## Veja também

`data/bind_rows` quando as tabelas devem ficar uma EMBAIXO da outra, e não lado
a lado; `data/get_dupes` para conferir a chave antes de juntar."),

      # Junta LADO a lado (duas portas), empilha UMA embaixo da outra (uma porta
      # variádica): a diferença entre as duas está na forma da entrada, e é ela
      # que o usuário reconhece na tela ao arrastar a terceira aresta.
      trama::tr_node("data/bind_rows", fn = tr_bind_rows, label = "Empilhar tabelas",
        category = "aggregate",
        description = "Empilha várias tabelas uma embaixo da outra, casando as colunas pelo nome.",
        icon = trama::tr_icon("layers"),
        inputs = list(tabelas = trama::tr_port(T, multiple = TRUE)),
        outputs = list(out = T),
        help = "## Descrição

Empilha várias tabelas uma EMBAIXO da outra, casando as colunas pelo nome. É o
nó de quem tem um arquivo por mês, por filial ou por exportação, e quer uma
tabela só.

A porta de entrada aceita quantos cabos forem ligados — não há um segundo nó
para a terceira tabela. A ORDEM das linhas no resultado é a ordem em que os
cabos foram ligados, e é ela que decide qual tabela aparece primeiro. Hoje não
há como reordenar as ligações pela interface: para trocar a ordem, apague os
cabos e ligue-os de novo na ordem desejada.

Coluna que só existe em parte das tabelas aparece no resultado, faltante nas
linhas que vieram de quem não a tinha. É de propósito, e sem aviso: empilhar
exportações de meses diferentes, uma delas com uma coluna a mais, é caso
legítimo.

Coluna de mesmo nome com tipos incompatíveis entre as tabelas — número de um
lado, texto do outro — para o nó. A alternativa seria converter tudo para
texto, calada, e a soma lá adiante deixaria de funcionar longe daqui.

Com nenhuma tabela ligada, o nó nem chega a rodar: a porta é obrigatória e o
fluxo aponta a entrada solta. Com uma só, o resultado é essa tabela.

## Valor

Uma tabela com a soma das linhas das entradas, e com a união das colunas delas.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"jan\", \"data/read_csv\", path = \"vendas-01.csv\") |>
  tr_add(\"fev\", \"data/read_csv\", path = \"vendas-02.csv\") |>
  tr_add(\"mar\", \"data/read_csv\", path = \"vendas-03.csv\") |>
  tr_add(\"ano\", \"data/bind_rows\", from = c(\"jan\", \"fev\", \"mar\")) |>
  tr_add(\"olhar\", \"data/summary\", from = \"ano\")
```

## Veja também

`data/join` quando as tabelas devem ficar LADO a lado; `data/distinct` para a
sobreposição que o empilhamento costuma produzir."),

      trama::tr_node("data/to_stream", fn = tr_to_stream, label = "Entrar em fluxo",
        category = "stream",
        description = "Reparte a tabela em pontos e abre a região de fluxo: daqui até o 'Sair de fluxo', o grafo roda ponto a ponto.",
        icon = trama::tr_icon("chevrons-right"),
        inputs = list(data = trama::tr_port(T, required = FALSE)),
        outputs = list(out = trama::tr_port(T, stream = TRUE)),
        params = list(
          lote = trama::tr_param_int(1, min = 1, label = "Linhas por passo"),
          ordenar_por = P("cols", "", label = "Ordenar por", example = "data, regiao"),
          max_passos = trama::tr_param_int(0, min = 0,
                                           label = "Máximo de passos (0 = todos)")),
        help = "## Descrição

É aqui que a região de fluxo COMEÇA. Este nó reparte a tabela numa sequência de
pontos, e tudo que estiver ligado entre ele e um **Sair de fluxo** passa a rodar
uma vez por ponto, em ordem, como uma execução só.

Cada ponto é uma TABELA — as mesmas colunas da entrada, **Linhas por passo**
linhas. É por isso que os nós que você já conhece funcionam dentro da região sem
nenhum ajuste: um **Filtrar** dentro do fluxo recebe uma tabela, como sempre
recebeu, só que menor e uma vez por passo.

O último lote é PARCIAL, nunca descartado: dez linhas em lotes de três são
quatro passos, de 3, 3, 3 e 1 linha. Descartar o resto perderia linha da tabela
sem dizer nada.

Tabela VAZIA não é erro: dá zero passos, e o **Sair de fluxo** devolve um
histórico vazio. É o que acontece quando um filtro mais acima não achou nada, e
um fluxo saudável não deve ficar vermelho por isso.

A velocidade, o pause e o \"um passo\" NÃO são parâmetros deste card: são botões
da sessão, não conteúdo do fluxo, e mexer neles não recalcula nada. Os três
parâmetros abaixo são o contrário — mudam o RESULTADO, e por isso mudar qualquer
um deles faz a região inteira recomputar.

## Parâmetros

- **Linhas por passo** — quantas linhas cada ponto carrega. 1 é uma linha por
  passo, que é o caso de quem quer ver o fluxo andar; lote maior é o caso de
  quem quer velocidade — menos passos, menos overhead do driver por linha.
  Um nó COM MEMÓRIA (como `models/rls`) processa cada linha do ponto em
  ordem, uma atualização por linha: o resultado com lote 3 é IDÊNTICO ao
  mesmo dado entregue em lotes de 1, e nenhuma linha do lote é descartada —
  é o contrato que todo nó de memória desta coleção tem que cumprir (ver
  `models/rls`, \"o mesmo resultado do lm(), no limite\"). Um nó SEM memória
  (`data/filter`, por exemplo) já recebe o lote inteiro como tabela e não
  perde linha nenhuma de qualquer jeito.
- **Ordenar por** — colunas que definem a ORDEM dos pontos, separadas por
  vírgula. Vazio mantém a ordem da tabela. A ordenação acontece ANTES do corte
  em lotes, e é o que faz um fluxo temporal andar no tempo. Nome de coluna
  inexistente para o nó e lista as colunas disponíveis.
- **Máximo de passos** — para a sequência depois de N passos, contados em
  PASSOS e não em linhas. `0` quer dizer todos. Serve para experimentar um fluxo
  longo sem esperar por ele inteiro.

## Valor

A sequência de pontos que a região percorre. Não é uma tabela: é o fluxo, e só
faz sentido ligado a um nó dentro da região. O resultado que vai para o disco é
o do **Sair de fluxo**, na outra ponta.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"entra\", \"data/to_stream\", lote = 1L, ordenar_por = \"data\",
         from = \"ler\") |>
  tr_add(\"grandes\", \"data/filter\", expr = \"valor > 100\", from = \"entra\") |>
  tr_add(\"sai\", \"data/from_stream\", from = \"grandes\")
```

## Veja também

`data/from_stream` fecha a região — sem ele o fluxo não tem onde terminar, e o
documento é recusado no planejamento, dizendo isso."),

      trama::tr_node("data/from_stream", fn = tr_from_stream, label = "Sair de fluxo",
        category = "stream",
        description = "Empilha os pontos do fluxo numa tabela só — o histórico da região — e fecha a região.",
        icon = trama::tr_icon("layers"),
        inputs = list(data = trama::tr_port(T, stream = TRUE)),
        outputs = list(out = T),
        params = list(passo = trama::tr_param_bool(TRUE, label = "Coluna do passo")),
        help = "## Descrição

É aqui que a região de fluxo TERMINA. Este nó recebe o que cada passo produziu e
empilha tudo numa tabela só — o HISTÓRICO da região —, que é o que fica guardado
e é dado comum a partir daqui: qualquer nó de gráfico plota essa tabela sem saber
que ela veio de um fluxo.

O histórico é o resultado da região porque é ele que responde às perguntas que se
fazem de um fluxo (\"em que passo virou\", \"a curva estabilizou\"), e não o valor
do último passo.

As colunas do resultado são as do PRIMEIRO passo. Passo com conjunto diferente de
colunas para o nó, dizendo QUAL passo — empilhar de qualquer jeito preencheria
faltante nas colunas ausentes, e o histórico sairia verde, com buracos cuja causa
ninguém acharia.

Passo que produziu ZERO linhas — um **Filtrar** dentro do fluxo que não achou
nada — é um passo que contribui zero linhas, e não um passo que deixou de
existir: o índice dos passos seguintes não muda por causa dele.

Fluxo de zero passos devolve uma tabela de zero linhas, com a coluna do passo e
mais nada. Sem nenhum passo não há de onde saber que colunas o fluxo teria — este
nó só vê o que passou pelas arestas.

O nó tem uma ENTRADA SÓ, e isso é deliberado. Toda porta dele alimentada de
DENTRO da região acumula um valor por passo, inclusive uma que não fosse
declarada como fluxo: uma \"constante\" produzida lá dentro chegaria aqui como N
cópias dela, uma por passo. Constante que precisa acompanhar o histórico se liga
DEPOIS da região, onde ela é constante de verdade.

## Parâmetros

- **Coluna do passo** — acrescenta a coluna `passo`, com o índice do passo que
  produziu cada linha, na primeira posição. É o eixo x de praticamente todo
  gráfico que se faz de um histórico, e por isso vem ligada. Se os pontos já
  trazem uma coluna chamada `passo`, a do nó substitui a deles: duas colunas de
  mesmo nome quebrariam todo verbo ligado adiante.

## Valor

Uma tabela — os pontos empilhados, na ordem dos passos, com as colunas do
primeiro passo e a coluna `passo` na frente.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"entra\", \"data/to_stream\", lote = 1L, from = \"ler\") |>
  tr_add(\"grandes\", \"data/filter\", expr = \"valor > 100\", from = \"entra\") |>
  tr_add(\"sai\", \"data/from_stream\", from = \"grandes\")
```

## Veja também

`data/to_stream` abre a região; os nós de gráfico consomem o histórico como
qualquer outra tabela."),

      trama::tr_node("data/write_csv", fn = tr_write_csv, label = "Gravar CSV",
        category = "sink", description = "Grava a tabela no disco e a repassa adiante.",
        icon = trama::tr_icon("save"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(path = P("path", "", label = "Arquivo",
                               example = "saida/vendas.csv")),
        pure = FALSE, volatile = TRUE,
        fingerprint = function(params, ctx) ctx$path(params$path),
        help = "## Descrição

Grava a tabela num arquivo de texto separado por vírgula e a REPASSA adiante.
Repassar é de propósito: um gravador que cortasse o fluxo obrigaria a duplicar
o nó anterior só para continuar a análise depois de gravar.

O arquivo é reescrito por inteiro a cada gravação, sem perguntar. Caminho
relativo é resolvido a partir da pasta do projeto.

O nó grava em TODA execução do fluxo, mesmo que nada tenha mudado. Isso é
deliberado: um gravador cujo arquivo foi apagado não fez o trabalho dele, e sem
isso apagar o arquivo e recalcular não o recriava — o nó saía do cache, verde,
sem tocar no disco. A consequência é que o que estiver ligado DEPOIS de um
gravador recalcula sempre, porque a saída dele muda de chave a cada passada. Na
prática, o gravador costuma ser a última caixa da cadeia.

Caminho em branco é nó desligado: não grava nada, não reclama, e a tabela
continua passando. Um card recém-arrastado da paleta não tem por que pintar de
vermelho.

CSV é texto: o tipo das colunas não é guardado junto, e quem ler o arquivo vai
deduzi-lo de novo. Para levar os tipos junto, grave em `.rds` ou em Parquet.

## Parâmetros

- **Arquivo** — caminho do `.csv`, relativo à pasta do projeto. Em branco, o nó
  fica desligado.

## Valor

A mesma tabela que entrou, intacta, para quem estiver ligado adiante.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"total\", \"data/group_summarise\", by = \"regiao\", name = \"receita\",
         expr = \"sum(valor)\", from = \"ler\") |>
  tr_add(\"grava\", \"data/write_csv\", path = \"saida/resumo.csv\",
         from = \"total\")
```

## Veja também

`data/read_csv` lê de volta; `data/write_rds` quando os tipos das colunas
precisam sobreviver; `data/write_parquet` para arquivo grande."),

      trama::tr_node("data/write_rds", fn = tr_write_rds, label = "Gravar RDS",
        category = "sink", description = "Grava a tabela em .rds e a repassa adiante.",
        icon = trama::tr_icon("archive"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(path = P("path", "", label = "Arquivo",
                               example = "saida/vendas.rds")),
        pure = FALSE, volatile = TRUE,
        fingerprint = function(params, ctx) ctx$path(params$path),
        help = "## Descrição

Grava a tabela no formato `.rds`, o formato binário do próprio R, e a REPASSA
adiante. Como guarda um objeto R inteiro, é o formato que devolve a tabela
IDÊNTICA na leitura: fator com a ordem dos níveis, data, hora com fuso, atributo
— tudo volta como estava. É o formato para o resultado que outro fluxo, ou
outro script, vai ler.

O nó grava em TODA execução do fluxo, mesmo que nada tenha mudado. Isso é
deliberado: um gravador cujo arquivo foi apagado não fez o trabalho dele, e sem
isso apagar o arquivo e recalcular não o recriava. A consequência é que o que
estiver ligado DEPOIS de um gravador recalcula sempre, porque a saída dele muda
de chave a cada passada. Na prática, o gravador costuma ser a última caixa da
cadeia.

Caminho em branco é nó desligado: não grava nada, não reclama, e a tabela
continua passando.

A pasta de destino precisa existir; se não existir, o nó para dizendo qual é.
Cru, o R falharia com \"não é possível abrir a conexão\", sem dizer o caminho —
que é justamente o que se precisa saber.

## Parâmetros

- **Arquivo** — caminho do `.rds`, relativo à pasta do projeto. Em branco, o nó
  fica desligado.

## Valor

A mesma tabela que entrou, intacta, para quem estiver ligado adiante.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\", delim = \";\") |>
  tr_add(\"limpo\", \"data/drop_na\", cols = \"valor\", from = \"ler\") |>
  tr_add(\"grava\", \"data/write_rds\", path = \"saida/vendas.rds\",
         from = \"limpo\")
```

## Veja também

`data/read_rds` lê de volta; `data/write_csv` quando o arquivo precisa ser lido
por outro programa; `data/write_parquet` para arquivo grande que ainda assim
guarda os tipos."),

      trama::tr_node("data/write_parquet", fn = tr_write_parquet, label = "Gravar Parquet",
        category = "sink", description = "Grava a tabela em Parquet e a repassa adiante.",
        icon = trama::tr_icon("hard-drive-download"),
        inputs = list(data = T), outputs = list(out = T),
        params = list(path = P("path", "", label = "Arquivo",
                               example = "saida/vendas.parquet")),
        pure = FALSE, volatile = TRUE,
        fingerprint = function(params, ctx) ctx$path(params$path),
        help = "## Descrição

Grava a tabela em Parquet — formato colunar e comprimido, que guarda o tipo de
cada coluna junto com os dados — e a REPASSA adiante. Ocupa bem menos disco que
o CSV equivalente e é lido por outras ferramentas além do R, o que faz dele o
formato de entrega para tabela grande.

O que o Parquet preserva está medido: inteiro, número, lógico, texto, data, hora
com fuso e fator com a ordem dos níveis voltam como estavam. O que ele NÃO
preserva também: coluna de duração volta em SEGUNDOS, qualquer que fosse a
unidade gravada, e coluna complexa ou lista com tipos misturados não grava.
Onde o resultado precisa voltar idêntico, o formato é o `.rds`.

O nó grava em TODA execução do fluxo, mesmo que nada tenha mudado. Isso é
deliberado: um gravador cujo arquivo foi apagado não fez o trabalho dele, e sem
isso apagar o arquivo e recalcular não o recriava. A consequência é que o que
estiver ligado DEPOIS de um gravador recalcula sempre, porque a saída dele muda
de chave a cada passada. Na prática, o gravador costuma ser a última caixa da
cadeia.

Caminho em branco é nó desligado: não grava nada, não reclama, e a tabela
continua passando — nem o pacote `arrow` é exigido nesse caso, porque não há
gravação a fazer. Com caminho preenchido e sem o `arrow` instalado, o nó falha
logo de saída dizendo o comando de instalação.

## Parâmetros

- **Arquivo** — caminho do `.parquet`, relativo à pasta do projeto. Em branco,
  o nó fica desligado.

## Valor

A mesma tabela que entrou, intacta, para quem estiver ligado adiante.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/read_csv\", path = \"vendas.csv\") |>
  tr_add(\"limpo\", \"data/drop_na\", cols = \"valor\", from = \"ler\") |>
  tr_add(\"grava\", \"data/write_parquet\", path = \"saida/vendas.parquet\",
         from = \"limpo\")
```

## Veja também

`data/read_parquet` lê de volta; `data/write_rds` quando a tabela precisa voltar
idêntica; `data/write_csv` quando o arquivo será aberto num editor de
planilha.")

    )
  )
}
