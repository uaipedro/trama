# trama

![Identidade visual do trama: R em blocos, diagramas que rodam](tools/video/out/ab_full.png)

O `trama` é um pacote R para construir e executar fluxos de computação em um editor visual interativo. Cada bloco exibe o próprio resultado e somente os blocos afetados são recalculados após uma alteração.

Os fluxos são documentos JSON que também podem ser escritos em R. As operações são fornecidas por coleções instaláveis.

As coleções abrangem manipulação de dados, gráficos, séries temporais, modelos estatísticos, análise multivariada, amostragem e aprendizado de máquina.

> **In English.** `trama` is an R package for building and running computation graphs in an interactive node editor based on Shiny and React. Flows are stored as JSON and can also be written in R. Documentation is in Portuguese.

## Instalação

O pacote requer R 4.1 ou versão posterior. Enquanto não estiver disponível no CRAN, a instalação pode ser feita pelo GitHub com o [`pak`](https://pak.r-lib.org):

```r
install.packages("pak")

# Núcleo e coleções recomendadas para iniciar
pak::pak(c(
  "uaipedro/trama",
  "uaipedro/trama/collections/trama.data",
  "uaipedro/trama/collections/trama.view"
))
```

As demais coleções são opcionais e instalam suas próprias dependências:

```r
pak::pak("uaipedro/trama/collections/trama.series")    # séries temporais
pak::pak("uaipedro/trama/collections/trama.models")    # modelos estatísticos
pak::pak("uaipedro/trama/collections/trama.multi")     # análise multivariada
pak::pak("uaipedro/trama/collections/trama.sampling")  # amostragem
pak::pak("uaipedro/trama/collections/trama.ml")        # aprendizado de máquina
```

Uma versão específica pode ser fixada pela tag, como em `"uaipedro/trama@v0.1.0"`.

Com o `remotes`, os pacotes devem ser instalados na ordem das dependências: núcleo, `trama.data`, `trama.view` e, em seguida, as demais coleções.

```r
remotes::install_github("uaipedro/trama")
remotes::install_github("uaipedro/trama", subdir = "collections/trama.data")
remotes::install_github("uaipedro/trama", subdir = "collections/trama.view")
```

## Primeiro fluxo

O código a seguir cria a pasta do projeto e abre o editor no navegador:

```r
library(trama)

tr_app(tr_project(
  "meu-projeto",
  collections = c("trama.data", "trama.view")
))
```

No editor, os blocos são arrastados da paleta, conectados pelas portas e configurados pelos parâmetros. Cada card apresenta o resultado da respectiva operação.

O fluxo é salvo em `meu-projeto/flows/main.json`.

## Conceitos centrais

Um fluxo organiza funções em um diagrama de blocos conectados. Alterações em parâmetros, blocos ou conexões recalculam somente os resultados que dependem da parte modificada.

Cada bloco declara entradas, saídas e parâmetros. As portas possuem tipo, obrigatoriedade e multiplicidade, o que permite validar as conexões antes da execução.

Os blocos são agrupados em **coleções**, que são pacotes R responsáveis por registrar tipos, operações e formas de apresentação dos resultados.

O editor visual, o documento JSON e a DSL em R representam o mesmo fluxo.

![O mesmo fluxo representado no editor visual, em JSON e em R](tools/video/out/v2_740.png)

O objetivo do projeto é permitir a construção, a inspeção e a execução de fluxos de funções e transformações. Os resultados intermediários permanecem visíveis, e novas operações podem ser acrescentadas como funções R comuns.

O [manifesto](docs/manifesto.md) registra o objetivo completo, o horizonte de desenvolvimento e os não objetivos do projeto.

## Coleções disponíveis

![Coleções de blocos disponíveis no trama](tools/video/out/leg_4.9.png)

As coleções separam a infraestrutura de execução dos diferentes domínios de análise:

| Coleção | Conteúdo |
|---|---|
| `trama.data` | Importação, inspeção, limpeza, transformação, agregação e gravação de dados |
| `trama.view` | Gráficos de relação, distribuição e comparação com `ggplot2` |
| `trama.series` | Operadores, decomposição, modelos, testes e gráficos para séries temporais |
| `trama.models` | Modelos estatísticos e diagnósticos |
| `trama.multi` | Análise multivariada |
| `trama.sampling` | Procedimentos de amostragem |
| `trama.ml` | Classificação, regressão, avaliação e regras interpretáveis |

`trama.data` é a coleção de referência. Seus blocos abrangem fontes, inspeção, limpeza, transformação, reformatação, agregação, regiões de fluxo e gravação.

`trama.view` fornece gráficos em `ggplot2` sem JavaScript próprio. O `preview` do tipo `view/plot` grava um PNG, exibido pelo renderizador de imagens do núcleo.

`trama.series` reúne operadores, decomposição clássica e STL, ARIMA, ETS, Holt-Winters, testes e gráficos específicos. Adaptadores convertem `series/ts` em `data/table` nas arestas, sem bloco intermediário.

A coleção [`trama.ml`](collections/trama.ml/README.md) inclui CART, FIGS, random forest, SVM e XGBoost, além de referências lineares e logísticas, divisão treino/teste, avaliação e extração de regras.

Os motores de aprendizado de máquina são opcionais. A coleção possui [fluxos de classificação e regressão](exemplos/machine-learning) e uma lista de [referências dos métodos e pacotes](collections/trama.ml/REFERENCES.md).

## Construção de fluxos em R

`tr_flow()`, `tr_add()` e `tr_link()` produzem o mesmo documento gerado pelo editor. `tr_flow_code()` realiza a conversão do documento para código R.

O fluxo de ETL em `exemplos/vendas` pode ser escrito da seguinte forma:

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("ler", "data/read_csv", path = "vendas.csv") |>
  tr_add("filtrar", "data/filter", expr = "valor > 180", from = "ler") |>
  tr_add("total", "data/mutate", name = "receita", expr = "valor * qtd", from = "filtrar") |>
  tr_add("porregiao", "data/group_summarise", by = "regiao", name = "receita_total", expr = "sum(receita)", from = "total") |>
  tr_add("porproduto", "data/group_summarise", by = "produto", name = "receita_total", expr = "sum(receita)", from = "total") |>
  tr_add("ranking", "data/arrange", cols = "receita_total", desc = TRUE, from = "porregiao")
```

O registro criado por `tr_registry()` armazena os blocos das coleções carregadas por `tr_use()`. `tr_app()` e `tr_project()` criam esse registro automaticamente para o editor.

O argumento `from` liga a primeira saída à primeira entrada obrigatória, livre e compatível. `tr_link()` atende às demais ligações, e `tr_set()` altera parâmetros após a criação do bloco.

`tr_set()` também define parâmetros cujos nomes coincidem com argumentos de `tr_add()`: `flow`, `id`, `type`, `from`, `label`, `seed` e `position`. Os quatro primeiros são recusados por `tr_add()` para evitar ambiguidade.

## Criação de coleções e blocos

Uma coleção registra tipos, categorias, blocos, renderizadores, editores de parâmetros e fluxos de exemplo. Sua implementação utiliza R e, quando necessário, arquivos JavaScript e CSS, sem bundler obrigatório.

```r
tr_node(
  "data/filter",
  fn = nd_filter,
  label = "Filtrar",
  description = "Mantém as linhas em que a condição é verdadeira.",
  help = "## Descrição\n\nCondição em branco é nó desligado…",
  inputs = list(data = "data/table"),
  outputs = list(out = "data/table"),
  params = list(
    expr = tr_param("expr", "", label = "Condição", example = "valor > 100")
  )
)
```

`description` é obrigatória e descreve a finalidade do bloco. `help` fornece o conteúdo do painel lateral, e `example` define o texto de exemplo do campo.

Um renderizador pode fornecer uma ou mais vistas para o mesmo artefato:

```js
import { registerRenderer, registerWidget } from "trama";

registerRenderer("data/table", MinhaTabela);

registerRenderer("data/table", { views: [
  { id: "tabela",  label: "tabela",  component: MinhaTabela },
  { id: "colunas", label: "colunas", component: MinhasColunas },
]});
```

As vistas são alternadas no card e recebem `{artifact, handle, assetUrl}`. Todo tipo que declara `summary` em `tr_type()` recebe automaticamente a vista `resumo`.

O worker chama a função do bloco com entradas e parâmetros nomeados. Quando declarados, `.ctx` fornece `progress()`, `partial()`, `path()` e `file()`, e `.seed` recebe a semente estável da instância.

Os contratos completos estão documentados em `?tr_collection`, `?tr_type` e `?tr_node`. `tr_errors()` lista os erros classificados do núcleo; cada coleção pode manter um catálogo equivalente para seu domínio.

## Arquitetura de execução

```text
comando → documento → plano (hash por nó) → fila → workers
                                                     ↓
                                store: <hash>.artefato + <hash>.preview
                                                     ↓
                                              eventos → front
```

O armazenamento endereçado por conteúdo constitui o protocolo entre os processos. O worker grava o artefato e o `preview` sob uma chave e devolve somente o identificador correspondente.

Esse protocolo permite executar funções fora do processo da interface sem transferir objetos R ativos. O núcleo é formado por cinco contratos: tipo, catálogo, documento, execução e renderização.

## Regiões de fluxo

Algoritmos iterativos podem ser executados em uma **região de fluxo**, na qual um subgrafo processa os dados em passos sucessivos. Esse mecanismo atende, por exemplo, a modelos atualizados a cada observação.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)
tr_use("trama.view", registry = reg)
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "data/read_csv", path = "serie.csv") |>
  tr_add("entra", "data/to_stream", lote = 1L, from = "dados") |>
  tr_add("modelo", "models/rls", resposta = "y", from = "entra") |>
  tr_add("sai", "data/from_stream", from = "modelo")
```

`data/to_stream` inicia a região, e `data/from_stream` a encerra. Blocos comuns são aplicados a cada passo; blocos com estado declaram `init` e `step`.

O resultado é um histórico tabular com uma linha por observação. Como a saída utiliza um tipo comum, os blocos de visualização e transformação podem ser conectados sem tratamento específico.

Durante a execução, os cards apresentam o passo atual. Pausa, avanço e velocidade são enviados pelo armazenamento com `tr_stream_command()` e funcionam nos executores sequencial e em pool.

No executor sequencial, a interface Shiny e a região ocupam o mesmo processo. Os controles interativos exigem uma segunda sessão ou um executor em pool enquanto o laço estiver em execução.

A chave usada por `tr_stream_command()` corresponde à unidade retornada por `tr_plan(...)$units[["<nó do colapso>"]]$key`. `tr_plan_keys()` retorna chaves de artefatos e não atende a esse comando.

Execuções interrompidas mantêm um checkpoint. Após uma falha registrada, `tr_retry()` remove o erro e preserva o checkpoint; a retomada ocorre no próximo `tr_run()` ou `tr_value()`.

Quando o processo é encerrado sem produzir um erro, `tr_retry()` retorna `FALSE`, e a execução seguinte retoma diretamente o checkpoint. O desenho completo consta da seção 5.8 de [design-trama.md](docs/design-trama.md).

## Estado do projeto

O projeto está em desenvolvimento e possui execução completa para fluxos tabulares.

| Componente | Estado |
|---|---|
| Registro, tipos, blocos, parâmetros e catálogo | ✅ |
| Documento, operações, identidade e JSON | ✅ |
| Armazenamento, plano e chaves de conteúdo | ✅ |
| Executores sequencial e em pool; erro como valor | ✅ |
| Transporte Shiny e interface | ✅ |
| Coleção `trama.data` e fluxo de ETL de aceite | ✅ |
| Coordenador assíncrono, cancelamento, progresso e resultados parciais | ✅ |
| Pool com `mirai` e cancelamento classificado | ✅ |
| DSL em R e conversão por `tr_flow_code()` | ✅ |
| Dependências da interface disponíveis para uso offline | ✅ |

Foram verificados no navegador a montagem de fluxos, a edição de parâmetros, a recomputação seletiva, o descarte de edições superadas, a execução em pool e o funcionamento da interface sem acesso à rede.

## Projetos de exemplo

Os projetos em `exemplos/` podem ser executados a partir de um clone do repositório. O núcleo e as coleções são carregados do código-fonte com `pkgload`.

```sh
git clone https://github.com/uaipedro/trama.git
cd trama

cd exemplos/vendas && Rscript -e 'shiny::runApp("app.R")'
cd exemplos/branco && Rscript -e 'shiny::runApp("app.R")'
cd exemplos/series && Rscript -e 'shiny::runApp("app.R")'
cd exemplos/experimentos && Rscript -e 'shiny::runApp("app.R")'
```

Os exemplos correspondem, respectivamente, a um ETL completo, um projeto vazio, uma análise de séries temporais e delineamentos agronômicos.

## Desenvolvimento

Os testes do núcleo são executados sem coleções de domínio:

```r
pkgload::load_all(".")
testthat::test_dir("tests/testthat")
```

Cada coleção mantém sua própria suíte. Para `trama.data`:

```r
pkgload::load_all(".")
pkgload::load_all("collections/trama.data")
testthat::test_dir("collections/trama.data/tests/testthat")
```

A ordem de carga segue as dependências. `trama.view` depende de `trama.data`; `trama.series`, `trama.multi` e `trama.models` dependem de ambas.

As regras puras da interface possuem testes em Node:

```sh
node --test 'tests/js/*.test.mjs'
```

O arquivo `trama.json` pode definir `temas` e `tema_padrao` para gráficos. Na ausência dessas opções, são usados os temas `escuro`, `claro` e `clássico`.

O mesmo arquivo pode definir `marca`, que controla a inclusão do símbolo do `trama` nos frames exportados. As opções também podem ser alteradas no editor ou por `tr_project_set_themes()` e `tr_project_set_marca()`.

## Documentação complementar

- [Manifesto](docs/manifesto.md): objetivo, horizonte de desenvolvimento e não objetivos.
- [Documento de projeto](docs/design-trama.md): arquitetura e decisões de fundação.
- [Guia de documentação](docs/guia-documentacao.md): convenções para a documentação do projeto.

## Licença

O `trama` é distribuído sob a licença MIT. As dependências da interface também utilizam essa licença.

No código R, `htmltools` utiliza GPL e integra a pilha do Shiny. Essa condição está registrada no [manifesto](docs/manifesto.md#licença-e-abertura).
