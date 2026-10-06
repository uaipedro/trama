# Desenvolvimento e referência

Material técnico: construção de fluxos em R, criação de coleções, arquitetura, regiões de fluxo, estado do projeto e instruções de desenvolvimento.

## Construção de fluxos em R

`tr_flow()`, `tr_add()` e `tr_link()` produzem o mesmo documento gerado pelo editor. `tr_flow_code()` converte o documento para essa DSL; `tr_export_code()` gera um script R comum, com chamadas diretas às funções das coleções, ou um documento Quarto. No editor, as opções **Exportar R** e **Exportar Quarto** ficam no menu **Mais ações**.

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

Quando o processo é encerrado sem produzir um erro, `tr_retry()` retorna `FALSE`, e a execução seguinte retoma diretamente o checkpoint. O desenho completo consta da seção 5.8 de [design-trama.md](design-trama.md).

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
| DSL em R, exportação R comum e Quarto | ✅ |
| Dependências da interface disponíveis para uso offline | ✅ |

Foram verificados no navegador a montagem de fluxos, a edição de parâmetros, a recomputação seletiva, o descarte de edições superadas, a execução em pool e o funcionamento da interface sem acesso à rede.

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

