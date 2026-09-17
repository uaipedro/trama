# trama — design alvo

Arquitetura de destino. "Insumo" é a prova de conceito (world-foundry) de onde
o trama saiu.

> **Proveniência.** Este design tem duas origens: o desenho candidato feito a
> partir da PoC (world-foundry v0.4) e um desenho independente encomendado a
> outro modelo, sem acesso ao primeiro. As duas fundações convergiram em tudo
> o que importa (§9). Onde o desenho independente corrigiu o candidato, está
> marcado **[ind]**; onde foi rejeitado, está em §10 com o motivo.

---

## 1. O que o trama é

Framework R para **computação em grafo com visualização interativa**: nós são
funções tipadas com portas e params; o grafo é um documento; a execução é
pull-based, endereçada por conteúdo e incremental; cada nó renderiza o próprio
resultado no canvas.

**Zero domínio no núcleo.** Dados/ETL, terreno, séries temporais, ML — todos
são coleções.

---

## 2. Tese

O motor nunca vê um valor. Ele vê **chaves de conteúdo** e **handles**; os
valores vivem num store em disco, produzidos e lidos por workers descartáveis.

Isso não é otimização — é o que faz o processo do Shiny virar um coordenador
que só manipula documento, plano e eventos. Sessão travada, ausência de
cancelamento, preview no processo errado e valores que não serializam deixam
de ser problemas separados: são todos a mesma consequência de o coordenador
carregar valor.

---

## 3. Os cinco contratos

Tudo que não é um destes é coleção, ou é v2.

1. **Tipo** — identidade, persistência, preview, compatibilidade.
2. **Catálogo** — o que existe e como se declara.
3. **Documento** — o grafo, e as ops que o mudam.
4. **Execução** — plano, chaves, fila, status, cancelamento. A região de fluxo
   (5.8) mora AQUI: é um modo de execução, não um contrato a mais.
5. **Renderização** — como um valor vira pixel/SVG/tabela.

---

## 4. Documento e ciclo de edição

### 4.1 Fonte da verdade

O servidor guarda o documento (imutável; cada op devolve um documento novo).
O front é uma view que **emite ops**, nunca estados. Some o "empurra o grafo
inteiro e o servidor diffa pra saber se mudou algo".

### 4.2 Semântica e apresentação no mesmo arquivo

```json
{
  "format": 1,
  "collections": {"data": ">=0.1"},
  "nodes": {
    "01HX2": {"type": "data/read_csv", "type_version": 1,
              "label": "Vendas", "params": {"path": "vendas.csv"},
              "seed": 8123441}
  },
  "edges": [{"from": ["01HX2","out"], "to": ["01HX7","data"], "index": 0}],
  "ui": {"positions": {"01HX2": [120, 80]},
         "sizes": {"01HX2": [480, 300]},
         "views": {"01HX2": "resumo"},
         "frames": {"01HY0": {"x": 0, "y": 0, "w": 1280, "h": 720,
                              "title": "Leitura", "aspect": "16:9",
                              "color": "azul", "order": 1}},
         "folds": {"01HX2": {"preview": false}}}
}
```

**Semântica** (`nodes`, `edges`, `seed`) entra na chave de cache.
**Apresentação** (`ui`) nunca entra — arrastar, redimensionar ou trocar a vista
de um card não dispara nada. `sizes` é `[largura do card, altura do preview]`;
`views` é o **id** da vista escolhida, nunca um índice — índice quebra em
silêncio quando a coleção reordena as vistas.

`frames` são retângulos de apresentação, com título, proporção e ordem de
slide. Não são subgrafo nem bloco de execução: o pertencimento de um card a um
frame é **geométrico** (card inteiro dentro do retângulo), calculado pelo
editor no começo de cada arrasto e nunca gravado. O documento continua plano,
com posições absolutas. `folds` guarda as partes recolhidas de cada card
(`preview`, `params`); chave ausente é aberto.

### 4.3 Ops e revisão **[ind]**

Ops: `add_node`, `remove_node`, `set_param`, `set_seed`, `connect`,
`disconnect`, `rename`, `move`, `resize`, `set_view`, `add_frame`,
`update_frame`, `remove_frame`, `reorder_frames`, `set_fold`, `batch`, `group`,
`ungroup`, `set_target`. (`group`, `ungroup` e `set_target` são vocabulário
pretendido, ainda não implementados.)

As cosméticas estão ENUMERADAS em `.tr_presentation_ops` (`R/document.R`), e
`tr_op_semantic()` deriva dali quem recomputa. Enumerar as cosméticas, e não as
semânticas, é a direção segura de errar: esquecer uma op nova custa "recomputou
à toa", barulhento e inofensivo, em vez de preview parado sem erro nenhum.

`batch` agrupa as ops de um gesto (mover N cards, apagar uma seleção, recolher
em massa) numa op só: atômica, uma revisão, um passo de undo, semântica se
qualquer op de dentro for. Não é só conveniência. Mandadas soltas, a segunda op
já sai com `base_rev` velho, porque o cliente só avança a revisão no eco, e o
servidor a recusaria. `group`/`ungroup` continuam sendo vocabulário pretendido
para **subgrafo** (semântico), e não têm relação com frames.

A linha entre os dois eixos passa pelo **conteúdo do artefato**, não por parecer
ajuste de aparência. Proporção de imagem é **param** — semântica, entra na chave
de cache, muda o PNG que fica gravado. Tamanho do card é **`ui.sizes`**, escrito
pela op cosmética `resize` e fora da chave. Um preview de imagem que dependesse
do tamanho do card recomputaria a cada quadro de arrasto da alça: os dois eixos
não podem se cruzar. `view/plot` (§7.3) é o caso concreto — `aspecto` é param, e
o que sobra ou falta entre a proporção do PNG e a caixa do card é absorvido por
`object-fit: contain`, não por um render novo.

Cada op viaja como `{seq, base_rev, op}`. O servidor rejeita se
`base_rev` != revisão atual (raro, sessão é single-user), se o envelope vem
malformado ou se a op não se aplica ao documento; senão aplica, valida,
incrementa `rev`, e devolve o próprio op aplicado como eco. **Toda** recusa vem
seguida do documento inteiro, não só a de revisão defasada: o front aplica
alguns gestos otimisticamente (apagar em lote some com os cards antes do eco), e
um batch recusado deixaria a tela descrevendo um documento que não mudou. Op
estrutural aplicada também devolve o documento junto do eco — criar/apagar nó,
ligar/desligar, criar/apagar/reordenar frame, e `batch` com qualquer uma delas
dentro (`.tr_doc_echo_ops` em `R/document.R`) —, pra o front não reimplementar a
semântica de cada op só pra desenhar o resultado. Param, posição, tamanho e
recolhimento ficam só no eco: o gesto já atualizou a tela.

Undo é um log de ops do **servidor**, por sessão: só cresce com op aplicada,
na ordem em que foi aplicada, e desfazer reaplica o log sem a última op sobre o
documento como estava quando a sessão começou (não sobre o vazio). A revisão
não volta: o undo avança a `rev` como qualquer mudança, porque uma rev já
emitida com outro conteúdo deixaria passar op pensada pra um documento que não
existe mais. O cliente só pede (`tr_undo`); não guarda log. (Redo ainda não
existe.)

Não existe debounce de documento — debounce vive no widget, antes de emitir
`set_param`. O widget `text` embutido comita no **blur** (e no Enter), não a
cada tecla: op de param entra na chave de cache e dispara recomputação, então
digitar um título de vinte caracteres mandava vinte ops.
Barato enquanto o nó filtra linhas; com um nó que renderiza PNG, são vinte PNGs.
Corrida entre mensagens vira **erro explícito de `base_rev`** em vez de estado
corrompido.

**Armadilha do Shiny [ind]:** `input$x` ignora valores idênticos consecutivos.
Por isso todo op carrega `seq` — sem ele, dois ops iguais em sequência somem.
Um único `registerInputHandler` pra cima, um único `sendCustomMessage` pra
baixo: Shiny é barramento, não lógica.

---

## 5. Execução

### 5.1 Três papéis, dois processos

| Papel | Onde | Faz |
|---|---|---|
| Coordenador | processo Shiny, ou o script em headless | documento, plano, chaves, scheduler, eventos |
| Worker | daemons `mirai` (N processos) | executa **uma unidade**: lê inputs do store, roda `fn`, grava output + preview |
| Store | diretório do projeto | valores e previews endereçados por chave |

O coordenador nunca carrega um valor pesado. "Como o valor atravessa a
fronteira?" — **não atravessa.** Atravessa um handle: chave, tipo, tamanho,
caminho do preview, resumo.

### 5.2 Chave de conteúdo

```
key(nó) = hash(type_id, type_version, fn_fingerprint,
               params semânticos, seed efetiva,
               key(input_1..n),
               fingerprint externo)   # só nós impuros
```

A chave sai do **plano**, não da execução — cache hit é decidido no
coordenador sem acordar worker.

### 5.3 `fn_fingerprint` — o furo da PoC **[ind]**

`hash(body(fn))` **não é suficiente**, e isso é um bug presente no insumo, não
hipótese: `nodes/viewer.R` define `.wm_categorical_palette()` no nível do
arquivo e chama de dentro do `fn`. Mudar a paleta não muda `body(fn)` — o
cache serve imagem velha, em silêncio.

O fingerprint precisa cobrir o fecho real: corpo, formals, e os objetos do
namespace da coleção que a função referencia (via `codetools::findGlobals()`,
resolvidos no namespace, transitivamente e com limite de profundidade).
Fallback: versão da coleção. Modo dev: `tr_bust("data")` invalida por prefixo.

Aceitar explicitamente: hot reload perfeito não existe sem análise estática, e
a versão da coleção como *única* chave é ruim — um patch release invalidaria
cache de nó que não mudou.

### 5.4 Scheduler

- Plano = fecho transitivo dos alvos. Pull-based.
- Unidade = um nó: `run_unit(spec, input_keys) -> handle`. Unidades sem
  dependência pendente vão pros daemons; paralelismo entre ramos é de graça.
- **Unidade superada é cancelada [ind]:** cada edição gera plano novo;
  unidades em voo cuja chave não está mais no plano morrem. Digitar `4`, `40`,
  `400` gera três chaves e só a última sobrevive. **Isso substitui o debounce
  de recomputação.**

Implementado em `R/scheduler.R` (`tr_scheduler()`, `step()`/`handoff()`).

### 5.5 Cancelamento **[ind]**

Cancelamento cooperativo em R arbitrário (e no C++ do `terra`) não existe.
Então cancelar = **matar o daemon** e relançar.

Isso só é honesto porque o worker é **sem estado**: o que ele produziu de útil
já está no store sob a chave certa, e o que não terminou não deixa rastro —
**escrita em arquivo temporário + rename atômico**. O contrato é: projete para
o worker morrer a qualquer momento.

Headless: mesmo scheduler, mesmo store, executor sequencial no mesmo processo.
Zero ramificação de código entre UI e script.

Implementado em `R/scheduler.R` (`handoff()`) e `R/executor.R`
(`tr_executor_pool()$cancel`, via `mirai::stop_mirai()`).

### 5.6 Progresso e parciais **[ind]**

Para progresso intra-nó (treino, iteração) o worker publica num canal lateral:

```r
fn <- function(x, epochs, .ctx) {
  for (e in seq_len(epochs)) {
    .ctx$progress(e / epochs, msg = sprintf("época %d", e))
    if (e %% 10 == 0) .ctx$partial(modelo)
  }
}
```

`.ctx$partial()` grava um handle parcial com preview; o card mostra; o handle
final substitui. Nó que não usa `.ctx` não paga nada.

Este era o **único** mecanismo de streaming, e a versão anterior desta seção
dizia que "não existe porta que emite N valores ao longo do tempo". Existe
desde a região de fluxo (5.8): o parcial por nó daqui é o canal que ela reusa
para mostrar cada passo, e a porta que emite N valores é a `stream = TRUE`.

Implementado em `R/worker.R` (`.tr_make_ctx()`) e `R/scheduler.R` (`collect()`,
que lê `tr_progress()` e emite `progress`/`partial` como evento).

### 5.7 Erro como valor **[ind]**

Nó que falha produz um **handle de erro** no store. O jusante mostra
"bloqueado por X" sem executar. Sem isso, um erro numa ponta reexecuta o mundo
a cada edição.

A condição atravessa a fronteira de processo serializada, com classe e
traceback preservados.

**Armadilha: validar dentro de um verbo do dplyr perde a classe do erro.** O
dplyr captura a condição levantada dentro de `mutate()`/`filter()`/`summarise()`
e a reembrulha num `rlang_error` novo; a classe original sobrevive só no
`parent`. Como o motor grava `class(e)[1]` no handle de erro, o card recebe
`"rlang_error"` e a classificação some. Pior: `expect_error(class = )` do
testthat percorre a cadeia de pais, então o teste passa verde enquanto o
usuário vê o erro genérico. **Valide antes de entrar no verbo**, e asserte
`class(e)[[1]]` nos testes, não `class = `.

**Armadilha: catálogo de erros conferido por grep precisa distinguir uso de
declaração.** `tr_errors()` e `tr_data_errors()` são testados nas duas direções
— nada indocumentado, nada morto. A segunda direção é vacuosa se o grep varrer
também o arquivo do catálogo, onde toda classe aparece como nome do `c()`: o
teste se satisfaz sozinho e passaria com o pacote inteiro apagado. Conte só
ocorrência de uso — entre aspas, no `abort()`.

---

### 5.8 Região de fluxo

Um sub-grafo que se desenrola ponto a ponto: fonte finita, nós que processam
cada ponto, colapso que junta o histórico. Serve ao algoritmo cujo valor está
no caminho e não no resultado — treino incremental, detecção de drift, um
ordenador mostrando as trocas.

**Não é um sexto contrato.** É um MODO DE EXECUÇÃO dentro do contrato de
execução que já existia. O plano detecta a região (fecho transitivo a partir das
portas `stream = TRUE`) e emite **uma unidade**, com chave determinística sobre
todos os nós dela. Do ponto de vista do executor, do store e do GC, é uma
unidade como qualquer outra: entra chave, sai chave. Por dentro, um driver
percorre a ordem topológica em lockstep.

Dois tipos de nó, e a distinção é a única coisa que exigiu contrato novo:

| | o que faz | o que declara |
|---|---|---|
| sem memória | um ponto entra, um ponto sai | nada — é nó comum, ELEVADO ponto a ponto |
| com memória | guarda estado entre pontos | `init` + `step`, com `step` devolvendo `list(state =, out =)` |

A elevação automática é o que faz `data/filter` e `data/mutate` funcionarem
dentro de uma região sem mudar uma linha: `map` e `reduce` ficam separados, e
nó puro não precisa saber que fluxo existe.

O artefato é o **histórico**, produzido pelo colapso — e é ele, não o driver,
que sabe juntar N pontos de um tipo, porque juntar é conhecimento de domínio
(`rbind` para tabela, `c()` para vetor, mosaico para raster). Por isso a região
colapsa de volta em dado trama comum: terminou o run, os nós de gráfico que já
existem plotam a curva.

Pausa, um passo, velocidade (`tempo`) e parar são **comandos**, não ops de
documento: viajam por `<store>/stream/<chave>/control.json`, escritos por
`tr_stream_command()` e lidos pelo driver entre passos. Nenhum deles entra na
chave — mudar a velocidade não pode recomputar dez mil pontos (é a razão de
nenhum ser param).

Os outros dois botões de operação NÃO vão por ali, e a distinção é deliberada:
`publish_every` (de quantos em quantos segundos se publica parcial) e
`checkpoint_every` (de quantos em quantos passos se grava checkpoint) são
**ajuste do run**, fixados quando ele começa, e descem pelo argumento
`ctx_extra` de `tr_run()`/`tr_value()`/`tr_server()`. O `control.json` carrega
gesto AO VIVO — alguém clicando enquanto a região roda —, e juntar os dois
faria um arrasto no controle de velocidade reescrever a política de
durabilidade. Consequência conhecida: `tr_app()` não repassa `ctx_extra`, então
pelo caminho da interface esses dois ficam no default. Checkpoint em
lote no mesmo diretório, e `tr_retry()` é o gesto que limpa o handle de erro
preservando o checkpoint, porque `tr_bust()` apaga os dois de propósito.

**Um gap de interface, conhecido:** as cinco recusas voltam de
`tr_doc_validate()` como `problems`, e o front hoje só as renderiza como
**banner do documento**, não como marca no card — porque marca de card vem de
evento de run, e um `data/to_stream` solto aborta o plano antes de existir
unidade. Consequência medida: o card fica indistinguível de um nó sadio, e o
banner nomeia o nó pelo id gerado, que nenhum card mostra — com dois cards de
mesmo rótulo não se sabe qual apagar. A sessão sobrevive, o documento reabre e o
texto do banner diz o que fazer; o que falta é a marca no lugar certo.

**Dois gaps conhecidos e aceitos:**

1. **Nós da região não paralelizam entre si.** E a razão é de implementação, não
   de natureza — a primeira versão desta linha dizia "sequencial por natureza" e
   isso é falso na maioria dos casos. Com um nó COM MEMÓRIA a dependência é real
   (o ponto *n* precisa do estado em *n-1*), mas região sem nó com memória é o
   caso comum — `models/rls` é o único `online` do repositório inteiro —, e ali
   cada ponto é independente de todos os outros. Mesmo com memória, dois ramos
   elevados independentes DENTRO de um ponto poderiam rodar concorrentes. O
   driver percorre um ponto por vez porque ninguém pagou para ele fazer
   diferente, e é isso que esta linha registra.
2. **A região reexecuta inteira quando qualquer nó dela muda.** A chave é sobre
   todos os nós, então editar um param de um membro invalida o histórico todo.
   O checkpoint mitiga a morte no meio, não a edição.

Implementado em `R/stream-region.R` (detecção e as cinco recusas),
`R/plan.R` (`resolve_region`, `region_of`), `R/hash.R` (`.tr_region_key`),
`R/stream-driver.R` (o driver e os contratos de fonte e colapso),
`R/stream-control.R` (os comandos) e `R/run.R` (`tr_retry`). A fronteira é
`data/to_stream`/`data/from_stream`, em `trama.data`.

## 6. Tipos

**Nominal, com adaptadores explícitos.** Sem genéricos, sem união, sem
estrutural, sem `Any`.

```r
tr_type("data/table",
  version = 1,
  store   = function(x, path) arrow::write_parquet(x, path),
  restore = function(path) arrow::read_parquet(path),
  ext     = "parquet",
  preview = function(x, ctx) tr_preview("data/table", rows = utils::head(x, 50)),
  summary = function(x) list(nrow = nrow(x), ncol = ncol(x))
)
```

**O tipo carrega a persistência [ind].** É aqui que "`SpatRaster` não sobrevive
a `saveRDS`" se resolve — não no cache. E é por isso que dispatch por classe R
não serve: `Heightmap` e `Field` são os dois `SpatRaster`, e foi exatamente
isso que gerou o nó identidade `height_as_field` no insumo.

**Compatibilidade:** `A == B`, ou existe `tr_adapter("A", "B", fn)`. O
adaptador é inserido pelo motor **na aresta** (marca visual, não nó) e entra
na chave. Mata o nó identidade e mantém o motor cego.

**Portas de fronteira de bloco não têm tipo próprio [ind]:** herdam o tipo da
porta interna que expõem. Some o coringa `Field` do insumo — e some a
necessidade de `Any`, que o design candidato propunha.

**Portas variádicas** (`multiple = TRUE`) entram desde o início: afetam o
schema da aresta (`index`), e "combinar N camadas" é onipresente.

Por que não mais: lattice de subtipos ou genéricos obrigam inferência no
motor e no front, e o front deixa de ser burro. Adaptadores dão 90% do valor e
são funções testáveis. Compatibilidade é **uma função** consultada num lugar
só, então dá pra crescer depois sem refazer nada.

---

## 7. Como um domínio se pluga

### 7.1 Coleção = pacote R

```r
trama_collection <- function() tr_collection(
  id = "data", version = "0.1.0",
  types = list(...), nodes = list(...), adapters = list(...),
  js  = "trama/index.js",    # em inst/, opcional
  css = "trama/data.css"     # em inst/, opcional
)
```

`css` é a folha de estilo das classes que os renderers da coleção usam. Entra
na página **depois** do `trama.css` do núcleo — regra de mesma especificidade
na coleção ganha, então ela pode refinar o núcleo (e por isso deve prefixar as
próprias classes). Sem `css =`, um `.css` em `inst/` é copiado pro servidor mas
nunca linkado: o estilo simplesmente não aplica.

`tr_use("data")` carrega pelo namespace. Registro num environment **dentro do
namespace do trama**, nunca `globalenv()` — e o registro é um objeto passado
ao projeto, então testes criam registries isolados.

### 7.2 Nó = função R + metadados

```r
select_cols <- function(data, cols) dplyr::select(data, dplyr::all_of(cols))

tr_node("data/select", fn = select_cols, version = 1,
  description = "Mantém apenas as colunas escolhidas.",
  help    = "## Descrição\n\nColunas em branco é nó desligado…",
  inputs  = list(data = "data/table"),
  outputs = list(out  = "data/table"),
  params  = list(cols = tr_param("cols", "", example = "regiao, valor")))
```

A função é a função — chamável no console. **`tr_test_collection()` gera
automaticamente, pra cada nó, o teste "console == grafo" [ind]**, então
terceiros ganham a garantia de graça.

### 7.2.1 Contrato de ajuda

- **`description` é obrigatória.** Uma linha, no imperativo, dizendo o que o nó
  faz. É o que aparece na paleta e no tooltip do card. Obrigar na declaração —
  mesma régua de `tr_param()` sem `default` — é o que impede uma coleção de
  nascer muda: documentação que é opcional não acontece, e o catálogo é lido
  por máquina também, então a ausência custa duas vezes.
- **`help` é opcional, markdown, e vira painel lateral.** O formato é o de uma
  página `?funcao`: Descrição / Parâmetros / Valor / Exemplos / Veja também. O
  renderer de markdown é do próprio front (umas 50 linhas em `editor.js`, só o
  subconjunto que as páginas usam, montando nós React em vez de `innerHTML`) —
  nenhuma dependência nova, a garantia offline continua.
- **`icon` é opcional, e tem duas portas.** `tr_icon("file-spreadsheet")` nomeia
  um ícone do conjunto Lucide vendorizado em `inst/www/vendor/lucide.svg`;
  `tr_icon(svg = "<path .../>")` entrega a geometria de um ícone próprio da
  coleção — o `<svg>` e o `viewBox` de 24×24 são do front, que é o que faz o
  ícone de terceiro herdar cor e tamanho como os do conjunto. O nome é
  conferido na DECLARAÇÃO contra os `<symbol id>` do sprite: `tr_icon("filter")`
  falha na carga da coleção dizendo o nome errado, em vez de virar um buraco no
  canto do card meses depois (o nome certo é `list-filter`). Ausente, o bloco
  cai numa bolinha da cor da categoria, na mesma calha — então uma coleção que
  não declara nenhum ícone continua alinhada, e adotar é incremental, nó a nó.
  Chame `tr_icon()` DENTRO de `trama_collection()`, nunca num objeto de topo do
  pacote: o segundo assa a validação no build da coleção e embarca o resultado
  contra o sprite daquele momento, em vez de conferir contra o instalado.
- **`example` por param não passa pelo núcleo.** `tr_param(kind, default,
  label, ...)` guarda o `...` na especificação e `.tr_json_param()` faz
  `unclass(p)`, então **qualquer campo extra viaja inteiro até o front**. O
  widget de texto embutido usa `example` como placeholder; um widget de coleção
  pode ler o campo que quiser. Acrescentar afordância de UI não exige mexer no
  núcleo — é o mesmo princípio do dispatch por id.

O que se ganha com isso não é só o usuário humano. O catálogo já dizia que nós
existem e que portas têm; agora diz **para que serve cada um e o que se digita
em cada campo** — que é a metade que faltava para um LLM montar fluxo válido a
partir do catálogo, sem ler o código da coleção.

**Armadilha: não dê a param o nome de um argumento formal de `tr_add()`.** São
sete: `flow`, `id` e `type`, posicionais obrigatórios, e `from`, `label`,
`seed` e `position`, opcionais. O param chega por `...`; um param chamado
`from` é casado com o formal de mesmo nome e vira **aresta**, calado — e se o
valor por acaso for o nome de um nó do fluxo, nada falha.

A guarda `.tr_check_param_shadow()` (`R/flow.R`) cobre **só os quatro
opcionais**: nesses sai `tr_error_param_shadow`, dizendo o que aconteceu e
mandando usar `tr_set()`. Nos posicionais o estrago é o mesmo e a mensagem é
pior, porque aponta longe da causa: em
`tr_add(f, "num", "data/convert", type = "numero")` o `type` casa com o formal
de `tr_add()`, `"data/convert"` sobra sem nome dentro do `...`, e o que se lê é
`Params de tr_add() precisam ser nomeados.` — verdade sobre o sintoma, silêncio
sobre a causa. O param `type` de `data/convert` é, portanto, inalcançável pelo
`tr_add()`; quem precisa dele usa `tr_set(f, "num", type = "numero")`. A saída
barata continua sendo escolher outro nome. Descoberto escrevendo a ajuda de
`data/rename` (param `from`) e `data/convert` (param `type`).

### 7.3 Visualização nova sem tocar na biblioteca

Dois estágios ligados por um **id de renderer**:

1. **Worker (R):** o `preview` do tipo produz um artefato —
   `list(renderer = "data/table", data = list(...), files = list(png = "..."))`.
   Arquivos vão pro store, servidos via `addResourcePath`.
2. **Front (JS):** o `index.js` da coleção registra:

```js
import { registerRenderer, registerWidget } from "trama";
registerRenderer("data/table", MinhaTabela);            // uma vista
registerRenderer("data/table", { views: [               // ou várias
  { id: "tabela",  label: "tabela",  component: MinhaTabela },
  { id: "colunas", label: "colunas", component: MinhasColunas },
]});
registerWidget("data/column-picker", (spec, value, onChange) => { ... });
```

Um renderer é um **conjunto de vistas do mesmo artefato**, alternáveis numa
faixa de abas no card. Cada vista recebe `{artifact, handle, assetUrl}` — o
mesmo de sempre: vistas são leituras diferentes do que **já** trafega, não
payloads paralelos. Uma vista que precise de dado novo é escolha explícita do
`preview` do tipo.

Todo tipo que declare `summary=` no `tr_type()` ganha de graça uma vista
`resumo`: o `summary` já é gravado no handle e já chega ao card. A coleção que
quiser um resumo melhor sobrescreve registrando uma vista com esse id.

E uma coleção pode ganhar visualização inteira **sem trazer JS nenhum**, se o
`preview` do tipo apontar para um renderer do núcleo. É o que a `trama.view`
faz: o `preview` de `view/plot` grava o PNG com `ragg` e devolve
`tr_preview("trama/image", files = list(png = p))` — a coleção não tem `inst/`
nem `js =`. O `trama/image` do núcleo abre a imagem em tela cheia num clique
(overlay por `createPortal` para o `<body>`, Esc ou clique fora fecha), então o
lightbox também sai de graça para qualquer coleção que produza imagem. A regra
"arquivo só quando o dado não cabe" continua valendo; um gráfico é o caso em que
ele não cabe, e reduzi-lo a dado seria redesenhar a gramática do ggplot2 no
navegador.

`files` chega à frente como **objeto com chave por nome** — `{"png": "..."}`,
não uma string nem um array. Quem garante isso é o `as.list()` de `R/store.R`:
`jsonlite::write_json(auto_unbox = TRUE)` desembrulha vetor atômico de
comprimento 1 e joga o nome fora, e o `Image` do `runtime.js` faz
`files.png || Object.values(files)[0]` — que sobre uma string devolve a
primeira letra, dando `src` 404 e card em branco sem erro em lugar nenhum.
Vetor nomeado o núcleo tolera; o contrato que se escreve é
`files = list(png = p)`.

Renderer ausente → placeholder nomeando o id, nunca erro. O core traz
`image`, `table`, `text`, `keyvalue`. **Dispatch por id de tipo, nunca por
`inherits()`.** O mesmo mecanismo serve param → widget e viewer completo.

### 7.4 Entrega do JS sem bundler

- `<script type="importmap">` fixa `react`, `react-dom`, `@xyflow/react` e
  `trama`. É o importmap que garante **uma única instância de React** entre
  core e coleções.
- Por coleção carregada: `addResourcePath("tr-<id>", inst)` +
  `<script type="module">` depois do core, e o `<link rel="stylesheet">` do
  `css =` depois do `trama.css`. js e css na mesma pasta viram uma
  `htmlDependency` só; em pastas diferentes, duas (com nomes distintos, porque
  o htmltools deduplica por nome).
- **Vendorizar os ESM em `inst/www/vendor/` [ind]** — não CDN ao vivo. Pacote
  instalado tem que funcionar offline, e CRAN não aceita dependência de rede.
- Contrato público é o módulo `trama`. Toolchain é decisão de cada coleção.

---

## 8. Identidade e versionamento

| Coisa | Identidade | Regra |
|---|---|---|
| Instância de nó | id aleatório na criação | nunca derivado de label, posição ou ordem |
| Tipo de nó | `coleção/nome` + `version` inteiro | doc guarda `type_version`; coleção fornece `migrate()` |
| Tipo de dado | `coleção/nome` + `version` | entra na chave e no nome do arquivo no store |
| Porta / param | nome | nunca índice |
| Seed | **materializada no documento** na criação | editável; renomear e agrupar não mudam |
| Seed dentro de bloco | `hash(seed da instância, seed interna do template)` | estável sob renome e reuso |
| Formato do doc | `format: 1` + migrações no core | JSON Schema publicado, pro LLM validar |
| Chave de cache | efêmera | pode mudar entre versões de R sem quebrar nada |

**Regra de ouro [ind]: hash é para cache, nunca para identidade.** O documento
tem que abrir daqui a um ano sem nenhum hash bater.

### 8.1 Store

Interface (`put`, `get`, `exists`, `preview_url`, `list`), backend `local` por
padrão. Handles não contêm caminho, só chave. Escrita atômica (temp + rename).

**GC [ind]:** o store cresce sem limite. Ao salvar, marcar chaves alcançáveis
do documento; `tr_gc()` remove o resto por idade.

---

## 9. Convergência entre os dois desenhos

Fundações em que o candidato e o independente chegaram no mesmo lugar sem se
verem: store como protocolo entre processos; worker devolve handle e nunca
valor; preview no worker; documento com ops; `mirai` com caminho síncrono pra
headless; coleção = pacote com registro fora do `globalenv()`; registro de
renderers com dispatch por id; identidade estável e seed materializada; hash
pra cache e não pra identidade; pureza declarada; adaptadores no lugar de nó
identidade; semântica separada de apresentação; sem bundler; bloco expandido
antes do motor.

Nenhuma fundação divergiu.

---

## 10. Rejeitado do desenho independente

- **Mini-linguagem `enabled_when`/`ports_when`** (expressão JSON interpretada
  no front). Mini-DSLs crescem — em duas semanas se quer `or`, `not`,
  aninhamento — e o servidor precisa validar o mesmo, então viram **dois
  interpretadores da mesma linguagem**. É desnecessário: o registry de JS já
  permite a coleção entregar essa lógica. Se entrar, é um caso trivial fechado
  (`visible_when: param == valor`), sem composição.
- **Multi-cliente no mesmo documento.** Ele avalia como custo zero; não é —
  são `ui` por cliente, presença e UX de conflito. Ferramenta local de uma
  pessoa não precisa.
- **`list<T>` e nós de ordem superior — adiado, não rejeitado.** É a adição
  mais valiosa dele e a de maior carga conceitual (unificação local, `map`/
  `reduce`, manifesto de handles). Os requisitos só ficam claros com dois
  domínios reais rodando. Entra depois da segunda coleção; a compatibilidade
  já é uma função, então o espaço está reservado.

---

## 11. Herdado do insumo, sem repensar

Front dirigido por catálogo; dois níveis de API com teste de equivalência;
pull-based com memo e motor sem saber de UI; expansão de subgrafo antes do
motor, em duas fases; `.history/` imutável por hash de versão de bloco;
comentários que documentam o bug real e não o código.

## 12. Resistir a mudar

Bundler no core; reatividade do Shiny pra execução; front conhecendo tipos de
nó; trocar o JSON do documento; sistema de tipos elaborado; motor sabendo de
blocos.
