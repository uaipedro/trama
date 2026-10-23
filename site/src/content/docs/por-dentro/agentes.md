---
title: Agentes
description: Deixar um agente (Claude Code, Codex, um script) montar e ler a trama aberta no editor, com cada passo aparecendo na tela.
section: por-dentro
order: 3
---

Com o editor aberto, um agente pode editar o fluxo que está na tela pela linha
de comando `trama-agente`. Cada comando vira uma operação comum do editor:
o card aparece e roda na hora, ganha um halo breve para você ver onde o
agente mexeu, e `Ctrl+Z` desfaz como qualquer gesto seu.

Para usar com um assistente que fala MCP (Claude Code, Codex, Cursor), veja
[Assistente (MCP)](/trama/por-dentro/mcp/): os mesmos comandos, como tools.

## Ligar

O canal sobe junto com o editor (`trama::tr_app()`). Ele só aceita conexões
da própria máquina e exige um token gravado em `.trama/control-agente.json`,
na pasta do projeto. Para desligar: `options(trama.controle = FALSE)` antes de
abrir.

Para ter o comando no terminal:

```bash
ln -s "$(Rscript -e 'cat(system.file("bin/trama-agente", package="trama"))')" ~/.local/bin/trama-agente
```

O CLI usa o pacote `curl` do R (`install.packages("curl")`).

Desenvolvendo o próprio trama (sem instalar, com `pkgload::load_all`), aponte
`TRAMA_DEV` para a raiz do repositório e o script carrega o código de lá:

```bash
TRAMA_DEV=~/trama Rscript ~/trama/inst/bin/trama-agente state
```

## Comandos

Rode de dentro da pasta do projeto. Toda saída é JSON; recusa sai com
`"ok": false`, uma mensagem e status 1.

```bash
trama-agente state                         # nós, parâmetros, status, ligações
trama-agente catalog                       # blocos disponíveis, com portas
trama-agente catalog --busca anova         # só os blocos que casam, melhor primeiro
trama-agente catalog models/lm             # um bloco inteiro: params, escolhas, ajuda
trama-agente add data/example --id dados dataset=iris
trama-agente add models/lm --id ajuste --from dados
trama-agente set ajuste resposta=Petal.Length 'preditores=["Sepal.Length","Species"]'
trama-agente link ajuste coef              # porta omitida = primeira compatível
trama-agente result coef --wait 30         # status, resumo, preview; gráfico como PNG
trama-agente rm coef
trama-agente undo
trama-agente accept                        # aceita a versão nova dos blocos em version_drift
```

O catálogo inteiro passa de 50 KB. Para achar um bloco, prefira `--busca`: é
a mesma busca do "+" do editor (sem acento nem caixa, toda palavra tem de
casar) e devolve até 12 blocos com o `total` que casou. Ela procura no texto
dos blocos, não em sinônimos: se "normalidade" não achar, tente "normal".

`valor` é lido como JSON quando dá (`n=3`, `x=true`, listas) e como texto
quando não (`coluna=peso`). `add --from` liga o bloco novo à primeira entrada
compatível e posiciona o card à direita da origem; o gesto inteiro é um passo
de desfazer. Para operações cruas do documento há `op '<json>'` e
`apply arquivo.json`, que aplica uma lista de operações como um passo só.

### Operações cruas

O açúcar cobre o dia a dia; para o resto, `op` recebe uma operação do
documento (as mesmas que o editor manda) e `apply` uma lista delas, aplicada
como um passo de desfazer. As mais usadas:

```json
{"op": "add_node", "id": "m", "type": "models/lm", "params": {"resposta": "mpg"}, "position": [400, 0]}
{"op": "connect", "from_node": "dados", "from_port": "out", "to_node": "m", "to_port": "dados"}
{"op": "disconnect", "from_node": "dados", "from_port": "out", "to_node": "m", "to_port": "dados"}
{"op": "set_param", "node": "m", "name": "preditores", "value": ["wt", "hp"]}
{"op": "move", "node": "m", "x": 400, "y": 300}
{"op": "rename", "node": "m", "label": "Modelo principal"}
{"op": "remove_node", "node": "m"}
{"op": "batch", "ops": [ ... ]}
```

Os nomes de porta e de param vêm de `catalog <tipo>`. A lista completa de
operações, com os campos que cada uma exige, está em `R/document.R`
(`.tr_op_fn()` e as funções `.tr_op_*`). Campo faltando volta como recusa
nomeando o campo.

### Editar e ver o efeito

Toda edição (`add`, `link`, `set`, `rm`, `op`, `apply`, `undo`, `accept`) aceita
`--wait N`: espera até N segundos o fluxo parar de rodar e devolve, em
`efeito`, o status do que a edição tocou e de tudo abaixo. Um passo só em vez
de editar e depois consultar cada bloco:

```bash
trama-agente set ajuste 'preditores=["wt","xyz"]' --wait 30
```

```json
"efeito": [
  {"node": "ajuste", "status": "failed",
   "message": "Param 'preditores': coluna inexistente: xyz. Disponíveis: ..."},
  {"node": "coef", "status": "blocked",
   "causa": [{"node": "ajuste", "status": "failed"}]}
]
```

Sem `--wait`, a edição volta assim que aplicada e o resultado chega depois.
Depois de `rm` e `undo` o efeito lista o fluxo inteiro.

### Contar a análise: frames, notas e layout

Um fluxo de dezenas de blocos numa linha só não se lê. O agente organiza a
tela como uma narrativa (perguntas, etapas, conclusão) com os mesmos frames,
notas e grupos do editor; nada novo entra no documento. `guide` imprime as
orientações, que valem tanto para o CLI quanto para o MCP:

1. Escreva as perguntas e as etapas antes de montar. Uma etapa é um frame com
   título numerado.
2. Monte cada etapa dentro do seu frame (`add ... --frame F`, ou
   `frame put F` para blocos que já existem).
3. Abra cada frame com uma nota Markdown: o que se testa ali e por quê.
4. Prenda uma nota aos resultados-chave, com o número lido em `result`, nunca
   escrito de memória.
5. Dê rótulos que se leem sozinhos (`rename`): o rótulo vira legenda no
   documento.
6. Feche com um frame de conclusão só com nota.
7. Depois de montar, `layout`; ajuste à mão com `move` e `frame set --order`.

```bash
trama-agente frame add "1. Dados e descritivas" dados niveis ibv sp --id etapa1 --color azul
trama-agente frame add "2. Estacionariedade" --id etapa2 --color verde
trama-agente add series/adf --id adf_ibv --from ibv --frame etapa2
trama-agente note add --frame etapa2 --file abre-etapa2.md
trama-agente note add "ADF no log do IBV: -2,10 > -3,45; não rejeita a raiz unitária." --node adf_ibv
trama-agente rename adf_ibv "ADF do log do Ibovespa"
trama-agente frame add "3. Conclusão" --id conclusao --color verde
trama-agente note add --frame conclusao --file conclusao.md --w 520
trama-agente layout
trama-agente move etapa2 --dx 0 --dy 200   # o frame leva o que tem dentro
```

- **Frame** (`frame add|set|rm|put|take`): frame novo nasce à direita de
  tudo, com proporção `livre`, e já traz os blocos pedidos, arrumados dentro
  dele; a resposta traz o `id`. Pertencer a um frame é estar dentro dele,
  como no editor: o frame se rearruma e cresce para caber, e o que estava à
  direita ou abaixo anda para abrir espaço. `frame set --order N` muda a
  posição dele na sequência; `frame rm` tira o frame e deixa os blocos.
- **Nota** (`note add|set|rm`): Markdown, com a altura calculada pelo texto
  (`--w`, `--h` para mudar; `--file` lê de um arquivo). Com `--frame`, abre a
  etapa, à esquerda dos blocos. Com `--node`, fica logo abaixo do bloco e
  entra num grupo com ele: anda junto no editor e no layout. Sem nenhum dos
  dois, fica solta à direita de tudo (ou em `--x --y`). Cores: `azul`,
  `verde`, `amarelo`, `laranja`, `rosa`, `roxo`, `cinza`; nota também aceita
  `nenhuma`, o padrão.
- **Posição** (`move`): `move dados=0,0 ajuste=400,0` põe o canto de cima à
  esquerda onde se pede; `move ids... --dx N --dy N` desloca. Como o arrasto:
  frame leva tudo o que contém, bloco leva o preview solto, e quem está num
  grupo leva o grupo.
- **Layout por etapas** (`layout`): cada frame vira uma coluna, na ordem dos
  frames, da esquerda para a direita. Dentro dele, o ramo principal (o que
  leva o fluxo para a etapa seguinte) fica em cima e os gráficos, testes e
  tabelas laterais embaixo; irmãos se empilham, as entradas da etapa seguem a
  ordem dos pais na etapa anterior e cada corrente sai reta. A nota de
  abertura fica à esquerda; notas presas, sob o bloco. Blocos fora de frame
  vão para uma faixa abaixo. Arrumar de novo o que já está arrumado não muda
  nada (`"Nada a mudar."`).

Cada um desses comandos é um passo de desfazer. As alturas dos cards vêm do
editor, que mede cada card na tela; um gráfico que chega depois e estica o
card não o tira da etapa: o próximo `layout` aumenta o frame.

### Sem editor aberto

`guide` (as orientações acima) roda sem editor e sem coleção. Outros três
comandos também dispensam o editor: leem as coleções instaladas (ou as de
`--colecoes trama.data,trama.models`). Com `TRAMA_DEV`, as coleções vêm de
`collections/` do repositório.

```bash
trama-agente catalog --offline --busca anova   # catalog cai aqui sozinho quando não há editor
trama-agente explain models/lm                 # bloco inteiro: params, portas, ajuda, referências
trama-agente validate fluxo.json               # lê, migra e valida um fluxo ou template
trama-agente guide                             # como contar a análise (frames, notas, layout)
```

`validate` aceita o `.json` de um fluxo (`flows/*.json`) ou um template.
As coleções vêm de `--colecoes`, do template ou do `trama.json` do projeto
acima do arquivo. A resposta traz `ok`, `migrated` e `problems`, a mesma
lista de `state` (`unknown_param`, `type_mismatch`,
`missing_required_input`...). Útil para conferir um fluxo escrito à mão antes
de abri-lo.

`help` também sai em JSON, e argumento malformado volta como
`{"ok": false, "reason": "args"}`.

## O que o agente vê

`result` não dispara execução: devolve o último estado do bloco nesta sessão.
Para tabela, vêm dimensões, colunas e as primeiras linhas; para teste e
modelo, o que o card mostra; para gráfico, o caminho do PNG, que o agente pode
abrir. Um bloco que ainda não rodou responde `idle`.

```json
{
  "ok": true, "node": "coef", "status": "done", "message": null,
  "outputs": {
    "out": {
      "type": "models/effects",
      "summary": null,
      "schema": null,
      "preview": {"renderer": "models/effects", "data": {"linhas": [ ... ]}, "files": null}
    }
  }
}
```

`status` é `done`, `cached`, `failed` (com `message`), `invalid`, `blocked`,
`running` ou `queued`. Um bloco `blocked` não tem mensagem própria; `causa`
aponta os blocos acima dele que falharam. `summary` e `schema` descrevem tabelas (linhas,
colunas, tipos); `preview.data` é o que o card desenha; `preview.files.png`,
quando existe, é o caminho absoluto da imagem. `result --wait N` espera até N
segundos o bloco sair de `queued`/`running`.

`state` devolve `rev`, `nodes` (id, tipo, rótulo, params, status,
`position`), `edges` (`"de:porta -> para:porta"`), `frames` (na ordem, com
`title`, `rect` `[x, y, w, h]`, os `nodes` e `notes` de dentro), `notes`
(texto, `rect`, o `frame` onde está e o `node` a que está presa) e `problems`
(validação do documento).

Um bloco que subiu de versão depois que o fluxo foi salvo aparece em
`problems` como `version_drift` (`from`, `to`, `aceitavel`): o resultado dele
pode ter mudado. Se a subida renomeou param, a migração roda sozinha ao abrir
e o aviso nem aparece. Senão, confira o resultado e rode `accept [no]...` (sem
nós, todos). Aceitar só grava a versão nova no fluxo; não muda param nem
resultado, e `undo` volta o aviso. No editor é o "aceitar" do aviso no canto;
no MCP, a tool `aceitar_versao`; em R, `tr_doc_accept_version()`.

## Limites

- Fala com a aba aberta mais recentemente. As abas do mesmo projeto e fluxo
  compartilham o documento e o desfazer: a edição do agente aparece em todas, e
  fechar uma aba não perde nada.
- Com execução sequencial, o editor não responde enquanto calcula um bloco
  pesado. O CLI aguarda a resposta definitiva sem limite de 60 s; uma operação
  só retorna depois que o editor confirma se foi aplicada ou recusada. Não repita
  uma edição por causa da demora.
- Não há aprovação de operação: o agente edita direto, e você desfaz.
