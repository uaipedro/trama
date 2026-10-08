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

Toda edição (`add`, `link`, `set`, `rm`, `op`, `apply`, `undo`) aceita
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

### Sem editor aberto

Três comandos não precisam do editor: leem as coleções instaladas (ou as de
`--colecoes trama.data,trama.models`). Com `TRAMA_DEV`, as coleções vêm de
`collections/` do repositório.

```bash
trama-agente catalog --offline --busca anova   # catalog cai aqui sozinho quando não há editor
trama-agente explain models/lm                 # bloco inteiro: params, portas, ajuda, referências
trama-agente validate fluxo.json               # lê, migra e valida um fluxo ou template
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

`state` devolve `rev`, `nodes` (id, tipo, rótulo, params, status),
`edges` (`"de:porta -> para:porta"`) e `problems` (validação do documento).

## Limites

- Fala com a aba aberta mais recentemente. Outras abas continuam funcionando,
  mas não recebem as edições do agente até recarregar.
- Com execução sequencial, o editor não responde enquanto calcula um bloco
  pesado. O CLI aguarda a resposta definitiva sem limite de 60 s; uma operação
  só retorna depois que o editor confirma se foi aplicada ou recusada. Não repita
  uma edição por causa da demora.
- Não há aprovação de operação: o agente edita direto, e você desfaz.
