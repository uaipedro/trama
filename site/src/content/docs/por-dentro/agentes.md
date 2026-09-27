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

## Comandos

Rode de dentro da pasta do projeto. Toda saída é JSON; recusa sai com
`"ok": false`, uma mensagem e status 1.

```bash
trama-agente state                         # nós, parâmetros, status, ligações
trama-agente catalog                       # blocos disponíveis, com portas
trama-agente catalog models/lm             # um bloco inteiro: params, escolhas, ajuda
trama-agente add data/example --id dados dataset=iris
trama-agente add models/lm --id ajuste --from dados
trama-agente set ajuste resposta=Petal.Length 'preditores=["Sepal.Length","Species"]'
trama-agente link ajuste coef              # porta omitida = primeira compatível
trama-agente result coef --wait 30         # status, resumo, preview; gráfico como PNG
trama-agente rm coef
trama-agente undo
```

`valor` é lido como JSON quando dá (`n=3`, `x=true`, listas) e como texto
quando não (`coluna=peso`). `add --from` liga o bloco novo à primeira entrada
compatível e posiciona o card à direita da origem; o gesto inteiro é um passo
de desfazer. Para operações cruas do documento há `op '<json>'` e
`apply arquivo.json`, que aplica uma lista de operações como um passo só.

## O que o agente vê

`result` não dispara execução: devolve o último estado do bloco nesta sessão.
Para tabela, vêm dimensões, colunas e as primeiras linhas; para teste e
modelo, o que o card mostra; para gráfico, o caminho do PNG, que o agente pode
abrir. Um bloco que ainda não rodou responde `idle`.

## Limites

- Fala com a aba aberta mais recentemente. Outras abas continuam funcionando,
  mas não recebem as edições do agente até recarregar.
- Com execução sequencial, o editor não responde enquanto calcula um bloco
  pesado; o CLI espera até 60 s e avisa.
- Não há aprovação de operação: o agente edita direto, e você desfaz.
