---
title: Por dentro do trama
description: Como o trama é instalado, o que roda no CLI e o caminho de um fluxo, do comando ao resultado no card.
section: por-dentro
order: 1
---

O trama é um pacote R para construir e executar fluxos de computação em um
editor visual. Esta seção reúne o que fica por trás da tela: como instalar,
como o CLI ajuda quem ainda não tem R e como um fluxo é executado por dentro.

## Instalação no R

O pacote requer R 4.1 ou versão posterior. Enquanto não estiver disponível no
CRAN, a instalação é feita pelo GitHub com o
[`pak`](https://pak.r-lib.org):

```r
install.packages("pak")

pak::pak(c(
  "uaipedro/trama",
  "uaipedro/trama/collections/trama.data",
  "uaipedro/trama/collections/trama.view"
))
```

As demais coleções são opcionais e instalam suas próprias dependências:
`trama.series`, `trama.models`, `trama.multi`, `trama.sampling` e
`trama.ml`. Detalhes e a ordem de instalação com `remotes` estão em
[Instalação](/trama/por-dentro/instalacao/).

## O CLI

Quem ainda não tem R instalado pode usar o `trama-cli`, um pacote npm que
baixa um R portátil e instala o núcleo do trama:

```bash
npm install -g @uaipedro/trama-cli
trama create meu-fluxo
```

`trama create` cria a pasta do projeto, pergunta quais coleções acrescentar e
abre o editor. A lista completa de comandos está em
[Instalação](/trama/por-dentro/instalacao/).

## Como um fluxo roda

Um fluxo organiza funções R em um diagrama de blocos conectados. Três peças
sustentam essa organização:

**Registro.** `tr_registry()` cria um registro vazio; `tr_use("trama.data",
registry = reg)` carrega os tipos, blocos e renderizadores de uma coleção
nele. `tr_app()` e `tr_project()` criam esse registro automaticamente para o
editor.

**Documento.** `tr_flow(reg)`, `tr_add()` e `tr_link()` descrevem o fluxo como
um documento — os mesmos que o editor visual produz e que também podem ser
escritos como JSON. `tr_add()` liga a primeira saída à primeira entrada
compatível de um bloco pelo argumento `from`; `tr_link()` cobre as demais
ligações e `tr_set()` altera parâmetros depois de criado o bloco.

**Execução.** A cada alteração, o núcleo recalcula só os nós afetados:

```text
comando → documento → plano (hash por nó) → fila → workers
                                                     ↓
                                store: <hash>.artefato + <hash>.preview
                                                     ↓
                                              eventos → front
```

Cada nó tem uma chave de conteúdo (hash) derivada de si e de suas entradas.
Um worker executa a função do bloco fora do processo da interface, grava o
artefato e o `preview` sob essa chave no armazenamento e devolve apenas o
identificador. A interface escuta os eventos e atualiza somente os cards que
mudaram.

Esse protocolo é o que permite que o editor visual, o documento JSON e a DSL
em R representem exatamente o mesmo fluxo, sem transferir objetos R ativos
entre processos.
