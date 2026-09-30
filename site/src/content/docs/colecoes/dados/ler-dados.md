---
title: Ler dados
description: Traga para o fluxo uma tabela de um arquivo do projeto ou de um link — CSV, JSON, RDS, Parquet, Excel ou .zip.
section: colecoes
collection: dados
node: data/read
category: fonte
order: 2
related: [data/summary, data/clean_names, data/public]
---

## O que o bloco faz

`data/read` lê uma tabela e a coloca no fluxo. O mesmo bloco serve para
qualquer formato: ele reconhece pela extensão se o arquivo é CSV (`.csv`,
`.tsv`, `.txt`), JSON, RDS, Parquet ou Excel (`.xlsx`, `.xls`), e abre um
`.zip` para ler o arquivo de dados que estiver dentro.

**Arquivo ou link** aceita um caminho relativo à pasta do projeto, como
`dados/vendas.csv`, um caminho absoluto, ou um link `https://`.

## Quando usar

Use sempre que o dado já existe fora do fluxo: uma planilha exportada, um CSV
publicado num repositório, uma base compartilhada no Google Sheets. Para os
conjuntos que vêm com o R ou com pacotes, veja **Dados de exemplo** e
**Base pública**.

## Configuração

- **Formato** fica em `auto` e decide pela extensão. Escolha à mão quando o
  arquivo tem extensão diferente da real.
- **Separador** e **Marcas de faltante** valem para CSV. Planilhas exportadas
  em português costumam usar `;`. Células vazias já contam como faltantes;
  acrescente outros códigos separados por vírgula, como `NA, -, sem dado`.
- **Planilha** vale para Excel: o nome da aba ou a posição (`1`).
- **Arquivo no zip** escolhe qual arquivo ler quando o `.zip` tem mais de um.
  Se ficar em branco e houver vários, o card lista os nomes.

## Links

Um link é baixado **uma vez** para a pasta `data/` do projeto, e dali em diante
o fluxo lê essa cópia: roda sem internet e dá o mesmo resultado amanhã. A
origem de cada cópia fica anotada em `data/.origens.json`. Para trazer a versão
atual do link, clique em **Baixar de novo** no card.

Links de navegador funcionam colados como estão:

- **GitHub**: o link da página do arquivo (`github.com/.../blob/...`) vira o
  do arquivo cru.
- **Google Sheets**: a planilha compartilhada é exportada como CSV, da aba que
  estava aberta no link.
- **Google Drive**: o link de visualização vira o de download.

O link precisa ser público. Se ele devolver uma página da web em vez de um
arquivo (login, aviso de arquivo grande), o card avisa.

Também dá para **colar o link direto no canvas**: o bloco aparece já
preenchido. Soltar um arquivo CSV ou JSON no canvas faz o mesmo.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

arquivo <- tempfile(fileext = ".csv")
writeLines(c("regiao;valor", "Norte;120", "Sul;NA"), arquivo)
tr_flow(reg) |>
  tr_add("ler", "data/read", path = arquivo, delim = ";", na = "NA") |>
  tr_add("perfil", "data/summary", from = "ler")
```

A tabela lida tem duas linhas, com `regiao` como texto e `valor` como número;
`NA` é reconhecido como faltante. O perfil expõe essa ausência na coluna
`faltantes`.

## Como interpretar

O tipo de cada coluna é inferido pelo conteúdo. Números com formato
brasileiro, como `1.234,56`, podem chegar como texto; confira o perfil e use
**Converter tipo** quando necessário.

Para arquivo local, o fluxo registra caminho, tamanho e data de modificação:
ao salvar o arquivo de novo, a leitura e as etapas seguintes são recalculadas.
Para link, quem manda é a cópia em `data/`.

Fluxos feitos com os antigos **Ler CSV**, **Ler JSON**, **Ler RDS**, **Ler
Parquet** e **Ler Excel** abrem já com este bloco, no formato correspondente.
