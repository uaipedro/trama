# Site de documentação do trama

O site é estático e é construído com Astro. O conteúdo público fica em
`src/content/docs/`, em Markdown.

## Desenvolvimento

```sh
cd site
npm install
npm run dev
```

`npm run check` valida tipos, frontmatter e rotas. `npm run build` gera o site
em `dist/`.

## Como escrever uma página

Crie um arquivo `.md` em `src/content/docs/`. O frontmatter é validado e exige
`title`, `description`, `section` e `order`; páginas de coleção também usam
`collection`, e páginas de bloco usam `node`.

```md
---
title: Ler CSV
description: Inicie um fluxo com uma tabela delimitada armazenada no projeto.
section: colecoes
collection: dados
node: data/read_csv
order: 2
related: [data/summary]
---
```

Para inserir a camada facilitadora, escreva uma citação Markdown cujo primeiro
termo em negrito seja `Antes de continuar`. O toggle do cabeçalho a mostra ou
oculta e preserva a escolha no navegador.

```md
> **Antes de continuar**
>
> Contexto necessário para acompanhar a explicação principal.
```

O conteúdo principal deve continuar completo com o toggle desligado. Consulte
`../docs/guia-estilo-site.md` antes de criar ou revisar uma página.
