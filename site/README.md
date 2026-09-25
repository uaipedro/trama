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

## Exemplo como canvas

Todo bloco ```r no corpo da página que usa `tr_add(...)` é convertido em build
por um plugin rehype (`src/lib/rehype-flow-example.ts`) num componente com
duas abas: Canvas (o grafo montado a partir do próprio código, com layout em
colunas por profundidade) e Código R (o bloco original, sem alteração). Não é
preciso — nem possível — desenhar o mockup à mão.

Para o canvas sair correto:

- o id curto de cada `tr_add("id", "tipo", ...)` só aparece como legenda
  pequena (`<code>tipo</code>`) no card; o título do card vem de
  `node-visuals.json` (campo `label`, exportado por
  `tools/site/export-node-visuals.R`);
- todo bloco do qual o exemplo depende tem de estar no próprio trecho `tr_add`
  — um bloco só citado em prosa não aparece no canvas;
- `from` é obrigatório em qualquer nó que não seja raiz do fluxo, como string
  única ou `c(...)` para vários pais; sem `from` explícito não há aresta.

A lógica de parsing (`src/lib/flow-example.ts`) e de renderização
(`src/lib/flow-canvas-html.ts`) tem testes em `src/lib/flow-example.test.ts`
(`npm test`).

## Pressupostos e referências dos blocos

As seções Pressupostos e Referências das páginas de bloco (frontmatter
`node:`) vêm de `src/data/node-docs.json`, gerado do núcleo e das coleções da
árvore por `Rscript tools/site/export-node-docs.R` (na raiz). O script apaga
`node_modules/.astro/data-store.json` para o Astro não servir páginas antigas;
o plugin (`src/lib/rehype-node-docs.ts`) relê o JSON quando ele muda.
