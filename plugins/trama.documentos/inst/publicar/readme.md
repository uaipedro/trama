# {{titulo}}

{{descricao}}

- **Autores:** {{autores}}
- **Versão:** {{versao}}
- **Licença:** {{licenca}} para o conteúdo; {{licenca_codigo}} para o código (ver `LICENSE`).

## Como citar

{{citacao}}

O arquivo `CITATION.cff` traz a mesma citação em formato de máquina.

## O que há aqui

Um pacote [RO-Crate](https://w3id.org/ro/crate/1.1): `ro-crate-metadata.json` descreve cada arquivo, com tamanho e sha256.

- `index.qmd` — o documento (Quarto), com o texto dos autores e os resultados do trama em blocos `::: {#trama-… .trama}`; `_extensions/` traz o modelo dele, com a versão fixada.
- `_trama/` — os resultados materializados que o documento mostra, um por pasta:

{{itens}}

- {{fluxo}} — o fluxo do [trama](https://github.com/uaipedro/trama) que gerou tudo; `fluxo.svg` é o diagrama.
- `dados-entrada.json` — os dados que o fluxo lê e que não vieram junto.
- `ambiente.json` — versões de R, do trama, das coleções e dos pacotes.

Os modelos vêm reduzidos: só o que prevê, sem os dados de treino. `modelo.rds` lê com o trama (`trama.models::tr_models_predict()`); `motor.rds` e a função `prever` de `web.json` preveem em R puro, como a página faz no navegador.

## Como reproduzir

1. Instale o trama e as coleções listadas em `ambiente.json` (r-universe: `install.packages("trama", repos = c("https://uaipedro.r-universe.dev", "https://cloud.r-project.org"))`).
2. Traga os dados de entrada para a pasta do projeto, com o mesmo nome e o mesmo sha256:

{{dados}}

3. Abra `flow.json` no editor (`trama::tr_app()`) e rode o fluxo.
